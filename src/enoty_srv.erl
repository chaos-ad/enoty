-module(enoty_srv).
-behaviour(gen_server).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([send/3]).
-export([start_link/0, stop/0, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    stop(normal).

stop(Reason) ->
    gen_server:call(?SERVER, {stop, Reason}).

send(Message, Users, Timeout) ->
    gen_server:call(?SERVER, {send, unicode:characters_to_binary(Message), Users}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, Network} = application:get_env(network),
    {ok, Network}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, State};

handle_call({send, Message, Users}, _, odnoklassniki=Net) ->
    Result =
    lists:foldl
    (
        fun(UsersPortion, Result) -> [do_send(Net, Message, UsersPortion)|Result] end,
        [],
        Users
    ),
    {reply, lists:concat(lists:reverse(Result)), Net};

handle_call({send, Message, Users}, _, vkontakte=Net) ->
    Result =
    lists:foldl
    (
        fun(UsersPortion, Result) -> [do_send(Net, Message, UsersPortion)|Result] end,
        [],
        split(100, Users)
    ),
    {reply, lists:concat(lists:reverse(Result)), Net};

handle_call({send, Message, Users}, _, mymail=Net) ->
    Result =
    lists:foldl
    (
        fun(UsersPortion, Result) -> [do_send(Net, Message, UsersPortion)|Result] end,
        [],
        split(200, Users)
    ),
    {reply, lists:concat(lists:reverse(Result)), Net};

handle_call(_, _, State) ->
    {noreply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_send(odnoklassniki=Net, Message, UserID) ->
    Res = social_api:invoke_method({notifications, sendSimple}, [{uid, UserID}, {text, Message}]),
    handle_response(Net, Message, UserID, Res);

do_send(vkontakte=Net, Message, Users) when is_list(Users) ->
    Res = social_api:invoke_method({secure, sendNotification}, [{uids, uids2string(Users)}, {message, Message}]),
    handle_response(Net, Message, Users, Res);

do_send(mymail=Net, Message, Users) when is_list(Users) ->
    Res = social_api:invoke_method({notifications, send}, [{uids, uids2string(Users)}, {text, Message}]),
    handle_response(Net, Message, Users, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_response(odnoklassniki, _, UserID, true) -> [{to_integer(UserID), ok}];
handle_response(odnoklassniki, _, UserID, {struct,ErrorInfo}) ->
    Code = proplists:get_value(<<"error_code">>, ErrorInfo),
    ErrMsg = proplists:get_value(<<"error_msg">>, ErrorInfo),
    [{to_integer(UserID), {error, {Code, ErrMsg}}}];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_response(vkontakte, _, Users, {struct,[{<<"response">>,Result}]}) ->
    {Delivered, Undelivered} = split_delivered(Users, Result),
    lists:zip(Delivered, lists:duplicate(length(Delivered), ok)) ++
    lists:zip(Undelivered, lists:duplicate(length(Undelivered), {error, undelivered}));

handle_response(vkontakte, Msg, Users, {struct, [{<<"error">>, {struct, ErrorInfo}}]}) ->
    Code = proplists:get_value(<<"error_code">>, ErrorInfo),
    case Code =:= 6 of
        true  ->
            timer:sleep(250),
            do_send(vkontakte, Msg, Users);
        false ->
            ErrMsg = proplists:get_value(<<"error_msg">>, ErrorInfo),
            lists:zip(Users, lists:duplicate(length(Users), {error, {Code, ErrMsg}}))
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_response(mymail, _, Users, {struct, [{<<"error">>, {struct, ErrorInfo}}]}) ->
    Code = proplists:get_value(<<"error_code">>, ErrorInfo),
    ErrMsg = proplists:get_value(<<"error_msg">>, ErrorInfo),
    lists:zip(Users, lists:duplicate(length(Users), {error, {Code, ErrMsg}}));

handle_response(mymail, _, Users, Result) when is_list(Result) ->
    {Delivered, Undelivered} = split_delivered(Users, Result),
    lists:zip(Delivered, lists:duplicate(length(Delivered), ok)) ++
    lists:zip(Undelivered, lists:duplicate(length(Undelivered), {error, undelivered})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split(N, List) ->
    split(N, List, []).
split(N, List, Res) when is_list(List), length(List) =< N ->
    lists:reverse([List|Res]);
split(N, List, Res) when is_list(List), length(List)  > N ->
    {L1, L2} = lists:split(N, List),
    split(N, L2, [L1|Res]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

uids2string(List) when is_list(List) -> social_api_utils:concat(List, $,).

split_delivered(Users, Result) when is_binary(Result) ->
    split_delivered(Users, string:tokens(binary_to_list(Result), ","));

split_delivered(Users, Result) when is_list(Result) ->
    List1 = ordsets:from_list(lists:sort(lists:map(fun to_integer/1, Users))),
    List2 = ordsets:from_list(lists:sort(lists:map(fun to_integer/1, Result))),
    Undelivered = ordsets:subtract(List1, List2),
    Delivered = ordsets:subtract(List1, Undelivered),
    {Delivered, Undelivered}.

to_integer(Int) when is_integer(Int) -> Int;
to_integer(List) when is_list(List) -> list_to_integer(List);
to_integer(Binary) when is_binary(Binary) -> to_integer(binary_to_list(Binary)).
