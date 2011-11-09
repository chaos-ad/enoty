-module(enoty_srv).
-behaviour(gen_server).

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
    gen_server:call(?SERVER, {send, Message, Users}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, Network} = application:get_env(network),
    {ok, Network}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, State};

handle_call({send, Message, Users}, _, odnoklassniki) ->
    SendFn =
    fun(UserID) ->
        Result =
        social_api:invoke_method({notifications, sendSimple}, [{uid, UserID}, {text, Message}]),
        io:format("Debug: ~p: ~p~n", [UserID, Result])
    end,
    {reply, lists:foreach(SendFn, Users), odnoklassniki};

handle_call({send, Message, Users}, _, vkontakte) ->
    Result = social_api:invoke_method({secure, sendNotification}, [{uids, Users}, {message, Message}]),
    io:format("Debug: ~p~n", [Result]),
    {reply, ok, vkontakte};

handle_call({send, Message, Users}, _, mymail) ->
    Result = social_api:invoke_method({notifications, send}, [{uids, Users}, {text, Message}]),
    io:format("Debug: ~p~n", [Result]),
    {reply, ok, mymail};

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
