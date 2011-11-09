-module(enoty).

-export([start/0, stop/0, send/2, send/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(?MODULE).

start(App) ->
    start_ok(App, application:start(App, permanent)).

send(Message, Users) ->
    send(Message, Users, infinity).

send(Message, Users, Timeout) ->
    enoty_srv:send(Message, Users, Timeout).

stop() ->
    application:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_ok(_, ok) ->
    ok;

start_ok(_, {error, {already_started, _App}}) ->
    ok;

start_ok(App, {error, {not_started, Dep}}) when App =/= Dep ->
    ok = start(Dep),
    start(App);

start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
