#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname enotyadm@`hostname` -cookie enoty_cookie

main(_) ->
    Host = list_to_atom("enotysrv@" ++ net_adm:localhost()),
    io:format("Stopping daemon: "),
    Res = rpc:call(Host, init, stop, []),
    io:format("~p~n", [Res]).


