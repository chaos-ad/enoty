#!/bin/bash
cd `dirname $0`
exec erl -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -sname enotysrv@`hostname` -cookie enoty_cookie -s enoty
