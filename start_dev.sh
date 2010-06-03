#!/bin/sh
cd `dirname $0`
rebar -v compile
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -pa $PWD/deps/*/deps/*/ebin -boot start_sasl  -eval "application:start(tempile)"

