erl -pa ebin/ -pa deps/mustache.erl/ebin/ -pa deps/erlydtl/ebin  -boot start_sasl -eval "application:start(tempile)"
