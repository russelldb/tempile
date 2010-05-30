erl -pa ebin/ -pa deps/mustache.erl/ebin/ -boot start_sasl -eval "application:start(tempile)"
