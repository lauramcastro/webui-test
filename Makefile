SUT=google_search
BROWSERDRV=deps/chromedriver
WEBDRV=deps/webdrv
FLAGS=-Ddebug

all: compile

compile: src/*.erl
	test -d ebin || mkdir ebin
	erlc -pa ebin -o ebin -I include $(FLAGS) src/*.erl test/*.erl

dialyzer:
	erlc +debug_info -pa ebin -o ebin -I include $(FLAGS) src/*.erl test/*.erl
	dialyzer -Wunmatched_returns \
                 -Werror_handling    \
                 -Wrace_conditions   \
                 -Wunderspecs ebin/*beam

$(SUT): compile
	./$(BROWSERDRV) &
	erl -pa ebin/ -pa $(WEBDRV)/ebin/ -run $(SUT) run -run init stop -noshell

docs:
	test -d doc || mkdir doc
	erl -noshell -run edoc_run files '["src/webui_actions.erl", "src/webui_model.erl"]' '[{dir,"doc/html"}]'

clean:
	rm -f  ebin/* include/*~ src/*~ test/*~ doc/*~
	rm -f  erl_crash.dump
	rm -rf doc/html
