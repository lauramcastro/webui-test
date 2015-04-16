all: compile

compile: src/*.erl
	erlc -o ebin/ src/*.erl

run: ebin/test.beam
	./chromedriver &
	erl -pa ebin/ -pa webdrv/ebin/

admingui: compile
	./chromedriver &
	erl -pa ebin/ -pa webdrv/ebin/ -run admingui_eqc run -run init stop -noshell

test: ebin/test.beam
	./chromedriver &
	erl -pa ebin/ -pa webdrv/ebin/ -run test test -run init stop -noshell
