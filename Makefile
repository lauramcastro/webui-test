all: compile

compile: src/*.erl
	erlc -o ebin/ src/*.erl

run: ebin/test.beam
	./chromedriver &
	erl -pa ebin/ -pa webdrv/ebin/

test: ebin/test.beam
	./chromedriver &
	erl -pa ebin/ -pa webdrv/ebin/ -run test test -run init stop -noshell
