all: ebin/test.beam

ebin/%.beam: src/%.erl
	erlc -o ebin/ $<

run: ebin/test.beam
	./chromedriver &
	erl -pa ebin/ -pa webdrv/ebin/

test: ebin/test.beam
	./chromedriver &
	erl -pa ebin/ -pa webdrv/ebin/ -run test test -run init stop -noshell
