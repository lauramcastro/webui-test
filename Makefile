SUT=vodkatv_adminui
BROWSERDRV=deps/chromedriver
WEBDRV=deps/webdrv

all: compile

compile: src/*.erl
	erlc -pa ebin -o ebin/ -I include src/*.erl test/*.erl

run: ebin/test.beam
	./$(BROWSERDRV) &
	erl -pa ebin/ -pa $(WEBDRV)/ebin/

$(SUT): compile
	./$(BROWSERDRV) &
	erl -pa ebin/ -pa $(WEBDRV)/ebin/ -run $(SUT) run -run init stop -noshell

clean:
	rm -f ebin/* src/*~ test/*~
