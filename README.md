# webui-test
## QuickCheck-based web-based GUI testing framework

WebUI-test is a QuickCheck-based testing framework intended for
automatic generation and execution of test sequences for web-based
graphical user interfaces (GUIs).

### What does that mean?

If you have an application or system for which the user interface is
your browser, then webui-test is for you. This tool is focused
primarly in enabling a sort of QuickCheck-based monkey testing. It is
QuickCheck-based because rather than writing unit tests, we provide a
generic interaction model (a QuickCheck model) that you can fine-tune
(but that will save you a lot of effort) and a set of generic
interaction actions (ways in which a person will usually interact with
a web-based GUI: clicking links and buttons, writing in textboxes,
going forward/backwards, reloading, etc.).  The interaction actions
can also be fine-tuned if you use a particular web framework to build
your user interface.

### What do I need to use it?

Apart from a [QuickCheck licence](http://quviq.com), there is only one
  major dependency: `chromedriver`, the webdriver implementation for
  the chromium browser (or equivalent if you intend to use a different
  browser).
  
### How do I use it?
  
There is a very simple example in the `examples` folder. In order to
get it running, make sure you have the following installed (or
equivalent):

* chromium-chromedriver
* chromium-browser
* Erlang/OTP R18

Download/clone WebUI-test, and to compile it, just run

```
$ ./rebar3 compile
```

Then, make sure to start the `chromedriver` before you run the
example. The example is then run by opening a shell, moving into the
`examples` folder, compiling the two modules and running the tests:

```
$ ./rebar3 shell
> cd(examples).
ok
> c(google_search,[{i,"../include"}]).
{ok,google_search}
> c(google_search_actions,[{i,"../include"}]).
{ok,google_search_actions}
> google_search:run().
.
OK, passed 1 tests
true
```

You should see a new window of the browser appear, the Google search
main page loading, and random interaction from there on. Of course,
you need to have internet access.

The current configuration will only generate and run 1 test sequence,
but this can easily be adjusted by calling

```
> google_search:run(10).
..........
OK, passed 10 tests
true
```
