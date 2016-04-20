# webui-test
## QuickCheck-based web-based GUI testing framework

WebUI-test is a QuickCheck-based testing framework intended for
automatic generation and execution of test sequences for web-based
graphical user interfaces (GUIs).

### What does that mean?

If you have an application or system for which the user interface is
your browser, then **WebUI-test** is for you. This tool is focused
primarly in enabling a sort of QuickCheck-based monkey testing. It is
QuickCheck-based because rather than writing unit tests, we provide a
generic interaction model (a QuickCheck model) that you can fine-tune
(but that will save you a lot of effort) and a set of generic
interaction actions (ways in which a person will usually interact with
a web-based GUI: clicking links and buttons, writing in textboxes,
going forward/backwards, reloading, etc.).  The interaction actions
can also be fine-tuned if you use a particular web framework to build
your user interface.

### How do I use it?

There is a very simple example in the `examples` folder. In order to
get it running, make sure you have the following installed (or
equivalent):

* chromium-browser + [chromium-chromedriver](https://sites.google.com/a/chromium.org/chromedriver/) (or equivalent if you intend to use a different browser)
* [Erlang/OTP R18](http://www.erlang.org)
* a [QuickCheck licence](http://quviq.com)

Download or clone this repository, and build it:

```
$ ./rebar3 compile
```

Now, make sure to start the `chromedriver` before you run the
example (this is usually done just by executing the `chromedriver`
binary in the background; you **do not** need to lauch your browser,
just the driver). The example is then executed by opening an Erlang
shell, moving into the `examples` folder, compiling the two modules
that constitute the example, and calling the `run/0` function in
module `google_search`:

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

You can have more than one go (i.e. run more than 1 test sequence)
easily by calling `run/1` instead:

```
> google_search:run(10).
..........
OK, passed 10 tests
true
```
