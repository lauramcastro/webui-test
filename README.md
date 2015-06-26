# webui-test: QuickCheck-based web-based GUI testing framework

WebUI-test is a QuickCheck-based testing framework intended for
automatic generation and execution of test sequences for
web-based graphical user interfaces (GUIs).

## What does that mean?

If you have an application or system for which the user interface
is your browser, then webui-test is for you. This tool
is focused primarly in enabling a sort of QuickCheck-based
monkey testing. It is QuickCheck-based because rather than writing
unit tests, we provide a generic interaction model (a QuickCheck model)
that you can fine-tune (but that will save you a lot of effort)
and a set of generic interaction actions (ways in which a person will
usually interact with a web-based GUI: clicking links and buttons,
writing in textboxes, going forward/backwards, reloading, etc.).
The interaction actions can also be fine-tuned if you use a
particular web framework to build your user interface.

## What do I need to use it?

Apart from a QuickCheck licence, there are two main dependencies:

* `webdriver`, which provides an API to control the behaviour and inspect the
  state of a web browser in a platform-independent and language-neutral manner;
  in this project we use the [Erlang implementation](https://github.com/lauramcastro/webdrv).

* `chromedriver`, the webdriver implementation for the chromium browser (or
  equivalent if you intend to use a different browser).
  
## How do I use it?
  
We will provide some examples of use in upcoming commits. Please bear
with us.
