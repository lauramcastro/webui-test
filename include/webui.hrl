%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright 2015
%%% @doc
%%%     Generic model for web-based GUIs, implemented as a behaviour
%%%     (header file).
%%% @end
%%%-------------------------------------------------------------------

-define(CHROMEDRIVER, "http://localhost:9515/").
-define(SESSION, webui_test).

-define(ID,     "id").
-define(TYPE,   "type").
-define(CLASS,  "class").
-define(SUBMIT, "submit").
-define(XPATH,  "xpath").
% these could be local to VoDKATV actions/model
-define(XPATH_BUTTON, "//button").
-define(XPATH_INPUT,  "//input").
-define(XPATH_LINK,   "//a").

-ifdef(debug).
-define(DEBUG(IOString, Args), io:format(IOString, Args)).
-define(WEBDRV_SESSION(X), R = webdrv_session:X, timer:sleep(500), R).
-else.
-define(DEBUG(IOString, Args), ok).
-define(WEBDRV_SESSION(X), webdrv_session:X).
-endif.
