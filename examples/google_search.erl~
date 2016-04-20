%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc
%%%      Specific model for Google search.
%%% @end

-module(google_search).
-behaviour(webui_model).

-export([run/0, setup/0, teardown/0]).
-include("webui.hrl").

-define(URL, "http://www.google.com/").

%% @doc
%%      Runs model for Google search.
%% @end
-spec run() -> boolean().
run() ->
    webui_model:run(?MODULE, ?URL).

%% @doc
%%      Setup function, to be executed before testing begins; performs
%%      specific initialisation for Google search (nothing in
%%      particular as of now, but this could involve log-in, for
%%      instance). We always need to initialise the specific actions
%%      component first of all.
%% @end
-spec setup() -> ok.
setup() ->
    ok = google_search_actions:setup(),
    % specific initialisation should follow here
    ok.

%% @doc
%%      Teardown function, to be executed after testing finishes; performs
%%      specific clean-up. We always need to terminate the specific actions
%%      component as the last step.
%% @end
-spec teardown() -> ok.
teardown() ->
    % specific cleanup should go here
    ok = google_search_actions:terminate().
