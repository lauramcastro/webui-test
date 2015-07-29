%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc
%%%      Specific actions for VoDKATV AdminUI (old).
%%% @end

-module(vodkatv_oldadminui_actions).
-behaviour(webui_actions).

-export([setup/0, terminate/0]).
-export([get_links/0, get_buttons/0, get_inputs/0]).

-include("webui.hrl").

%% @doc
%%      Specific actions for the old VoDKATV AdminUI: initialisation function.
%%      It always needs to fall back to the behaviour initialisation as well.
%% @end
-spec setup() -> ok.
setup() ->
    ok = webui_actions:setup(?MODULE).
    
%% @doc
%%      Specific actions for the old VoDKATV AdminUI: termination function.
%%      It always needs to fall back to the behaviour termination as well.
%% @end
-spec terminate() -> ok.
terminate() ->
    webui_actions:terminate().

%% @doc
%%      Generic action to find the interaction possibilities on the current
%%      active page based on following visible links.
%% @end
-spec get_links() -> {ok, [term()]}.
get_links() ->
    webui_actions:find_elements_by_type(?XPATH_LINK, [visible]).

%% @doc
%%      Generic action to find the interaction possibilities on the current
%%      active page based on clicking visible buttons.
%% @end
-spec get_buttons() -> {ok, [term()]}.
get_buttons() ->
    webui_actions:find_elements_by_type(?XPATH_BUTTON, [visible]).

%% @doc
%%      Generic action to find the interaction possibilities on the current
%%      active page based on filling-in visible input text fields.
%% @end
-spec get_inputs() -> {ok, [term()]}.
get_inputs() ->
    webui_actions:find_elements_by_type(?XPATH_INPUT, [visible]).
