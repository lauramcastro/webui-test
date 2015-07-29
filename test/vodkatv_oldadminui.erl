%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc
%%%      Specific model for VoDKATV AdminUI (old).
%%% @end

-module(vodkatv_oldadminui).
-behaviour(webui_model).

-export([run/0, setup/0, teardown/0]).
-include("webui.hrl").

%-define(URL, "http://193.144.63.20:8082/admin").
%-define(URL, "http://10.56.34.156:8082/admin").
-define(URL, "http://10.20.21.75:8082/admin").

%% @doc
%%      Runs model for the old VodKATV Admin GUI.
%% @end
-spec run() -> boolean().
run() ->
    webui_model:run(?MODULE, ?URL).

%% @doc
%%      Setup function, to be executed before testing begins; performs specific
%%      initialisation for the old VoDKATV Admin GUI, namely we login
%%      beforehand. We always need to initialise the specific actions component
%%      first of all.
%% @end
-spec setup() -> ok.
setup() ->
    ok = vodkatv_oldadminui_actions:setup(),
    ok = login().

%% @doc
%%      Teardown function, to be executed after testing finishes; performs
%%      specific clean-up. We always need to terminate the specific actions
%%      component as the last step.
%% @end
-spec teardown() -> ok.
teardown() ->
    ok = logout(),
    ok = vodkatv_oldadminui_actions:terminate().

%% @private
login() ->
    {ok, LoginField} = webui_actions:find_element_by_id("loginName"),
    {ok, PasswordField} = webui_actions:find_element_by_id("password"),
    {ok, [Submit]} = webui_actions:find_elements_by_type(?XPATH_INPUT, [visible,submit]),
    ok = webui_actions:set_element_value(LoginField,    "vodkatv"),
    ok = webui_actions:set_element_value(PasswordField, "vodkatv"),
    ok = webui_actions:activate_element(Submit).

%% @private
logout() ->
    {ok, LogoutLink} = webui_actions:find_element_by_id("logoutMsg"),
    ok = webui_actions:activate_element(LogoutLink).
