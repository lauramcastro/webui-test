%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc
%%%      Generic model for web-based GUIs, implemented as a behaviour.
%%% @end

-module(webui_model).

-export([run/1, setup/1, teardown/1]).
-export([initial_state/0]).

-callback run() -> boolean().
-callback setup() -> ok.
-callback teardown() -> ok.

-include("webui.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-record(test_state, {field1, field2}). % these are sample fields as of now

run(Mod) ->
    eqc:quickcheck(prop_ui(Mod)).

prop_ui(Mod) ->
    ?SETUP(fun() -> setup(Mod),
		    fun() -> teardown(Mod) end
	   end,
	   ?FORALL(Cmds, dynamic_commands(?MODULE),
		   ?CHECK_COMMANDS(HSR={_History, _FinalState, Result}, ?MODULE, Cmds,
				   pretty_commands(?MODULE, Cmds, HSR,
						   Result == ok)))).

setup(Mod) ->
    {ok, _Pid} = webdrv_session:start_session(?SESSION,
					      ?CHROMEDRIVER,
					      webdrv_cap:default_chrome(),
					      10000), % what is this?
    Mod:setup().

teardown(Mod) ->
    ok = webdrv_session:stop_session(?SESSION),
    Mod:teardown().

initial_state() ->
    #test_state{}.
