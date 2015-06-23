%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc
%%%      Generic model for web-based GUIs, implemented as a behaviour.
%%% @end

-module(webui_model).

-callback run() -> boolean().
-callback setup() -> ok.
-callback teardown() -> ok.

% exported for behaviour implementation
-export([run/1, setup/1, teardown/1]).
% exported for QC
-export([initial_state/0]).

-include("webui.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-record(test_state, {field1, field2}). % these are sample fields as of now

%% @doc
%%      Runs model, this is, executes a generic QuickCheck property to trigger
%%      sequences of interactions with the web-based GUI.
%% @end
-spec run(Mod :: atom()) -> boolean().
run(Mod) ->
    eqc:quickcheck(prop_webui(Mod)).

% Generic QC property (body).
prop_webui(Mod) ->
    ?SETUP(fun() -> setup(Mod),
		    fun() -> teardown(Mod) end
	   end,
	   ?FORALL(Cmds, dynamic_commands(?MODULE),
		   ?CHECK_COMMANDS(HSR={_History, _FinalState, Result}, ?MODULE, Cmds,
				   pretty_commands(?MODULE, Cmds, HSR,
						   Result == ok)))).

%% @doc
%%      Setup function, to be executed before QuickCheck tests begin; performs
%%      generic initialisation, then calls back to the setup/0 function in the
%%      specific implementation of the behaviour.
%% @end
-spec setup(Mod :: atom()) -> ok.
setup(Mod) ->
    {ok, _Pid} = webdrv_session:start_session(?SESSION,
					      ?CHROMEDRIVER,
					      webdrv_cap:default_chrome(),
					      10000), % what is this?
    Mod:setup().

%% @doc
%%      Teardown function, to be executed after QuickCheck tests finish; perform
%%      generic clean-up, then calls back to the teardown/0 function in the
%%      specific implementation of the behaviour.
%% @end
-spec teardown(Mod :: atom()) -> ok.
teardown(Mod) ->
    Mod:teardown(),
    ok = webdrv_session:stop_session(?SESSION),
    ok.

%% @private
initial_state() ->
    #test_state{}.
