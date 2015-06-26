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
-export([run/2, setup/2, teardown/1]).
% exported for QC
-export([initial_state/0]).

-include("webui.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-record(test_state, {current_page,
		     pages_and_actions}).

%% @doc
%%      Runs model, this is, executes a generic QuickCheck property to trigger
%%      sequences of interactions with the web-based GUI.
%% @end
-spec run(Mod :: atom(),
	  Url :: string()) -> boolean().
run(Mod, Url) ->
    eqc:quickcheck(eqc:numtests(1,prop_webui(Mod, Url))).

% Generic QC property (body).
prop_webui(Mod, Url) ->
    ?SETUP(fun() -> setup(Mod, Url),
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
-spec setup(Mod :: atom(),
	    Url :: string()) -> ok.
setup(Mod, Url) ->
    {ok, _Pid} = webdrv_session:start_session(?SESSION,
					      ?CHROMEDRIVER,
					      webdrv_cap:default_chrome(),
					      10000), % what is this?
    ok = webdrv_session:set_url(?SESSION, Url),
    Mod:setup().

%% @doc
%%      Teardown function, to be executed after QuickCheck tests finish; performs
%%      generic clean-up, then calls back to the teardown/0 function in the
%%      specific implementation of the behaviour.
%% @end
-spec teardown(Mod :: atom()) -> ok.
teardown(Mod) ->
    Mod:teardown(),
    ok = webdrv_session:stop_session(?SESSION).

%% @private
initial_state() ->
    #test_state{current_page = webui_actions:find_active_page(),
		pages_and_actions = []}.

