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
-export([do_nothing/0, run_action/1]).
-export([api_spec/0, initial_state/0, command/1]).
-export([precondition/2, callouts/2, postcondition/3, next_state/3]).

-include("webui.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_mocking.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-record(test_state, {current_page,
		     actions}).

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
				   eqc_component:pretty_commands(?MODULE, Cmds, HSR,
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
    {ok, URL} = webdrv_session:get_url(?SESSION),
    {ok, Actions} = webui_actions:get_actions(),
    #test_state{current_page = URL,
		actions = Actions}.

%% @private
command(S) ->
    case S#test_state.actions of
	[] ->
	    {call, ?MODULE, do_nothing, []};
	ListOfActions ->
	    {call, ?MODULE, run_action, [oneof(ListOfActions)]}
    end.

%% @private
do_nothing() ->
    ?DEBUG("No actions available~n", []),
    ok.

%% @private
run_action({link, E}) ->
    ?DEBUG("Follow link ~p~n", [E]),
    webui_actions:activate_element(E);
run_action({button, E}) ->
    ?DEBUG("Press button ~p~n", [E]),
    webui_actions:activate_element(E);
run_action({input, _E}) ->
    ?DEBUG("Input text is not yet supported~n", []),
    ok.

%% @private
precondition(_S, _C) ->
    true.

%% @private
postcondition(_S, _C, ok) ->
    true;
postcondition(_S, _C, _R) ->
    false.

%% @private
next_state(S, _R, _C) ->
    {ok, Actions} = webui_actions:get_actions(),
    S#test_state{actions = Actions}.

%% @private
api_spec() ->
    #api_spec{}.

%% @private
callouts(_,_) ->
    empty.
