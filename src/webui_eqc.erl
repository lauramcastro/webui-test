%%% @author Macias <>
%%% @copyright (C) 2015, Macias
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2015 by Macias <>

-module(webui_eqc).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include_lib("../webdrv/include/webdrv.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CHROMEDRIVER, "http://localhost:9515/").
-define(SESSION, test).
-define(URL, "http://udc.quickcheck-ci.com/").

-compile(export_all).

-record(state,{current_page,
               pages_and_actions}).

%% @doc Returns the state in which each test case starts. (Unless a different 
%%      initial state is supplied explicitly to, e.g. commands/2.)
-spec initial_state() -> eqc_statem:symbolic_state().
initial_state() ->
    #state{current_page = test:find_active_page(),
           pages_and_actions = test:get_pages_and_actions()}.

%% @doc Command generator, S is the current state
-spec command(S :: eqc_statem:symbolic_state()) -> eqc_gen:gen(eqc_statem:call()).
command(S) ->
    oneof([{call, ?MODULE, run_action, [S#state.current_page, "page_main_find_project"]},
           {call, ?MODULE, run_action, [S#state.current_page, "page_project_search_home"]}           
           ]).

%% @doc Next state transformation, S is the current state. Returns next state.
-spec next_state(S :: eqc_statem:symbolic_state(), V :: eqc_statem:var(), 
                 C :: eqc_statem:call()) -> eqc_statem:symbolic_state().
next_state(S, _V, {call, ?MODULE, run_action, [_C, _Action]}) ->
     S#state{current_page = test:find_active_page()}.

%% @doc Precondition, checked before command is added to the command sequence. 
-spec precondition(S :: eqc_statem:symbolic_state(), C :: eqc_statem:call()) -> boolean().
precondition(S, {call, ?MODULE, run_action, [CP, Action ]}) ->
    CP == test:get_page_from_action(Action, S#state.pages_and_actions);
precondition(_S, {call, _, _, _}) ->
    true.


%% @doc <i>Optional callback</i>, used to test a precondition during test execution.
%% -spec dynamic_precondition(S :: eqc_statem:dynamic_state(), C :: eqc_statem:call()) -> boolean().
%% dynamic_precondition(_S, {call, _, _, _}) ->
%%   true.

%% @doc Postcondition, checked after command has been evaluated
%%      Note: S is the state before next_state(S,_,C) 
-spec postcondition(S :: eqc_statem:dynamic_state(), C :: eqc_statem:call(), 
                    Res :: term()) -> boolean().
postcondition(_S, {call, _, _, _}, _Res) ->
    true.

%% @doc <i>Optional callback</i>, Invariant, checked for each visited state 
%%      during test execution.
%% -spec invariant(S :: eqc_statem:dynamic_state()) -> boolean().
%% invariant(_S) ->
%%   true.

%% @doc <i>Optional callback</i>, Returns true if operation is blocking 
%%      in current state. 
%% -spec blocking(S :: eqc_statem:symbolic_state(), C :: eqc_statem:call()) -> boolean().
%% blocking(_S, {call, _, _, _}) ->
%%   false.

%% @doc Default generated property
-spec prop_ui() -> eqc:property().
prop_ui() ->
     {ok, _Pid} = webdrv_session:start_session(?SESSION, ?CHROMEDRIVER,
                                              webdrv_cap:default_chrome(),
                                              10000),
    io:format("URL ~s~n", [?URL]),
    ok = webdrv_session:set_url(?SESSION, ?URL),

    ?FORALL(Cmds, commands(?MODULE),
            begin
                {H, S, Res} = run_commands(?MODULE,Cmds),
                pretty_commands(?MODULE, Cmds, {H, S, Res},
                                Res == ok)
            end).
    

run_action(S, ActionName) ->
    test:run_action(test:get_action_id(ActionName, S#state.pages_and_actions)).
