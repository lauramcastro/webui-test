%%% @author Macías López <macias.lopez@udc.es>
%%% @copyright (C) 2015, Maías López
%%% @doc
%%% EQC state machine to test QuickCheck-CI interface
%%% @end
%%% Created : 22 Jan 2015

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
           pages_and_actions = get_pa()}.

%% @doc Command generator, S is the current state
-spec command(S :: eqc_statem:symbolic_state()) -> eqc_gen:gen(eqc_statem:call()).
command(S) ->
    oneof([{call, ?MODULE, run_action, [S, "page_main_home_a"]},
           {call, ?MODULE, run_action, [S, "page_main_find_project_a"]},
           {call, ?MODULE, run_action, [S, "page_main_project_overview_a"]},
           {call, ?MODULE, run_action, [S, "page_main_queued_projects_a"]},
           {call, ?MODULE, run_action, [S, "page_main_register_project_a"]},
           {call, ?MODULE, run_action, [S, "page_main_help_a"]},
           {call, ?MODULE, run_action, [S, "page_main_register_your_project_button"]},
           {call, ?MODULE, run_action, [S, "page_main_find_registered_project_button"]},
           
           {call, ?MODULE, run_action, [S, "page_project_search_home_a"]},
           {call, ?MODULE, run_action, [S, "page_project_search_find_project_a"]},
           {call, ?MODULE, run_action, [S, "page_project_search_project_overview_a"]},
           {call, ?MODULE, run_action, [S, "page_project_search_queued_projects_a"]},
           {call, ?MODULE, run_action, [S, "page_project_search_register_project_a"]},
           {call, ?MODULE, run_action, [S, "page_project_search_help_a"]},
           {call, ?MODULE, run_action, [S, "page_project_search_search_button"]},
    
           {call, ?MODULE, run_action, [S, "page_project_overview_home_a"]},
           {call, ?MODULE, run_action, [S, "page_project_overview_find_project_a"]},
           {call, ?MODULE, run_action, [S, "page_project_overview_project_overview_a"]},
           {call, ?MODULE, run_action, [S, "page_project_overview_queued_projects_a"]},
           {call, ?MODULE, run_action, [S, "page_project_overview_register_project_a"]},
           {call, ?MODULE, run_action, [S, "page_project_overview_help_a"]},
           {call, ?MODULE, run_action, [S, "page_project_overview_find_registered_project_button"]},

           {call, ?MODULE, run_action, [S, "page_queued_builds_home_a"]},
           {call, ?MODULE, run_action, [S, "page_queued_builds_find_project_a"]},
           {call, ?MODULE, run_action, [S, "page_queued_builds_project_overview_a"]},
           {call, ?MODULE, run_action, [S, "page_queued_builds_queued_projects_a"]},
           {call, ?MODULE, run_action, [S, "page_queued_builds_register_project_a"]},
           {call, ?MODULE, run_action, [S, "page_queued_builds_help_a"]},

           {call, ?MODULE, run_action, [S, "page_register_project_home_a"]},
           {call, ?MODULE, run_action, [S, "page_register_project_find_project_a"]},
           {call, ?MODULE, run_action, [S, "page_register_project_project_overview_a"]},
           {call, ?MODULE, run_action, [S, "page_register_project_queued_projects_a"]},
           {call, ?MODULE, run_action, [S, "page_register_project_register_project_a"]},
           {call, ?MODULE, run_action, [S, "page_register_project_help_a"]},
           {call, ?MODULE, run_action, [S, "page_register_project_here_a"]},
           {call, ?MODULE, run_action, [S, "page_register_project_register_project_button"]},
           
           {call, ?MODULE, run_action, [S, "page_help_home_a"]},
           {call, ?MODULE, run_action, [S, "page_help_find_project_a"]},
           {call, ?MODULE, run_action, [S, "page_help_project_overview_a"]},
           {call, ?MODULE, run_action, [S, "page_help_queued_projects_a"]},
           {call, ?MODULE, run_action, [S, "page_help_register_project_a"]},
           {call, ?MODULE, run_action, [S, "page_help_help_a"]}

           ]).


%% @doc Next state transformation, S is the current state. Returns next state.
-spec next_state(S :: eqc_statem:symbolic_state(), V :: eqc_statem:var(), 
                 C :: eqc_statem:call()) -> eqc_statem:symbolic_state().
next_state(S, _V, {call, ?MODULE, run_action, [_S, Action]}) ->
%    io:format("next_state: Current page=~p~n", [S#state.current_page]),
%    io:format("next_state: action performed=~p~n", [Action]),
    {_AId, NPage} = test:get_action_id_and_next_page(Action, S#state.pages_and_actions),
    S1 = S#state{current_page = NPage},
    io:format("next_state: current_page = ~p~n", [S1#state.current_page]),
    io:format("next_state: OK~n", []),
    S1;
next_state(S, _V, {call, _, _, _}) ->
    S.


%% @doc Precondition, checked before command is added to the command sequence. 
-spec precondition(S :: eqc_statem:symbolic_state(), C :: eqc_statem:call()) -> boolean().
precondition(S, {call, ?MODULE, run_action, [_S, Action]}) ->
    io:format("precondition: action to be performed=~p~n", [Action]),
    io:format("precondition: Current page=~p~n", [S#state.current_page]),
    io:format("precondition: get_page_from_action=~p~n", [test:get_page_from_action(Action, S#state.pages_and_actions)]),
    S#state.current_page == test:get_page_from_action(Action, S#state.pages_and_actions);
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
postcondition(S, {call, ?MODULE, run_action, [_S, Action]}, _Res) ->
    {_AId, NPage} = test:get_action_id_and_next_page(Action, S#state.pages_and_actions),
    io:format("postcondition: State current page =~p~n", [NPage]),
    io:format("postcondition: find_active_page=~p~n", [test:find_active_page()]), 
    case NPage == test:find_active_page() of
        true ->
            true;
        false ->
            clean(S)
     end;
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
    ?SETUP(fun() ->
                   setup(),
                   fun() ->
                           teardown() end end, 
           

           ?FORALL(Cmds, commands(?MODULE),
            begin
                {H, S, Res} = run_commands(?MODULE,Cmds),
                pretty_commands(?MODULE, Cmds, {H, S, Res},
                                Res == ok)
            end)).
    
setup() ->
    {ok, _Pid} = webdrv_session:start_session(?SESSION, ?CHROMEDRIVER,
                                                webdrv_cap:default_chrome(),
                                                10000),
    io:format("URL ~s~n", [?URL]),
    ok = webdrv_session:set_url(?SESSION, ?URL).


teardown() ->
    ok = webdrv_session:stop_session(test).
    

run_action(S, ActionName) ->
    io:format("run_action: going to run=~p~n", [ActionName]),
    {AId,_} = test:get_action_id_and_next_page(ActionName, S#state.pages_and_actions),
    Result = test:run_action(AId),
    io:format("run_action: runned action=~p~n", [ActionName]),
    io:format("run_action: result = ~p~n", [Result]).


clean(S) ->
    S#state{current_page = "",
            pages_and_actions = ""}.


get_pa() ->
    [{"page_main",
      [{"page_main_home_a","(//a)[2]","page_main"},
       {"page_main_find_project_a","(//a)[3]","page_project_search"},
       {"page_main_project_overview_a","(//a)[4]","page_project_overview"},
       {"page_main_queued_projects_a","(//a)[5]","page_queued_builds"},
       {"page_main_register_project_a","(//a)[6]","page_register_project"},
       {"page_main_help_a","(//a)[7]","page_help"},
       {"page_main_register_your_project_button","(//button)[5]",
        "page_register_project"},
       {"page_main_find_registered_project_button","(//button)[6]",
        "page_project_search"}]},
     {"page_project_search",
      [{"page_project_search_home_a","(//a)[2]","page_main"},
       {"page_project_search_find_project_a","(//a)[3]","page_project_search"},
       {"page_project_search_project_overview_a","(//a)[4]",
        "page_project_overview"},
       {"page_project_search_queued_projects_a","(//a)[5]",
        "page_queued_builds"},
       {"page_project_search_register_project_a","(//a)[6]",
        "page_register_project"},
       {"page_project_search_help_a","(//a)[7]","page_help"},
       {"page_project_search_search_button","(//button)[10]",
        "page_project_search"}]},
     {"page_project_overview",
      [{"page_project_overview_home_a","(//a)[2]","page_main"},
       {"page_project_overview_find_project_a","(//a)[3]",
        "page_project_search"},
       {"page_project_overview_project_overview_a","(//a)[4]",
        "page_project_overview"},
       {"page_project_overview_queued_projects_a","(//a)[5]",
        "page_queued_builds"},
       {"page_project_overview_register_project_a","(//a)[6]",
        "page_register_project"},
       {"page_project_overview_help_a","(//a)[7]","page_help"},
       {"page_project_overview_find_registered_project_button",
        "(//button)[7]","page_project_search"}]},
     {"page_queued_builds",
      [{"page_queued_builds_home_a","(//a)[2]","page_main"},
       {"page_queued_builds_find_project_a","(//a)[3]","page_project_search"},
       {"page_queued_builds_project_overview_a","(//a)[4]",
        "page_project_overview"},
       {"page_queued_builds_queued_projects_a","(//a)[5]",
        "page_queued_builds"},
       {"page_queued_builds_register_project_a","(//a)[6]",
        "page_register_project"},
       {"page_queued_builds_help_a","(//a)[7]","page_help"}]},
     {"page_register_project",
      [{"page_register_project_home_a","(//a)[2]","page_main"},
       {"page_register_project_find_project_a","(//a)[3]",
        "page_project_search"},
       {"page_register_project_project_overview_a","(//a)[4]",
        "page_project_overview"},
       {"page_register_project_queued_projects_a","(//a)[5]",
        "page_queued_builds"},
       {"page_register_project_register_project_a","(//a)[6]",
        "page_register_project"},
       {"page_register_project_help_a","(//a)[7]","page_help"},
       {"page_register_project_register_project_button","(//button)[9]",
        "page_register_project"}]},
     {"page_help",
      [{"page_help_home_a","(//a)[2]","page_main"},
       {"page_help_find_project_a","(//a)[3]","page_project_search"},
       {"page_help_project_overview_a","(//a)[4]","page_project_overview"},
       {"page_help_queued_projects_a","(//a)[5]","page_queued_builds"},
       {"page_help_register_project_a","(//a)[6]","page_register_project"},
       {"page_help_help_a","(//a)[7]","page_help"}]}].

get_pa2() ->
[{{"page_main","(//div)[16]"},
      [{"page_main_home_a","(//a)[2]","(//div)[16]"},
       {"page_main_find_project_a","(//a)[3]","(//div)[59]"},
       {"page_main_project_overview_a","(//a)[4]","(//div)[21]"},
       {"page_main_queued_projects_a","(//a)[5]","(//div)[37]"},
       {"page_main_register_project_a","(//a)[6]","(//div)[44]"},
       {"page_main_help_a","(//a)[7]","(//div)[56]"},
       {"page_main_register_your_project_button","(//button)[5]",
        "(//div)[44]"},
       {"page_main_find_registered_project_button","(//button)[6]",
        "(//div)[59]"}]},
     {{"page_project_search","(//div)[59]"},
      [{"page_project_search_home_a","(//a)[2]","(//div)[16]"},
       {"page_project_search_find_project_a","(//a)[3]","(//div)[59]"},
       {"page_project_search_project_overview_a","(//a)[4]","(//div)[21]"},
       {"page_project_search_queued_projects_a","(//a)[5]","(//div)[37]"},
       {"page_project_search_register_project_a","(//a)[6]","(//div)[44]"},
       {"page_project_search_help_a","(//a)[7]","(//div)[56]"},
       {"page_project_search_search_button","(//button)[10]","(//div)[59]"}]},
     {{"page_project_overview","(//div)[21]"},
      [{"page_project_overview_home_a","(//a)[2]","(//div)[16]"},
       {"page_project_overview_find_project_a","(//a)[3]","(//div)[59]"},
       {"page_project_overview_project_overview_a","(//a)[4]","(//div)[21]"},
       {"page_project_overview_queued_projects_a","(//a)[5]","(//div)[37]"},
       {"page_project_overview_register_project_a","(//a)[6]","(//div)[44]"},
       {"page_project_overview_help_a","(//a)[7]","(//div)[56]"},
       {"page_project_overview_find_registered_project_button",
        "(//button)[7]","(//div)[59]"}]},
     {{"page_queued_builds","(//div)[37]"},
      [{"page_queued_builds_home_a","(//a)[2]","(//div)[16]"},
       {"page_queued_builds_find_project_a","(//a)[3]","(//div)[59]"},
       {"page_queued_builds_project_overview_a","(//a)[4]","(//div)[21]"},
       {"page_queued_builds_queued_projects_a","(//a)[5]","(//div)[37]"},
       {"page_queued_builds_register_project_a","(//a)[6]","(//div)[44]"},
       {"page_queued_builds_help_a","(//a)[7]","(//div)[56]"}]},
     {{"page_register_project","(//div)[44]"},
      [{"page_register_project_home_a","(//a)[2]","(//div)[16]"},
       {"page_register_project_find_project_a","(//a)[3]","(//div)[59]"},
       {"page_register_project_project_overview_a","(//a)[4]","(//div)[21]"},
       {"page_register_project_queued_projects_a","(//a)[5]","(//div)[37]"},
       {"page_register_project_register_project_a","(//a)[6]","(//div)[44]"},
       {"page_register_project_help_a","(//a)[7]","(//div)[56]"},
       {"page_register_project_register_project_button","(//button)[9]",
        "(//div)[44]"}]},
     {{"page_help","(//div)[56]"},
      [{"page_help_home_a","(//a)[2]","(//div)[16]"},
       {"page_help_find_project_a","(//a)[3]","(//div)[59]"},
       {"page_help_project_overview_a","(//a)[4]","(//div)[21]"},
       {"page_help_queued_projects_a","(//a)[5]","(//div)[37]"},
       {"page_help_register_project_a","(//a)[6]","(//div)[44]"},
       {"page_help_help_a","(//a)[7]","(//div)[56]"}]}].
