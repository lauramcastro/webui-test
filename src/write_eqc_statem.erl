%%% @author Macías López <macias.lopez@udc.es>
%%% @copyright (C) 2015, Macías López
%%% @doc
%%% Generate an EQC machine for testing web UI
%%% @end
%%% Created : 30 Jan 2015

-module(write_eqc_statem).

-compile(export_all).


test(PA) ->
    write_eqc_statem(
      "http://localhost:9515/",
      "test",
      "http://udc.quickcheck-ci.com/",
      PA,
      "test_eqc.erl").


write_eqc_statem(URLChromeDriver, WebUI, URLTest, PagesActions, OutFile) ->
    Heading = gen_heading(URLChromeDriver, WebUI, URLTest, OutFile),
    Commands = gen_commands(WebUI, PagesActions),
    Pre = gen_preconditions(WebUI),
    NextState = gen_next_state(WebUI),
    Post = gen_postconditions(WebUI),
    Property = gen_property(),
    WrapperFuns = gen_wrapper_funs(WebUI),
    UtilFuns = gen_util_funs(WebUI, PagesActions),
    Content = Heading ++ Commands ++ Pre ++ 
        NextState ++ Post ++ Property ++ WrapperFuns ++ UtilFuns,
  %  io:format("Content=~p~n",[Content]).
    file:write_file("src/"++OutFile, list_to_binary(Content)).

gen_heading(URLChromeDriver, WebUI, URLTest, OutFile) ->
    BaseName = filename:basename(OutFile, ".erl"),
    "-module("++BaseName++").\n\n"
        "-include_lib(\"eqc/include/eqc.hrl\").\n"
        "-include_lib(\"eqc/include/eqc_statem.hrl\").\n\n"
        "-include_lib(\"../webdrv/include/webdrv.hrl\").\n\n"
        "-include_lib(\"eunit/include/eunit.hrl\").\n"
        "-define(CHROMEDRIVER, \""++URLChromeDriver++"/\").\n"
        "-define(SESSION, "++WebUI++").\n"
        "-define(URL, \""++URLTest++"/\").\n\n"
        
        "-compile(export_all).\n\n"  
        "-record(state, {current_page,\n"
        "       pages_and_actions}).\n\n\n"
     
        "%%---------------------------------------------------------\n"
        "%% initial_state\n"
        "%%---------------------------------------------------------\n"
        "initial_state()->\n"
        "   #state{current_page = "++WebUI++":find_active_page(),\n"
        "          pages_and_actions = get_pa()}.\n\n".
        
gen_commands(WebUI, PA)->
    Cmds=gen_commands_list(WebUI, PA),
    "\n\n"
    "%%----------------------------------------------------------\n"
    "%% command\n"
    "%%----------------------------------------------------------\n"
    "command(S)->\n"
    "    oneof([\n"
        ++Cmds++
    "      ]).\n\n\n".    

gen_commands_list(WebUI, PA) ->
    AtomUI = list_to_atom(WebUI),
    Actions = AtomUI:get_all_actions(PA),
    Last = lists:last(Actions),
    lists:map(fun(A) ->
                      "       {call, ?MODULE, run_action, [S, \""++A++"\"]},\n"
                          end, Actions) ++ 
        "{call, ?MODULE, run_action, [S, \"" ++ Last ++ "\"]}\n".


gen_preconditions(WebUI) ->
     "\n\n"
    "%%----------------------------------------------------------\n"
    "%% precondition\n"
    "%%----------------------------------------------------------\n"
    "precondition(S, {call, ?MODULE, run_action, [_S, Action]}) ->\n"
    "  S#state.current_page == "++WebUI++":get_page_from_action(Action, S#state.pages_and_actions);\n"
    "precondition(_S, {call, _, _, _}) ->\n"
    "  true.\n\n\n".

gen_next_state(WebUI) ->
     "\n\n"
    "%%----------------------------------------------------------\n"
    "%% next_state\n"
    "%%----------------------------------------------------------\n"
    "next_state(S, _V, {call, ?MODULE, run_action, [_S, Action]}) ->\n"
    "  {_AId, NPage} = "++WebUI++":get_action_id_and_next_page(Action, S#state.pages_and_actions),\n"
    "  S#state{current_page = NPage};\n"
    "next_state(S, _V, {call, _, _, _}) ->\n"
    "S.\n\n\n".

gen_postconditions(WebUI) ->
      "\n\n"
    "%%----------------------------------------------------------\n"
    "%% postcondition\n"
    "%%----------------------------------------------------------\n"
    "postcondition(S, {call, ?MODULE, run_action, [_S, Action]}, _Res) ->\n"
    "  {_AId, NPage} = "++WebUI++":get_action_id_and_next_page(Action, S#state.pages_and_actions),\n"
    "  case NPage == test:find_active_page() of\n"
    "    true ->\n"
    "        true;\n"
    "    false ->\n"
    "        clean(S)\n"
    "  end;\n"
    "postcondition(_S, {call, _, _, _}, _Res) ->\n"
    "  true.\n\n\n".

gen_property() ->
    "%%========================================================\n"
        "%% Prop\n"
        "%%========================================================\n"
        "prop_ui() ->\n"
        "    ?SETUP(\n"
        "      fun() -> setup(),\n"
        "      fun() -> teardown() end end,\n"
        "      ?FORALL(\n"
        "         Cmds, commands(?MODULE),\n"
        "         begin\n"
        "           {H, S, Res} = run_commands(?MODULE, Cmds),\n"
        "           pretty_commands(?MODULE, Cmds, {H, S, Res},\n"
        "                           Res==ok)\n"
        "         end)).\n\n".



gen_wrapper_funs(WebUI)->
    "%%---------------------------------------------------------------\n"
    "%% Wrapper functions\n"
    "%%---------------------------------------------------------------\n"
    "run_action(S, ActionName) ->\n"
    "{AId,_} = "++WebUI++":get_action_id_and_next_page(ActionName, S#state.pages_and_actions),\n"++
    WebUI ++":run_action(AId).\n\n".


gen_util_funs(WebUI, PagesActions) ->
    lists:flatten(
    "%%---------------------------------------------------------------\n"
    "%% Utilities\n"
    "%%---------------------------------------------------------------\n"
    "setup() -> \n"
    "  {ok, _Pid} = webdrv_session:start_session(?SESSION, ?CHROMEDRIVER,\n"
    "                                            webdrv_cap:default_chrome(),\n"
    "                                            10000),\n"
    "  io:format(\"URL ~s~n\", [?URL]),\n"
    "  ok = webdrv_session:set_url(?SESSION, ?URL).\n\n"
    "teardown() ->\n"
    "   ok = webdrv_session:stop_session("++ WebUI ++ ").\n\n"
    "clean(S) ->\n"
    "  S#state{current_page = \"\",\n"
    "        pages_and_actions = \"\"}.\n\n"
    "get_pa() ->\n"
    "  " ++ io_lib:format("~p", [PagesActions]) ++ ".\n").




                                        

