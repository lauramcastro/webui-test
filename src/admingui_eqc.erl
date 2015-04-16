-module(admingui_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
%-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-include_lib("../webdrv/include/webdrv.hrl").

-define(CHROMEDRIVER, "http://localhost:9515/").
-define(SESSION, test).
-define(URL, "http://193.144.63.20:8082/admin/new-admin/app/").
%-define(URL, "http://193.144.63.20:8082/admin"). % old AdminGUI

-record(state, {st, links}).

-compile(export_all).

run() ->
    eqc:quickcheck(prop_ui()).

api_spec() ->
    #api_spec{}.

initial_state() ->
    As = test:get_actions(),
    io:format("~p~n", [As]),
    #state{st = admingui:initial_state(), links = As}.

click_args(#state{links = Links}) ->
    [eqc_gen:oneof(Links)].
click(A) ->
    test:run_action(A),
    test:get_actions().
click_pre(#state{st = St}, [A]) ->
    admingui:precondition(St, A),
    true.
click_post(#state{st = St}, [A], _Res) ->
    admingui:postcondition(St, A).
click_next(#state{st = S}, Links, [A]) ->
    St = admingui:next_state(S, A),
    #state{st = St, links = Links}.

prop_ui() ->
    ?SETUP(fun() -> setup(), fun() -> teardown() end end,
    ?FORALL(Cmds, dynamic_commands(?MODULE),
    ?CHECK_COMMANDS(HSR={_History, _FinalState, Result}, ?MODULE, Cmds,
    pretty_commands(?MODULE, Cmds, HSR,
        Result == ok)))).

setup() ->
    {ok, _Pid} = webdrv_session:start_session(?SESSION, ?CHROMEDRIVER,
                                                webdrv_cap:default_chrome(),
                                                10000),
    io:format("URL ~s~n", [?URL]),
    ok = webdrv_session:set_url(?SESSION, ?URL).

teardown() ->
    ok = webdrv_session:stop_session(test).
