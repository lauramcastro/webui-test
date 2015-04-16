-module(admingui).

-include_lib("../webdrv/include/webdrv.hrl").

-define(SESSION, test).

-compile(export_all).

-record(st, {active_page}).

initial_state() ->
    #st{active_page = "loginForm"}.

next_state(_S, _A) ->
    % new active page
    P = find_active_page(),
    io:format("[ST] new active page ~p~n", [P]),

    #st{active_page = P}.

precondition(S, _A) ->
    P = S#st.active_page,
    io:format("[PRE] active page ~p~n", [P]),
    assertEquals(P, find_active_page()),
    case P of
        "loginForm" ->
            {User, Password} = {"vodkatv", "vodkatv"},
            {ok, U} = test:find_element_id("loginName"),
            webdrv_session:send_value(?SESSION, U, User),
            {ok, Pass} = test:find_element_id("password"),
            webdrv_session:send_value(?SESSION, Pass, Password);
        _ ->
            io:format("precondition not implemented for ~p~n", [P]),
            error(precondition)
    end,
    true.

postcondition(S, _A) ->
    P = S#st.active_page,
    %P1 = find_active_page(),
    io:format("[POS] active page ~p~n", [P]),

    %P == P1.
    true.

find_active_page() ->
    % login form
    Ps0 = case test:find_element_id("loginForm") of
              {ok, _} ->
                  ["loginForm"];
              _ ->
                  []
          end,
    Ps = Ps0,
    case length(Ps) of
        0 ->
            {error, no_active_page};
        1 ->
            hd(Ps);
        _ ->
            {error, many_active_pages}
    end.

%% Utils

assertEquals(Var, Value) ->
    Var = Value.
