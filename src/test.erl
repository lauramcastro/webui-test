-module(test).

-compile(export_all).

-include_lib("../webdrv/include/webdrv.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CHROMEDRIVER, "http://localhost:9515/").
-define(SESSION, test).
-define(URL, "http://udc.quickcheck-ci.com/").
   


test() ->
    {ok, _Pid} = webdrv_session:start_session(?SESSION, ?CHROMEDRIVER,
                                              webdrv_cap:default_chrome(),
                                              10000),
    io:format("URL ~s~n", [?URL]),
    ok = webdrv_session:set_url(?SESSION, ?URL),

    P = find_active_page(),
    As = get_actions(),
    State = loop(lists:map(fun(A) -> {[?URL], A} end, As), [{[?URL], P, As}]),
    io:format("~n"),
    print(State, []),

    webdrv_session:stop_session(test).

print_actions() ->
     {ok, _Pid} = webdrv_session:start_session(?SESSION, ?CHROMEDRIVER,
                                              webdrv_cap:default_chrome(),
                                              10000),
    io:format("URL ~s~n", [?URL]),
    ok = webdrv_session:set_url(?SESSION, ?URL),
    As = get_actions(),
    [get_text_action(A) || A <- As].

   
remove_invalid_actions(L) ->
    lists:foldr(fun({State,Actions}, Acc) ->
                        NewActions = lists:filter(fun({ActionId,_ActionName}) ->
                                                          ActionId /= [] end, Actions),
                        case NewActions of
                            [] ->
                                Acc;
                            _ ->
                                [{State, NewActions}|Acc]
                        end
                end, [], L).

                                                



print([], L) ->
    io:format("~p~n", [L]);
print([{Path, P, As}|S], L) ->
    case goto(Path) of
        ok ->
            {ok, E} = find_element_xpath(P),
            {ok, Id} = webdrv_session:element_attribute(?SESSION, E, "id"),
            Ts = lists:map(fun(A) ->
                                   {ok, E1} = find_element_xpath(A),
                                   {ok, Text} = webdrv_session:get_text(?SESSION, E1),
                                   Text
                           end, As),
            NewL = [{ {Id,P}, lists:zip(Ts,As) } | L],
            FilteredL = remove_invalid_actions(NewL),
            %io:format("~p -> ~p~n", [Id, Ts]),
            print(S, FilteredL);
        _ ->
            print(S, L)
    end.

loop([], State) ->
    State;
loop([{Path, A} | T], State) ->
    Path1 = Path ++ [A],
    case goto(Path1) of
        ok ->
            case find_active_page() of
                {error, _} ->
                    loop(T, State);
                P ->
                    {ok, E} = find_element_xpath(P),
                    {ok, Id} = webdrv_session:element_attribute(?SESSION, E, "id"),
                    case lists:keyfind(P, 2, State) of
                        false ->
                            io:format("New page ~p~n", [Id]),
                            As = get_actions(),
                            loop(T ++ lists:map(fun(A1) -> {Path1, A1} end, As), [{Path1, P, As} | State]);
                        _ ->
                            io:format("Old page ~p~n", [Id]),
                            loop(T, State)
                    end
            end;
        {error, _} ->
            loop(T, State)
    end.

%% Utils
find_active_page() ->
    Ps = find_elements_xpath("//div"),
    Ps1 = [P || P <- Ps,
                {ok, E} <- [find_element_xpath(P)],
                {ok, Cl} <- [webdrv_session:element_attribute(?SESSION, E,
                                                              "class")],
                string:str(Cl, "main")   > 0 andalso
                string:str(Cl, "page")   > 0 andalso
                string:str(Cl, "active") > 0
          ],
    case length(Ps1) of
        0 ->
            {error, no_active_page};
        1 ->
            hd(Ps1);
        _ ->
            {error, many_active_pages}
    end.

goto([Url|As]) ->
    io:format("~nGOTO ~p~n", [Url]),
    ok = webdrv_session:set_url(?SESSION, Url),
    run_actions(As).

run_actions([]) ->
    ok;
run_actions([A|As]) ->
    case run_action(A) of
        ok ->
            run_actions(As);
        {error, _} ->
            {error, cannot_run_actions}
    end.

get_text_action(A) ->
  case string:str(A, "(//a)") > 0 orelse
         string:str(A, "(//button)") > 0 of
        true ->
            case is_displayed(A) of
                true ->
                    case find_element_xpath(A) of
                        {ok, E} ->
                            {ok, Text} = webdrv_session:get_text(?SESSION, E),
                            Text;
                        _ ->
                            {error, element_not_found}
                    end;
                false ->
                    {error, element_not_displayed}
            end;
        false ->
            {error, not_an_action}
    end.
    

run_action(A) ->
    case string:str(A, "(//a)") > 0 orelse
         string:str(A, "(//button)") > 0 of
        true ->
            case is_displayed(A) of
                true ->
                    case find_element_xpath(A) of
                        {ok, E} ->
                            {ok, Text} = webdrv_session:get_text(?SESSION, E),
                            io:format("CLICK ~p~n", [Text]),
                            ok = webdrv_session:click_element(?SESSION, E);
                        _ ->
                            {error, element_not_found}
                    end;
                false ->
                    {error, element_not_displayed}
            end;
        false ->
            {error, not_an_action}
    end.

get_actions() ->
    L = get_links(),
    B = get_buttons(),
    L ++ B.

get_links() ->
    L = find_elements_xpath("//a"),
    [X || X <- L, is_displayed(X),
                  filter_url(X)].

get_buttons() ->
    L = find_elements_xpath("//button"),
    [X || X <- L, is_displayed(X)].

find_element_xpath(X) ->
    webdrv_session:find_element(?SESSION, "xpath", X).

find_elements_xpath(E) ->
    find_elements_xpath(E, 1).
find_elements_xpath(E, N) ->
    X = lists:flatten(io_lib:format("(~s)[~w]", [E, N])),
    case webdrv_session:find_element(?SESSION, "xpath", X) of
        {ok, _} ->
            [X | find_elements_xpath(E, N+1)];
        _ ->
            []
    end.

is_displayed(X) ->
    {ok, E} = find_element_xpath(X),
    {ok, R} = webdrv_session:is_displayed_element(?SESSION, E),
    R.

filter_url(X) ->
    {ok, E} = find_element_xpath(X),
    Link = webdrv_session:element_attribute(?SESSION, E, "href"),
    case Link of
        {ok, null} ->
            false;
        {ok, Url} ->
            case string:str(Url, "http") of
                0 ->
                    false;
                _ ->
                    filter_host(Url)
            end;
        _ ->
            false
    end.

filter_host(Url) ->
    {ok, Current} = webdrv_session:get_url(?SESSION),
    {ok, {_, _, HostC, _, _, _}} = http_uri:parse(Current),
    {ok, {_, _, HostU, _, _, _}} = http_uri:parse(Url),
    HostC == HostU.
