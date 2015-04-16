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
    io:format("~p~n", [State]),

    L = print(State,[]),

    webdrv_session:stop_session(test),
%    io:format("Before printing~n", []),
%    io:format("L = ~p~n", [L]),
    L.

stop() ->
    webdrv_session:stop_session(test).


%%%%%
%% Util functions to EQC machine
%%%%
get_pages_and_actions() ->
    test().
    
get_page_from_action(Action, PA) ->
    case PA of
        [] ->
            [];
        [H|T] ->
            case explore_actions(Action, H) of
                [] ->
                    get_page_from_action(Action,T);
                P ->
                    P
            end
    end.


explore_actions(_A, {_PName, []}) ->
    [];
explore_actions(A, {PName, Actions}) ->
    case Actions of
        [] ->
            [];
        [{AName,_AId, _NPage}|T] ->
            case AName == A of
                true ->
                    PName;
                _ ->
                    explore_actions(A, {PName, T})
            end
    end.
                     
    


get_action_id_and_next_page(ActionName, PA) ->
    case PA of
        [] ->
            [];
        [{_Page, Actions}|T] ->
            ExploredActions = lists:map(fun({AName, AId, NPage}) ->
                                                case AName == ActionName of
                                                    true ->
                                                        {AId, NPage};
                                                    _ ->
                                                        []
                                                end end, Actions),
            case lists:flatten(ExploredActions) of
                [] ->
                    get_action_id_and_next_page(ActionName,T);
                [Some] ->
                    Some;
                Any ->
                    {error, repeated_pages, Any}
            end
    end.

get_all_actions(PA) ->
    case PA of
        [] ->
            [];
        [{_Page,Actions}|T] ->
            
              lists:map(fun({AName,_AId,_Npage}) ->
                                 AName end,Actions)  ++ get_all_actions(T)
    end.

    

%%%%%%%%%%%%%%%%%%%%%%

rename_all_actions([]) ->
    [];
rename_all_actions([{PName, Actions}|T]) ->
    [{PName, rename_actions_from_page(PName,Actions)} | rename_all_actions(T)].


rename_actions_from_page(PName, Actions) ->
    lists:map(fun({AName,AId, PNext}) ->
                    %  io:format("AId=~p~n", [AId]),
                      {create_action_name(PName, AName, AId), AId, PNext} end, Actions).
    
create_action_name(PName, AName, AId) ->
    Suffix = case string:str(AId, "(//a)")>0 of
                  true -> "a";
                  _ -> "button"
              end,
    PName ++ "_" ++ string:to_lower(re:replace(AName, " ", "_", [global, {return, list}])) ++ "_" ++ Suffix.




remove_invalid_actions(L) ->
    lists:foldr(fun({PName,Actions}, Acc) ->
                        NewActions = lists:filter(fun({ActionId,_ActionName,_}) ->
                                                          ActionId /= [] end, Actions),
                        case NewActions of
                            [] ->
                                Acc;
                            _ ->
                                [{PName, NewActions}|Acc]
                        end
                end, [], L).

                                                



print([], L) ->
    rename_all_actions(L);
print([{Path, Id, As}|S], L) ->
    case goto(Path) of
        ok ->
          %  {ok, E} = find_element_xpath(P),
          %  {ok, PName} = webdrv_session:element_attribute(?SESSION, E, "id"),
            NewAs = lists:filter(fun(A) ->
                                         not is_list(A) end, As),
            Ts = lists:map(fun({A,_P}) ->
                                   {ok, E1} = find_element_xpath(A),
                                   {ok, Text} = webdrv_session:get_text(?SESSION, E1),
                                   Text
                           end, NewAs),
            Ps =  lists:map(fun({_A,Page}) ->
                                    Page                                  
                            end, NewAs),

            As2 = lists:map(fun({A, _P}) ->
                                    A
                            end, NewAs),
            NewL = [{ Id, lists:zip3(Ts,As2,Ps) } | L],
            FilteredL = remove_invalid_actions(NewL),                                             
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
                Id ->
                    %{ok, E} = find_element_xpath(P),
                    %{ok, Id} = webdrv_session:element_attribute(?SESSION, E, "id"),
                    NewState = add_action_page(Path, A, Id, State),
                    case lists:keyfind(Id, 2, State) of
                        false ->
                            io:format("New page ~p~n", [Id]),
                            As = get_actions(),
                            loop(T ++ lists:map(fun(A1) -> {Path1, A1} end, As), [{Path1, Id, As} | NewState]);
                        _ ->
                            io:format("Old page ~p~n", [Id]),
                            loop(T, NewState)
                    end
            end;
        {error, _} ->
            loop(T, State)
    end.


add_action_page(Path, A, P, State) ->
    case lists:keyfind(Path, 1, State) of
        {PathF, PF, As} ->
            NewAs = lists:foldr(fun(X,Acc) ->
                                        case X == A of
                                            true -> [{A,P}|Acc];
                                            _ -> [X|Acc]
                                        end end, [], As),
            NewState = lists:keyreplace(Path, 1, State, {PathF, PF, NewAs}),
            NewState;
        
        false ->
            State
    end.

%% Utils
find_active_page() ->
    Ps = find_elements_xpath("//div"),
    Ps1 = [Id || P <- Ps,
                 {ok, E} <- [find_element_xpath(P)],
                 {ok, Id} <- [webdrv_session:element_attribute(?SESSION, E, "id")],

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
         string:str(A, "(//button)") > 0 orelse
         string:str(A, "(//input)") > 0 of
        true ->
            case is_displayed(A) of
                true ->
                    case find_element_xpath(A) of
                        {ok, E} ->
                            Text = get_text_xpath(A),
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

get_text_xpath(X) ->
    case find_element_xpath(X) of
        {ok, E} ->
            case string:str(X, "(//a)") > 0 orelse
                 string:str(X, "(//button)") > 0 of
                true ->
                    {ok, Text} = webdrv_session:get_text(?SESSION, E);
                _ ->
                    case string:str(X, "(//input)") > 0 of
                        true ->
                            {ok, Text} = webdrv_session:element_attribute(?SESSION, E, "value");
                        _ ->
                            Text = ""
                    end
            end;
        _ ->
            Text = ""
    end,
    Text.

get_actions() ->
    L = get_links(),
    B = get_buttons(),
    I = get_inputs(),
    L ++ B ++ I.

get_links() ->
    L = find_elements_xpath("//a"),
    [X || X <- L, is_displayed(X),
                  filter_url(X)].

get_buttons() ->
    L = find_elements_xpath("//button"),
    [X || X <- L, is_displayed(X)].

get_inputs() ->
    L = find_elements_xpath("//input"),
    [X || X <- L, is_displayed(X), is_type_submit(X)].

find_element_id(X) ->
    webdrv_session:find_element(?SESSION, "id", X).

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
    case find_element_xpath(X) of
        {ok, E} ->
            {ok, R} = webdrv_session:is_displayed_element(?SESSION, E),
            R;
        {error, _} ->
            false
    end.

is_type_submit(X) ->
    case find_element_xpath(X) of
        {ok, E} ->
            {ok, R} = webdrv_session:element_attribute(?SESSION, E, "type"),
            R == "submit";
        {error, _} ->
            false
    end.

filter_url(X) ->
    {ok, E} = find_element_xpath(X),
    Link = webdrv_session:element_attribute(?SESSION, E, "href"),
    case Link of
        {ok, null} ->
            false;
        {ok, Url} ->
            case string:str(Url, "mailto") of
                0 ->
                    case string:str(Url, "http") of
                        0 ->
                            true;
                        _ ->
                            filter_host(Url)
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.

filter_host(Url) ->
    {ok, Current} = webdrv_session:get_url(?SESSION),
    {ok, {_, _, HostC, _, _, _}} = http_uri:parse(Current),
    {ok, {_, _, HostU, _, _, _}} = http_uri:parse(Url),
    HostC == HostU.
