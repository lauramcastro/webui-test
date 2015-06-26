%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc
%%%      Generic actions for web-base GUIs browsing, implemented as a behaviour.
%%% @end

-module(webui_actions).
-behaviour(gen_server).

-callback setup() -> ok.
-callback terminate() -> ok.

% exported for behaviour implementation
-export([setup/1, terminate/0, find_active_page/0,
	 find_element_by_id/1, set_element_value/2, activate_element/1,
	 find_elements_by_type/1, find_elements_by_type/2]).
% exported to comply with gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("webui.hrl").

-define(ACTIONSERVER, ?MODULE).

%% @doc
%%      Actions are handled by a server (implemented as a gen_server).
%%      This is the function to start the server.
%% @end
-spec setup(Mod :: atom()) -> ok.
setup(Mod) ->
    {ok, _Pid} = gen_server:start_link({local, ?ACTIONSERVER}, ?MODULE, [Mod], []), % start without link?
    ok.

%% @doc
%%      Actions are handled by a server (implemented as a gen_server).
%%      This is the function to stop the server.
%% @end
-spec terminate() -> ok.
terminate() ->
    gen_server:cast(?ACTIONSERVER, stop).

%% @doc
%%      Generic action to find the active page.
%% @end
-spec find_active_page() -> term().
find_active_page() ->
    ?DEBUG("===> finding active page ", []),
    todo.

%% @doc
%%      Generic action to find a given element by identifier.
%% @end
-spec find_element_by_id(Id :: string()) -> term().
find_element_by_id(Id) ->
    ?DEBUG("===> finding element by id ~p ", [Id]),
    webdrv_session:find_element(?SESSION, ?ID, Id).

%% @doc
%%      Generic action to find elements using XPath types.
%% @end
-spec find_elements_by_type(Type :: string()) -> term().
find_elements_by_type(Type) ->
    find_elements_by_type(Type, []).

%% @doc
%%      Generic action to find elements using XPath types.
%%      A list of options to apply when performing the search can be supplied.
%% @end
-spec find_elements_by_type(Type :: string(),
			   Options :: [atom()]) -> term().
find_elements_by_type(Type, Options) ->
    ?DEBUG("===> finding element by type ~p ", [Type]),
    {ok, Elements} = webdrv_session:find_elements(?SESSION, ?XPATH, Type),
    {ok, filter(Elements, Options)}.

%% @doc
%%      Generic action to set an element to a given value.
%% @end
-spec set_element_value(E :: string(),
			V :: string()) -> term().
set_element_value(E, V) ->
    ?DEBUG("===> setting ~p to ~p~n", [E, V]),
    ?WEBDRV_SESSION(send_value(?SESSION, E, V)).

%% @doc
%%      Generic action to interact with an element (i.e., click a button).
%% @end
-spec activate_element(E :: string) -> term().
activate_element(E) ->
    ?DEBUG("===> activating ~p~n", [E]),
    ?WEBDRV_SESSION(click_element(?SESSION, E)).

%%
%% gen_server callbacks implementation
%%

%% @private
init([Mod]) ->
    {ok, [Mod]}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal stuff
%%

filter(Elements, []) ->
    Elements;
filter(Elements, [visible | Options]) ->
    FilteredElements =
	lists:filter(fun(E) ->
			     erlang:element(2,webdrv_session:is_displayed_element(?SESSION, E))
		     end,
		     Elements),
    filter(FilteredElements, Options);
filter(Elements, [submit | Options]) ->
    FilteredElements =
	lists:filter(fun(E) ->
			     {ok, ?SUBMIT} == webdrv_session:element_attribute(?SESSION, E, ?TYPE)
		     end,
		     Elements),
    filter(FilteredElements, Options);
filter(Elements, [_Option | Options]) ->
    filter(Elements, Options).
