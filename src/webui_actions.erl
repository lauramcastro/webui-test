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
-callback get_links() -> {ok, [term()]}.
-callback get_buttons() -> {ok, [term()]}.
-callback get_inputs() -> {ok, [term()]}.

% exported for behaviour implementation
-export([setup/1, terminate/0,
	 find_element_by_id/1, set_element_value/2, activate_element/1,
	 find_elements_by_type/1, find_elements_by_type/2,
	 get_actions/0]).
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
%%      Generic action to find a given element by identifier.
%% @end
-spec find_element_by_id(Id :: string()) -> {ok, term()}.
find_element_by_id(Id) ->
    ?DEBUG("===> finding element by id ~p ", [Id]),
    {ok, Element} = webdrv_session:find_element(?SESSION, ?ID, Id),
    ?DEBUG("=> ~p~n", [Element]),
    {ok, Element}.

%% @doc
%%      Generic action to find elements by type.
%% @end
-spec find_elements_by_type(Type :: string()) -> {ok, term()}.
find_elements_by_type(Type) ->
    find_elements_by_type(Type, []).

%% @doc
%%      Generic action to find elements using by type.
%%      A list of options to apply when performing the search can be supplied.
%% @end
-spec find_elements_by_type(Type :: string(),
			   Options :: [atom()]) -> {ok, [term()]}.
find_elements_by_type(Type, Options) ->
    ?DEBUG("===> finding elements by type ~p ", [Type]),
    {ok, Elements} = webdrv_session:find_elements(?SESSION, ?XPATH, Type),
    ?DEBUG("=> filtering ~p with ~p ", [length(Elements), Options]),
    Result = filter(Elements, Options),
    ?DEBUG("=> ~p~n", [length(Result)]),
    {ok, Result}.

%% @doc
%%      Generic action to set an element to a given value.
%% @end
-spec set_element_value(E :: string(),
			V :: string()) -> ok.
set_element_value(E, V) ->
    ?DEBUG("===> setting ~p to ~p~n", [E, V]),
    ?WEBDRV_SESSION(send_value(?SESSION, E, V)).

%% @doc
%%      Generic action to interact with an element (i.e., click a button).
%% @end
-spec activate_element(E :: string) -> ok.
activate_element(E) ->
    ?DEBUG("===> activating ~p ", [E]),
    Result = ?WEBDRV_SESSION(click_element(?SESSION, E)),
    ?DEBUG("=> (~p) and goes to ~p~n", [Result,webdrv_session:get_url(?SESSION)]),
    Result.

%% @doc
%%      Generic action to find the interaction possibilities on the current
%%      active page.
%% @end
-spec get_actions() -> {ok, [term()]}.
get_actions() ->
    {ok, L} = get_links(),
    {ok, B} = get_buttons(),
    {ok, I} = get_inputs(),
    % TODO: consider browser buttons: reload, back, forward
    {ok, [{link, Link}     || Link <- L] ++
	 [{button, Button} || Button <- B] ++
	 [{input, Input}   || Input <- I]}.

%% @private
get_links() ->
    gen_server:call(?ACTIONSERVER, get_links).

%% @private
get_buttons() ->
    gen_server:call(?ACTIONSERVER, get_buttons).

%% @private
get_inputs() ->
    gen_server:call(?ACTIONSERVER, get_inputs).

%%
%% gen_server callbacks implementation
%%

%% @private
init([Mod]) ->
    {ok, [Mod]}.

%% @private
handle_call(get_links, _From, [Mod]) ->
    Reply = Mod:get_links(),
    {reply, Reply, [Mod]};
handle_call(get_buttons, _From, [Mod]) ->
    Reply = Mod:get_buttons(),
    {reply, Reply, [Mod]};
handle_call(get_inputs, _From, [Mod]) ->
    Reply = Mod:get_inputs(),
    {reply, Reply, [Mod]};
handle_call(_Request, _From, State) ->
    {noreply, State}.

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
