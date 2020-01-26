%%%-------------------------------------------------------------------
%%% @author danny
%%% @copyright (C) 2018, danny
%%% @doc
%%%
%%% @end
%%% Created : 2018-07-17 09:45:03.637624
%%%-------------------------------------------------------------------
-module(bifrost_web).

-behaviour(gen_server).

%% API
-export([start_link/0
        ,add_route/3
        ,remove_route/1]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_route(Path, Module, Init) ->
  gen_server:cast(?MODULE, {add_route, Path, Module, Init}).

remove_route(Path) ->
  gen_server:cast(?MODULE, {remove_route, Path}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  Routes = get_routes(),
  apply_dispatch(Routes),
  PortSpec = case application:get_env(port) of
               undefined -> [];
               {ok, Port} -> [{port, Port}]
             end,
  {ok, _} = cowboy:start_clear(bifrost, PortSpec,
                               #{env => #{dispatch => {persistent_term, bifrost_dispatch}}}),
  PortFromRanch = ranch:get_port(bifrost),
  lager:info("Bifrost opened on port : ~p", [{PortFromRanch}]),
  {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_route, Path, Module, Init}, DynamicRoutes) ->
  DynamicRoutesUpdate = lists:keystore(Path, 1, DynamicRoutes, {Path, Module, Init}),
  Routes = get_routes(DynamicRoutesUpdate),
  apply_dispatch(Routes),
  {noreply, DynamicRoutesUpdate};
handle_cast({remove_route, Path}, DynamicRoutes) ->
  DynamicRoutesUpdate = lists:keydelete(Path, 1, DynamicRoutes),
  Routes = get_routes(DynamicRoutesUpdate),
  apply_dispatch(Routes),
  {noreply, DynamicRoutesUpdate};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_routes() ->
  get_routes([]).

get_routes(DynamicRoutes) ->
  IndexFileSpec = case application:get_env(index_file_path) of
                    undefined ->
                      {priv_file, bifrost, "index.html"};
                    {ok, IndexFilePath} ->
                      {file, IndexFilePath}
                  end,
  StaticDirSpec = case application:get_env(static_dir_path) of
                    undefined ->
                      {priv_dir, bifrost, ""};
                    {ok, StaticDirPath} ->
                      {dir, StaticDirPath}
                  end,
  DynamicRoutesAligned = lists:reverse(
                              lists:keysort(1, DynamicRoutes)
                             ),
  [{"/", cowboy_static, IndexFileSpec},
   {"/sock", bifrost_sock, []} ]
  ++ DynamicRoutesAligned
  ++ [{"/[...]", cowboy_static, StaticDirSpec}].

apply_dispatch(Routes) ->
  Dispatch = cowboy_router:compile([{'_', Routes }]),
  persistent_term:put(bifrost_dispatch, Dispatch).
