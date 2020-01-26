%%%-------------------------------------------------------------------
%%% @author Chaitanya Chalasani
%%% @copyright (C) 2020, Access Europe GmbH
%%% @doc
%%%
%%% @end
%%% Created : 2020-01-21 13:22:17.378474
%%%-------------------------------------------------------------------
-module(bifrost_api).

-behaviour(gen_server).

-callback init(Args :: list()) ->
  {'ok', Routes :: list()} | {'ok', Route :: map()} | {'stop', Reason :: term()}.
-callback handle_options(Headers :: map()
                         ,ReqParams :: map()
                         ,PathInfo :: 'undefined' | binary()
                         ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_get(Headers :: map()
                     ,ReqParams :: map()
                     ,PathInfo :: 'undefined' | binary()
                     ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_head(Headers :: map()
                     ,ReqParams :: map()
                     ,PathInfo :: 'undefined' | binary()
                     ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_post(Headers :: map()
                     ,ReqParams :: map()
                     ,PathInfo :: 'undefined' | binary()
                     ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_put(Headers :: map()
                     ,ReqParams :: map()
                     ,PathInfo :: 'undefined' | binary()
                     ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_delete(Headers :: map()
                        ,ReqParams :: map()
                        ,PathInfo :: 'undefined' | binary()
                        ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_trace(Headers :: map()
                        ,ReqParams :: map()
                        ,PathInfo :: 'undefined' | binary()
                        ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_connect(Headers :: map()
                         ,ReqParams :: map()
                         ,PathInfo :: 'undefined' | binary()
                         ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_patch(Headers :: map()
                       ,ReqParams :: map()
                       ,PathInfo :: 'undefined' | binary()
                       ,State :: map()) ->
  'ok' | {'ok', Reply :: map()} | 'error' | {'error', Reason :: map() | atom() }.
-callback handle_info(Info :: term()) -> 'ok'.

-optional_callbacks([handle_options/4
                    ,handle_get/4
                    ,handle_head/4
                    ,handle_post/4
                    ,handle_put/4
                    ,handle_delete/4
                    ,handle_trace/4
                    ,handle_connect/4
                    ,handle_patch/4
                    ,handle_info/1]).

%% API
-export([start_link/4
        ,initialize/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% cowboy callbacks
-export([init/2]).

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
start_link(Registration, Callback, Arguments, Options) ->
  gen_server:start_link(Registration, ?MODULE, [Callback|Arguments], Options).

initialize(Server) ->
  gen_server:cast(Server, initialize).

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
% Init for gen_server behaviour
init([Callback|Arguments]) ->
  process_flag(trap_exit, true),
  case Callback:init(Arguments) of
    {ok, Routes} when is_list(Routes) ->
      [ publish_route(Route, Callback) || Route <- Routes ],
      {ok, #{callback => Callback, init_arguments => Arguments, routes => Routes}};
    {ok, Route} ->
      publish_route(Route, Callback),
      {ok, #{callback => Callback, init_arguments => Arguments, routes => [Route]}};
    Other ->
      Other
  end.

% Init for cowboy behaviour
init(Req, State) ->
  ReqFinalState = handle_api(Req, State),
  {ok, ReqFinalState, State}.

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
handle_cast(initialize, #{callback := Callback
                          ,init_arguments := Arguments
                          ,routes := Routes} = State) ->
  [ unpublish_route(Route) || Route <- Routes ],
  case init([Callback|Arguments]) of
    {ok, NewState} ->
      {noreply, NewState};
    {stop, Reason} ->
      {stop, Reason, State}
  end;
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
handle_info(Info, #{callback := Callback} = State) ->
  case erlang:function_exported(Callback, handle_info, 1) of
    true -> Callback:handle_info(Info);
    false -> ok
  end,
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
terminate(_Reason, #{routes := Routes}) ->
  [ unpublish_route(Route) || Route <- Routes ],
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

publish_route(#{path := Path} = Route, Callback) ->
  Module = maps:get(module, Route, Callback),
  Init = maps:get(init, Route, []),
  AuthFlag = maps:get(auth, Route, application:get_env(bifrost, auth_default, true)),
  AuthFun = maps:get(auth_fun, Route, application:get_env(auth_fun)),
  bifrost_web:add_route(Path, ?MODULE
                        ,#{module => Module, auth => AuthFlag, auth_fun => AuthFun, state => Init}).

unpublish_route(#{path := Path}) ->
  bifrost_web:remove_route(Path).

handle_api(#{method := Method, path_info := PathInfo} = Req, State) ->
  case is_authenticated(Req, State) of
    {ok, HeadersU} ->
      Bindings = cowboy_req:bindings(Req),
      QueryStringMap = try_atomify_keys( maps:from_list( cowboy_req:parse_qs(Req) )),
      QueryWithBindings = maps:merge(QueryStringMap, Bindings),
      {ReqParams, Req1} = case maps:get(has_body, Req) of
                            false ->
                              { QueryWithBindings, Req };
                            true ->
                              {ok, HttpBodyJson, ReqU} = cowboy_req:read_body(Req),
                              case catch json_decode(HttpBodyJson) of
                                {'EXIT', Reason} ->
                                  lager:error("Body decode failed ~p", [Reason]),
                                  { QueryWithBindings, ReqU };
                                HttpBodyList when is_list(HttpBodyList) ->
                                  lager:info("Body is list ~p", [HttpBodyList]),
                                  { QueryWithBindings#{<<"_body">> => HttpBodyList}, ReqU };
                                HttpBodyMap ->
                                  lager:info("Body is map ~p", [HttpBodyMap]),
                                  { maps:merge(HttpBodyMap, QueryWithBindings), ReqU }
                              end
                          end,
      handle_api(Method, PathInfo, ReqParams, HeadersU, Req1, State);
    false ->
      cowboy_req:reply(401, #{}, [], Req)
  end.

is_authenticated(#{headers := Headers}, #{auth := false}) ->
  {ok, Headers};
is_authenticated(_Req, #{auth_fun := undefined}) ->
  false;
is_authenticated(#{headers := Headers} = Req, #{auth_fun := AuthFun}) ->
  Authorization = cowboy_req:parse_header(<<"authorization">>, Req),
  HeadersWithoutAuthorization = maps:remove(<<"authorization">>, Headers),
  case invoke_auth_fun(AuthFun, Authorization) of
    ok ->
      {ok, HeadersWithoutAuthorization};
    {ok, AuthIdentities} when is_map(AuthIdentities) ->
      {ok, map:merge(HeadersWithoutAuthorization, AuthIdentities)};
    {ok, AuthIdentities} ->
      {ok, HeadersWithoutAuthorization#{<<"identity">> => AuthIdentities}};
    false ->
      false
  end.

invoke_auth_fun({Module, Function}, Authorization) ->
  apply(Module, Function, [Authorization]);
invoke_auth_fun(AuthFun, Authorization) when is_function(AuthFun, 1) ->
  apply(AuthFun, [Authorization]).

handle_api(Method, PathInfo, ReqParams, Headers, Req, #{module := Callback, state := State}) ->
  Function = get_callback_function(Method),
  case erlang:function_exported(Callback, Function, 4) of
    true ->
      case Callback:Function(Headers, ReqParams, PathInfo, State) of
        ok ->
          cowboy_req:reply(204, #{}, [], Req);
        {ok, Reply} ->
          ReplyJson = json_encode(Reply),
          cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ReplyJson, Req);
        {ok, Reply, ReplyHeaders} ->
          ReplyJson = json_encode(Reply),
          cowboy_req:reply(200, ReplyHeaders#{<<"content-type">> => <<"application/json">>}, ReplyJson, Req);
        error ->
          cowboy_req:reply(400, #{}, [], Req);
        {error, Reason} ->
          ReplyJson = json_encode(#{reason => Reason}),
          cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ReplyJson, Req);
        {Code, Reply, ReplyHeaders} ->
          cowboy_req:reply(Code, ReplyHeaders, Reply, Req)
      end;
    false ->
      cowboy_req:reply(405, #{}, [], Req)
  end.

get_callback_function(<<"OPTIONS">>) -> handle_options;
get_callback_function(<<"GET">>) -> handle_get;
get_callback_function(<<"HEAD">>) -> handle_head;
get_callback_function(<<"POST">>) -> handle_post;
get_callback_function(<<"PUT">>) -> handle_put;
get_callback_function(<<"DELETE">>) -> handle_delete;
get_callback_function(<<"TRACE">>) -> handle_trace;
get_callback_function(<<"CONNECT">>) -> handle_connect;
get_callback_function(<<"PATCH">>) -> handle_patch.

json_decode(JsonObject) ->
  try_atomify_keys(
    jiffy:decode(JsonObject, [return_maps, {null_term, undefined}, dedupe_keys])
   ).

try_atomify_keys(Map) ->
  maps:fold(
    fun(Key, Value, Acc) ->
        KeyMaybeAtom = case catch binary_to_existing_atom(Key, latin1) of
                         {'EXIT', _} -> Key;
                         KeyAtom -> KeyAtom
                       end,
        Acc#{KeyMaybeAtom => Value}
    end,
    #{},
    Map
   ).

json_encode(Objects) when is_list(Objects) ->
  ObjectsU = format_values(Objects),
  jiffy:encode(ObjectsU);
json_encode(Object) when is_binary(Object) -> Object;
json_encode(Object) ->
  ObjectU = format_values(Object),
  jiffy:encode(ObjectU).

format_values(List) when is_list(List) ->
  [ format_values(Object) || Object <- List ];
format_values(Map) when is_map(Map) ->
  maps:map(
    fun(_Key, Value) ->
        format_values(Value)
    end,
    Map
   );
format_values(undefined) -> null;
format_values(Value) when is_reference(Value) -> list_to_binary(ref_to_list(Value));
format_values(Value) -> Value.

