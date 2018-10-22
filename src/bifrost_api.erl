-module(bifrost_api).
-export([init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         handle_request/2]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, APIDispatchRules) ->
  lager:info("Got to handle_request ~p @ ~p", [Req, APIDispatchRules]),
  Method = cowboy_req:method(Req),
  PathInfo = cowboy_req:path_info(Req),
  % Methods = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
  Methods = case get_api_function(hd(PathInfo), Method, APIDispatchRules) of
              undefined ->
                [];
              _Function ->
                [Method]
            end,
  {Methods, Req, APIDispatchRules}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_request}
   ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, handle_request}
   ], Req, State}.

handle_request(Req, APIDispatchRules) ->
  lager:info("Got to handle_request ~p @ ~p", [Req, APIDispatchRules]),
  Method = cowboy_req:method(Req),
  Headers = cowboy_req:headers(Req),
  PathInfo = cowboy_req:path_info(Req),
  QueryString = cowboy_req:parse_qs(Req),
  {ok, Body, Req1} = read_body(Req),
  APIReq = #{ method => Method, headers => Headers, path_info => PathInfo,
              query_string => QueryString, body => Body },
  lager:info("All details ~p~n", [APIReq]),
  Function = get_api_function(hd(PathInfo), Method, APIDispatchRules),
  Reply = call(Function, APIReq),
  ReqReply = cowboy_req:set_resp_body(Reply, Req1),
  {true, ReqReply, APIDispatchRules}.


read_body(Req0) ->
  case read_body(Req0, <<>>) of
    {ok, <<>>, Req} ->
      {ok, #{}, Req};
    {ok, Data, Req} ->
      lager:info("Data is ~p", [Data]),
      Body = jiffy:decode(Data, [return_maps]),
      {ok, Body, Req}
  end.

read_body(Req0, Acc) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, Req} -> lager:info("~p", [Data]), {ok, << Acc/binary, Data/binary >>, Req};
    {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
  end.

get_api_function(Resource, Method, APIDispatchRules) ->
  case maps:find(Resource, APIDispatchRules) of
    {ok, #{Method := Function}} ->
      Function;
    _Other ->
      undefined
  end.

call({Module, Function}, Params) ->
  Module:Function(Params);
call(Function, Params) ->
  Function(Params).
