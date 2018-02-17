%%%-------------------------------------------------------------------
%% @doc bifrost public API
%% @end
%%%-------------------------------------------------------------------

-module(bifrost_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    bifrost_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
