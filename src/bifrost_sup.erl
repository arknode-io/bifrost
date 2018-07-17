%%%-------------------------------------------------------------------
%% @doc bifrost top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bifrost_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(ID), #{id => ID, start => {ID, start_link, []}}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupervisorFlags = #{strategy => one_for_one,
                      intensity => 25,
                      period => 60},
  ChildSpecs = [?CHILD(bifrost_web)],
  {ok, {SupervisorFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
