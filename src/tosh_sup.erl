
-module(tosh_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ToshDispatch = [ {['*'], goma_fs_resource,
                      [{port, 7070},
                       {base, "/Users/bryan/log"}]} ],
    GomaSupSpec = goma:child_spec(tosh, {127,0,0,1}, 7070, ToshDispatch),
    {ok, { {one_for_one, 5, 10}, [GomaSupSpec]} }.
