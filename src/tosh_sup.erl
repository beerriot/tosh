
-module(tosh_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(DEFAULT_RIAK_IP, "127.0.0.1").
-define(DEFAULT_RIAK_PORT, 8087).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case application:get_env(tosh, riak) of
        {ok, V} ->
            IP = proplists:get_value(ip, V, ?DEFAULT_RIAK_IP),
            Port = proplists:get_value(port, V, ?DEFAULT_RIAK_PORT);
        undefined ->
            IP = ?DEFAULT_RIAK_IP,
            Port = ?DEFAULT_RIAK_PORT
    end,
    PoolArgs = [{size, 5},
                {max_overflow, 10},
                {name, {local, clients}},
                {worker_module, tosh_riakc_poolboy_shim}],
    ClientPool = poolboy:child_spec(clients, PoolArgs, [IP, Port]),
    ToshDispatch = [ {['*'], goma_fs_resource,
                      [{port, 7070},
                       {base, "/Users/bryan/log"}]} ],
    GomaSupSpec = goma:child_spec(tosh, {127,0,0,1}, 7070, ToshDispatch),
    {ok, { {one_for_one, 5, 10}, [ClientPool, GomaSupSpec]} }.
