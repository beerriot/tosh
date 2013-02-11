%% @doc The riakc_pb_socket:start_link function is arity-2, but
%% poolboy calls it as arity-1, so this is a shim to get from one to
%% the other. Interact with the workers later by using the
%% riakc_pb_socket module.
%%
-module(tosh_riakc_poolboy_shim).

-export([
         start_link/1
        ]).

%% @doc start_link shim for poolboy
start_link([IP, Port]) ->
    riakc_pb_socket:start_link(IP, Port).
