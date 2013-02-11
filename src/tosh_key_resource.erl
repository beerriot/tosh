-module(tosh_key_resource).

-export([
         init/1,
         resource_exists/2,
         produce_content/2
        ]).

-include_lib("goma/include/goma.hrl").

-record(ctx, {
          port,
          object
         }).

init(Args) ->
    {ok, #ctx{port=proplists:get_value(port, Args, 70)}}.

resource_exists(Sel, Ctx) ->
    Client = poolboy:checkout(clients),
    try
        case riakc_pb_socket:get(Client, bucket(Sel), key(Sel)) of
            {ok, Object} ->
                {true, Ctx#ctx{object=Object}};
            {error, notfound} ->
                {false, Ctx}
        end
    after
        poolboy:checkin(clients, Client)
    end.

produce_content(_Sel, #ctx{object=Obj}=Ctx) ->
    {{binary, format_object(Obj)}, Ctx}.

format_object(Obj) ->
    riakc_obj:get_value(Obj).

bucket(#goma_selector{bindings=B}) ->
    proplists:get_value(bucket, B).

key(#goma_selector{bindings=B}) ->
    proplists:get_value('*', B).
