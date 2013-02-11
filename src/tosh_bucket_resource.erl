-module(tosh_bucket_resource).

-export([
         init/1,
         resource_exists/2,
         produce_content/2
        ]).

-include_lib("goma/include/goma.hrl").

-record(ctx, {
          port
         }).

init(Args) ->
    {ok, #ctx{port=proplists:get_value(port, Args, 70)}}.

resource_exists(_Sel, Ctx) ->
    {true, Ctx}. %% all buckets always exists

produce_content(Sel, Ctx) ->
    Client = poolboy:checkout(clients),
    try
        {ok, Keys} = riakc_pb_socket:list_keys(Client, bucket(Sel)),
        Menu = [ key_menu(Sel, Ctx, K) || K <- Keys ],
        {{text, goma_util:format_menu(Menu)}, Ctx}
    after
        poolboy:checkin(clients, Client)
    end.

key_menu(#goma_selector{raw=Selector}, #ctx{port=Port}, Key) ->
    #goma_menu{
             type = ?GOMA_TYPE_BINARY,
             display = Key,
             selector = goma_util:join_selector([Selector, Key]),
             host = "127.0.0.1",
             port = Port}.

bucket(#goma_selector{bindings=B}) ->
    proplists:get_value(bucket, B).

