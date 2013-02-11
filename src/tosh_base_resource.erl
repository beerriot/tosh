-module(tosh_base_resource).

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
    {true, Ctx}. %% top-level list always exists

produce_content(Sel, Ctx) ->
    Client = poolboy:checkout(clients),
    try
        {ok, Buckets} = riakc_pb_socket:list_buckets(Client),
        Menu = [ bucket_menu(Sel, Ctx, B) || B <- Buckets ],
        {{text, goma_util:format_menu(Menu)}, Ctx}
    after
        poolboy:checkin(clients, Client)
    end.

bucket_menu(#goma_selector{raw=Selector}, #ctx{port=Port}, Bucket) ->
    #goma_menu{
             type = ?GOMA_TYPE_MENU,
             display = Bucket,
             selector = goma_util:join_selector([Selector, Bucket]),
             host = "127.0.0.1",
             port = Port}.
