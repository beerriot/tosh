-module(tosh_gopher).

-export([start_link/4, init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
    loop(Socket, Transport).
 
loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            Transport:send(Socket, lists:reverse(binary_to_list(Data))),
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.
