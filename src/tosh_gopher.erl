-module(tosh_gopher).

-export([start_link/4, init/4]).

-include_lib("kernel/include/file.hrl").

-define(MAX_SELECTOR_LENGTH, 255).
-define(SELECTOR_TIMEOUT, 5000).

-define(CRLF, <<13, 10>>).
-define(TAB, <<9>>).

-define(TYPE_TEXT_FILE, <<"0">>).
-define(TYPE_MENU,      <<"1">>).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
    handle_request(Socket, Transport,
                   interpret_line(read_line(Socket, Transport))),
    close_connection(Socket, Transport).

read_line(Socket, Transport) ->
    Transport:setopts(Socket,
                      [{recbuf, ?MAX_SELECTOR_LENGTH+size(?CRLF)},
                       {packet, line}]),
    Transport:recv(Socket, 0, ?SELECTOR_TIMEOUT).

interpret_line({ok, Data}) ->
    case re:run(Data, "([^\t\n\r]+)\t*([^\n\r]*)",
                [{capture, all_but_first, binary}]) of
        {match, [Selector, <<>>]} ->
            {ok, Selector};
        {match, [Selector, Search]} ->
            {ok, Selector, Search};
        nomatch ->
            {ok, ""}
    end;
interpret_line(Error) ->
    Error.

close_connection(Socket, Transport) ->
    Transport:send(Socket, [?CRLF, <<".">>, ?CRLF]),
    ok = Transport:close(Socket).

handle_request(Socket, Transport, {ok, Selector}) ->
    case file:read_file_info(Selector) of
        {ok, #file_info{type=directory}} ->
            list_directory(Socket, Transport, Selector);
        {ok, _} ->
            {ok,_} = Transport:sendfile(Socket, Selector);
        _ ->
            error
    end;
handle_request(Socket, Transport, {ok, Selector, Search}) ->
    case file:read_file_info(Selector) of
        {ok, #file_info{type=directory}} ->
            search_directory(Socket, Transport, Selector, Search);
        _ ->
            error
    end;
handle_request(_Socket, _Transport, Error) ->
    Error.

list_directory(Socket, Transport, Dirname) ->
    {ok, Names} = file:list_dir(Dirname),
    [ ok = Transport:send(Socket, [format_menu_line(Dirname, Name)])
      || Name <- Names],
    ok.

format_menu_line(Dirname, Name) ->
    Fullname = filename:join(Dirname, Name),
    case file:read_file_info(Fullname) of
        {ok, #file_info{type=Type}} when Type == directory;
                                         Type == regular ->
            [ menu_entry_type(Type, Fullname),
              ?TAB,
              Name,
              ?TAB,
              filename:join(Dirname, Name),
              ?TAB,
              "TODOHOST",
              ?TAB,
              "TODOPORT",
              ?CRLF ];
        {ok, #file_info{type=Type}} ->
            io:format("Unhandled type '~p' for '~s'~n", [Type, Fullname]),
            [];
        {error, Reason} ->
            io:format("Error '~p' for '~s'~n", [Reason, Fullname]),
            []
    end.

menu_entry_type(directory, _) ->
    ?TYPE_MENU;
menu_entry_type(regular, _) ->
    %% TODO guess text/binary/image by extension
    ?TYPE_TEXT_FILE.

search_directory(Socket, Transport, Dirname, Search) ->
    Grep = "grep -i -d skip -l \""++
        escape_grep_search(Search)++
        "\" "++
        binary_to_list(filename:join(Dirname, "*")),
    Matches = string:tokens(os:cmd(Grep), "\n"),
    [ ok = Transport:send(Socket,
                          %% grep includes Dirname in match
                          format_menu_line(Dirname, filename:basename(Name)))
      || Name <- Matches ],
    ok.
    
escape_grep_search(Search) ->
    re:replace(re:replace(Search, "\\\\", "\\\\\\\\"),
               "\"", "\\\\\"", [{return, list}]).

%%%%%% KILL
