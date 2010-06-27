-module(crawler).
-compile(export_all).

-import(http).
-import(db).

start() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

visit(Pid, URL) ->
    gen_server:cast(Pid, {visit, URL}).

init(Args) ->
    {ok, Args, 0}.

handle_info(timeout, State) ->
    case db:next() of
        nothing ->
            {noreply, State, 1000};
        {ok, Next} ->
            db:visit(Next, undefined, "<visiting...>"),
            visit(self(), Next),
            {noreply, State, 1}
   end.

is_text("text/html") -> true;
is_text("text/plain") -> true;
is_text(_) -> false.

stripPage(Content) ->
    Body = re:replace(Content, ".*<body>(.*)</body>.*", "\1", 
        [global, {return, list}, caseless]),
    re:replace(Body, "<[^>]+>", "", [global, {return, list}]).

handle_cast({visit, URL}, State) ->
    io:format("Getting http://~s... ", [URL]),
    case http:getPage(URL) of
        {ok, Code, Headers, Content, Links} ->
            io:format(" ~w\n", [Code]),
            {ok, Type} = http:header("Content-Type", Headers),
            case is_text(hd(string:tokens(Type, ";"))) of
                true ->
                    db:visit(URL, Code, stripPage(Content)),
                    db:links(Links, URL),
                    db:queue(Links);
                false ->
                    db:visit(URL, Code, "<binary>")
            end;

        {error, Reason} ->
            db:visit(URL, error, Reason)
    end,
    {noreply, State, 1};

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(normal, _) ->
    io:format("Stopping...\n").

    
