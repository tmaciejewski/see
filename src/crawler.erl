-module(crawler).
-compile(export_all).

-import(http).
-import(db).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

visit(Pid, URL) ->
    gen_server:cast(Pid, {visit, URL}).

init(Args) ->
    {ok, Args, 0}.

links(_URL, WebPage) ->
    case  re:run(WebPage, "<a *href=\"(http://[^\"# ]*)", 
                [global, {capture, [1], list}]) of
        {match, Match} ->
            lists:append(Match);
        nomatch -> []
    end.  

handle_info(timeout, State) ->
    case db:next() of
        nothing ->
            {noreply, State, 1000};
        {ok, Next} ->
            handle_cast({visit, Next}, State),
            {noreply, State, 1}
   end.

handle_cast({visit, URL}, State) ->
    io:format("Getting ~s... ", [URL]),
    {Code, Content} = http:getPage(URL),
    Links = links(URL, Content),
    io:format(" ~w\n", [Code]),
    db:visit(URL, Code, Content),
    db:links(Links, URL),
    db:queue(Links),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(normal, _) ->
    io:format("Stopping...\n").

    
