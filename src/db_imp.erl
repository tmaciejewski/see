-module(db_imp).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl"). 

-record(page, {url, code, content, last_visit}).
-record(link, {to, from}).

init(_Args) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    mnesia:create_table(page, [{disc_copies, [node()]}, 
             {attributes, record_info(fields, page)}]),
    mnesia:create_table(link, [{disc_copies, [node()]}, 
             {attributes, record_info(fields, link)}]),

    {ok, []}.

terminate(_, _) ->
    mnesia:stop().

handle_cast({visit, URL, Code, Content}, _) ->
    F = fun() -> 
            mnesia:write(#page{url = URL, code = Code,
                    content = Content, last_visit = now()})
    end,
    mnesia:transaction(F),
    {noreply, []};

handle_cast({link, To, From}, _) ->
    F = fun() -> mnesia:write(#link{to = To, from = From}) end,
    mnesia:transaction(F),
    {noreply, []};

handle_cast({queue, URL}, _) ->
    case mnesia:transaction(fun() -> mnesia:read({page, URL}) end) of
        {atomic, []} ->
            mnesia:transaction(fun() -> mnesia:write(#page{url = URL,
                                last_visit = now()}) end);
        {atomic, _}  ->
            ok 
    end,
    {noreply, []}.

handle_call(stop, _, _) ->
    {stop, shutdown, ok, []};

handle_call(next, _, _) ->
    F = fun() -> 
        Q = qlc:q([{Page#page.url, Page#page.last_visit} || 
                    Page <- mnesia:table(page), Page#page.content == undefined]),
        qlc:eval(qlc:sort(Q, {order, fun({_, A}, {_, B}) -> 
                            timer:now_diff(A, B) < 0 end}))
    end,
    case mnesia:transaction(F) of
       {atomic, [{Next, _}|_]} -> 
           {reply, {ok, Next}, []};
       {atomic, []} ->
           {reply, nothing, []}
    end;

handle_call({search, Phrases}, _, _) ->
    F = fun() -> 
            Q = qlc:q([{Page#page.url, lists:sum(occurs(Phrases, Page#page.content))} || Page <- mnesia:table(page), 
                is_list(Page#page.content), lists:all(fun(X) -> X > 0 end, occurs(Phrases, Page#page.content))]),
        qlc:eval(qlc:sort(Q, {order, fun({_, A}, {_, B}) -> A > B end}))
    end,
    case mnesia:transaction(F) of
        {atomic, Results} ->
            {reply, Results, []};
        {aborted, Reason} ->
            {reply, {error, Reason}, []}
    end.

occurs(Phrases, Content) ->
    F = fun(Phrase) ->
        case re:run(Content, Phrase, [global, caseless]) of
            nomatch -> 0;
            {match, Match} -> length(Match)
        end
    end, 
    lists:map(F, Phrases).
