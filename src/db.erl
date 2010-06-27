-module(db).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl"). 

-record(page, {url, code, content, last_visit}).
-record(link, {to, from}).

start() ->
    mnesia:start().

stop() ->
    mnesia:stop().

init() ->
    mnesia:create_schema([node()]),

    start(),

    mnesia:delete_table(page),
    mnesia:delete_table(link),

    {atomic, ok} = mnesia:create_table(page, [{disc_copies, [node()]}, 
             {attributes, record_info(fields, page)}]),
    {atomic, ok} = mnesia:create_table(link, [{disc_copies, [node()]}, 
             {attributes, record_info(fields, link)}]),
    ok.

visit(URL, Code, Content) ->
    F = fun() -> 
            mnesia:write(#page{url = URL, code = Code,
                    content = Content, last_visit = now()})
    end,
    mnesia:transaction(F).

link(To, From) ->
    F = fun() -> mnesia:write(#link{to = To, from = From}) end,
    mnesia:transaction(F).

links(Links, From) ->
    lists:foreach(fun(Link) -> link(Link, From) end, Links).

queue([]) -> ok;
queue([URL|Rest]) when is_list(URL) ->
    case mnesia:transaction(fun() -> mnesia:read({page, URL}) end) of
        {atomic, []} ->
            mnesia:transaction(fun() -> mnesia:write(#page{url = URL,
                                last_visit = now()}) end);
        {atomic, _}  ->
            ok 
    end,
    queue(Rest);

queue(URL) ->
    queue([URL]).

next() ->
    F = fun() -> 
        Q = qlc:q([{Page#page.url, Page#page.last_visit} || 
                    Page <- mnesia:table(page), Page#page.content == undefined]),
        qlc:eval(qlc:sort(Q, {order, fun({_, A}, {_, B}) -> 
                            timer:now_diff(A, B) < 0 end}))
    end,
    case mnesia:transaction(F) of
       {atomic, [{Next, _}|_]} -> 
           {ok, Next};
       {atomic,    []   } ->
           nothing
    end.

occurs(Phrases, Content) ->
    F = fun(Phrase) ->
        case re:run(Content, Phrase, [global, caseless]) of
            nomatch -> 0;
            {match, Match} -> length(Match)
        end
    end, 
    lists:map(F, Phrases).

search(Phrases) ->
    F = fun() -> 
            Q = qlc:q([{Page#page.url, lists:sum(occurs(Phrases, Page#page.content))} || Page <- mnesia:table(page), 
                is_list(Page#page.content), lists:all(fun(X) -> X > 0 end, occurs(Phrases, Page#page.content))]),
        qlc:eval(qlc:sort(Q, {order, fun({_, A}, {_, B}) -> A > B end}))
    end,
    {atomic, Results} = mnesia:transaction(F),
    Results.

select_all(Table) -> 
    {atomic, Results} = mnesia:transaction(fun() -> qlc:eval(qlc:q(
        [ X || X <- mnesia:table(Table) ] )) end ),
    Results.
