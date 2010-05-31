-module(db).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl"). 

-record(page, {url, code, content, last_visit}).
-record(link, {to, from}).

start() ->
    mnesia:start().

init() ->
     mnesia:create_schema([node()]),

     mnesia:delete_table(page),
     mnesia:delete_table(link),

     mnesia:create_table(page, [{disc_copies, [node()]}, 
             {attributes, record_info(fields, page)}]),
     mnesia:create_table(link, [{disc_copies, [node()]}, 
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
    lists:map(fun(Link) -> link(Link, From) end, Links),
    ok.

queue([]) -> ok;
queue([URL|Rest]) when is_list(URL) ->
    case mnesia:transaction(fun() -> mnesia:read({page, URL}) end) of
        {atomic, []} ->
            mnesia:transaction(fun() -> mnesia:write(#page{url = URL}) end);
        {atomic, _}  ->
            ok 
    end,
    queue(Rest).

next() ->
    F = fun() -> qlc:eval(qlc:q([Page#page.url || Page <- mnesia:table(page), 
       Page#page.content == undefined])) end,
   case mnesia:transaction(F) of
       {atomic, [Next|_]} -> {ok, Next};
       {atomic,    []   } -> nothing
    end.

search(Phrase) ->
    F = fun() -> qlc:eval(qlc:q([Page#page.url || Page <- mnesia:table(page), 
        Page#page.content /= undefined, string:str(Page#page.content, Phrase) > 0])) end,
    {atomic, Results} = mnesia:transaction(F),
    Results.

select_all(Table) -> 
    {atomic, Results} = mnesia:transaction(fun() -> qlc:eval(qlc:q(
        [ X || X <- mnesia:table(Table) ] )) end ),
    Results.
