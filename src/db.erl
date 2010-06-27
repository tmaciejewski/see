-module(db).
-compile(export_all).

start() ->
    gen_server:start({global, db}, db_imp, [], []).

stop() ->
    gen_server:call({global, db}, stop).

visit(URL, Code, Content) ->
    gen_server:cast({global, db}, {visit, URL, Code, Content}).

link(To, From) ->
    gen_server:cast({global, db}, {link, To, From}).

links(Links, From) ->
    lists:foreach(fun(Link) -> link(Link, From) end, Links).

queue([]) -> ok;
queue([URL|Rest]) when is_list(URL) ->
    gen_server:cast({global, db}, {queue, URL}),
    queue(Rest);

queue(URL) ->
    queue([URL]).

next() ->
    gen_server:call({global, db}, next).

search(Phrases) ->
    gen_server:call({global, db}, {search, Phrases}).
