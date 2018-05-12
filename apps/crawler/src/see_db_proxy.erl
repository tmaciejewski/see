-module(see_db_proxy).

-export([next/1, visited/3, queue/2]).

next(DbNode) ->
    rpc:call(DbNode, see_db_srv, next, []).

visited(DbNode, URL, Content) ->
    rpc:cast(DbNode, see_db_srv, visited, [URL, Content]).

queue(DbNode, URL) ->
    rpc:call(DbNode, see_db_srv, queue, [URL]).
