-module(see_db).

-export([start/0, stop/0]).

start() ->
    application:start(see_db).
    
stop() ->
    application:stop(see_db).
