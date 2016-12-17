-module(see_crawler).

-export([start/0, stop/0]).

start() ->
    application:start(see_crawler).
    
stop() ->
    application:stop(see_crawler).
