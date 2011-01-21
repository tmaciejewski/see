-module(see).

-export([start/0, stop/0]).

start() ->
    application:start(see).
    
stop() ->
    application:stop(see).
