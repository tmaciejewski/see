-module(see_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    see_sup:start_link().

stop(_State) ->
    erlang:kill(see_sup).
