-module(see_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    see_db_sup:start_link(application:get_all_env()).

stop(_State) ->
    ok.
