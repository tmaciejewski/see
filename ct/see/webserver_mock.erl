-module(webserver_mock).

-export([start/1, stop/0]).

-define(HEADERS, [{"Content-Type", "text/html; charset=utf-8"}]).

start(Options) ->
    Port = proplists:get_value(port, Options),
    Ip = proplists:get_value(ip, Options),
    DataDir = proplists:get_value(data_dir, Options),
    TestPid = proplists:get_value(test_pid, Options),
    HTTPOptions = [{name, ?MODULE},
               {ip, Ip},
               {port, Port},
               {loop, fun(Req) -> loop(Req, TestPid, DataDir) end}],
    mochiweb_http:start(HTTPOptions).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, TestPid, DataDir) ->
    Path = Req:get(path),
    handle_request(Req, Path, DataDir),
    TestPid ! {request, Path}.

handle_request(Req, "/", DataDir) ->
    {ok, Files} = file:list_dir(DataDir),
    Links = [["<a href=", http_uri:encode(F), ">", F, "</a> "] || F <- Files],
    Req:respond({200, ?HEADERS, Links});

handle_request(Req, "/" ++ Path, DataDir) ->
    Req:serve_file(Path, DataDir, ?HEADERS).
