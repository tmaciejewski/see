-module(see_web).

-export([start_link/1, stop/0, loop/1]).

-define(HEADERS, [{"Content-Type", "text/html; charset=utf-8"}]).

start_link(Options) ->
    Port = proplists:get_value(port, Options),
    Ip = proplists:get_value(ip, Options),
    HTTPOptions = [{name, ?MODULE},
               {ip, Ip},
               {port, Port},
               {loop, fun loop/1}],
    mochiweb_http:start_link(HTTPOptions).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    Path = Req:get(path),
    try
        handle_request(Req, Path)
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, ?HEADERS, "Internal Server Error"})
    end.

handle_request(Req, "/") ->
    handle_request(Req, "/index.html");

handle_request(Req, "/search") ->
    QueryStringData = Req:parse_qs(),
    case proplists:get_value("query", QueryStringData) of
        undefined ->
            Req:respond({200, [{"content-type", "application/json"}],
                         mochijson2:encode({struct, [{"results", []}]})});
        Query ->
            Results = [list_to_binary(R) || R <- see_db_srv:search(list_to_binary(Query))],
            error_logger:info_report([{query, Query}, {result, Results}]),
            Req:respond({200, [{"content-type", "application/json"}],
                         mochijson2:encode({struct, [{"results", Results}]})})
    end;

handle_request(Req, "/add") ->
    PostData = Req:parse_post(),
    case proplists:get_value("url", PostData) of
        undefined ->
            Req:respond({200, ?HEADERS, "Missing URL"});
        URL ->
            case see_db_srv:queue(URL) of
                ok ->
                    error_logger:info_report([{added, URL}]),
                    Req:respond({200, ?HEADERS, "OK"});
                error ->
                    Req:respond({200, ?HEADERS, "Wrong URL"})
            end
    end;

handle_request(Req, "/" ++ Path) ->
    Req:serve_file(Path, local_path(["priv", "html"]), ?HEADERS).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).
