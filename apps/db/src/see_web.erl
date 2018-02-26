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
            respond_json(Req, [{results, []}]);
        Query ->
            Results = see_db_srv:search(list_to_binary(Query)),
            JSONResults = [[{url, URL}, {title, iolist_to_binary(Title)}] || {URL, Title} <- Results],
            respond_json(Req, [{results, JSONResults}])
    end;

handle_request(Req, "/add") ->
    PostData = Req:parse_post(),
    case proplists:get_value("url", PostData) of
        undefined ->
            respond_json(Req, [{result, error}]);
        URL ->
            case see_db_srv:queue(list_to_binary(URL)) of
                ok ->
                    respond_json(Req, [{result, ok}]);
                error ->
                    respond_json(Req, [{result, error}])
            end
    end;

handle_request(Req, "/" ++ Path) ->
    Req:serve_file(Path, local_path(["priv", "html"]), ?HEADERS).

respond_json(Req, JSON) ->
    Req:respond({200, [{"content-type", "application/json"}],
                 mochijson2:encode({struct, JSON})}).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).
