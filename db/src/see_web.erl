-module(see_web).

-export([start_link/0, stop/0, loop/1]).

-define(HEADERS, [{"Content-Type", "text/html; charset=utf-8"}]).

start_link() ->
    {ok, Port} = application:get_env(port),
    {ok, Ip} = application:get_env(ip),
    Options = [{name, ?MODULE},
               {ip, Ip},
               {port, Port},
               {loop, fun loop/1}],
    mochiweb_http:start_link(Options).

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
    case proplists:get_value("q", QueryStringData) of
        undefined ->
            Req:respond({200, ?HEADERS, "Missing search query"});
        Query ->
            Result = see_db_srv:search(list_to_binary(Query)),
            error_logger:info_report([{query, Query}, {result, Result}]),
            Req:respond({200, ?HEADERS, ["<html>\n<body>\n",
                                         "<h1>Results</h1>\n",
                                         result_to_html(Result),
                                         "</body>\n</html>"]})
    end;

handle_request(Req, "/queue") ->
    QueryStringData = Req:parse_qs(),
    case proplists:get_value("url", QueryStringData) of
        undefined ->
            Req:respond({200, ?HEADERS, "Missing URL"});
        URL ->
            case mochiweb_util:urlsplit(URL) of
                {"http", _, _, _, _} = Parts ->
                    see_db_srv:queue(mochiweb_util:urlunsplit(Parts)),
                    error_logger:info_report([{added, URL}]),
                    Req:respond({200, ?HEADERS, "OK"});
                _ ->
                    Req:respond({200, ?HEADERS, "Wrong URL"})
            end
    end;

handle_request(Req, "/" ++ Path) ->
    Req:serve_file(Path, local_path(["priv", "html"]), ?HEADERS).

result_to_html(Result) ->
    ["<ol>\n",
     [["<li><a href=\"", URL, "\">", URL, "</a></li>\n"] || URL <- Result],
     "</ol>\n"].

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).
