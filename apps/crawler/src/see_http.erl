-module(see_http).

-export([get_page/1]).

-define(CODE_OK, 200).
-define(CODE_MOVED, 301).
-define(CODE_FOUND, 302).

-define(TEXT_MIME, ["text/html", "text/plain"]).

get_page(URL) ->
    Headers = [{"user-agent", "SEE (Search Engine in Erlang; https://github.com/tmaciejewski/see)"}],
    HTTPOptions = [{autoredirect, false}, {relaxed, true}],
    Options = [{body_format, binary}],
    Response = httpc:request(get, {encode_url(URL), Headers}, HTTPOptions, Options),
    handle_response(Response).

encode_url(URL) ->
    {Schema, NetLoc, Path, Query, _} = mochiweb_util:urlsplit(URL),
    mochiweb_util:urlunsplit({Schema, NetLoc, encode_path(Path), encode_query(Query), []}).

encode_path(Path) ->
    PathElements = filename:split(Path),
    case PathElements of
        [] ->
            [];
        ["/"|Elements] ->
            EncodedElements = [http_uri:encode(E) || E <- Elements],
            filename:join(["/"|EncodedElements]);
        Elements ->
            EncodedElements = [http_uri:encode(E) || E <- Elements],
            filename:join(EncodedElements)
    end.

encode_query(Query) ->
    Query.

handle_response({ok, {{_, ?CODE_OK, _}, Headers, Content}}) ->
    case is_text_page(Headers) of
        true ->
            {ok, Content};
        false ->
            binary
    end;

handle_response({ok, {{_, ?CODE_MOVED, _}, Headers, _}}) ->
    {redirect, proplists:get_value("location", Headers)};

handle_response({ok, {{_, ?CODE_FOUND, _}, Headers, _}}) ->
    {redirect, proplists:get_value("location", Headers)};

handle_response({ok, {{_, Code, _}, Headers, Content}}) ->
    {error, {Code, Headers, Content}};

handle_response({error, Reason}) ->
    {error, Reason}.

is_text_page(Headers) ->
    MIME = hd(string:tokens(proplists:get_value("content-type", Headers), ";")),
    lists:member(MIME, ?TEXT_MIME).
