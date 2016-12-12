-module(see_http).

-export([get_page/1]).

-define(CODE_OK, 200).
-define(CODE_MOVED, 301).
-define(CODE_FOUND, 302).

-define(TEXT_MIME, ["text/html", "text/plain"]).

get_page(URL) ->
    Response = httpc:request(get, {URL, []}, [{autoredirect, false}], [{body_format, binary}]),
    handle_response(Response, URL).

handle_response({ok, {{_, ?CODE_OK, _}, Headers, Content}}, URL) ->
    case is_text_page(Headers) of
        true ->
            {ok, text(Content), links(URL, Content)};
        false ->
            binary
    end;

handle_response({ok, {{_, ?CODE_MOVED, _}, Headers, _}}, _URL) ->
    {redirect, proplists:get_value("location", Headers)};

handle_response({ok, {{_, ?CODE_FOUND, _}, Headers, _}}, _URL) ->
    {redirect, proplists:get_value("location", Headers)};

handle_response({ok, {{_, Code, _}, Headers, Content}}, _URL) ->
    {error, {Code, Headers, Content}};

handle_response({error, Reason}, _URL) ->
    {error, Reason}.

is_text_page(Headers) ->
    MIME = hd(string:tokens(proplists:get_value("content-type", Headers), ";")),
    lists:member(MIME, ?TEXT_MIME).

text(Content) ->
    DataTokens = lists:map(fun token_data/1, mochiweb_html:tokens(Content)),
    lists:filter(fun(D) -> D /= <<>> end, DataTokens).

token_data({data, Data, _}) ->
    Data;

token_data(_) ->
    <<>>.

links(URL, Content) ->
    case re:run(Content, "<a *href=\"([^\"# ]*)", 
                 [global, {capture, [1], list}]) of
        {match, Match} ->
            Links = lists:map(fun(Link) -> absLink(URL, Link) end, 
                              lists:append(Match)),
            lists:filter(fun(X) -> length(X) > 0 end, Links);
        nomatch -> []
    end.  

absLink(URL, "/" ++ Link) ->
    case http_uri:parse(URL) of
        {ok, {_, _, Host, _, _, _}} ->
            "http://" ++ string:join([Host, Link], "/");
        _ ->
            ""
    end;

absLink(_, Link) ->
    Link.
