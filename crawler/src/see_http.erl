-module(see_http).

-export([get_page/1]).

-define(CODE_OK, 200).
-define(CODE_MOVED, 301).
-define(CODE_FOUND, 302).

-define(TEXT_MIME, ["text/html", "text/plain"]).

get_page(URL) ->
    case httpc:request(get, {URL, []}, [{autoredirect, false}], [{body_format, binary}]) of
        {ok, {{_, ?CODE_OK, _}, Headers, Content}} ->
            case is_text(Headers) of
                true ->
                    {ok, words(Content), links(URL, Content)};
                false ->
                    binary
            end;
        {ok, {{_, ?CODE_MOVED, _}, Headers, _}} ->
            {redirect, proplists:get_value("location", Headers)};
        {ok, {{_, ?CODE_FOUND, _}, Headers, _}} ->
            {redirect, proplists:get_value("location", Headers)};
        {ok, {{_, Code, _}, Headers, Content}} ->
            {error, {Code, Headers, Content}};
        {error, Reason} ->
            {error, Reason}
    end.

is_text(Headers) ->
    MIME = hd(string:tokens(proplists:get_value("content-type", Headers), ";")),
    lists:member(MIME, ?TEXT_MIME).

words(Content) ->
    DataTokens = lists:filter(fun is_data_token/1, mochiweb_html:tokens(Content)),
    lists:flatmap(fun words_from_data_token/1, DataTokens).

is_data_token({data, _, _}) ->
    true;

is_data_token(_) ->
    false.

words_from_data_token({data, Data, _}) ->
    Separators = lists:map(fun(X) -> <<X>> end, " \n\t\r,.()[]{}\"'`"),
    binary:split(Data, Separators, [global, trim_all]).

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
