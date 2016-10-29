-module(see_html).

-export([is_text/1,
         words/1,
         links/2]).

-define(TEXT_MIME, ["text/html", "text/plain"]).

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
