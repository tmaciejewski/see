-module(see_html).

-export([is_text/1,
         words/1,
         links/2]).

-define(TEXT_MIME, ["text/html", "text/plain"]).

is_text(Headers) ->
    MIME = proplists:get_value("content-type", Headers),
    lists:member(MIME, ?TEXT_MIME).

words(Content) ->
    string:tokens(Content, " ").

links(URL, Content) ->
    case  re:run(Content, "<a *href=\"([^\"# ]*)", 
                 [global, {capture, [1], list}]) of
        {match, Match} ->
            Links = lists:map(fun(Link) -> absLink(URL, Link) end, 
                              lists:append(Match)),
            lists:filter(fun(X) -> length(X) > 0 end, Links);
        nomatch -> []
    end.  

absLink(_, "http://" ++ Link) ->
    Link;

absLink(URL, "/" ++ Link) ->
    [Host|_] = string:tokens(URL, "/"),
    string:join([Host, Link], "/");

absLink(_, _) ->
    "".
