-module(see_html).

-export([is_text/1,
         words/1,
         links/2]).

is_text({{_, _, _}, Headers, _}) ->
    MIME = hd(string:tokens(proplists:get_value("content-type", Headers), ";")),
    is_text(MIME);

is_text("text/html") -> true;
is_text("text/plain") -> true;
is_text(_) -> false.

words({{_, _, _}, _, Content}) ->
    string:tokens(Content, " ").

links(URL, {{_, _, _}, _, Content}) ->
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
