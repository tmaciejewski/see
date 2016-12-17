-module(see_http).

-export([get_page/1]).

-define(CODE_OK, 200).
-define(CODE_MOVED, 301).
-define(CODE_FOUND, 302).

-define(TEXT_MIME, ["text/html", "text/plain"]).
-define(A_TAG, <<"a">>).
-define(HREF_ATTR, <<"href">>).

get_page(URL) ->
    Response = httpc:request(get, {URL, []}, [{autoredirect, false}], [{body_format, binary}]),
    handle_response(Response, URL).

handle_response({ok, {{_, ?CODE_OK, _}, Headers, Content}}, URL) ->
    case is_text_page(Headers) of
        true ->
            {TextChunks, Links} = lists:foldl(fun accumulate_data/2, {[], []}, mochiweb_html:tokens(Content)),
            AbsLinks = lists:map(fun(Link) -> absLink(URL, Link) end, Links),
            {ok, lists:reverse(TextChunks), lists:reverse(AbsLinks)};
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

accumulate_data({data, Data, false}, {[], Links}) ->
    {[Data], Links};

accumulate_data({data, Data, false}, {DataChunks, Links}) ->
    {[Data, <<" ">> | DataChunks], Links};

accumulate_data({start_tag, ?A_TAG, Attributes, false}, {DataChunks, Links}) ->
    case proplists:get_value(?HREF_ATTR, Attributes) of
        undefined ->
            {DataChunks, Links};
        ?HREF_ATTR ->
            {DataChunks, Links};
        Link ->
            {DataChunks, [binary_to_list(Link)|Links]}
    end;

accumulate_data(_, {DataChunks, Links}) ->
    {DataChunks, Links}.

absLink(URL, Link) ->
    {URLScheme, URLNetloc, URLPath, _, _} = mochiweb_util:urlsplit(URL),
    case mochiweb_util:urlsplit(Link) of
        {[], [], "/" ++ LinkPath, LinkQuery, _} ->
            mochiweb_util:urlunsplit({URLScheme, URLNetloc, "/" ++ LinkPath, LinkQuery, []});
        {[], [], LinkPath, LinkQuery, _} when length(URLPath) == 0 ->
            Dir = "/",
            mochiweb_util:urlunsplit({URLScheme, URLNetloc, filename:join(Dir, LinkPath), LinkQuery, []});
        {[], [], LinkPath, LinkQuery, _} ->
            Dir = filename:dirname(URLPath),
            mochiweb_util:urlunsplit({URLScheme, URLNetloc, filename:join(Dir, LinkPath), LinkQuery, []});
        {LinkScheme, LinkNetloc, LinkPath, LinkQuery, _} ->
            mochiweb_util:urlunsplit({LinkScheme, LinkNetloc, LinkPath, LinkQuery, []})
    end.
