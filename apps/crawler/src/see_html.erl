-module(see_html).

-export([parse/1, title/1, text/1, links/1]).

-define(TITLE_TAG, <<"title">>).
-define(A_TAG, <<"a">>).
-define(HREF_ATTR, <<"href">>).

parse(Content) ->
    parse_tokens(mochiweb_html:tokens(Content), [{root, [], []}]).

parse_tokens([], [{root, [], Elements}]) ->
    {root, [], lists:reverse(Elements)};

parse_tokens([], [{Tag, Attr, Children}, {ParentTag, ParentAttr, ParentChildren} | Elements]) ->
    parse_tokens([], [{ParentTag, ParentAttr, [{Tag, Attr, lists:reverse(Children)} | ParentChildren]} | Elements]);

parse_tokens([{data, _, true} | Tokens], Elements) ->
    parse_tokens(Tokens, Elements);

parse_tokens([{data, Data, false} | Tokens], [{Tag, Attr, Children} | Elements]) ->
    parse_tokens(Tokens, [{Tag, Attr, [Data | Children]} | Elements]);

parse_tokens([{start_tag, Tag, Attr, true} | Tokens], [{OpenTag, OpenAttr, Children} | Elements]) ->
    parse_tokens(Tokens, [{OpenTag, OpenAttr, [{Tag, Attr, []} | Children]} | Elements]);

parse_tokens([{start_tag, Tag, Attr, false} | Tokens], Elements) ->
    parse_tokens(Tokens, [{Tag, Attr, []} | Elements]);

parse_tokens([{end_tag, Tag} | Tokens], [{Tag, Attr, Children}, {ParentTag, ParentAttr, ParentChildren} | Elements]) ->
    parse_tokens(Tokens, [{ParentTag, ParentAttr, [{Tag, Attr, lists:reverse(Children)} | ParentChildren]} | Elements]);

parse_tokens([{end_tag, _} | Tokens], Elements) ->
    parse_tokens(Tokens, Elements);

parse_tokens([_ | Tokens], Elements) ->
    parse_tokens(Tokens, Elements).

text(Page) ->
    lists:reverse(text(Page, [])).

text({_, _, []}, Text) ->
    Text;

text({Tag, Attr, [Child | Children]}, Text) ->
    text({Tag, Attr, Children}, text(Child, Text));

text(Data, []) ->
    [Data];

text(Data, Text) ->
    [Data, <<" ">> | Text].

links(Page) ->
    lists:reverse(links(Page, [])).

links({?A_TAG, [], [Child | Children]}, Links) ->
    links({?A_TAG, [], Children}, links(Child, Links));

links({?A_TAG, [], []}, Links) ->
    Links;

links({?A_TAG, Attributes, Children}, Links) ->
    NewLinks = case proplists:get_value(?HREF_ATTR, Attributes) of
        undefined ->
            Links;
        ?HREF_ATTR ->
            Links;
        Link ->
            [Link | Links]
    end,
    links({?A_TAG, [], Children}, NewLinks);

links({Tag, Attributes, [Child | Children]}, Links) ->
    links({Tag, Attributes, Children}, links(Child, Links));

links(_, Links) ->
    Links.

title(Page) ->
    lists:flatten(title(Page, [])).

title({?TITLE_TAG, Attr, Children}, _) ->
    text({?TITLE_TAG, Attr, Children});

title({Tag, Attr, [Child | Children]}, _) ->
    case title(Child) of
        [] ->
            title({Tag, Attr, Children});
        Title ->
            Title
    end;

title(_, _) ->
    [].
