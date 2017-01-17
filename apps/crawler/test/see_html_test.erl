-module(see_html_test).
-include_lib("eunit/include/eunit.hrl").

parse_html_test_() ->
     Content = "lorem <!doctype html>ipsum<b foo='bar'> dolor </b> <empty attr=val/> <p> sit <i> amet</i> <? xxx ?> <!-- aaa --> </xx>",
     ?_assertEqual({root, [], [<<"lorem ">>, <<"ipsum">>, {<<"b">>, [{<<"foo">>, <<"bar">>}], [<<" dolor ">>]},
                               {<<"empty">>, [{<<"attr">>, <<"val">>}], []}, 
                               {<<"p">>, [], [<<" sit ">>, {<<"i">>, [], [<<" amet">>]}]}]},
                   see_html:parse(Content)).

text_test_() ->
    Page = see_html:parse("lorem<p foo=bar> <p>ipsum</p>dolor</p>sit amet"),
    ?_assertEqual([<<"lorem">>, <<" ">>, <<"ipsum">>, <<" ">>, <<"dolor">>, <<" ">>, <<"sit amet">>], see_html:text(Page)).

title_test_() ->
    Page = see_html:parse("lorem <title foo=bar>this is<p>title</p></title> ipsum <title>not title</title>"),
    ?_assertEqual([<<"this is">>, <<" ">>, <<"title">>], see_html:title(Page)).

links_test_() ->
    Page = see_html:parse("<html><a href=url1>link1</a></html> <a href='url2'>link2</a> <a>bad link</a> <a href>bad link</a>"),
    ?_assertEqual([<<"url1">>, <<"url2">>], see_html:links(Page)).
