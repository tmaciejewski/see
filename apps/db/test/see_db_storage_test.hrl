-define(URL, <<"url1">>).
-define(URL2, <<"url2">>).
-define(TITLE, <<"Page title">>).
-define(TITLE2, <<"Page title 2">>).
-define(WORD1, <<"word1">>).
-define(WORD2, <<"word2">>).

search(Storage, Word) ->
    Pages = sets:to_list(Storage:get_pages_from_index(Word)),
    lists:map(fun Storage:get_page/1, Pages).

when_no_url_added__gets_unvisited_returns_nothing__test_(Storage) ->
    ?_assertEqual(nothing, Storage:get_unvisited()).

when_new_url_is_added__gets_unvisited_returns_it_once__test_(Storage) ->
    Storage:add_url(?URL),
    [?_assertEqual({ok, ?URL}, Storage:get_unvisited()),
     ?_assertEqual(nothing, Storage:get_unvisited())].

when_url_is_pending__set_unvisited_enqueues_it_back__test_(Storage) ->
    Storage:add_url(?URL),
    Storage:get_unvisited(),
    Storage:set_unvisited(?URL),
    ?_assertEqual({ok, ?URL}, Storage:get_unvisited()).

when_url_is_pending__update_url_sets_add_to_index_test_(Storage) ->
    Storage:add_url(?URL),
    {ok, ?URL} = Storage:get_unvisited(),
    Storage:update_url(?URL, ?TITLE, [?WORD1, ?WORD2]),
    [?_assertEqual([{?URL, ?TITLE}], search(Storage, ?WORD1)),
     ?_assertEqual([{?URL, ?TITLE}], search(Storage, ?WORD2))].

when_url_is_revisited__update_url_updates_index_test_(Storage) ->
    Storage:add_url(?URL),
    {ok, ?URL} = Storage:get_unvisited(),
    Storage:update_url(?URL, ?TITLE, [?WORD1]),
    Storage:update_url(?URL, ?TITLE, [?WORD2]),
    [?_assertEqual([], search(Storage, ?WORD1)),
     ?_assertEqual([{?URL, ?TITLE}], search(Storage, ?WORD2))].

when_two_urls_has_the_same_word__update_url_adds_them_both_to_index_test_(Storage) ->
    Storage:add_url(?URL),
    Storage:add_url(?URL2),
    Storage:get_unvisited(),
    Storage:get_unvisited(),
    Storage:update_url(?URL, ?TITLE, [?WORD1]),
    Storage:update_url(?URL2, ?TITLE2, [?WORD1]),
    [?_assertEqual([{?URL, ?TITLE}, {?URL2, ?TITLE2}], search(Storage, ?WORD1))].

when_url_is_already_indexed__add_url_does_nothing_test_(Storage) ->
    Storage:add_url(?URL),
    Storage:get_unvisited(),
    Storage:update_url(?URL, ?TITLE, [?WORD1]),
    Storage:add_url(?URL),
    [?_assertEqual([{?URL, ?TITLE}], search(Storage, ?WORD1))].

when_no_url_has_given_word__return_empty_set_test_(Storage) ->
    [?_assertEqual([], search(Storage, ?WORD1))].
