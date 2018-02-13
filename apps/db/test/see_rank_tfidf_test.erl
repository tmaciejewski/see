-module(see_rank_tfidf_test).
-include_lib("eunit/include/eunit.hrl").

start() ->
    meck:new(storage_mock, [non_strict]).

stop(_) ->
    ?assert(meck:validate(storage_mock)),
    meck:unload(storage_mock).

one_page_many_words_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, get_words, fun(page) -> [word1, word2, word3, word2, word2, word3] end),
             meck:expect(storage_mock, get_page_count, fun() -> 1 end),
             meck:expect(storage_mock, get_pages_from_index,
                         [{[word1], sets:from_list([page])}, {[word2], sets:from_list([page])}, {[word3], sets:from_list([page])}]),
             IDF = math:log(1 / 2),
             [
              ?_assertEqual((1/6) * IDF, see_rank_tfidf:rank(page, [word1], storage_mock)),
              ?_assertEqual((3/6) * IDF, see_rank_tfidf:rank(page, [word2], storage_mock)),
              ?_assertEqual((2/6) * IDF, see_rank_tfidf:rank(page, [word3], storage_mock)),
              ?_assertEqual((4/6) * IDF, see_rank_tfidf:rank(page, [word1, word2], storage_mock)),
              ?_assertEqual((5/6) * IDF, see_rank_tfidf:rank(page, [word2, word3], storage_mock)),
              ?_assertEqual((6/6) * IDF, see_rank_tfidf:rank(page, [word1, word2, word3], storage_mock))
             ]
     end}.

many_pages_one_word_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, get_words,
                         [{[page1], [word1]}, {[page2], [word1]}, {[page3], [word2]}, {[page4], []}]),
             meck:expect(storage_mock, get_page_count, fun() -> 4 end),
             meck:expect(storage_mock, get_pages_from_index,
                         [{[word1], sets:from_list([page1, page2])}, {[word2], sets:from_list([page3])}]),
             TF = 1,
             [
              ?_assertEqual(TF * math:log(4 / 3), see_rank_tfidf:rank(page1, [word1], storage_mock)),
              ?_assertEqual(TF * math:log(4 / 3), see_rank_tfidf:rank(page2, [word1], storage_mock)),
              ?_assertEqual(TF * math:log(4 / 2), see_rank_tfidf:rank(page3, [word2], storage_mock))
             ]
     end}.
