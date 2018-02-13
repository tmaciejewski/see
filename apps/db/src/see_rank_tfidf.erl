-module(see_rank_tfidf).

-export([rank/3]).

rank(Page, Words, Storage) ->
    lists:sum([tf(Word, Page, Storage) * idf(Word, Storage) || Word <- Words]).

tf(Word, Page, Storage) ->
    PageWords = Storage:get_words(Page),
    WordCount = length(lists:filter(fun(W) -> W == Word end, PageWords)),
    WordCount / length(PageWords).

idf(Word, Storage) ->
    PageCount = sets:size(Storage:get_pages_from_index(Word)),
    math:log(Storage:get_page_count() / (1 + PageCount)).
