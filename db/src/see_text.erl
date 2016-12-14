-module(see_text).

-export([extract_words/1]).

extract_words(Data) ->
    StrippedData = strip_nonalpha_characters(Data),
    Words = split_to_words(StrippedData),
    lists:filtermap(fun process_word/1, Words).

strip_nonalpha_characters(Data) ->
    re:replace(Data, <<"[^A-Za-z0-9]">>, <<" ">>, [global]).

split_to_words(Data) ->
    re:split(Data, <<" ">>).

process_word(Word) when size(Word) < 2 ->
    false;

process_word(Word) ->
    try
        {true, unistring:to_lower(Word)}
    catch
        _:_ ->
            false
    end.

