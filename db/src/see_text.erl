-module(see_text).

-export([extract_words/1]).

-spec extract_words(iodata()) -> list(binary()).
extract_words(Data) ->
    StrippedData = strip_nonalpha_characters(Data),
    Words = split_to_words(StrippedData),
    lists:filtermap(fun process_word/1, Words).

-spec strip_nonalpha_characters(iodata()) -> iodata().
strip_nonalpha_characters(Data) ->
    Separators = "[@!#$%^&*()\\-_=+\\]\\[\\\\{}|;:\\\"',.<>/?]",
    re:replace(Data, Separators, <<" ">>, [global]).

-spec split_to_words(iodata()) -> list(binary()).
split_to_words(Data) ->
    re:split(Data, <<"[ \t\n\r]">>, [{return, binary}]).

-spec process_word(binary()) -> false | {ok, binary()}.
process_word(Word) when size(Word) < 2 ->
    false;

process_word(Word) ->
    try
        {true, unistring:to_lower(Word)}
    catch
        _:_ ->
            false
    end.

