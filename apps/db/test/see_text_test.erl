-module(see_text_test).
-include_lib("eunit/include/eunit.hrl").

split_words_test_() ->
    [?_assertEqual([<<"aaa">>], see_text:extract_words(<<"aaa">>)),
     ?_assertEqual([<<"aaa">>], see_text:extract_words([<<"aaa">>])),
     ?_assertEqual([<<"aabb">>], see_text:extract_words([<<"aa">>, [<<"bb">>]])),
     ?_assertEqual([<<"aa">>, <<"bb">>], see_text:extract_words(<<"aa bb">>)),
     ?_assertEqual([<<"aa1">>, <<"b2b">>], see_text:extract_words(<<"aa1 b2b">>)),
     ?_assertEqual([<<"aa">>, <<"bb">>], see_text:extract_words([<<"aa">>, <<" \n\t\r">>, <<"bb">>])),
     ?_assertEqual([<<"aa">>, <<"bb">>], see_text:extract_words(<<"<[(aa)]>!@#$%^&*.,.|;:'\"?/={bb}">>))].

normalize_words_test_() ->
     ?_assertMatch([<<"zażółć"/utf8>>, <<"gęślą"/utf8>>, <<"jaźń"/utf8>>],
                    see_text:extract_words(<<"ZAŻÓŁĆ GĘŚLĄ JAŹŃ"/utf8>>)).

skip_one_letter_words_test_() ->
     ?_assertMatch([<<"aaa">>, <<"bb">>], see_text:extract_words(<<"x aaa a f bb t">>)).
