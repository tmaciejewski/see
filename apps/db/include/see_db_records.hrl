-record(page, {id, url, title, content, last_visit = erlang:timestamp()}).
-record(index, {word, pages}).
