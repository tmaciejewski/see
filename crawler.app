{application, crawler,
  [{description,  "Web crawlers for search engine"},
   {id,           "crawler"},
   {vsn,          "0.1"},
   {modules,      [crawler, crawler_sup, http]},
   {registered,   [crawler_sup]},
   {mod,          {crawler_sup, []}}]}.
