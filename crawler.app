{application, crawler,
  [{description,  "Web crawlers for search engine"},
   {id,           "crawler"},
   {vsn,          "0.1"},
   {modules,      [crawler, crawler_sup, http]},
   {registered,   [crawler_sup]},
   {included_applications, [db]},
   {start_phases, [{db,[]}]},
   {mod,          {crawler_sup, []}}]}.
