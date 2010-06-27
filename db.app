{application, db,
  [{description,  "Database for search engine"},
   {id,           "db"},
   {vsn,          "0.1"},
   {modules,      [db, db_imp]},
   {applications, [kernel, stdlib]},
   {registered,   [db]},
   {mod,          {db, []}}]}.
