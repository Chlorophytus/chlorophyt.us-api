-record(chlorophytus_date_t, {
  dt :: calendar:datetime(),
  ms :: 0..999
}).

-define(SQL_QUERY_LIMIT, 50).
-define(SQL_PAGES_LIMIT, 5).