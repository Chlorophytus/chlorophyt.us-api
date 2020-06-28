-record(chlorophytus_date_t, {
  dt :: calendar:datetime(),
  ms :: 0..999
}).

-record(chlorophytus_entry_t, {
  title :: binary(),
  description :: binary(),
  date :: calendar:datetime()
}).

-record(chlorophytus_folio_t, {
  title :: binary(),
  description :: binary(),
  date :: calendar:datetime(),
  link :: binary(),
  image :: binary()
}).

%% Limit of objects we can query. Set low so we don't flood the database.
-define(SQL_QUERY_LIMIT, 10).
-define(SQL_QUERIES_PER_PAGE, 5).

%% TODO: Do we need this?
-define(SQL_PAGES_LIMIT, 5).