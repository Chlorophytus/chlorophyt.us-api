%%%-------------------------------------------------------------------
%% @doc chlorophytus public API
%% @end
%%%-------------------------------------------------------------------

-module(chlorophytus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start Cowboy and friends
    {ok, _} = application:ensure_all_started(mysql),
    % Get IP to connect to, otherwise connecting by localhost is fine
    {ok, [{ip, IP}, {user, User}, {pass, Pass}], _} =
	file:path_consult([code:priv_dir(chlorophytus)],
			  "mysql.txt"),
    chlorophytus_sup:start_link(#{argv_websup =>
				      {chlorophytus_web_sup, start_link, []},
				  argv_asyncdb =>
				      {chlorophytus_asyncdb, start_link,
				       [#{ip => IP, user => User,
					  pass => Pass}]}}),
    chlorophytus_sup:ensure_lastly().

stop(_State) -> ok.

%% internal functions

