%%%-------------------------------------------------------------------
%% @doc chlorophytus public API
%% @end
%%%-------------------------------------------------------------------

-module(chlorophytus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start Cowboy and friends
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(mysql),
    % Dispatcher
    Dispatch = cowboy_router:compile([{'_',
				       [{"/v0_2/list/:id/[:sub_id]",
					 chlorophytus_listpage, [#{}]},
					{"/v0_2/text/:id",
					 chlorophytus_textpage, [#{}]},
					{"/v0_2/ping", chlorophytus_ackpage,
					 [#{}]}]}]),
    % Get IP to connect to, otherwise connecting by localhost is fine
    {ok, [{ip, IP}, {user, User}, {pass, Pass}], _} =
	file:path_consult([code:priv_dir(chlorophytus)],
			  "mysql.txt"),
    chlorophytus_sup:start_link(#{argv_cowboy =>
				      {cowboy, start_clear,
				       [http, [{port, 8080}],
					#{env => #{dispatch => Dispatch}}]},
				  argv_asyncdb =>
				      {chlorophytus_asyncdb, start_link,
				       [#{ip => IP, user => User,
					  pass => Pass}]}}).

stop(_State) -> ok.

%% internal functions

