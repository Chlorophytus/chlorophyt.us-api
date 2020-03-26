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
    {ok, _} = application:ensure_all_started(eredis),
    % Dispatcher
    Dispatch = cowboy_router:compile([{'_',
				       [{"/v0_1/list/:id/[:sub_id]",
					 chlorophytus_listpage, [#{}]},
					{"/v0_1/text/:id",
					 chlorophytus_textpage, [#{}]}]}]),
    % Get IP to connect to, otherwise connecting by localhost is fine
    {ok, IP} = case application:get_env(chlorophytus, ip) of
		 undefined -> {ok, "127.0.0.1"};
		 IPTuple -> IPTuple
	       end,
    chlorophytus_sup:start_link(#{argv_cowboy =>
				      {cowboy, start_clear,
				       [http, [{port, 8080}],
					#{env => #{dispatch => Dispatch}}]},
				  argv_asyncdb =>
				      {chlorophytus_asyncdb, start_link, [#{ip => IP}]}}).

stop(_State) -> ok.

%% internal functions

