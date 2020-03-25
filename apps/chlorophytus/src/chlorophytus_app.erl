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
    % Dispatcher
    Dispatch = cowboy_router:compile([{'_',
				       [{"/v0_1/list/:id/[:sub_id]",
					 chlorophytus_listpage, [#{}]},
					{"/v0_1/text/:id",
					 chlorophytus_textpage, [#{}]}]}]),
    % Start plain HTTP JSON, I may change this if browsers whine about HTTPS.
    {ok, _} = cowboy:start_clear(http, [{port, 8080}],
				 #{env => #{dispatch => Dispatch}}),
    chlorophytus_sup:start_link().

stop(_State) -> ok.

%% internal functions

