%%%-------------------------------------------------------------------
%% @doc chlorophytus public API
%% @end
%%%-------------------------------------------------------------------

-module(chlorophytus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(cowboy),
    chlorophytus_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
