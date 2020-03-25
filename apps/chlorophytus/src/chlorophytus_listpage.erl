%%%-------------------------------------------------------------------
%% @doc chlorophytus lists page handler
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_listpage).
-export([init/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.