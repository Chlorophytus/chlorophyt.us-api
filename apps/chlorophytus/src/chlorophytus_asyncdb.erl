%%%-------------------------------------------------------------------
%% @doc chlorophytus asynchronous database engine
%%
%%      this ensures eredis doesn't bring everything down due to it
%%      being not up-to-date anymore
%%
%%      if everything fails then I may have to fork eredis
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_asyncdb).

-include("chlorophytus.hrl").

-behaviour(gen_statem).

-export([init/1]).

-export([callback_mode/0]).

init(_Args)-> unimplemented.
callback_mode() -> state_functions.