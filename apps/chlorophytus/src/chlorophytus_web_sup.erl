%%%-------------------------------------------------------------------
%% @doc chlorophytus web level supervisor, Ranch is very crashy
%%
%% 		ready for v0.4
%%
%% @end
%%%-------------------------------------------------------------------

-module(chlorophytus_web_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 2,
		 period => 3},
    ChildSpecs = [#{id => chlorophytus_web_cowboy,
		    start => {cowboy_sup, start_link, []}},
		  #{id => chlorophytus_web_ranch,
		    start => {ranch_sup, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.
