%%%-------------------------------------------------------------------
%% @doc chlorophytus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chlorophytus_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-export([insert/2]).

-define(SERVER, ?MODULE).

start_link(Map) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Map]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Map]) ->
    SupFlags = #{strategy => one_for_one, intensity => 0,
		 period => 2},
    I = maps:iterator(Map),
    ChildSpecs = insert(maps:next(I), []),
    {ok, {SupFlags, ChildSpecs}}.

%% No need to launch if we don't need to
insert({K, ignore, I}, Tail) ->
    error_logger:info_msg("not launching ~p...~n", [K]),
    insert(maps:next(I), Tail);
%% Other cases, yes
insert({K, V, I}, Tail) ->
    error_logger:info_msg("launching ~p as attr ~p...~n",
			  [V, K]),
    insert(maps:next(I), [#{id => K, start => V} | Tail]);
%% Done iterating
insert(none, Tail) ->
    error_logger:info_msg("...launching complete~n"), Tail.