%%%-------------------------------------------------------------------
%% @doc chlorophytus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chlorophytus_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-export([ensure_lastly/0]).

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
    SupFlags = #{strategy => one_for_one, intensity => 2,
		 period => 3},
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

ensure_lastly() ->
    % add in these after everything's been done.
    Dispatch = cowboy_router:compile([{'_',
				       [{"/v0_3/motd", chlorophytus_motdpage,
					 [#{}]},
					{"/v0_3/pages/[:at]",
					 chlorophytus_textpage, [#{}]},
					{"/v0_3/ping", chlorophytus_ackpage,
					 [#{}]}]}]),
    supervisor:start_child(chlorophytus_sup,
			 #{id => tail_cowboy,
			   start =>
			       {cowboy, start_clear,
				[http, [{port, 8080}],
				 #{env => #{dispatch => Dispatch}}]}}).
