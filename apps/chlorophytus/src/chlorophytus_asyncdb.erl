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

-export([start_link/1]).

-export([callback_mode/0, init/1, terminate/3]).

-export([idle/3, running/3]).

-define(SERVER, ?MODULE).

start_link(IData) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE,
			  [IData], []).

callback_mode() -> state_functions.

init([#{ip := IP}] = IData) ->
    INonce = integer_to_binary(erlang:unique_integer(), 36),
    ChID = <<"eredis-unique-", INonce/binary>>,
    {ok, ChC} = supervisor:start_child(chlorophytus_sup,
				       #{id => ChID,
					 start => {eredis, start_link, [IP]}}),
    {ok, idle,
     IData#{c => ChC, c_id => ChID, q => queue:new(),
	    q_count => 0}}.

idle(cast, {asyncdb, Req}, #{q := Q} = Data) ->
    % do redis stuff
    {next_state, running,
     Data#{q => queue:cons(Req, Q), q_count => 1}}.

running(cast, {asyncdb, Req},
	#{q := Q, q_count := QC} = Data)
    when QC < 16 ->
    {keep_state, Data#{q => queue:cons(Req, Q), q_count => QC + 1},
     {next_event, internal, pop}};
running(internal, pop, #{q := Q, c := ChC, q_count := QC} = Data) ->
    case queue:daeh(Q) of
      empty -> keep_state_and_data;
      #{pid := P, req := R} ->
	  {ok, Result} = eredis:q(ChC, [<<"PING">>, R]),
	  P ! {ok, {asyncdb, Result}},
	  {keep_state, Data#{q => queue:init(Q), q_count => QC - 1},
	   {next_event, internal, pop}}
    end;
running(cast, {asyncdb, #{pid := P} = Req}, _Data) ->
    error_logger:warning_msg("rejecting request ~p due to overload...~n",
			     [Req]),
    P ! {error, {asyncdb, overloaded}},
    keep_state_and_data.

terminate(_Reason, _State, #{c_id := ChID}) ->
    error_logger:info_msg("removing temporary eredis instance ~p...~n",
			  [ChID]),
    supervisor:terminate_child(chlorophytus_sup, ChID),
    supervisor:delete_child(chlorophytus_sup, ChID).
