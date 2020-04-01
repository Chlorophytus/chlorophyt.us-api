%%%-------------------------------------------------------------------
%% @doc chlorophytus asynchronous database engine
%%
%%      this ensures mysql/otp doesn't bring everything down due to it
%%      being not up-to-date anymore
%%
%%      if everything fails then I may have to fork mysql/otp
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_asyncdb).

-include("chlorophytus.hrl").

-behaviour(gen_statem).

-export([start_link/1]).

-export([callback_mode/0, init/1]).

-export([idle/3, running/3]).

-define(SERVER, ?MODULE).

start_link(Data) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, Data,
			  []).

callback_mode() -> state_functions.

init(#{ip := IP, user := User, pass := Pass} =
	 Data) ->
    {ok, MySQL} = mysql:start_link([{host, IP},
				    {user, User}, {password, Pass}, {database, "ChlorophytusSchema"}]),
    {ok, idle,
     Data#{mysql => MySQL, q => queue:new(), q_count => 0}}.

%% Idling okay.
idle(cast, {asyncdb, Req}, #{q := Q} = Data) ->
    {next_state, running,
     Data#{q => queue:cons(Req, Q), q_count => 1},
     {next_event, internal, pop}}.

%% Fulfill state request
running(cast, {asyncdb, Req},
	#{q := Q, q_count := QC} = Data)
    when QC < 16 ->
    {keep_state,
     Data#{q => queue:cons(Req, Q), q_count => QC + 1}};
%% Handling right now...
running(internal, pop,
	#{q := Q, q_count := QC, mysql := ChC} = Data) when QC > 0 ->
    case queue:daeh(Q) of
      empty -> keep_state_and_data;
      #{pid := P, req := R} ->
	  {ok, Cols, Rows} = case R of
			       {load, {motd, most_recent}} ->
				   mysql:query(ChC,
					       <<"SELECT `title`,`text`, MAX(`date_posted`) "
						 "FROM `ChlorophytusSchema`.`BlogTable` "
						 "WHERE `is_motd` = 1;">>);
			       {load, {side, most_recent}} ->
				   mysql:query(ChC,
					       <<"SELECT `title`,`text`, MAX(`date_posted`) "
						 "FROM `ChlorophytusSchema`.`BlogTable` "
						 "WHERE `is_motd` = 0;">>)
			     end,
	  P ! {ok, {asyncdb, {Cols, Rows}}},
	  {keep_state,
	   Data#{q => queue:init(Q), q_count => QC - 1},
	   {next_event, internal, pop}}
    end;
running(internal, pop, Data) ->
        {next_state, idle, Data};
%% Fulfill overloadish state request
running(cast, {asyncdb, #{pid := P} = Req}, _Data) ->
    error_logger:warning_msg("rejecting request ~p due to overload...~n",
			     [Req]),
    P ! {error, {asyncdb, overloaded}},
    keep_state_and_data.