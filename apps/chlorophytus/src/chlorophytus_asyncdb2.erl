%%%-------------------------------------------------------------------
%% @doc chlorophytus asynchronous database engine v2
%%
%% 		ready for v0.4
%%
%%      this ensures mysql/otp doesn't bring everything down due to it
%%      being not up-to-date anymore
%%
%%      if everything fails then I may have to fork mysql/otp
%%
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_asyncdb2).

-behaviour(gen_statem).

-export([start_link/1]).

-export([callback_mode/0, init/1]).

-export([idle/3, running/3]).

-define(SERVER, ?MODULE).

-include("chlorophytus.hrl").

start_link(Data) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, Data,
			  []).

callback_mode() -> state_functions.

init(#{ip := IP, user := User, pass := Pass} = Data) ->
    {ok, MySQL} = mysql:start_link([{host, IP},
				    {user, User}, {password, Pass}]),
    {ok, idle,
     Data#{mysql => MySQL, q => queue:new(), q_count => 0}}.

%% Idling okay.
idle(cast, {asyncdb2, Req}, #{q := Q} = Data) ->
    {next_state, running,
     Data#{q => queue:cons(Req, Q), q_count => 1},
     {next_event, internal, pop}}.

%% Fulfill state request
running(cast, {asyncdb2, Req},
	#{q := Q, q_count := QC} = Data)
    when QC < (?SQL_QUERY_LIMIT) ->
    {keep_state,
     Data#{q => queue:cons(Req, Q), q_count => QC + 1}};
%% Handling right now...
running(internal, pop,
	#{q := Q, q_count := QC, mysql := ChC} = Data)
    when QC > 0 ->
    case queue:daeh(Q) of
      empty -> keep_state_and_data;
      #{pid := P, req := R} ->
	  {atomic, List} = case R of
			     {count, folio} ->
				 mysql:transaction(ChC,
						   fun () ->
							   {ok, _, [[Count]]} =
							       mysql:query(ChC,
									   <<"SELECT COUNT(*) FROM `ChlorophytusSchema`.`Fo"
									     "lioTableEntries`">>),
							   Count
						   end);
			     {count, blog} ->
				 mysql:transaction(ChC,
						   fun () ->
							   {ok, _, [[Count]]} =
							       mysql:query(ChC,
									   <<"SELECT COUNT(*) FROM `ChlorophytusSchema`.`Bl"
									     "ogTableEntries`">>),
							   Count
						   end);
			     {count, motd} ->
				 mysql:transaction(ChC,
						   fun () ->
							   {ok, _, [[Count]]} =
							       mysql:query(ChC,
									   <<"SELECT COUNT(*) FROM `ChlorophytusSchema`.`MO"
									     "TDTableEntries`">>),
							   Count
						   end);
			     {get, blog, Pages}
				 when length(Pages) =< (?SQL_QUERY_LIMIT)
					andalso length(Pages) > 0 ->
				 %  SPages = lists:seq(Page * (?SQL_QUERY_LIMIT),
				 % 		    (Page + 1) *
				 % 		      (?SQL_QUERY_LIMIT)),
				 % Return the pages, modified into records
				 %
				 % We prevent floods by limiting object queries with guards.
				 mysql:transaction(ChC,
						   fun () ->
							   lists:map(fun
								       (Page) ->
									   {ok,
									    _,
									    SQLObj} =
									       mysql:query(ChC,
											   <<"SELECT `title`, `description`, `date` "
											     "FROM `ChlorophytusSchema`.`BlogTableEntries` "
											     "WHERE `id` = ?;\n">>,
											   [Page]),
									   case
									     SQLObj
									       of
									     [] ->
										 [];
									     [[Title,
									      Description,
									      Date]] ->
										 #chlorophytus_entry_t{title
													   =
													   Title,
												       description
													   =
													   Description,
												       date
													   =
													   Date}
									   end
								     end,
								     Pages)
						   end);
			     {get, folio, Projs}
				 when length(Projs) =< (?SQL_QUERY_LIMIT)
					andalso length(Projs) > 0 ->
				 mysql:transaction(ChC,
						   fun () ->
							   lists:map(fun
								       (Proj) ->
									   {ok,
									    _,
									    SQLObj} =
									       mysql:query(ChC,
											   <<"SELECT `title`, `description`, `date`, "
											     "`link`, `image` FROM `ChlorophytusSchema`.`Fo"
											     "lioTableEntries` WHERE `id` = ?">>,
											   [Proj]),
									   case
									     SQLObj
									       of
									     [] ->
										 [];
									     [[Title,
									      Description,
									      Date,
									      Link,
									      Image]] ->
										 #chlorophytus_folio_t{title
													   =
													   Title,
												       description
													   =
													   Description,
												       date
													   =
													   Date,
												       link
													   =
													   Link,
												       image
													   =
													   Image}
									   end
								     end,
								     Projs)
						   end);
			     {get, motd, MOTDs}
				 when length(MOTDs) =< (?SQL_QUERY_LIMIT)
					andalso length(MOTDs) > 0 ->
				 mysql:transaction(ChC,
						   fun () ->
							   lists:map(fun
								       (MOTD) ->
									   {ok,
									    _,
									    SQLObj} =
									       mysql:query(ChC,
											   <<"SELECT `title`, `description`, `date` "
											     "FROM `ChlorophytusSchema`.`MOTDTableEntries` "
											     "WHERE `id` = ?">>,
											   [MOTD]),
									   case
									     SQLObj
									       of
									     [] ->
										 [];
									     [[Title,
									      Description,
									      Date]] ->
										 #chlorophytus_entry_t{title
													   =
													   Title,
												       description
													   =
													   Description,
												       date
													   =
													   Date}
									   end
								     end,
								     MOTDs)
						   end);
			     {get, _, []} -> {atomic, []}
			   end,
	  P ! {ok, {asyncdb2, List}},
	  {keep_state,
	   Data#{q => queue:init(Q), q_count => QC - 1},
	   {next_event, internal, pop}}
    end;
running(internal, pop, Data) ->
    {next_state, idle, Data};
%% Fulfill overloadish state request
running(cast, {asyncdb2, #{pid := P} = Req}, _Data) ->
    error_logger:warning_msg("rejecting request ~p due to overload...~n",
			     [Req]),
    P ! {error, {asyncdb2, overloaded}},
    keep_state_and_data.
