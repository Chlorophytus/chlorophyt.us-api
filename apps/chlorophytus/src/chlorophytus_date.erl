%%%-------------------------------------------------------------------
%% @doc chlorophytus date system
%%      this was for another personal site I was working on in the
%%      past
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_date).

-include("chlorophytus.hrl").

-export([now/0]).

-export([stringify_to_iolist/1]).

-export([encode/2]).

-export([get_span/3]).

%% Get the current time.
now() ->
    TS = erlang:timestamp(),
    TSd = calendar:now_to_datetime(TS),
    {_, _, TSu} = TS,
    #chlorophytus_date_t{dt = TSd, ms = TSu div 1000}.

%% If you're lazy then you can use this in `erl` shell.
encode(DT, MS) ->
    stringify_to_iolist(#chlorophytus_date_t{dt = DT,
					     ms = MS}).

%% Encode a chlorophytus_date_t to an iolist
stringify_to_iolist(D) ->
    stringify_split(D#chlorophytus_date_t.dt,
		    D#chlorophytus_date_t.ms).

stringify_split({{0, 1, 1}, {0, 0, 0}}, 0) ->
    [<<"<1ms">>];
stringify_split({{0, 1, 1}, {0, 0, 0}}, Dms) ->
    [integer_to_binary(Dms), <<"ms">>];
stringify_split(Ddt, 0) ->
    stringify_split_without_ms(Ddt);
stringify_split(Ddt, Dms) ->
    [stringify_split_without_ms(Ddt), <<", ">>,
     integer_to_binary(Dms), <<"ms">>].

stringify_split_without_ms({{0, 1, 1},
			    {0, 0, DdtTS}}) ->
    [integer_to_binary(DdtTS), <<"sec">>];
stringify_split_without_ms({{0, 1, 1},
			    {0, DdtTM, DdtTS}}) ->
    [integer_to_binary(DdtTM), <<"min ">>,
     stringify_split_without_ms({{0, 1, 1}, {0, 0, DdtTS}})];
stringify_split_without_ms({{0, 1, 1},
			    {DdtTH, DdtTM, DdtTS}}) ->
    [integer_to_binary(DdtTH), <<"hr ">>,
     stringify_split_without_ms({{0, 1, 1},
				 {0, DdtTM, DdtTS}})];
%% HACK: Pathetic UNIX bias...
stringify_split_without_ms({{0, 1, DdtDD}, DdtT}) ->
    [integer_to_binary(DdtDD), <<"d, ">>,
     stringify_split_without_ms({{0, 1, 1}, DdtT})];
%% HACK: Pathetic UNIX bias...
stringify_split_without_ms({{0, DdtDM, DdtDD}, DdtT}) ->
    [integer_to_binary(DdtDM), <<"m ">>,
     stringify_split_without_ms({{0, 1, DdtDD}, DdtT})];
stringify_split_without_ms({{DdtDY, DdtDM, DdtDD},
			    DdtT}) ->
    [integer_to_binary(DdtDY), <<"y ">>,
     stringify_split_without_ms({{0, DdtDM, DdtDD}, DdtT})].

%% Get the span between two points in time and a constant
get_span(unix, D1, D0)
    when D1#chlorophytus_date_t.ms >=
	   D0#chlorophytus_date_t.ms ->
    SC = calendar:datetime_to_gregorian_seconds({{1970, 1,
						  1},
						 {0, 0, 0}}),
    S1 =
	calendar:datetime_to_gregorian_seconds(D1#chlorophytus_date_t.dt),
    S0 =
	calendar:datetime_to_gregorian_seconds(D0#chlorophytus_date_t.dt),
    #chlorophytus_date_t{dt =
			     calendar:gregorian_secondate_to_datetime(S1 - S0 -
									SC),
			 ms =
			     D1#chlorophytus_date_t.ms -
			       D0#chlorophytus_date_t.ms};
get_span(unix, D1, D0)
    when D1#chlorophytus_date_t.ms <
	   D0#chlorophytus_date_t.ms ->
    SC = calendar:datetime_to_gregorian_seconds({{1970, 1,
						  1},
						 {0, 0, 0}}),
    S1 =
	calendar:datetime_to_gregorian_seconds(D1#chlorophytus_date_t.dt),
    S0 =
	calendar:datetime_to_gregorian_seconds(D0#chlorophytus_date_t.dt),
    #chlorophytus_date_t{dt =
			     calendar:gregorian_secondate_to_datetime(S1 - S0 -
									SC
									- 1),
			 ms =
			     D1#chlorophytus_date_t.ms -
			       D0#chlorophytus_date_t.ms
			       + 1000};
get_span(unix, D1, D0) ->
    SC = calendar:datetime_to_gregorian_seconds({{1970, 1,
						  1},
						 {0, 0, 0}}),
    S1 =
	calendar:datetime_to_gregorian_seconds(D1#chlorophytus_date_t.dt),
    S0 =
	calendar:datetime_to_gregorian_seconds(D0#chlorophytus_date_t.dt),
    #chlorophytus_date_t{dt =
			     calendar:gregorian_secondate_to_datetime(S1 - S0 -
									SC),
			 ms = 0};
%% Get the span between two points in time and a constant
get_span(null, D1, D0)
    when D1#chlorophytus_date_t.ms >=
	   D0#chlorophytus_date_t.ms ->
    S1 =
	calendar:datetime_to_gregorian_seconds(D1#chlorophytus_date_t.dt),
    S0 =
	calendar:datetime_to_gregorian_seconds(D0#chlorophytus_date_t.dt),
    #chlorophytus_date_t{dt =
			     calendar:gregorian_secondate_to_datetime(S1 - S0),
			 ms =
			     D1#chlorophytus_date_t.ms -
			       D0#chlorophytus_date_t.ms};
get_span(null, D1, D0)
    when D1#chlorophytus_date_t.ms <
	   D0#chlorophytus_date_t.ms ->
    S1 =
	calendar:datetime_to_gregorian_seconds(D1#chlorophytus_date_t.dt),
    S0 =
	calendar:datetime_to_gregorian_seconds(D0#chlorophytus_date_t.dt),
    #chlorophytus_date_t{dt =
			     calendar:gregorian_secondate_to_datetime(S1 - S0 -
									1),
			 ms =
			     D1#chlorophytus_date_t.ms -
			       D0#chlorophytus_date_t.ms
			       + 1000};
get_span(null, D1, D0) ->
    S1 =
	calendar:datetime_to_gregorian_seconds(D1#chlorophytus_date_t.dt),
    S0 =
	calendar:datetime_to_gregorian_seconds(D0#chlorophytus_date_t.dt),
    #chlorophytus_date_t{dt =
			     calendar:gregorian_secondate_to_datetime(S1 - S0),
			 ms = 0}.
