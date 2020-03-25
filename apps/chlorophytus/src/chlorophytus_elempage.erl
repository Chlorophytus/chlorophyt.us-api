%%%-------------------------------------------------------------------
%% @doc chlorophytus element page handler
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_elempage).

-export([init/2]).

-export([content_types_provided/2]).

-export([to_json/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, #{t0 := T0} = State) ->
    Span = chlorophytus_date:get_span(null,
				      chlorophytus_date:now(), T0),
    {mochijson2:encode([{<<"text">>,
			 <<"this does nothing">>},
			{<<"time">>,
			 chlorophytus_date:stringify_to_iolist(Span)}]),
     Req, State}.
