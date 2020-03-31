%%%-------------------------------------------------------------------
%% @doc chlorophytus text page handler
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_textpage).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([options/2]).

-export([to_json/2]).

%% INITIALIZE REST
init(Req, [State]) ->
    ID = cowboy_req:binding(id, Req),
    {cowboy_rest, Req,
     [State#{t0 => chlorophytus_date:now(), id => ID}]}.

%% REST CALLBACKS
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req,
     State}.

options(Req0, State) ->
    Req1 =
	cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
				   <<"GET, OPTIONS">>, Req0),
    Req2 =
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
				   <<"*">>, Req1),
    Req3 =
	cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
				   <<"*">>, Req2),
    {ok, Req3, State}.

%% FINALIZE REST
to_json(Req0, [#{t0 := T0}] = State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
				   <<"https://chlorophyt.us">>, Req0),
    Span = chlorophytus_date:get_span(null,
				      chlorophytus_date:now(), T0),
    {mochijson2:encode([{<<"text">>,
			 <<"this does nothing">>},
			{<<"time">>,
			 chlorophytus_date:stringify_to_iolist(Span)}]),
     Req1, State}.
