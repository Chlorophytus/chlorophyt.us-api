%%%-------------------------------------------------------------------
%% @doc chlorophytus text page handler
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_textpage).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([to_json/2]).

-export([to_text/2]).

%% INITIALIZE REST
init(Req, [State]) ->
    ID = cowboy_req:binding(id, Req),
    {cowboy_rest, Req,
     [State#{t0 => chlorophytus_date:now(), id => ID}]}.

%% REST CALLBACKS
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, '*'}, to_text},
      {{<<"application">>, <<"json">>, '*'}, to_json}],
     Req, State}.

%% FINALIZE REST
to_json(Req, [#{t0 := T0}] = State) ->
    Span = chlorophytus_date:get_span(null,
				      chlorophytus_date:now(), T0),
    {mochijson2:encode([{<<"text">>,
			 <<"this does nothing">>},
			{<<"time">>,
			 chlorophytus_date:stringify_to_iolist(Span)}]),
     Req, State}.

to_text(Req, [#{t0 := T0}] = State) ->
    Span = chlorophytus_date:get_span(null,
				      chlorophytus_date:now(), T0),
    T =
	iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span)),
    {<<"this text does nothing. time: ", T/binary, "\r\n">>,
     Req, State}.
