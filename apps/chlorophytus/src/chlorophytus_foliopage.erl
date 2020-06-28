%%%-------------------------------------------------------------------
%% @doc chlorophytus portfolio page handler
%%
%% 		ready for v0.4
%%
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_foliopage).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([options/2]).

-export([to_json/2]).

-include("chlorophytus.hrl").

%% INITIALIZE REST
init(Req, [State]) ->
    ID = cowboy_req:binding(at, Req),
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
				   <<"https://chlorophyt.us">>, Req1),
    Req3 =
	cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
				   <<"*">>, Req2),
    {ok, Req3, State}.

%% GATHER LIST ENTRIES
gather(In) -> gather(In, []).

gather([InHead | InTail], OutTail) when InHead =:= [] ->
    gather(InTail, OutTail);

gather([InHead | InTail], OutTail) ->
    OutHead = [{<<"date">>,
		iso8601:format(InHead#chlorophytus_folio_t.date)},
	       {<<"title">>, InHead#chlorophytus_folio_t.title},
	       {<<"description">>,
		InHead#chlorophytus_folio_t.description},
		{<<"link">>, InHead#chlorophytus_folio_t.link},
		{<<"image">>, InHead#chlorophytus_folio_t.image}],
    gather(InTail, [OutHead | OutTail]);
gather([], Out) -> Out.

%% FINALIZE REST
to_json(Req0, [#{t0 := T0, id := undefined}] = State) ->
    ok = gen_statem:cast(chlorophytus_asyncdb2,
			 {asyncdb2, #{pid => self(), req => {count, folio}}}),
    Req1 =
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
				   <<"https://chlorophyt.us">>, Req0),
    {_, _, Vsn} = lists:keyfind(chlorophytus, 1,
				application:loaded_applications()),
    Response = receive
		 {ok, {asyncdb2, Count}} ->
		     Span = chlorophytus_date:get_span(null,
						       chlorophytus_date:now(),
						       T0),
		     mochijson2:encode([{<<"latency">>,
					 iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span))},
					{<<"version">>, list_to_binary(Vsn)},
					{<<"count">>, Count}])
		 after 1000 ->
			   Span = chlorophytus_date:get_span(null,
							     chlorophytus_date:now(),
							     T0),
			   mochijson2:encode([{<<"e">>, <<"timed_out">>},
					      {<<"latency">>,
					       iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span))},
					      {<<"version">>,
					       list_to_binary(Vsn)}])
	       end,
    {Response, Req1, State};
to_json(Req0, [#{t0 := T0, id := RawID}] = State) ->
	ID = binary_to_integer(RawID),
    Entries = lists:seq(ID * (?SQL_QUERIES_PER_PAGE),
			((ID + 1) * (?SQL_QUERIES_PER_PAGE) - 1)),
    ok = gen_statem:cast(chlorophytus_asyncdb2,
			 {asyncdb2,
			  #{pid => self(), req => {get, folio, Entries}}}),
    Req1 =
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
				   <<"https://chlorophyt.us">>, Req0),
    {_, _, Vsn} = lists:keyfind(chlorophytus, 1,
				application:loaded_applications()),
    Response = receive
		 {ok, {asyncdb2, Records}} ->
		     Gathered = gather(Records),
		     Span = chlorophytus_date:get_span(null,
						       chlorophytus_date:now(),
						       T0),
		     mochijson2:encode([{<<"latency">>,
					 iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span))},
					{<<"version">>, list_to_binary(Vsn)},
					{<<"data">>, Gathered}])
		 after 1000 ->
			   Span = chlorophytus_date:get_span(null,
							     chlorophytus_date:now(),
							     T0),
			   mochijson2:encode([{<<"e">>, <<"timed_out">>},
					      {<<"latency">>,
					       iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span))},
					      {<<"version">>,
					       list_to_binary(Vsn)}])
	       end,
    {Response, Req1, State}.
