%%%-------------------------------------------------------------------
%% @doc chlorophytus MOTD page handler
%% @end
%%%-------------------------------------------------------------------
-module(chlorophytus_motdpage).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([options/2]).

-export([to_json/2]).

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
				   <<"*">>, Req1),
    Req3 =
	cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
				   <<"https://chlorophyt.us">>, Req2),
    {ok, Req3, State}.

%% FINALIZE REST
to_json(Req0, [#{t0 := T0}] = State) ->
    ok = gen_statem:cast(chlorophytus_asyncdb,
			 {asyncdb, #{pid => self(), req => {load, motd, []}}}),
    Req1 =
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
				   <<"https://chlorophyt.us">>, Req0),
    {_, _, Vsn} = lists:keyfind(chlorophytus, 1,
				application:loaded_applications()),
    Response = receive
		 {ok, {asyncdb, SQL}} ->
		     {_Rows, MOTD} = SQL,
		     case MOTD of
		       [[Date, Title, Text]] ->
			   Span = chlorophytus_date:get_span(null,
							     chlorophytus_date:now(),
							     T0),
			   mochijson2:encode([{<<"time">>,
					       iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span))},
					      {<<"version">>,
					       list_to_binary(Vsn)},
					      {<<"data">>,
					       {[{<<"date">>,
						  iso8601:format(Date)},
						 {<<"title">>, Title},
						 {<<"text">>, Text}]}}]);
		       [] ->
			   Span = chlorophytus_date:get_span(null,
							     chlorophytus_date:now(),
							     T0),
			   mochijson2:encode([{<<"time">>,
					       iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span))},
					      {<<"version">>,
					       list_to_binary(Vsn)},
					      {<<"data">>, null}])
		     end
		 after 1000 ->
			   Span = chlorophytus_date:get_span(null,
							     chlorophytus_date:now(),
							     T0),
			   mochijson2:encode([{<<"e">>, <<"timed_out">>},
					      {<<"time">>,
					       iolist_to_binary(chlorophytus_date:stringify_to_iolist(Span))},
					      {<<"version">>,
					       list_to_binary(Vsn)}])
	       end,
    {Response, Req1, State}.