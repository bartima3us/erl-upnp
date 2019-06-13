%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2019 00.57
%%%-------------------------------------------------------------------
-module(erl_upnp_subscriber_handler).
-author("bartimaeus").

%% API
-export([
    start_http/0
]).

%% Cowboy callbacks
-export([
    init/2
]).



%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Start HTTP server.
%%
start_http() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/:action", ?MODULE, []}
        ]}
    ]),
    cowboy:start_clear(upnp_callback, [{port, 0}], #{
        env => #{dispatch => Dispatch}
    }).



%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%%  @doc
%%  Cowboy HTTP server init.
%%
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"NOTIFY">> ->
            {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
            io:format("xxxxxxx ReqBody=~p~n~n", [ReqBody]),
            io:format("xxxxxxx Req=~p", [Req1]);
        Other        ->
            io:format("xxxxxxx Unknown method=~p", [Other])
    end,
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"">>, Req0),
    {ok, Req2, State}.


