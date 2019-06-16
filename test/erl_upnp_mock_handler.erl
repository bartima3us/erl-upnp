%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Control point description and action mocks.
%%% @end
%%% Created : 08. Jun 2019 20.29
%%%-------------------------------------------------------------------
-module(erl_upnp_mock_handler).

%% API
-export([
    init/2,
    start/2
]).

-record(state, {
    resp_fun
}).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%%  @doc
%%  Cowboy mock server init.
%%
init(Req0, State = #state{resp_fun = RespFun}) ->
    {ok, #{action := Action}} = maps:find(bindings, Req0),
    Resp = RespFun(Action),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"plain/text">>}, Resp, Req0),
    {ok, Req, State}.



%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Start HTTP server.
%%
start(Port, RespFun) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/:action", ?MODULE, #state{resp_fun = RespFun}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear({http, self()}, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }).


