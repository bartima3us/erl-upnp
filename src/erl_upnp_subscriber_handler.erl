%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% HTTP server to handle subscription callbacks.
%%% @end
%%% Created : 12. Jun 2019 00.57
%%%-------------------------------------------------------------------
-module(erl_upnp_subscriber_handler).
-author("bartimaeus").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([
    start_http/2
]).

%% Cowboy callbacks
-export([
    init/2
]).

-record(state, {
    event_mgr_pid   :: pid()
}).


%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Start HTTP server.
%%
start_http(EventMgrPid, HttpId) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/:action", ?MODULE, #state{event_mgr_pid = EventMgrPid}}
        ]}
    ]),
    cowboy:start_clear(HttpId, [{port, 0}], #{
        env => #{dispatch => Dispatch}
    }).



%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%%  @doc
%%  Cowboy HTTP server init.
%%
init(Req0, State = #state{event_mgr_pid = EventMgrPid}) ->
    ok = case cowboy_req:method(Req0) of
        <<"NOTIFY">> ->
            {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
            SID = cowboy_req:header(<<"sid">>, Req1),
            SEQ = cowboy_req:header(<<"seq">>, Req1),
            Peer = cowboy_req:peer(Req1),
            {ParsedResp, _} = xmerl_scan:string(binary_to_list(ReqBody)),
            PropertySet = xmerl_xpath:string("//e:propertyset/e:property", ParsedResp),
            lists:foreach(
                fun
                    (#xmlElement{name = 'e:property', content = [#xmlElement{name = Var, content = [#xmlText{value = Val}]}]}) ->
                        Event = {state_var, Var, Val, [{"SEQ", binary_to_integer(SEQ)}, {"host", Peer}, {"SID", SID}]},
                        gen_event:notify(EventMgrPid, Event);
                    (_) ->
                        ok
                end,
                PropertySet
            );
        _Other       ->
            ok
    end,
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"">>, Req0),
    {ok, Req2, State}.


