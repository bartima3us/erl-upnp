%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% UPnP subscription implementation.
%%% @end
%%% Created : 12. Jun 2019 00.57
%%%-------------------------------------------------------------------
-module(erl_upnp_subscriber).
-author("bartimaeus").
-include("erl_upnp.hrl").

-behaviour(gen_statem).

%% API
-export([
    start_link/0,
    subscribe/4,
    unsubscribe/2,
    get_event_mgr_pid/1,
    get_callback/1,
    get_subscriptions/1,
    stop/1
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

-type sub_param() ::
    {Param :: string(), Value :: term()}.

-type subscription() ::
    {ServiceType :: string(),
        {ParentUDN :: string(), [sub_param()]}}.

-define(DELAY(S), S * 1000 + 150).
-define(TIMEOUT, 5000).
-define(MAX_TTL, 4233600). % seconds (49 days)
-define(INFINITE_TTL, 10). % seconds (30 min)
-define(REFRESH_INFINITE, 9500). % milliseconds (29 min 59.5 s)

-record(state, {
    client_pid                      :: pid(),
    http_pid                        :: pid(),
    event_mgr_pid                   :: pid(),
    callback                        :: string(),
    subscriptions           = []    :: [subscription()]
}).



%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Starts the server.
%%
-spec start_link() ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start_link() ->
    gen_statem:start_link(?MODULE, [], []).


%%  @doc
%%  Subscribe state variable changes.
%%
-spec subscribe(
    Pid         :: pid(),
    Service     :: string(),
    StateVars   :: [string()],
    TTL         :: pos_integer() | infinite
) ->
    [subscription()].

subscribe(Pid, Service, StateVars, TTL) ->
    gen_statem:call(Pid, {subscribe, Service, StateVars, TTL}).


%%  @doc
%%  Unsubscribe state variable changes.
%%
-spec unsubscribe(
    Pid         :: pid(),
    Service     :: string()
) ->
    ok |
    {error, no_subscription}.

unsubscribe(Pid, Service) ->
    gen_statem:call(Pid, {unsubscribe, Service}).


%%  @doc
%%  Returns UDP port of the control point.
%%
-spec get_callback(
    Pid :: pid()
) ->
    string().

get_callback(Pid) ->
    gen_statem:call(Pid, get_callback).


%%  @doc
%%  Returns event manager pid.
%%
-spec get_event_mgr_pid(
    Pid :: pid()
) ->
    pid().

get_event_mgr_pid(Pid) ->
    gen_statem:call(Pid, get_event_mgr_pid).


%%  @doc
%%  Returns all subscriptions.
%%
-spec get_subscriptions(
    Pid :: pid()
) ->
    pid().

get_subscriptions(Pid) ->
    gen_statem:call(Pid, get_subscriptions).


%%  @doc
%%  Stops the client.
%%
-spec stop(
    Pid :: pid()
) ->
    ok.

stop(Pid) ->
    gen_statem:stop(Pid).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%
%%
%%
init([]) ->
    TTL = 2,
    HttpId = {upnp_callback, self()},
    {ok, ClientPid} = erl_upnp_client:start_discover_link(TTL, ssdp_all),
    {ok, EventMgrPid} = gen_event:start_link(),
    {ok, HttpPid} = erl_upnp_subscriber_handler:start_http(EventMgrPid, HttpId),
    true = link(HttpPid),
    Port = ranch:get_port(HttpId),
    IP = erl_upnp_helper:get_internal_ip(),
    Callback = "http://" ++ inet:ntoa(IP) ++ ":" ++ integer_to_list(Port) ++ "/callback",
    State = #state{
        http_pid        = HttpPid,
        client_pid      = ClientPid,
        event_mgr_pid   = EventMgrPid,
        callback        = Callback
    },
    {ok, waiting, State, [{next_event, internal, start}]}.


%%
%%
%%
callback_mode() ->
    [handle_event_function].



%%%===================================================================
%%% States
%%%===================================================================

%--------------------------------------------------------------------
%   Waiting state
%
handle_event(internal, start, waiting, _SD) ->
    keep_state_and_data;

%
%
handle_event({call, _From}, {subscribe, Service, _StateVars, _TTL}, waiting, SD = #state{client_pid = Pid}) ->
    case get_client_state(Pid, Service) of
        still_discovering -> {keep_state_and_data, [postpone, {state_timeout, 1000, {get_services, Service}}]};
        ok                -> {next_state, open, SD, [postpone]}
    end;

%
%
handle_event({call, From}, {unsubscribe, _Service}, waiting, _SD) ->
    {keep_state_and_data, [{reply, From, {error, no_subscription}}]};

%
%
handle_event(state_timeout, {get_services, Service}, waiting, SD = #state{client_pid = Pid}) ->
    case get_client_state(Pid, Service) of
        still_discovering -> {keep_state_and_data, [{state_timeout, 1000, {get_services, Service}}]};
        ok                -> {next_state, open, SD}
    end;

%--------------------------------------------------------------------
%   Open state
%
handle_event(Request, Msg = {subscribe, Service, StateVars, TTL}, open, SD) when
    element(1, Request) =:= call;
    element(1, Request) =:= timeout
    ->
    #state{
        client_pid      = ClientPid,
        callback        = Callback,
        subscriptions   = CurrSubs,
        event_mgr_pid   = EventMgrPid
    } = SD,
    Timeout = encode_args(ttl, TTL),
    {ok, FoundServices} = erl_upnp_client:find_entity(ClientPid, Service),
    ServiceStateVars = lists:map(fun (Srv) -> {Srv, StateVars} end, FoundServices),
    Res = make_subscription(ServiceStateVars, Callback, Timeout),
    TimeoutActions = get_timeout_actions(TTL, Msg, Res),
    {NewSubs, ReplyAction} = case Request of
        {call, From} ->
            {CurrSubs ++ Res, [{reply, From, Res}]};
        {timeout, {refresh_subscription, ParentUDN, OldSID}} ->
            NewSID = find_sid_by_parent_udn(Res, ParentUDN),
            gen_event:notify(EventMgrPid, {subscription_refresh, OldSID, NewSID}),
            {remove_subscription(CurrSubs, OldSID) ++ Res, []}
    end,
    {keep_state, SD#state{subscriptions = NewSubs}, ReplyAction ++ TimeoutActions};

%
%
handle_event({call, From}, {unsubscribe, _Service}, open, SD = #state{subscriptions = Subs}) ->
    Res = ok,
    {keep_state, SD#state{subscriptions = Subs}, [{reply, From, Res}]};

%
%
handle_event({timeout, remove_subscription}, SID, open, SD) ->
    #state{
        subscriptions = Subs,
        event_mgr_pid = EventMgrPid
    } = SD,
    NewSubs = remove_subscription(Subs, SID),
    gen_event:notify(EventMgrPid, {subscription_timeout, SID}),
    {keep_state, SD#state{subscriptions = NewSubs}};

%--------------------------------------------------------------------
%   All state events
%
handle_event({call, From}, get_callback, _, #state{callback = Callback}) ->
    {keep_state_and_data, [{reply, From, Callback}]};

%
%
handle_event({call, From}, get_event_mgr_pid, _, #state{event_mgr_pid = Pid}) ->
    {keep_state_and_data, [{reply, From, Pid}]};

%
%
handle_event({call, From}, get_subscriptions, _, #state{subscriptions = Subscriptions}) ->
    {keep_state_and_data, [{reply, From, Subscriptions}]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%  @private
%%  Encode arguments.
%%
encode_args(ttl, infinite) -> ?INFINITE_TTL;
encode_args(ttl, Secs) when Secs =< ?MAX_TTL -> Secs.


%%  @private
%%  Find subscription by parent UDN.
%%
find_sid_by_parent_udn(Res, UDN) ->
    lists:foldl(
        fun
            ({_Srv, {ParentUDN, Data}}, false) when ParentUDN =:= UDN ->
                proplists:get_value("SID", Data);
            (_Sub, Found) ->
                Found
        end,
        false,
        Res
    ).


%%  @private
%%  Remove subscription from state by SID.
%%
remove_subscription(Subs, SID) ->
    lists:filter(
        fun ({_Srv, {_ParentUDN, Data}}) ->
            case proplists:get_value("SID", Data) of
                SID     -> false;
                _Other  -> true
            end
        end,
        Subs
    ).

%%  @private
%%  Create a timers.
%%
get_timeout_actions(TTL, {subscribe, Service, StateVars, TTL}, Res) ->
    case TTL of
        infinite ->
            lists:foldl(
                fun ({_Srv, {ParentUDN, Data}}, Acc) ->
                    SID = proplists:get_value("SID", Data),
                    [{{timeout, {refresh_subscription, ParentUDN, SID}}, ?REFRESH_INFINITE, {subscribe, Service, StateVars, TTL}} | Acc]
                end,
                [],
                Res
            );
        TTL      ->
            lists:foldl(
                fun ({_Srv, {_ParentUDN, Data}}, Acc) ->
                    SID = proplists:get_value("SID", Data),
                    [{{timeout, remove_subscription}, TTL * 1000, SID} | Acc]
                end,
                [],
                Res
            )
    end.


%%  @private
%%  Get SSDP client state.
%%
get_client_state(Pid, Service) ->
    case erl_upnp_client:find_entity(Pid, Service) of
        {still_discovering, _} -> still_discovering;
        {ok, _}                -> ok
    end.


%%  @private
%%  Make state variables subscription.
%%
make_subscription(ServiceStateVars, Callback, TTL) ->
    lists:map(
        fun ({Service = #{service := Data}, StateVars}) ->
            ServiceType = proplists:get_value("serviceType", Data),
            ParentUDN = proplists:get_value("parentUDN", Data),
            ReqRes = subscribe_request(Service, StateVars, Callback, TTL),
            {ServiceType, {ParentUDN, ReqRes}}
        end,
        ServiceStateVars
    ).


%%  @private
%%  Make a subscription request.
%%
subscribe_request(Service, StateVars, Callback, Timeout) ->
    OS = case os:type() of
        {unix,  _} -> "Unix";
        {win32, _} -> "Windows"
    end,
    C = fun (Int) -> erlang:integer_to_list(Int) end,
    OSVsn = case os:version() of
        {Maj, Min, Rel}             -> C(Maj) ++ "." ++ C(Min) ++ "." ++ C(Rel);
        String when is_list(String) -> String;
        _Other                      -> "0"
    end,
    AppVsn = erl_upnp_helper:get_app_vsn(erl_upnp),
    StateVarsEncoded = string:join(StateVars, ","),
    TimeoutEncoded = integer_to_list(Timeout),
    subscribe_request(Service, StateVarsEncoded, Callback, OS, OSVsn, AppVsn, TimeoutEncoded).

subscribe_request(#{service := Service}, StateVarsEncoded, Callback, OS, OSVsn, AppVsn, Timeout) ->
    EventUrl = proplists:get_value("eventSubURL", Service),
    {ok, {_, _, Host, Port, Path, _}} = http_uri:parse(EventUrl),
    {StateVarsHeader, HeadersNeeded} = case StateVarsEncoded of
        []      -> {"", 3};
        [_|_]   -> {"STATEVAR: " ++ StateVarsEncoded ++ "\r\n", 4} % Device may not implement support of this header
    end,
    Msg =
        "SUBSCRIBE " ++ Path ++ " HTTP/1.1\r\n" ++
        "HOST: " ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
        "USER-AGENT: " ++ OS ++ "/" ++ OSVsn ++ " UPnP/1.1 Erl-UPnP/" ++ AppVsn ++ "\r\n" ++
        "CALLBACK: <" ++ Callback ++ ">\r\n" ++
        "NT: upnp:event\r\n" ++
        "TIMEOUT: Second-" ++ Timeout ++ "\r\n" ++
        StateVarsHeader ++
        "\r\n",
    {ok, ParsedHost} = inet:parse_address(Host),
    {ok, Socket} = gen_tcp:connect(ParsedHost, Port, [{active, false}, binary], ?TIMEOUT),
    ok = gen_tcp:send(Socket, Msg),
    Result = do_recv(Socket, HeadersNeeded, fun decode_packet/2),
    ok = case proplists:get_value("Connection", Result, "keep-alive") of
        "close"      -> gen_tcp:close(Socket);
        "keep-alive" -> ok
    end,
    [{"host", {ParsedHost, Port}} | proplists:delete("Connection", Result)].


%%  @private
%%  Receive from socket loop.
%%
do_recv(Socket, HeadersNeeded, ParseFun) ->
    do_recv(Socket, 0, <<>>, [], HeadersNeeded, ParseFun).

do_recv(Socket, Length, Rest, Result, HeadersNeeded, ParseFun) ->
    case gen_tcp:recv(Socket, Length, ?TIMEOUT) of
        {ok, Packet} ->
            case erlang:decode_packet(http, Packet, []) of
                {ok, {http_response, _Vsn, 200, "OK"}, ParsedPacket} ->
                    FullPacket = <<Rest/binary, ParsedPacket/binary>>,
                    case ParseFun(FullPacket, HeadersNeeded) of
                        {ok, NewResult}        -> NewResult;
                        {need_more, NewResult} -> do_recv(Socket, Length, FullPacket, NewResult, HeadersNeeded, ParseFun)
                    end;
                {more, Length} ->
                    do_recv(Socket, Length, Packet, Result, HeadersNeeded, ParseFun);
                {error, Reason} ->
                    Reason
            end;
        {error, _Reason} ->
            Result
    end.


%%  @private
%%  Decoding packets.
%%
decode_packet(Packet, HeadersNeeded) ->
    case re:split(Packet, "\\r\\n", [{return, list}]) of
        []  ->
            {need_more, Packet};
        Lines ->
            lists:foldl(
                fun (Line, {_Status, Res}) ->
                    NewRes = case re:split(Line, ":", [trim, {parts, 2}, {return, list}]) of
                        [Field, Val] when
                            Field == "Connection";
                            Field == "Server";
                            Field == "SID";
                            Field == "ACCEPTED-STATEVAR"
                            ->
                            [{Field, string:trim(Val)} | Res];
                        _ ->
                            Res
                    end,
                    case length(NewRes) >= HeadersNeeded of
                        true  -> {ok, NewRes};
                        false -> {need_more, NewRes}
                    end
                end,
                {need_more, []},
                Lines
            )
    end.


