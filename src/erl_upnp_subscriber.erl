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

-ifdef(TEST).
-export([
    decode_packet/3,
    get_timeout_actions/2,
    remove_subscription/2
]).
-endif.

-type sub_param() ::
    {Param :: string(), Value :: term()}.

-type subscription() ::
    {ServiceType :: string(),
        {ParentUDN :: string(), [sub_param()]}}.

-define(DELAY(S), S * 1000 + 150).
-define(TIMEOUT, 5000).
-define(MAX_TTL, 4233600). % seconds (49 days)
-define(INFINITE_TTL, 1800). % seconds (30 min)
-define(REFRESH_AFTER, 1740000). % milliseconds (29 min)

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
    HttpId = {upnp_callback, self()},
    {ok, ClientPid} = erl_upnp_client:start_discover_link(ssdp_all, []),
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
handle_event({call, From}, {subscribe, Service, StateVars, TTL}, open, SD) ->
    #state{
        client_pid      = ClientPid,
        callback        = Callback,
        subscriptions   = CurrSubs
    } = SD,
    {ok, FoundServices} = erl_upnp_client:find_entity(ClientPid, Service),
    ServiceStateVars = lists:map(fun (Srv) -> {Srv, StateVars} end, FoundServices),
    Res = make_subscription(ServiceStateVars, Callback, TTL),
    TimeoutActions = get_timeout_actions(TTL, Res),
    {keep_state, SD#state{subscriptions = CurrSubs ++ Res}, [{reply, From, Res} | TimeoutActions]};

%
%
handle_event({timeout, {refresh_subscription, ParentUDN, SID}}, ServiceType, open, SD) ->
    #state{
        client_pid      = ClientPid,
        subscriptions   = CurrSubs
    } = SD,
    TTL = infinite,
    {ok, FoundServices} = erl_upnp_client:find_entity(ClientPid, ServiceType),
    ok = make_subscription_refresh(FoundServices, ParentUDN, SID, TTL),
    Actions = [{{timeout, {refresh_subscription, ParentUDN, SID}}, ?REFRESH_AFTER, ServiceType}],
    {keep_state, SD#state{subscriptions = CurrSubs}, Actions};

%
%
handle_event({call, From}, {unsubscribe, Service}, open, SD) ->
    #state{
        client_pid      = ClientPid,
        subscriptions   = CurrSubs
    } = SD,
    {ok, FoundServices} = erl_upnp_client:find_entity(ClientPid, Service),
    {NewSubs, TimersAction} = lists:foldl(
        fun (Sub = {ServiceType, {ParentUDN, Data}}, {SubsAcc, TimersAcc}) ->
            SID = proplists:get_value("SID", Data),
            Stay = lists:foldl(
                fun
                    (Srv = #{service := SrvData}, true) ->
                        ok = unsubscribe_request(Srv, SID),
                        SrvParentUDN = proplists:get_value("parentUDN", SrvData),
                        SrvServiceType = proplists:get_value("serviceType", SrvData),
                        not (ServiceType =:= SrvServiceType andalso SrvParentUDN =:= ParentUDN);
                    (_, false) ->
                        false
                end,
                true,
                FoundServices
            ),
            case Stay of
                true  ->
                    {[Sub | SubsAcc], TimersAcc};
                false ->
                     NewTimersAcc = case proplists:get_value("TTL", Data) of
                        infinite    -> [{{timeout, {refresh_subscription, ParentUDN, SID}}, infinity, ok} | TimersAcc];
                        _Secs       -> TimersAcc
                    end,
                    {SubsAcc, NewTimersAcc}
            end
        end,
        {[], []},
        CurrSubs
    ),
    {keep_state, SD#state{subscriptions = NewSubs}, [{reply, From, ok} | TimersAction]};

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
encode_args(ttl, infinite) -> encode_args(ttl, ?INFINITE_TTL);
encode_args(ttl, Secs) when is_integer(Secs), Secs =< ?MAX_TTL -> integer_to_list(Secs).


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
get_timeout_actions(TTL, Res) ->
    case TTL of
        infinite ->
            lists:foldl(
                fun ({ServiceType, {ParentUDN, Data}}, Acc) ->
                    SID = proplists:get_value("SID", Data),
                    [{{timeout, {refresh_subscription, ParentUDN, SID}}, ?REFRESH_AFTER, ServiceType} | Acc]
                end,
                [],
                Res
            );
        TTL      ->
            lists:foldl(
                fun ({_ServiceType, {_ParentUDN, Data}}, Acc) ->
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
%%  Make refresh of infinite subscription.
%%
make_subscription_refresh(FoundServices, ParentUDN, SID, TTL) ->
    Timeout = encode_args(ttl, TTL),
    lists:foreach(
        fun (Service = #{service := Data}) ->
            ok = case proplists:get_value("parentUDN", Data) of
                ParentUDN  -> subscribe_refresh_request(Service, SID, Timeout);
                _Other     -> ok
            end
        end,
        FoundServices
    ).


%%  @private
%%  Make a subscription request.
%%
subscribe_request(Service, StateVars, Callback, TTL) ->
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
    Timeout = encode_args(ttl, TTL),
    [{"TTL", TTL} | subscribe_request(Service, StateVarsEncoded, Callback, OS, OSVsn, AppVsn, Timeout)].

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
    Result = do_recv(Socket, HeadersNeeded, fun decode_packet/3),
    ok = case proplists:get_value("connection", Result, "keep-alive") of
        "close"      -> gen_tcp:close(Socket);
        "keep-alive" -> ok
    end,
    [{"host", {ParsedHost, Port}} | proplists:delete("connection", Result)].


%%  @private
%%  Make a subscription refresh.
%%
subscribe_refresh_request(#{service := Service}, SID, Timeout) ->
    EventUrl = proplists:get_value("eventSubURL", Service),
    {ok, {_, _, Host, Port, Path, _}} = http_uri:parse(EventUrl),
    Msg =
        "SUBSCRIBE " ++ Path ++ " HTTP/1.1\r\n" ++
        "HOST: " ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
        "SID: " ++ SID ++ "\r\n" ++
        "TIMEOUT: Second-" ++ Timeout ++ "\r\n" ++
        "\r\n",
    {ok, ParsedHost} = inet:parse_address(Host),
    {ok, Socket} = gen_tcp:connect(ParsedHost, Port, [{active, false}, binary], ?TIMEOUT),
    ok = gen_tcp:send(Socket, Msg),
    Result = do_recv(Socket, 0, fun (_, _, CurrRes) -> {ok, CurrRes} end),
    ok = case proplists:get_value("connection", Result, "keep-alive") of
        "close"      -> gen_tcp:close(Socket);
        "keep-alive" -> ok
    end.


%%  @private
%%  Make unsubscribe request.
%%
unsubscribe_request(#{service := Service}, SID) ->
    EventUrl = proplists:get_value("eventSubURL", Service),
    {ok, {_, _, Host, Port, Path, _}} = http_uri:parse(EventUrl),
    Msg =
        "UNSUBSCRIBE " ++ Path ++ " HTTP/1.1\r\n" ++
        "HOST: " ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
        "SID: " ++ SID ++ "\r\n" ++
        "\r\n",
    {ok, ParsedHost} = inet:parse_address(Host),
    {ok, Socket} = gen_tcp:connect(ParsedHost, Port, [{active, false}, binary], ?TIMEOUT),
    ok = gen_tcp:send(Socket, Msg),
    Result = do_recv(Socket, 0, fun (_, _, CurrRes) -> {ok, CurrRes} end),
    ok = case proplists:get_value("connection", Result, "keep-alive") of
        "close"      -> gen_tcp:close(Socket);
        "keep-alive" -> ok
    end.


%%  @private
%%  Receive from socket loop.
%%
do_recv(Socket, HeadersNeeded, ParseFun) ->
    do_recv(Socket, 0, <<>>, [], HeadersNeeded, ParseFun).

do_recv(Socket, Length, Rest, Result, HeadersNeeded, ParseFun) ->
    case gen_tcp:recv(Socket, Length, ?TIMEOUT) of
        {ok, Packet} ->
            case erlang:decode_packet(http, Packet, []) of
                {ok, {http_response, _Vsn, 200, OK}, ParsedPacket} ->
                    "OK" = string:trim(OK),
                    FullPacket = <<Rest/binary, ParsedPacket/binary>>,
                    case ParseFun(FullPacket, HeadersNeeded, Result) of
                        {ok, NewResult}        -> NewResult;
                        {need_more, NewResult} -> do_recv(Socket, Length, FullPacket, NewResult, HeadersNeeded, ParseFun)
                    end;
                {ok, {http_response, _Vsn, 412, "Precondition Failed"}, _ParsedPacket} ->
                    Result;
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
decode_packet(Packet, HeadersNeeded, _CurrResult) ->
    case re:split(Packet, "\\r\\n", [{return, list}]) of
        []  ->
            {need_more, Packet};
        Lines ->
            lists:foldl(
                fun (Line, {_Status, Res}) ->
                    NewRes = case re:split(Line, ":", [trim, {parts, 2}, {return, list}]) of
                        [Field, Val] when
                            Field == "Connection";
                            Field == "CONNECTION";
                            Field == "Server";
                            Field == "SERVER";
                            Field == "ACCEPTED-STATEVAR"
                            ->
                            [{string:lowercase(Field), string:trim(Val)} | Res];
                        [Field, Val] when Field == "SID" ->
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


