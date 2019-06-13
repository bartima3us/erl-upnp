%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2019 20.57
%%%-------------------------------------------------------------------
-module(erl_upnp_subscriber).
-author("bartimaeus").
-include("erl_upnp.hrl").

-behaviour(gen_server).

-define(TIMEOUT, 5000).

%% API
-export([
    start_link/0,
    subscribe/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    service_state_vars  = []    :: [{service(), string()}],
    http_pid                    :: pid(),
    callback                    :: pid()
}).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%
%%
%%
subscribe(Pid, ServiceStateVars) ->
    gen_server:call(Pid, {subscribe, ServiceStateVars}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, HttpPid} = erl_upnp_subscriber_handler:start_http(),
    Port = ranch:get_port(upnp_callback),
    IP = erl_upnp_helper:get_internal_ip(),
    Callback = "http://" ++ inet:ntoa(IP) ++ ":" ++ integer_to_list(Port) ++ "/callback",
    {ok, #state{http_pid = HttpPid, callback = Callback}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({subscribe, ServiceStateVars}, _From, State) ->
    #state{
        callback            = Callback,
        service_state_vars  = CurrServiceStateVars
    } = State,
    SubRes = make_subscription(ServiceStateVars, Callback),
    NewState = State#state{
        service_state_vars  = CurrServiceStateVars ++ ServiceStateVars
    },
    {reply, SubRes, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("xxxxx Info=~p~n~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%  @private
%%  Make state variables subscription.
%%
make_subscription(ServiceStateVars, Callback) ->
    lists:map(
        fun ({Service = #{service := Data}, StateVars}) ->
            ServiceType = proplists:get_value("serviceType", Data),
            ReqRes = subscribe_request(Service, StateVars, Callback, 60),
            {ServiceType, ReqRes}
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
    Result = do_recv(Socket, HeadersNeeded),
    ok = case proplists:get_value("Connection", Result, "keep-alive") of
        "close"      -> gen_tcp:close(Socket);
        "keep-alive" -> ok
    end,
    proplists:delete("Connection", Result).


%%  @private
%%  Receive from socket loop.
%%
do_recv(Socket, HeadersNeeded) ->
    do_recv(Socket, 0, <<>>, [], HeadersNeeded).

do_recv(Socket, Length, Rest, Result, HeadersNeeded) ->
    case gen_tcp:recv(Socket, Length, ?TIMEOUT) of
        {ok, Packet} ->
            case erlang:decode_packet(http, Packet, []) of
                {ok, {http_response, _V, 200, "OK"}, ParsedPacket} ->
                    FullPacket = <<Rest/binary, ParsedPacket/binary>>,
                    case decode_packet(FullPacket, HeadersNeeded) of
                        {ok, NewResult}        -> NewResult;
                        {need_more, NewResult} -> do_recv(Socket, Length, FullPacket, NewResult, HeadersNeeded)
                    end;
                {more, Length} ->
                    do_recv(Socket, Length, Packet, Result, HeadersNeeded);
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


