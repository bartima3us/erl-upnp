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
            ReqRes = subscribe_request(Service, StateVars, Callback),
            {ServiceType, ReqRes}
        end,
        ServiceStateVars
    ).


%%
%%
%%
subscribe_request(Service, StateVars, Callback) ->
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
    subscribe_request(Service, StateVars, Callback, OS, OSVsn, AppVsn).

subscribe_request(#{service := Service}, _StateVars, Callback, OS, OSVsn, AppVsn) ->
    %%    join(StringList, Separator)
    EventUrl = proplists:get_value("eventSubURL", Service),
    {ok, {_, _, Host, Port, _, _}} = http_uri:parse(EventUrl),
    Headers = [
        {"HOST",        Host ++ ":" ++ integer_to_list(Port)},
        {"USER-AGENT",  OS ++ "/" ++ OSVsn ++ " UPnP/1.1 Erl-UPnP/" ++ AppVsn},
        {"CALLBACK",    "<" ++ Callback ++ ">"},
        {"NT",          "upnp:event"},
        {"TIMEOUT",     "Second-60"}
    ],
%%    Msg =
%%        "SUBSCRIBE " ++ Path ++ " HTTP/1.1\r\n" ++
%%        "HOST: " ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n" ++
%%        "USER-AGENT: " ++ OS ++ "/" ++ OSVsn ++ " UPnP/1.1 Erl-UPnP/" ++ AppVsn ++ "\r\n" ++
%%        "CALLBACK: <" ++ Callback ++ ">\r\n" ++
%%        "NT: upnp:event\r\n" ++
%%        "TIMEOUT: Second-120\r\n" ++
%%        "\r\n",
%%    {ok, Socket} = gen_udp:open(0, [binary, {active, once}]),
%%    Result = gen_udp:send(Socket, Host, Port, Msg),
    io:format("xxxxx EventUrl=~p~n~n", [EventUrl]),
    Result = case hackney:request(subscribe, EventUrl, Headers, <<"">>) of
        {ok, Status, RespHeaders, _} ->
            io:format("xxxxx Status=~p~n~n", [Status]),
            io:format("xxxxx RespHeaders=~p~n~n", [RespHeaders]),
            RespHeaders;
        Other ->
            Other
    end,
    io:format("xxxxx Headers=~p~n~n", [Headers]),
    Result.


