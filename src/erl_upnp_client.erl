%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% UPnP control point implemented as FSM with 3 states:
%%%     * waiting
%%%     * discovering
%%%     * open
%%% @end
%%% Created : 04. Jun 2019 19.47
%%%-------------------------------------------------------------------
-module(erl_upnp_client).
-author("bartimaeus").
-behavior(gen_statem).
-include("erl_upnp.hrl").

-define(BROADCAST_IP, {239, 255, 255, 250}).
-define(BROADCAST_PORT, 1900).
-define(DELAY(S), S * 1000 + 100).

-type target()  :: ssdp_all | upnp_rootdevice | {uuid, list()} | list().

-record(state, {
    src_port                        :: inet:port_number(),
    socket                          :: inet:socket(),
    parser_pid                      :: pid(),
    event_mgr_pid                   :: pid(),
    devices                 = []    :: [device()],
    unidentified_devices    = []    :: [term()],
    delay                   = 2     :: pos_integer(),
    last_target                     :: target()
}).

%% API
-export([
    start_link/0,
    start_link/1,
    start_discover_link/2,
    start_discover_link/3,
    start_discover/3,
    find_entity/2,
    get_devices/2,
    get_unidentified_devices/1,
    get_port/1,
    get_event_mgr_pid/1,
    stop/1
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

%% Internal API
-export([
    message_parsed/2
]).



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
    gen_statem:start_link(?MODULE, [0, waiting, undefined, undefined], []).


%%  @doc
%%  Starts the server.
%%
-spec start_link(
    SrcPort :: inet:port_number()
) ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start_link(SrcPort) ->
    gen_statem:start_link(?MODULE, [SrcPort, waiting, undefined, undefined], []).


%%  @doc
%%  Starts the server and discovery.
%%
-spec start_discover_link(
    Delay   :: pos_integer(),
    Target  :: target()
) ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start_discover_link(Delay, Target) ->
    gen_statem:start_link(?MODULE, [0, discovering, Delay, Target], []).


%%  @doc
%%  Starts the server and discovery.
%%
-spec start_discover_link(
    SrcPort :: inet:port_number(),
    Delay   :: pos_integer(),
    Target  :: target()
) ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start_discover_link(SrcPort, Delay, Target) ->
    gen_statem:start_link(?MODULE, [SrcPort, discovering, Delay, Target], []).


%%  @doc
%%  Start discovering devices from private network using SSDP protocol.
%%
-spec start_discover(
    Pid     :: pid(),
    Delay   :: pos_integer(),
    Target  :: target()
) ->
    ok | {error, term()}.

start_discover(Pid, Delay, Target) ->
    gen_statem:cast(Pid, {start_discover, Delay, encode_target(Target)}).


%%  @doc
%%  Find entity (device or service) from client's state by some criteria.
%%
-spec find_entity(
    Pid         :: pid(),
    Request     :: list()
) ->
    {ok, [entity()] | false} |
    {still_discovering, [entity()] | false} |
    {error, term()}.

find_entity(Pid, Request) ->
    gen_statem:call(Pid, {find_entity, Request}).


%%  @doc
%%  Return all devices from client's state.
%%
-spec get_devices(
    Pid             :: pid(),
    ReturnFormat    :: list()
) ->
    {ok, [device()]} |
    {still_discovering, [device()]} |
    {error, term()}.

get_devices(Pid, ReturnFormat) ->
    gen_statem:call(Pid, {find_entity, all, ReturnFormat}).


%%  @doc
%%  Return all unidentified devices from client's state.
%%
-spec get_unidentified_devices(
    Pid :: pid()
) ->
    {ok, [term()]} |
    {still_discovering, [term()]} |
    {error, term()}.

get_unidentified_devices(Pid) ->
    gen_statem:call(Pid, {find_entity, unidentified}).


%%  @doc
%%  Returns UDP port of the control point.
%%
-spec get_port(
    Pid :: pid()
) ->
    inet:port_number().

get_port(Pid) ->
    gen_statem:call(Pid, get_port).


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
%%  Stops the client.
%%
-spec stop(
    Pid :: pid()
) ->
    ok.

stop(Pid) ->
    gen_statem:stop(Pid).



%%%===================================================================
%%% Internal API.
%%%===================================================================

%%  @doc
%%  Returns parsed message.
%%
message_parsed(Pid, Data) ->
    gen_statem:cast(Pid, {message_parsed, Data}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%
%%
%%
init([SrcPort, NextAction, Delay, Target]) ->
    {ok, Socket} = gen_udp:open(SrcPort, [binary, {active, once}]),
    {ok, Port} = inet:port(Socket),
    {ok, EventMgrPid} = gen_event:start_link(),
    {ok, ParserPid} = erl_upnp_parser:start_link(EventMgrPid),
    State = #state{
        socket          = Socket,
        src_port        = Port,
        parser_pid      = ParserPid,
        delay           = Delay,
        last_target     = encode_target(Target),
        event_mgr_pid   = EventMgrPid
    },
    case NextAction of
        waiting     ->
            {ok, waiting, State};
        discovering ->
            Actions = [
                {next_event, internal, start},
                {state_timeout, ?DELAY(Delay), stop_discover}
            ],
            {ok, discovering, State, Actions}
    end.


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
handle_event({call, From}, {find_entity, all, _ReturnFormat}, waiting, _SD) ->
    {keep_state_and_data, [{reply, From, {error, discover_not_started}}]};

handle_event({call, From}, {find_entity, _Request}, waiting, _SD) ->
    {keep_state_and_data, [{reply, From, {error, discover_not_started}}]};

%--------------------------------------------------------------------
%   Discovering state
%
handle_event(internal, start, discovering, SD) ->
    #state{
        socket      = Socket,
        last_target = Target,
        delay       = Delay
    } = SD,
    ok = discover(Socket, ?BROADCAST_IP, ?BROADCAST_PORT, Target, Delay),
    keep_state_and_data;

%
%
handle_event(state_timeout, stop_discover, discovering, SD) ->
    {next_state, open, SD};

%
%
handle_event({call, From}, {find_entity, unidentified}, discovering, #state{unidentified_devices = UD}) ->
    {keep_state_and_data, [{reply, From, {still_discovering, UD}}]};

%
%
handle_event({call, _From}, {find_entity, all, _Request}, discovering, #state{devices = []}) ->
    {keep_state_and_data, [postpone]};

%
%
handle_event({call, _From}, {find_entity, _Request}, discovering, #state{devices = []}) ->
    {keep_state_and_data, [postpone]};

%
%
handle_event({call, From}, {find_entity, all, ReturnFormat}, discovering, #state{devices = Devices}) ->
    Result = case ReturnFormat of
        hierarchical -> Devices;
        flat         -> lists:map(fun (D) -> erl_upnp_helper:flatten_result(D) end, Devices)
    end,
    {keep_state_and_data, [{reply, From, {still_discovering, Result}}]};

%
%
handle_event({call, From}, {find_entity, Request}, discovering, #state{devices = Devices}) ->
    Action = case erl_upnp_helper:filter_result(Devices, Request) of
        []     -> postpone;
        Result -> {reply, From, {still_discovering, Result}}
    end,
    {keep_state_and_data, [Action]};

%--------------------------------------------------------------------
%   All state events
%
handle_event(cast, {start_discover, Delay, Target}, _, SD) ->
    NewSD = SD#state{
        delay        = Delay,
        last_target  = Target
    },
    Actions = [
        {next_event, internal, start},
        {state_timeout, ?DELAY(Delay), stop_discover}
    ],
    {next_state, discovering, NewSD, Actions};

%
%
handle_event(cast, {message_parsed, {identified, NewDev = #{device := NewDevData}}}, _, SD) ->
    #state{
        devices = Devices
    } = SD,
    % UDN = Unique Device Name
    DevId = proplists:get_value("UDN", NewDevData),
    New = lists:foldl(
        fun
            (_, false)            -> false ;
            (#{device := Dev}, _) -> not lists:member({"UDN", DevId}, Dev)
        end,
        true,
        Devices
    ),
    NewSD = case New of
        true  -> SD#state{devices = [NewDev | Devices]};
        false -> SD
    end,
    {keep_state, NewSD};

%
%
handle_event(cast, {message_parsed, {unidentified, Device}}, _, SD) ->
    #state{
        unidentified_devices = UD
    } = SD,
    DevId = proplists:get_value("USN", Device),
    NewSD = SD#state{
        unidentified_devices = [{DevId, Device} | proplists:delete(DevId, UD)]
    },
    {keep_state, NewSD};

%
%
handle_event({call, From}, {find_entity, unidentified}, _, #state{unidentified_devices = UD}) ->
    {keep_state_and_data, [{reply, From, {ok, UD}}]};

%
%
handle_event({call, From}, {find_entity, all, ReturnFormat}, _, #state{devices = Devices}) ->
    Result = case ReturnFormat of
        hierarchical -> Devices;
        flat         -> lists:map(fun (D) -> erl_upnp_helper:flatten_result(D) end, Devices)
    end,
    {keep_state_and_data, [{reply, From, {ok, Result}}]};

%
%
handle_event({call, From}, {find_entity, Request}, _, #state{devices = Devices}) ->
    Result = erl_upnp_helper:filter_result(Devices, Request),
    {keep_state_and_data, [{reply, From, {ok, Result}}]};

%
%
handle_event({call, From}, get_port, _, #state{src_port = Port}) ->
    {keep_state_and_data, [{reply, From, Port}]};

%
%
handle_event({call, From}, get_event_mgr_pid, _, #state{event_mgr_pid = Pid}) ->
    {keep_state_and_data, [{reply, From, Pid}]};

%
%
handle_event(info, {udp, _Port, DstIp, DstPort, Message}, _, SD) ->
    #state{
        socket      = Socket,
        parser_pid  = ParserPid
    } = SD,
    inet:setopts(Socket, [{active, once}]),
    ok = erl_upnp_parser:parse_message(ParserPid, self(), Message, DstIp, DstPort),
    {keep_state, SD}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%  @private
%%  Send HTTPU M-SEARCH message to discover devices on the network.
%%  SSDP start line should be one of the following:
%%      NOTIFY * HTTP/1.1\r\n
%%      M-SEARCH * HTTP/1.1\r\n
%%      HTTP/1.1 200 OK\r\n
%%
discover(Socket, BroadcastIp, BroadcastPort, Target, Delay) ->
    Msg =
        "M-SEARCH * HTTP/1.1\r\n"
        "HOST:" ++ inet:ntoa(BroadcastIp) ++ ":" ++ integer_to_list(BroadcastPort) ++"\r\n"
        "ST:" ++ Target ++ "\r\n"                   % Search target
        "MX:" ++ integer_to_list(Delay) ++"\r\n"    % Delay responses to avoid flood
        "MAN:\"ssdp:discover\"\r\n"
        "\r\n",
    gen_udp:send(Socket, BroadcastIp, BroadcastPort, Msg).


%%  @private
%%  Encode search target.
%%
encode_target(undefined)                        -> undefined;
encode_target(ssdp_all)                         -> "ssdp:all";
encode_target(upnp_rootdevice)                  -> "upnp:rootdevice";
encode_target({uuid, Uuid})                     -> "uuid:" ++ Uuid;
encode_target(Target) when is_list(Target)      -> Target;
encode_target(Target) when is_binary(Target)    -> binary_to_list(Target).

    
