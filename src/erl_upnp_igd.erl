%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Configure router using Internet Gateway Device Protocol (IGD) protocol.
%%% @end
%%% Created : 01. Jun 2019 12.02
%%%-------------------------------------------------------------------
-module(erl_upnp_igd).
-author("bartimaeus").
-include("erl_upnp.hrl").

-behaviour(gen_statem).

%% API
-export([
    start_link/0,
    add_port_mapping/7,
    add_any_port_mapping/7,
    delete_port_mapping/4,
    get_port_mapping/4,
    stop/1
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

-type action_response() :: [
    {ServiceType :: string(),
        SOAPResponse :: string() | {error, term()}}
].

-define(COMPATIBLE_SERVICES, [
    "urn:schemas-upnp-org:service:WANIPConnection:1", % http://upnp.org/specs/gw/UPnP-gw-WANIPConnection-v1-Service.pdf
    "urn:schemas-upnp-org:service:WANIPConnection:2", % http://upnp.org/specs/gw/UPnP-gw-WANIPConnection-v2-Service.pdf
    "urn:schemas-upnp-org:service:WANPPPConnection:1" % http://upnp.org/specs/gw/UPnP-gw-WANPPPConnection-v1-Service.pdf
]).

-define(ADD_PORT_MAPPING(NRH, NEP, NIP, NP, NIC, NE, NPMD, NLD), [
    {"urn:schemas-upnp-org:service:WANIPConnection:1", [
        {"NewRemoteHost", NRH}, % Domain name or IP from which can be received packets only. Empty string - wildcard (all IP's).
        {"NewExternalPort", NEP}, % External port which will be opened. 0 means all external ports will be opened (very dangerous!)
        {"NewInternalPort", NIP}, % Internal ports which will be opened.
        {"NewProtocol", NP}, % TCP or UDP
        {"NewInternalClient", NIC}, % Internal IP
        {"NewEnabled", NE}, % 1 or 0
        {"NewPortMappingDescription", NPMD}, % Just description
        {"NewLeaseDuration", NLD} % Port forward TTL in seconds. Range: [1, 604800]
    ]},
    {"urn:schemas-upnp-org:service:WANIPConnection:2", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewInternalPort", NIP},
        {"NewProtocol", NP},
        {"NewInternalClient", NIC},
        {"NewEnabled", NE}, % 1 or 0
        {"NewPortMappingDescription", NPMD},
        {"NewLeaseDuration", NLD}
    ]},
    {"urn:schemas-upnp-org:service:WANPPPConnection:1", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewInternalPort", NIP},
        {"NewProtocol", NP},
        {"NewInternalClient", NIC},
        {"NewEnabled", NE}, % 1 or 0
        {"NewPortMappingDescription", NPMD},
        {"NewLeaseDuration", NLD}
    ]}
]).

-define(ADD_ANY_PORT_MAPPING(NRH, NEP, NIP, NP, NIC, NE, NPMD, NLD), [
    {"urn:schemas-upnp-org:service:WANIPConnection:2", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewInternalPort", NIP},
        {"NewProtocol", NP},
        {"NewInternalClient", NIC},
        {"NewEnabled", NE}, % 1 or 0
        {"NewPortMappingDescription", NPMD},
        {"NewLeaseDuration", NLD}
    ]}
]).

-define(DELETE_PORT_MAPPING(NRH, NEP, NP), [
    {"urn:schemas-upnp-org:service:WANIPConnection:1", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewProtocol", NP}
    ]},
    {"urn:schemas-upnp-org:service:WANIPConnection:2", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewProtocol", NP}
    ]},
    {"urn:schemas-upnp-org:service:WANPPPConnection:1", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewProtocol", NP}
    ]}
]).

-define(GET_SPECIFIC_PORT_MAP_ENTRY(NRH, NEP, NP), [
    {"urn:schemas-upnp-org:service:WANIPConnection:1", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewProtocol", NP}
    ]},
    {"urn:schemas-upnp-org:service:WANIPConnection:2", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewProtocol", NP}
    ]},
    {"urn:schemas-upnp-org:service:WANPPPConnection:1", [
        {"NewRemoteHost", NRH},
        {"NewExternalPort", NEP},
        {"NewProtocol", NP}
    ]}
]).

-define(DELAY(S), S * 1000 + 150).

-record(state, {
    client_pid              :: pid(),
    services        = []    :: [service()]
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
%%  Make a new port mapping (port forwarding).
%%
-spec add_port_mapping(
    Pid             :: pid(),
    Host            :: inet:ip4_address() | string(),
    ExternalPort    :: inet:port_number(),
    InternalPort    :: inet:port_number(),
    Protocol        :: tcp | udp,
    Description     :: string(),
    TTL             :: pos_integer()
) ->
    {ok, Response :: action_response()} |
    {error, term()}.

add_port_mapping(Pid, Host, ExternalPort, InternalPort, Protocol, Desc, TTL) ->
    E = fun (Arg) -> encode_args(Arg) end,
    gen_statem:call(Pid, {add_port_mapping, strict, E(Host), E(ExternalPort), E(InternalPort), E(Protocol), E(Desc), E(TTL)}).


%%  @doc
%%  Make a new port mapping (port forwarding).
%%  If given port is already in use, it will make a map on any other free port.
%%
-spec add_any_port_mapping(
    Pid             :: pid(),
    Host            :: inet:ip4_address() | string(),
    ExternalPort    :: inet:port_number(),
    InternalPort    :: inet:port_number(),
    Protocol        :: tcp | udp,
    Description     :: string(),
    TTL             :: pos_integer()
) ->
    {ok, Response :: action_response()} |
    {error, term()}.

add_any_port_mapping(Pid, Host, ExternalPort, InternalPort, Protocol, Desc, TTL) ->
    E = fun (Arg) -> encode_args(Arg) end,
    gen_statem:call(Pid, {add_port_mapping, any, E(Host), E(ExternalPort), E(InternalPort), E(Protocol), E(Desc), E(TTL)}).


%%  @doc
%%  Delete created port mapping.
%%
-spec delete_port_mapping(
    Pid             :: pid(),
    Host            :: inet:ip4_address() | string(),
    ExternalPort    :: inet:port_number(),
    Protocol        :: tcp | udp
) ->
    {ok, Response :: action_response()} |
    {error, term()}.

delete_port_mapping(Pid, Host, ExternalPort, Protocol) ->
    E = fun (Arg) -> encode_args(Arg) end,
    gen_statem:call(Pid, {delete_port_mapping, E(Host), E(ExternalPort), E(Protocol)}).


%%  @doc
%%  Get existence port mapping data.
%%
-spec get_port_mapping(
    Pid             :: pid(),
    Host            :: inet:ip4_address() | string(),
    ExternalPort    :: inet:port_number(),
    Protocol        :: tcp | udp
) ->
    {ok, Response :: action_response()} |
    {error, term()}.

get_port_mapping(Pid, Host, ExternalPort, Protocol) ->
    E = fun (Arg) -> encode_args(Arg) end,
    gen_statem:call(Pid, {get_port_mapping, E(Host), E(ExternalPort), E(Protocol)}).


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
    {ok, Pid} = erl_upnp_client:start_discover_link(TTL, ssdp_all),
    State = #state{
        client_pid = Pid
    },
    Actions = [
        {next_event, internal, start},
        {state_timeout, ?DELAY(TTL), get_services}
    ],
    {ok, waiting, State, Actions}.


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
handle_event({call, _From}, Request, waiting, _SD) when
    element(1, Request) =:= add_port_mapping;
    element(1, Request) =:= delete_port_mapping;
    element(1, Request) =:= get_port_mapping
    ->
    {keep_state_and_data, [postpone]};

%
%
handle_event(state_timeout, get_services, waiting, SD = #state{client_pid = Pid}) ->
    {ClientState, Services} = lists:foldl(
        fun (Service, Acc = {_, AccServs}) ->
            case erl_upnp_client:find_entity(Pid, Service) of
                {still_discovering, _} -> Acc;
                {ok, []}               -> {ok, AccServs};
                {ok, FoundServs}       -> {ok, AccServs ++ FoundServs}
            end
        end,
        {still_discovering, []},
        ?COMPATIBLE_SERVICES
    ),
    case ClientState of
        still_discovering -> {keep_state_and_data, [{state_timeout, 1000, get_services}]};
        ok                -> {next_state, open, SD#state{services = Services}}
    end;

%--------------------------------------------------------------------
%   Open state
%
handle_event({call, From}, Request, open, SD) when
    element(1, Request) =:= add_port_mapping;
    element(1, Request) =:= delete_port_mapping;
    element(1, Request) =:= get_port_mapping
    ->
    #state{
        client_pid  = ClientPid,
        services    = Services
    } = SD,
    IP = inet:ntoa(erl_upnp_helper:get_internal_ip()),
    {Action, Args} = case Request of
        {add_port_mapping, strict, Host, ExternalPort, InternalPort, Protocol, Desc, TTL} ->
            {"AddPortMapping", ?ADD_PORT_MAPPING(Host, ExternalPort, InternalPort, Protocol, IP, "1", Desc, TTL)};
        {add_port_mapping, any, Host, ExternalPort, InternalPort, Protocol, Desc, TTL} ->
            {"AddAnyPortMapping", ?ADD_ANY_PORT_MAPPING(Host, ExternalPort, InternalPort, Protocol, IP, "1", Desc, TTL)};
        {delete_port_mapping, Host, ExternalPort, Protocol} ->
            {"DeletePortMapping", ?DELETE_PORT_MAPPING(Host, ExternalPort, Protocol)};
        {get_port_mapping, Host, ExternalPort, Protocol} ->
            {"GetSpecificPortMappingEntry", ?GET_SPECIFIC_PORT_MAP_ENTRY(Host, ExternalPort, Protocol)}
    end,
    Result = lists:map(
        fun (#{service := Service}) ->
            ControlUrl = proplists:get_value("controlURL", Service),
            ServiceType = proplists:get_value("serviceType", Service),
            case proplists:get_value(ServiceType, Args) of
                undefined   ->
                    {error, {no_action, Action}};
                ServiceArgs ->
                    Resp = erl_upnp_helper:make_request(ClientPid, ControlUrl, Action, ServiceType, ServiceArgs),
                    {ServiceType, Resp}
            end
        end,
        Services
    ),
    {keep_state_and_data, [{reply, From, Result}]}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%  @private
%%  Encode arguments.
%%
encode_args(Arg) when is_list(Arg)      -> Arg;
encode_args(Arg) when is_integer(Arg)   -> integer_to_list(Arg);
encode_args(Arg) when is_binary(Arg)    -> binary_to_list(Arg);
encode_args(Arg) when is_tuple(Arg)     -> inet:ntoa(Arg);
encode_args(tcp)                        -> "TCP";
encode_args(udp)                        -> "UDP".


