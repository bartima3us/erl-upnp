%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Configure TV using Rendering Control service (experimental).
%%% @end
%%% Created : 01. Jun 2019 12.02
%%%-------------------------------------------------------------------
-module(erl_upnp_rendering_control).
-author("bartimaeus").
-include("erl_upnp.hrl").

-behaviour(gen_statem).

%% API
-export([
    start_link/0,
    get_mute/1,
    get_volume/1,
    set_volume/2,
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
    "urn:schemas-upnp-org:service:RenderingControl:1", % http://upnp.org/specs/av/UPnP-av-RenderingControl-v1-Service.pdf
    "urn:schemas-upnp-org:service:RenderingControl:2", % http://upnp.org/specs/av/UPnP-av-RenderingControl-v2-Service.pdf
    "urn:schemas-upnp-org:service:RenderingControl:3"  % http://upnp.org/specs/av/UPnP-av-RenderingControl-v3-Service.pdf
]).

-define(GET_MUTE(IID, C), [
    {"urn:schemas-upnp-org:service:RenderingControl:1", [
        {"InstanceID", IID},
        {"Channel", C}
    ]},
    {"urn:schemas-upnp-org:service:RenderingControl:2", [
        {"InstanceID", IID},
        {"Channel", C}
    ]},
    {"urn:schemas-upnp-org:service:RenderingControl:3", [
        {"InstanceID", IID},
        {"Channel", C}
    ]}
]).

-define(GET_VOLUME(IID, C), [
    {"urn:schemas-upnp-org:service:RenderingControl:1", [
        {"InstanceID", IID},
        {"Channel", C}
    ]},
    {"urn:schemas-upnp-org:service:RenderingControl:2", [
        {"InstanceID", IID},
        {"Channel", C}
    ]},
    {"urn:schemas-upnp-org:service:RenderingControl:3", [
        {"InstanceID", IID},
        {"Channel", C}
    ]}
]).

-define(SET_VOLUME(IID, C, DV), [
    {"urn:schemas-upnp-org:service:RenderingControl:1", [
        {"InstanceID", IID},
        {"Channel", C},
        {"DesiredVolume", DV}
    ]},
    {"urn:schemas-upnp-org:service:RenderingControl:2", [
        {"InstanceID", IID},
        {"Channel", C},
        {"DesiredVolume", DV}
    ]},
    {"urn:schemas-upnp-org:service:RenderingControl:3", [
        {"InstanceID", IID},
        {"Channel", C},
        {"DesiredVolume", DV}
    ]}
]).

-define(DELAY(S), S * 1000 + 150).

-record(state, {
    client_pid          :: pid(),
    services    = []    :: [service()]
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
%%  Get TV current mute status.
%%
-spec get_mute(
    Pid :: pid()
) ->
    {ok, Response :: action_response()} |
    {error, term()}.

get_mute(Pid) ->
    gen_statem:call(Pid, get_mute).


%%  @doc
%%  Get TV current volume.
%%
-spec get_volume(
    Pid :: pid()
) ->
    {ok, Response :: action_response()} |
    {error, term()}.

get_volume(Pid) ->
    gen_statem:call(Pid, get_volume).


%%  @doc
%%  Set TV volume.
%%
-spec set_volume(
    Pid     :: pid(),
    Volume  :: non_neg_integer()
) ->
    {ok, Response :: action_response()} |
    {error, term()}.

set_volume(Pid, Volume) ->
    gen_statem:call(Pid, {set_volume, encode_args(Volume)}).


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
    {ok, Pid} = erl_upnp_client:start_discover_link(ssdp_all, []),
    State = #state{
        client_pid = Pid
    },
    Actions = [
        {next_event, internal, start},
        {state_timeout, ?DELAY(?DEFAULT_DELAY), get_services}
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
    Request =:= get_mute;
    Request =:= get_volume
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
    Request =:= get_mute;
    Request =:= get_volume;
    element(1, Request) =:= set_volume
    ->
    #state{
        client_pid  = ClientPid,
        services    = Services
    } = SD,
    {Action, Args} = case Request of
        get_mute    ->
            {"GetMute", ?GET_MUTE("0", "Master")};
        get_volume  ->
            {"GetVolume", ?GET_VOLUME("0", "Master")};
        {set_volume, Volume}  ->
            {"SetVolume", ?SET_VOLUME("0", "Master", Volume)}
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
encode_args(Arg) when is_integer(Arg)   -> integer_to_list(Arg).


