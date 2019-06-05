%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Search devices in local network using Simple Service Discovery Protocol (SSDP) protocol.
%%% @end
%%% Created : 30. May 2019 23.00
%%%-------------------------------------------------------------------
-module(erl_upnp_parser).
-author("bartimaeus").

%% API
-export([
    start_link/1,
    parse_message/5
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
    event_mgr_pid   :: pid()
}).


%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Start a parser.
%%
start_link(EventMgrPid) ->
    gen_server:start_link(?MODULE, [EventMgrPid], []).


%%  @doc
%%  Parse SSDP message.
%%
parse_message(Pid, From, Message, DstIp, DstPort) ->
    gen_server:cast(Pid, {parse_message, From, Message, DstIp, DstPort}).


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
init([EventMgrPid]) ->
    {ok, #state{event_mgr_pid = EventMgrPid}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({parse_message, From, Message, DstIp, DstPort}, State) ->
    #state{
        event_mgr_pid = EventMgrPid
    } = State,
    Lines = re:split(Message, "\\r\\n", [{return, list}]),
    Info = lists:foldl(
        fun
            ("", Acc) ->
                Acc;
            ("HTTP/1.1 200 OK", Acc) ->
                Acc;
            (Line, Acc) ->
                case re:split(Line, ":", [trim, {parts, 2}, {return, list}]) of
                    [Key, Val] -> [{Key, string:trim(Val)} | Acc];
                    [Key]      -> [{Key, ""} | Acc];
                    _          -> Acc
                end
        end,
        [],
        Lines
    ),
    Result = case proplists:get_value("LOCATION", Info) of
        undefined ->
            ok = gen_event:notify(EventMgrPid, {raw_entity_discovered, Info}),
            {unidentified, Info};
        Location  ->
            {ok, {{_V, _C, _RP}, _Headers, Body}} = httpc:request(get, {Location, []}, [], [{body_format, binary}]),
            {ok, Description, _} = erlsom:simple_form(Body),
            Namespace = derive_namespace(Description),
            RootContent = get_content_by_tag(Namespace ++ "root", Description),
            [DeviceContent] = get_content_by_tag(Namespace ++ "device", RootContent),
            HierarchicalRes = #{device := Dev} = get_device_info(Namespace, Location, DeviceContent),
            ExtraData = [{"LOCATION", Location}, {"sender_ip", DstIp}, {"sender_port", DstPort}],
            NewDevice = HierarchicalRes#{device => ExtraData ++ Dev},
            ok = gen_event:notify(EventMgrPid, {device_discovered, NewDevice}),
            {identified, NewDevice}
    end,
    ok = erl_upnp_client:message_parsed(From, Result),
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
handle_info(_Info, State) ->
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
%%  Get namespace of XML.
%%
derive_namespace(Description) ->
    {RootElement, _RootAttr, _} = Description,
    re:replace(RootElement, "root", "", [{return, list}]).


%%  @private
%%  Return XML tag content.
%%
get_content_by_tag(SearchingTag, Description) when is_list(Description) ->
    ResultList = lists:filtermap(
        fun
            ({Tag, _, Content}) when Tag =:= SearchingTag ->
                {true, Content};
            (_) ->
                false
        end,
        Description
    ),
    case ResultList of
        Result -> Result;
        _      -> undefined
    end;

get_content_by_tag(SearchingTag, Description) when is_tuple(Description) ->
    case Description of
        {SearchingTag, _, Content} -> Content;
        _                          -> undefined
    end.


%%  @private
%%  Convert devices XML to maps list in hierarchical format.
%%
get_device_info(Namespace, Location, DeviceContent) ->
    ServiceListTag = Namespace ++ "serviceList",
    DeviceListTag  = Namespace ++ "deviceList",
    #{device := RootDevInfo} = lists:foldl(
        fun
            %
            % Get embedded services info
            ({Tag, _, Content}, Acc0 = #{device := DevInfo0}) when Tag =:= ServiceListTag ->
                ParentUDN = proplists:get_value("UDN", DevInfo0, undefined),
                CurrServices = proplists:get_value("services", DevInfo0, []),
                DevInfo1 = proplists:delete("services", DevInfo0),
                NewEmbServ = lists:foldl(
                    fun (Serv, Acc1) ->
                        EmbServContent = get_content_by_tag(Namespace ++ "service", Serv),
                        [get_service_info(Namespace, Location, EmbServContent, ParentUDN) | Acc1]
                    end,
                    [],
                    Content
                ),
                Acc0#{device => [{"services", CurrServices ++ NewEmbServ} | DevInfo1]};
            %
            % Get embedded devices info
            ({Tag, _, Content}, Acc0 = #{device := DevInfo0}) when Tag =:= DeviceListTag  ->
                CurrEmbDev = proplists:get_value("embedded_devices", DevInfo0, []),
                DevInfo1 = proplists:delete("embedded_devices", DevInfo0),
                NewDev = lists:foldl(
                    fun (Dev, Acc1) ->
                        EmbDevContent = get_content_by_tag(Namespace ++ "device", Dev),
                        EmbDev = get_device_info(Namespace, Location, EmbDevContent),
                        [EmbDev | Acc1]
                    end,
                    [],
                    Content
                ),
                Acc0#{device => [{"embedded_devices", CurrEmbDev ++ NewDev} | DevInfo1]};
            %
            % Get device info
            ({Tag, _, [Content]}, Acc = #{device := RootDevInfo}) ->
                Key = re:replace(Tag, Namespace, "", [{return, list}]),
                UpdatedRootDev = [{Key, Content} | RootDevInfo],
                Acc#{device => UpdatedRootDev};
            %
            % Who knows??
            (_, Acc) ->
                Acc
        end,
        #{device => []},
        DeviceContent
    ),
    #{device => lists:reverse(RootDevInfo)}.


%%  @private
%%  Convert services XML to maps list.
%%
get_service_info(Namespace, Location, ServiceContent, ParentUDN) ->
    #{service := ServiceNewInfo} = lists:foldl(
        fun
            ({Tag, _, [Content0]}, #{service := AccServiceInfo}) ->
                Key = re:replace(Tag, Namespace, "", [{return, list}]),
                Content1 = case Key of
                    "SCPDURL"     -> fix_address(Content0, Location);
                    "eventSubURL" -> fix_address(Content0, Location);
                    "controlURL"  -> fix_address(Content0, Location);
                    _             -> Content0
                end,
                #{service => [{Key, Content1} | AccServiceInfo]};
            (_, Acc) ->
                Acc
        end,
        #{service => []},
        ServiceContent
    ),
    #{service => [{"parentUDN", ParentUDN} | ServiceNewInfo]}.


%%  @private
%%  Fix address.
%%
fix_address(Address, Location) ->
    case http_uri:parse(Address) of
        {error, no_scheme} ->
            {ok, {Scheme0, [], Url, Port, _Tail, []}} = http_uri:parse(Location),
            Scheme1 = case Scheme0 of
                http  -> "http://";
                https -> "https://"
            end,
            case Address of
                [$/ | _] ->
                    Scheme1 ++ Url ++ ":" ++ integer_to_list(Port) ++ Address;
                [_ | _]   ->
                    Scheme1 ++ Url ++ ":" ++ integer_to_list(Port) ++ "/" ++ Address
            end;
        {ok, {_, _, _, _, _, _}} ->
            Address
    end.


