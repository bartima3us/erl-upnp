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
-include_lib("xmerl/include/xmerl.hrl").

-behaviour(gen_server).

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

-ifdef(TEST).
-export([
    get_device_info/2,
    fix_address/2,
    filter_elements/2
]).
-endif.

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
            case httpc:request(get, {Location, []}, [], [{body_format, string}]) of
                {ok, {{_V, 200, _RP}, _Headers, Body}} ->
                    {ParsedDescription, _} = xmerl_scan:string(Body),
                    {URLBase, ExtraURLBase} = case xmerl_xpath:string("//root/URLBase/text()", ParsedDescription) of
                        []                        -> {Location, []};
                        [#xmlText{value = Base}]  -> {Base, [{"URLBase", Base}]}
                    end,
                    DeviceContent = xmerl_xpath:string("//root/device", ParsedDescription),
                    HierarchicalRes = #{device := Dev} = get_device_info(URLBase, filter_elements(DeviceContent, device)),
                    ExtraData = ExtraURLBase ++ [{"LOCATION", Location}, {"sender_ip", DstIp}, {"sender_port", DstPort}],
                    NewDevice = HierarchicalRes#{device => ExtraData ++ Dev},
                    ok = gen_event:notify(EventMgrPid, {device_discovered, NewDevice}),
                    {identified, NewDevice};
                _Other ->
                    ok = gen_event:notify(EventMgrPid, {raw_entity_discovered, Info}),
                    {unidentified, Info}
            end
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
%%  Convert devices XML to maps list in hierarchical format.
%%
get_device_info(URLBase, [#xmlElement{name = device, content = DeviceContent}]) ->
    #{device := RootDevInfo} = lists:foldl(
        fun
            %
            % Get embedded services info
            (#xmlElement{name = serviceList, content = RawContent}, Acc0 = #{device := DevInfo0}) ->
                ParentUDN = proplists:get_value("UDN", DevInfo0, undefined),
                ParentDevType = proplists:get_value("deviceType", DevInfo0, undefined),
                CurrServices = proplists:get_value("services", DevInfo0, []),
                DevInfo1 = proplists:delete("services", DevInfo0),
                NewEmbServ = lists:foldl(
                    fun (Serv, Acc1) ->
                        ExtraData = [{"parentUDN", ParentUDN}, {"parentDeviceType", ParentDevType}],
                        [get_service_info(URLBase, [Serv], ExtraData) | Acc1]
                    end,
                    [],
                    filter_elements(RawContent, service)
                ),
                Acc0#{device => [{"services", CurrServices ++ NewEmbServ} | DevInfo1]};
            %
            % Get embedded devices info
            (#xmlElement{name = deviceList, content = RawContent}, Acc0 = #{device := DevInfo0})  ->
                CurrEmbDev = proplists:get_value("embedded_devices", DevInfo0, []),
                DevInfo1 = proplists:delete("embedded_devices", DevInfo0),
                NewDev = lists:foldl(
                    fun (Dev, Acc1) ->
                        EmbDev = get_device_info(URLBase, [Dev]),
                        [EmbDev | Acc1]
                    end,
                    [],
                    filter_elements(RawContent, device)
                ),
                Acc0#{device => [{"embedded_devices", CurrEmbDev ++ NewDev} | DevInfo1]};
            %
            % Get device info
            (#xmlElement{name = Tag, content = [#xmlText{value = Content}]}, Acc = #{device := RootDevInfo}) ->
                UpdatedRootDev = [{atom_to_list(Tag), Content} | RootDevInfo],
                Acc#{device => UpdatedRootDev};
            %
            % Something not interesting
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
get_service_info(URLBase, [#xmlElement{name = service, content = ServiceContent}], ExtraData) ->
    #{service := ServiceNewInfo} = lists:foldl(
        fun
            (#xmlElement{name = Tag, content = [#xmlText{value = Content0}]}, #{service := AccServiceInfo}) ->
                Content1 = case atom_to_list(Tag) of
                    "SCPDURL"     -> fix_address(Content0, URLBase);
                    "eventSubURL" -> fix_address(Content0, URLBase);
                    "controlURL"  -> fix_address(Content0, URLBase);
                    _             -> Content0
                end,
                #{service => [{atom_to_list(Tag), Content1} | AccServiceInfo]};
            (_, Acc) ->
                Acc
        end,
        #{service => []},
        ServiceContent
    ),
    #{service => ExtraData ++ ServiceNewInfo}.


%%  @private
%%  Fix address.
%%
fix_address(Address, URLBase) ->
    case http_uri:parse(Address) of
        {error, no_scheme} ->
            {ok, {Scheme0, [], Url, Port, _Tail, []}} = http_uri:parse(URLBase),
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


%%  @private
%%  Leave only elements with some name.
%%
filter_elements(RawContent, LeftOnly) ->
    lists:filter(
        fun
            (#xmlElement{name = Tag}) when Tag =:= LeftOnly -> true;
            (_)                                             -> false
        end,
        RawContent
    ).


