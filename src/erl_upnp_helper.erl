%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2019 22.20
%%%-------------------------------------------------------------------
-module(erl_upnp_helper).
-author("bartimaeus").

%% API
-export([
    filter_result/2,
    flatten_result/1,
    make_request/5,
    get_internal_ip/0
]).

%%  @doc
%%  Search entity.
%%
filter_result(HierarchicalRes, Key) when is_list(HierarchicalRes) ->
    lists:flatten(lists:filtermap(
        fun (Device) ->
            case filter_result(Device, Key) of
                []   -> false;
                Data -> {true, Data}
            end
        end,
        HierarchicalRes
    ));


%%  @doc
%%  Search entity.
%%
filter_result(HierarchicalRes, Key) ->
    %
    % Key and type comparing function.
    Compare = fun
        (EntityType) when EntityType == Key ->
            true;
        (EntityType) when is_list(EntityType) ->
            case re:split(EntityType, ":", [{return, list}]) of
                % urn:schemas-upnp-org:device:deviceType:ver
                % or
                % urn:domain-name:device:deviceType:ver
                [_, _, _, Type, Vsn] ->
                    lists:member(Key, [Type, Vsn, Type ++ ":" ++ Vsn]);
                % Maybe someone skipped architecture doc?
                _                    ->
                    false
            end;
        (_) ->
            false
    end,
    %
    % Searching through devices and services function.
    Search = fun (Dev = #{device := DevInfo}) ->
        case Compare(proplists:get_value("deviceType", DevInfo)) of
            true  ->
                [Dev];
            false ->
                lists:foldl(
                    fun
                        (Serv = #{service := ServInfo}, Acc) ->
                            case Compare(proplists:get_value("serviceType", ServInfo)) of
                                false -> Acc;
                                true  -> [Serv | Acc]
                            end;
                        (_, Serv) ->
                            Serv
                    end,
                    [],
                    proplists:get_value("services", DevInfo)
                )
        end
    end,
    IterationFun = fun
        IterateFun([], Acc) ->
            Acc;
        IterateFun([Dev = #{device := DevInfo} | OtherDevs], Acc) ->
            EmbeddedDevs = proplists:get_value("embedded_devices", DevInfo, []),
            Result = Search(Dev),
            IterateFun(OtherDevs ++ EmbeddedDevs, Acc ++ Result)
    end,
    IterationFun([HierarchicalRes], []).


%%  @doc
%%  Make flat result from hierarchical.
%%
flatten_result(HierarchicalRes) when is_list(HierarchicalRes) ->
    lists:map(fun flatten_result/1, HierarchicalRes);

flatten_result(HierarchicalRes) ->
    #{device := TopDev0} = HierarchicalRes,
    TopEmbDev0 = proplists:get_value("embedded_devices", TopDev0, []),
    TopDev1 = proplists:delete("embedded_devices", TopDev0),
    FlattenedEmbDevs = [{"embedded_devices", flatten_concat(TopEmbDev0, [])}],
    HierarchicalRes#{device := TopDev1 ++ FlattenedEmbDevs}.


%%  @private
%%  Make flat result from hierarchical.
%%
flatten_concat([], Acc) ->
    Acc;

flatten_concat([Dev = #{device := DevInfo} | Devices], Acc) ->
    EmbDevs = proplists:get_value("embedded_devices", DevInfo, []),
    UpdatedDev = Dev#{device => proplists:delete("embedded_devices", DevInfo)},
    case EmbDevs of
        []      -> flatten_concat(Devices, [UpdatedDev | Acc]);
        NextDev -> flatten_concat(Devices ++ NextDev, [UpdatedDev | Acc])
    end.


%%  @doc
%%  Make SOAP request to the control point.
%%
make_request(ClientPid, ControlUrl, Action, ServiceType, Args) ->
    Port = erl_upnp_client:get_port(ClientPid),
    ParsedArgs = lists:foldl(
        fun ({Arg, Val}, Acc) ->
            Acc ++ "<" ++ Arg ++ ">" ++ Val ++ "</" ++ Arg ++ ">"
        end,
        "",
        Args
    ),
    Body = case Args of
        []    ->
            "";
        [_|_] ->
            "<?xml version=\"1.0\"?>" ++
            "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">" ++
                "<s:Body>" ++
                    "<u:" ++ Action ++ " xmlns:u=\"" ++ ServiceType ++ "\">"
                        ++ ParsedArgs ++
                    "</u:" ++ Action ++ ">" ++
                "</s:Body>" ++
            "</s:Envelope>"
    end,
    Headers = [
        {"SOAPAction",          "\"" ++ ServiceType ++ "#" ++ Action ++ "\""},
        {"Host",                inet:ntoa(get_internal_ip()) ++ ":" ++ integer_to_list(Port)},
        {"Content-Length",      length(Body)},
        {"TRANSFER-ENCODING",   "\"chunked\""}
    ],
    {ok, {_S, _H, Resp}}= httpc:request(post, {ControlUrl, Headers, "application/xml; charset=\"utf-8\"", Body}, [], []),
    Resp.


%%  @doc
%%  Get your internal IP.
%%
get_internal_ip() ->
    {ok, L} = inet:getif(),
    element(1, hd(L)).


