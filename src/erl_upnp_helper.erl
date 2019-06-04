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
    flatten_result/1
]).

%%  @doc
%%  Search several results.
%%
filter_result(HierarchicalRes, Key) when is_list(HierarchicalRes) ->
    lists:filtermap(
        fun (Device) ->
            case filter_result(Device, Key) of
                false -> false;
                Data  -> {true, Data}
            end
        end,
        HierarchicalRes
    );


%%  @doc
%%  Search single result.
%%
filter_result(HierarchicalRes, Key) ->
    %
    % Key and type comparing function.
    Compare = fun
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
                Dev;
            false ->
                lists:foldl(
                    fun
                        (Serv = #{service := ServInfo}, false) ->
                            case Compare(proplists:get_value("serviceType", ServInfo)) of
                                false -> false;
                                true  -> Serv
                            end;
                        (_, Serv) ->
                            Serv
                    end,
                    false,
                    proplists:get_value("services", DevInfo)
                )
        end
    end,
    case Search(HierarchicalRes) of
        false ->
            FlatRes = flatten_result(HierarchicalRes),
            #{device := TopDev1} = FlatRes,
            Res = lists:foldl(
                fun
                    (EmbDev, false)  -> Search(EmbDev);
                    (_, FoundEntity) -> FoundEntity
                end,
                false,
                proplists:get_value("embedded_devices", TopDev1)
            ),
            Res;
        #{device := FoundDev} ->
            #{device => proplists:delete("embedded_devices", FoundDev)};
        FoundService ->
            FoundService
    end.

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


