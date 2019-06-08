%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2019 17.14
%%%-------------------------------------------------------------------
-module(erl_upnp_parser_tests).
-author("bartimaeus").
-include_lib("eunit/include/eunit.hrl").


filter_result_test_() ->
    {_, _, Content} = get_device_description(),
    NS = "{urn:schemas-upnp-org:device-1-0}",
    Loc = "http://192.168.1.254:49152/desc.xml",
    Device = #{device =>
    [{"deviceType",
        "urn:schemas-wifialliance-org:device:WFADevice:1"},
        {"friendlyName", "MediaAccess TG389ac"},
        {"manufacturer", "Technicolor"},
        {"modelName", "MediaAccess TG"},
        {"modelNumber", "389ac"},
        {"serialNumber", "1709SAAN6"},
        {"UDN", "uuid:d7b9df0e-55c7-5b1d-932d-e03e0f1bf233"},
        {"services",
            [#{service =>
            [{"parentUDN",
                "uuid:d7b9df0e-55c7-5b1d-932d-e03e0f1bf233"},
                {"parentDeviceType",
                    "urn:schemas-wifialliance-org:device:WFADevice:1"},
                {"eventSubURL",
                    "http://192.168.1.254:49152/wps_event"},
                {"controlURL",
                    "http://192.168.1.254:49152/wps_control"},
                {"SCPDURL",
                    "http://192.168.1.254:49152/wps_scpd.xml"},
                {"serviceId",
                    "urn:wifialliance-org:serviceId:WFAWLANConfig1"},
                {"serviceType",
                    "urn:schemas-wifialliance-org:service:WFAWLANConfig:1"}]}]}]},
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [{"Derice device namespace.",
            fun() ->
                ?assertEqual(
                    NS,
                    erl_upnp_parser:derive_namespace(get_device_description())
                )
            end
        },
        {"Get content by tag if tuple.",
            fun() ->
                ?assertEqual(
                    Content,
                    erl_upnp_parser:get_content_by_tag("{urn:schemas-upnp-org:device-1-0}root", get_device_description())
                )
            end
        },
        {"Get content by tag if list.",
            fun() ->
                ?assertEqual(
                    [[{"{urn:schemas-upnp-org:device-1-0}major",[],["1"]},
                        {"{urn:schemas-upnp-org:device-1-0}minor",[],["0"]}]],
                    erl_upnp_parser:get_content_by_tag("{urn:schemas-upnp-org:device-1-0}specVersion", Content)
                )
            end
        },
        {"Get device info.",
            fun() ->
                RootContent = erl_upnp_parser:get_content_by_tag(NS ++ "root", get_device_description()),
                [DeviceContent] = erl_upnp_parser:get_content_by_tag(NS ++ "device", RootContent),
                ?assertEqual(
                    Device,
                    erl_upnp_parser:get_device_info(NS, Loc, DeviceContent)
                )
            end
        },
        {"Fix not full address which begins by slash.",
            fun() ->
                ?assertEqual(
                    "http://192.168.1.254:49152/serv_desc.xml",
                    erl_upnp_parser:fix_address("/serv_desc.xml", Loc)
                )
            end
        },
        {"Fix not full address which begins by letter.",
            fun() ->
                ?assertEqual(
                    "http://192.168.1.254:49152/serv_desc.xml",
                    erl_upnp_parser:fix_address("serv_desc.xml", Loc)
                )
            end
        },
        {"Fix full address.",
            fun() ->
                ?assertEqual(
                    "http://192.168.1.254:49152/serv_desc.xml",
                    erl_upnp_parser:fix_address("http://192.168.1.254:49152/serv_desc.xml", Loc)
                )
            end
        }]

    }.


%%  @private
%%  Get XML parsed device description.
%%
get_device_description() ->
    {"{urn:schemas-upnp-org:device-1-0}root", [],
        [{"{urn:schemas-upnp-org:device-1-0}specVersion", [],
            [{"{urn:schemas-upnp-org:device-1-0}major", [], ["1"]},
                {"{urn:schemas-upnp-org:device-1-0}minor", [], ["0"]}]},
            {"{urn:schemas-upnp-org:device-1-0}device", [],
                [{"{urn:schemas-upnp-org:device-1-0}deviceType", [],
                    ["urn:schemas-wifialliance-org:device:WFADevice:1"]},
                    {"{urn:schemas-upnp-org:device-1-0}friendlyName", [],
                        ["MediaAccess TG389ac"]},
                    {"{urn:schemas-upnp-org:device-1-0}manufacturer", [],
                        ["Technicolor"]},
                    {"{urn:schemas-upnp-org:device-1-0}modelName", [],
                        ["MediaAccess TG"]},
                    {"{urn:schemas-upnp-org:device-1-0}modelNumber", [],
                        ["389ac"]},
                    {"{urn:schemas-upnp-org:device-1-0}serialNumber", [],
                        ["1709SAAN6"]},
                    {"{urn:schemas-upnp-org:device-1-0}UDN", [],
                        ["uuid:d7b9df0e-55c7-5b1d-932d-e03e0f1bf233"]},
                    {"{urn:schemas-upnp-org:device-1-0}serviceList", [],
                        [{"{urn:schemas-upnp-org:device-1-0}service", [],
                            [{"{urn:schemas-upnp-org:device-1-0}serviceType", [],
                                ["urn:schemas-wifialliance-org:service:WFAWLANConfig:1"]},
                                {"{urn:schemas-upnp-org:device-1-0}serviceId", [],
                                    ["urn:wifialliance-org:serviceId:WFAWLANConfig1"]},
                                {"{urn:schemas-upnp-org:device-1-0}SCPDURL", [],
                                    ["wps_scpd.xml"]},
                                {"{urn:schemas-upnp-org:device-1-0}controlURL", [],
                                    ["wps_control"]},
                                {"{urn:schemas-upnp-org:device-1-0}eventSubURL", [],
                                    ["wps_event"]}]}]}]}]}.


