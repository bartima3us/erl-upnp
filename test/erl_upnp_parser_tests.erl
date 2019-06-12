%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% erl_upnp_parser module unit tests.
%%% @end
%%% Created : 08. Jun 2019 17.14
%%%-------------------------------------------------------------------
-module(erl_upnp_parser_tests).
-author("bartimaeus").
-include_lib("eunit/include/eunit.hrl").


filter_result_test_() ->
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
        [{"Get device info.",
            fun() ->
                DeviceContent = xmerl_xpath:string("//root/device", get_device_description()),
                ?assertEqual(
                    Device,
                    erl_upnp_parser:get_device_info(Loc, DeviceContent)
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
        },
        {"Filter elements.",
            fun() ->
                DeviceContent = xmerl_xpath:string("//root/device", get_device_description()),
                ?assertEqual(
                    DeviceContent,
                    erl_upnp_parser:filter_elements(DeviceContent, device)
                )
            end
        }]
    }.


%%  @private
%%  Get XML parsed device description.
%%
get_device_description() ->
    {ParsedDescription, _} = xmerl_scan:string(get_device_description_xml()),
    ParsedDescription.

%%  @private
%%  Get XML raw device description.
%%
get_device_description_xml() ->
    "<root xmlns=\"urn:schemas-upnp-org:device-1-0\">"
        "<specVersion>"
            "<major>1</major>"
            "<minor>0</minor>"
        "</specVersion>"
        "<device>"
            "<deviceType>urn:schemas-wifialliance-org:device:WFADevice:1</deviceType>"
            "<friendlyName>MediaAccess TG389ac</friendlyName>"
            "<manufacturer>Technicolor</manufacturer>"
            "<modelName>MediaAccess TG</modelName>"
            "<modelNumber>389ac</modelNumber>"
            "<serialNumber>1709SAAN6</serialNumber>"
            "<UDN>uuid:d7b9df0e-55c7-5b1d-932d-e03e0f1bf233</UDN>"
            "<serviceList>"
                "<service>"
                    "<serviceType>urn:schemas-wifialliance-org:service:WFAWLANConfig:1</serviceType>"
                    "<serviceId>urn:wifialliance-org:serviceId:WFAWLANConfig1</serviceId>"
                    "<SCPDURL>wps_scpd.xml</SCPDURL>"
                    "<controlURL>wps_control</controlURL>"
                    "<eventSubURL>wps_event</eventSubURL>"
                "</service>"
            "</serviceList>"
        "</device>"
    "</root>".


