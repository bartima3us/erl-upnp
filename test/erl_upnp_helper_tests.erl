%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% erl_upnp_helper module unit tests.
%%% @end
%%% Created : 08. Jun 2019 17.14
%%%-------------------------------------------------------------------
-module(erl_upnp_helper_tests).
-author("bartimaeus").
-include_lib("eunit/include/eunit.hrl").


filter_result_test_() ->
    Service = [#{service =>
    [{"parentUDN",
        "uuid:d7b9df0e-55c7-5b1d-932d-e03e0f1bf233"},
        {"parentDeviceType",
            "urn:schemas-wifialliance-org:device:WFADevice:1"},
        {"eventSubURL",
            "http://192.168.1.254:49152/wps_event"},
        {"controlURL",
            "http://192.168.1.254:49152/wps_control"},
        {"SCPDURL", "http://192.168.1.254:49152/wps_scpd.xml"},
        {"serviceId",
            "urn:wifialliance-org:serviceId:WFAWLANConfig1"},
        {"serviceType",
            "urn:schemas-wifialliance-org:service:WFAWLANConfig:1"}]}],
    Device = [#{device =>
    [{"LOCATION",
        "http://192.168.1.254:49152/ky3gPg8b8j/wps_device.xml"},
        {"sender_ip", {192, 168, 1, 254}},
        {"sender_port", 49331},
        {"deviceType",
            "urn:schemas-wifialliance-org:device:WFADevice:1"},
        {"friendlyName", "MediaAccess TG389ac"},
        {"manufacturer", "Technicolor"},
        {"modelName", "MediaAccess TG"},
        {"modelNumber", "389ac"},
        {"serialNumber", "1709SAAN6"},
        {"UDN",
            "uuid:d7b9df0e-55c7-5b1d-932d-e03e0f1bf233"},
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
                    "urn:schemas-wifialliance-org:service:WFAWLANConfig:1"}]}]}]}],
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [{"Service is found by full type.",
            fun() ->
                ?assertEqual(
                    Service,
                    erl_upnp_helper:filter_result(get_hierarchical_res(), "urn:schemas-wifialliance-org:service:WFAWLANConfig:1")
                )
            end
        },
        {"Service is found by type and version.",
            fun() ->
                ?assertEqual(
                    Service,
                    erl_upnp_helper:filter_result(get_hierarchical_res(), "WFAWLANConfig:1")
                )
            end
        },
        {"Service is found by type.",
            fun() ->
                ?assertEqual(
                    Service,
                    erl_upnp_helper:filter_result(get_hierarchical_res(), "WFAWLANConfig")
                )
            end
        },
        {"Not existent service is not found.",
            fun() ->
                ?assertEqual(
                    [],
                    erl_upnp_helper:filter_result(get_hierarchical_res(), "NotExistentServ")
                )
            end
        },
        {"Device is found.",
            fun() ->
                ?assertEqual(
                    Device,
                    erl_upnp_helper:filter_result(get_hierarchical_res(), "WFADevice:1")
                )
            end
        }]
    }.

form_request_test_() ->
    Args = [
        {"NewRemoteHost", ""},
        {"NewExternalPort", "6300"},
        {"NewProtocol", "TCP"}
    ],
    ServiceType = "urn:schemas-upnp-org:service:WANIPConnection:1",
    Result = #{
        body => "<?xml version=\"1.0\"?><s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:GetSpecificPortMappingEntry xmlns:u=\"urn:schemas-upnp-org:service:WANIPConnection:1\"><NewRemoteHost></NewRemoteHost><NewExternalPort>6300</NewExternalPort><NewProtocol>TCP</NewProtocol></u:GetSpecificPortMappingEntry></s:Body></s:Envelope>",
        headers => [
            {"SOAPAction", "\"urn:schemas-upnp-org:service:WANIPConnection:1#GetSpecificPortMappingEntry\""},
            {"Host", "192.168.1.149:43214"},
            {"Content-Length", 395}
        ]
    },
    {setup,
        fun() ->
            ok = meck:new(erl_upnp_client),
            ok = meck:new(erl_upnp_helper, [passthrough]),
            ok = meck:expect(erl_upnp_helper, get_internal_ip, [], {192, 168, 1, 149}),
            ok = meck:expect(erl_upnp_client, get_port, ['_'], 43214)
        end,
        fun(_) ->
            true = meck:validate([erl_upnp_helper, erl_upnp_client]),
            ok = meck:unload([erl_upnp_helper, erl_upnp_client])
        end,
        [{"Form SOAP request.",
            fun() ->
                ?assertEqual(
                    Result,
                    erl_upnp_helper:form_request(list_to_pid("<0.0.1>"), "GetSpecificPortMappingEntry", ServiceType, Args)
                )
            end
        }]
    }.

flatten_result_test_() ->
    [
        ?_assertEqual(
            get_flatten_res(),
            erl_upnp_helper:flatten_result(get_hierarchical_res())
        )
    ].


%%  @private
%%  Hierarchical result fixture.
%%
get_hierarchical_res() ->
    [#{device =>
    [{"LOCATION",
        "http://192.168.1.254:49152/ky3gPg8b8j/wps_device.xml"},
        {"sender_ip", {192, 168, 1, 254}},
        {"sender_port", 49331},
        {"deviceType",
            "urn:schemas-wifialliance-org:device:WFADevice:1"},
        {"friendlyName", "MediaAccess TG389ac"},
        {"manufacturer", "Technicolor"},
        {"modelName", "MediaAccess TG"},
        {"modelNumber", "389ac"},
        {"serialNumber", "1709SAAN6"},
        {"UDN",
            "uuid:d7b9df0e-55c7-5b1d-932d-e03e0f1bf233"},
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
        #{device =>
        [{"LOCATION",
            "http://192.168.1.254:41952/no_nonce_string/"},
            {"sender_ip", {192, 168, 1, 254}},
            {"sender_port", 50298},
            {"UDN",
                "uuid:429ec14c-8e4a-4074-89cc-01fc84e31e1f"},
            {"deviceType",
                "urn:schemas-upnp-org:device:MediaServer:1"},
            {"friendlyName", "Technicolor DLNA Server"},
            {"{urn:schemas-dlna-org:device-1-0}X_DLNADOC",
                "DMS-1.50"},
            {"manufacturer", "Technicolor"},
            {"manufacturerURL", "http://www.technicolor.com"},
            {"modelDescription", "MediaAccess TG"},
            {"modelName", "MediaAccess TG389ac"},
            {"modelNumber", "389ac"},
            {"modelURL", "http://www.technicolor.com"},
            {"serialNumber", "CP1709SAAN6"},
            {"services",
                [#{service =>
                [{"parentUDN",
                    "uuid:429ec14c-8e4a-4074-89cc-01fc84e31e1f"},
                    {"parentDeviceType",
                        "urn:schemas-upnp-org:device:MediaServer:1"},
                    {"controlURL",
                        "http://192.168.1.254:41952/no_nonce_string/ConnectionManager/control"},
                    {"eventSubURL",
                        "http://192.168.1.254:41952/no_nonce_string/ConnectionManager/event"},
                    {"SCPDURL",
                        "http://192.168.1.254:41952/no_nonce_string/ConnectionManager/scpd.xml"},
                    {"serviceId",
                        "urn:upnp-org:serviceId:ConnectionManager"},
                    {"serviceType",
                        "urn:schemas-upnp-org:service:ConnectionManager:1"}]},
                    #{service =>
                    [{"parentUDN",
                        "uuid:429ec14c-8e4a-4074-89cc-01fc84e31e1f"},
                        {"parentDeviceType",
                            "urn:schemas-upnp-org:device:MediaServer:1"},
                        {"controlURL",
                            "http://192.168.1.254:41952/no_nonce_string/ContentDirectory/control"},
                        {"eventSubURL",
                            "http://192.168.1.254:41952/no_nonce_string/ContentDirectory/event"},
                        {"SCPDURL",
                            "http://192.168.1.254:41952/no_nonce_string/ContentDirectory/scpd.xml"},
                        {"serviceId",
                            "urn:upnp-org:serviceId:ContentDirectory"},
                        {"serviceType",
                            "urn:schemas-upnp-org:service:ContentDirectory:1"}]}]}]},
        #{device =>
        [{"LOCATION",
            "http://192.168.1.254:5000/rootDesc.xml"},
            {"sender_ip", {192, 168, 1, 254}},
            {"sender_port", 1900},
            {"deviceType",
                "urn:schemas-upnp-org:device:InternetGatewayDevice:2"},
            {"friendlyName",
                "MediaAccess TG389ac (1709SAAN6)"},
            {"manufacturer", "Technicolor"},
            {"manufacturerURL", "http://www.technicolor.com/"},
            {"modelDescription",
                "Technicolor Internet Gateway Device"},
            {"modelName", "MediaAccess TG"},
            {"modelNumber", "389ac"},
            {"modelURL", "http://www.technicolor.com/"},
            {"serialNumber", "1709SAAN6"},
            {"UDN",
                "uuid:4fb866f7-1858-4d82-ba43-e572961c1dbc"},
            {"services",
                [#{service =>
                [{"parentUDN",
                    "uuid:4fb866f7-1858-4d82-ba43-e572961c1dbc"},
                    {"parentDeviceType",
                        "urn:schemas-upnp-org:device:InternetGatewayDevice:2"},
                    {"SCPDURL",
                        "http://192.168.1.254:5000/lWAuGyh/L3F.xml"},
                    {"eventSubURL",
                        "http://192.168.1.254:5000/lWAuGyh/evt/L3F"},
                    {"controlURL",
                        "http://192.168.1.254:5000/lWAuGyh/ctl/L3F"},
                    {"serviceId",
                        "urn:upnp-org:serviceId:Layer3Forwarding1"},
                    {"serviceType",
                        "urn:schemas-upnp-org:service:Layer3Forwarding:1"}]}]},
            {"embedded_devices",
                [#{device =>
                [{"deviceType",
                    "urn:schemas-upnp-org:device:WANDevice:2"},
                    {"friendlyName", "WANDevice"},
                    {"manufacturer", "MiniUPnP"},
                    {"manufacturerURL",
                        "http://miniupnp.free.fr/"},
                    {"modelDescription", "WAN Device"},
                    {"modelName", "WAN Device"},
                    {"modelNumber", "20160321"},
                    {"modelURL", "http://miniupnp.free.fr/"},
                    {"serialNumber", "1709SAAN6"},
                    {"UDN",
                        "uuid:4fb866f7-1858-4d82-ba44-e572961c1dbc"},
                    {"UPC", "000000000000"},
                    {"services",
                        [#{service =>
                        [{"parentUDN",
                            "uuid:4fb866f7-1858-4d82-ba44-e572961c1dbc"},
                            {"parentDeviceType",
                                "urn:schemas-upnp-org:device:WANDevice:2"},
                            {"SCPDURL",
                                "http://192.168.1.254:5000/lWAuGyh/WANCfg.xml"},
                            {"eventSubURL",
                                "http://192.168.1.254:5000/lWAuGyh/evt/CmnIfCfg"},
                            {"controlURL",
                                "http://192.168.1.254:5000/lWAuGyh/ctl/CmnIfCfg"},
                            {"serviceId",
                                "urn:upnp-org:serviceId:WANCommonIFC1"},
                            {"serviceType",
                                "urn:schemas-upnp-org:service:WANCommonInterfaceConfig:1"}]}]},
                    {"embedded_devices",
                        [#{device =>
                        [{"deviceType",
                            "urn:schemas-upnp-org:device:WANConnectionDevice:2"},
                            {"friendlyName",
                                "WANConnectionDevice"},
                            {"manufacturer", "MiniUPnP"},
                            {"manufacturerURL",
                                "http://miniupnp.free.fr/"},
                            {"modelDescription",
                                "MiniUPnP daemon"},
                            {"modelName", "MiniUPnPd"},
                            {"modelNumber", "20160321"},
                            {"modelURL",
                                "http://miniupnp.free.fr/"},
                            {"serialNumber", "1709SAAN6"},
                            {"UDN",
                                "uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc"},
                            {"UPC", "000000000000"},
                            {"services",
                                [#{service =>
                                [{"parentUDN",
                                    "uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc"},
                                    {"parentDeviceType",
                                        "urn:schemas-upnp-org:device:WANConnectionDevice:2"},
                                    {"SCPDURL",
                                        "http://192.168.1.254:5000/lWAuGyh/WANIPCn.xml"},
                                    {"eventSubURL",
                                        "http://192.168.1.254:5000/lWAuGyh/evt/IPConn"},
                                    {"controlURL",
                                        "http://192.168.1.254:5000/lWAuGyh/ctl/IPConn"},
                                    {"serviceId",
                                        "urn:upnp-org:serviceId:WANIPConn1"},
                                    {"serviceType",
                                        "urn:schemas-upnp-org:service:WANIPConnection:2"}]}]}]}]}]}]},
            {"presentationURL", "http://192.168.1.254/"}]}].


%%  @private
%%  Flatten result fixture.
%%
get_flatten_res() ->
    [#{device =>
    [{"LOCATION",
        "http://192.168.1.254:49152/ky3gPg8b8j/wps_device.xml"},
        {"sender_ip", {192, 168, 1, 254}},
        {"sender_port", 49331},
        {"deviceType",
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
                    "urn:schemas-wifialliance-org:service:WFAWLANConfig:1"}]}]},
        {"embedded_devices", []}]},
        #{device =>
        [{"LOCATION",
            "http://192.168.1.254:41952/no_nonce_string/"},
            {"sender_ip", {192, 168, 1, 254}},
            {"sender_port", 50298},
            {"UDN", "uuid:429ec14c-8e4a-4074-89cc-01fc84e31e1f"},
            {"deviceType",
                "urn:schemas-upnp-org:device:MediaServer:1"},
            {"friendlyName", "Technicolor DLNA Server"},
            {"{urn:schemas-dlna-org:device-1-0}X_DLNADOC",
                "DMS-1.50"},
            {"manufacturer", "Technicolor"},
            {"manufacturerURL", "http://www.technicolor.com"},
            {"modelDescription", "MediaAccess TG"},
            {"modelName", "MediaAccess TG389ac"},
            {"modelNumber", "389ac"},
            {"modelURL", "http://www.technicolor.com"},
            {"serialNumber", "CP1709SAAN6"},
            {"services",
                [#{service =>
                [{"parentUDN",
                    "uuid:429ec14c-8e4a-4074-89cc-01fc84e31e1f"},
                    {"parentDeviceType",
                        "urn:schemas-upnp-org:device:MediaServer:1"},
                    {"controlURL",
                        "http://192.168.1.254:41952/no_nonce_string/ConnectionManager/control"},
                    {"eventSubURL",
                        "http://192.168.1.254:41952/no_nonce_string/ConnectionManager/event"},
                    {"SCPDURL",
                        "http://192.168.1.254:41952/no_nonce_string/ConnectionManager/scpd.xml"},
                    {"serviceId",
                        "urn:upnp-org:serviceId:ConnectionManager"},
                    {"serviceType",
                        "urn:schemas-upnp-org:service:ConnectionManager:1"}]},
                    #{service =>
                    [{"parentUDN",
                        "uuid:429ec14c-8e4a-4074-89cc-01fc84e31e1f"},
                        {"parentDeviceType",
                            "urn:schemas-upnp-org:device:MediaServer:1"},
                        {"controlURL",
                            "http://192.168.1.254:41952/no_nonce_string/ContentDirectory/control"},
                        {"eventSubURL",
                            "http://192.168.1.254:41952/no_nonce_string/ContentDirectory/event"},
                        {"SCPDURL",
                            "http://192.168.1.254:41952/no_nonce_string/ContentDirectory/scpd.xml"},
                        {"serviceId",
                            "urn:upnp-org:serviceId:ContentDirectory"},
                        {"serviceType",
                            "urn:schemas-upnp-org:service:ContentDirectory:1"}]}]},
            {"embedded_devices", []}]},
        #{device =>
        [{"LOCATION", "http://192.168.1.254:5000/rootDesc.xml"},
            {"sender_ip", {192, 168, 1, 254}},
            {"sender_port", 1900},
            {"deviceType",
                "urn:schemas-upnp-org:device:InternetGatewayDevice:2"},
            {"friendlyName", "MediaAccess TG389ac (1709SAAN6)"},
            {"manufacturer", "Technicolor"},
            {"manufacturerURL", "http://www.technicolor.com/"},
            {"modelDescription",
                "Technicolor Internet Gateway Device"},
            {"modelName", "MediaAccess TG"},
            {"modelNumber", "389ac"},
            {"modelURL", "http://www.technicolor.com/"},
            {"serialNumber", "1709SAAN6"},
            {"UDN", "uuid:4fb866f7-1858-4d82-ba43-e572961c1dbc"},
            {"services",
                [#{service =>
                [{"parentUDN",
                    "uuid:4fb866f7-1858-4d82-ba43-e572961c1dbc"},
                    {"parentDeviceType",
                        "urn:schemas-upnp-org:device:InternetGatewayDevice:2"},
                    {"SCPDURL",
                        "http://192.168.1.254:5000/lWAuGyh/L3F.xml"},
                    {"eventSubURL",
                        "http://192.168.1.254:5000/lWAuGyh/evt/L3F"},
                    {"controlURL",
                        "http://192.168.1.254:5000/lWAuGyh/ctl/L3F"},
                    {"serviceId",
                        "urn:upnp-org:serviceId:Layer3Forwarding1"},
                    {"serviceType",
                        "urn:schemas-upnp-org:service:Layer3Forwarding:1"}]}]},
            {"presentationURL", "http://192.168.1.254/"},
            {"embedded_devices",
                [#{device =>
                [{"deviceType",
                    "urn:schemas-upnp-org:device:WANConnectionDevice:2"},
                    {"friendlyName", "WANConnectionDevice"},
                    {"manufacturer", "MiniUPnP"},
                    {"manufacturerURL",
                        "http://miniupnp.free.fr/"},
                    {"modelDescription", "MiniUPnP daemon"},
                    {"modelName", "MiniUPnPd"},
                    {"modelNumber", "20160321"},
                    {"modelURL", "http://miniupnp.free.fr/"},
                    {"serialNumber", "1709SAAN6"},
                    {"UDN",
                        "uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc"},
                    {"UPC", "000000000000"},
                    {"services",
                        [#{service =>
                        [{"parentUDN",
                            "uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc"},
                            {"parentDeviceType",
                                "urn:schemas-upnp-org:device:WANConnectionDevice:2"},
                            {"SCPDURL",
                                "http://192.168.1.254:5000/lWAuGyh/WANIPCn.xml"},
                            {"eventSubURL",
                                "http://192.168.1.254:5000/lWAuGyh/evt/IPConn"},
                            {"controlURL",
                                "http://192.168.1.254:5000/lWAuGyh/ctl/IPConn"},
                            {"serviceId",
                                "urn:upnp-org:serviceId:WANIPConn1"},
                            {"serviceType",
                                "urn:schemas-upnp-org:service:WANIPConnection:2"}]}]}]},
                    #{device =>
                    [{"deviceType",
                        "urn:schemas-upnp-org:device:WANDevice:2"},
                        {"friendlyName", "WANDevice"},
                        {"manufacturer", "MiniUPnP"},
                        {"manufacturerURL",
                            "http://miniupnp.free.fr/"},
                        {"modelDescription", "WAN Device"},
                        {"modelName", "WAN Device"},
                        {"modelNumber", "20160321"},
                        {"modelURL", "http://miniupnp.free.fr/"},
                        {"serialNumber", "1709SAAN6"},
                        {"UDN",
                            "uuid:4fb866f7-1858-4d82-ba44-e572961c1dbc"},
                        {"UPC", "000000000000"},
                        {"services",
                            [#{service =>
                            [{"parentUDN",
                                "uuid:4fb866f7-1858-4d82-ba44-e572961c1dbc"},
                                {"parentDeviceType",
                                    "urn:schemas-upnp-org:device:WANDevice:2"},
                                {"SCPDURL",
                                    "http://192.168.1.254:5000/lWAuGyh/WANCfg.xml"},
                                {"eventSubURL",
                                    "http://192.168.1.254:5000/lWAuGyh/evt/CmnIfCfg"},
                                {"controlURL",
                                    "http://192.168.1.254:5000/lWAuGyh/ctl/CmnIfCfg"},
                                {"serviceId",
                                    "urn:upnp-org:serviceId:WANCommonIFC1"},
                                {"serviceType",
                                    "urn:schemas-upnp-org:service:WANCommonInterfaceConfig:1"}]}]}]}]}]}].


