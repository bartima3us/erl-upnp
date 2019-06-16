%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% IGD integration testing.
%%% @end
%%% Created : 08. Jun 2019 19.24
%%%-------------------------------------------------------------------
-module(erl_upnp_igd_SUITE).
-author("bartimaeus").

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    test_add_port_mapping/1
]).

% Internal export
-export([
    network_response/1,
    http_response/1
]).

-define(BROADCAST_PORT, 19001).
-define(CONTROL_PORT, 45621).
-define(DESC_URL, "http://localhost:" ++ integer_to_list(?CONTROL_PORT) ++ "/api/desc").
-define(CONTROL_URL, "http://localhost:" ++ integer_to_list(?CONTROL_PORT) ++ "/api/control").


%%%===================================================================
%%% API for CT.
%%%===================================================================

%%  @doc
%%  List of test cases.
%%
all() ->
    [
        test_add_port_mapping
    ].


%%  @doc
%%  Init.
%%
init_per_suite(Config) ->
    application:ensure_all_started(cowboy),
    Config.


%%  @doc
%%  Clear.
%%
end_per_suite(_Config) ->
    application:stop(cowboy),
    ok.



%%%===================================================================
%%% Testcases.
%%%===================================================================

%%  @doc
%%  Add port mapping action integration test.
%%
test_add_port_mapping(_Config) ->
    Response = "<?xml version=\"1.0\"?>\r\n"
    "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
        "<s:Body>"
            "<u:AddPortMappingResponse xmlns:u=\"urn:schemas-upnp-org:service:WANIPConnection:2\"/>"
        "</s:Body>"
    "</s:Envelope>\r\n",
    Result = [{"urn:schemas-upnp-org:service:WANIPConnection:2", Response}],
    ok = meck:new(erl_upnp_helper, [passthrough]),
    ok = meck:expect(erl_upnp_helper, get_broadcast_ip, [], {127, 0, 0, 1}),
    ok = meck:expect(erl_upnp_helper, get_broadcast_port, [], ?BROADCAST_PORT),
    spawn_link(erl_upnp_mock_udp, start, [?BROADCAST_PORT, ?DESC_URL, fun network_response/1]),
    erl_upnp_mock_handler:start(?CONTROL_PORT, fun http_response/1),
    {ok, ClientPid} = erl_upnp_igd:start_link(),
    Result = erl_upnp_igd:add_port_mapping(ClientPid, "", 6020, 6030, tcp, "For BitTorrent.", 60),
    ok = erl_upnp_igd:stop(ClientPid),
    true = meck:validate(erl_upnp_helper),
    ok = meck:unload(erl_upnp_helper),
    ok.


%%  @doc
%%  Response to request to broadcast IP.
%%
network_response(DescUrl) ->
    <<
        "HTTP/1.1 200 OK\r\n",
        "CACHE-CONTROL: max-age=120\r\n",
        "DATE: Sat, 08 Jun 2019 17:56:46 GMT\r\n",
        "ST: upnp:rootdevice\r\n",
        "USN: uuid:4fb866f7-1858-4d82-ba43-e572961c1dbc::upnp:rootdevice\r\n",
        "EXT:\r\n",
        "SERVER: OpenWRT/OpenWrt/Attitude_Adjustment__r43446_ UPnP/1.1 MiniUPnPd/1.8\r\n",
        "LOCATION: ", DescUrl/binary, "\r\n",
        "OPT: \"http://schemas.upnp.org/upnp/1/0/\"; ns=01\r\n01-NLS: 1\r\n",
        "BOOTID.UPNP.ORG: 1\r\nCONFIGID.UPNP.ORG: 1337\r\n\r\n"
    >>.


%%  @doc
%%  Cowboy mock server description URL response.
%%
http_response(<<"desc">>) ->
    "<?xml version=\"1.0\"?>"
    "<root xmlns=\"urn:schemas-upnp-org:device-1-0\">"
        "<specVersion>"
            "<major>1</major>"
            "<minor>0</minor>"
        "</specVersion>"
        "<device>"
            "<deviceType>urn:schemas-upnp-org:device:InternetGatewayDevice:2</deviceType>"
            "<friendlyName>MediaAccess TG389ac (1709SAAN6)</friendlyName>"
            "<manufacturer>Technicolor</manufacturer>"
            "<manufacturerURL>http://www.technicolor.com/</manufacturerURL>"
            "<modelDescription>Technicolor Internet Gateway Device</modelDescription>"
            "<modelName>MediaAccess TG</modelName>"
            "<modelNumber>389ac</modelNumber>"
            "<modelURL>http://www.technicolor.com/</modelURL>"
            "<serialNumber>1709SAAN6</serialNumber>"
            "<UDN>uuid:4fb866f7-1858-4d82-ba43-e572961c1dbc</UDN>"
            "<serviceList>"
                "<service>"
                    "<serviceType>urn:schemas-upnp-org:service:Layer3Forwarding:1</serviceType>"
                    "<serviceId>urn:upnp-org:serviceId:Layer3Forwarding1</serviceId>"
                    "<controlURL>/lWAuGyh/ctl/L3F</controlURL>"
                    "<eventSubURL>/lWAuGyh/evt/L3F</eventSubURL>"
                    "<SCPDURL>/lWAuGyh/L3F.xml</SCPDURL>"
                "</service>"
            "</serviceList>"
            "<deviceList>"
                "<device>"
                    "<deviceType>urn:schemas-upnp-org:device:WANDevice:2</deviceType>"
                    "<friendlyName>WANDevice</friendlyName>"
                    "<manufacturer>MiniUPnP</manufacturer>"
                    "<manufacturerURL>http://miniupnp.free.fr/</manufacturerURL>"
                    "<modelDescription>WAN Device</modelDescription>"
                    "<modelName>WAN Device</modelName>"
                    "<modelNumber>20160321</modelNumber>"
                    "<modelURL>http://miniupnp.free.fr/</modelURL>"
                    "<serialNumber>1709SAAN6</serialNumber>"
                    "<UDN>uuid:4fb866f7-1858-4d82-ba44-e572961c1dbc</UDN>"
                    "<UPC>000000000000</UPC>"
                    "<serviceList>"
                        "<service>"
                            "<serviceType>urn:schemas-upnp-org:service:WANCommonInterfaceConfig:1</serviceType>"
                            "<serviceId>urn:upnp-org:serviceId:WANCommonIFC1</serviceId>"
                            "<controlURL>/lWAuGyh/ctl/CmnIfCfg</controlURL>"
                            "<eventSubURL>/lWAuGyh/evt/CmnIfCfg</eventSubURL>"
                            "<SCPDURL>/lWAuGyh/WANCfg.xml</SCPDURL>"
                        "</service>"
                    "</serviceList>"
                    "<deviceList>"
                        "<device>"
                            "<deviceType>urn:schemas-upnp-org:device:WANConnectionDevice:2</deviceType>"
                            "<friendlyName>WANConnectionDevice</friendlyName>"
                            "<manufacturer>MiniUPnP</manufacturer>"
                            "<manufacturerURL>http://miniupnp.free.fr/</manufacturerURL>"
                            "<modelDescription>MiniUPnP daemon</modelDescription>"
                            "<modelName>MiniUPnPd</modelName>"
                            "<modelNumber>20160321</modelNumber>"
                            "<modelURL>http://miniupnp.free.fr/</modelURL>"
                            "<serialNumber>1709SAAN6</serialNumber>"
                            "<UDN>uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc</UDN>"
                            "<UPC>000000000000</UPC>"
                            "<serviceList>"
                                "<service>"
                                    "<serviceType>urn:schemas-upnp-org:service:WANIPConnection:2</serviceType>"
                                    "<serviceId>urn:upnp-org:serviceId:WANIPConn1</serviceId>"
                                    "<controlURL>/api/control</controlURL>"
                                    "<eventSubURL>/lWAuGyh/evt/IPConn</eventSubURL>"
                                    "<SCPDURL>/lWAuGyh/WANIPCn.xml</SCPDURL>"
                                "</service>"
                            "</serviceList>"
                        "</device>"
                    "</deviceList>"
                "</device>"
            "</deviceList>"
            "<presentationURL>http://192.168.1.254/</presentationURL>"
        "</device>"
    "</root>";


%%  @doc
%%  Add port mapping action response
%%
http_response(<<"control">>) ->
    "<?xml version=\"1.0\"?>\r\n"
    "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
        "<s:Body>"
            "<u:AddPortMappingResponse xmlns:u=\"urn:schemas-upnp-org:service:WANIPConnection:2\"/>"
        "</s:Body>"
    "</s:Envelope>\r\n".


