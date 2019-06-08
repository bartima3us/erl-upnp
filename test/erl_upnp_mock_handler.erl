%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Control point description and action mocks.
%%% @end
%%% Created : 08. Jun 2019 20.29
%%%-------------------------------------------------------------------
-module(erl_upnp_mock_handler).

%% API
-export([
    init/2,
    start/1
]).


%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%%  @doc
%%  Cowboy mock server init.
%%
init(Req0, State) ->
    {ok, #{action := Action}} = maps:find(bindings, Req0),
    Resp = response(Action),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"plain/text">>}, Resp, Req0),
    {ok, Req, State}.



%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Start HTTP server.
%%
start(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/:action", ?MODULE, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }).



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%  @private
%%  Cowboy mock server description URL response.
%%
response(<<"desc">>) ->
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

%%  @private
%%  Add port mapping action response
%%
response(<<"control">>) ->
    "<?xml version=\"1.0\"?>\r\n"
    "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
        "<s:Body>"
            "<u:AddPortMappingResponse xmlns:u=\"urn:schemas-upnp-org:service:WANIPConnection:2\"/>"
        "</s:Body>"
    "</s:Envelope>\r\n".


