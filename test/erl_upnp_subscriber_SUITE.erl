%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% IGD integration testing.
%%% @end
%%% Created : 08. Jun 2019 19.24
%%%-------------------------------------------------------------------
-module(erl_upnp_subscriber_SUITE).
-author("bartimaeus").

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    test_subscribe/1
]).

% Internal export
-export([
    network_response/1,
    http_response/1
]).

-define(BROADCAST_PORT, 19001).
-define(EVENT_PORT, 45620).
-define(DESC_URL, "http://127.0.0.1:" ++ integer_to_list(?EVENT_PORT) ++ "/api/desc").
-define(EVENT_URL, "http://127.0.0.1:" ++ integer_to_list(?EVENT_PORT) ++ "/api/event").


%%%===================================================================
%%% API for CT.
%%%===================================================================

%%  @doc
%%  List of test cases.
%%
all() ->
    [
        test_subscribe
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
test_subscribe(_Config) ->
    SID = "uuid:3370dd89-a972-4ca2-bc85-049f65689e51",
    Result = [{"urn:schemas-upnp-org:service:RenderingControl:1",
                {"uuid:378e77bc-a198-4796-8439-720c070f7a98",
                    [{"TTL", 60},
                    {"host", {{127, 0, 0, 1}, ?EVENT_PORT}},
                    {"server", "UPnP/1.1 Samsung AllShare Server/1.0"},
                    {"SID", SID}]}
    }],
    ok = meck:new([erl_upnp_helper, erl_upnp_event_handler], [passthrough]),
    ok = meck:expect(erl_upnp_helper, get_broadcast_ip, [], {127, 0, 0, 1}),
    ok = meck:expect(erl_upnp_helper, get_broadcast_port, [], ?BROADCAST_PORT),
    ok = meck:expect(erl_upnp_helper, get_app_vsn, ['_'], "1.0.0"),
    spawn_link(erl_upnp_mock_udp, start, [?BROADCAST_PORT, ?DESC_URL, fun network_response/1]),
    erl_upnp_mock_handler:start(?EVENT_PORT, fun http_response/1),
    {ok, ClientPid} = erl_upnp_subscriber:start_link(),
    EventMgrPid = erl_upnp_subscriber:get_event_mgr_pid(ClientPid),
    CallbackUrl = erl_upnp_subscriber:get_callback(ClientPid),
    NotifyBody = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\r\n"
    "<e:propertyset xmlns:e=\"urn:schemas-upnp-org:event-1-0\">"
        "<e:property>"
            "<LastChange>"
                "&lt;Event xmlns=&quot;urn:schemas-upnp-org:metadata-1-0/RCS/&quot;&gt;&lt;InstanceID val=&quot;0&quot;&gt;&lt;Mute channel=&quot;Master&quot; val=&quot;0&quot;/&gt;&lt;PresetNameList val=&quot;FactoryDefaults&quot;/&gt;&lt;Volume channel=&quot;Master&quot; val=&quot;2&quot;/&gt;&lt;X_360View latitudeCenter=&quot;0.0000&quot; longitudeCenter=&quot;0.0000&quot; scaleFactor=&quot;2.4000&quot; val=&quot;&quot;/&gt;&lt;X_AspectRatio val=&quot;Default&quot;/&gt;&lt;X_Captions val=&quot;&quot;/&gt;&lt;X_EnabledCaptions val=&quot;&quot;/&gt;&lt;X_ServiceCapabilities val=&quot;GetVolume,SetVolume,GetMute,SetMute&quot;/&gt;&lt;/InstanceID&gt;&lt;/Event&gt;"
            "</LastChange>"
        "</e:property>\r\n"
    "</e:propertyset>",
    ok = gen_event:add_handler(EventMgrPid, erl_upnp_event_handler, []),
    Result = erl_upnp_subscriber:subscribe(ClientPid, "RenderingControl:1", [], 60),
    ok = erl_upnp_helper:notify_request(CallbackUrl, SID, NotifyBody),
    Arg = {state_var,
        'LastChange',
        "<Event xmlns=\"urn:schemas-upnp-org:metadata-1-0/RCS/\"><InstanceID val=\"0\"><Mute channel=\"Master\" val=\"0\"/><PresetNameList val=\"FactoryDefaults\"/><Volume channel=\"Master\" val=\"2\"/><X_360View latitudeCenter=\"0.0000\" longitudeCenter=\"0.0000\" scaleFactor=\"2.4000\" val=\"\"/><X_AspectRatio val=\"Default\"/><X_Captions val=\"\"/><X_EnabledCaptions val=\"\"/><X_ServiceCapabilities val=\"GetVolume,SetVolume,GetMute,SetMute\"/></InstanceID></Event>",
        '_'
    },
    ok = meck:wait(erl_upnp_event_handler, handle_event, [Arg, '_'], 5000),
    1 = meck:num_calls(erl_upnp_event_handler, handle_event, [Arg, '_']),
    ok = erl_upnp_subscriber:stop(ClientPid),
    true = meck:validate(erl_upnp_helper),
    ok = meck:unload(erl_upnp_helper),
    ok.


%%  @doc
%%  Response to request to broadcast IP.
%%
network_response(DescUrl) ->
    <<
        "HTTP/1.1 200 OK\r\n",
        "CACHE-CONTROL: max-age=1800\r\n",
        "DATE: Sun, 16 Jun 2019 11:25:35 GMT\r\n",
        "EXT: \r\n",
        "LOCATION: ", DescUrl/binary, "\r\n",
        "SERVER: Samsung-Linux/4.1, UPnP/1.0, Samsung_UPnP_SDK/1.0\r\n",
        "ST: urn:schemas-upnp-org:service:RenderingControl:1\r\n",
        "USN: uuid:378e77bc-a198-4796-8439-720c070f7a98::urn:schemas-upnp-org:service:RenderingControl:1\r\n",
        "Content-Length: 0\r\n",
        "BOOTID.UPNP.ORG: 4\r\n\r\n"
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
            "<deviceType>urn:schemas-upnp-org:device:MediaRenderer:1</deviceType>"
            "<pnpx:X_compatibleId>MS_DigitalMediaDeviceClass_DMR_V001</pnpx:X_compatibleId>"
            "<df:X_deviceCategory>Display.TV.LCD Multimedia.DMR</df:X_deviceCategory>"
            "<dlna:X_DLNADOC xmlns:dlna=\"urn:schemas-dlna-org:device-1-0\">DMR-1.50</dlna:X_DLNADOC>"
            "<friendlyName>[TV] Samsung 7 Series (50)</friendlyName>"
            "<manufacturer>Samsung Electronics</manufacturer>"
            "<manufacturerURL>http://www.samsung.com/sec</manufacturerURL>"
            "<modelDescription>Samsung TV DMR</modelDescription>"
            "<modelName>UE50NU7472</modelName>"
            "<modelNumber>AllShare1.0</modelNumber>"
            "<modelURL>http://www.samsung.com/sec</modelURL>"
            "<serialNumber>0BGB3SBK900434X</serialNumber>"
            "<UDN>uuid:378e77bc-a198-4796-8439-720c070f7a98</UDN>"
            "<iconList>"
                "<icon>"
                    "<mimetype>image/jpeg</mimetype>"
                    "<width>48</width>"
                    "<height>48</height>"
                    "<depth>24</depth>"
                    "<url>/icon_SML.jpg</url>"
                "</icon>"
            "</iconList>"
            "<serviceList>"
                "<service>"
                    "<serviceType>urn:schemas-upnp-org:service:RenderingControl:1</serviceType>"
                    "<serviceId>urn:upnp-org:serviceId:RenderingControl</serviceId>"
                    "<controlURL>/upnp/control/RenderingControl1</controlURL>"
                    "<eventSubURL>/api/event</eventSubURL>"
                    "<SCPDURL>/RenderingControl_1.xml</SCPDURL>"
                "</service>"
                "<service>"
                    "<serviceType>urn:schemas-upnp-org:service:ConnectionManager:1</serviceType>"
                    "<serviceId>urn:upnp-org:serviceId:ConnectionManager</serviceId>"
                    "<controlURL>/upnp/control/ConnectionManager1</controlURL>"
                    "<eventSubURL>/upnp/event/ConnectionManager1</eventSubURL>"
                    "<SCPDURL>/ConnectionManager_1.xml</SCPDURL>"
                "</service>"
                "<service>"
                    "<serviceType>urn:schemas-upnp-org:service:AVTransport:1</serviceType>"
                    "<serviceId>urn:upnp-org:serviceId:AVTransport</serviceId>"
                    "<controlURL>/upnp/control/AVTransport1</controlURL>"
                    "<eventSubURL>/upnp/event/AVTransport1</eventSubURL>"
                    "<SCPDURL>/AVTransport_1.xml</SCPDURL>"
                "</service>"
            "</serviceList>"
            "<sec:ProductCap>Tizen,Y2018,WebURIPlayable,NavigateInPause,ScreenMirroringP2PMAC=66:1c:b0:e0:2f:84,UHD_SCREEN,SerialNumber=0BGB3SBK900434X,vdProductType=TV,OCF=1</sec:ProductCap>"
            "<pnpx:X_hardwareId>VEN_0105&amp;DEV_VD0001</pnpx:X_hardwareId>"
        "</device>"
    "</root>";


%%  @doc
%%  Add port mapping action response
%%
http_response(<<"event">>) ->
    "HTTP/1.1 200 OK\r\n"
    "CONNECTION: close\r\n"
    "SID: uuid:3370dd89-a972-4ca2-bc85-049f65689e51\r\n"
    "DATE: Sun, 16 Jun 2019 11:51:32 GMT\r\n"
    "SERVER:UPnP/1.1 Samsung AllShare Server/1.0\r\n"
    "TIMEOUT: Second-60\r\n"
    "CONTENT-LENGTH: 0\r\n\r\n".


