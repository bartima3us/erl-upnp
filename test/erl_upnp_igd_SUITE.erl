%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
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
    spawn_link(erl_upnp_mock_udp, start, [?BROADCAST_PORT, ?DESC_URL]),
    erl_upnp_mock_handler:start(?CONTROL_PORT),
    {ok, ClientPid} = erl_upnp_igd:start_link(),
    Result = erl_upnp_igd:add_port_mapping(ClientPid, "", 6020, 6030, tcp, "For BitTorrent.", 60),
    true = meck:validate(erl_upnp_helper),
    ok = meck:unload(erl_upnp_helper),
    ok.


