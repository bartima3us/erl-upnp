%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Broadcast mocks.
%%% @end
%%% Created : 08. Jun 2019 20.58
%%%-------------------------------------------------------------------
-module(erl_upnp_mock_udp).
-author("bartimaeus").

%% API
-export([
    start/2
]).


%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Start UDP mock server.
%%
start(BroadcastPort, DescUrl) ->
    {ok, Socket} = gen_udp:open(BroadcastPort, [binary, {active, once}]),
    WaitingReq = <<
        "M-SEARCH * HTTP/1.1\r\n",
        "HOST:127.0.0.1:", (integer_to_binary(BroadcastPort))/binary ,"\r\n",
        "ST:ssdp:all\r\n",
        "MX:2\r\n",
        "MAN:\"ssdp:discover\"\r\n\r\n"
    >>,
    receive
        {udp, _SockPort, Addr = {127, 0, 0, 1}, Port, WaitingReq} ->
            gen_udp:send(Socket, Addr, Port, response(list_to_binary(DescUrl)))
    after 5000 ->
        erlang:error(timeout)
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%  @private
%%  Response to request to broadcast IP.
%%
response(DescUrl) ->
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


