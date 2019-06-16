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
    start/3
]).


%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Start UDP mock server.
%%
start(BroadcastPort, DescUrl, RespFun) ->
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
            gen_udp:send(Socket, Addr, Port, RespFun(list_to_binary(DescUrl)))
    after 5000 ->
        erlang:error(timeout)
    end.


