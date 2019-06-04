%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Configure router using Internet Gateway Device Protocol (IGD) protocol.
%%% @end
%%% Created : 01. Jun 2019 12.02
%%%-------------------------------------------------------------------
-module(erl_upnp_igd).
-author("bartimaeus").
-behavior(erl_upnp_gen_client).

%% API
-export([]).

%% erl_upnp_gen_client callbacks
-export([
    new_device/2,
    entity_found/2
]).

-define(COMPATIBLE_DEVICES, [
    "urn:schemas-upnp-org:device:WANIPConnection:1",
    "urn:schemas-upnp-org:device:WANIPConnection:2",
    "urn:schemas-upnp-org:device:WANPPPConnection:1"
]).



%%%===================================================================
%%% erl_upnp_gen_client callbacks
%%%===================================================================

new_device(_From, Device) ->
    io:format("Device = ~p~n~n", [Device]).

entity_found(_From, Entity) ->
    io:format("Entity = ~p~n~n", [Entity]).


