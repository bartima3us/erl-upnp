Universal Plug and Play (UPnP) control point
=====

This Universal Plug and Play client (control point) which was originally created as a support library for https://github.com/bartima3us/erl-bittorrent to make file seeding process easily.
Based on that fact it has only automated port forwarding and subscription. Other actions could be made manually or easily automated as described in "Extending" section.

UPnP architecture and specification documents: https://openconnectivity.org/developer/specifications/upnp-resources/upnp

```
{ok, Pid} = erl_upnp_client:start_link().
erl_upnp_client:start_discover(Pid, 10, ssdp_all).
erl_upnp_client:find_devices(Pid, flat).
erl_upnp_client:find_entity(Pid, "InternetGatewayDevice:2").
erl_upnp_client:find_entity(Pid, "ConnectionManager").
```

Got event manager pid: erl_upnp_client:get_event_mgr_pid(ClientPid)

Events:
```
{raw_entity_discovered, Data}
{device_discovered, Data}
```