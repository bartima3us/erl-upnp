Universal Plug and Play (UPnP) control point
=====

This Universal Plug and Play client (control point) which was originally created as a support library for https://github.com/bartima3us/erl-bittorrent to make file seeding process easily.
Based on that fact it has only automated port forwarding and subscription. Other actions could be made manually or easily automated as described in "Extending" section.

UPnP architecture and specification documents: https://openconnectivity.org/developer/specifications/upnp-resources/upnp


SSDP client usage
=====

Start client and network discovering in two steps
```
{ok, ClientPid} = erl_upnp_client:start_link().
erl_upnp_client:start_discover(ClientPid, Delay, Target).
```

Start client and network discovering in one step
```
{ok, ClientPid} = erl_upnp_client:start_discover_link(Delay, Target).
```

- ```Delay :: pos_integer()``` Time (in **seconds**) by which network should send responses. It is needed to avoid packets flood (network congestion). Recommended: 2.


- ```Target :: ssdp_all``` Returns all UPnP compatible devices and services from the network.
- ```Target :: upnp_rootdevice``` Returns all UPnP root devices from the network.
- ```Target :: {uuid, list()}``` Returns device with concrete UUID. UUID example: "429ec14c-8e4a-4074-89cc-01fc84e31e1f"
- ```Target :: list()``` Some custom search request. Example: "urn:schemas-upnp-org:service:Layer3Forwarding:1"

Return all entities from the state
```
erl_upnp_client:get_devices(ClientPid, ReturnFormat).
```

- ```ReturnFormat :: hierarchical``` Return all devices and entities in their original places from hierarchical point of view.
- ```ReturnFormat :: flat``` Return all embedded devices in one list.

Find entities in the state by the key
```
erl_upnp_client:find_entity(ClientPid, Key).
erl_upnp_client:find_entity(ClientPid, Key).
```

- ```Key :: list()``` Key is a device or service type with or without version. Key examples: "InternetGatewayDevice", "Layer3Forwarding:1", "WANDevice:2"

Return all unidentified entities
```
erl_upnp_client:get_unidentified_devices(ClientPid).
```

Return a port which control point (client) is using as a source port
```
erl_upnp_gen_client:get_port(ClientPid)
```

Stop the client
```
erl_upnp_gen_client:stop(ClientPid)
```

Attach event handler to client event manager and get data about new entities as soon as they are discovered.
```
EventMgrPid = erl_upnp_client:get_event_mgr_pid(ClientPid)
add_sup_handler(EventMgrPid, your_handler_mod, []).
```

Events which should be handled in the attached handler:
- ```{raw_entity_discovered, Data}```
- ```{device_discovered, Data}```

Port mapper (IGD)
-----

Extending
-----

Tests
-----