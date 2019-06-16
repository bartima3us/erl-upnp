Universal Plug and Play (UPnP) control point
=====

- [Introduction](#introduction)
- [SSDP client usage](#ssdp_client_usage)
- [Port forwarding (IGD)](#port_forwarding_igd)
- [Subscription](#subscription)
- [Extending](#extending)
- [Tests](#tests)

## <a name="introduction">Introduction</a> ##

Universal Plug and Play client (control point) which was originally created as a support library for https://github.com/bartima3us/erl-bittorrent to make file seeding process easily.
Based on that fact it has only automated port forwarding and subscription. Other actions can be made manually or easily automated as described in "Extending" section.

UPnP architecture and specification documents: https://openconnectivity.org/developer/specifications/upnp-resources/upnp

## <a name="ssdp_client_usage">SSDP client usage</a> ##

Start the client and network discovering in two steps
```
{ok, ClientPid} = erl_upnp_client:start_link().
erl_upnp_client:start_discover(ClientPid, Delay, Target).
```

Start the client and network discovering in one step
```
{ok, ClientPid} = erl_upnp_client:start_discover_link(Target, Opts).
```

- ```Delay :: pos_integer()``` Time (in **seconds**) by which network should send responses. It is needed to avoid packets flood (network congestion). Recommended: 2.


- ```Target```:
    - ```Target :: ssdp_all``` Return all UPnP compatible devices and services from the network.
    - ```Target :: upnp_rootdevice``` Return all UPnP root devices from the network.
    - ```Target :: {uuid, string()}``` Return device with concrete UUID. UUID example: "429ec14c-8e4a-4074-89cc-01fc84e31e1f"
    - ```Target :: string()``` Some custom search request. Example: "urn:schemas-upnp-org:service:Layer3Forwarding:1"


- ```Opts :: [option()]```:
    - ```{delay, Time :: pos_integer()}``` - The same as Delay in erl_upnp_client:start_discover/3 function. Default: 2.
    - ```{poll, Time :: pos_integer()}``` - Time in **milliseconds**. Will poll network every `Time` milliseconds to search a new devices. Default: false (that means poll will occur only one time).

Constant network polling can also be enabled by direct call
```
erl_upnp_client:start_poll(ClientPid, Time :: pos_integer()).
```

...and disabled
```
erl_upnp_client:stop_poll(ClientPid).
```

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

- ```Key :: string()``` Key is a device or service type with or without version, or a full service type. Key examples: "InternetGatewayDevice", "Layer3Forwarding:1", "WANDevice:2", "urn:schemas-upnp-org:service:Layer3Forwarding:1".

Return all unidentified entities
```
erl_upnp_client:get_unidentified_devices(ClientPid).
```

Return a port which control point (client) is using as a source port
```
erl_upnp_client:get_port(ClientPid)
```

Stop the client
```
erl_upnp_client:stop(ClientPid)
```

Attach event handler to client event manager and get data about new entities as soon as they are discovered.
```
EventMgrPid = erl_upnp_client:get_event_mgr_pid(ClientPid)
add_sup_handler(EventMgrPid, your_handler_mod, []).
```

Events which should be handled in the attached handler:
- ```{raw_entity_discovered, Data}```
- ```{device_discovered, Data}```

## <a name="port_forwarding_igd">Port forwarding (IGD)</a> ##

Start the client
```
{ok, ClientPid} = erl_upnp_igd:start_link().
```

Add new port mapping (port forwarding)
```
erl_upnp_igd:add_port_mapping(ClientPid, Host, ExternalPort, InternalPort, Protocol, Description, TTL).
```

- ```Host :: inet:ip4_address() | string()``` Domain name or IP from which can be received packets only after port forward. Empty string - wildcard (all IP's).
- ```ExternalPort :: inet:port_number()``` External port **to** which port should be mapped. 0 means all external ports will be opened (very dangerous!).
- ```InternalPort :: inet:port_number()``` External port **from** which port should be mapped.
- ```Protocol :: tcp | udp``` TCP or UDP mapper.
- ```Description :: string()``` Just plain description what this port forward about.
- ```TTL :: pos_integer()``` Time to live for port forward in seconds. Valid range: [1, 604800].

Example<br/>
TCP port forwarding from internal port 6020 to external port 6030 which will be automatically deleted after 60 seconds.
```
erl_upnp_igd:add_port_mapping(ClientPid, "", 6020, 6030, tcp, "For BitTorrent.", 60).
```

Add new any port mapping. This function is almost the same as erl_upnp_igd:add_port_mapping/7 except that if given external port is already in use, it will select one of the free external ports and make a port mapper on it.
```
erl_upnp_igd:add_any_port_mapping(ClientPid, Host, ExternalPort, InternalPort, Protocol, Description, TTL).
```

Return data about particular port mapping
```
erl_upnp_igd:get_port_mapping(ClientPid, Host, ExternalPort, Protocol).
```

Example
```
erl_upnp_igd:get_port_mapping(ClientPid, "", 6020, tcp).
```

Delete particular port mapping
```
erl_upnp_igd:delete_port_mapping(ClientPid, Host, ExternalPort, Protocol).
```

Example
```
erl_upnp_igd:delete_port_mapping(ClientPid, "", 6020, tcp).
```

Stop the client
```
erl_upnp_igd:stop(ClientPid).
```

## <a name="subscription">Subscription</a> ##

TODO

## <a name="extending">Extending</a> ##

This UPnP control point can be easily extended with more services support.<br/>
Helpers to make request can be found in erl_upnp_helper module.<br/>
2 examples how to use device discovering and make a request: erl_upnp_igd and erl_upnp_rendering_control (this one just for fun).

## <a name="tests">Tests</a> ##

EUnit and CT tests
```
$ make tests
```
