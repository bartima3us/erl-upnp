%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% erl_upnp_subscriber module unit tests.
%%% @end
%%% Created : 16. Jun 2019 18.02
%%%-------------------------------------------------------------------
-module(erl_upnp_subscriber_tests).
-author("bartimaeus").
-include_lib("eunit/include/eunit.hrl").


decode_packet_test_() ->
    Packet = <<
        "Content-Type: text/xml; charset=\"utf-8\"\r\n",
        "Connection: close\r\n",
        "Content-Length: 0\r\n",
        "Server: OpenWRT/OpenWrt/Attitude_Adjustment__r43446_ UPnP/1.1 MiniUPnPd/1.8\r\n",
        "Ext:\r\n",
        "Date: Sun, 16 Jun 2019 15:07:48 GMT\r\n",
        "Timeout: Second-1800\r\n",
        "SID: uuid:4fb866f7-1858-4d82-ba43-e572961c823a\r\n\r\n"
    >>,
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [{"Decode packet.",
            fun() ->
                ?assertEqual(
                    {ok,[{"SID", "uuid:4fb866f7-1858-4d82-ba43-e572961c823a"},
                          {"server", "OpenWRT/OpenWrt/Attitude_Adjustment__r43446_ UPnP/1.1 MiniUPnPd/1.8"},
                          {"connection","close"}]},
                    erl_upnp_subscriber:decode_packet(Packet, 3, [])
                )
            end
        }]
    }.


get_timeout_actions_test_() ->
    ResInt = [{"urn:schemas-upnp-org:service:RenderingControl:1",
        {"uuid:378e77bc-a198-4796-8439-720c070f7a98",
         [{"TTL",1800},
          {"host",{{192,168,1,166},9197}},
          {"SID", "uuid:6703330f-c50a-46ab-bd68-d563dede8fc0"}]}}],
    ResInfinite = [{"urn:schemas-upnp-org:service:WANIPConnection:2",
        {"uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc",
         [{"TTL",infinite},
          {"host", {{192,168,1,254},5000}},
          {"SID", "uuid:4fb866f7-1858-4d82-ba43-e572961c823a"},
          {"Server", "OpenWRT/OpenWrt/Attitude_Adjustment__r43446_ UPnP/1.1 MiniUPnPd/1.8"}]}}],
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [{"Get timeout actions with integer timeout.",
            fun() ->
                ?assertEqual(
                    [{{timeout, remove_subscription},
                        1800000,
                        "uuid:6703330f-c50a-46ab-bd68-d563dede8fc0"}],
                    erl_upnp_subscriber:get_timeout_actions(1800, ResInt)
                )
            end
        },
        {"Get timeout actions with infinite timeout.",
            fun() ->
                ?assertEqual(
                    [{{timeout,
                        {refresh_subscription,
                        "uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc",
                        "uuid:4fb866f7-1858-4d82-ba43-e572961c823a"}},
                    1740000,
                    "urn:schemas-upnp-org:service:WANIPConnection:2"}],
                    erl_upnp_subscriber:get_timeout_actions(infinite, ResInfinite)
                )
            end
        }]
    }.


remove_subscription_test_() ->
    Subs = [{"urn:schemas-upnp-org:service:WANIPConnection:2",
              {"uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc",
               [{"TTL",infinite},
                {"host",{{192,168,1,254},5000}},
                {"SID",
                 "uuid:4fb866f7-1858-4d82-ba43-e572961c823a"},
                {"Server",
                 "OpenWRT/OpenWrt/Attitude_Adjustment__r43446_ UPnP/1.1 MiniUPnPd/1.8"}]}},
            {"urn:schemas-upnp-org:service:RenderingControl:1",
              {"uuid:378e77bc-a198-4796-8439-720c070f7a98",
               [{"TTL",10},
                {"host",{{192,168,1,166},9197}},
                {"SID",
                 "uuid:6703330f-c50a-46ab-bd68-d563dede8fc0"}]}}],
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [{"Remove subscription from state.",
            fun() ->
                ?assertEqual(
                    [{"urn:schemas-upnp-org:service:WANIPConnection:2",
                         {"uuid:4fb866f7-1858-4d82-ba45-e572961c1dbc",
                          [{"TTL",infinite},
                           {"host",{{192,168,1,254},5000}},
                           {"SID",
                            "uuid:4fb866f7-1858-4d82-ba43-e572961c823a"},
                           {"Server",
                            "OpenWRT/OpenWrt/Attitude_Adjustment__r43446_ UPnP/1.1 MiniUPnPd/1.8"}]}}],
                    erl_upnp_subscriber:remove_subscription(Subs, "uuid:6703330f-c50a-46ab-bd68-d563dede8fc0")
                )
            end
        }]
    }.


