%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2019 21.31
%%%-------------------------------------------------------------------
-module(erl_upnp_test_impl).
-author("bartimaeus").
-behaviour(gen_server).

%% API
-export([
    start_link/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    devices = []
}).


%%%===================================================================
%%% API
%%%===================================================================

%%
%%
%%
start_link(Target) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Target], []).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Target]) ->
    {ok, Pid} = erl_upnp_client:start_link(?MODULE),
    self() ! {discover_devices, Pid, Target},
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({discover_devices, Pid, Target}, State) ->
    ok = erl_upnp_client:start_discover(Pid, 2, Target),
    {noreply, State};

handle_info({make_request, Pid, _, _}, State) ->
    Port = erl_upnp_client:get_port(Pid),
%%    Body = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
%%    "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
%%      "<s:Body>"
%%        "<u:GetDefaultConnectionService xmlns:u=\"urn:schemas-upnp-org:service:Layer3Forwarding:1\">"
%%        "</u:GetDefaultConnectionService>"
%%      "</s:Body>"
%%    "</s:Envelope>",
    Body = "",
    Link = "http://192.168.1.254:5000/xsWPWRe/ctl/L3F",
%%    Link = "http://192.168.1.167:9197/upnp/control/ConnectionManager1",
    Headers = [
        {"SOAPAction",     "urn:schemas-upnp-org:service:ConnectionManager:1#GetDefaultConnectionService"},
        {"Host",           "192.168.1.149:" ++ integer_to_list(Port)},
        {"Content-Length", length(Body)}
    ],
    Resp = httpc:request(post, {Link, Headers, "application/xml; charset=\"utf-8\"", Body}, [], []),
    io:format("Resp =~p~n", [Resp]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================


