-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {sip_pid, sip_proxy, advaddr, peer, cookie}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket, _Req, _Opts}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:debug("WSI ~p ------ ~n -------- ~n ~p", [Req, _Opts]),
    Req1 = case cowboy_req:header(<<"sec-websocket-protocol">>, Req) of
        {undefined, _} -> Req;
        {SWSProto,  _} -> cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, SWSProto, Req)
    end,
    {{Ip,Port}, _} = cowboy_req:peer(Req),
    {MKHPCookie, Req3} = case cowboy_req:cookie(<<"mkh_pair_cookie">>, Req1, <<"">>) of
        {<<"">>, Req2} ->
            {A1,A2,A3} = now(),
            random:seed(A1, A2, A3),
            Rnd  = list_to_binary(integer_to_list(random:uniform(10000000000000))),
            CV   = Rnd, 
            Opts = [{max_age, 3600}], 
            lager:debug("MSG: GPG1", []),
            {Rnd, cowboy_req:set_resp_cookie(<<"mkh_pair_cookie">>, CV, Opts, Req2)};
        {C, R} ->
            lager:debug("MSG: GPG2", []),
            {C, R}
    end,
    pair_keeper:set(list_to_integer(binary_to_list(MKHPCookie)), {self(), now()}),
    {ok, Req3, #state{ peer = {list_to_binary(inet_parse:ntoa(Ip)), 
                               list_to_binary(integer_to_list(Port))}, 
                               cookie = MKHPCookie}}.

websocket_handle({text, <<"get_pair_guid">>}, Req, #state{peer = {_Ip, _Port}, cookie = MKHPCookie} = State) ->
    lager:debug("MSG: GPG", []),
    Ret = <<"{\"type\":\"get_pair_guid\", \"value\":\"",MKHPCookie/binary,"\"}">>,
    {reply, {text, << Ret/binary >>}, Req, State};

websocket_handle({text, <<"command:unpair">>}, Req, #state{peer = {_Ip, _Port}, cookie = MKHPCookie} = State) ->
    lager:debug("MSG: unpair", []),
    UNPR = pair_keeper:unpair(MKHPCookie, self()),
    lager:debug("umpair ret: ~p", [UNPR]),
    Ret = <<"{\"type\":\"event\", \"event_name\":\"unpair_m\"}">>,
    {reply, {text, << Ret/binary >>}, Req, State};

websocket_handle({text, <<"command:",Command/binary>>}, Req, #state{cookie = MKHPCookie} = State) ->
    lager:debug("MSG: command ~p ", [Command]),
    Ret = case pair_keeper:check_pair(MKHPCookie) of
        {ok, SlaveKey, SlavePid} ->
              lager:debug("SKSP: ~p ~p", [SlaveKey, SlavePid]),
              case pair_keeper:process_command(SlaveKey, SlavePid, MKHPCookie, Command) of
                ok -> <<"{\"type\":\"command\", \"status\":\"ok\"}">>;
                {error, Reason} -> <<"{\"type\":\"command\", \"status\":\"failed\", \"error\":\"",
                                   (mkhu:to_bin(Reason))/binary,"\"}">> 
              end
        ;_ -> <<"{\"type\":\"command\", \"status\":\"failed\"}">>
    end,
    lager:debug("Command ret: ~p", [Ret]),
    {reply, {text, << Ret/binary >>}, Req, State};

websocket_handle({text, <<"pair:",PairGuid/binary>>}, Req, #state{cookie = MKHPCookie} = State) ->
    lager:debug("MSG: pair ~p ", [PairGuid]),
    Dst = list_to_integer(binary_to_list(PairGuid)),
    PairStatus = case pair_keeper:get(Dst) of
        [{Dst, {Pid, _}}] ->
            case is_process_alive(Pid) of
                true ->
                          Pid ! {pair, self(), MKHPCookie},
                          <<"ok">>
                ;_   ->   <<"failed2">>
            end
        ;_ -> <<"failed1">>
    end,
    Ret = <<"{\"type\":\"pair\", \"status\":\"",PairStatus/binary,"\"}">>,
    {reply, {text, << Ret/binary >>}, Req, State};

websocket_handle({text, Msg}, Req, #state{peer = {_Ip, _Port}} = State) ->
    lager:debug("MSG: ~p ", [Msg]),
    Ret = <<"{\"reply\":\"muhaha\"}">>,
    {reply, {text, << Ret/binary >>}, Req, State};


websocket_handle(_Data, Req, State) ->
    lager:debug("WSH ~p", [_Data]),
    {ok, Req, State}.

websocket_info({pair, _Pid, PairCookie10}, Req, #state{cookie = MKHPCookie0} = State) ->
    MKHPCookie  = list_to_integer(binary_to_list(MKHPCookie0)),
    PairCookie1 = list_to_integer(binary_to_list(PairCookie10)),
    pair_keeper:setptp(PairCookie1, {slave, MKHPCookie}),   
    pair_keeper:setptp(MKHPCookie, {master, PairCookie1}),
    Resp = <<"{\"type\":\"event\", \"event_name\":\"paired\", \"pv\":\"", MKHPCookie0/binary, "\"}">>,
    {reply, {text, Resp}, Req, State};
websocket_info({unpair, _Pid}, Req, State) ->
    Resp = <<"{\"type\":\"event\", \"event_name\":\"unpair_s\"}">>,
    {reply, {text, Resp}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info({<<"goto">>, Url}, Req, State) ->
    Resp = <<"{\"type\":\"event\", \"event_name\":\"s_goto\", \"src\":\"", Url/binary ,"\"}">>,
    {reply, {text, Resp}, Req, State};
websocket_info({<<"proxy">>, Url}, Req, State) ->
    Resp = <<"{\"type\":\"event\", \"event_name\":\"s_proxy\", \"src\":\"", Url/binary ,"\"}">>,
    {reply, {text, Resp}, Req, State};
websocket_info(_Info, Req, State) ->
    lager:debug("WSInf ~p", [_Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{cookie = MKHPCookie} = _State) ->
    pair_keeper:unset(MKHPCookie),
    case pair_keeper:getptp(MKHPCookie) of
        [{MKHPCookie, {slave, PairCookie1}}] -> pair_keeper:unsetptp(PairCookie1)
        ;_ -> ok
    end,
    pair_keeper:unsetptp(MKHPCookie),
    ok.

