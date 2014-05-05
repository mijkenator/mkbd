-module(mkh_proxy_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Reply} = case Method of
        <<"POST">> -> 
            HasBody = cowboy_req:has_body(Req2),
            post_proxer(HasBody, Req2);
        <<"GET">>  ->
            {To, Req3} = cowboy_req:qs_val(<<"__mkh_to">>, Req2),
            get_proxer(To ,Req3)
        ;_ -> unsupport_request(Method, Req2)
    end,
    {ok, Reply, State}.


get_proxer(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
get_proxer(To, Req) ->
    lager:debug("GP1: To: ~p", [To]),
    {ok,{_,_,Ret}} =mkhu:get_url(To),

    {Ret, Req2} = case mkhu:get_url(To) of
        {ok,{200, H, R}} -> 
            Fun = fun({<<"set-cookie">>, C}, ReqN) ->
                                  cowboy_req:set_resp_header(<<"set-cookie">>, C, ReqN)
                    ;(_, ReqN) -> ReqN
            end,
            {R, lists:foldl(Fun, Req, H)};
        {ok,{S,H,R}}     ->
              lager:error("GR unkret1 ~p ~p ~p", [S,H,R]),
              {<<"unknown ret1">>, Req}
        ;R -> 
              lager:error("GR unkret2 ~p", [R]),
              {<<"unknown ret2">>, Req}
    end,

    case cowboy_req:qs_val(<<"__mkh_i">>, Req2) of
        {undefined, _} -> ok;
        {<<"1">>,   _} ->
            case cowboy_req:cookie(<<"mkh_pair_cookie">>, Req2) of
                {Cookie, _} when is_binary(Cookie), size(Cookie) > 1 
                   -> pair_keeper:setbu(Cookie, {To})
                ;_ -> ok
            end
    end,

    cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/html; charset=utf-8">>}
    ], mkhu:to_bin(Ret), Req2).


post_proxer(true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    Echo = proplists:get_value(<<"echo">>, PostVals),
    cowboy_req:reply2(200, [
        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
    ], <<"post proxier">>, Req2);
post_proxer(false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req).


unsupport_request(Method, Req) ->
    %% Method not allowed.
    lager:debug("UNSUPPORT REQ: ~p ~p ", [Method, Req]),
    cowboy_req:reply(405, Req).


terminate(_Reason, _Req, _State) ->
    ok.

