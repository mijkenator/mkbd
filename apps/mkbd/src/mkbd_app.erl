-module(mkbd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/mkh_mkbd", ws_handler, []},
                    {"/__mkh_proxy", mkh_proxy_handler, []},
                    {"/pair", cowboy_static,  {file, "../../static/pair.html"}},
                    {"/[...]", cowboy_static, {dir, "../../static"}}
            ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    mkbd_sup:start_link().

stop(_State) ->
    ok.
