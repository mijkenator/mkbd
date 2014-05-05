-module(pair_keeper).

-behaviour(gen_server).

%% API
-export([start_link/0, 
    set/2, get/1, unset/1,
    setbu/2, getbu/1, unsetbu/1,
    setptp/2, getptp/1, unsetptp/1,
    unpair/2,
    process_command/4
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, check_pair/1]).

-record(state, {tab, tabptp, tabbu}).
-define(SERVER, ?MODULE).
-define(TAB, pair_keeper_tab).
-define(TABPTP, pair_to_pair_tab).
-define(TABBU, base_url_tab).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set(Key, Value) ->
  gen_server:call(?MODULE, {set, Key, Value}).

get(Key) -> ets:lookup(?TAB, Key).

unset(Key) -> 
  gen_server:call(?MODULE, {unset, Key}).

setptp(Key, Value) ->
  gen_server:call(?MODULE, {setptp, Key, Value}).

getptp(Key) -> ets:lookup(?TABPTP, Key).

unsetptp(Key) -> 
  gen_server:call(?MODULE, {unsetptp, Key}).

setbu(Key, Value) ->
  gen_server:call(?MODULE, {setbu, Key, Value}).

getbu(Key) -> ets:lookup(?TABBU, Key).

unsetbu(Key) -> 
  gen_server:call(?MODULE, {unsetbu, Key}).

unpair(MasterKey, MasterPid) when is_binary(MasterKey) ->
    unpair(list_to_integer(binary_to_list(MasterKey)), MasterPid);
unpair(MasterKey, _MasterPid) ->
    lager:debug("PK unpair ~p ~p ~p", [MasterKey, _MasterPid, self()]),
    case getptp(MasterKey) of
        [{MasterKey, {_, SlaveKey}}] ->
            case ?MODULE:get(SlaveKey) of
               [{SlaveKey, {SlavePid, _}}] -> SlavePid ! {unpair, self()},
                unsetptp(MasterKey),
                unsetptp(SlaveKey),
                %unset(MasterKey),
                %unset(SlaveKey),
               <<"ok">>
               ;_ -> <<"unpair failed 2">>
            end
        ;_ -> <<"unpair failed 1">>
    end.

check_pair(MasterKey) when is_binary(MasterKey) -> check_pair(mkhu:to_type(MasterKey, integer));
check_pair(MasterKey) when is_integer(MasterKey)->
    case getptp(MasterKey) of
        [{MasterKey, {_, SlaveKey}}] ->
            case ?MODULE:get(SlaveKey) of
               [{SlaveKey, {SlavePid, _}}] -> {ok, SlaveKey, SlavePid}
               ;_ -> error
            end
        ;_ -> error
    end.
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  Tab = ets:new(?TAB, [set, protected, named_table]),
  TabPtp = ets:new(?TABPTP, [set, protected, named_table]),
  TabBaseUrl = ets:new(?TABBU, [set, protected, named_table]),
  {ok, #state{tab=Tab, tabptp=TabPtp, tabbu=TabBaseUrl}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({unsetptp, K}, _From, #state{tabptp = TabPtp} = State) ->
  ets:delete(TabPtp, K),
  {reply, ok, State};
handle_call({setptp, K, V}, _From, #state{tabptp = TabPtp} = State) ->
  ets:insert(TabPtp, {K, V}),
  {reply, ok, State};
handle_call({unset, K}, _From, #state{tab = Tab} = State) ->
  ets:delete(Tab, K),
  {reply, ok, State};
handle_call({set, K, V}, _From, #state{tab = Tab} = State) ->
  ets:insert(Tab, {K, V}),
  {reply, ok, State};
handle_call({unsetbu, K}, _From, #state{tabbu = Tab} = State) ->
  ets:delete(Tab, K),
  {reply, ok, State};
handle_call({setbu, K, V}, _From, #state{tabbu = Tab} = State) ->
  ets:insert(Tab, {K, V}),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

process_command(_,SlavePid,_,<<"goto:", Url/binary>>) ->
    lager:debug("goto to url: ~p", [Url]),
    SlavePid ! {<<"goto">>, Url},
    ok;
process_command(_,SlavePid,_,<<"proxy:", Url/binary>>) ->
    lager:debug("proxy to url: ~p", [Url]),
    SlavePid ! {<<"proxy">>, Url},
    ok;
process_command(SlaveKey, _SlavePid, MKHPCookie, Command) ->
    lager:debug("PC unknown command ~p ~p ~p", [SlaveKey, MKHPCookie, Command]),
    ok.

