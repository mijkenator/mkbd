-module(mkhu).

-export([
    to_binary/1,
    get_type/1,
    to_type/3,
    to_type/2,
    '_atom_to_binary'/1, 
    '_binary_to_atom'/1, 
    '_binary_to_pid'/1,  
    '_binary_to_tuple'/1,
    '_pid_to_binary'/1,
    to_bin/1,
    parse_uri/1,
    get_url/1,
    ht/0,
    html_p/3,
    get_base/1,
    get_base_p/1,
    t/0
]).

-spec to_binary(any()) -> binary().

to_binary(B) when is_binary(B) -> B;
to_binary(B) when is_list(B) -> list_to_binary(B);
to_binary(B) when is_atom(B) -> atom_to_binary(B, utf8);
to_binary(B) when is_integer(B) -> integer_to_binary(B);
to_binary(B) -> B.

-spec get_type(
    atom()      |
    binary()    |
    float()     |
    integer()   |
    list()      | 
    pid()       |
    port()      |
    reference() |
    tuple()) -> atom().
get_type(T) when is_atom(T)         -> atom;
get_type(T) when is_binary(T)       -> binary;
get_type(T) when is_float(T)        -> float;
get_type(T) when is_integer(T)      -> integer;
get_type(T) when is_list(T)         -> list;
get_type(T) when is_pid(T)          -> pid;
get_type(T) when is_port(T)         -> port;
get_type(T) when is_tuple(T)        -> tuple.

-spec to_type(any(), binary(), any()) -> any().
to_type(Term, Type, Default) ->
    try get_type(Term) of
        Type  -> Term;
        RType -> 
            ConvertFun = list_to_existing_atom("_" ++ atom_to_list(RType) ++ "_to_" ++ atom_to_list(Type)),
            ConvertFun(Term)
    catch
        _:_ -> Default
    end.

-spec to_type(any(), binary()) -> any().
to_type(Term, Type) ->
    case get_type(Term) of
        Type  -> Term;
        RType -> 
            {M, ConvertFun} = gbif(atom_to_list(RType) ++ "_to_" ++ atom_to_list(Type)),
            M:ConvertFun(Term)
    end.

existing_type_converters() -> [
    "atom_to_list", "binary_to_float", "binary_to_integer", "binary_to_list",
    "float_to_binary", "float_to_list", "integer_to_list", "integer_to_binary", 
    "list_to_atom", "list_to_binary",
    "list_to_float", "list_to_integer", "list_to_pid", "list_to_tuple", "pid_to_list",
    "port_to_list", "tuple_to_list" 
].

gbif(L) ->
    case lists:member(L, existing_type_converters()) of
        true -> {erlang, list_to_existing_atom(L)}
        ;_   -> {?MODULE, list_to_existing_atom("_" ++ L)}
    end.


'_atom_to_binary' (A) -> atom_to_binary(A, utf8).
'_binary_to_atom' (B) -> binary_to_atom(B, utf8).
'_binary_to_pid'  (B) -> list_to_pid(binary_to_list(B)).
'_binary_to_tuple'(B) -> list_to_tuple(binary_to_list(B)).
'_pid_to_binary'  (P) -> list_to_binary(pid_to_list(P)).

to_bin(T) -> to_type(T, binary).

-spec parse_uri(binary()) -> {list(), integer(), list()} | error.
parse_uri(U) when is_list(U) -> parse_uri(list_to_binary(U));
parse_uri(<<"http://", _/binary>> = U)  -> '_parse_uri'(U);
parse_uri(<<"https://", _/binary>> = U) -> '_parse_uri'(U);
parse_uri(U) when is_binary(U)-> '_parse_uri'(<<"http://", U/binary>>).

'_parse_uri'(Uri)  -> case http_uri:parse(to_type(Uri, list)) of
        {ok, {_, _, Host, Port, Path, _}} -> {Host, Port, Path}
        ;_ -> error
    end.

get_url(Url) -> get_url(Url, [], [
    {follow_redirects, true},
    {redirect_loop, 10},
    {timeout, 5000}]). 
get_url(Url, RHeaders, Opts) -> 
    {Host, Port, Uri} = mkhu:parse_uri(Url),
    lager:debug("GP2: To: ~p ~p ~p ", [Host, Port, Uri]),
    {ok, Pid} = gun:open(Host, Port),
    StreamRef = gun:get(Pid, Uri, [
        {"user-agent", "revolver/1.0"}]),

    FR = proplists:get_value(follow_redirects, Opts, false),
    RL = proplists:get_value(redirect_loop, Opts, 10),
    TO = proplists:get_value(timeout, Opts, 5000),
    case receive_resp(Pid, StreamRef, TO) of
        {ok, {S, H, _} = Ret} when S=:=301;S=:=302 ->
           case FR of
              true when RL > 0  ->
                NUrl  = proplists:get_value(<<"location">>, H),
                NOpts = proplists:delete(redirect_loop, Opts) ++ [{redirect_loop, RL - 1}],
                get_url(NUrl, RHeaders, NOpts);
              false -> {ok, Ret} 
           end
        ;{ok, Ret} -> 
            gun:shutdown(Pid),
            {ok, Ret}
        ;R -> R
    end.

receive_resp(Pid, StreamRef, Timeout) ->
    receive
        {'DOWN', _Tag, _, _, Reason} ->
            {error, Reason};
        {gun_response, Pid, StreamRef, fin, Status, Headers} ->
            {ok, {Status, Headers, <<>>}};
        {gun_response, Pid, StreamRef, nofin, Status, Headers} ->
            {ok, {Status, Headers, receive_data(Pid, StreamRef, <<>>, Timeout)}}
    after Timeout ->
        {error, timeout}
    end.

receive_data(Pid, StreamRef, A, Timeout) ->
    receive
        {'DOWN', _Tag, _, _, Reason} ->
            {error, Reason};
        {gun_data, Pid, StreamRef, nofin, Data} ->
            %lager:debug("DATA ~s~n", [Data]),
            receive_data(Pid, StreamRef, <<A/binary, Data/binary>>, Timeout);
        {gun_data, Pid, StreamRef, fin, Data} ->
            %lager:debug("Data end: ~s~n", [Data]),
            <<A/binary, Data/binary>>
    after Timeout ->
        {error, timeout}
    end.

ht() ->
    {ok, F} = file:read_file("../../erl.html"),
    case mochiweb_html:parse(F) of
        {<<"html">>, _B, R} ->
            R
        ;_-> nok1
    end.

html_p([], A, _) -> A;
html_p([{<<"head">>,HO,HB}|T], A, Url) -> html_p(T, [process_head(HO, HB, Url)] ++ A, Url);
html_p([{<<"body">>,BO,BB}|T], A, Url) -> html_p(T, A ++ [process_body(BO, BB, Url)], Url);
html_p([H|T], A, Url) -> html_p(T, A ++ [H], Url).

process_head(HO, HB, Url) -> {<<"head">>, HO, src_repl(HB, Url)}.
process_body(BO, BB, Url) -> {<<"body">>, BO, src_repl(BB, Url)}.

src_repl(B, Url) -> src_repl(B, [], Url).

src_repl([], A, _) -> A;
src_repl([{Tag, Attrs, Chldrs}|T], A, Url) -> src_repl(T, A ++ [{Tag, attr_proc(Attrs, [], Url), src_repl(Chldrs, Url)}], Url);
src_repl([H|T], A, Url) -> src_repl(T, A ++ [H], Url).

attr_proc([], A, _) -> A;
attr_proc([{AN, H}|T], A, Url) when 
    AN =:= <<"href">>;
    AN =:= <<"src">> -> attr_proc(T, A ++ [{AN, fix_dst(H, Url)}], Url);
attr_proc([H|T], A, Url) -> attr_proc(T, A ++[H], Url).

fix_dst(<<"http://", _/binary>>  = H, _) -> H;
fix_dst(<<"https://", _/binary>> = H, _) -> H;
%fix_dst(<<"//", RH/binary>>, Url) -> B = get_base(Url), <<B/binary, "/", RH/binary>>;
fix_dst(<<"//", _/binary>> = H, _) -> H;
fix_dst(<<"/",  RH/binary>>, Url) -> B = get_base(Url), <<B/binary, "/", RH/binary>>;
fix_dst(H, Url) -> B = get_base_p(Url), <<B/binary, H/binary>>.

get_base(U) when is_list(U) ->
    case http_uri:parse(U) of
        {ok, {P,_,H,_,_,_}} -> <<(to_type(P, binary))/binary, "://", (to_type(H, binary))/binary>>;
        {error, no_scheme}  -> get_base("http://" ++ U)
        ;_ -> <<"">>
    end;
get_base(U) -> get_base(to_type(U, list)).

get_base_p(U) when is_list(U) ->
    case http_uri:parse(U) of
        {ok, {P,_,H,_,Pa,_}} -> 
            <<(to_type(P, binary))/binary, "://", (to_type(H, binary))/binary, (get_path(Pa))/binary>>;
        {error, no_scheme}  -> get_base_p("http://" ++ U)
        ;_ -> <<"">>
    end;
get_base_p(U) -> get_base_p(to_type(U, list)).

get_path(P) when is_list(P)   -> get_path(to_bin(P));
get_path(P) when is_binary(P) ->
    case re:run(P, "^(.+\/)[^/]+\\.[^/]+$") of
        {match, [_,{_,L}]} -> <<Pa:L/binary,_/binary>> = P, Pa
        ;_ -> P
    end.

t() ->
    R = mochiweb_html:to_html({<<"html">>, [], mkhu:html_p(mkhu:ht(),[],"http://www.erlang.org/")}),
    file:write_file("../../erl1.html", R).
