%% @copyright 2012 bonusbox GmbH
%% @doc
%%
%% See LICENSE for licensing information.
-module(zanox_utils_tests).

%% Must be declared before include eunit.hrl (?LET conflict)
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MODNAME, zanox_utils).

%% makes compiler shut up.
-spec test() -> ok.

%% +-----------------------------------------------------------------+
%% | TESTS                                                           |
%% +-----------------------------------------------------------------+

timestamp_test() ->
    meck:new(?MODNAME, [passthrough]),
    meck:expect(?MODNAME, time_now, fun() -> {1331,804305,694973} end),
    ?assertEqual("Thu, 15 Mar 2012 09:38:25 GMT", ?MODNAME:timestamp()),
    meck:unload(?MODNAME).


binary_to_hex_test() ->
    ?assertEqual("7bc64e70", ?MODNAME:binary_to_hex(<<123,454,334,112>>)).


nonce_test() ->
    Nonce = ?MODNAME:nonce(),
    ?assertEqual(32, length(Nonce)),
    ?assertMatch({match, _}, re:run(Nonce,"[0-9A-Fa-f]+")).


request_signature_test() ->
    SecretKey = "verysecretkey",
    Method = "/reports/sales/data/2011-04-01",
    Timestamp = ?MODNAME:timestamp(),
    Nonce = ?MODNAME:nonce(),

    meck:new(?MODNAME, [passthrough]),
    meck:expect(?MODNAME, signature, fun(_, _) -> "abcd" end),

    Result = ?MODNAME:request_signature(SecretKey, Method, Timestamp, Nonce),

    ?assert(meck:called(?MODNAME, signature,
                        [SecretKey, "GET" ++ Method ++ Timestamp ++ Nonce])),
    ?assertEqual("abcd", Result),
    meck:unload(?MODNAME).


signature_test() ->
    ?assertEqual("HA2CNFVg8tJ53Sg2k9jvv88MJr8=", ?MODNAME:signature("abcd", "string to sign")).


request_test() ->
    Method    = "/reports/sales/data/2011-03-15",
    ConnectId = "12345",
    SecretKey = "verysecretkey",
    Timestamp = ?MODNAME:timestamp(),
    Nonce     = ?MODNAME:nonce(),
    Signature = "abcd",

    meck:new(?MODNAME, [passthrough]),
    meck:expect(?MODNAME, timestamp, fun() -> Timestamp end),
    meck:expect(?MODNAME, nonce    , fun() -> Nonce     end),
    meck:expect(?MODNAME, request_signature,
                fun(LSecretKey, LMethod, LTimestamp, LNonce) when
                          LSecretKey == SecretKey,
                          LMethod == Method,
                          LTimestamp == Timestamp,
                          LNonce == Nonce ->
                        Signature
                end),
    meck:expect(?MODNAME, url_encode, fun(_) -> "encoded=params&param=value" end),
    meck:expect(?MODNAME, http_get_request,
                fun("/reports/sales/data/2011-03-15?encoded=params&param=value") ->
                        {ok,{{"HTTP/1.1",200,"OK"}, [], request_ok }} end),
    ?assertEqual(request_ok, ?MODNAME:request(Method, ConnectId, SecretKey)),
    meck:unload(?MODNAME).
