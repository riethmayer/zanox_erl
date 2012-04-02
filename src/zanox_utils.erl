%% @copyright 2012 bonusbox GmbH
%% @doc
%%
%% See LICENSE for licensing information.
-module(zanox_utils).

%% Interface exports
-export([request/3]).
-export([timestamp/0]).

%% exports needed to mocking... I dont have any idea how makes this better
%% now
-export([time_now/0]).
-export([signature/2]).
-export([http_get_request/1]).
-export([nonce/0]).
-export([request_signature/4]).
-export([url_encode/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%% Constants
-define(ENDPOINT, "https://api.zanox.com/json/2011-03-01").

%% +-----------------------------------------------------------------+
%% | INTERFACE FUNCTIONS                                             |
%% +-----------------------------------------------------------------+

-spec request(Method :: string(), ConnectId :: string(), SecretKey :: string())
             -> term().
request(Method, ConnectId, SecretKey) ->
    NormalizedMethod = string:to_lower(Method),
    Nonce = zanox_utils:nonce(),
    Timestamp = zanox_utils:timestamp(),
    QueryParams = [
                   {"connectid", ConnectId},
                   {"currency", "EUR"},
                   {"date", Timestamp},
                   {"nonce", Nonce},
                   {"signature", zanox_utils:request_signature(
                                   SecretKey,
                                   NormalizedMethod,
                                   Timestamp,
                                   Nonce)}
                  ],
    {ok,{{"HTTP/1.1",200,"OK"}, _, Response }} =
        zanox_utils:http_get_request(
          NormalizedMethod ++ "?" ++
              zanox_utils:url_encode(QueryParams)),
    Response.

-spec timestamp() -> string().
%% returns today date in proper format
%% @hidden
timestamp() ->
    strftime:f(zanox_utils:time_now(),
               "%a, %d %b %Y %H:%M:%S GMT",
               universal).

%% +-----------------------------------------------------------------+
%% | PRIVATE FUNCTIONS                                               |
%% +-----------------------------------------------------------------+

-spec http_get_request(Query :: string()) -> tuple().
%% request alias
%% @private
http_get_request(Query) -> httpc:request(?ENDPOINT ++ Query).


-spec request_signature(SecretKey :: string(), Method :: string(),
                        Timestamp :: string(), Nonce :: string())
                       -> string().
%% prepares String2Sign for  signature
%% @private
request_signature(SecretKey, Method, Timestamp, Nonce) ->
    zanox_utils:signature(SecretKey,
                          "GET" ++ Method ++ Timestamp ++ Nonce).


-spec signature(SecretKey :: string(), String2Sign :: string())
               -> string().
%% generates signature
%% @private
signature(SecretKey, String2Sign) ->
    Context = crypto:hmac_init(sha, SecretKey),
    NewContext = crypto:hmac_update(Context, String2Sign),
    base64:encode_to_string(crypto:hmac_final(NewContext)).


-spec binary_to_hex(Binary :: binary()) -> string().
%% converts binary to hex string
%% @private
binary_to_hex(Binary) ->
    lists:flatten(
      [io_lib:format("~2.16.0b",[Byte]) || <<Byte>> <= Binary]).


-spec nonce() -> string().
%% generates random seed in proper format
%% @private
nonce() ->
    {_, _, Usec} = time_now(),
    string:left(binary_to_hex
                  (crypto:md5(
                     float_to_list(Usec + random:uniform()))), 32).


-spec url_encode(ParamsList :: [tuple()]) -> string().
%% generate url params string based on lists of params
%% @private
url_encode(ParamsList) ->
    url_encode(ParamsList, "").
url_encode([], Acc) ->
    Acc;

url_encode([{Key, Value}| R], "") ->
    url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value));
url_encode([{Key, Value}| R], Acc) ->
    url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)).


-spec time_now() -> tuple().
%% time now alias
%% @private
time_now() -> erlang:now().
