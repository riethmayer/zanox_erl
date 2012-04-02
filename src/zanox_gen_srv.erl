%% @copyright 2012 bonusbox GmbH
%% @doc
%%
%% See LICENSE for licensing information.
-module(zanox_gen_srv).
-behaviour(gen_server).

%% Interface exports
-export([start_link/0]).
-export([set_credentials/2]).
-export([request/1]).

%% Callbacks exports
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Constants
-define(SERVER, ?MODULE).


-record(state, {
          connect_id = "" :: string(),
          secret_key = "" :: string()
         }).

%% +-----------------------------------------------------------------+
%% | INTERFACE FUNCTIONS                                             |
%% +-----------------------------------------------------------------+

-spec start_link() -> ok.
start_link() ->
    random:seed(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec set_credentials(ConnectId :: string(), SecretKey :: string()) -> term().
set_credentials(ConnectId, SecretKey) ->
    gen_server:call(?SERVER,
                    {set_credentials, [
                                       {connect_id, ConnectId},
                                       {secret_key, SecretKey}]}).

-spec request(Method :: string()) -> term().
request(Method) ->
    {ok, Response} = gen_server:call(?SERVER, {request, Method}),
    Response.
%% +-----------------------------------------------------------------+
%% | GEN_SERVER CALLBACKS                                            |
%% +-----------------------------------------------------------------+

-spec init([]) -> {ok, state}.
%% @hidden
init([]) ->
    {ok, #state{}}.

-spec handle_call(_Request, _From, State :: state)
                 -> {reply, ok, State :: state}.
%% @hidden
handle_call({request, Method}, _From, State) ->
    ConnectId = State#state.connect_id,
    SecretKey = State#state.secret_key,
    Response = zanox_utils:request(Method, ConnectId, SecretKey),
    {reply, {ok, Response}, State};
handle_call({set_credentials, Options}, _From, State) ->
    ConnectId = proplists:get_value(connect_id, Options),
    SecretKey = proplists:get_value(secret_key, Options),
    NewState = State#state{connect_id=ConnectId, secret_key=SecretKey},
    {reply, {ok, Options}, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(_Msg, State :: state)
                 -> {noreply, State :: state}.
%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_Info, State :: state)
                 -> {noreply, State :: state}.
%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_Reason, _State) -> ok.
%% @hidden
terminate(_Reason, _State) ->
    ok.

-spec code_change(_OldVsn, State :: state, _Extra) -> ok.
%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% +-----------------------------------------------------------------+
%% | PRIVATE FUNCTIONS                                               |
%% +-----------------------------------------------------------------+

%% @private
