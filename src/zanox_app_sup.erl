%% @copyright 2012 bonusbox GmbH
%% @doc
%%
%% See LICENSE for licensing information.
-module(zanox_app_sup).
-behaviour(application).
-behaviour(supervisor).

%% Interface exports - SUPERVISOR
-export([start_link/0]).
-export([get_sales/1]).
-export([get_program_applications/1]).
-export([set_credentials/2]).
-export([get_products/2]).

%% Callbacks exports - APPLICATION
-export([start/2]).
-export([stop/1]).

%% Callbacks exports - SUPERVISOR
-export([init/1]).

%% Constants
-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% +-----------------------------------------------------------------+
%% | INTERFACE FUNCTIONS                                             |
%% +-----------------------------------------------------------------+

-spec start_link() -> supervisor:startchild_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec get_sales(Date :: string()) -> term().
%% get sales for specific date ex."2012-03-26"
get_sales(Date) ->
    ejson:decode(zanox_gen_srv:request("/reports/sales/date/" ++ Date)).

get_program_applications(Page) ->
    ejson:decode(zanox_gen_srv:request("/programapplications",
                          [{"page", integer_to_list(Page)},
                           {"status", "confirmed"}])).

get_products(ProgramId, Page) ->
    ejson:decode(zanox_gen_srv:request("/products",
                                       [{"page", integer_to_list(Page)}])).

-spec set_credentials(ConnectId :: string(), SecretKey :: string())
                     -> {ok, Options :: list()}.
set_credentials(ConnectId, SecretKey) ->
    zanox_gen_srv:set_credentials(ConnectId, SecretKey).
%% +-----------------------------------------------------------------+
%% | APPLICATION CALLBACKS                                           |
%% +-----------------------------------------------------------------+

-spec start(_StartType, _StartArgs) -> supervisor:startchild_ret().
%% @hidden
start(_StartType, _StartArgs) ->
    zanox_app_sup:start_link().

%% @hidden
-spec stop(_State :: term()) -> term().
stop(_State) ->
    ok.


%% +-----------------------------------------------------------------+
%% | SUPERVISOR CALLBACKS                                            |
%% +-----------------------------------------------------------------+

%% @hidden
-spec init(Args :: term())
          -> {ok, { {RestartStrategy :: atom(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                    Childs :: list()} }.
init([]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(zanox_gen_srv, worker)]} }.


%% +-----------------------------------------------------------------+
%% | PRIVATE FUNCTIONS                                               |
%% +-----------------------------------------------------------------+

%% @private
