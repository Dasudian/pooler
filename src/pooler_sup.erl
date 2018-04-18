%%%-------------------------------------------------------------------
%% @doc pooler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pooler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, App} = application:get_application(),
    Pools = application:get_env(App, pools, []),
    %% PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
    %%                               PoolArgs = [{name, {local, Name}},
    %%                                           {worker_module, riakc_worker}] ++ SizeArgs,
    %%                               poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    %%                       end, Pools),
    PoolSpecs = lists:map(fun(PoolConf) ->
                                  Name = proplists:get_value(name,       PoolConf),
                                  Init = proplists:get_value(init_count, PoolConf),
                                  Max  = proplists:get_value(max_count,  PoolConf),
                                  MFA  = proplists:get_value(start_mfa,  PoolConf),
                                  PoolArgs = [{name, {local, Name}},
                                              {worker_module, 'pooler_worker'},
                                              {size, Init},
                                              {max_overflow, Max}],
                                  WorkerArgs = [{start_mfa, MFA}],
                                  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    io:format("PoolSpecs : ~p", [PoolSpecs]),
    {ok, { {one_for_one, 0, 1}, PoolSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
