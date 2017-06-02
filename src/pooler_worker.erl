-module(pooler_worker).
-behaviour(poolboy_worker).

-export([start_link/1]).

start_link(Args) ->
    {M, F, A} = proplists:get_value(start_mfa, Args),
    {ok, Pid} = erlang:apply(M, F, A),
    {ok, Pid}.
