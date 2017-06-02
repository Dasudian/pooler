-module(pooler).
-export([
         start/0,
         stop/0,
         take_member/1,
         return_member/3
        ]).

start() ->
    {ok, _} = application:ensure_all_started(pooler),
    ok.

stop() ->
    ok = application:stop(pooler).

take_member(Pool) ->
    poolboy:checkout(Pool).

return_member(Pool, Member, ok) ->
    poolboy:checkin(Pool, Member);
return_member(Pool, Member, fail) ->
    poolboy:checkin(Pool, Member).
