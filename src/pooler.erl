-module(pooler).
-export([
         start/0,
         stop/0,
         take_member/1,
         take_member/2,
         return_member/3,
         transaction/2,
         transaction/3
        ]).

-define(TIMEOUT, 5000).

start() ->
    {ok, _} = application:ensure_all_started(pooler),
    ok.

stop() ->
    ok = application:stop(pooler).

take_member(Pool) ->
    poolboy:checkout(Pool).

take_member(Pool, Timeout) ->
    poolboy:checkout(Pool, true, Timeout).

return_member(Pool, Member, ok) ->
    poolboy:checkin(Pool, Member);
return_member(Pool, Member, fail) ->
    poolboy:checkin(Pool, Member).

transaction(Pool, Fun) ->
    poolboy:transaction(Pool, Fun, ?TIMEOUT).

transaction(Pool, Fun, Timeout) ->
    poolboy:transaction(Pool, Fun, Timeout).
