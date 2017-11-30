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

-spec take_member(Pool :: atom()) -> {ok, {CallBack :: fun((Res :: atom()) -> any()), pid()}} | {error, disconnect}.
take_member(Pool) ->
    Pid = poolboy:checkout(Pool),
    case pooler_worker:get_conn(Pid) of
        {ok, MemPid} ->
            CallBack = fun(Res) ->
                poolboy:checkin(Pool, Pid),
                case Res of
                    ok -> ok;
                    fail -> pooler_worker:fail_conn(Pid)
                end end,
            {ok, {CallBack, MemPid}};
        {error, _R} -> take_member_error(Pool, Pid)
    end.

take_member_error(Pool, Pid) ->
    poolboy:checkin(Pool, Pid),
    {error, disconnect}.


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
