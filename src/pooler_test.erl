-module(pooler_test).
-compile(export_all).

test_send(Msg) ->
    Worker = pooler:take_member(test),
    Res = send(Worker, Msg),
    io:format("~p~n", [Res]),
    pooler:return_member(test, Worker, ok).

test_error() ->
    Worker = pooler:take_member(test),
    ?MODULE:error(Worker),
    pooler:return_member(test, Worker, fail).

start() ->
    Pid = spawn(fun loop/0),
    {ok, Pid}.

error(Pid) ->
    Pid ! error.

send(Pid, Msg) ->
    Pid ! {self(), Msg},
    receive
        X -> X
    after
        1000 -> {error, timeout}
    end.

loop() ->
    receive
        error ->
            io:format("[~p] exit~n", [self()]),
            exit(hello_pooler);
        {From, Msg} ->
            io:format("[~p] recv: ~p~n", [self(), Msg]),
            From ! Msg,
            loop()
    end.
