-module(pooler_worker).

-behaviour(poolboy_worker).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([state/1, get_conn/1]).

-define(ReconnectTimer, 10 * 1000).
-record(state, {start_mfa, conn, status, reconnect}).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).


state(Pid) ->
    gen_server:call(Pid, state).

get_conn(Pid) ->
    gen_server:call(Pid, conn).


init([Args]) ->
    process_flag(trap_exit, true),
    lager:info("init  : ~p~n", [Args]),
    {M, F, A} = proplists:get_value(start_mfa, Args),
    case try_connect(M, F, A) of
        {ok, Pid} ->
            true = link(Pid),
            {ok, #state{start_mfa = {M, F, A}, conn = Pid, status = connected}};
        {error, R} ->
            lager:info("connect error  : ~p ~n", [R]),
            {ok, TRef} = timer:send_interval(?ReconnectTimer, timer),
            {ok, #state{start_mfa = {M, F, A}, status = disconnect, reconnect = TRef}}
    end.


handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(conn, _From, #state{conn = Conn, status = connected} = State) ->
    {reply, {ok, Conn}, State};

handle_call(conn, _From, #state{status = disconnect} = State) ->
    {reply, {error, disconnect}, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, _Reason}, #state{reconnect = undefined} = State) ->
    lager:info("pid ~p is down. Reason : ~p state:~p ~n ", [Pid, _Reason, State]),
    unlink(Pid),
    {ok, TRef} = timer:send_interval(?ReconnectTimer, timer),
    {noreply, State#state{status = disconnect, reconnect = TRef}};

handle_info({'EXIT', Pid, _Reason}, State) ->
    lager:info("pid ~p is down. Reason : ~p ~n ", [Pid, _Reason]),
    {noreply, State};

handle_info(timer, #state{status = connected} = State) ->
    {noreply, State};


handle_info(timer, #state{start_mfa = {M, F, A}, status = disconnect, reconnect = Ref} = State) ->
    lager:info("try to connect : ~p ~n", [State]),
    case try_connect(M, F, A) of
        {ok, Pid} ->
            true = link(Pid),
            timer:cancel(Ref),
            {noreply, #state{start_mfa = {M, F, A}, conn = Pid, status = connected}};
        {error, R} ->
            lager:info("reconnect error : ~p ~n", [R]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    lager:info("get unkown info : ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("terminate : ~p ~n", [{_Reason}]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

try_connect(M, F, A) ->
    try erlang:apply(M, F, A)
    catch
        _:Reason ->
            lager:info("connect error Reason : ~p ~n", [Reason]),
            {error, Reason}
    end.
