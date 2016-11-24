%% @doc

-module(server_a).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/1,
         ensure_started_and_add1/2,
         ensure_started_and_add2/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Launch a new gen_server for Id if not exists.
start_link(Id) ->
    ServerName = {local, Id},
    gen_server:start_link(ServerName, ?MODULE, [], []).

%% @doc Add a number to the state and return result.
ensure_started_and_add1(Id, A) ->
    try
        {ok, _Pid} = ensure_started(Id),

        %% _Pid might be down here.

        gen_server:call(Id, {add, A})
    catch
        Class:Reason ->
            {error, {catched, {Class, Reason}}}
    end.

%% @doc Add a number to the state and return result.
ensure_started_and_add2(Id, A) ->
    Fun = fun() ->
                  ensure_started_and_add1(Id, A)
          end,
    %% Call Fun. Retry if gen_server:call fail.
    call_with_retry(Fun, fun need_retry/1, 5).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
init([]) ->
    Ms = 1, %% This process will die soon!
    _ = timer:apply_after(Ms, gen_server, cast, [self(), die]),
    {ok, 0}.

handle_call({add, A}, _From, State) ->
    State1 = A+State,
    {reply, {ok, State1}, State1};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(die, State) ->
    {stop, shutdown, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% Ensure gen_server for Id is started (probably)
ensure_started(Id) ->
    case server_a_sup:start_child(Id) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        Err -> Err
    end.

%% Judge whether the return value is the one of gen_server:call errors.
need_retry({error, {catched, {exit, {shutdown, _}}}}) -> true;
need_retry({error, {catched, {exit, {noproc, _}}}}) -> true;
need_retry(_) -> false.

%% Call Fun until some condition fulfilled.
call_with_retry(Fun, NeedRetryFun, N) ->
    Ret = Fun(),
    case NeedRetryFun(Ret) of
        false ->
            Ret;
        true ->
            case N of
                0 ->
                    _ = io:format("Give up...~n"),
                    Ret;
                _ ->
                    %% _ = io:format("Retry... ~p~n", [N-1]),
                    call_with_retry(Fun, NeedRetryFun, N-1)
            end
    end.

