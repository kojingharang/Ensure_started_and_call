%% @doc

-module(server_a).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/1,
         ensure_started_and_doit_badly/1,
         ensure_started_and_doit_goodly/1
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

%% @doc Do it badly.
-spec ensure_started_and_doit_badly(atom()) -> ok.
ensure_started_and_doit_badly(Id) ->
    {ok, _Pid} = ensure_started(Id),

    %% !! _Pid might be down here !!

    gen_server:cast(Id, doit).

%% @doc Do it goodly.
-spec ensure_started_and_doit_goodly(atom()) -> ok.
ensure_started_and_doit_goodly(Id) ->
    Fun =
        fun() ->
                try
                    {ok, _Pid} = ensure_started(Id),

                    %% !! _Pid might be down here !!

                    gen_server:call(Id, doit)
                catch
                    Class:Reason ->
                        {error, {catched, {Class, Reason}}}
                end
        end,
    %% Call Fun. Retry if gen_server:call fail.
    call_with_retry(Fun, fun need_retry/1, 5).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
init([]) ->
    {ok, 0}.

handle_call(doit, _From, State) ->
    _ = doit(),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(doit, State) ->
    _ = doit(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(die, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

doit() ->
    Ms = 1, %% This process will die soon!
    _ = erlang:send_after(Ms, self(), die),
    _ = ets:update_counter(table, key, 1),
    ok.


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
                    _ = io:format("Giving up...~n"),
                    Ret;
                _ ->
                    call_with_retry(Fun, NeedRetryFun, N-1)
            end
    end.

