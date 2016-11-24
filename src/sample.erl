%% @doc
%% ```
%% %% Sample output
%% > sample:a().
%% Finished. OKs 209090 Errs 498
%% Finished. OKs 196872 Errs 0
%% ok
%% '''


-module(sample).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         a/0
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
a() ->
    ok = run(foo, ensure_started_and_add1, 0, 0, 0, 0),
    ok = run(bar, ensure_started_and_add2, 0, 0, 0, 0).

run(Name, Fun, I, Last, OKs, Errs) ->
    _ = case {OKs, Errs} of
        {0, 0} ->
            _ = erlang:send_after(1000, self(), owari);
        _ ->
            ok
    end,
    receive
        owari ->
            _ = io:format("Finished. OKs ~p Errs ~p~n", [OKs, Errs]),
            ok
    after
        0 ->
            case server_a:Fun(Name, I) of
                {ok, _V} ->
                    %% _ = io:format("Value: ~p~n", [{I, Last, _V}]),
                    true = lists:member(_V, [I, Last+I]), %% Added or reset -> added
                    run(Name, Fun, I+1, _V, OKs+1, Errs);
                {error, _Reason} ->
                    case _Reason of
                        {catched, {exit, {noproc, _}}} ->
                            ok;
                        {catched, {exit, {shutdown, _}}} ->
                            ok;
                        _ ->
                            _ = io:format("error ~p~n", [_Reason])
                    end,
                    run(Name, Fun, I+1, Last, OKs, Errs+1)
            end
    end.
