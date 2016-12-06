%% @doc
%% ```
%% %% Sample output
%% > sample:a().
%% bad_sample finished. missing calls: 731 (242159/242890)
%% good_sample finished. missing calls: 0 (252840/252840)
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
    table = ets:new(table, [named_table, public]),

    ok = run(bad_sample, ensure_started_and_doit_badly, 0),
    ok = run(good_sample, ensure_started_and_doit_goodly, 0),
    _ = ets:delete(table),
    ok.

run(Name, Fun, I) ->
    _ = case I of
            0 ->
                _ = ets:delete(table, key),
                _ = ets:insert_new(table, {key, 0}),
                _ = erlang:send_after(1000, self(), owari);
            _ ->
                ok
        end,
    receive
        owari ->
            %% Print summary
            All = I,
            [{key, Actual}] = ets:lookup(table, key),
            _ = io:format("~p finished. missing calls: ~p (~p/~p)~n", [Name, All-Actual, Actual, All]),
            ok
    after
        0 ->
            ok = server_a:Fun(Name),
            run(Name, Fun, I+1)
    end.
