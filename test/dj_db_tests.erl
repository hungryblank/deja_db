-module(dj_db_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DB, ?MODULE).

main_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [fun test_key_lifecycle/0
     ]}.

test_key_lifecycle() ->
    Key = <<"foo">>,
    ?assertEqual(not_found, gen_server:call(?TEST_DB, {get, Key})),
    ?assertEqual(ok, gen_server:call(?TEST_DB, {put, Key, <<"bar">>})),
    ?assertEqual({ok, <<"bar">>}, gen_server:call(?TEST_DB, {get, Key})),
    ?assertEqual(ok, gen_server:call(?TEST_DB, {delete, Key})),
    ?assertEqual(not_found, gen_server:call(?TEST_DB, {get, Key})).

setup() ->
    {ok, _Pid} = dj_db:start_link(?TEST_DB).

cleanup({ok, Pid}) ->
    DbDir = filename:join(["/tmp", ?TEST_DB]),
    os:cmd(lists:flatten("rm -rf ", DbDir)),
    gen_server:call(Pid, stop).
