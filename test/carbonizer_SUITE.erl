-module(carbonizer_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("carbonizer.hrl").

-define(a(Exp), ?assert(Exp)).
-define(ae(E, A), ?assertEqual(E, A)).

-compile(export_all).

all() ->
    tests().

groups() ->
    [{tests, [sequence], tests()}].

tests() ->
    [startup_test,
     send_to_carbon_test,
     send_batch_to_carbon_test,
     flush_incomplete_batch_test,
     send_count_is_ok].

init_per_testcase(CaseName, Config)
  when CaseName =:= send_to_carbon_test;
       CaseName =:= send_batch_to_carbon_test;
       CaseName =:= flush_incomplete_batch_test ->
    meck:new(gen_udp, [unstick]),
    meck:expect(gen_udp, open, fun (_, _) -> {ok, meck_fake_socket} end),
    Self = self(),
    meck:expect(gen_udp, send, fun (meck_fake_socket, _, _, Data) ->
                                       Self ! {meck_fake_udp_send, Data},
                                       ok
                               end),
    Config;
init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(CaseName, _Config)
  when CaseName =:= send_to_carbon_test;
       CaseName =:= send_batch_to_carbon_test;
       CaseName =:= flush_incomplete_batch_test ->
    meck:unload(gen_udp),
    stopdown(),
    ok;
end_per_testcase(_CaseName, _Config) ->
    stopdown(),
    ok.

%%
%% Tests
%%

startup_test(_) ->
    startup(),
    ?a(is_running()).

%% The data should be flushed to the socket once,
%% i.e. just one call to gen_udp:send/4.
send_to_carbon_test(_) ->
    startup([{flush_freq, 1}]),
    ?a(is_running()),
    send_to_carbon("foo.bar.baz", 124, os:timestamp()),
    receive {meck_fake_udp_send, _} -> ok end,
    ?ae(1, meck:num_calls(gen_udp, send, '_')).

%% The data should be flushed to the socket once per big enough batch,
%% i.e. just one call to gen_udp:send/4 per 5 samples.
send_batch_to_carbon_test(_) ->
    startup([{flush_freq, 5}]),
    ?a(is_running()),
    BaseVal = 220,
    BaseTS = os:timestamp(),
    [send_to_carbon("foo.bar.baz", BaseVal + I, timestamp_add(BaseTS, {0,I,0}))
     || I <- lists:seq(1, 7)],
    receive {meck_fake_udp_send, _} -> ok end,
    ?ae(1, meck:num_calls(gen_udp, send, '_')),
    [send_to_carbon("foo.bar.baz", BaseVal + I, timestamp_add(BaseTS, {0,I,0}))
     || I <- lists:seq(8, 10)],
    receive {meck_fake_udp_send, _} -> ok end,
    ?ae(2, meck:num_calls(gen_udp, send, '_')).

flush_incomplete_batch_test() ->
    [{timetrap, {seconds, 10}}].

%% If no samples are published for a period of time,
%% the server will flush whatever it's got in the buffer.
flush_incomplete_batch_test(_) ->
    startup([{flush_freq, 5}, {flush_time, 1}]),
    ?a(is_running()),
    send_to_carbon("foo.bar.baz", 124, os:timestamp()),
    receive {meck_fake_udp_send, _} -> ok end,
    ?ae(1, meck:num_calls(gen_udp, send, '_')).

send_count_is_ok(_) ->
    dbg:tracer(),
    dbg:tpl(udp_listener, x),
    dbg:p(all, call),
    ExpectedNDatagrams = 1113,
    {Pid, Port} = start_udp_listener(8899, ExpectedNDatagrams),
    startup({127,0,0,1}, Port, [{flush_freq, 700}, {flush_time, 1}]),
    ?a(is_running()),
    StartAt = timestamp_add(os:timestamp(), {0, -ExpectedNDatagrams, 0}),
    [send_to_carbon("foo.counted", I, timestamp_add(StartAt, {0,I,0}))
     || I <- lists:seq(1, ExpectedNDatagrams)],
    ?a(udp_listener:is_valid(Pid)),
    stop_udp_listener(Pid).


%%
%% Helpers
%%

startup() ->
    startup([]).

startup(Opts) ->
    startup({127,0,0,1}, 8899, Opts).

startup(Addr, Port, Opts) ->
    {ok, Pid} = carbonizer:start(Addr, Port, Opts),
    Pid.

stopdown() ->
    MRef = erlang:monitor(process, carbonizer),
    carbonizer:stop(),
    receive {'DOWN', MRef, process, {carbonizer, _}, _} -> ok end.

is_running() ->
    Pid = erlang:whereis(carbonizer),
    is_pid(Pid) andalso erlang:is_process_alive(Pid).

sample(Metric, Value, Timestamp) ->
    #carbon_sample{metric = Metric,
                   value = Value,
                   timestamp = Timestamp}.

timestamp_add({Mega1, Secs1, Mili1}, {Mega2, Secs2, Mili2}) ->
    {Mega1 + Mega2, Secs1 + Secs2, Mili1 + Mili2}.

send_to_carbon(Metric, Value, TS) ->
    carbonizer:send(sample(Metric, Value, TS)).

start_udp_listener(Port, ExpectedNDatagrams) ->
    Pid = udp_listener:start(Port, ExpectedNDatagrams),
    {Pid, Port}.

stop_udp_listener(Pid) ->
    Pid ! stop.
