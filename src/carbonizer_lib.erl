-module(carbonizer_lib).

-include("carbonizer.hrl").
-define(INFO(Fmt, Args), error_logger:info_msg(Fmt, Args)).
-define(state, carbonizer_state).

-export([handle_sample/2,
         buffer_sample/2,
         maybe_send_batch/1,
         send_batch/1,
         samples_to_iodata/1,
         sample_to_iodata/1,
         timestamp_to_seconds/1,
         timeout/1]).

-spec handle_sample(carbon_sample(),
                    carbonizer_state()) -> {ok, carbonizer_state()} |
                                           {error, any()}.
handle_sample(#carbon_sample{} = Sample, #?state{} = S) ->
    BufferedS = buffer_sample(Sample, S),
    maybe_send_batch(BufferedS).

-spec buffer_sample(carbon_sample(),
                    carbonizer_state()) -> carbonizer_state().
buffer_sample(#carbon_sample{} = Sample,
              #?state{samples = Samples,
                      nsamples = NSamples} = S) ->
    S#?state{samples = [Sample | Samples],
             nsamples = NSamples + 1}.

-spec maybe_send_batch(carbonizer_state()) -> {ok, carbonizer_state()} |
                                              {error, any()}.
maybe_send_batch(#?state{flush_freq = FlushFreq,
                         nsamples = NSamples} = S) ->
    if
        NSamples == FlushFreq -> send_batch(S);
        NSamples /= FlushFreq -> {ok, S}
    end.

-spec send_batch(carbonizer_state()) -> {ok, carbonizer_state()} |
                                        {error, any()}.
send_batch(#?state{nsamples = 0} = S) ->
    {ok, S};
send_batch(#?state{carbon_host = Addr, carbon_port = Port,
                   udp_socket = Socket, samples = Samples} = S) ->
    case gen_udp:send(Socket, Addr, Port,
                      samples_to_iodata(Samples)) of
        {error, _} = Error ->
            Error;
        ok ->
            {ok, S#?state{samples = [],
                          nsamples = 0}}
    end.

samples_to_iodata(Samples) ->
    [[sample_to_iodata(Sample), $\n] || Sample <- Samples].

sample_to_iodata(#carbon_sample{metric = Metric, value = Value,
                                timestamp = TS}) ->
    BValue = integer_to_binary(Value),
    BTS = integer_to_binary(timestamp_to_seconds(TS)),
    [Metric, $\ , BValue, $\ , BTS].

timestamp_to_seconds({Mega, Seconds, _}) ->
    1000 * 1000 * Mega + Seconds.

timeout(#?state{flush_time = Time}) ->
    timer:seconds(Time).
