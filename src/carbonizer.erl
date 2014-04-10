-module(carbonizer).

-behaviour(gen_server).

-include("carbonizer.hrl").

-define(INFO(Fmt, Args), error_logger:info_msg(Fmt, Args)).

%% Named process API
-export([start/0, start/2, start/3,
         stop/0,
         send/1]).

%% Anonymous process API
-export([start_link/2, start_link/3,
         send/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(carbonizer_lib, [handle_sample/2,
                         send_batch/1,
                         timeout/1]).

-define(SERVER, ?MODULE).
-define(state, carbonizer_state).

%%
%% Standalone process API
%%

start() ->
    Host = application:get_env(carbon_host, ?MODULE, "localhost"),
    Port = application:get_env(carbon_port, ?MODULE, 2003),
    start(Host, Port).

start(Host, Port) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Host, Port], []).

start(Host, Port, Opts) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Host, Port | Opts], []).

stop() ->
    gen_server:cast(?SERVER, stop).

send(#carbon_sample{} = Sample) ->
    gen_server:cast(?SERVER, Sample).

%%
%% Anonymous process API
%%

start_link(Host, UDPPort) ->
    gen_server:start_link(?MODULE, [Host, UDPPort], []).

start_link(Host, UDPPort, Opts) ->
    gen_server:start_link(?MODULE, [Host, UDPPort | Opts], []).

send(Pid, #carbon_sample{} = Sample) ->
    gen_server:cast(Pid, Sample).

%%
%% gen_server callbacks
%%

init([Host, UDPPort | Opts]) ->
    case gen_udp:open(0, []) of
        {ok, Socket} ->
            {ok,
             #?state{carbon_host = Host,
                     carbon_port = UDPPort,
                     udp_socket = Socket,
                     flush_freq = proplists:get_value(flush_freq, Opts,
                                                      ?CARBONIZER_FLUSH_FREQ),
                     flush_time = proplists:get_value(flush_time, Opts,
                                                      ?CARBONIZER_FLUSH_TIME)}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(Request, From, #?state{} = S) ->
    ?INFO("unexpected call from ~p: ~p~n", [From, Request]),
    Reply = ok,
    {reply, Reply, S, timeout(S)}.

handle_cast(#carbon_sample{} = Sample, #?state{} = S) ->
    case handle_sample(Sample, S) of
        {ok, NewS} -> {noreply, NewS, timeout(S)};
        {error, _} = ER -> {stop, ER, S}
    end;
handle_cast(stop, #?state{} = S) ->
    {stop, normal, S};
handle_cast(Msg, #?state{} = S) ->
    ?INFO("unexpected cast: ~p~n", [Msg]),
    {noreply, S, timeout(S)}.

handle_info(#carbon_sample{} = Sample, #?state{} = S) ->
    case handle_sample(Sample, S) of
        {ok, NewS} -> {noreply, NewS, timeout(S)};
        {error, _} = ER -> {stop, ER, S}
    end;
handle_info(timeout, #?state{} = S) ->
    case send_batch(S) of
        {ok, NewS} -> {noreply, NewS, timeout(S)};
        {error, _} = ER -> {stop, ER, S}
    end;
handle_info(Info, #?state{} = S) ->
    ?INFO("unexpected info: ~p~n", [Info]),
    {noreply, S, timeout(S)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
