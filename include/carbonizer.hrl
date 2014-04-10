-record(carbon_sample, {metric,
                        value,
                        timestamp}).
-type carbon_sample() :: #carbon_sample{}.

-define(CARBONIZER_FLUSH_FREQ, 100).
-define(CARBONIZER_FLUSH_TIME, 5).  %% seconds

-record(carbonizer_state,
        {carbon_host    :: inet:ip_address() | inet:hostname() | undefined,
         carbon_port    :: inet:port_number() | undefined,
         udp_socket     :: inet:socket() | undefined,
         flush_freq     = ?CARBONIZER_FLUSH_FREQ :: pos_integer(),
         flush_time     = ?CARBONIZER_FLUSH_TIME :: pos_integer(),
         samples        = [] :: [carbon_sample()],
         nsamples       = 0 :: non_neg_integer()}).
-type carbonizer_state() :: #carbonizer_state{}.
