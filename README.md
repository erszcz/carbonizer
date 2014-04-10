# Carbonizer: push data from Erlang to Carbon/Graphite!

Quickstart (pastable into `erl`):

```erlang
rr("include/carbonizer.hrl").
% [carbon_sample,carbonizer_state]
{ok, C} = carbonizer:start({10,100,0,70}, 2003).
% {ok,<0.42.0>}
C ! #carbon_sample{metric = "foo.bar.baz",
                   value = 122,
                   timestamp = os:timestamp()}.
% #carbon_sample{metric = "foo.bar.baz",
%                value = 122,
%                timestamp = {1397,122710,486136}}
{Mega, Seconds, _} = os:timestamp().
[C ! #carbon_sample{metric = "foo.bar.baz",
                    value = erlang:round(100 + 20 * math:sin(math:pi() * I / 8)),
                    timestamp = {Mega, Seconds - 200 + I, 0}}
 || I <- lists:seq(-800,0)].
% [#carbon_sample{metric = "foo.bar.baz",value = 100,
%                 timestamp = {1397,122660,0}},
% ...
timer:sleep(5000).  %% make sure the last batch is flushed
```
