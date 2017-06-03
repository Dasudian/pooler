# pooler
A erlang pooler based on poolerboy.

## Config
Add some config like this in your `app.config`
```
{pooler, [
          {pools, [
                   [{name, test},
                    {group, test},
                    {max_count, 10},
                    {init_count, 5},
                    {start_mfa,
                      {pooler_test, start, []}}]
                ]}
          ]}.
```
## Usage
```erlang
pooler:start().

Worker = pooler:take_member(Pool).

do_something(Worker, X).

pooler:return_member(Pool, Member, ok) %% if no exception
pooler:return_member(Pool, Member, fail) %% if occur an exception
```
