-record(pool_options, {
    name          :: name(),
    backlog_size  :: shackle_backlog:backlog_size(),
    client        :: shackle:client(),
    max_retries   :: max_retries(),
    pool_size     :: pool_size(),
    pool_strategy :: pool_strategy()
}).

%% types
-type max_retries() :: non_neg_integer().
-type name() :: atom().
-type pool_size() :: pos_integer().
-type pool_strategy() :: random | round_robin.
-type pool_options() :: #pool_options{}.
-type option() :: {backlog_size, shackle_backlog:backlog_size()} |
                  {max_retries, max_retries()} |
                  {pool_size, pool_size()} |
                  {pool_strategy, pool_strategy()}.
-type options() :: [option()].
