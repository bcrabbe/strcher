-module(strcher_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         all/0]).

all() -> [].

init_per_suite(Config) ->
    application:start(strcher),
    Config.

end_per_suite(_Config) ->
    application:stop(strcher),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.
