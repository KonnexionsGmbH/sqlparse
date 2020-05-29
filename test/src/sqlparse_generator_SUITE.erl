%% -----------------------------------------------------------------------------
%%
%% sqlparse_generator_SUITE.erl: SQL - test data generator.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-module(sqlparse_generator_SUITE).

-export([all/0, end_per_suite/1, init_per_suite/1, suite/0, test_generate/1]).

-include_lib("common_test/include/ct.hrl").

%%------------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%------------------------------------------------------------------------------

suite() -> [{timetrap, {minutes, 30}}].

init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

%%------------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%------------------------------------------------------------------------------

all() -> [test_generate].

%%------------------------------------------------------------------------------
%% TEST CASES
%%------------------------------------------------------------------------------

test_generate(_Config) ->
  %%    ok = sqlparse_generator:generate(),
  ok.
