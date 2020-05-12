%% -----------------------------------------------------------------------------
%%
%% sqlparse_test.hrl: SQL - test data generator.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(SQLPARSE_TEST_HRL).

-define(SQLPARSE_TEST_HRL, true).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("sqlparse.hrl").

-define(ENV_VAR_FILE_TYPE, ".tst").
-define(ENV_VAR_FILE_WILDCARD, "SOURCEFILES").
-define(ENV_VAR_LOGGING_LEVEL, "LOG").
-define(PARSER_MODULE, sqlparse).
-define(TIMEOUT, 60).
-endif.
