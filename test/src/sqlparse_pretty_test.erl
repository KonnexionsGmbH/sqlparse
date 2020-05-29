%% -----------------------------------------------------------------------------
%%
%% sqlparse_pretty_test.erl: SQL - pretty format test driver.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-module(sqlparse_pretty_test).

-define(NODEBUG, true).

-include("sqlparse_pretty_test.hrl").

%%------------------------------------------------------------------------------
%% Testing PROBLEM with default options.
%%------------------------------------------------------------------------------

default_PROBLEM_01_test() ->
  ?D("Start ~n"),
  ?assertEqual(
    {ok, list_to_binary(?PROBLEM_01_RESULT_DEFAULT)},
    sqlparse_test_utils:eunit_test(?PROBLEM_01, [{case_identifier, init_cap}])
  ).


default_PROBLEM_02_test() ->
  ?D("Start ~n"),
  ?assertEqual(
    {ok, list_to_binary(?PROBLEM_02_RESULT_DEFAULT)},
    sqlparse_test_utils:eunit_test(?PROBLEM_02, [{case_identifier, init_cap}])
  ).


default_PROBLEM_03_test() ->
  ?D("Start ~n"),
  ?assertEqual(
    {ok, list_to_binary(?PROBLEM_03_RESULT_DEFAULT)},
    sqlparse_test_utils:eunit_test(?PROBLEM_03, [{case_identifier, init_cap}])
  ).


default_PROBLEM_04_test() ->
  ?D("Start ~n"),
  ?assertEqual(
    {ok, list_to_binary(?PROBLEM_04_RESULT_DEFAULT)},
    sqlparse_test_utils:eunit_test(?PROBLEM_04, [{case_identifier, init_cap}])
  ).


default_PROBLEM_05_test() ->
  ?D("Start ~n"),
  ?assertEqual(
    {ok, list_to_binary(?PROBLEM_05_RESULT_DEFAULT)},
    sqlparse_test_utils:eunit_test(?PROBLEM_05, [{case_identifier, init_cap}])
  ).


default_PROBLEM_06_test() ->
  ?D("Start ~n"),
  ?assertEqual(
    {ok, list_to_binary(?PROBLEM_06_RESULT_DEFAULT)},
    sqlparse_test_utils:eunit_test(?PROBLEM_06, [{case_identifier, init_cap}])
  ).

%%------------------------------------------------------------------------------
%% Testing ALTER USER with default options:
%%
%% - identifier: init_cap
%% - keyword:    upper
%% - indent:     4 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

default_ALTER_USER_test_() ->
  ?D("Start ~n"),
  LOpts = maps:put(case_identifier, init_cap, maps:new()),
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("ALTER_USER_01", ?ALTER_USER_01, ?ALTER_USER_01_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_02", ?ALTER_USER_02, ?ALTER_USER_02_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_03", ?ALTER_USER_03, ?ALTER_USER_03_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_04", ?ALTER_USER_04, ?ALTER_USER_04_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_05", ?ALTER_USER_05, ?ALTER_USER_05_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_06", ?ALTER_USER_06, ?ALTER_USER_06_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_07", ?ALTER_USER_07, ?ALTER_USER_07_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_08", ?ALTER_USER_08, ?ALTER_USER_08_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_09", ?ALTER_USER_09, ?ALTER_USER_09_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_10", ?ALTER_USER_10, ?ALTER_USER_10_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_11", ?ALTER_USER_11, ?ALTER_USER_11_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_12", ?ALTER_USER_12, ?ALTER_USER_12_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_13", ?ALTER_USER_13, ?ALTER_USER_13_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_14", ?ALTER_USER_14, ?ALTER_USER_14_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_15", ?ALTER_USER_15, ?ALTER_USER_15_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_16", ?ALTER_USER_16, ?ALTER_USER_16_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_17", ?ALTER_USER_17, ?ALTER_USER_17_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_18", ?ALTER_USER_18, ?ALTER_USER_18_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_19", ?ALTER_USER_19, ?ALTER_USER_19_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_20", ?ALTER_USER_20, ?ALTER_USER_20_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_21", ?ALTER_USER_21, ?ALTER_USER_21_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_22", ?ALTER_USER_22, ?ALTER_USER_22_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_23", ?ALTER_USER_23, ?ALTER_USER_23_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_24", ?ALTER_USER_24, ?ALTER_USER_24_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_25", ?ALTER_USER_25, ?ALTER_USER_25_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_26", ?ALTER_USER_26, ?ALTER_USER_26_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_27", ?ALTER_USER_27, ?ALTER_USER_27_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_28", ?ALTER_USER_28, ?ALTER_USER_28_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_29", ?ALTER_USER_29, ?ALTER_USER_29_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_30", ?ALTER_USER_30, ?ALTER_USER_30_RESULT_DEFAULT, LOpts)},
          {formatter("ALTER_USER_31", ?ALTER_USER_31, ?ALTER_USER_31_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing CREATE with default options:
%%------------------------------------------------------------------------------

default_CREATE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("CREATE_01", ?CREATE_01, ?CREATE_01_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_02", ?CREATE_02, ?CREATE_02_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_03", ?CREATE_03, ?CREATE_03_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_04", ?CREATE_04, ?CREATE_04_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_05", ?CREATE_05, ?CREATE_05_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_06", ?CREATE_06, ?CREATE_06_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_07", ?CREATE_07, ?CREATE_07_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_08", ?CREATE_08, ?CREATE_08_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_09", ?CREATE_09, ?CREATE_09_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_10", ?CREATE_10, ?CREATE_10_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_11", ?CREATE_11, ?CREATE_11_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_12", ?CREATE_12, ?CREATE_12_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_13", ?CREATE_13, ?CREATE_13_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_14", ?CREATE_14, ?CREATE_14_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_15", ?CREATE_15, ?CREATE_15_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_16", ?CREATE_16, ?CREATE_16_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_17", ?CREATE_17, ?CREATE_17_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_18", ?CREATE_18, ?CREATE_18_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_19", ?CREATE_19, ?CREATE_19_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_20", ?CREATE_20, ?CREATE_20_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_21", ?CREATE_21, ?CREATE_21_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_22", ?CREATE_22, ?CREATE_22_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_23", ?CREATE_23, ?CREATE_23_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_24", ?CREATE_24, ?CREATE_24_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_25", ?CREATE_25, ?CREATE_25_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_26", ?CREATE_26, ?CREATE_26_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_27", ?CREATE_27, ?CREATE_27_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_28", ?CREATE_28, ?CREATE_28_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_29", ?CREATE_29, ?CREATE_29_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_30", ?CREATE_30, ?CREATE_30_RESULT_DEFAULT, LOpts)},
          {formatter("CREATE_31", ?CREATE_31, ?CREATE_31_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing DELETE with default options:
%%------------------------------------------------------------------------------

default_DELETE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("DELETE_01", ?DELETE_01, ?DELETE_01_RESULT_DEFAULT, LOpts)},
          {formatter("DELETE_03", ?DELETE_03, ?DELETE_03_RESULT_DEFAULT, LOpts)},
          {formatter("DELETE_04", ?DELETE_04, ?DELETE_04_RESULT_DEFAULT, LOpts)},
          {formatter("DELETE_05", ?DELETE_05, ?DELETE_05_RESULT_DEFAULT, LOpts)},
          {formatter("DELETE_06", ?DELETE_06, ?DELETE_06_RESULT_DEFAULT, LOpts)},
          {formatter("DELETE_07", ?DELETE_07, ?DELETE_07_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing DROP with default options:
%%------------------------------------------------------------------------------

default_DROP_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("DROP_01", ?DROP_01, ?DROP_01_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_02", ?DROP_02, ?DROP_02_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_03", ?DROP_03, ?DROP_03_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_04", ?DROP_04, ?DROP_04_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_05", ?DROP_05, ?DROP_05_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_07", ?DROP_07, ?DROP_07_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_09", ?DROP_09, ?DROP_09_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_10", ?DROP_10, ?DROP_10_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_11", ?DROP_11, ?DROP_11_RESULT_DEFAULT, LOpts)},
          {formatter("DROP_12", ?DROP_12, ?DROP_12_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing FROM with default options.
%%------------------------------------------------------------------------------

default_FROM_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("FROM_01", ?FROM_01, ?FROM_01_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_02", ?FROM_02, ?FROM_02_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_03", ?FROM_03, ?FROM_03_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_04", ?FROM_04, ?FROM_04_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_05", ?FROM_05, ?FROM_05_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_06", ?FROM_06, ?FROM_06_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_07", ?FROM_07, ?FROM_07_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_08", ?FROM_08, ?FROM_08_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_09", ?FROM_09, ?FROM_09_RESULT_DEFAULT, LOpts)},
          {formatter("FROM_10", ?FROM_10, ?FROM_10_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing GRANT with default options:
%%------------------------------------------------------------------------------

default_GRANT_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("GRANT_01", ?GRANT_01, ?GRANT_01_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_02", ?GRANT_02, ?GRANT_02_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_03", ?GRANT_03, ?GRANT_03_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_04", ?GRANT_04, ?GRANT_04_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_05", ?GRANT_05, ?GRANT_05_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_06", ?GRANT_06, ?GRANT_06_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_07", ?GRANT_07, ?GRANT_07_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_08", ?GRANT_08, ?GRANT_08_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_09", ?GRANT_09, ?GRANT_09_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_10", ?GRANT_10, ?GRANT_10_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_11", ?GRANT_11, ?GRANT_11_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_12", ?GRANT_12, ?GRANT_12_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_13", ?GRANT_13, ?GRANT_13_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_14", ?GRANT_14, ?GRANT_14_RESULT_DEFAULT, LOpts)},
          {formatter("GRANT_15", ?GRANT_15, ?GRANT_15_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing GROUP BY with default options.
%%------------------------------------------------------------------------------

default_GROUP_BY_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("GROUP_BY_01", ?GROUP_BY_01, ?GROUP_BY_01_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_02", ?GROUP_BY_02, ?GROUP_BY_02_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_03", ?GROUP_BY_03, ?GROUP_BY_03_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_04", ?GROUP_BY_04, ?GROUP_BY_04_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_05", ?GROUP_BY_05, ?GROUP_BY_05_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_06", ?GROUP_BY_06, ?GROUP_BY_06_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_07", ?GROUP_BY_07, ?GROUP_BY_07_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_08", ?GROUP_BY_08, ?GROUP_BY_08_RESULT_DEFAULT, LOpts)},
          {formatter("GROUP_BY_09", ?GROUP_BY_09, ?GROUP_BY_09_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing HAVING with default options.
%%------------------------------------------------------------------------------

default_HAVING_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("HAVING_01", ?HAVING_01, ?HAVING_01_RESULT_DEFAULT, LOpts)},
          {formatter("HAVING_02", ?HAVING_02, ?HAVING_02_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing INSERT with default options:
%%------------------------------------------------------------------------------

default_INSERT_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("INSERT_01", ?INSERT_01, ?INSERT_01_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_02", ?INSERT_02, ?INSERT_02_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_03", ?INSERT_03, ?INSERT_03_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_04", ?INSERT_04, ?INSERT_04_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_05", ?INSERT_05, ?INSERT_05_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_06", ?INSERT_06, ?INSERT_06_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_07", ?INSERT_07, ?INSERT_07_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_08", ?INSERT_08, ?INSERT_08_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_09", ?INSERT_09, ?INSERT_09_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_10", ?INSERT_10, ?INSERT_10_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_11", ?INSERT_11, ?INSERT_11_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_12", ?INSERT_12, ?INSERT_12_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_13", ?INSERT_13, ?INSERT_13_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_14", ?INSERT_14, ?INSERT_14_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_15", ?INSERT_15, ?INSERT_15_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_16", ?INSERT_16, ?INSERT_16_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_17", ?INSERT_17, ?INSERT_17_RESULT_DEFAULT, LOpts)},
          {formatter("INSERT_18", ?INSERT_18, ?INSERT_18_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing JOIN with default options.
%%------------------------------------------------------------------------------

default_JOIN_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("JOIN_01", ?JOIN_01, ?JOIN_01_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_02", ?JOIN_02, ?JOIN_02_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_03", ?JOIN_03, ?JOIN_03_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_04", ?JOIN_04, ?JOIN_04_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_05", ?JOIN_05, ?JOIN_05_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_06", ?JOIN_06, ?JOIN_06_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_07", ?JOIN_07, ?JOIN_07_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_08", ?JOIN_08, ?JOIN_08_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_09", ?JOIN_09, ?JOIN_09_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_10", ?JOIN_10, ?JOIN_10_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_11", ?JOIN_11, ?JOIN_11_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_12", ?JOIN_12, ?JOIN_12_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_13", ?JOIN_13, ?JOIN_13_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_14", ?JOIN_14, ?JOIN_14_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_15", ?JOIN_15, ?JOIN_15_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_16", ?JOIN_16, ?JOIN_16_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_17", ?JOIN_17, ?JOIN_17_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_18", ?JOIN_18, ?JOIN_18_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_19", ?JOIN_19, ?JOIN_19_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_20", ?JOIN_20, ?JOIN_20_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_21", ?JOIN_21, ?JOIN_21_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_22", ?JOIN_22, ?JOIN_22_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_23", ?JOIN_23, ?JOIN_23_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_24", ?JOIN_24, ?JOIN_24_RESULT_DEFAULT, LOpts)},
          {formatter("JOIN_25", ?JOIN_25, ?JOIN_25_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing MISCELLANEOUS with default options.
%%------------------------------------------------------------------------------

default_MISCELLANEOUS_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {
            formatter(
              "MISCELLANEOUS_02",
              ?MISCELLANEOUS_02,
              ?MISCELLANEOUS_02_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_03",
              ?MISCELLANEOUS_03,
              ?MISCELLANEOUS_03_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_04",
              ?MISCELLANEOUS_04,
              ?MISCELLANEOUS_04_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_05",
              ?MISCELLANEOUS_05,
              ?MISCELLANEOUS_05_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_06",
              ?MISCELLANEOUS_06,
              ?MISCELLANEOUS_06_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_07",
              ?MISCELLANEOUS_07,
              ?MISCELLANEOUS_07_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_08",
              ?MISCELLANEOUS_08,
              ?MISCELLANEOUS_08_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_09",
              ?MISCELLANEOUS_09,
              ?MISCELLANEOUS_09_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_10",
              ?MISCELLANEOUS_10,
              ?MISCELLANEOUS_10_RESULT_DEFAULT,
              LOpts
            )
          },
          {
            formatter(
              "MISCELLANEOUS_11",
              ?MISCELLANEOUS_11,
              ?MISCELLANEOUS_11_RESULT_DEFAULT,
              LOpts
            )
          }
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing MULTIPLE with default options.
%%------------------------------------------------------------------------------

default_MULTIPLE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("MULTIPLE_01", ?MULTIPLE_01, ?MULTIPLE_01_RESULT_DEFAULT, LOpts)},
          {formatter("MULTIPLE_02", ?MULTIPLE_02, ?MULTIPLE_02_RESULT_DEFAULT, LOpts)},
          {formatter("MULTIPLE_03", ?MULTIPLE_03, ?MULTIPLE_03_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing ORDER BY with default options.
%%------------------------------------------------------------------------------

default_ORDER_BY_1_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("ORDER_BY_01", ?ORDER_BY_01, ?ORDER_BY_01_RESULT_DEFAULT, LOpts)},
          {formatter("ORDER_BY_02", ?ORDER_BY_02, ?ORDER_BY_02_RESULT_DEFAULT, LOpts)},
          {formatter("ORDER_BY_03", ?ORDER_BY_03, ?ORDER_BY_03_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.


default_ORDER_BY_2_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}, {line_break_after, 0}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("ORDER_BY_01", ?ORDER_BY_01, ?ORDER_BY_01_RESULT_DEFAULT, LOpts)},
          {formatter("ORDER_BY_02", ?ORDER_BY_02, ?ORDER_BY_02_RESULT_DEFAULT, LOpts)},
          {formatter("ORDER_BY_03", ?ORDER_BY_03, ?ORDER_BY_03_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing PARENTHESES with default options.
%%------------------------------------------------------------------------------

default_PARENTHESES_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("PARENTHESES_01", ?PARENTHESES_01, ?PARENTHESES_01_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_02", ?PARENTHESES_02, ?PARENTHESES_02_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_03", ?PARENTHESES_03, ?PARENTHESES_03_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_04", ?PARENTHESES_04, ?PARENTHESES_04_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_05", ?PARENTHESES_05, ?PARENTHESES_05_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_06", ?PARENTHESES_06, ?PARENTHESES_06_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_07", ?PARENTHESES_07, ?PARENTHESES_07_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_11", ?PARENTHESES_11, ?PARENTHESES_11_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_12", ?PARENTHESES_12, ?PARENTHESES_12_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_13", ?PARENTHESES_13, ?PARENTHESES_13_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_14", ?PARENTHESES_14, ?PARENTHESES_14_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_21", ?PARENTHESES_21, ?PARENTHESES_21_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_22", ?PARENTHESES_22, ?PARENTHESES_22_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_23", ?PARENTHESES_23, ?PARENTHESES_23_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_24", ?PARENTHESES_24, ?PARENTHESES_24_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_25", ?PARENTHESES_25, ?PARENTHESES_25_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_26", ?PARENTHESES_26, ?PARENTHESES_26_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_27", ?PARENTHESES_27, ?PARENTHESES_27_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_28", ?PARENTHESES_28, ?PARENTHESES_28_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_29", ?PARENTHESES_29, ?PARENTHESES_29_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_30", ?PARENTHESES_30, ?PARENTHESES_30_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_31", ?PARENTHESES_31, ?PARENTHESES_31_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_32", ?PARENTHESES_32, ?PARENTHESES_32_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_40", ?PARENTHESES_40, ?PARENTHESES_40_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_41", ?PARENTHESES_41, ?PARENTHESES_41_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_42", ?PARENTHESES_42, ?PARENTHESES_42_RESULT_DEFAULT, LOpts)},
          {formatter("PARENTHESES_43", ?PARENTHESES_43, ?PARENTHESES_43_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing PLSQL with default options.
%%------------------------------------------------------------------------------

default_PLSQL_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("PLSQL_03", ?PLSQL_03, ?PLSQL_03_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_05", ?PLSQL_05, ?PLSQL_05_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_07", ?PLSQL_07, ?PLSQL_07_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_08", ?PLSQL_08, ?PLSQL_08_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_09", ?PLSQL_09, ?PLSQL_09_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_10", ?PLSQL_10, ?PLSQL_10_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_11", ?PLSQL_11, ?PLSQL_11_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_12", ?PLSQL_12, ?PLSQL_12_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_14", ?PLSQL_14, ?PLSQL_14_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_15", ?PLSQL_15, ?PLSQL_15_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_16", ?PLSQL_16, ?PLSQL_16_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_17", ?PLSQL_17, ?PLSQL_17_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_18", ?PLSQL_18, ?PLSQL_18_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_19", ?PLSQL_19, ?PLSQL_19_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_20", ?PLSQL_20, ?PLSQL_20_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_22", ?PLSQL_22, ?PLSQL_22_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_24", ?PLSQL_24, ?PLSQL_24_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_26", ?PLSQL_26, ?PLSQL_26_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_27", ?PLSQL_27, ?PLSQL_27_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_28", ?PLSQL_28, ?PLSQL_28_RESULT_DEFAULT, LOpts)},
          {formatter("PLSQL_29", ?PLSQL_29, ?PLSQL_29_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing REVOKE with default options:
%%------------------------------------------------------------------------------

default_REVOKE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("REVOKE_01", ?REVOKE_01, ?REVOKE_01_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_02", ?REVOKE_02, ?REVOKE_02_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_03", ?REVOKE_03, ?REVOKE_03_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_04", ?REVOKE_04, ?REVOKE_04_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_05", ?REVOKE_05, ?REVOKE_05_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_06", ?REVOKE_06, ?REVOKE_06_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_07", ?REVOKE_07, ?REVOKE_07_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_08", ?REVOKE_08, ?REVOKE_08_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_09", ?REVOKE_09, ?REVOKE_09_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_10", ?REVOKE_10, ?REVOKE_10_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_11", ?REVOKE_11, ?REVOKE_11_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_12", ?REVOKE_12, ?REVOKE_12_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_13", ?REVOKE_13, ?REVOKE_13_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_14", ?REVOKE_14, ?REVOKE_14_RESULT_DEFAULT, LOpts)},
          {formatter("REVOKE_15", ?REVOKE_15, ?REVOKE_15_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing ROLE with default options:
%%------------------------------------------------------------------------------

default_ROLE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("ROLE_01", ?ROLE_01, ?ROLE_01_RESULT_DEFAULT, LOpts)},
          {formatter("ROLE_02", ?ROLE_02, ?ROLE_02_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing SELECT with default options.
%%------------------------------------------------------------------------------

default_SELECT_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("SELECT_01", ?SELECT_01, ?SELECT_01_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_02", ?SELECT_02, ?SELECT_02_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_03", ?SELECT_03, ?SELECT_03_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_04", ?SELECT_04, ?SELECT_04_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_05", ?SELECT_05, ?SELECT_05_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_06", ?SELECT_06, ?SELECT_06_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_07", ?SELECT_07, ?SELECT_07_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_08", ?SELECT_08, ?SELECT_08_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_09", ?SELECT_09, ?SELECT_09_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_10", ?SELECT_10, ?SELECT_10_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_11", ?SELECT_11, ?SELECT_11_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_12", ?SELECT_12, ?SELECT_12_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_13", ?SELECT_13, ?SELECT_13_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_14", ?SELECT_14, ?SELECT_14_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_15", ?SELECT_15, ?SELECT_15_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_16", ?SELECT_16, ?SELECT_16_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_17", ?SELECT_17, ?SELECT_17_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_18", ?SELECT_18, ?SELECT_18_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_19", ?SELECT_19, ?SELECT_19_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_20", ?SELECT_20, ?SELECT_20_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_21", ?SELECT_21, ?SELECT_21_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_22", ?SELECT_22, ?SELECT_22_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_23", ?SELECT_23, ?SELECT_23_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_24", ?SELECT_24, ?SELECT_24_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_25", ?SELECT_25, ?SELECT_25_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_26", ?SELECT_26, ?SELECT_26_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_27", ?SELECT_27, ?SELECT_27_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_28", ?SELECT_28, ?SELECT_28_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_29", ?SELECT_29, ?SELECT_29_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_30", ?SELECT_30, ?SELECT_30_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_31", ?SELECT_31, ?SELECT_31_RESULT_DEFAULT, LOpts)},
          {formatter("SELECT_32", ?SELECT_32, ?SELECT_32_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing STRUCTURE with default options.
%%------------------------------------------------------------------------------

default_STRUCTURE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("STRUCTURE_01", ?STRUCTURE_01, ?STRUCTURE_01_RESULT_DEFAULT, LOpts)},
          {formatter("STRUCTURE_02", ?STRUCTURE_02, ?STRUCTURE_02_RESULT_DEFAULT, LOpts)},
          {formatter("STRUCTURE_03", ?STRUCTURE_03, ?STRUCTURE_03_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing TRANSACTION with default options.
%%------------------------------------------------------------------------------

default_TRANSACTION_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("TRANSACTION_01", ?TRANSACTION_01, ?TRANSACTION_01_RESULT_DEFAULT, LOpts)},
          {formatter("TRANSACTION_02", ?TRANSACTION_02, ?TRANSACTION_02_RESULT_DEFAULT, LOpts)},
          {formatter("TRANSACTION_03", ?TRANSACTION_03, ?TRANSACTION_03_RESULT_DEFAULT, LOpts)},
          {formatter("TRANSACTION_04", ?TRANSACTION_04, ?TRANSACTION_04_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing TRUNCATE with default options.
%%------------------------------------------------------------------------------

default_TRUNCATE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("TRUNCATE_01", ?TRUNCATE_01, ?TRUNCATE_01_RESULT_DEFAULT, LOpts)},
          {formatter("TRUNCATE_02", ?TRUNCATE_02, ?TRUNCATE_02_RESULT_DEFAULT, LOpts)},
          {formatter("TRUNCATE_03", ?TRUNCATE_03, ?TRUNCATE_03_RESULT_DEFAULT, LOpts)},
          {formatter("TRUNCATE_04", ?TRUNCATE_04, ?TRUNCATE_04_RESULT_DEFAULT, LOpts)},
          {formatter("TRUNCATE_05", ?TRUNCATE_05, ?TRUNCATE_05_RESULT_DEFAULT, LOpts)},
          {formatter("TRUNCATE_06", ?TRUNCATE_06, ?TRUNCATE_06_RESULT_DEFAULT, LOpts)},
          {formatter("TRUNCATE_07", ?TRUNCATE_07, ?TRUNCATE_07_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing UNBREAKABLE with default options.
%%------------------------------------------------------------------------------

default_UNBREAKABLE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("UNBREAKABLE_01", ?UNBREAKABLE_01, ?UNBREAKABLE_01_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_02", ?UNBREAKABLE_02, ?UNBREAKABLE_02_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_03", ?UNBREAKABLE_03, ?UNBREAKABLE_03_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_04", ?UNBREAKABLE_04, ?UNBREAKABLE_04_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_05", ?UNBREAKABLE_05, ?UNBREAKABLE_05_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_06", ?UNBREAKABLE_06, ?UNBREAKABLE_06_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_07", ?UNBREAKABLE_07, ?UNBREAKABLE_07_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_08", ?UNBREAKABLE_08, ?UNBREAKABLE_08_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_09", ?UNBREAKABLE_09, ?UNBREAKABLE_09_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_10", ?UNBREAKABLE_10, ?UNBREAKABLE_10_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_11", ?UNBREAKABLE_11, ?UNBREAKABLE_11_RESULT_DEFAULT, LOpts)},
          {formatter("UNBREAKABLE_12", ?UNBREAKABLE_12, ?UNBREAKABLE_12_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing UNION with default options.
%%------------------------------------------------------------------------------

default_UNION_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("UNION_01", ?UNION_01, ?UNION_01_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_02", ?UNION_02, ?UNION_02_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_03", ?UNION_03, ?UNION_03_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_04", ?UNION_04, ?UNION_04_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_05", ?UNION_05, ?UNION_05_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_06", ?UNION_06, ?UNION_06_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_07", ?UNION_07, ?UNION_07_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_08", ?UNION_08, ?UNION_08_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_09", ?UNION_09, ?UNION_09_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_10", ?UNION_10, ?UNION_10_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_11", ?UNION_11, ?UNION_11_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_12", ?UNION_12, ?UNION_12_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_13", ?UNION_13, ?UNION_13_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_14", ?UNION_14, ?UNION_14_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_15", ?UNION_15, ?UNION_15_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_16", ?UNION_16, ?UNION_16_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_17", ?UNION_17, ?UNION_17_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_18", ?UNION_18, ?UNION_18_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_19", ?UNION_19, ?UNION_19_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_20", ?UNION_20, ?UNION_20_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_21", ?UNION_21, ?UNION_21_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_22", ?UNION_22, ?UNION_22_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_23", ?UNION_23, ?UNION_23_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_24", ?UNION_24, ?UNION_24_RESULT_DEFAULT, LOpts)},
          % wwe
          %%                {formatter("UNION_25", ?UNION_25, ?UNION_25_RESULT_DEFAULT,
          %%                    LOpts)},
          {formatter("UNION_26", ?UNION_26, ?UNION_26_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_27", ?UNION_27, ?UNION_27_RESULT_DEFAULT, LOpts)},
          % wwe
          %%                {formatter("UNION_28", ?UNION_28, ?UNION_28_RESULT_DEFAULT,
          %%                    LOpts)},
          {formatter("UNION_29", ?UNION_29, ?UNION_29_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_30", ?UNION_30, ?UNION_30_RESULT_DEFAULT, LOpts)},
          % wwe
          %%                {formatter("UNION_31", ?UNION_31, ?UNION_31_RESULT_DEFAULT,
          %%                    LOpts)},
          {formatter("UNION_32", ?UNION_32, ?UNION_32_RESULT_DEFAULT, LOpts)},
          {formatter("UNION_33", ?UNION_33, ?UNION_33_RESULT_DEFAULT, LOpts)},
          % wwe
          %%                {formatter("UNION_34", ?UNION_34, ?UNION_34_RESULT_DEFAULT,
          %%                    LOpts)},
          {formatter("UNION_35", ?UNION_35, ?UNION_35_RESULT_DEFAULT, LOpts)}
          % wwe
          %%                {formatter("UNION_36", ?UNION_36, ?UNION_36_RESULT_DEFAULT,
          %%                    LOpts)},
          %%                {formatter("UNION_37", ?UNION_37, ?UNION_37_RESULT_DEFAULT,
          %%                    LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing UPDATE with default options.
%%------------------------------------------------------------------------------

default_UPDATE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("UPDATE_01", ?UPDATE_01, ?UPDATE_01_RESULT_DEFAULT, LOpts)},
          {formatter("UPDATE_02", ?UPDATE_02, ?UPDATE_02_RESULT_DEFAULT, LOpts)},
          {formatter("UPDATE_03", ?UPDATE_03, ?UPDATE_03_RESULT_DEFAULT, LOpts)},
          {formatter("UPDATE_04", ?UPDATE_04, ?UPDATE_04_RESULT_DEFAULT, LOpts)},
          {formatter("UPDATE_05", ?UPDATE_05, ?UPDATE_05_RESULT_DEFAULT, LOpts)},
          {formatter("UPDATE_06", ?UPDATE_06, ?UPDATE_06_RESULT_DEFAULT, LOpts)},
          {formatter("UPDATE_07", ?UPDATE_07, ?UPDATE_07_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing VIEW with default options.
%%------------------------------------------------------------------------------

default_VIEW_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("VIEW_01", ?VIEW_01, ?VIEW_01_RESULT_DEFAULT, LOpts)},
          {formatter("VIEW_02", ?VIEW_02, ?VIEW_02_RESULT_DEFAULT, LOpts)},
          {formatter("VIEW_03", ?VIEW_03, ?VIEW_03_RESULT_DEFAULT, LOpts)},
          {formatter("VIEW_04", ?VIEW_04, ?VIEW_04_RESULT_DEFAULT, LOpts)},
          {formatter("VIEW_05", ?VIEW_05, ?VIEW_05_RESULT_DEFAULT, LOpts)},
          {formatter("VIEW_06", ?VIEW_06, ?VIEW_06_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Testing WHERE with default options.
%%------------------------------------------------------------------------------

default_WHERE_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("WHERE_01", ?WHERE_01, ?WHERE_01_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_02", ?WHERE_02, ?WHERE_02_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_03", ?WHERE_03, ?WHERE_03_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_04", ?WHERE_04, ?WHERE_04_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_05", ?WHERE_05, ?WHERE_05_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_06", ?WHERE_06, ?WHERE_06_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_07", ?WHERE_07, ?WHERE_07_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_08", ?WHERE_08, ?WHERE_08_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_09", ?WHERE_09, ?WHERE_09_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_10", ?WHERE_10, ?WHERE_10_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_11", ?WHERE_11, ?WHERE_11_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_12", ?WHERE_12, ?WHERE_12_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_13", ?WHERE_13, ?WHERE_13_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_14", ?WHERE_14, ?WHERE_14_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_15", ?WHERE_15, ?WHERE_15_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_16", ?WHERE_16, ?WHERE_16_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_17", ?WHERE_17, ?WHERE_17_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_18", ?WHERE_18, ?WHERE_18_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_19", ?WHERE_19, ?WHERE_19_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_20", ?WHERE_20, ?WHERE_20_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_21", ?WHERE_21, ?WHERE_21_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_22", ?WHERE_22, ?WHERE_22_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_23", ?WHERE_23, ?WHERE_23_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_24", ?WHERE_24, ?WHERE_24_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_25", ?WHERE_25, ?WHERE_25_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_26", ?WHERE_26, ?WHERE_26_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_27", ?WHERE_27, ?WHERE_27_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_28", ?WHERE_28, ?WHERE_28_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_29", ?WHERE_29, ?WHERE_29_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_30", ?WHERE_30, ?WHERE_30_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_31", ?WHERE_31, ?WHERE_31_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_32", ?WHERE_32, ?WHERE_32_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_33", ?WHERE_33, ?WHERE_33_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_34", ?WHERE_34, ?WHERE_34_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_35", ?WHERE_35, ?WHERE_35_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_36", ?WHERE_36, ?WHERE_36_RESULT_DEFAULT, LOpts)},
          {formatter("WHERE_37", ?WHERE_37, ?WHERE_37_RESULT_DEFAULT, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: keep_unchanged
%% - keyword:    init_cap
%% - indent:     4 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_K_I_4_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, init_cap}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_K_I_4_S_T", ?OPTION_01, ?OPTION_01_RESULT_K_I_4_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: keep_unchanged
%% - keyword:    lower
%% - indent:     4 spaces
%% - whitespace: false
%%------------------------------------------------------------------------------

option_K_L_4_S_F_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {ws_operator, false}],
  {
    setup,
    fun setup_default/0,
    fun
      () ->
        [
          {formatter("OPTION_01_K_L_4_S_F", ?OPTION_01, ?OPTION_01_RESULT_K_L_4_S_F, LOpts)},
          {formatter("OPTION_02_K_L_4_S_F", ?OPTION_02, ?OPTION_02_RESULT_K_L_4_S_F, LOpts)}
        ]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: lower
%% - keyword:    upper
%% - indent:     4 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_L_U_4_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_identifier, lower}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_L_U_4_S_T", ?OPTION_01, ?OPTION_01_RESULT_L_U_4_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     1 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_1_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_space, 1}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_1_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_1_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     2 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_2_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_space, 2}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_2_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_2_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     3 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_3_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_space, 3}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_3_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_3_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     4 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_4_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_4_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_4_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     5 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_5_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_space, 5}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_5_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_5_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     6 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_6_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_space, 6}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_6_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_6_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     7 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_7_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_space, 7}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_7_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_7_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     8 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L_8_S_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_space, 8}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L_8_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_8_S_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests with the following options:
%%
%% - identifier: upper
%% - keyword:    lower
%% - indent:     tab
%% - whitespace: true
%%------------------------------------------------------------------------------

option_U_L___T_T_test_() ->
  ?D("Start ~n"),
  LOpts = [{case_keyword, lower}, {case_identifier, upper}, {indent_with, tab}],
  {
    setup,
    fun setup_default/0,
    fun
      () -> [{formatter("OPTION_01_U_L___T_T", ?OPTION_01, ?OPTION_01_RESULT_U_L___T_T, LOpts)}]
    end
  }.

%%------------------------------------------------------------------------------
%% Tests invalid_parameter_value.
%%------------------------------------------------------------------------------

parameter_invalid_test() ->
  ?D("Start ~n"),
  ?assertThrow(
    invalid_parameter_value,
    sqlparse_test_utils:eunit_test("select * from dual", [{case_identifier, wwe}])
  ),
  ?assertThrow(
    invalid_parameter_value,
    sqlparse_test_utils:eunit_test("select * from dual", [{case_keyword, wwe}])
  ),
  ?assertThrow(
    invalid_parameter_value,
    sqlparse_test_utils:eunit_test("select * from dual", [{indent_space, wwe}])
  ),
  ?assertThrow(
    invalid_parameter_value,
    sqlparse_test_utils:eunit_test("select * from dual", [{indent_with, wwe}])
  ),
  ?assertThrow(
    invalid_parameter_value,
    sqlparse_test_utils:eunit_test("select * from dual", [{line_break_after, wwe}])
  ),
  ?assertThrow(
    invalid_parameter_value,
    sqlparse_test_utils:eunit_test("select * from dual", [{ws_operator, wwe}])
  ),
  ?assertThrow(
    invalid_parameter,
    sqlparse_test_utils:eunit_test("select * from dual", [{wwe, wwe}])
  ).

%%------------------------------------------------------------------------------
%% Tests valid parameter values.
%%------------------------------------------------------------------------------

parameter_ok_test() ->
  ?D("Start ~n"),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    Column_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    Dual;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{case_identifier, init_cap}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    columN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test(
      "selecT columN_1 froM duaL;",
      [{case_identifier, keep_unchanged}]
    )
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    column_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    dual;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{case_identifier, lower}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    COLUMN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    DUAL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{case_identifier, upper}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("Select" ++ "\n" ++ "    columN_1" ++ "\n" ++ "From" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{case_keyword, init_cap}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("select" ++ "\n" ++ "    columN_1" ++ "\n" ++ "from" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{case_keyword, lower}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    columN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{case_keyword, upper}])
  ),
  ?assertEqual(
    {ok, list_to_binary("SELECT" ++ "\n" ++ "  columN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "  duaL;")},
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{indent_space, 2}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    columN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{indent_with, space}])
  ),
  ?assertEqual(
    {ok, list_to_binary("SELECT" ++ "\n" ++ "\tcolumN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "\tduaL;")},
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{indent_with, tab}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    columN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{line_break_after, 80}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    columN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{ws_operator, false}])
  ),
  ?assertEqual(
    {
      ok,
      list_to_binary("SELECT" ++ "\n" ++ "    columN_1" ++ "\n" ++ "FROM" ++ "\n" ++ "    duaL;")
    },
    sqlparse_test_utils:eunit_test("selecT columN_1 froM duaL;", [{ws_operator, true}])
  ).

%%------------------------------------------------------------------------------
%% Helper functions.
%%------------------------------------------------------------------------------

formatter(Title, Source, Result, LOpts) ->
  ?D("Start ~n Title: ~p~n Source: ~p~n Result: ~p~n LOpts: ~p~n", [Title, Source, Result, LOpts]),
  case sqlparse_test_utils:eunit_test(Source, LOpts) of
    {ok, Source_Format} -> ?assertEqual(Result, binary_to_list(Source_Format));

    ErrorResult ->
      io:format(
        user,
        "~n" ++ ?MODULE_STRING ++ " : Error in eunit_test : Title      ~n > ~p~n",
        [Title]
      ),
      io:format(
        user,
        "~n" ++ ?MODULE_STRING ++ " : Error in eunit_test : ErrorResult~n > ~p~n",
        [ErrorResult]
      )
  end.

%%------------------------------------------------------------------------------
%% Setup functions.
%%------------------------------------------------------------------------------

setup_default() ->
  ?D("Start ~n"),
  ok.
