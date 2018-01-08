%% -----------------------------------------------------------------------------
%%
%% formatter_test.erl: SQL - formatter test driver.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-module(formatter_test).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-include("formatter_test.hrl").

%%------------------------------------------------------------------------------
%% Testing ALTER USER with default options:
%%
%% - identifier: init_cap
%% - keyword:    upper
%% - indent:     4 spaces
%% - whitespace: true
%%------------------------------------------------------------------------------

default_ALTER_USER_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("ALTER_USER_01", ?ALTER_USER_01, ?ALTER_USER_01_RESULT_DEFAULT)},
                {formatter("ALTER_USER_02", ?ALTER_USER_02, ?ALTER_USER_02_RESULT_DEFAULT)},
                {formatter("ALTER_USER_03", ?ALTER_USER_03, ?ALTER_USER_03_RESULT_DEFAULT)},
                {formatter("ALTER_USER_04", ?ALTER_USER_04, ?ALTER_USER_04_RESULT_DEFAULT)},
                {formatter("ALTER_USER_05", ?ALTER_USER_05, ?ALTER_USER_05_RESULT_DEFAULT)},
                {formatter("ALTER_USER_06", ?ALTER_USER_06, ?ALTER_USER_06_RESULT_DEFAULT)},
                {formatter("ALTER_USER_07", ?ALTER_USER_07, ?ALTER_USER_07_RESULT_DEFAULT)},
                {formatter("ALTER_USER_08", ?ALTER_USER_08, ?ALTER_USER_08_RESULT_DEFAULT)},
                {formatter("ALTER_USER_09", ?ALTER_USER_09, ?ALTER_USER_09_RESULT_DEFAULT)},
                {formatter("ALTER_USER_10", ?ALTER_USER_10, ?ALTER_USER_10_RESULT_DEFAULT)},
                {formatter("ALTER_USER_11", ?ALTER_USER_11, ?ALTER_USER_11_RESULT_DEFAULT)},
                {formatter("ALTER_USER_12", ?ALTER_USER_12, ?ALTER_USER_12_RESULT_DEFAULT)},
                {formatter("ALTER_USER_13", ?ALTER_USER_13, ?ALTER_USER_13_RESULT_DEFAULT)},
                {formatter("ALTER_USER_14", ?ALTER_USER_14, ?ALTER_USER_14_RESULT_DEFAULT)},
                {formatter("ALTER_USER_15", ?ALTER_USER_15, ?ALTER_USER_15_RESULT_DEFAULT)},
                {formatter("ALTER_USER_16", ?ALTER_USER_16, ?ALTER_USER_16_RESULT_DEFAULT)},
                {formatter("ALTER_USER_17", ?ALTER_USER_17, ?ALTER_USER_17_RESULT_DEFAULT)},
                {formatter("ALTER_USER_18", ?ALTER_USER_18, ?ALTER_USER_18_RESULT_DEFAULT)},
                {formatter("ALTER_USER_19", ?ALTER_USER_19, ?ALTER_USER_19_RESULT_DEFAULT)},
                {formatter("ALTER_USER_20", ?ALTER_USER_20, ?ALTER_USER_20_RESULT_DEFAULT)},
                {formatter("ALTER_USER_21", ?ALTER_USER_21, ?ALTER_USER_21_RESULT_DEFAULT)},
                {formatter("ALTER_USER_22", ?ALTER_USER_22, ?ALTER_USER_22_RESULT_DEFAULT)},
                {formatter("ALTER_USER_23", ?ALTER_USER_23, ?ALTER_USER_23_RESULT_DEFAULT)},
                {formatter("ALTER_USER_24", ?ALTER_USER_24, ?ALTER_USER_24_RESULT_DEFAULT)},
                {formatter("ALTER_USER_25", ?ALTER_USER_25, ?ALTER_USER_25_RESULT_DEFAULT)},
                {formatter("ALTER_USER_26", ?ALTER_USER_26, ?ALTER_USER_26_RESULT_DEFAULT)},
                {formatter("ALTER_USER_27", ?ALTER_USER_27, ?ALTER_USER_27_RESULT_DEFAULT)},
                {formatter("ALTER_USER_28", ?ALTER_USER_28, ?ALTER_USER_28_RESULT_DEFAULT)},
                {formatter("ALTER_USER_29", ?ALTER_USER_29, ?ALTER_USER_29_RESULT_DEFAULT)},
                {formatter("ALTER_USER_30", ?ALTER_USER_30, ?ALTER_USER_30_RESULT_DEFAULT)},
                {formatter("ALTER_USER_31", ?ALTER_USER_31, ?ALTER_USER_31_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing CREATE with default options:
%%------------------------------------------------------------------------------

default_CREATE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("CREATE_01", ?CREATE_01, ?CREATE_01_RESULT_DEFAULT)},
                {formatter("CREATE_02", ?CREATE_02, ?CREATE_02_RESULT_DEFAULT)},
                {formatter("CREATE_03", ?CREATE_03, ?CREATE_03_RESULT_DEFAULT)},
                {formatter("CREATE_04", ?CREATE_04, ?CREATE_04_RESULT_DEFAULT)},
                {formatter("CREATE_05", ?CREATE_05, ?CREATE_05_RESULT_DEFAULT)},
                {formatter("CREATE_06", ?CREATE_06, ?CREATE_06_RESULT_DEFAULT)},
                {formatter("CREATE_07", ?CREATE_07, ?CREATE_07_RESULT_DEFAULT)},
                {formatter("CREATE_08", ?CREATE_08, ?CREATE_08_RESULT_DEFAULT)},
                {formatter("CREATE_09", ?CREATE_09, ?CREATE_09_RESULT_DEFAULT)},
                {formatter("CREATE_10", ?CREATE_10, ?CREATE_10_RESULT_DEFAULT)},
                {formatter("CREATE_11", ?CREATE_11, ?CREATE_11_RESULT_DEFAULT)},
                {formatter("CREATE_12", ?CREATE_12, ?CREATE_12_RESULT_DEFAULT)},
                {formatter("CREATE_13", ?CREATE_13, ?CREATE_13_RESULT_DEFAULT)},
                {formatter("CREATE_14", ?CREATE_14, ?CREATE_14_RESULT_DEFAULT)},
                {formatter("CREATE_15", ?CREATE_15, ?CREATE_15_RESULT_DEFAULT)},
                {formatter("CREATE_16", ?CREATE_16, ?CREATE_16_RESULT_DEFAULT)},
                {formatter("CREATE_17", ?CREATE_17, ?CREATE_17_RESULT_DEFAULT)},
                {formatter("CREATE_18", ?CREATE_18, ?CREATE_18_RESULT_DEFAULT)},
                {formatter("CREATE_19", ?CREATE_19, ?CREATE_19_RESULT_DEFAULT)},
                {formatter("CREATE_20", ?CREATE_20, ?CREATE_20_RESULT_DEFAULT)},
                {formatter("CREATE_21", ?CREATE_21, ?CREATE_21_RESULT_DEFAULT)},
                {formatter("CREATE_22", ?CREATE_22, ?CREATE_22_RESULT_DEFAULT)},
                {formatter("CREATE_23", ?CREATE_23, ?CREATE_23_RESULT_DEFAULT)},
                {formatter("CREATE_24", ?CREATE_24, ?CREATE_24_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing DELETE with default options:
%%------------------------------------------------------------------------------

default_DELETE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("DELETE_01", ?DELETE_01, ?DELETE_01_RESULT_DEFAULT)},
                {formatter("DELETE_03", ?DELETE_03, ?DELETE_03_RESULT_DEFAULT)},
                {formatter("DELETE_04", ?DELETE_04, ?DELETE_04_RESULT_DEFAULT)},
                {formatter("DELETE_05", ?DELETE_05, ?DELETE_05_RESULT_DEFAULT)},
                {formatter("DELETE_06", ?DELETE_06, ?DELETE_06_RESULT_DEFAULT)},
                {formatter("DELETE_07", ?DELETE_07, ?DELETE_07_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing DROP with default options:
%%------------------------------------------------------------------------------

default_DROP_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("DROP_01", ?DROP_01, ?DROP_01_RESULT_DEFAULT)},
                {formatter("DROP_02", ?DROP_02, ?DROP_02_RESULT_DEFAULT)},
                {formatter("DROP_03", ?DROP_03, ?DROP_03_RESULT_DEFAULT)},
                {formatter("DROP_04", ?DROP_04, ?DROP_04_RESULT_DEFAULT)},
                {formatter("DROP_05", ?DROP_05, ?DROP_05_RESULT_DEFAULT)},
                {formatter("DROP_06", ?DROP_06, ?DROP_06_RESULT_DEFAULT)},
                {formatter("DROP_07", ?DROP_07, ?DROP_07_RESULT_DEFAULT)},
                {formatter("DROP_08", ?DROP_08, ?DROP_08_RESULT_DEFAULT)},
                {formatter("DROP_09", ?DROP_09, ?DROP_09_RESULT_DEFAULT)},
                {formatter("DROP_10", ?DROP_10, ?DROP_10_RESULT_DEFAULT)},
                {formatter("DROP_11", ?DROP_11, ?DROP_11_RESULT_DEFAULT)},
                {formatter("DROP_12", ?DROP_12, ?DROP_12_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing FROM with default options.
%%------------------------------------------------------------------------------

default_FROM_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("FROM_01", ?FROM_01, ?FROM_01_RESULT_DEFAULT)},
                {formatter("FROM_02", ?FROM_02, ?FROM_02_RESULT_DEFAULT)},
                {formatter("FROM_03", ?FROM_03, ?FROM_03_RESULT_DEFAULT)},
                {formatter("FROM_04", ?FROM_04, ?FROM_04_RESULT_DEFAULT)},
                {formatter("FROM_05", ?FROM_05, ?FROM_05_RESULT_DEFAULT)},
                {formatter("FROM_06", ?FROM_06, ?FROM_06_RESULT_DEFAULT)},
                {formatter("FROM_07", ?FROM_07, ?FROM_07_RESULT_DEFAULT)},
                {formatter("FROM_08", ?FROM_08, ?FROM_08_RESULT_DEFAULT)},
                {formatter("FROM_09", ?FROM_09, ?FROM_09_RESULT_DEFAULT)},
                {formatter("FROM_10", ?FROM_10, ?FROM_10_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing GRANT with default options:
%%------------------------------------------------------------------------------

default_GRANT_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("GRANT_01", ?GRANT_01, ?GRANT_01_RESULT_DEFAULT)},
                {formatter("GRANT_02", ?GRANT_02, ?GRANT_02_RESULT_DEFAULT)},
                {formatter("GRANT_03", ?GRANT_03, ?GRANT_03_RESULT_DEFAULT)},
                {formatter("GRANT_04", ?GRANT_04, ?GRANT_04_RESULT_DEFAULT)},
                {formatter("GRANT_05", ?GRANT_05, ?GRANT_05_RESULT_DEFAULT)},
                {formatter("GRANT_06", ?GRANT_06, ?GRANT_06_RESULT_DEFAULT)},
                {formatter("GRANT_07", ?GRANT_07, ?GRANT_07_RESULT_DEFAULT)},
                {formatter("GRANT_08", ?GRANT_08, ?GRANT_08_RESULT_DEFAULT)},
                {formatter("GRANT_09", ?GRANT_09, ?GRANT_09_RESULT_DEFAULT)},
                {formatter("GRANT_10", ?GRANT_10, ?GRANT_10_RESULT_DEFAULT)},
                {formatter("GRANT_11", ?GRANT_11, ?GRANT_11_RESULT_DEFAULT)},
                {formatter("GRANT_12", ?GRANT_12, ?GRANT_12_RESULT_DEFAULT)},
                {formatter("GRANT_13", ?GRANT_13, ?GRANT_13_RESULT_DEFAULT)},
                {formatter("GRANT_14", ?GRANT_14, ?GRANT_14_RESULT_DEFAULT)},
                {formatter("GRANT_15", ?GRANT_15, ?GRANT_15_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing GROUP BY with default options.
%%------------------------------------------------------------------------------

default_GROUP_BY_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("GROUP_BY_01", ?GROUP_BY_01, ?GROUP_BY_01_RESULT_DEFAULT)},
                {formatter("GROUP_BY_02", ?GROUP_BY_02, ?GROUP_BY_02_RESULT_DEFAULT)},
                {formatter("GROUP_BY_03", ?GROUP_BY_03, ?GROUP_BY_03_RESULT_DEFAULT)},
                {formatter("GROUP_BY_04", ?GROUP_BY_04, ?GROUP_BY_04_RESULT_DEFAULT)},
                {formatter("GROUP_BY_05", ?GROUP_BY_05, ?GROUP_BY_05_RESULT_DEFAULT)},
                {formatter("GROUP_BY_06", ?GROUP_BY_06, ?GROUP_BY_06_RESULT_DEFAULT)},
                {formatter("GROUP_BY_07", ?GROUP_BY_07, ?GROUP_BY_07_RESULT_DEFAULT)},
                {formatter("GROUP_BY_08", ?GROUP_BY_08, ?GROUP_BY_08_RESULT_DEFAULT)},
                {formatter("GROUP_BY_09", ?GROUP_BY_09, ?GROUP_BY_09_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing HAVING with default options.
%%------------------------------------------------------------------------------

default_HAVING_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("HAVING_01", ?HAVING_01, ?HAVING_01_RESULT_DEFAULT)},
                {formatter("HAVING_02", ?HAVING_02, ?HAVING_02_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing INSERT with default options:
%%------------------------------------------------------------------------------

default_INSERT_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("INSERT_01", ?INSERT_01, ?INSERT_01_RESULT_DEFAULT)},
                {formatter("INSERT_02", ?INSERT_02, ?INSERT_02_RESULT_DEFAULT)},
                {formatter("INSERT_03", ?INSERT_03, ?INSERT_03_RESULT_DEFAULT)},
                {formatter("INSERT_04", ?INSERT_04, ?INSERT_04_RESULT_DEFAULT)},
                {formatter("INSERT_05", ?INSERT_05, ?INSERT_05_RESULT_DEFAULT)},
                {formatter("INSERT_06", ?INSERT_06, ?INSERT_06_RESULT_DEFAULT)},
                {formatter("INSERT_07", ?INSERT_07, ?INSERT_07_RESULT_DEFAULT)},
                {formatter("INSERT_08", ?INSERT_08, ?INSERT_08_RESULT_DEFAULT)},
                {formatter("INSERT_09", ?INSERT_09, ?INSERT_09_RESULT_DEFAULT)},
                {formatter("INSERT_10", ?INSERT_10, ?INSERT_10_RESULT_DEFAULT)},
                {formatter("INSERT_11", ?INSERT_11, ?INSERT_11_RESULT_DEFAULT)},
                {formatter("INSERT_12", ?INSERT_12, ?INSERT_12_RESULT_DEFAULT)},
                {formatter("INSERT_13", ?INSERT_13, ?INSERT_13_RESULT_DEFAULT)},
                {formatter("INSERT_14", ?INSERT_14, ?INSERT_14_RESULT_DEFAULT)},
                {formatter("INSERT_15", ?INSERT_15, ?INSERT_15_RESULT_DEFAULT)},
                {formatter("INSERT_16", ?INSERT_16, ?INSERT_16_RESULT_DEFAULT)},
                {formatter("INSERT_17", ?INSERT_17, ?INSERT_17_RESULT_DEFAULT)},
                {formatter("INSERT_18", ?INSERT_18, ?INSERT_18_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing JOIN with default options.
%%------------------------------------------------------------------------------

default_JOIN_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("JOIN_01", ?JOIN_01, ?JOIN_01_RESULT_DEFAULT)},
                {formatter("JOIN_02", ?JOIN_02, ?JOIN_02_RESULT_DEFAULT)},
                {formatter("JOIN_03", ?JOIN_03, ?JOIN_03_RESULT_DEFAULT)},
                {formatter("JOIN_04", ?JOIN_04, ?JOIN_04_RESULT_DEFAULT)},
                {formatter("JOIN_05", ?JOIN_05, ?JOIN_05_RESULT_DEFAULT)},
                {formatter("JOIN_06", ?JOIN_06, ?JOIN_06_RESULT_DEFAULT)},
                {formatter("JOIN_07", ?JOIN_07, ?JOIN_07_RESULT_DEFAULT)},
                {formatter("JOIN_08", ?JOIN_08, ?JOIN_08_RESULT_DEFAULT)},
                {formatter("JOIN_09", ?JOIN_09, ?JOIN_09_RESULT_DEFAULT)},
                {formatter("JOIN_10", ?JOIN_10, ?JOIN_10_RESULT_DEFAULT)},
                {formatter("JOIN_11", ?JOIN_11, ?JOIN_11_RESULT_DEFAULT)},
                {formatter("JOIN_12", ?JOIN_12, ?JOIN_12_RESULT_DEFAULT)},
                {formatter("JOIN_13", ?JOIN_13, ?JOIN_13_RESULT_DEFAULT)},
                {formatter("JOIN_14", ?JOIN_14, ?JOIN_14_RESULT_DEFAULT)},
                {formatter("JOIN_15", ?JOIN_15, ?JOIN_15_RESULT_DEFAULT)},
                {formatter("JOIN_16", ?JOIN_16, ?JOIN_16_RESULT_DEFAULT)},
                {formatter("JOIN_17", ?JOIN_17, ?JOIN_17_RESULT_DEFAULT)},
                {formatter("JOIN_18", ?JOIN_18, ?JOIN_18_RESULT_DEFAULT)},
                {formatter("JOIN_19", ?JOIN_19, ?JOIN_19_RESULT_DEFAULT)},
                {formatter("JOIN_20", ?JOIN_20, ?JOIN_20_RESULT_DEFAULT)},
                {formatter("JOIN_21", ?JOIN_21, ?JOIN_21_RESULT_DEFAULT)},
                {formatter("JOIN_22", ?JOIN_22, ?JOIN_22_RESULT_DEFAULT)},
                {formatter("JOIN_23", ?JOIN_23, ?JOIN_23_RESULT_DEFAULT)},
                {formatter("JOIN_24", ?JOIN_24, ?JOIN_24_RESULT_DEFAULT)},
                {formatter("JOIN_25", ?JOIN_25, ?JOIN_25_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing MISCELLANEOUS with default options.
%%------------------------------------------------------------------------------

default_MISCELLANEOUS_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("MISCELLANEOUS_02", ?MISCELLANEOUS_02, ?MISCELLANEOUS_02_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_03", ?MISCELLANEOUS_03, ?MISCELLANEOUS_03_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_04", ?MISCELLANEOUS_04, ?MISCELLANEOUS_04_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_05", ?MISCELLANEOUS_05, ?MISCELLANEOUS_05_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_06", ?MISCELLANEOUS_06, ?MISCELLANEOUS_06_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_07", ?MISCELLANEOUS_07, ?MISCELLANEOUS_07_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_08", ?MISCELLANEOUS_08, ?MISCELLANEOUS_08_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_09", ?MISCELLANEOUS_09, ?MISCELLANEOUS_09_RESULT_DEFAULT)},
                {formatter("MISCELLANEOUS_10", ?MISCELLANEOUS_10, ?MISCELLANEOUS_10_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing MULTIPLE with default options.
%%------------------------------------------------------------------------------

default_MULTIPLE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("MULTIPLE_01", ?MULTIPLE_01, ?MULTIPLE_01_RESULT_DEFAULT)},
                {formatter("MULTIPLE_02", ?MULTIPLE_02, ?MULTIPLE_02_RESULT_DEFAULT)},
                {formatter("MULTIPLE_03", ?MULTIPLE_03, ?MULTIPLE_03_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing ORDER BY with default options.
%%------------------------------------------------------------------------------

default_ORDER_BY_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("ORDER_BY_01", ?ORDER_BY_01, ?ORDER_BY_01_RESULT_DEFAULT)},
                {formatter("ORDER_BY_02", ?ORDER_BY_02, ?ORDER_BY_02_RESULT_DEFAULT)},
                {formatter("ORDER_BY_03", ?ORDER_BY_03, ?ORDER_BY_03_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing PLSQL with default options.
%%------------------------------------------------------------------------------

default_PLSQL_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("PLSQL_01", ?PLSQL_01, ?PLSQL_01_RESULT_DEFAULT)},
                {formatter("PLSQL_02", ?PLSQL_02, ?PLSQL_02_RESULT_DEFAULT)},
                {formatter("PLSQL_03", ?PLSQL_03, ?PLSQL_03_RESULT_DEFAULT)},
                {formatter("PLSQL_04", ?PLSQL_04, ?PLSQL_04_RESULT_DEFAULT)},
                {formatter("PLSQL_05", ?PLSQL_05, ?PLSQL_05_RESULT_DEFAULT)},
                {formatter("PLSQL_06", ?PLSQL_06, ?PLSQL_06_RESULT_DEFAULT)},
                {formatter("PLSQL_07", ?PLSQL_07, ?PLSQL_07_RESULT_DEFAULT)},
                {formatter("PLSQL_08", ?PLSQL_08, ?PLSQL_08_RESULT_DEFAULT)},
                {formatter("PLSQL_09", ?PLSQL_09, ?PLSQL_09_RESULT_DEFAULT)},
                {formatter("PLSQL_10", ?PLSQL_10, ?PLSQL_10_RESULT_DEFAULT)},
                {formatter("PLSQL_11", ?PLSQL_11, ?PLSQL_11_RESULT_DEFAULT)},
                {formatter("PLSQL_12", ?PLSQL_12, ?PLSQL_12_RESULT_DEFAULT)},
                {formatter("PLSQL_13", ?PLSQL_13, ?PLSQL_13_RESULT_DEFAULT)},
                {formatter("PLSQL_14", ?PLSQL_14, ?PLSQL_14_RESULT_DEFAULT)},
                {formatter("PLSQL_15", ?PLSQL_15, ?PLSQL_15_RESULT_DEFAULT)},
                {formatter("PLSQL_16", ?PLSQL_16, ?PLSQL_16_RESULT_DEFAULT)},
                {formatter("PLSQL_17", ?PLSQL_17, ?PLSQL_17_RESULT_DEFAULT)},
                {formatter("PLSQL_18", ?PLSQL_18, ?PLSQL_18_RESULT_DEFAULT)},
                {formatter("PLSQL_19", ?PLSQL_19, ?PLSQL_19_RESULT_DEFAULT)},
                {formatter("PLSQL_20", ?PLSQL_20, ?PLSQL_20_RESULT_DEFAULT)},
                {formatter("PLSQL_21", ?PLSQL_21, ?PLSQL_21_RESULT_DEFAULT)},
                {formatter("PLSQL_22", ?PLSQL_22, ?PLSQL_22_RESULT_DEFAULT)},
                {formatter("PLSQL_23", ?PLSQL_23, ?PLSQL_23_RESULT_DEFAULT)},
                {formatter("PLSQL_24", ?PLSQL_24, ?PLSQL_24_RESULT_DEFAULT)},
                {formatter("PLSQL_25", ?PLSQL_25, ?PLSQL_25_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing REVOKE with default options:
%%------------------------------------------------------------------------------

default_REVOKE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("REVOKE_01", ?REVOKE_01, ?REVOKE_01_RESULT_DEFAULT)},
                {formatter("REVOKE_02", ?REVOKE_02, ?REVOKE_02_RESULT_DEFAULT)},
                {formatter("REVOKE_03", ?REVOKE_03, ?REVOKE_03_RESULT_DEFAULT)},
                {formatter("REVOKE_04", ?REVOKE_04, ?REVOKE_04_RESULT_DEFAULT)},
                {formatter("REVOKE_05", ?REVOKE_05, ?REVOKE_05_RESULT_DEFAULT)},
                {formatter("REVOKE_06", ?REVOKE_06, ?REVOKE_06_RESULT_DEFAULT)},
                {formatter("REVOKE_07", ?REVOKE_07, ?REVOKE_07_RESULT_DEFAULT)},
                {formatter("REVOKE_08", ?REVOKE_08, ?REVOKE_08_RESULT_DEFAULT)},
                {formatter("REVOKE_09", ?REVOKE_09, ?REVOKE_09_RESULT_DEFAULT)},
                {formatter("REVOKE_10", ?REVOKE_10, ?REVOKE_10_RESULT_DEFAULT)},
                {formatter("REVOKE_11", ?REVOKE_11, ?REVOKE_11_RESULT_DEFAULT)},
                {formatter("REVOKE_12", ?REVOKE_12, ?REVOKE_12_RESULT_DEFAULT)},
                {formatter("REVOKE_13", ?REVOKE_13, ?REVOKE_13_RESULT_DEFAULT)},
                {formatter("REVOKE_14", ?REVOKE_14, ?REVOKE_14_RESULT_DEFAULT)},
                {formatter("REVOKE_15", ?REVOKE_15, ?REVOKE_15_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing ROLE with default options:
%%------------------------------------------------------------------------------

default_ROLE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("ROLE_01", ?ROLE_01, ?ROLE_01_RESULT_DEFAULT)},
                {formatter("ROLE_02", ?ROLE_02, ?ROLE_02_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing SELECT with default options.
%%------------------------------------------------------------------------------

default_SELECT_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("SELECT_01", ?SELECT_01, ?SELECT_01_RESULT_DEFAULT)},
                {formatter("SELECT_02", ?SELECT_02, ?SELECT_02_RESULT_DEFAULT)},
                {formatter("SELECT_03", ?SELECT_03, ?SELECT_03_RESULT_DEFAULT)},
                {formatter("SELECT_04", ?SELECT_04, ?SELECT_04_RESULT_DEFAULT)},
                {formatter("SELECT_05", ?SELECT_05, ?SELECT_05_RESULT_DEFAULT)},
                {formatter("SELECT_06", ?SELECT_06, ?SELECT_06_RESULT_DEFAULT)},
                {formatter("SELECT_07", ?SELECT_07, ?SELECT_07_RESULT_DEFAULT)},
                {formatter("SELECT_08", ?SELECT_08, ?SELECT_08_RESULT_DEFAULT)},
                {formatter("SELECT_09", ?SELECT_09, ?SELECT_09_RESULT_DEFAULT)},
                {formatter("SELECT_10", ?SELECT_10, ?SELECT_10_RESULT_DEFAULT)},
                {formatter("SELECT_11", ?SELECT_11, ?SELECT_11_RESULT_DEFAULT)},
                {formatter("SELECT_12", ?SELECT_12, ?SELECT_12_RESULT_DEFAULT)},
                {formatter("SELECT_13", ?SELECT_13, ?SELECT_13_RESULT_DEFAULT)},
                {formatter("SELECT_14", ?SELECT_14, ?SELECT_14_RESULT_DEFAULT)},
                {formatter("SELECT_15", ?SELECT_15, ?SELECT_15_RESULT_DEFAULT)},
                {formatter("SELECT_16", ?SELECT_16, ?SELECT_16_RESULT_DEFAULT)},
                {formatter("SELECT_17", ?SELECT_17, ?SELECT_17_RESULT_DEFAULT)},
                {formatter("SELECT_18", ?SELECT_18, ?SELECT_18_RESULT_DEFAULT)},
                {formatter("SELECT_19", ?SELECT_19, ?SELECT_19_RESULT_DEFAULT)},
                {formatter("SELECT_20", ?SELECT_20, ?SELECT_20_RESULT_DEFAULT)},
                {formatter("SELECT_21", ?SELECT_21, ?SELECT_21_RESULT_DEFAULT)},
                {formatter("SELECT_22", ?SELECT_22, ?SELECT_22_RESULT_DEFAULT)},
                {formatter("SELECT_23", ?SELECT_23, ?SELECT_23_RESULT_DEFAULT)},
                {formatter("SELECT_24", ?SELECT_24, ?SELECT_24_RESULT_DEFAULT)},
                {formatter("SELECT_25", ?SELECT_25, ?SELECT_25_RESULT_DEFAULT)},
                {formatter("SELECT_26", ?SELECT_26, ?SELECT_26_RESULT_DEFAULT)},
                {formatter("SELECT_27", ?SELECT_27, ?SELECT_27_RESULT_DEFAULT)},
                {formatter("SELECT_28", ?SELECT_28, ?SELECT_28_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing STRUCTURE with default options.
%%------------------------------------------------------------------------------

default_STRUCTURE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("STRUCTURE_01", ?STRUCTURE_01, ?STRUCTURE_01_RESULT_DEFAULT)},
                {formatter("STRUCTURE_02", ?STRUCTURE_02, ?STRUCTURE_02_RESULT_DEFAULT)},
                {formatter("STRUCTURE_03", ?STRUCTURE_03, ?STRUCTURE_03_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing TRANSACTION with default options.
%%------------------------------------------------------------------------------

default_TRANSACTION_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TRANSACTION_01", ?TRANSACTION_01, ?TRANSACTION_01_RESULT_DEFAULT)},
                {formatter("TRANSACTION_02", ?TRANSACTION_02, ?TRANSACTION_02_RESULT_DEFAULT)},
                {formatter("TRANSACTION_03", ?TRANSACTION_03, ?TRANSACTION_03_RESULT_DEFAULT)},
                {formatter("TRANSACTION_04", ?TRANSACTION_04, ?TRANSACTION_04_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing TRUNCATE with default options.
%%------------------------------------------------------------------------------

default_TRUNCATE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TRUNCATE_01", ?TRUNCATE_01, ?TRUNCATE_01_RESULT_DEFAULT)},
                {formatter("TRUNCATE_02", ?TRUNCATE_02, ?TRUNCATE_02_RESULT_DEFAULT)},
                {formatter("TRUNCATE_03", ?TRUNCATE_03, ?TRUNCATE_03_RESULT_DEFAULT)},
                {formatter("TRUNCATE_04", ?TRUNCATE_04, ?TRUNCATE_04_RESULT_DEFAULT)},
                {formatter("TRUNCATE_05", ?TRUNCATE_05, ?TRUNCATE_05_RESULT_DEFAULT)},
                {formatter("TRUNCATE_06", ?TRUNCATE_06, ?TRUNCATE_06_RESULT_DEFAULT)},
                {formatter("TRUNCATE_07", ?TRUNCATE_07, ?TRUNCATE_07_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing UNION with default options.
%%------------------------------------------------------------------------------

default_UNION_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("UNION_01", ?UNION_01, ?UNION_01_RESULT_DEFAULT)},
                {formatter("UNION_02", ?UNION_02, ?UNION_02_RESULT_DEFAULT)},
                {formatter("UNION_03", ?UNION_03, ?UNION_03_RESULT_DEFAULT)},
                {formatter("UNION_04", ?UNION_04, ?UNION_04_RESULT_DEFAULT)},
                {formatter("UNION_05", ?UNION_05, ?UNION_05_RESULT_DEFAULT)},
                {formatter("UNION_06", ?UNION_06, ?UNION_06_RESULT_DEFAULT)},
                {formatter("UNION_07", ?UNION_07, ?UNION_07_RESULT_DEFAULT)},
                {formatter("UNION_08", ?UNION_08, ?UNION_08_RESULT_DEFAULT)},
                {formatter("UNION_09", ?UNION_09, ?UNION_09_RESULT_DEFAULT)},
                {formatter("UNION_10", ?UNION_10, ?UNION_10_RESULT_DEFAULT)},
                {formatter("UNION_11", ?UNION_11, ?UNION_11_RESULT_DEFAULT)},
                {formatter("UNION_12", ?UNION_12, ?UNION_12_RESULT_DEFAULT)},
                {formatter("UNION_13", ?UNION_13, ?UNION_13_RESULT_DEFAULT)},
                {formatter("UNION_14", ?UNION_14, ?UNION_14_RESULT_DEFAULT)},
                {formatter("UNION_15", ?UNION_15, ?UNION_15_RESULT_DEFAULT)},
                {formatter("UNION_16", ?UNION_16, ?UNION_16_RESULT_DEFAULT)},
                {formatter("UNION_17", ?UNION_17, ?UNION_17_RESULT_DEFAULT)},
                {formatter("UNION_18", ?UNION_18, ?UNION_18_RESULT_DEFAULT)},
                {formatter("UNION_19", ?UNION_19, ?UNION_19_RESULT_DEFAULT)},
                {formatter("UNION_20", ?UNION_20, ?UNION_20_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing UPDATE with default options.
%%------------------------------------------------------------------------------

default_UPDATE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("UPDATE_01", ?UPDATE_01, ?UPDATE_01_RESULT_DEFAULT)},
                {formatter("UPDATE_02", ?UPDATE_02, ?UPDATE_02_RESULT_DEFAULT)},
                {formatter("UPDATE_03", ?UPDATE_03, ?UPDATE_03_RESULT_DEFAULT)},
                {formatter("UPDATE_04", ?UPDATE_04, ?UPDATE_04_RESULT_DEFAULT)},
                {formatter("UPDATE_05", ?UPDATE_05, ?UPDATE_05_RESULT_DEFAULT)},
                {formatter("UPDATE_06", ?UPDATE_06, ?UPDATE_06_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing VIEW with default options.
%%------------------------------------------------------------------------------

default_VIEW_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("VIEW_01", ?VIEW_01, ?VIEW_01_RESULT_DEFAULT)},
                {formatter("VIEW_02", ?VIEW_02, ?VIEW_02_RESULT_DEFAULT)},
                {formatter("VIEW_03", ?VIEW_03, ?VIEW_03_RESULT_DEFAULT)},
                {formatter("VIEW_04", ?VIEW_04, ?VIEW_04_RESULT_DEFAULT)},
                {formatter("VIEW_05", ?VIEW_05, ?VIEW_05_RESULT_DEFAULT)},
                {formatter("VIEW_06", ?VIEW_06, ?VIEW_06_RESULT_DEFAULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Testing WHERE with default options.
%%------------------------------------------------------------------------------

default_WHERE_test_() ->
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("WHERE_01", ?WHERE_01, ?WHERE_01_RESULT_DEFAULT)},
                {formatter("WHERE_02", ?WHERE_02, ?WHERE_02_RESULT_DEFAULT)},
                {formatter("WHERE_03", ?WHERE_03, ?WHERE_03_RESULT_DEFAULT)},
                {formatter("WHERE_04", ?WHERE_04, ?WHERE_04_RESULT_DEFAULT)},
                {formatter("WHERE_05", ?WHERE_05, ?WHERE_05_RESULT_DEFAULT)},
                {formatter("WHERE_06", ?WHERE_06, ?WHERE_06_RESULT_DEFAULT)},
                {formatter("WHERE_07", ?WHERE_07, ?WHERE_07_RESULT_DEFAULT)},
                {formatter("WHERE_08", ?WHERE_08, ?WHERE_08_RESULT_DEFAULT)},
                {formatter("WHERE_09", ?WHERE_09, ?WHERE_09_RESULT_DEFAULT)},
                {formatter("WHERE_10", ?WHERE_10, ?WHERE_10_RESULT_DEFAULT)},
                {formatter("WHERE_11", ?WHERE_11, ?WHERE_11_RESULT_DEFAULT)},
                {formatter("WHERE_12", ?WHERE_12, ?WHERE_12_RESULT_DEFAULT)},
                {formatter("WHERE_13", ?WHERE_13, ?WHERE_13_RESULT_DEFAULT)},
                {formatter("WHERE_14", ?WHERE_14, ?WHERE_14_RESULT_DEFAULT)},
                {formatter("WHERE_15", ?WHERE_15, ?WHERE_15_RESULT_DEFAULT)},
                {formatter("WHERE_16", ?WHERE_16, ?WHERE_16_RESULT_DEFAULT)},
                {formatter("WHERE_17", ?WHERE_17, ?WHERE_17_RESULT_DEFAULT)},
                {formatter("WHERE_18", ?WHERE_18, ?WHERE_18_RESULT_DEFAULT)},
                {formatter("WHERE_19", ?WHERE_19, ?WHERE_19_RESULT_DEFAULT)},
                {formatter("WHERE_20", ?WHERE_20, ?WHERE_20_RESULT_DEFAULT)},
                {formatter("WHERE_21", ?WHERE_21, ?WHERE_21_RESULT_DEFAULT)}
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
    {
        setup,
        fun setup_K_I_4_S_T/0,
        fun() ->
            [
                {formatter("OPTION_01_K_I_4_S_T", ?OPTION_01, ?OPTION_01_RESULT_K_I_4_S_T)}
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

option_L_U_4_S_T_test() ->
    {
        setup,
        fun setup_L_U_4_S_T/0,
        fun() ->
            [
                {formatter("OPTION_01_L_U_4_S_T", ?OPTION_01, ?OPTION_01_RESULT_L_U_4_S_T)}
            ]
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

option_U_L_4_S_T_test() ->
    {
        setup,
        fun setup_U_L_4_S_T/0,
        fun() ->
            [
                {formatter("OPTION_01_U_L_4_S_T", ?OPTION_01, ?OPTION_01_RESULT_U_L_4_S_T)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Helper functions.
%%------------------------------------------------------------------------------

%%formatter(Title, Statement, Result) ->
%%    ?debugFmt(?MODULE_STRING ++ ":formatter ===> Start ~n Title: ~p~n", [Title]),
%%
%%    case ?PARSER_MODULE:parsetree_with_tokens(Statement) of
%%        {ok, {ParseTree, _Tokens}} ->
%%            FormattedStatement = case ?PARSER_MODULE:pt_to_string_format(ParseTree) of
%%                                     {error, Error_TD} ->
%%                                         throw({error, Error_TD});
%%                                     Formatted ->
%%                                         Formatted
%%                                 end,
%%            ?debugFmt(?MODULE_STRING ++ ":formatter ===>~nFormattedStatement:~p~n", [FormattedStatement]),
%%            ?assertEqual(Result, binary_to_list(FormattedStatement));
%%        {lex_error, _Error} ->
%%            throw({error, "Failed lex_error - Statement " ++ Title ++ ": " ++ Statement});
%%        {parse_error, {_Error, _Tokens}} ->
%%            throw({error, "Failed parse_error - Statement " ++ Title ++ ": " ++ Statement})
%%    end.

formatter(Title, Statement, Result) ->
    ?debugFmt(?MODULE_STRING ++ ":formatter ===> Start ~n Title: ~p~n", [Title]),

    %% -------------------------------------------------------------------------
    %% 1. Source ==> ParseTree
    %% -------------------------------------------------------------------------
    case ?PARSER_MODULE:parsetree_with_tokens(Statement) of
        {ok, {ParseTree, _Tokens}} ->
            %% -----------------------------------------------------------------
            %% Test TopDown
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source (=NSource_TD)
            %% -----------------------------------------------------------------
            NSource_TD = case ?PARSER_MODULE:pt_to_string(ParseTree) of
                             {error, Error_TD} ->
                                 io:format(user, "~n[TD] Error ParseTree -> NewSource : ParseTree~n > ~p", [ParseTree]),
                                 io:format(user, "~n[TD] Error ParseTree -> NewSource : Error~n > ~p", [Error_TD]),
                                 throw({error, Error_TD});
                             NS_TD ->
                                 NS_TD
                         end,
            %% -----------------------------------------------------------------
            %% 3. Source (=NSource_TD) ==> ParseTree (=NPT_TD)
            %% -----------------------------------------------------------------
            {ok, {NPTree_TD, _NToks_TD}}
                = try
                      {ok, {NPT_TD, NT_TD}} = ?PARSER_MODULE:parsetree_with_tokens(NSource_TD),
                      {ok, {NPT_TD, NT_TD}}
                  catch
                      ExceptionTD:ReasonTD ->
                          io:format(user, "~n[TD] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Title    ~n > ~p", [Title]),
                          io:format(user, "~n[TD] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Statement~n > ~p", [Statement]),
                          io:format(user, "~n[TD] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : NewSource~n > ~p", [NSource_TD]),
                          io:format(user, "~n[TD] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Exception~n > ~p", [ExceptionTD]),
                          io:format(user, "~n[TD] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Reason   ~n > ~p", [ReasonTD]),
                          throw({error, "[TD] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens"})
                  end,
            if ParseTree /= NPTree_TD ->
                io:format(user, "~n[TD] Error ParseTree = NPTree : Title       ~n > ~p", [Title]),
                io:format(user, "~n[TD] Error ParseTree = NPTree : Statement   ~n > ~p", [Statement]),
                io:format(user, "~n[TD] Error ParseTree = NPTree : NewParseTree~n > ~p", [NPTree_TD]),
                io:format(user, "~n[TD] Error ParseTree = NPTree : Tokens      ~n > ~p", [_Tokens]),
                io:format(user, "~n[TD] Error ParseTree = NPTree : NewTokens   ~n > ~p", [_NToks_TD]);
                true -> ok
            end,
            ?assertEqual(ParseTree, NPTree_TD),
            StringNSource_TD = binary:bin_to_list(NSource_TD),
            StringNSource_TDMultipleSpace = string:str(StringNSource_TD, "  "),
            case StringNSource_TDMultipleSpace of
                0 -> ok;
                _ ->
                    io:format(user, "~n[TD] Error redundant whitespace(s) : Title          ~n > ~p", [Title]),
                    io:format(user, "~n[TD] Error redundant whitespace(s) : Statement      ~n > ~p", [Statement]),
                    io:format(user, "~n[TD] Error redundant whitespace(s) : NewSource      ~n > ~p", [StringNSource_TD]),
                    io:format(user, "~n[TD] Error redundant whitespace(s) : 1. Redundant WS~n > ~p", [StringNSource_TDMultipleSpace]),
                    throw({error, "[TD] Error redundant whitespace(s)"})
            end,
            %% -----------------------------------------------------------------
            %% Test BottomUp
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source (=NSource_BU)
            %% -----------------------------------------------------------------
            NSource_BU = case ?PARSER_MODULE:pt_to_string_bu(ParseTree) of
                             {error, Error_BU} ->
                                 io:format(user, "~n[BU] Error ParseTree -> NewSource : ParseTree~n > ~p", [ParseTree]),
                                 throw({error, "[BU] Error ParseTree -> NewSource : " ++ Error_BU});
                             NS_BU ->
                                 NS_BU
                         end,
            %% -----------------------------------------------------------------
            %% 3. Source (=NSource_BU) ==> ParseTree (=NPT_BU)
            %% -----------------------------------------------------------------
            {ok, {NPTree_BU, _NToks_BU}}
                = try
                      {ok, {NPT_BU, NT_BU}} = ?PARSER_MODULE:parsetree_with_tokens(NSource_BU),
                      {ok, {NPT_BU, NT_BU}}
                  catch
                      ExceptionBU:ReasonBU ->
                          io:format(user, "~n[BU] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Title    ~n > ~p", [Title]),
                          io:format(user, "~n[BU] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Statement~n > ~p", [Statement]),
                          io:format(user, "~n[BU] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : NewSource~n > ~p", [NSource_BU]),
                          io:format(user, "~n[BU] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Exception~n > ~p", [ExceptionBU]),
                          io:format(user, "~n[BU] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Reason   ~n > ~p", [ReasonBU]),
                          throw({error, "[BU] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens"})
                  end,
            if ParseTree /= NPTree_BU ->
                io:format(user, "~n[BU] Error ParseTree = NPTree : Title       ~n > ~p", [Title]),
                io:format(user, "~n[BU] Error ParseTree = NPTree : Statement   ~n > ~p", [Statement]),
                io:format(user, "~n[BU] Error ParseTree = NPTree : NewParseTree~n > ~p", [NPTree_BU]),
                io:format(user, "~n[BU] Error ParseTree = NPTree : Tokens      ~n > ~p", [_Tokens]),
                io:format(user, "~n[BU] Error ParseTree = NPTree : NewTokens   ~n > ~p", [_NToks_BU]);
                true -> ok
            end,
            ?assertEqual(ParseTree, NPTree_BU),
            StringNSource_BU = binary:bin_to_list(NSource_BU),
            StringNSource_BUMultipleSpace = string:str(StringNSource_BU, "  "),
            case StringNSource_BUMultipleSpace of
                0 -> ok;
                _ ->
                    io:format(user, "~n[BU] Error redundant whitespace(s) : Title          ~n > ~p", [Title]),
                    io:format(user, "~n[BU] Error redundant whitespace(s) : Statement      ~n > ~p", [Statement]),
                    io:format(user, "~n[BU] Error redundant whitespace(s) : NewSource      ~n > ~p", [StringNSource_BU]),
                    io:format(user, "~n[BU] Error redundant whitespace(s) : 1. Redundant WS~n > ~p", [StringNSource_BUMultipleSpace]),
                    throw({error, "[BU] Error redundant whitespace(s)"})
            end,
            %% -----------------------------------------------------------------
            %% Test Format
            %% -----------------------------------------------------------------
            %% 4. ParseTree ==> Source (=FormattedStatement)
            %% -----------------------------------------------------------------
            FormattedStatement = case ?PARSER_MODULE:pt_to_string_format(ParseTree) of
                                     {error, Error_Format} ->
                                         io:format(user, "~n[BU] Error ParseTree -> NewSource Format : ParseTree~n > ~p", [ParseTree]),
                                         throw({error, "[BU] Error ParseTree -> NewSource Format : " ++ Error_Format});
                                     NS_Format ->
                                         NS_Format
                                 end,
            ?debugFmt(?MODULE_STRING ++ ":formatter ===>~nFormattedStatement:~p~n", [FormattedStatement]),
            ?assertEqual(Result, binary_to_list(FormattedStatement)),
            %% -----------------------------------------------------------------
            %% 5. Source (=FormattedStatement) ==> ParseTree (=NPT_FORMAT)
            %% -----------------------------------------------------------------
            {ok, {_NPTree_FORMAT, _NToks_FORMAT}}
                = try
                      {ok, {NPT_FORMAT, NT_FORMAT}} = ?PARSER_MODULE:parsetree_with_tokens(FormattedStatement),
                      {ok, {NPT_FORMAT, NT_FORMAT}}
                  catch
                      ExceptionFORMAT:ReasonFORMAT ->
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Title          ~n > ~p", [Title]),
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Statement      ~n > ~p", [Statement]),
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : FormattedSource~n > ~p", [FormattedStatement]),
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Exception      ~n > ~p", [ExceptionFORMAT]),
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens : Reason         ~n > ~p", [ReasonFORMAT]),
                          throw({error, "[FORMAT] Error " ++ ?MODULE_STRING ++ ":parsetree_with_tokens"})
                  end;
        {lex_error, _Error} ->
            io:format(user, "~nFailed lex_error : Title    ~n > ~p", [Title]),
            io:format(user, "~nFailed lex_error : Statement~n > ~p", [Statement]),
            io:format(user, "~nFailed lex_error : Error    ~n > ~p", [_Error]),
            throw({error, "Failed lex_error"});
        {parse_error, {_Error, _Tokens}} ->
            io:format(user, "~nFailed parse_error : Title    ~n > ~p", [Title]),
            io:format(user, "~nFailed parse_error : Statement~n > ~p", [Statement]),
            io:format(user, "~nFailed parse_error : Tokens   ~n > ~p", [_Tokens]),
            io:format(user, "~nFailed parse_error : Error    ~n > ~p", [_Error]),
            throw({error, "Failed parse_error"})
    end.

%%------------------------------------------------------------------------------
%% Setup functions.
%%------------------------------------------------------------------------------

setup_default() ->
    os:putenv("CASE_IDENTIFIER", "init_cap"),
    os:putenv("CASE_KEYWORD", "upper"),

    os:putenv("CR_LIMIT_ALTER_ROLES", "3"),
    os:putenv("CR_LIMIT_ALTER_USERS", "3"),
    os:putenv("CR_LIMIT_CREATE_INDEX", "3"),
    os:putenv("CR_LIMIT_DROP_TABLE", "3"),
    os:putenv("CR_LIMIT_FUNC_ARGS", "3"),
    os:putenv("CR_LIMIT_GRANT_GRANTEE", "3"),
    os:putenv("CR_LIMIT_GRANT_PRIVILEGE", "3"),
    os:putenv("CR_LIMIT_GROUP_BY", "3"),
    os:putenv("CR_LIMIT_INSERT", "3"),
    os:putenv("CR_LIMIT_INTO", "3"),
    os:putenv("CR_LIMIT_ORDER_BY", "3"),
    os:putenv("CR_LIMIT_PARTITION", "3"),
    os:putenv("CR_LIMIT_RETURNING", "3"),
    os:putenv("CR_LIMIT_REVOKE_PRIVILEGE", "3"),
    os:putenv("CR_LIMIT_REVOKE_REVOKEE", "3"),
    os:putenv("CR_LIMIT_SELECT", "3"),
    os:putenv("CR_LIMIT_USING", "3"),
    os:putenv("CR_LIMIT_VIEW", "3"),

    os:putenv("INDENT_SPACE", "4"),
    os:putenv("INDENT_WITH", "spaces"),
    os:putenv("WS_OPERATORS", "true").

setup_K_I_4_S_T() ->
    setup_default(),
    os:putenv("CASE_IDENTIFIER", "keep_unchanged"),
    os:putenv("CASE_KEYWORD", "init_cap").


setup_L_U_4_S_T() ->
    setup_default(),
    os:putenv("CASE_IDENTIFIER", "lower").

setup_U_L_4_S_T() ->
    setup_default(),
    os:putenv("CASE_IDENTIFIER", "upper"),
    os:putenv("CASE_KEYWORD", "lower")
.
