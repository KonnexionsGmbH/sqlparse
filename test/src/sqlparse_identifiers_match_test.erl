%% -----------------------------------------------------------------------------
%%
%% sqlparse_identifiers_match_test.erl: SQL - test driver.
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

-module(sqlparse_identifiers_match_test).

-define(NODEBUG, true).

-include("sqlparse_identifiers_match_test.hrl").

%%------------------------------------------------------------------------------
%% Testing.
%%------------------------------------------------------------------------------

overall_test_() ->
    ?D("Start ~n"),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {prune("TEST_01", ?TEST_01, ?TEST_01_RESULT)},
                {prune("TEST_02", ?TEST_02, ?TEST_02_RESULT)},
                {prune("TEST_03", ?TEST_03, ?TEST_03_RESULT)},
                {prune("TEST_04", ?TEST_04, ?TEST_04_RESULT)},
                {prune("TEST_05", ?TEST_05, ?TEST_05_RESULT)},
                {prune("TEST_06", ?TEST_06, ?TEST_06_RESULT)},
                {prune("TEST_07", ?TEST_07, ?TEST_07_RESULT)},
                {prune("TEST_08", ?TEST_08, ?TEST_08_RESULT)},
                {prune("TEST_09", ?TEST_09, ?TEST_09_RESULT)},
                {prune("TEST_10", ?TEST_10, ?TEST_10_RESULT)},
                {prune("TEST_11", ?TEST_11, ?TEST_11_RESULT)},
                {prune("TEST_12", ?TEST_12, ?TEST_12_RESULT)},
                {prune("TEST_13", ?TEST_13, ?TEST_13_RESULT)},
                {prune("TEST_14", ?TEST_14, ?TEST_14_RESULT)},
                {prune("TEST_15", ?TEST_15, ?TEST_15_RESULT)},
                {prune("TEST_16", ?TEST_16, ?TEST_16_RESULT)},
                {prune("TEST_17", ?TEST_17, ?TEST_17_RESULT)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Helper functions.
%%------------------------------------------------------------------------------

prune(Title, Source, Result) ->
    ?D("Start ~n Title: ~p~n Source: ~p~n Result: ~p~n",
        [Title, Source, Result]),
    {ok, ParseTree} = sqlparse:parsetree(Source),
    ?D("~n ParseTree: ~p~n", [ParseTree]),
    case sqlparse_fold:top_down(sqlparse_identifiers_match, ParseTree,
        ?IN_FIELDS) of
        OutFields when is_list(OutFields) ->
            ?assertEqual(Result, OutFields, Title);
        ErrorResult ->
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test : Title      ~n > ~p~n", [Title]),
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test : ErrorResult~n > ~p~n", [ErrorResult])
    end.

%%------------------------------------------------------------------------------
%% Setup functions.
%%------------------------------------------------------------------------------

setup_default() ->
    ?D("Start ~n"),
    ok.
