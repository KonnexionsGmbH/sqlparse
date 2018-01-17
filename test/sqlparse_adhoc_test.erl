%% -----------------------------------------------------------------------------
%%
%% sqlparse_adhoc_test.erl: SQL - test driver for development purposes.
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

-module(sqlparse_adhoc_test).

-export([eunit_test_source/1]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-include("sqlparse_test.hrl").

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Determine Files.
%%------------------------------------------------------------------------------

eunit_test_() ->
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
    os:putenv("WS_OPERATORS", "true"),

    {ok, Cwd} = file:get_cwd(),
    RootPath = lists:reverse(filename:split(Cwd)),
    TestDir = filename:join(lists:reverse(["test" | RootPath])),
    TestFile = filename:join(TestDir, ?MODULE_STRING ++ ?ENV_VAR_FILE_TYPE),

                    {ok, [Opts | Tests]} = file:consult(TestFile),
                    {ok, TestFileBin} = file:read_file(TestFile),
                    TestLines = [begin
                     TRe0 = re:replace(T, "(.*)(\")(.*)", "\\1\\\\\"\\3",
                         [{return, list}, ungreedy, global, dotall]),
                     TRe = list_to_binary(io_lib:format("~p", [TRe0])),
                                     case binary:match(TestFileBin, TRe) of
                                         {I1, _} ->
                             <<Head:I1/binary, _/binary>> = TestFileBin,
                                             case re:run(Head, ".*[\r\n]",
                                                 [global]) of
                                 {match, Matches} -> length(Matches) + 1;
                                                 nomatch ->
                                                     io:format(user,
                                                         "~p~n"
                                                         ">>>>>>>>>>> HEAD ~p <<<<<<<<<<<~n"
                                                         "Opts ~p~n"
                                                         "Tests ~p~n"
                                                         "T ~p~n"
                                                         "TRe ~p~n"
                                                         ,
                                         [TestFile, Head, Opts, Tests, T, TRe]),
                                                     error(nomatch)
                                             end;
                         nomatch -> I
                                     end
                                 end
        || {I, T} <- lists:zip(lists:seq(1, length(Tests)), Tests)],
                    AugTests = lists:zip(TestLines, Tests),
    tests_gen(AugTests, Opts).

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test_source(Source) ->
    ?debugFmt(?MODULE_STRING ++ ":eunit_test_source ===> Start ~nSource: ~p~n",
        [Source]),
    io:format(user, "~n", []),
    io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ===========================================>           Statement(s):~n~ts~n",
        [Source]),

    case sqlparse_test_utils:eunit_test(Source) of
        {ok, Source_FORMAT} ->
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : -------------------------------------------> Formatted Statement(s):~n~n~ts~n",
                [binary_to_list(Source_FORMAT)]),
            io:format(user, "~n", []),
            ok;
        Result ->
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test_source : ErrorResult~n > ~p~n",
                [Result])
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Groups.
%%------------------------------------------------------------------------------

tests_gen(Tests, Opts) ->
    SelTests = case proplists:get_value(tests, Opts) of
                   St when St =:= undefined; St =:= [] ->
                       {Indices, _} = lists:unzip(Tests),
                       Indices;
                   St -> St
               end,
    tests_gen(Tests, SelTests, []).

tests_gen([], _SelTests, Acc) ->
    {inorder, lists:reverse(Acc)};
tests_gen([{I, T} | Tests], SelTests, Acc) ->
    case lists:member(I, SelTests) of
        true ->
            tests_gen(Tests, SelTests,
                [{I, fun() ->
                    {timeout, ?TIMEOUT, ?MODULE:eunit_test_source(T)}
                    end} | Acc]);
        _ -> Acc
    end.
