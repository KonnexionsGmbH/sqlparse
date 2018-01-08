%% -----------------------------------------------------------------------------
%%
%% develop_formatter_test.erl: SQL - formatter test driver for development purposes.
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

-module(develop_formatter_test).

-export([eunit_test_source/2]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-define(ENV_VAR_FILE_TYPE, ".tst").
-define(ENV_VAR_FILE_WILDCARD, "SOURCEFILES").
-define(PARSER_MODULE, sqlparse).
-define(TIMEOUT, 60).

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

    WCard = ?MODULE_STRING ++ ?ENV_VAR_FILE_TYPE,
    ?debugFmt(?MODULE_STRING ++ ":eunit_test_ ===>~nFile = ~s", [WCard]),
    {ok, Cwd} = file:get_cwd(),
    ?debugFmt(?MODULE_STRING ++ ":eunit_test_ ===>~nCwd: ~p~n", [Cwd]),
    RootPath = lists:reverse(filename:split(Cwd)),
    ?debugFmt(?MODULE_STRING ++ ":eunit_test_ ===>~nRootPath: ~p~n", [RootPath]),
    TestDir1 = filename:join(lists:reverse(["test" | RootPath])),
    ?debugFmt(?MODULE_STRING ++ ":eunit_test_ ===>~nTestDir1: ~p~n", [TestDir1]),
    TestDir2 = filename:join(lists:reverse(["eunit", "generated", "test"] ++ RootPath)),
    ?debugFmt(?MODULE_STRING ++ ":eunit_test_ ===>~nTestDir2: ~p~n", [TestDir2]),
    TestFiles = lists:sort(
        [filename:join(TestDir1, T) || T <- filelib:wildcard(WCard, TestDir1)] ++
        [filename:join(TestDir2, T) || T <- filelib:wildcard(WCard, TestDir2)]
    ),
    group_gen(TestFiles).

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test_source(_TestGroup, Statement) ->
    ?debugFmt(?MODULE_STRING ++ ":eunit_test_source ===> TestGroup:~n~p~n", [_TestGroup]),
    io:format(user, "~n", []),
    io:format(user, "~n===========================================>           Statement(s):~n~ts~n", [Statement]),

    %% -------------------------------------------------------------------------
    %% 1. Statement ==> ParseTree
    %% -------------------------------------------------------------------------
    case ?PARSER_MODULE:parsetree_with_tokens(Statement) of
        {ok, {ParseTree, _Tokens}} ->
            ?debugFmt(?MODULE_STRING ++ ":eunit_test_source ===> ParseTree:~n~p~n", [ParseTree]),
            ?debugFmt(?MODULE_STRING ++ ":eunit_test_source ===> Tokens:   ~n~p~n", [_Tokens]),
            %% -----------------------------------------------------------------
            %% Test Format
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Statement (=FormattedStatement)
            %% -----------------------------------------------------------------
            FormattedStatement = case ?PARSER_MODULE:pt_to_string_format(ParseTree) of
                                     {error, Error_TD} ->
                                         io:format(user, ?MODULE_STRING ++ ":eunit_test_source ===> Error_TD:~n~p~n", [Error_TD]),
                                         throw({error, Error_TD});
                                     NS_TD ->
                                         NS_TD
                                 end,
            io:format(user, "~n-------------------------------------------> Formatted Statement(s):~n~n~ts~n", [binary_to_list(FormattedStatement)]),
            io:format(user, "~n", []),
            %% -----------------------------------------------------------------
            %% 3. Statement (=FormattedStatement) ==> ParseTree (=NPT_FORMAT)
            %% -----------------------------------------------------------------
            {ok, {_NPTree_FORMAT, _NToks_FORMAT}}
                = try
                      {ok, {NPT_FORMAT, NT_FORMAT}} = ?PARSER_MODULE:parsetree_with_tokens(FormattedStatement),
                      {ok, {NPT_FORMAT, NT_FORMAT}}
                  catch
                      ExceptionFORMAT:ReasonFORMAT ->
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":eunit_test_source : Statement      ~n > ~p", [Statement]),
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":eunit_test_source : FormattedSource~n > ~p", [FormattedStatement]),
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":eunit_test_source : Exception      ~n > ~p", [ExceptionFORMAT]),
                          io:format(user, "~n[FORMAT] Error " ++ ?MODULE_STRING ++ ":eunit_test_source : Reason         ~n > ~p", [ReasonFORMAT]),
                          throw({error, "[FORMAT] Error " ++ ?MODULE_STRING ++ ":eunit_test_source"})
                  end;        {lex_error, _Error} ->
        io:format(user, ?MODULE_STRING ++ ":eunit_test_source ===> Error:    ~n~p~n", [_Error]),
        throw({error, "Failed lex_error"});
        {parse_error, {_Error, _Tokens}} ->
            io:format(user, ?MODULE_STRING ++ ":eunit_test_source ===> Error:    ~n~p~n", [_Error]),
            io:format(user, ?MODULE_STRING ++ ":eunit_test_source ===> Tokens:   ~n~p~n", [_Tokens]),
            throw({error, "Failed parse_error"})
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Files.
%%------------------------------------------------------------------------------

group_gen(TestFiles) ->
    ?debugFmt(?MODULE_STRING ++ ":group_gen ===>~nTestFiles = ~p", [TestFiles]),
    {generator,
        fun() ->
            case TestFiles of
                [] ->
                    [];
                [TestFile | RestTestFiles] ->
                    {ok, [Opts | Tests]} = file:consult(TestFile),
                    {ok, TestFileBin} = file:read_file(TestFile),
                    TestLines = [begin
                                     TRe0 = re:replace(T, "(.*)(\")(.*)", "\\1\\\\\"\\3"
                                         , [{return, list}, ungreedy, global, dotall]),
                                     TRe = list_to_binary(io_lib:format("~p", [TRe0])),
                                     case binary:match(TestFileBin, TRe) of
                                         {I1, _} ->
                                             <<Head:I1/binary, _/binary>> = TestFileBin,
                                             case re:run(Head, ".*[\r\n]", [global]) of
                                                 {match, Matches} ->
                                                     length(Matches) + 1;
                                                 nomatch ->
                                                     io:format(user,
                                                         "~p~n"
                                                         ">>>>>>>>>>> HEAD ~p <<<<<<<<<<<~n"
                                                         "Opts ~p~n"
                                                         "Tests ~p~n"
                                                         "T ~p~n"
                                                         "TRe ~p~n"
                                                         , [TestFile, Head, Opts, Tests, T, TRe]),
                                                     error(nomatch)
                                             end;
                                         nomatch ->
                                             I
                                     end
                                 end
                        || {I, T} <- lists:zip(lists:seq(1, length(Tests)), Tests)],
                    AugTests = lists:zip(TestLines, Tests),
                    TestGroup = filename:rootname(
                        filename:basename(TestFile)),
                    [tests_gen(TestGroup, AugTests, Opts)
                        | group_gen(RestTestFiles)]
            end
        end}.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Groups.
%%------------------------------------------------------------------------------

tests_gen(TestGroup, Tests, Opts) ->
    ?debugFmt(?MODULE_STRING ++ ":tests_gen ===>~nTestGroup = ~p~n Tests = ~p~n Opts = ~p~n", [TestGroup, Tests, Opts]),
    SelTests = case proplists:get_value(tests, Opts) of
                   St when St =:= undefined; St =:= [] ->
                       {Indices, _} = lists:unzip(Tests),
                       Indices;
                   St -> St
               end,
    tests_gen(TestGroup, Tests, SelTests, []).

tests_gen(_TestGroup, [], _SelTests, Acc) ->
    {inorder, lists:reverse(Acc)};
tests_gen(TestGroup, [{I, T} | Tests], SelTests, Acc) ->
    case lists:member(I, SelTests) of
        true ->
            tests_gen(TestGroup, Tests, SelTests,
                [{TestGroup, I,
                    fun() ->
                        {timeout, ?TIMEOUT, ?MODULE:eunit_test_source(TestGroup, T)}
                    end} | Acc]);
        _ -> Acc
    end.
