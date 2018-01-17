%% -----------------------------------------------------------------------------
%%
%% sqlparse_test_utils.erl: SQL - test driver utilities.
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

-module(sqlparse_test_utils).

-export([eunit_test/1]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-include("sqlparse_test.hrl").

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test(Source) ->
    ?debugFmt(?MODULE_STRING ++ ":eunit_test ===> Start ~nSource: ~p~n",
        [Source]),
    ?debugFmt(?MODULE_STRING ++ ":eunit_test ===>~n CASE_IDENTIFIER: ~p~n",
        [string:to_lower(os:getenv("CASE_IDENTIFIER"))]),
    ?debugFmt(?MODULE_STRING ++ ":eunit_test ===>~n CASE_KEYWORD   : ~p~n",
        [string:to_lower(os:getenv("CASE_KEYWORD"))]),
    %% -------------------------------------------------------------------------
    %% 1. Source ==> ParseTree
    %% -------------------------------------------------------------------------
    case ?PARSER_MODULE:parsetree_with_tokens(Source) of
        {ok, {ParseTree, Tokens}} ->
            %% -----------------------------------------------------------------
            %% Test TopDown
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source_TD
            %% -----------------------------------------------------------------
            Source_TD = case ?PARSER_MODULE:pt_to_string(ParseTree) of
                            {error, Error_1_TD} ->
                                io:format(user, "~n" ++ ?MODULE_STRING ++
                                    " : [TD] Error ParseTree ==> Source_TD : Error    ~n > ~p",
                                    [Error_1_TD]),
                                io:format(user, "~n" ++ ?MODULE_STRING ++
                                    " : [TD] Error ParseTree ==> Source_TD : Source   ~n > ~p",
                                    [Source]),
                                io:format(user, "~n" ++ ?MODULE_STRING ++
                                    " : [TD] Error ParseTree ==> Source_TD : ParseTree~n > ~p",
                                    [ParseTree]),
                                throw("[TD] Error ParseTree ==> Source_TD");
                            NS_TD ->
                                NS_TD
                        end,
            %% -----------------------------------------------------------------
            %% 3. Source_TD ==> ParseTree_TD
            %% -----------------------------------------------------------------
            {ok, {ParseTree_TD, Tokens_TD}}
                = try
                      case ?PARSER_MODULE:parsetree_with_tokens(Source_TD) of
                          {ok, RT_TD} -> {ok, RT_TD};
                          Error_2_TD -> throw(Error_2_TD)
                      end
                  catch
                      Exception_TD:Reason_TD ->
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Exception~n > ~p",
                              [Exception_TD]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Reason   ~n > ~p",
                              [Reason_TD]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Source   ~n > ~p",
                              [Source]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Source_TD~n > ~p",
                              [Source_TD])
                  end,
            if ParseTree /= ParseTree_TD ->
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Source      ~n > ~p",
                    [Source]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Source_TD   ~n > ~p",
                    [Source_TD]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : ParseTree   ~n > ~p",
                    [ParseTree]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : ParseTree_TD~n > ~p",
                    [ParseTree_TD]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Tokens      ~n > ~p",
                    [Tokens]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Tokens_TD   ~n > ~p",
                    [Tokens_TD]),
                throw("[TD] Error ParseTree /= ParseTree_TD");
                true -> ok
            end,
            ?assertEqual(ParseTree, ParseTree_TD),
            Source_TD_String = binary:bin_to_list(Source_TD),
            Source_TD_MultipleSpace = string:str(Source_TD_String, "  "),
            case Source_TD_MultipleSpace of
                0 -> ok;
                _ ->
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [TD] Error redundant whitespace(s) : 1. Redundant WS~n > ~p",
                        [Source_TD_MultipleSpace]),
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [TD] Error redundant whitespace(s) : Source         ~n > ~p",
                        [Source]),
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [TD] Error redundant whitespace(s) : Source_TD      ~n > ~p",
                        [Source_TD_String]),
                    throw("[TD] Error redundant whitespace(s)")
            end,
            %% -----------------------------------------------------------------
            %% Test BottomUp
            %% -----------------------------------------------------------------
            %% 4. ParseTree ==> Source_BU
            %% -----------------------------------------------------------------
            Source_BU = case ?PARSER_MODULE:pt_to_string_bu(ParseTree) of
                            {error, Error_1_BU} ->
                                io:format(user, "~n" ++ ?MODULE_STRING ++
                                    " : [BU] Error ParseTree ==> Source_BU : Error    ~n > ~p",
                                    [Error_1_BU]),
                                io:format(user, "~n" ++ ?MODULE_STRING ++
                                    " : [BU] Error ParseTree ==> Source_BU : Source   ~n > ~p",
                                    [Source]),
                                io:format(user, "~n" ++ ?MODULE_STRING ++
                                    " : [BU] Error ParseTree ==> Source_BU : ParseTree~n > ~p",
                                    [ParseTree]),
                                throw("[BU] Error ParseTree ==> Source_BU");
                            NS_BU ->
                                NS_BU
                        end,
            %% -----------------------------------------------------------------
            %% 5. Source_BU ==> ParseTree_BU
            %% -----------------------------------------------------------------
            {ok, {ParseTree_BU, Tokens_BU}}
                = try
                      case ?PARSER_MODULE:parsetree_with_tokens(Source_BU) of
                          {ok, RT_BU} -> {ok, RT_BU};
                          Error_2_BU -> throw(Error_2_BU)
                      end
                  catch
                      Exception_BU:Reason_BU ->
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [BU] Error Source_BU ==> ParseTree_BU : Exception~n > ~p",
                              [Exception_BU]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [BU] Error Source_BU ==> ParseTree_BU : Reason   ~n > ~p",
                              [Reason_BU]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [BU] Error Source_BU ==> ParseTree_BU : Source   ~n > ~p",
                              [Source]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [BU] Error Source_BU ==> ParseTree_BU : Source_BU~n > ~p",
                              [Source_BU])
                  end,
            if ParseTree /= ParseTree_BU ->
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [BU] Error ParseTree /= ParseTree_BU : Source      ~n > ~p",
                    [Source]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [BU] Error ParseTree /= ParseTree_BU : Source_BU   ~n > ~p",
                    [Source_BU]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [BU] Error ParseTree /= ParseTree_BU : ParseTree   ~n > ~p",
                    [ParseTree]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [BU] Error ParseTree /= ParseTree_BU : ParseTree_BU~n > ~p",
                    [ParseTree_BU]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [BU] Error ParseTree /= ParseTree_BU : Tokens      ~n > ~p",
                    [Tokens]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [BU] Error ParseTree /= ParseTree_BU : Tokens_BU   ~n > ~p",
                    [Tokens_BU]),
                throw("[BU] Error ParseTree /= ParseTree_BU");
                true -> ok
            end,
            ?assertEqual(ParseTree, ParseTree_BU),
            Source_BU_String = binary:bin_to_list(Source_BU),
            Source_BU_MultipleSpace = string:str(Source_BU_String, "  "),
            case Source_BU_MultipleSpace of
                0 -> ok;
                _ ->
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [BU] Error redundant whitespace(s) : 1. Redundant WS~n > ~p",
                        [Source_BU_MultipleSpace]),
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [BU] Error redundant whitespace(s) : Source         ~n > ~p",
                        [Source]),
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [BU] Error redundant whitespace(s) : Source_BU      ~n > ~p",
                        [Source_BU_String]),
                    throw("[BU] Error redundant whitespace(s)")
            end,
            %% -----------------------------------------------------------------
            %% Test FORMAT
            %% -----------------------------------------------------------------
            %% 6. ParseTree ==> Source_FORMAT
            %% -----------------------------------------------------------------
            Source_FORMAT =
                case ?PARSER_MODULE:pt_to_string_format(ParseTree) of
                    {error, Error_1_FORMAT} ->
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [FORMAT] Error ParseTree ==> Source_FORMAT : Error    ~n > ~p",
                            [Error_1_FORMAT]),
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [FORMAT] Error ParseTree ==> Source_FORMAT : Source   ~n > ~p",
                            [Source]),
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [FORMAT] Error ParseTree ==> Source_FORMAT : ParseTree~n > ~p",
                            [ParseTree]),
                        throw({error, Error_1_FORMAT});
                    NS_FORMAT ->
                        NS_FORMAT
                end,
            %% -----------------------------------------------------------------
            %% 7. Source_FORMAT ==> ParseTree_FORMAT
            %% -----------------------------------------------------------------
            {ok, {ParseTree_FORMAT, Tokens_FORMAT}}
                = try
                      case ?PARSER_MODULE:parsetree_with_tokens(
                          Source_FORMAT) of
                          {ok, RT_FORMAT} -> {ok, RT_FORMAT};
                          Error_2_FORMAT -> throw(Error_2_FORMAT)
                      end
                  catch
                      Exception_FORMAT:Reason_FORMAT ->
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Exception    ~n > ~p",
                              [Exception_FORMAT]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Reason       ~n > ~p",
                              [Reason_FORMAT]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Source       ~n > ~p",
                              [Source]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Source_FORMAT~n > ~p",
                              [Source_FORMAT])
                  end,
            ParseTreeLower = string:casefold(io_lib:format("~p", [ParseTree])),
            ParseTree_FORMATLower =
                string:casefold(io_lib:format("~p", [ParseTree_FORMAT])),
            if ParseTreeLower /= ParseTree_FORMATLower ->
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Source          ~n > ~p",
                    [Source]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Source_FORMAT   ~n > ~p",
                    [Source_FORMAT]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : ParseTree       ~n > ~p",
                    [ParseTree]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : ParseTree       ~n > ~p",
                    [ParseTreeLower]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : ParseTree_FORMAT~n > ~p",
                    [ParseTree_FORMATLower]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Tokens          ~n > ~p",
                    [Tokens]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Tokens_FORMAT   ~n > ~p",
                    [Tokens_FORMAT]),
                throw("[FORMAT] Error ParseTree /= ParseTree_FORMAT");
                true -> ok
            end,
            ?assertEqual(ParseTreeLower, ParseTree_FORMATLower),
            {ok, Source_FORMAT};
        {lex_error, _Error} ->
            io:format(user,
                "~n" ++ ?MODULE_STRING ++ " : Failed lex_error : Source~n > ~p",
                [Source]),
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed lex_error : Error    ~n > ~p",
                [_Error]),
            throw({error, "Failed lex_error"});
        {parse_error, {_Error, Tokens}} ->
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed parse_error : Source~n > ~p",
                [Source]),
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed parse_error : Tokens   ~n > ~p",
                [Tokens]),
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed parse_error : Error    ~n > ~p",
                [_Error]),
            throw({error, "Failed parse_error"})
    end.
