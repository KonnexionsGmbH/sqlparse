%% -----------------------------------------------------------------------------
%%
%% sqlparse_test_utils.erl: SQL - test driver utilities.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-module(sqlparse_test_utils).

-export([eunit_test/1, eunit_test/2]).

-define(NODEBUG, true).

-include_lib("eunit/include/eunit.hrl").

-include("sqlparse.hrl").
-include("sqlparse_test.hrl").

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test(Source) -> eunit_test(Source, []).

eunit_test(Source, LOpts) ->
  ?D("Start ~nSource: ~p~n LOpts: ~p~n", [Source, LOpts]),
  %% -------------------------------------------------------------------------
  %% 1. Source ==> ParseTree
  %% -------------------------------------------------------------------------
  case ?PARSER_MODULE:parsetree_with_tokens(Source) of
    {ok, {ParseTree, Tokens}} ->
      ?D("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, Tokens]),
      %% -----------------------------------------------------------------
      %% Test TopDown
      %% -----------------------------------------------------------------
      %% 2. ParseTree ==> Source_TD
      %% -----------------------------------------------------------------
      Source_TD =
        case sqlparse_fold:top_down(sqlparse_format_flat, ParseTree, []) of
          {error, Error_1_TD} ->
            io:format(
              user,
              "~n" ++ ?MODULE_STRING ++ " : [TD] Error ParseTree ==> Source_TD : Error    ~n > ~p",
              [Error_1_TD]
            ),
            io:format(
              user,
              "~n" ++ ?MODULE_STRING ++ " : [TD] Error ParseTree ==> Source_TD : Source   ~n > ~p",
              [Source]
            ),
            io:format(
              user,
              "~n" ++ ?MODULE_STRING ++ " : [TD] Error ParseTree ==> Source_TD : ParseTree~n > ~p",
              [ParseTree]
            ),
            throw("[TD] Error ParseTree ==> Source_TD");

          NS_TD -> binary_to_list(NS_TD)
        end,
      %% -----------------------------------------------------------------
      %% 3. Source_TD ==> ParseTree_TD
      %% -----------------------------------------------------------------
      {ok, {ParseTree_TD, Tokens_TD}} =
        try
          case ?PARSER_MODULE:parsetree_with_tokens(Source_TD) of
            {ok, RT_TD} -> {ok, RT_TD};
            Error_2_TD -> throw(Error_2_TD)
          end
        catch
          Exception_TD:Reason_TD ->
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD] Error Source_TD ==> ParseTree_TD : Exception~n > ~p",
              [Exception_TD]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD] Error Source_TD ==> ParseTree_TD : Reason   ~n > ~p",
              [Reason_TD]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD] Error Source_TD ==> ParseTree_TD : Source   ~n > ~p",
              [Source]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD] Error Source_TD ==> ParseTree_TD : Source_TD~n > ~p",
              [Source_TD]
            )
        end,
      %% -----------------------------------------------------------------
      %% 4. ParseTree == ParseTree_TD ?
      %% -----------------------------------------------------------------
      if
        ParseTree /= ParseTree_TD ->
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error ParseTree /= ParseTree_TD : Source      ~n > ~p",
            [Source]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error ParseTree /= ParseTree_TD : Source_TD   ~n > ~p",
            [Source_TD]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error ParseTree /= ParseTree_TD : ParseTree   ~n > ~p",
            [ParseTree]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error ParseTree /= ParseTree_TD : ParseTree_TD~n > ~p",
            [ParseTree_TD]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error ParseTree /= ParseTree_TD : Tokens      ~n > ~p",
            [Tokens]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error ParseTree /= ParseTree_TD : Tokens_TD   ~n > ~p",
            [Tokens_TD]
          ),
          throw("[TD] Error ParseTree /= ParseTree_TD");

        true -> ok
      end,
      ?assertEqual(ParseTree, ParseTree_TD),
      %% -----------------------------------------------------------------
      %% 5. No redundant whitespaces.
      %% -----------------------------------------------------------------
      Source_TD_MultipleSpace = string:str(Source_TD, "  "),
      case Source_TD_MultipleSpace of
        0 -> ok;

        _ ->
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error redundant whitespace(s) : 1. Redundant WS~n > ~p",
            [Source_TD_MultipleSpace]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error redundant whitespace(s) : Source         ~n > ~p",
            [Source]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING ++ " : [TD] Error redundant whitespace(s) : Source_TD      ~n > ~p",
            [Source_TD]
          ),
          throw("[TD] Error redundant whitespace(s)")
      end,
      %% -----------------------------------------------------------------
      %% Test TopDown == BottomUp
      %% -----------------------------------------------------------------
      %% 6. ParseTree ==> Source_Check_TD
      %% -----------------------------------------------------------------
      Source_Check_TD =
        case sqlparse_fold:top_down(sqlparse_check_td_vs_bu, ParseTree, top_down) of
          {error, Error_Check_TD} ->
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD==BU] Error ParseTree ==> Check_TD : Error    ~n > ~p",
              [Error_Check_TD]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD==BU] Error ParseTree ==> Check_TD : Source   ~n > ~p",
              [Source]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD==BU] Error ParseTree ==> Check_TD : ParseTree~n > ~p",
              [ParseTree]
            ),
            throw("[TD==BU] Error ParseTree ==> Check_TD");

          NS_Check_TD -> NS_Check_TD
        end,
      %% -----------------------------------------------------------------
      %% 7 ParseTree ==> Source_Check_BU
      %% -----------------------------------------------------------------
      Source_Check_BU =
        case sqlparse_fold:bottom_up(sqlparse_check_td_vs_bu, ParseTree, bottom_up) of
          {error, Error_Check_BU} ->
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD==BU] Error ParseTree ==> Check_BU : Error    ~n > ~p",
              [Error_Check_BU]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD==BU] Error ParseTree ==> Check_BU : Source   ~n > ~p",
              [Source]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [TD==BU] Error ParseTree ==> Check_BU : ParseTree~n > ~p",
              [ParseTree]
            ),
            throw("[TD==BU] Error ParseTree ==> Check_BU");

          NS_Check_BU -> NS_Check_BU
        end,
      %% -----------------------------------------------------------------
      %% 8. Source_Check_TD == Source_Check_BU ?
      %% -----------------------------------------------------------------
      if
        Source_Check_TD /= Source_Check_BU ->
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [TD==BU] Error Source_Check_TD /= Source_Check_BU : Source  ~n > ~p",
            [Source]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [TD==BU] Error Source_Check_TD /= Source_Check_BU : Check_TD~n > ~p",
            [Source_Check_TD]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [TD==BU] Error Source_Check_TD /= Source_Check_BU : Check_BU~n > ~p",
            [Source_Check_BU]
          ),
          throw("[TD==BU] Error Source_Check_TD /= Source_Check_BU");

        true -> ok
      end,
      ?assertEqual(Source_Check_TD, Source_Check_BU),
      %% -----------------------------------------------------------------
      %% Test FORMAT
      %% -----------------------------------------------------------------
      %% 9. ParseTree ==> Source_FORMAT
      %% -----------------------------------------------------------------
      Source_FORMAT =
        case sqlparse_fold:top_down(sqlparse_format_pretty, ParseTree, LOpts) of
          {error, Error_1_FORMAT} ->
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [FORMAT] Error ParseTree ==> Source_FORMAT : Error    ~n > ~p",
              [Error_1_FORMAT]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [FORMAT] Error ParseTree ==> Source_FORMAT : Source   ~n > ~p",
              [Source]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING ++ " : [FORMAT] Error ParseTree ==> Source_FORMAT : ParseTree~n > ~p",
              [ParseTree]
            ),
            throw({error, Error_1_FORMAT});

          NS_FORMAT -> NS_FORMAT
        end,
      %% -----------------------------------------------------------------
      %% 10. Source_FORMAT ==> ParseTree_FORMAT
      %% -----------------------------------------------------------------
      {ok, {ParseTree_FORMAT, Tokens_FORMAT}} =
        try
          case ?PARSER_MODULE:parsetree_with_tokens(Source_FORMAT) of
            {ok, RT_FORMAT} -> {ok, RT_FORMAT};
            Error_2_FORMAT -> throw(Error_2_FORMAT)
          end
        catch
          Exception_FORMAT:Reason_FORMAT ->
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING
              ++
              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Exception    ~n > ~p",
              [Exception_FORMAT]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING
              ++
              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Reason       ~n > ~p",
              [Reason_FORMAT]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING
              ++
              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Source       ~n > ~p",
              [Source]
            ),
            io:format(
              user,
              "~n"
              ++
              ?MODULE_STRING
              ++
              " : [FORMAT] Error Source_FORMAT ==> ParseTree_FORMAT : Source_FORMAT~n > ~p",
              [Source_FORMAT]
            )
        end,
      ParseTreeLower = string:casefold(io_lib:format("~p", [ParseTree])),
      ParseTree_FORMATLower = string:casefold(io_lib:format("~p", [ParseTree_FORMAT])),
      if
        ParseTreeLower /= ParseTree_FORMATLower ->
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Source          ~n > ~p",
            [Source]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Source_FORMAT   ~n > ~p",
            [Source_FORMAT]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : ParseTree       ~n > ~p",
            [ParseTree]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : ParseTree       ~n > ~p",
            [ParseTreeLower]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : ParseTree_FORMAT~n > ~p",
            [ParseTree_FORMATLower]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Tokens          ~n > ~p",
            [Tokens]
          ),
          io:format(
            user,
            "~n"
            ++
            ?MODULE_STRING
            ++
            " : [FORMAT] Error ParseTree /= ParseTree_FORMAT : Tokens_FORMAT   ~n > ~p",
            [Tokens_FORMAT]
          ),
          throw("[FORMAT] Error ParseTree /= ParseTree_FORMAT");

        true -> ok
      end,
      ?assertEqual(ParseTreeLower, ParseTree_FORMATLower),
      {ok, Source_FORMAT};

    {lex_error, _Error} ->
      io:format(user, "~n" ++ ?MODULE_STRING ++ " : Failed lex_error : Source~n > ~p", [Source]),
      io:format(user, "~n" ++ ?MODULE_STRING ++ " : Failed lex_error : Error    ~n > ~p", [_Error]),
      throw({error, "Failed lex_error"});

    {parse_error, {_Error, Tokens}} ->
      io:format(user, "~n" ++ ?MODULE_STRING ++ " : Failed parse_error : Source~n > ~p", [Source]),
      io:format(
        user,
        "~n" ++ ?MODULE_STRING ++ " : Failed parse_error : Tokens   ~n > ~p",
        [Tokens]
      ),
      io:format(
        user,
        "~n" ++ ?MODULE_STRING ++ " : Failed parse_error : Error    ~n > ~p",
        [_Error]
      ),
      throw({error, "Failed parse_error"})
  end.
