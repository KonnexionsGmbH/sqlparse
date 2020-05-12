%% -----------------------------------------------------------------------------
%%
%% sqlparse_adhoc_test.erl: SQL - test driver for development purposes.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-module(sqlparse_adhoc_test).

-export([eunit_test_source/1]).

-define(LOPTS, [{case_identifier, init_cap}, {line_break_after, 80}]).
-define(NODEBUG, true).

%% Possible values:
%%  - bottom_up,
%%  - check,
%%  - flat,
%%  - full (= default value),
%%  - pretty,
%%  - top_down

-define(TEST_VERSION, full).

-include_lib("eunit/include/eunit.hrl").

-include("sqlparse.hrl").
-include("sqlparse_test.hrl").

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Determine Files.
%%------------------------------------------------------------------------------

eunit_test_() ->
  ?D("Start~n"),
  {ok, Cwd} = file:get_cwd(),
  RootPath = lists:reverse(filename:split(Cwd)),
  TestDir = filename:join(lists:reverse(["test" | RootPath])),
  TestFile = filename:join(TestDir, ?MODULE_STRING ++ ?ENV_VAR_FILE_TYPE),
  {ok, [Opts | Tests]} = file:consult(TestFile),
  {ok, TestFileBin} = file:read_file(TestFile),
  TestLines =
    [
      begin
        TRe0 =
          re:replace(T, "(.*)(\")(.*)", "\\1\\\\\"\\3", [{return, list}, ungreedy, global, dotall]),
        TRe = list_to_binary(io_lib:format("~p", [TRe0])),
        case binary:match(TestFileBin, TRe) of
          {I1, _} ->
            <<Head:I1/binary, _/binary>> = TestFileBin,
            case re:run(Head, ".*[\r\n]", [global]) of
              {match, Matches} -> length(Matches) + 1;

              nomatch ->
                io:format(
                  user,
                  "~p~n"
                  ">>>>>>>>>>> HEAD ~p <<<<<<<<<<<~n"
                  "Opts ~p~n"
                  "Tests ~p~n"
                  "T ~p~n"
                  "TRe ~p~n",
                  [TestFile, Head, Opts, Tests, T, TRe]
                ),
                error(nomatch)
            end;

          nomatch -> I
        end
      end || {I, T} <- lists:zip(lists:seq(1, length(Tests)), Tests)
    ],
  AugTests = lists:zip(TestLines, Tests),
  tests_gen(AugTests, Opts).

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test_source(Source) ->
  ?D("Start~n Source: ~p~n", [Source]),
  io:format(user, "~n", []),
  io:format(
    user,
    ?MODULE_STRING
    ++
    " : ===========================================>           Test version:~n~ts~n~n",
    [?TEST_VERSION]
  ),
  io:format(user, "~n", []),
  io:format(
    user,
    ?MODULE_STRING
    ++
    " : ===========================================>     Input statement(s):~n~ts~n~n",
    [Source]
  ),
  try {ok, ParseTree} = sqlparse:parsetree(Source), ?D("~n ParseTree: ~p~n", [ParseTree]), case
  ?TEST_VERSION of
    bottom_up ->
      case sqlparse_fold:bottom_up(sqlparse_check_td_vs_bu, ParseTree, bottom_up) of
        {error, _} = BU -> BU;
        BU -> {top_down, BU}
      end;

    check ->
      case sqlparse_fold:top_down(sqlparse_check_td_vs_bu, ParseTree, top_down) of
        {error, _} = TD -> TD;

        TD ->
          case sqlparse_fold:bottom_up(sqlparse_check_td_vs_bu, ParseTree, bottom_up) of
            {error, _} = BU -> BU;

            BU ->
              io:format(user, "~n", []),
              io:format(
                user,
                ?MODULE_STRING
                ++
                " : ------------------------------------------->              Output TD:~n~ts~n~n",
                [TD]
              ),
              io:format(
                user,
                ?MODULE_STRING
                ++
                " : ------------------------------------------->              Output BU:~n~ts~n~n",
                [BU]
              ),
              TD = BU,
              {check, TD, BU}
          end
      end;

    full -> sqlparse_test_utils:eunit_test(Source);
    pretty -> sqlparse_fold:top_down(sqlparse_format_pretty, ParseTree, ?LOPTS);

    top_down ->
      case sqlparse_fold:top_down(sqlparse_check_td_vs_bu, ParseTree, top_down) of
        {error, _} = TD -> TD;
        TD -> {top_down, TD}
      end;

    _ -> binary_to_list(sqlparse_fold:top_down(sqlparse_format_flat, ParseTree, []))
  end of
    {bottom_up, Result} ->
      io:format(user, "~n", []),
      io:format(
        user,
        ?MODULE_STRING
        ++
        " : ------------------------------------------->              Output BU:~n~ts~n~n",
        [Result]
      );

    {check, _ResultTD, _ResultBU} -> ok;

    {error, Reason} ->
      io:format(user, "~n", []),
      io:format(
        user,
        ?MODULE_STRING
        ++
        " : -------------------------------------------> Statement error reason:~n~p~n~n",
        [Reason]
      );

    {ok, Result} ->
      io:format(user, "~n", []),
      io:format(
        user,
        ?MODULE_STRING
        ++
        " : ------------------------------------------->    Output statement(s):~n~ts~n~n",
        [Result]
      );

    {top_down, Result} ->
      io:format(user, "~n", []),
      io:format(
        user,
        ?MODULE_STRING
        ++
        " : ------------------------------------------->              Output TD:~n~ts~n~n",
        [Result]
      );

    Result ->
      io:format(user, "~n", []),
      io:format(
        user,
        ?MODULE_STRING
        ++
        " : ------------------------------------------->    Output statement(s):~n~ts~n~n",
        [Result]
      )
  catch
    error : Reason ->
      io:format(user, "~n", []),
      io:format(
        user,
        ?MODULE_STRING
        ++
        " : -------------------------------------------> Statement catch reason:~n~p~n~n",
        [Reason]
      )
  end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Groups.
%%------------------------------------------------------------------------------

tests_gen(Tests, Opts) ->
  ?D("Start~n Tests: ~p~n Opts: ~p~n", [Tests, Opts]),
  SelTests =
    case proplists:get_value(tests, Opts) of
      St when St =:= undefined; St =:= [] ->
        {Indices, _} = lists:unzip(Tests),
        Indices;

      St -> St
    end,
  tests_gen(Tests, SelTests, []).


tests_gen([], _SelTests, Acc) -> {inorder, lists:reverse(Acc)};

tests_gen([{I, T} | Tests], SelTests, Acc) ->
  case lists:member(I, SelTests) of
    true ->
      tests_gen(
        Tests,
        SelTests,
        [{I, fun () -> {timeout, ?TIMEOUT, ?MODULE:eunit_test_source(T)} end} | Acc]
      );

    _ -> Acc
  end.
