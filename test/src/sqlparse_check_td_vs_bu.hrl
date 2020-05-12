%% -----------------------------------------------------------------------------
%%
%% sqlparse_check_td_vs_bu.hrl: SQL - test driver top-down versus bottom-up
%%                                    parsing.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(SQLPARSE_CHECK_TD_VS_BU_HRL).

-define(SQLPARSE_CHECK_TD_VS_BU_HRL, true).

-include("sqlparse_fold.hrl").

-define(
  LAYOUT_RESULT_CHECK(Ctx, Rule, RT),
  CtxOut =
    case RT of
      none -> Ctx;
      _ -> Ctx ++ [erlang:insert_element(1, RT, atom_to_list(Rule))]
    end,
  ?D("Processed~n CtxIn: ~p~n CtxNew: ~p~n CtxOut: ~p~n", [Ctx, RT, CtxOut]),
  CtxOut
).
-endif.
