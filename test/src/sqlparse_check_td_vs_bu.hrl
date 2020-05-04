%% -----------------------------------------------------------------------------
%%
%% sqlparse_check_td_vs_bu.hrl: SQL - test driver top-down versus bottom-up
%%                                    parsing.
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
