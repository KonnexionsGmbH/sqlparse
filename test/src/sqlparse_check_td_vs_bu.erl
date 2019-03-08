%% -----------------------------------------------------------------------------
%%
%% sqlparse_check_td_vs_bu.erl: SQL - test driver top-down versus bottom-up
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

-module(sqlparse_check_td_vs_bu).

-export([
    finalize/2,
    fold/5,
    init/1
]).

-define(NODEBUG, true).

-include("sqlparse_check_td_vs_bu.hrl").

-type layout_opts() :: atom().

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting up optional parameters.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(LOpts :: layout_opts()) -> LOpts :: layout_opts().
init(LOpts) ->
    ?D("Start~n LOpts: ~p~n", [LOpts]),
    RT = case LOpts of
             bottom_up -> LOpts;
             _ -> top_down
         end,
    ?D("~n LOpts: ~p~n", [RT]),
    RT.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Postprocessing of the resulting SQL statement.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec finalize(LOpts :: layout_opts(), list()|tuple()) -> string()|tuple().
finalize(_LOpts, Ctx) ->
    ?D("Start~n LOpts: ~p~n CtxOut: ~p~n", [_LOpts, Ctx]),
    RT = lists:flatten([lists:append(
        ["(", lists:join(",", tuple_to_list(Token)), ")"]) || Token <- Ctx]),
    ?D("~n CtxOut: ~p~n", [RT]),
    RT.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Layout methods for processing the various parser subtrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ( & base_table_element_commalist & cols & column_commalist & create_index_spec
% & fun_arg_commalist & ref_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fold(LOpts :: layout_opts(), FunState :: tuple(), Ctx :: tuple(),
    PTree :: list()|tuple(), FoldState :: tuple()) ->
    Ctx :: tuple()|none().
fold(_LOpts, _FunState, Ctx, _PTree, {Rule, _Step} = _FoldState)
    when Rule == "(";Rule == base_table_element_commalist;Rule == cols;Rule ==
    column_commalist;Rule == create_index_spec;Rule == fun_arg_commalist;Rule ==
             ref_commalist ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_op
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Op, AnyAllSome, _SubQuery} = _PTree,
    {all_or_any_op = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op), atom_to_list(AnyAllSome)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {_Op, ScalarExp, _} = _PTree,
    {all_or_any_predicate = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alter_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'alter user', User, _Spec} = _PTree,
    {alter_user_def = Rule, Step} = _FoldState)
    when is_binary(User) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"alter user", binary_to_list(User)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'alter user', Users, _Spec} = _PTree,
    {alter_user_def = Rule, Step} = _FoldState)
    when is_list(Users) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"alter user"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% anchor
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Anchor, _Bracket} = _PTree,
    {anchor = Rule, Step} = _FoldState)
    when is_binary(Anchor) ->
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Anchor)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% as
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Value, Alias} = _PTree, {Rule, Step} =
    _FoldState)
    when
    (Rule == as orelse Rule == explicit_as) andalso is_binary(Value) andalso
        is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value), binary_to_list(Alias)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {_Value, Alias} = _PTree, {Rule, Step} =
    _FoldState)
    when (Rule == as orelse Rule == explicit_as) andalso is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(Alias)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assignment
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'=', Column, _ScalarOptAsExp} = _PTree,
    {assignment = Rule, Step, _Pos} = _FoldState)
    when is_binary(Column) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Column), "="};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_and
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, ScalarExp = _PTree, {between_and = Rule, Step} =
    _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"and", binary_to_list(ScalarExp)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {between_and = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"and"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_between
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, ScalarExp = _PTree,
    {between_between = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"between", binary_to_list(ScalarExp)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {between_between = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"between"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold(LOpts, _FunState, Ctx, {between, ScalarExp1, _ScalarExp2, _ScalarExp3} =
    _PTree, {between_predicate = Rule, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp1)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% binary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, Op = _PTree, {binary = Rule, Step} = _FoldState)
    when Op == 'and'; Op == 'or'; Op == '+'; Op == '-'; Op == '*'; Op == '/';
         Op == 'div'; Op == '||'; Op == '='; Op == '!='; Op == '^='; Op == '<>';
         Op == '<'; Op == '>'; Op == '<='; Op == '>='; Op == ':=' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, Value1, Value2} = _PTree,
    {binary = Rule, Step} = _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value1) andalso is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value1), atom_to_list(Op), binary_to_list(
                     Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, Value1, Value2} = _PTree,
    {binary = Rule, Step} = _FoldState)
    when is_atom(Op) andalso is_binary(Value1), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value1), atom_to_list(Op), binary_to_list(
                     Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, Value1, _Value2} = _PTree,
    {binary = Rule, Step} = _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value1), atom_to_list(Op)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, Value1, _Value2} = _PTree,
    {binary = Rule, Step} = _FoldState)
    when is_atom(Op), is_binary(Value1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value1), atom_to_list(Op)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, _Value1, Value2} = _PTree,
    {binary = Rule, Step} = _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {atom_to_list(Op), binary_to_list(Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, _Value1, Value2} = _PTree,
    {binary = Rule, Step} = _FoldState)
    when is_atom(Op), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {atom_to_list(Op), binary_to_list(Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'case'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {ScalarOptAsExpr, _CaseWhenThenList, Else} =
    _PTree, {'case' = Rule, Step} = _FoldState)
    when is_list(_CaseWhenThenList) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case ScalarOptAsExpr of
                     <<>> -> {"case"};
                     B when is_binary(B) ->
                         {"case", binary_to_list(ScalarOptAsExpr)};
                     _ -> {"case"}
                 end;
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 case is_binary(Else) of
                     true -> {"else", binary_to_list(Else), "end"};
                     _ -> {"end"}
                 end;
             % satisfying the dialyzer
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {check = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% close_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {close, _Cursor} = _PTree,
    {close_statement = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"close"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {column = Rule, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {column, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Column, _DataType, _} = _PTree,
    {column_def = Rule, Step, Pos} = _FoldState)
    when is_binary(Column) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Column)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {column_def_opt = Rule, Step, _Pos} =
    _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {check, _Value} = _PTree,
    {column_def_opt = Rule, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"check"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {default, _Value} = _PTree,
    {column_def_opt = Rule, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"default"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, {ref, _Value} = _PTree,
    {column_def_opt, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% commit_statement & rollback_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {Type = Rule, Step} = _FoldState)
    when Type == commit_statement;Type == rollback_statement ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comparison_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {_Op, {prior, ScalarExp1}, _ScalarExp2} = _PTree,
    {comparison_predicate = Rule, Step} = _FoldState) when is_binary(
    ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"prior", binary_to_list(ScalarExp1)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {_Op, {prior, _ScalarExp1}, _ScalarExp2} = _PTree,
    {comparison_predicate = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"prior"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {_Op, ScalarExp1, {prior, _ScalarExp2}} = _PTree,
    {comparison_predicate = Rule, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp1)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

fold(LOpts, _FunState, Ctx, {Op, {prior, ScalarExp}} = _PTree,
    {comparison_predicate = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op), "prior", binary_to_list(ScalarExp)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, {prior, _ScalarExp}} = _PTree,
    {comparison_predicate = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op), "prior"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, ScalarExp} = _PTree,
    {comparison_predicate = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op), binary_to_list(ScalarExp)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, _ScalarExp} = _PTree,
    {comparison_predicate = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% connect_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'connect by', <<>>, SearchCondition} = _PTree,
    {connect_by = Rule, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"connect by", binary_to_list(SearchCondition)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'connect by', NoCycle, SearchCondition} = _PTree,
    {connect_by = Rule, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"connect by", binary_to_list(NoCycle), binary_to_list(
                     SearchCondition)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'connect by', <<>>, _SearchCondition} = _PTree,
    {connect_by = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"connect by"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'connect by', NoCycle, _SearchCondition} = _PTree,
    {connect_by = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"connect by", binary_to_list(NoCycle)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'create index', CreateIndexFoldState, _IndexName, _TableAlias, _CreateIndexSpec, _CreateIndexNorm, _CreateIndexFilter} =
        _PTree, {create_index_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case CreateIndexFoldState of
                     {} -> {"create"};
                     _ -> {"create", atom_to_list(CreateIndexFoldState)}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_name
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, IndexName = _PTree,
    {create_index_name = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case IndexName of
                     {} -> {"index"};
                     _ -> {"index", binary_to_list(IndexName)}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_filter & create_index_norm
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {_, Value} = _PTree, {Rule, Step} = _FoldState)
    when Rule == filter_with;Rule == norm_with ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Rule), binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_spec_column
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree,
    {create_index_spec_column = Rule, Step, _Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {create_index_spec_column, _Step, _Pos} =
    _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {create_index_table = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case is_binary(PTree) of
                     true -> {"on", binary_to_list(PTree)};
                     _ -> {"on"}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'create role', Value} = _PTree,
    {create_role_def = Rule, Step} = _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"create role", binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {create_table_def = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"create"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {create_table_table = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case is_binary(PTree) of
                     true -> {"table", binary_to_list(PTree)};
                     _ -> {"table"}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'create user', User, _Identified, _UserFoldStateList} = _PTree,
    {create_user_def = Rule, Step} = _FoldState)
    when is_binary(User) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"create user", binary_to_list(User)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cur
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {cur, Cursor} = _PTree, {cur = Rule, Step} =
    _FoldState)
    when is_list(Cursor) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {Cursor};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {cursor_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"cursor"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {cursor_query_spec = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"is"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% data_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {{Value, Prec, Scale}, _DOpts} = _PTree,
    {data_type = Rule, Step} = _FoldState)
    when is_binary(Value), is_binary(Prec), is_binary(Scale) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value), binary_to_list(Prec), binary_to_list(
                     Scale)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {{Value, Prec}, _DOpts} = _PTree,
    {data_type = Rule, Step} = _FoldState)
    when is_binary(Value), is_binary(Prec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value), binary_to_list(Prec)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Value, _DOpts} = _PTree,
    {data_type = Rule, Step} = _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {dblink = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% default
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {default = Rule, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, PTree, {default = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {delete, Table, _, _} = _PTree,
    {delete_statement = Rule, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"delete from", binary_to_list(Table)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {delete_statement = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"delete from"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_cluster_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop cluster', Name, _Table} = _PTree,
    {drop_cluster_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop cluster", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_cluster_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {drop_cluster_extensions = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_context_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop context', Name} = _PTree,
    {drop_context_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop context", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop database'} = _PTree,
    {drop_database_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop database"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop database link', Name, {}} = _PTree,
    {drop_database_link_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop database link", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_public_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop database link', Name, public} = _PTree,
    {drop_database_link_public_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop public database link", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_directory_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop directory', Name} = _PTree,
    {drop_directory_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop directory", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_function_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop function', Name} = _PTree,
    {drop_function_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop function", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'drop index', IndexName, _Table, _DropIndexExtensions} = _PTree,
    {drop_index_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case IndexName of
                     {} -> {"drop index"};
                     _ -> {"drop index", binary_to_list(IndexName)}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {drop_index_extensions = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {drop_index_from = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case is_binary(PTree) of
                     true -> {"from", binary_to_list(PTree)};
                     _ -> {"from"}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_materialized_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop materialized view', Name, _Table} = _PTree,
    {drop_materialized_view_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop materialized view", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_materialized_view_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree,
    {drop_materialized_view_extensions = Rule, Step} =
        _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop package', Name, {}} = _PTree,
    {drop_package_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop package", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop package', Name, body} = _PTree,
    {drop_package_body_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop package body", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_procedure_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop procedure', Name} = _PTree,
    {drop_procedure_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop procedure", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_profile_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop profile', Name, _Table} = _PTree,
    {drop_profile_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop profile", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_profile_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {drop_profile_extensions = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop role', Role} = _PTree,
    {drop_role_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop role", binary_to_list(Role)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_sequence_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop sequence', Name} = _PTree,
    {drop_sequence_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop sequence", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop synonym', Name, {}, _DropSynonymExtensions} =
    _PTree, {drop_synonym_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop synonym", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {drop_synonym_extensions = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_public_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'drop synonym', Name, public, _DropSynonymExtensions} = _PTree,
    {drop_synonym_public_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop public synonym", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'drop table', _Tables, _Exists, _RestrictCascade, DropOpt} = _PTree,
    {drop_table_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case DropOpt of
                     [] -> {"drop"};
                     _ -> {"drop", DropOpt}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {drop_table_extensions = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_trigger_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop trigger', Name} = _PTree,
    {drop_trigger_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop trigger", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop type body', Name} = _PTree,
    {drop_type_body_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop type body", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'drop user', User, []} = _PTree,
    {drop_user_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop user", binary_to_list(User)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'drop user', User, [Cascade]} = _PTree,
    {drop_user_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"drop user", binary_to_list(User), atom_to_list(Cascade)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% else
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {else = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"else"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% existence_test
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {existence_test = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"exists"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exists
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {exists = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case PTree of
                     {} ->
                         {"table"};
                     _ ->
                         {"table if exists"}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fetch_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {fetch_statement = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"fetch"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Table, Join} = _PTree,
    {from = Rule, Step, _Pos} = _FoldState)
    when is_binary(Table), is_list(Join) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Table)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, PTree, {from = Rule, Step, _Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {from, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from (list)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {from = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"from"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun_arg
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {as, {'fun', _, _}, Alias} = _PTree,
    {fun_arg = Rule, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(Alias)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Type, Value} = _PTree,
    {fun_arg = Rule, Step, _Pos} = _FoldState)
    when (Type == all orelse Type == distinct) andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Type), binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Type, _Value} = _PTree,
    {fun_arg = Rule, Step, _Pos} = _FoldState)
    when Type == all; Type == distinct ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Type)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

fold(LOpts, _FunState, Ctx, PTree, {fun_arg = Rule, Step, _Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function_ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'fun', Name, _} = _PTree,
    {function_ref = Rule, Step} =
        _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {as, {{'fun', _, _}, JSON, []}, Alias} = _PTree,
    {function_ref = Rule, Step} = _FoldState)
    when is_tuple(JSON) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(Alias)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% goto
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {goto = Rule, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"goto", PTree};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'grant connect' & 'revoke connect'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Value1, Value2} = _PTree, {Rule, Step} =
    _FoldState)
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(Value1) andalso is_atom(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Rule), "through", atom_to_list(
                     Value1), atom_to_list(Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Rule), "through", atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {{_Type, _Roles}, ProxyAuthReq} = _PTree,
    {Rule, Step} = _FoldState)
    when Rule == 'grant connect'; Rule == 'revoke connect' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Rule), "through"};
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {atom_to_list(ProxyAuthReq)};
             % satisfying the dialyzer
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {_Type, _Roles} = _PTree, {Rule, Step} = _FoldState)
    when Rule == 'grant connect'; Rule == 'revoke connect' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Rule), "through"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grant_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {grant_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"grant"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {grantee = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"to"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee_revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {grantee_revokee = Rule, Step, Pos} =
    _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, PTree, {grantee_revokee = Rule, Step, Pos} =
    _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'identified by', Name, Password} = _PTree,
    {grantee_revokee = Rule, Step} = _FoldState)
    when is_binary(Name);is_binary(Password) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Name), "identified by", binary_to_list(
                     Password)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% group_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {group_by = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"group by"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% having
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {having = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"having", binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {having = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"having"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hints
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {hints = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% identified & spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified by' andalso is_binary(Value) andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Type), binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified extern' andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case is_binary(Value) of
                     true -> {atom_to_list(
                         'identified externally'), "as", binary_to_list(Value)};
                     _ -> {atom_to_list('identified externally')}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified globally' andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 case is_binary(Value) of
                     true -> {atom_to_list(Type), "as", binary_to_list(Value)};
                     _ -> {atom_to_list(Type)}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in_in
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {in_in = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"in"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {in, ScalarExp, _} = _PTree,
    {in_predicate = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {insert, Table, _, _, _} = _PTree,
    {insert_statement = Rule, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"insert into", binary_to_list(Table)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {insert_statement = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"insert into"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% into
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {into = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"into"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% join
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {JoinType, JoinRef, _JoinOnOrUsingClause} = _PTree,
    {join = Rule, Step, _Pos} = _FoldState)
    when is_binary(JoinRef) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(JoinType), binary_to_list(JoinRef)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {JoinType, JoinRef} = _PTree,
    {join = Rule, Step, _Pos} = _FoldState)
    when is_binary(JoinRef) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(JoinType), binary_to_list(JoinRef)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {JoinType, _JoinRef, _JoinOnOrUsingClause} = _PTree,
    {join = Rule, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(JoinType)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {JoinType, _JoinRef} = _PTree,
    {join = Rule, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(JoinType)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% jpparse
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {jpparse = Rule, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {PTree};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {keyword = Rule, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {PTree};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword & privilege
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {Rule, Step, Pos} = _FoldState)
    when (Rule == keyword orelse Rule == privilege) andalso is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword & with_grant_option & with_revoke_option
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == keyword orelse Rule == with_grant_option orelse
    Rule == with_revoke_option) andalso is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_escape
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {like_escape = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"escape", binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {like_escape = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"escape"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_like
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {like_like = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"like", binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {like_like = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"like"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {like, ScalarExp1, _ScalarExp2, _ScalarExp3} =
    _PTree,
    {like_predicate = Rule, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp1)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% materialized
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'materialized view log' = Value1, Value2} = _PTree,
    {materialized = Rule, Step} = _FoldState)
    when is_atom(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Value2), atom_to_list(Value1)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {on = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"on", binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {on = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"on"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on_obj_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Target, Value} = _PTree,
    {on_obj_clause = Rule, Step} = _FoldState)
    when (Target == on orelse Target == 'on directory') andalso
             is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Target), binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Target, _Value} = _PTree,
    {on_obj_clause = Rule, Step} = _FoldState)
    when (Target == on orelse Target == 'on directory') ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Target)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% open_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {open, _Cursor} = _PTree,
    {open_statement = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"open"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {opt = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% order_by_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {order_by_clause = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"order by"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ordering_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {ordering_spec, _Step, _Pos} =
    _FoldState) ->
    Ctx;

fold(LOpts, _FunState, Ctx, {ScalarExp, <<>>} = _PTree,
    {ordering_spec = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {ScalarExp, AscDesc} = _PTree,
    {ordering_spec = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp)};
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(AscDesc)};
             % satisfying the dialyzer
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, {_ScalarExp, <<>>} = _PTree,
    {ordering_spec, _Step} = _FoldState) ->
    Ctx;
fold(LOpts, _FunState, Ctx, {_ScalarExp, AscDesc} = _PTree,
    {ordering_spec = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(AscDesc)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param param
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {indicator, {param = Rule, Value1}, {param = Rule, Value2}} = _PTree,
    {param = Rule, Step} = _FoldState)
    when is_binary(Value1), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value1), "indicator", binary_to_list(Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

fold(LOpts, _FunState, Ctx, {{param = Rule, Value1}, {param = Rule, Value2}} =
    _PTree, {param = Rule, Step} = _FoldState)
    when is_binary(Value1), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value1), binary_to_list(Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param & table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == param orelse Rule == table) andalso is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partition_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {partition_by = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"partition by"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsql_body
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {plsql_body, _StatementPragmaList} = _PTree,
    {plsql_body = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"begin"};
             _ -> {"end"}
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure_call
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {procedure_call, _Step, _Pos} =
    _FoldState) ->
    Ctx;
fold(LOpts, _FunState, Ctx, {'begin procedure', _} = _PTree,
    {procedure_call = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"begin"};
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {"end"};
             % satisfying the dialyzer
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'call procedure', _} = _PTree,
    {procedure_call = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"call"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, {_, _QuerySpec1, _QuerySpec2} = _PTree,
    {query_exp, _Step} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {select, Clauses} = _PTree,
    {query_spec = Rule, Step} = _FoldState)
    when is_list(Clauses) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"select"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quota
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {limited, Number, Unit, Name} = _PTree,
    {quota = Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 case Unit of
                     <<"">> -> {
                         "quota",
                         binary_to_list(Number),
                         "on",
                         binary_to_list(Name)};
                     _ -> {
                         "quota",
                         binary_to_list(Number),
                         binary_to_list(Unit),
                         "on",
                         binary_to_list(Name)}
                 end;
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'unlimited on', Name} = _PTree,
    {quota = Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {"quota unlimited on", binary_to_list(Name)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {ref = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"references", binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Value, _} = _PTree, {ref = Rule, Step} =
    _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"references", binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {ref = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"references"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return & returning
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {_Selection1, _Selection2} = _PTree,
    {Rule, Step} = _FoldState)
    when Rule == return;Rule == returning ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Rule)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {Rule, Step} = _FoldState)
    when Rule == return;Rule == returning ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"into"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revoke_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {revoke_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"revoke"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {revokee = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"from"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role & table & user
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {Rule, Step, Pos} = _FoldState)
    when (Rule == role orelse Rule == table orelse Rule == user) andalso
             is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Type, RoleList} = _PTree,
    {role_list = Rule, Step} = _FoldState)
    when is_atom(Type), is_list(RoleList) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Type)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {scalar_exp = Rule, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {scalar_exp, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_opt_as_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {scalar_opt_as_exp = Rule, Step, Pos} =
    _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {scalar_opt_as_exp, _Step, _Pos} =
    _FoldState) ->
    Ctx;
fold(LOpts, _FunState, Ctx, PTree, {scalar_opt_as_exp = Rule, Step} =
    _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'create schema authorization', Name, _SchemaElementList} = _PTree,
    {schema = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"create schema authorization", Name};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema_element_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {schema_element_list, _Step} =
    _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% select_field
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {select_field = Rule, Step, Pos} =
    _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {select_field, _Step, _Pos} =
    _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {set = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"set"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item & user_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, [{Type, Value}] = _PTree, {Rule, Step, Pos} =
    _FoldState)
    when (Rule == spec_item orelse Rule == user_opt) andalso
             (Type == 'default tablespace' orelse Type == profile orelse
                 Type == 'temporary tablespace') andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {spec_item = Rule, Step, Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Value1, Value2} = _PTree,
    {spec_item = Rule, Step, _Pos} = _FoldState)
    when is_atom(Value1), is_atom(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Value1), atom_to_list(Value2)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {spec_item, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, {_SQL, _Pos, {extra, <<>>}} = _PTree,
    {sql_list, _Step, _Pos} = _FoldState) ->
    Ctx;
fold(LOpts, _FunState, Ctx, {_SQL, Pos, {extra, Extra}} = _PTree,
    {sql_list = Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(Extra)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start_with
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {'start with', SearchCondition} = _PTree,
    {start_with = Rule, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"start with", binary_to_list(SearchCondition)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {start_with = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"start with"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% statement_pragma
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {statement_pragma, _Step, _Pos} =
    _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% statement_pragma_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {statement_pragma_list, _Step} =
    _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% storage
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {storage = Rule, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(PTree), atom_to_list(Rule)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {table, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_coll_expr
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {table_coll_expr, _CollExpr} = _PTree,
    {table_coll_expr = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"table ("};
             _ -> {")"}
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_constraint_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {check, [], _Value} = _PTree,
    {table_constraint_def = Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {"check"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {check, ConstraintName, _Value} = _PTree,
    {table_constraint_def = Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {lists:append([
                     "constraint ",
                     binary_to_list(ConstraintName),
                     " ",
                     "check"
                 ])};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'foreign key' = Type, [], _, _} = _PTree,
    {table_constraint_def = Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Type)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {'foreign key' = Type, ConstraintName, _, _} =
    _PTree,
    {table_constraint_def = Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {lists:append([
                     "constraint ",
                     binary_to_list(ConstraintName),
                     " ",
                     atom_to_list(Type)
                 ])};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Type, [], _Value} = _PTree,
    {table_constraint_def = Rule, Step, Pos} = _FoldState)
    when is_atom(Type) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Type)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Type, ConstraintName, _Value} = _PTree,
    {table_constraint_def = Rule, Step, Pos} = _FoldState)
    when is_atom(Type) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step, Pos} of
             {L, S, _} when L == top_down andalso S == start orelse
                                L == bottom_up andalso S == 'end' ->
                 {lists:append([
                     "constraint ",
                     binary_to_list(ConstraintName),
                     " ",
                     atom_to_list(Type)
                 ])};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {as, Table, Alias, {dblink, _Value}} = _PTree,
    {table_dblink = Rule, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Table)};
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(Alias)};
             % satisfying the dialyzer
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {as, _Table, Alias, {dblink, _Value}} = _PTree,
    {table_dblink = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {binary_to_list(Alias)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

fold(LOpts, _FunState, Ctx, {Table, {dblink, _Value}} = _PTree,
    {table_dblink = Rule, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Table)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% target
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {target = Rule, Step, _Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {target, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tbl_scope & tbl_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {_, Value} = _PTree, {Rule, Step} = _FoldState)
    when (Rule == tbl_scope orelse Rule == tbl_type) andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_for_null
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {is, ScalarExp, <<"null">>} = _PTree,
    {test_for_null = Rule, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {binary_to_list(ScalarExp), "is null"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {test_for_null = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == 'end' orelse
                             L == bottom_up andalso S == start ->
                 {"is null"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% then
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {then = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"then"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'truncate table', Table, _Materialized, _Storage} = _PTree,
    {truncate_table = Rule, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"truncate table", binary_to_list(Table)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx,
    {'truncate table', _Table, _Materialized, _Storage} = _PTree,
    {truncate_table = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"truncate table"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Op, Value} = _PTree,
    {unary = Rule, Step} = _FoldState)
    when is_atom(Op), is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op), binary_to_list(Value)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, {Op, _Value} = _PTree, {unary = Rule, Step} =
    _FoldState)
    when is_atom(Op) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {atom_to_list(Op)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {update, Table, _, _, _} = _PTree,
    {update_statement = Rule, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"update", binary_to_list(Table)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {update_statement = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"update"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, PTree, {user_list, _Step} = _FoldState)
    when is_list(PTree) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_opts_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {user_opts_list, _Step} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {using = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"using"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% values_or_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {values, _} = _PTree,
    {values_or_query_spec = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"values"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(_LOpts, _FunState, Ctx, _PTree, {values_or_query_spec, _Step} =
    _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx,
    {'create view', Table, _ColumnCommalist, _QuerySpec} = _PTree,
    {view_def = Rule, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"create view", binary_to_list(Table)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {view_def = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"create view"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {view_query_spec = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"as"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'when'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {'when' = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"when", binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {'when' = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"when"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% when_not_found
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {when_not_found = Rule, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"whenever not found", atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {when_not_found = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"whenever not found"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% when_sql_err
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {when_sql_err = Rule, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"whenever sqlerror", atom_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {when_sql_err = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"whenever sqlerror"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {where_clause = Rule, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"where", binary_to_list(PTree)};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);
fold(LOpts, _FunState, Ctx, _PTree, {where_clause = Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"where"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_current_of
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {where_current_of = Rule, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {LOpts, Step} of
             {L, S} when L == top_down andalso S == start orelse
                             L == bottom_up andalso S == 'end' ->
                 {"where current of"};
             _ -> none
         end,
    ?LAYOUT_RESULT_CHECK(Ctx, Rule, RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NO ACTION.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {Rule, _Step, _Pos}) when
    Rule == case_when_then;
    Rule == fun_arg;
    Rule == function_ref;
    Rule == join;
    Rule == sql;
    Rule == statement_pragma;
    Rule == statement_pragma_list;
    Rule == user_opt ->
    Ctx;

fold(_LOpts, _FunState, Ctx, _PTree, {Rule, _Step}) when
    Rule == all_or_any_predicate;
    Rule == anchor;
    Rule == assignment_statement;
    Rule == between_predicate;
    Rule == binary;
    Rule == case_when_then;
    Rule == case_when_then_list;
    Rule == check;
    Rule == column_def_list;
    Rule == column_ref_commalist;
    Rule == comparison_predicate;
    Rule == create_opts;
    Rule == default;
    Rule == fields;
    Rule == 'fun';
    Rule == function_ref;
    Rule == function_ref_list_list;
    Rule == hierarchical_query;
    Rule == in_predicate;
    Rule == jpparse;
    Rule == join_list;
    Rule == like_predicate;
    Rule == param;
    Rule == prior;
    Rule == procedure_call;
    Rule == query_spec;
    Rule == query_spec_jpparse;
    Rule == quotas;
    Rule == scalar_exp_commalist;
    Rule == scalar_opt_as_exp;
    Rule == sql_list;
    Rule == sql_list_list;
    Rule == statement_pragma_list;
    Rule == statement_pragma_list_list;
    Rule == table;
    Rule == table_dblink;
    Rule == tables;
    Rule == user_opt;
    Rule == user_opts_list ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, _Ctx, PTree, {Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, _Ctx, PTree, _FoldState),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] error parser subtree not supported [rule=",
        atom_to_list(Rule),
        " / step=",
        atom_to_list(Step),
        " / pos=",
        atom_to_list(Pos),
        "]"
    ]), PTree});
fold(_LOpts, _FunState, _Ctx, PTree, {Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, _Ctx, PTree, _FoldState),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] error parser subtree not supported [rule=",
        atom_to_list(Rule),
        " / step=",
        atom_to_list(Step),
        "]"
    ]), PTree}).
