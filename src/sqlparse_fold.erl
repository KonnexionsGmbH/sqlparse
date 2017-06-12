%% -----------------------------------------------------------------------------
%%
%% sqlparse_fold.erl: SQL - utilities.
%%
%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights ResFgloballyerved.
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

-module(sqlparse_fold).

-export([fold/5]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression with parentheses
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Value, "("} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["(", ValueStr, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_ref with JSON
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, [Value1, ".", Value2] = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value2Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Value2),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Value1, ".", Value2Str]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, [Value1, ".", Value2, ".", Value3] = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value3Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Value3),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Value1, ".", Value2, ".", Value3Str]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of parsetrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, [{_, {extra, _}} | _] = STs)
    when is_list(STs) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, STs]),
    NewCtx = case FType of
                 top_down -> Fun(STs, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1}
        = lists:foldl(
        fun(ST, {Sql, AccCtx}) ->
            {NewSql, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl, ST),
            {Sql ++
                if length(Sql) > 0 -> "; " ++ NewSql; true -> NewSql end,
                NewAccCtx}
        end, {"", NewCtx}, STs),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(STs, NewCtx1)
              end,
    RT = {SqlStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Statement list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {stmtList, STs})
    when is_list(STs) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, STs]),
    NewCtx = case FType of
                 top_down -> Fun(STs, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1}
        = lists:foldl(
        fun(ST, {Sql, AccCtx}) ->
            {NewSql, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl, ST),
            {Sql ++
                if length(Sql) > 0 andalso length(NewSql) > 0 ->
                    ";" ++ NewSql; true -> NewSql end,
                NewAccCtx}
        end, {"", NewCtx}, STs),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(STs, NewCtx1)
              end,
    RT = {SqlStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other lists
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, STs)
    when is_list(STs) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, STs]),
    NewCtx = case FType of
                 top_down -> Fun(STs, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1}
        = lists:foldl(
        fun(ST, {Sql, AccCtx}) ->
            {NewSql, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl, ST),
            {Sql ++
                if length(Sql) > 0 andalso length(NewSql) > 0 ->
                    " " ++ NewSql; true -> NewSql end,
                NewAccCtx}
        end, {"", NewCtx}, STs),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(STs, NewCtx1)
              end,
    RT = {SqlStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACCOUNT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {account, LockUnlock} = ST)
    when LockUnlock == 'lock'; LockUnlock == 'unlock' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(LockUnlock, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"account " ++ atom_to_list(LockUnlock), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of aggregate function types 3/4: ALL
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {all, Column} = ST)
    when is_binary(Column) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    RT = {"all " ++ binary_to_list(Column), NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALL
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {all, [Value1, ".", Value2]} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value2Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value2),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["all ", Value1, ".", Value2Str]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {all, [Value1, ".", Value2, ".", Value3]} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value3Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value3),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["all ", Value1, ".", Value2, ".", Value3Str]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALL / ANY / SOME
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {N, Args} = ST)
    when (N == 'any' orelse N == 'all' orelse N == 'some') andalso is_list(Args) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(N, NewCtx),
    {ArgsStr, NewCtx2} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, A),
        {Acc ++ [SubAcc], CtxAcc1}
                                     end,
        {[], NewCtx1},
        Args),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([atom_to_list(N), " ", string:join(ArgsStr, ", ")]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {N, Arg} = ST)
    when N == 'any'; N == 'all'; N == 'some' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(N, NewCtx),
    {ArgStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl, Arg),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([atom_to_list(N), " ", ArgStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALTER USER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'alter user', Usr, {spec, Opts}} = ST)
    when is_binary(Usr) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptsStr, NewCtx1} =
        lists:foldl(
            fun(Opt, {OptsS, INewCtx}) ->
                {OS, INewCtx1} = fold(FType, Fun, INewCtx, Lvl + 1, Opt),
                {OptsS ++ case length(OptsS) == 0 of
                              true -> [];
                              _ -> " "
                          end ++ OS, INewCtx1}
            end, {"", NewCtx}, Opts),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(["alter user ", binary_to_list(Usr), " ", OptsStr]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {'alter user', [Usr | _] = Users, {Grant, GrantArg}} = ST)
    when is_binary(Usr) andalso
    (Grant == 'grant connect' orelse Grant == 'revoke connect') ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    GrantStr = [atom_to_list(Grant), " ", "through", " ",
        if is_atom(GrantArg) -> atom_to_list(GrantArg);
            true ->
                case GrantArg of
                    {W, A} when is_atom(W), is_atom(A) ->
                        [atom_to_list(W), " ", atom_to_list(A)];
                    {W, Roles} when is_atom(W) ->
                        [atom_to_list(W), " ",
                            string:join([binary_to_list(R) || R <- Roles],
                                ",")];
                    {{W, Roles}, Authrec} when is_atom(W) ->
                        [atom_to_list(W), " ",
                            string:join([binary_to_list(R) || R <- Roles],
                                ","), " ", atom_to_list(Authrec)]
                end
        end],
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {lists:flatten(["alter user ", string:join([binary_to_list(U) || U <- Users], ","), " ", GrantStr]), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AS SELECT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'as', {select, _} = QuerySpec, [], "as " = As} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {QuerySpecStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, QuerySpec),
    NewCtx2 = case FType of
                  top_down -> Fun(ST, NewCtx1);
                  bottom_up -> NewCtx1
              end,
    RT = {As ++ QuerySpecStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'as', {select, _} = QuerySpec, " with check option" = Check, "as " = As} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {QuerySpecStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, QuerySpec),
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n QuerySpecStr: ~p~n", [QuerySpecStr]),
    NewCtx2 = case FType of
                  top_down -> Fun(ST, NewCtx1);
                  bottom_up -> NewCtx1
              end,
    RT = {lists:append([As, QuerySpecStr, Check]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All aliases
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {as, A, B, C} = ST)
    when is_binary(A), is_binary(B) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(A, NewCtx),
    NewCtx2 = Fun(B, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten([binary_to_list(A), C, binary_to_list(B)]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {as, A, B, C} = ST)
    when is_tuple(A), is_binary(B) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, A),
    NewCtx2 = Fun(B, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten([AStr, C, binary_to_list(B)]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {as, A, B, C} = ST)
    when is_binary(B) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, A),
    NewCtx2 = Fun(B, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten([AStr, C, binary_to_list(B)]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(_FType, Fun, Ctx, _Lvl, Tab = _ST)
    when is_binary(Tab) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, _ST]),
    RT = {binary_to_list(Tab), Fun(Tab, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% betwen operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, A, B, C} = ST)
    when Type == between; Type == 'not between' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {A1, NewCtx1} = if is_binary(A) -> {binary_to_list(A), NewCtx}; true ->
        fold(FType, Fun, NewCtx, Lvl + 1, A) end,
    {B1, NewCtx2} = if is_binary(B) -> {binary_to_list(B), NewCtx1}; true ->
        fold(FType, Fun, NewCtx1, Lvl + 1, B) end,
    {C1, NewCtx3} = if is_binary(C) -> {binary_to_list(C), NewCtx2}; true ->
        fold(FType, Fun, NewCtx2, Lvl + 1, C) end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([A1, " ", atom_to_list(Type), " ", B1, " and ", C1]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure calls ('call ...')
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'call procedure', Function} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FunctionStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Function),
    NewCtx2 = case FType of
                  top_down -> Fun(ST, NewCtx1);
                  bottom_up -> NewCtx1
              end,
    RT = {"call " ++ FunctionStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'case', Expr, WhenThenList, Else} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExprStr, NewCtx1} = case is_binary(Expr) of
                             true -> {binary_to_list(Expr), NewCtx};
                             _ -> fold(FType, Fun, NewCtx, Lvl + 1, Expr)
                         end,
    {WhenThenStr, NewCtx2}
        = lists:foldl(
        fun({When, Then}, {Sql, AccCtx}) ->
            {WhenStr, AccCtx1} = fold(FType, Fun, AccCtx, Lvl + 1, When),
            {ThenStr, AccCtx2} = fold(FType, Fun, AccCtx1, Lvl + 1, Then),
            {lists:append([Sql, "when ", WhenStr, " then ", ThenStr, " "]), AccCtx2}
        end, {"", NewCtx1}, WhenThenList),
    {ElseStr, NewCtx3} = case Else of
                             {} -> {"", NewCtx2};
                             Else ->
                                 {EStr, NewCtx21} = fold(FType, Fun, NewCtx2, Lvl + 1, Else),
                                 {lists:append(["else ", EStr, " "]), NewCtx21}
                         end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["case ", ExprStr, case length(ExprStr) == 0 of
                                              true -> [];
                                              _ -> " "
                                          end, WhenThenStr, ElseStr, "end"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHECK
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {check, Condition} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ConditionStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Condition),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["check (", ConditionStr, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLOSE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {close, {cur, CurName}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"close " ++ CurName, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECT BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'connect by', NoCycle, ConnectBy} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NoCycleStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NoCycle),
    {ConnectByStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, ConnectBy),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten([
        "connect by ",
        if byte_size(NoCycle) > 0 -> NoCycleStr ++ " "; true -> [] end,
        ConnectByStr
    ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE INDEX
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'create index', Opts, Idx, Table, Spec, Norm, Filter} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptsStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Opts),
    {IdxStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Idx),
    {TableStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Table),
    {Specs, NewCtx4} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, S),
        {Acc ++ [SubAcc], CtxAcc1}
                                   end,
        {[], NewCtx3},
        Spec),
    {NormStr, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Norm),
    {FilterStr, NewCtx6} = fold(FType, Fun, NewCtx5, Lvl + 1, Filter),
    NewCtx7 = case FType of
                  top_down -> NewCtx6;
                  bottom_up -> Fun(ST, NewCtx6)
              end,
    RT = {lists:append([
        "create ",
        if length(OptsStr) > 0 -> OptsStr ++ " "; true -> [] end,
        "index ",
        if length(IdxStr) > 0 -> IdxStr ++ " "; true -> [] end,
        "on ", TableStr,
        if length(Specs) > 0 ->
            lists:append([" (", string:join(Specs, " | "), ")"]); true ->
            "" end,
        if NormStr =/= [] -> " " ++ NormStr; true -> [] end,
        if FilterStr =/= [] -> " " ++ FilterStr; true -> [] end
    ]), NewCtx7},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE ROLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'create role', Role} = ST)
    when is_binary(Role) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RoleStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Role),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"create role " ++ RoleStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE SCHEMA AUTHORIZATION
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'create schema authorization', User, SchemaElements} = ST)
    when is_binary(User), is_list(SchemaElements) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(User, NewCtx),
    {SchemaElementsStr, NewCtx2} = lists:foldl(fun(SE, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, SE),
        {Acc ++ case length(Acc) == 0 of
                    true -> [];
                    _ -> " "
                end ++ SubAcc, CtxAcc1}
                                               end,
        {[], NewCtx1},
        SchemaElements),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["create schema authorization ", binary_to_list(User), " ", SchemaElementsStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'create table', Tab, Fields, Opts} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptsStr, NewCtx1}
        = lists:foldl(
        fun(Opt, {Str, AccCtx}) ->
            {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl + 1, Opt),
            {Str ++ case length(Str) == 0 of
                        true -> [];
                        _ -> " "
                    end ++ NewStr, NewAccCtx}
        end, {"", NewCtx}, Opts),
    NewCtx2 = Fun(Tab, NewCtx1),
    {Clms, NewCtx3} = lists:foldl(fun(Clm, {Acc, CtxAcc}) ->
        case Clm of
            {C, {T, "(", N, N1}, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                CtxAcc3 = Fun(N, CtxAcc2),
                CtxAcc4 = Fun(N1, CtxAcc3),
                {SubAcc, CtxAcc5} = fold(FType, Fun, CtxAcc4, Lvl + 1, O),
                {Acc ++ [binary_to_list(
                    list_to_binary([C, " ", T, "(", N, ",", N1, ")", SubAcc])
                )], CtxAcc5};
            {C, {T, "(", N}, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                CtxAcc3 = Fun(N, CtxAcc2),
                {SubAcc, CtxAcc4} = fold(FType, Fun, CtxAcc3, Lvl + 1, O),
                {Acc ++ [binary_to_list(
                    list_to_binary([C, " ", T, "(", N, ")", SubAcc])
                )], CtxAcc4};
            {C, T, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                {SubAcc, CtxAcc3} = fold(FType, Fun, CtxAcc2, Lvl + 1, O),
                {Acc ++ [binary_to_list(
                    list_to_binary([C, " ", T, " ", SubAcc])
                )], CtxAcc3};
            Clm ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Clm),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                  end,
        {[], NewCtx2},
        Fields),
    {TabStr, NewCtx4} = case is_binary(Tab) of
                            true -> {binary_to_list(Tab), NewCtx3};
                            _ -> fold(FType, Fun, NewCtx3, Lvl + 1, Tab)
                        end,
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([
        "create ",
        if length(OptsStr) > 0 -> OptsStr ++ " "; true -> [] end,
        "table ",
        TabStr,
        " (",
        string:join(Clms, ", "),
        ")"
    ]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE USER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'create user', Usr, Id, []} = ST)
    when is_binary(Usr) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Usr, NewCtx),
    {IdStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Id),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["create user ", binary_to_list(Usr), " ", IdStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'create user', Usr, Id, Opts} = ST)
    when is_binary(Usr) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Usr, NewCtx),
    {IdStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Id),
    {OptsStr, NewCtx3}
        = lists:foldl(
        fun(Opt, {Str, AccCtx}) ->
            {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl + 1, Opt),
            {Str ++ case length(Str) == 0 of
                        true -> [];
                        _ -> " "
                    end ++ NewStr, NewAccCtx}
        end, {"", NewCtx2}, Opts),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["create user ", binary_to_list(Usr), " ", IdStr, " ", OptsStr]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE VIEW
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'create view', Table, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} = case is_binary(Table) of
                              true ->
                                  {binary_to_list(Table), Fun(Table, NewCtx)};
                              _ -> fold(FType, Fun, NewCtx, Lvl + 1, Table)
                          end,
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"create view " ++ TableStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

fold(FType, Fun, Ctx, Lvl, {'create view', Table, {Columns, "("}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} = case is_binary(Table) of
                              true ->
                                  {binary_to_list(Table), Fun(Table, NewCtx)};
                              _ -> fold(FType, Fun, NewCtx, Lvl + 1, Table)
                          end,
    {Clms, NewCtx2} = lists:foldl(fun(Clm, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Clm),
        {Acc ++ [SubAcc], CtxAcc1}
                                  end,
        {[], NewCtx1},
        Columns),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["create view ", TableStr, " (", string:join(Clms, ", "), ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DECLARE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {declare, {cur, CurName}, {cur_for, Stmt},
    OrderByST} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StmtStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Stmt),
    {OptOrderByStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, OrderByST),
    NewCtx3 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["declare ", CurName, " cursor for ", StmtStr, " ", OptOrderByStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure calls ('declare begin procedure' or 'begin procedure')
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {D, StmtList} = ST)
    when D =:= 'declare begin procedure'; D =:= 'begin procedure' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {BodyStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {stmtList, StmtList}),
    RT = {lists:append([
        case D of
            'declare begin procedure' -> "declare begin ";
            'begin procedure' -> "begin "
        end,
        BodyStr,
        "; end"
    ]), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {default, Def} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {DefStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Def),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten([
        "default ",
        case Def of
            Def when is_binary(Def) -> binary_to_list(Def);
            _ -> DefStr
        end
    ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT TABLESPACE / TEMPORARY TABLESPACE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {TS, Tab} = ST)
    when TS == 'default tablespace'; TS == 'temporary tablespace' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Tab, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten([atom_to_list(TS), " ", binary_to_list(Tab)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DELETE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {delete, Table, Where, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        case is_binary(Table) of
            true -> {binary_to_list(Table), NewCtx};
            _ -> fold(FType, Fun, NewCtx, Lvl + 1, Table)
        end,
    {WhereStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Where),
    {ReturnStr, NewCtx3} =
        case Return of
            {_, {}} -> {[], NewCtx2};
            _ -> fold(FType, Fun, NewCtx2, Lvl + 1, Return)
        end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["delete from ", TableStr, " ", WhereStr, " ", ReturnStr]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of aggregate function types 3/4: DISTINCT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {distinct, Column} = ST)
    when is_binary(Column) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    RT = {"distinct " ++ binary_to_list(Column), NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {distinct, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"distinct " ++ ValueStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP INDEX
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'drop index', Indx, Tbl} = ST)
    when is_binary(Tbl) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {lists:append([
        "drop index ",
        if Indx == {} -> "from ";
            true -> binary_to_list(Indx) ++ " from "
        end,
        binary_to_list(Tbl)
    ]), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'drop index', Indx, Tbl} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TblStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Tbl),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([
        "drop index ",
        if Indx == {} -> "from ";
            true -> binary_to_list(Indx) ++ " from "
        end,
        TblStr
    ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP ROLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'drop role', Role} = ST)
    when is_binary(Role) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RoleStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Role),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"drop role " ++ RoleStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'drop table', {tables, Ts}, E, RC, Types} = ST)
    when (is_atom(RC) orelse (RC =:= {})) andalso
    (is_atom(E) orelse (E =:= {})) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(E, NewCtx),
    {Tables, NewCtx2} = lists:foldl(fun(T, {Acc, CtxAcc}) ->
        CtxAcc1 = Fun(T, CtxAcc),
        {Acc ++ [case is_binary(T) of
                     true -> binary_to_list(T);
                     _ -> {TNew, _} = fold(FType, Fun, NewCtx, Lvl + 1, T),
                         TNew
                 end], CtxAcc1}
                                    end,
        {[], NewCtx1},
        Ts),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    TypesStr = binary_to_list(Types),
    RT = {lists:append([
        "drop ",
        if length(TypesStr) > 0 -> TypesStr ++ " "; true -> [] end,
        "table ",
        if E =:= exists -> "if exists "; true -> [] end,
        string:join(Tables, ", "),
        if is_atom(RC) -> " " ++ atom_to_list(RC); true -> [] end
    ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP USER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'drop user', Usr, Opts} = ST)
    when is_binary(Usr) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Usr, NewCtx),
    {OptsStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Opts),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["drop user ", binary_to_list(Usr), " ", OptsStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXISTS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {exists, Sql} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Sql),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(["exists ", SqlStr]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of Extra part
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Pt, {extra, Bin}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Pt),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {SqlStr ++ if Bin /= <<>> -> ";" ++ binary_to_list(Bin); true ->
        "" end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cursor statements: FETCH
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {fetch, {cur, CurName}, IntoST} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntoStr, NewCtx1} = fold(FType, Fun, Ctx, Lvl + 1, IntoST),
    NewCtx2 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["fetch ", CurName, " ", IntoStr]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIELDS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {fields, Fields} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ [SubAcc], CtxAcc1}
                                       end,
        {[], NewCtx},
        Fields),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {string:join(FieldsStr, ", "), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Index norm or filter
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {FunType, FunBody} = ST)
    when FunType =:= norm; FunType =:= filter ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    FunHead = case FunType of
                  norm -> "norm_with ";
                  filter -> "filter_with "
              end,
    RT = {FunHead ++ binary_to_list(FunBody), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FOREIGN KEY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'foreign key', ClmList, {ref, Ref}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ColStrList, NewCtx1} =
        lists:foldl(
            fun(Clm, {StrList, ICtx}) ->
                {CStr, ICtx1} = fold(FType, Fun, ICtx, Lvl + 1, Clm),
                {[CStr | StrList], ICtx1}
            end, {[], NewCtx}, ClmList),
    ClmStr = string:join(lists:reverse(ColStrList), ", "),
    case Ref of
        {Table, TblClmList}
            when is_list(TblClmList) ->
            {TblStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Table),
            {TblColStrList, NewCtx3} =
                lists:foldl(
                    fun(Clm, {StrList, ICtx}) ->
                        {CStr, ICtx1} = fold(FType, Fun, ICtx, Lvl + 1, Clm),
                        {[CStr | StrList], ICtx1}
                    end, {[], NewCtx2}, TblClmList),
            RefStr = lists:append([TblStr, " (", string:join(lists:reverse(TblColStrList), ", "), ")"]);
        _ ->
            {RefStr, NewCtx3} = fold(FType, Fun, NewCtx1, Lvl + 1, Ref)
    end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["foreign key (", ClmStr, ") references ", RefStr]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FROM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {from, Froms} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FrmStr, NewCtx2} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ [SubAcc], CtxAcc1}
                                    end, {[], NewCtx}, Froms),
    {FromStr, NewCtx1} = {string:join(FrmStr, ", "), NewCtx2},
    NewCtx3 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"from " ++ FromStr, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% funs
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'fun', N, []} = ST)
    when is_binary(N) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(N, NewCtx),
    RT = {binary_to_list(N), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'fun', N, Args} = ST)
    when is_binary(N) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(N, NewCtx),
    {ArgsStr, NewCtx2} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
        case A of
            A when is_binary(A) -> {Acc ++ [binary_to_list(A)], Fun(A, CtxAcc)};
            _ ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, A),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                     end,
        {[], NewCtx1},
        Args),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([binary_to_list(N), "(", string:join(ArgsStr, ", "), ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GOTO
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {goto, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"goto " ++ Value, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GRANT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {grant, Objs, {OnTyp, On}, {'to', Tos}, Opts} = ST)
    when is_atom(OnTyp), is_atom(Opts) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [atom_to_list(O)], Fun(O, CtxAcc)}
                                     end,
        {[], NewCtx},
        Objs),
    {OnTypNew, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, OnTyp),
    {OnNew, NewCtx3} = case is_binary(On) of
                           true -> {binary_to_list(On), NewCtx2};
                           _ -> fold(FType, Fun, NewCtx2, Lvl + 1, On)
                       end,
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [case is_binary(O) of
                     true -> binary_to_list(O);
                     _ -> {ONew, _} = fold(FType, Fun, NewCtx, Lvl + 1, O),
                         ONew
                 end], Fun(O, CtxAcc)}
                                    end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {lists:append([
        "grant ",
        case length(ObjsStr) == 0 of
            true -> [];
            _ -> string:join(ObjsStr, ",") ++ " "
        end,
        if On =/= <<"">> ->
            lists:append([OnTypNew, " ", OnNew, " "]);
            true -> []
        end,
        lists:append(["to ", string:join(TosStr, ","), " "]),
        atom_to_list(Opts)
    ]), NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GROUP BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'group by', GroupBy} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {GroupByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        {F1, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ [F1], CtxAcc1}
                                        end,
        {[], NewCtx},
        GroupBy),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {if length(GroupByStr) > 0 ->
        "group by " ++ string:join(GroupByStr, ", "); true -> [] end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAVING
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {having, Having} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {HavingStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Having),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {if length(HavingStr) > 0 -> "having " ++ HavingStr; true ->
        "" end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hierarchical query
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, {'hierarchical query', {}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    RT = {"", Fun(ST, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'hierarchical query', {Part1, Part2}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Part1Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Part1),
    {Part2Str, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Part2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten([Part1Str, " ", Part2Str]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HINTS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {hints, Hints} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    Size = byte_size(Hints),
    NewCtx1 = Fun(Hints, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {if Size > 0 -> binary_to_list(Hints); true -> [] end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IDENTIFIED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'identified by', Pswd} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Pswd, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"identified by " ++ binary_to_list(Pswd), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {'identified extern', []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"identified externally", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {'identified extern', E} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(E, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"identified externally as " ++ binary_to_list(E), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {'identified globally', []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"identified globally", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {'identified globally', E} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(E, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"identified globally as " ++ binary_to_list(E), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, L, {'list', _} = R} = ST)
    when (Type == in orelse Type == 'not in') andalso is_binary(L) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(L, NewCtx),
    {RStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, R),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten([binary_to_list(L), " ", atom_to_list(Type), " ", RStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSERT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {insert, Tab, {}, {}, {}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TabStr, NewCtx1} = case is_binary(Tab) of
                            true -> {binary_to_list(Tab), NewCtx};
                            _ -> fold(FType, Fun, NewCtx, Lvl + 1, Tab)
                        end,
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"insert into " ++ TabStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {insert, Tab, {cols, Columns}, {values, Values}, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TabStr, NewCtx1} = case is_binary(Tab) of
                            true -> {binary_to_list(Tab), NewCtx};
                            _ -> fold(FType, Fun, NewCtx, Lvl + 1, Tab)
                        end,
    {CStrs, NewCtx2} = case Columns of
                           {Cols, _} -> lists:foldl(fun(C, {Acc, CtxAcc}) ->
                               {CT, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, C),
                               {Acc ++ [CT], CtxAcc1}
                                                    end,
                               {[], NewCtx1},
                               Cols);
                           [] -> {[], NewCtx1}
                       end,
    {Vals, NewCtx3} = lists:foldl(fun(V, {Acc1, CtxAcc1}) ->
        case V of
            V when is_binary(V) ->
                {Acc1 ++ [binary_to_list(V)], Fun(V, CtxAcc1)};
            V ->
                {VT, CtxAcc2} = fold(FType, Fun, CtxAcc1, Lvl + 1, V),
                {Acc1 ++ [VT], CtxAcc2}
        end
                                  end,
        {[], NewCtx2},
        Values),
    {Ret, NewCtx4} = fold(FType, Fun, NewCtx3, Lvl + 1, Return),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([
        "insert into ",
        TabStr,
        case length(CStrs) == 0 of
            true -> [];
            _ -> lists:flatten([" (", string:join(CStrs, ","), ")"])
        end,
        " values (",
        string:join(Vals, ","),
        ")",
        Ret
    ]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {insert, Tab, {cols, Columns}, {select, _} = SubQuery, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TabStr, NewCtx1} = case is_binary(Tab) of
                            true -> {binary_to_list(Tab), NewCtx};
                            _ -> fold(FType, Fun, NewCtx, Lvl + 1, Tab)
                        end,
    {CStrs, NewCtx2} = case Columns of
                           {Cols, _} -> lists:foldl(fun(C, {Acc, CtxAcc}) ->
                               {CT, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, C),
                               {Acc ++ [CT], CtxAcc1}
                                                    end,
                               {[], NewCtx1},
                               Cols);
                           [] -> {[], NewCtx1}
                       end,
    {SubQueryStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, SubQuery),
    {Ret, NewCtx4} = fold(FType, Fun, NewCtx3, Lvl + 1, Return),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([
        "insert into ",
        TabStr,
        case length(CStrs) == 0 of
            true -> [];
            _ -> lists:flatten([" (", string:join(CStrs, ","), ")"])
        end,
        " ",
        SubQueryStr,
        " ",
        Ret
    ]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTO
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {into, {Into, {in, In}}} = ST)
    when is_list(Into), is_binary(In) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntoStr, NewCtx1} = lists:foldl(fun(I, {Acc, CtxAcc}) ->
        {Acc ++ [case is_binary(I) of
                     true -> binary_to_list(I);
                     _ -> {INew, _} = fold(FType, Fun, NewCtx, Lvl + 1, I),
                         INew
                 end], Fun(I, CtxAcc)}
                                     end,
        {[], NewCtx},
        Into),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["into ", string:join(IntoStr, ","), " in ", binary_to_list(In)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {into, Into} = ST)
    when is_list(Into) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntoStr, NewCtx1} = lists:foldl(fun(I, {Acc, CtxAcc}) ->
        {Acc ++ [case is_binary(I) of
                     true -> binary_to_list(I);
                     _ -> {INew, _} = fold(FType, Fun, NewCtx, Lvl + 1, I),
                         INew
                 end], Fun(I, CtxAcc)}
                                     end,
        {[], NewCtx},
        Into),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"into " ++ string:join(IntoStr, ","), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% joins
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {JoinType, Tab} = ST)
    when JoinType =:= cross_join;
    JoinType =:= natural_join;
    JoinType =:= natural_inner_join ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Tab),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case JoinType of
              cross_join -> "cross join ";
              natural_join -> "natural join ";
              natural_inner_join -> "natural inner join "
          end ++ TabStr, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {{JoinType, OptPartition, OptNatural}, Tab, OptPartition1, OnOrUsing} = ST)
    when JoinType =:= full;
    JoinType =:= left;
    JoinType =:= right;
    JoinType =:= full_outer;
    JoinType =:= left_outer;
    JoinType =:= right_outer ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptPartitionStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, OptPartition),
    {OptNaturalStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, OptNatural),
    NewCtx3 = Fun(JoinType, NewCtx2),
    {TabStr, NewCtx4} = fold(FType, Fun, NewCtx3, Lvl + 1, Tab),
    {OptPartition1Str, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, OptPartition1),
    {OnOrUsingStr, NewCtx6} = fold(FType, Fun, NewCtx5, Lvl + 1, OnOrUsing),

    NewCtx7 = case FType of
                  top_down -> NewCtx6;
                  bottom_up -> Fun(ST, NewCtx6)
              end,
    RT = {lists:append([
        case length(OptPartitionStr) == 0 of
            true -> [];
            _ -> OptPartitionStr ++ " "
        end,
        case length(OptNaturalStr) == 0 of
            true -> [];
            _ -> OptNaturalStr ++ " "
        end,
        case JoinType of
            full -> "full join ";
            left -> "left join ";
            right -> "right join ";
            full_outer -> "full outer join ";
            left_outer -> "left outer join ";
            right_outer -> "right outer join "
        end,
        TabStr,
        case length(OptPartition1Str) == 0 of
            true -> [];
            _ -> " " ++ OptPartition1Str
        end,
        case length(OnOrUsingStr) == 0 of
            true -> [];
            _ -> " " ++ OnOrUsingStr
        end
    ]), NewCtx7},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {JoinType, Tab, OnOrUsing} = ST)
    when JoinType =:= join; JoinType =:= join_inner ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Tab),
    {OnOrUsingStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, OnOrUsing),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([
        case JoinType of
            join -> "join ";
            join_inner -> "inner join "
        end,
        TabStr,
        " ",
        OnOrUsingStr
    ]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Like operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Var, Like, []} = ST)
    when Type == like; Type == 'not like' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VarStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Var),
    {LikeStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Like),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([VarStr, " ", atom_to_list(Type), " ", LikeStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Type, Var, Like, OptEsc} = ST)
    when (Type == like orelse Type == 'not like') andalso is_binary(OptEsc) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VarStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Var),
    {LikeStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Like),
    NewCtx3 = Fun(OptEsc, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([VarStr, " ", atom_to_list(Type), " ", LikeStr, " escape ", binary_to_list(OptEsc)]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Type, Var, Like, OptEsc} = ST)
    when Type == like; Type == 'not like' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VarStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Var),
    {LikeStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Like),
    {OptEscStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, OptEsc),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([VarStr, " ", atom_to_list(Type), " ", LikeStr, " escape ", OptEscStr]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIMITED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {limited, Q, T} = ST)
    when is_binary(Q), is_binary(T) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Q, NewCtx),
    NewCtx2 = Fun(T, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten(["quota ", binary_to_list(Q), " on ", binary_to_list(T)]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIST
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {list, Elms} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ElmsStr, NewCtx1} = lists:foldl(fun(E, {Acc, CtxAcc}) ->
        case E of
            E when is_binary(E) -> {Acc ++ [binary_to_list(E)], Fun(E, CtxAcc)};
            E ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, E),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                     end,
        {[], NewCtx},
        Elms),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {string:join(ElmsStr, ", "), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NATURAL
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, natural = _ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, _ST]),
    RT = {"natural", Fun(natural, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unary - and 'not' operators
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Op, A} = ST)
    when Op =:= '+' orelse Op =:= '-' orelse Op =:= 'not' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Op, NewCtx),
    {Str, NewCtx3} = case A of
                         A when is_binary(A) ->
                             NewCtx2 = Fun(A, NewCtx1),
                             {lists:flatten([atom_to_list(Op), " ", binary_to_list(A)])
                                 , NewCtx2};
                         A ->
                             {As, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, A),
                             {lists:flatten([atom_to_list(Op), " ", As])
                                 , NewCtx2}
                     end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Str, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ON
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {on, Condition} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {CondStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Condition),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"on " ++ CondStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OPEN
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {open, {cur, CurName}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"open " ++ CurName, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OPT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {opt, Opt} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Opt, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {binary_to_list(Opt), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ORDER BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'order by', OrderBy} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    Size = length(OrderBy),
    {OrderByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {O, Op} when is_binary(O), is_binary(Op) ->
                CtxAcc1 = Fun(O, CtxAcc),
                {OpStr, CtxAcc2} = {binary_to_list(Op), Fun(Op, CtxAcc1)},
                {Acc ++ [lists:flatten([binary_to_list(O), case length(OpStr) == 0 of
                                                               true -> [];
                                                               _ -> " " ++ OpStr
                                                           end])], CtxAcc2};
            {O, Op} when is_binary(Op) ->
                {Os, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, O),
                {OpStr, CtxAcc2} = {binary_to_list(Op), Fun(Op, CtxAcc1)},
                {Acc ++ [lists:flatten([Os, case length(OpStr) == 0 of
                                                true -> [];
                                                _ -> " " ++ OpStr
                                            end])], CtxAcc2}
        end
                                        end,
        {[], NewCtx},
        OrderBy),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {if Size > 0 -> "order by " ++ string:join(OrderByStr, ", ");
              true -> []
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARAM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {{param, P1}, {param, P2}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(P1, NewCtx),
    P1New = binary_to_list(P1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    NewCtx3 = Fun(P2, NewCtx2),
    P2New = binary_to_list(P2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([P1New, " ", P2New]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {param, P} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(P, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {binary_to_list(P), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARTITION_BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {partition_by, Fields, Parentheses} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            F when is_binary(F) -> {Acc ++ [binary_to_list(F)], Fun(F, CtxAcc)};
            _ -> {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                       end,
        {[], NewCtx},
        Fields),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([
        "partition by ",
        Parentheses,
        string:join(FieldsStr, ","),
        case Parentheses of
            "(" -> ")";
            _ -> []
        end
    ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PASSWORD EXPIRE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {password, expire} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"password expire ", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIMARY KEY / UNIQUE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, ClmList} = ST)
    when Type == 'primary key';
    Type == unique ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ColStrList, NewCtx1} =
        lists:foldl(
            fun(Clm, {StrList, ICtx}) ->
                {CStr, ICtx1} = fold(FType, Fun, ICtx, Lvl + 1, Clm),
                {[CStr | StrList], ICtx1}
            end, {[], NewCtx}, ClmList),
    ClmStr = string:join(lists:reverse(ColStrList), ", "),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([atom_to_list(Type), " (", ClmStr, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIOR
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {prior, Field} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FieldsStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Field),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(["prior ", FieldsStr]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROFILE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {profile, Profile} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Profile, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(["profile ", binary_to_list(Profile)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% QUOTAS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {quotas, Quotas} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {QuotaStr, NewCtx1}
        = lists:foldl(
        fun(Quota, {Str, AccCtx}) ->
            {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl + 1, Quota),
            {Str ++ case length(Str) == 0 of
                        true -> [];
                        _ -> " "
                    end ++ NewStr, NewAccCtx}
        end, {"", NewCtx}, Quotas),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {QuotaStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REFERENCES
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {ref, {Value1, Value2}} = ST)
    when is_list(Value2) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value1),
    {Value2Str, NewCtx2} = lists:foldl(fun(V, {Acc, CtxAcc}) ->
        {VNew, _} = fold(FType, Fun, NewCtx1, Lvl + 1, V),
        {lists:append([
            Acc,
            case length(Acc) == 0 of
                true -> [];
                _ -> ","
            end,
            VNew
        ]), Fun(V, CtxAcc)}
                                       end,
        {[], NewCtx},
        Value2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten(["references ", Value1Str, " (", Value2Str, ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {ref, Value1} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(["references ", Value1Str]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optional Returning phrase
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {R, Sel, Var} = ST)
    when R =:= return; R =:= returning ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(R, NewCtx),
    {SelStr, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, S),
        {Acc ++ [SubAcc], CtxAcc1}
                                    end,
        {[], NewCtx1},
        Sel),
    {VarStr, NewCtx3} = lists:foldl(fun(V, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, V),
        {Acc ++ [SubAcc], CtxAcc1}
                                    end,
        {[], NewCtx2},
        Var),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([atom_to_list(R), " " ++ string:join(SelStr, ","), " INTO ", string:join(VarStr, ",")]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(_FType, Fun, Ctx, _Lvl, {R, {}} = _ST)
    when R =:= return; R =:= returning ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, _ST]),
    RT = {"", Fun(R, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REVOKE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {revoke, Objs, {OnTyp, On}, {'from', Tos}, Opts} = ST)
    when is_atom(OnTyp), is_atom(Opts) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [atom_to_list(O)], Fun(O, CtxAcc)}
                                     end,
        {[], NewCtx},
        Objs),
    {OnTypNew, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, OnTyp),
    {OnNew, NewCtx3} = case is_binary(On) of
                           true -> {binary_to_list(On), NewCtx2};
                           _ -> fold(FType, Fun, NewCtx2, Lvl + 1, On)
                       end,
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [case is_binary(O) of
                     true -> binary_to_list(O);
                     _ -> {ONew, _} = fold(FType, Fun, NewCtx, Lvl + 1, O),
                         ONew
                 end], Fun(O, CtxAcc)}
                                    end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {lists:append([
        "revoke ",
        case length(ObjsStr) == 0 of
            true -> [];
            _ -> string:join(ObjsStr, ",") ++ " "
        end,
        if On =/= <<"">> ->
            lists:append([OnTypNew, " ", OnNew, " "]);
            true -> []
        end,
        if length(Tos) > 0 ->
            lists:append(["from ", string:join(TosStr, ","), " "]); true -> []
        end,
        atom_to_list(Opts)
    ]), NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Role
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {Role, Roles} = ST)
    when Role == 'default role'; Role == 'default role all except' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {lists:flatten([
        atom_to_list(Role),
        " ",
        string:join([binary_to_list(R) || R <- Roles],
            ",")
    ]), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, 'default role all' = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {lists:flatten([atom_to_list(ST)]), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SCOPE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {scope, S} = ST)
    when S == <<"local">>; S == <<"cluster">>; S == <<"schema">> ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(S, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten([binary_to_list(S)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SELECT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {select, Opts} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NewOs, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, O),
        if length(SubAcc) > 0 ->
            {Acc ++ [" ", SubAcc], CtxAcc1};
            true ->
                {Acc, CtxAcc1}
        end
                                   end,
        {[], NewCtx},
        Opts),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"select" ++ lists:flatten(NewOs), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% START WITH
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'start with', StartWith} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StartWithStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, StartWith),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(["start with ", StartWithStr]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tab
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Tab, [J | _] = Joins} = ST)
    when is_tuple(J) andalso (is_binary(Tab) orelse is_tuple(Tab)) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TabStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Tab),
    {JoinsStr, NewCtx2} = lists:foldl(fun(Join, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Join),
        {Acc ++ case length(Acc) == 0 of
                    true -> [];
                    _ -> " "
                end ++ [SubAcc], CtxAcc1}
                                      end,
        {[], NewCtx1},
        Joins),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([TabStr, " ", JoinsStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRUNCATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'truncate table', Tbl, Mvl, Storage} = ST)
    when is_binary(Tbl) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Tbl, NewCtx),
    NewCtx2 = Fun(Mvl, NewCtx1),
    NewCtx3 = Fun(Storage, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([
        "truncate table ",
        binary_to_list(Tbl),
        " ",
        case Mvl of
            {} -> [];
            {'materialized view log', T} ->
                lists:flatten([atom_to_list(T), " materialized view log "])
        end,
        case Storage of
            {} -> [];
            {'storage', T} -> lists:flatten([atom_to_list(T), " storage"])
        end
    ]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TYPE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {type, T} = ST)
    when T == 'set'; T == 'ordered_set'; T == 'bag'; is_binary(T) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(T, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten([binary_to_list(T)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Union
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {union, A, B} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, A),
    {BStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, B),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:flatten([AStr, " union ", BStr]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNLIMITED ON
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'unlimited on', T} = ST)
    when is_binary(T) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(T, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"quota unlimited on " ++ binary_to_list(T), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UPDATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {update, Table, {set, Set}, Where, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        case is_binary(Table) of
            true -> {binary_to_list(Table), NewCtx};
            _ -> fold(FType, Fun, NewCtx, Lvl + 1, Table)
        end,
    {Sets, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, S),
        {Acc ++ [SubAcc], CtxAcc1}
                                  end,
        {[], NewCtx1},
        Set),
    {WhereStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
    {ReturnStr, NewCtx4} =
        case Return of
            {_, {}} -> {[], NewCtx3};
            _ -> fold(FType, Fun, NewCtx3, Lvl + 1, Return)
        end,
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([
        "update ",
        TableStr,
        " set ",
        string:join(Sets, ","),
        if length(WhereStr) > 0 orelse length(ReturnStr) > 0 ->
            lists:append([" ", WhereStr, " ", ReturnStr]);
            true -> []
        end
    ]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USING
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {using, ColumnList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ColumnListStr, NewCtx1} = lists:foldl(fun(C, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, C),
        {Acc ++ [SubAcc], CtxAcc1}
                                           end,
        {[], NewCtx},
        ColumnList),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["using (", string:join(ColumnListStr, ","), ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHENEVER NOT FOUND
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {when_not_found, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"whenever not found " ++ ValueStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHENEVER SQLERROR
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {when_sql_err, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"whenever sqlerror " ++ ValueStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All where clauses
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, {where, {}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    RT = {"", Fun(ST, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {where, Where} = ST)
    when is_tuple(Where) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {WhereStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Where),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"where " ++ WhereStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHERE_CURRENT_OF
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {where_current_of, {cur, CurName}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"where current of " ++ CurName, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Empty list or tuples
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, _Fun, Ctx, _Lvl, X = _ST)
    when X =:= {}; X =:= [] ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, _ST]),
    RT = {"", Ctx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% JSON parser hooking
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, _Fun, Ctx, _Lvl, {Op, _, _} = ST)
    when Op =:= ':'; Op =:= '::'; Op =:= '#';
    Op =:= '{}'; Op =:= '[]' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    {ok, JPPath} = jpparse_fold:string(ST),
    RT = {lists:append(["|", binary_to_list(JPPath), "|"]), Ctx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Boolean and arithmetic binary operators handled with precedence
% *,/ > +,- > and > or
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_tuple(L), is_tuple(R), Op /= fetch, Op /= 'connect by' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Fl, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, L),
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
                        {'and', 'or'} ->
                            {Rs, NC2} = fold(FType, Fun, NewCtx2, Lvl + 1, R),
                            {Rs, NC2};
                        _ -> fold(FType, Fun, NewCtx2, Lvl + 1, R)
                    end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([Fl, " ", atom_to_list(Op), " ", Fr]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_binary(L), is_tuple(R), Op /= 'connect by' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, R),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", Fr]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_tuple(L), is_binary(R), Op /= 'connect by' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Fl, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, L),
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([Fl, " ", atom_to_list(Op), " ", binary_to_list(R)]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_binary(L), is_binary(R) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", binary_to_list(R)]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_binary(L), is_list(R) orelse is_tuple(R) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    {RNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl, R),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", RNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_list(L) orelse is_tuple(L), is_binary(R) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {LNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, L),
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([LNew, " ", atom_to_list(Op), " ", binary_to_list(R)]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_list(L) orelse is_tuple(L), is_list(R) orelse is_tuple(R) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {LNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, L),
    NewCtx2 = Fun(Op, NewCtx1),
    {RNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl, R),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten([LNew, " ", atom_to_list(Op), " ", RNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Index options
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, ST) when is_atom(ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {atom_to_list(ST), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_ref -> table range_variable
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Value1, Value2} = ST)
    when is_binary(Value2) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Value1Str, " ", binary_to_list(Value2)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
