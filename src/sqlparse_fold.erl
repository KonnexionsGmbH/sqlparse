%% -----------------------------------------------------------------------------
%%
%% sqlparse_fold.erl: SQL parser - utilities.
%%
%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
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
% List of parsetrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, STs) when is_list(STs) ->
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
% ACCOUNT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'account', LockUnlock} = ST)
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
    RT = {" account " ++ atom_to_list(LockUnlock) ++ " ",
        NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of aggregate function types 3/4: ALL
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {all, Column} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    RT = {" all " ++ binary_to_list(Column)
        , NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALL / ANY / SOME
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {N, Args} = ST)
    when N == 'ANY'; N == 'ALL'; N == 'SOME' ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(N, NewCtx),
    {ArgsStr, NewCtx2} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
        case A of
            A when is_binary(A) -> {Acc ++ [binary_to_list(A)], Fun(A, CtxAcc)};
            A when is_tuple(A) ->
                case lists:member(element(1, A),
                    ['select', 'insert', 'create table',
                        'create user', 'alter user',
                        'truncate table', 'update', 'delete',
                        'grant', 'revoke']) of
                    true ->
                        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, A),
                        {Acc ++ ["(" ++ string:strip(SubAcc) ++ ")"], CtxAcc1};
                    _ ->
                        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, A),
                        {Acc ++ [SubAcc], CtxAcc1}
                end;
            A ->
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
    RT = {atom_to_list(N) ++ "(" ++ string:join(ArgsStr, ", ") ++ ")", NewCtx3},
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
                {OptsS ++ " " ++ OS, INewCtx1}
            end, {"", NewCtx}, Opts),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {lists:flatten(["alter user ", binary_to_list(Usr), " ", OptsStr]),
        NewCtx2},
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
                        [" ", atom_to_list(W), " ", atom_to_list(A)];
                    {W, Roles} when is_atom(W) ->
                        [" ", atom_to_list(W), " ",
                            string:join([binary_to_list(R) || R <- Roles],
                                ",")];
                    {{W, Roles}, Authrec} when is_atom(W) ->
                        [" ", atom_to_list(W), " ",
                            string:join([binary_to_list(R) || R <- Roles],
                                ","), " ", atom_to_list(Authrec)]
                end
        end],
    NewCtx1 = case FType of
        top_down -> NewCtx;
        bottom_up -> Fun(ST, NewCtx)
    end,
    RT = {lists:flatten(
        ["alter user ", string:join([binary_to_list(U) || U <- Users], ","),
            " ", GrantStr]), NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All aliases
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'as', A, B} = ST)
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
    RT = {lists:flatten([binary_to_list(A), " as ", binary_to_list(B)]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'as', A, B} = ST)
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
    RT = {lists:flatten([string:strip(AStr), " as ", binary_to_list(B)]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {'as', A} = ST)
    when is_binary(A) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(A, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {lists:flatten(["as ", binary_to_list(A)]), NewCtx2},
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

fold(FType, Fun, Ctx, Lvl, {'between', A, B, C} = ST) ->
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
    RT = {lists:flatten([A1, " between ", B1, " and ", C1]), NewCtx4},
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
    RT = {"call " ++ FunctionStr
        , NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASCADE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, [cascade | Opts] = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {OptsStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Opts),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {lists:flatten([" cascade ", OptsStr]), NewCtx2},
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
    {WhenThenStr, NewCtx1}
        = lists:foldl(
        fun({When, Then}, {Sql, AccCtx}) ->
            {WhenStr, AccCtx1} = fold(FType, Fun, AccCtx, Lvl + 1, When),
            {ThenStr, AccCtx2} = fold(FType, Fun, AccCtx1, Lvl + 1, Then),
            {Sql ++ " when " ++ WhenStr ++ " then " ++ ThenStr, AccCtx2}
        end, {"", NewCtx}, WhenThenList),
    {ElseStr, NewCtx2} = case Else of
        {} -> {"", NewCtx1};
        Else ->
            {EStr, NewCtx21} = fold(FType, Fun, NewCtx1, Lvl + 1, Else),
            {" else " ++ EStr, NewCtx21}
    end,
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    RT = {"case " ++ binary_to_list(Expr) ++ WhenThenStr ++ ElseStr ++ " end"
        , NewCtx3},
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
    RT = {" check (" ++ ConditionStr ++ ")", NewCtx2},
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
    RT = {lists:flatten(["connect by "
        , if byte_size(NoCycle) > 0 -> NoCycleStr ++ " "; true -> "" end
        , ConnectByStr])
        , NewCtx3},
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
    RT = {"create " ++ if length(OptsStr) > 0 -> OptsStr ++ " "; true -> "" end
        ++ "index " ++ if length(IdxStr) > 0 -> IdxStr ++ " "; true -> "" end
        ++ "on " ++ TableStr
        ++ if length(Specs) > 0 -> " (" ++ string:join(Specs, "|") ++ ") ";
        true -> "" end
        ++ if NormStr =/= [] -> NormStr; true -> "" end
        ++ if FilterStr =/= [] -> FilterStr; true -> "" end
        , NewCtx7},
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
    RT = {"create role " ++ RoleStr,
        NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'create table', Tab, Fields, Opts} = ST)
    when is_binary(Tab) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {OptsStr, NewCtx1}
        = lists:foldl(
        fun(Opt, {Str, AccCtx}) ->
            {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl + 1, Opt),
            {Str ++ NewStr, NewAccCtx}
        end, {"", NewCtx}, Opts),
    NewCtx2 = Fun(Tab, NewCtx1),
    {Clms, NewCtx3} = lists:foldl(fun(Clm, {Acc, CtxAcc}) ->
        case Clm of
            {C, {T, N, N1}, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                CtxAcc3 = Fun(N, CtxAcc2),
                CtxAcc4 = Fun(N1, CtxAcc3),
                {SubAcc, CtxAcc5} = fold(FType, Fun, CtxAcc4, Lvl + 1, O),
                {Acc ++ [binary_to_list(
                    list_to_binary([C, " ", T, "(", N, ",", N1, ") ", SubAcc])
                )], CtxAcc5};
            {C, {T, N}, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                CtxAcc3 = Fun(N, CtxAcc2),
                {SubAcc, CtxAcc4} = fold(FType, Fun, CtxAcc3, Lvl + 1, O),
                {Acc ++ [binary_to_list(
                    list_to_binary([C, " ", T, "(", N, ") ", SubAcc])
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
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {"create " ++ if length(OptsStr) > 0 ->
        OptsStr ++ " ";
        true -> "" end ++ "table " ++ binary_to_list(Tab)
        ++ " (" ++ string:join(Clms, ", ") ++ ")"
        , NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE USER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
            {Str ++ NewStr, NewAccCtx}
        end, {"", NewCtx2}, Opts),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {"create user " ++ binary_to_list(Usr)
        ++ IdStr ++ " " ++ OptsStr
        , NewCtx4},
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
        cross_join -> " cross join ";
        natural_join -> " natural join ";
        natural_inner_join -> " natural inner join "
    end ++ TabStr
        , NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {{JoinType, OptPartition, OptNatural}, Tab, OptPartition1, OnOrUsing} = ST)
    when JoinType =:= full;       JoinType =:= left;
    JoinType =:= right;      JoinType =:= full_outer;
    JoinType =:= left_outer; JoinType =:= right_outer ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {OptPartitionStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, OptPartition),
    {OptNaturalStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, OptNatural),
    NewCtx3 = Fun(JoinType, NewCtx2),
    {TabStr0, NewCtx4} = fold(FType, Fun, NewCtx3, Lvl + 1, Tab),
    TabStr = case Tab of
        {select, _} -> "(" ++ string:strip(TabStr0) ++ ")";
        _ -> string:strip(TabStr0)
    end,
    {OptPartition1Str, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, OptPartition1),
    {OnOrUsingStr, NewCtx6} = fold(FType, Fun, NewCtx5, Lvl + 1, OnOrUsing),

    NewCtx7 = case FType of
        top_down -> NewCtx6;
        bottom_up -> Fun(ST, NewCtx6)
    end,
    RT = {OptPartitionStr ++ OptNaturalStr ++
        case JoinType of
            full -> " full join ";
            left -> " left join ";
            right -> " right join ";
            full_outer -> " full outer join ";
            left_outer -> " left outer join ";
            right_outer -> " right outer join "
        end ++ TabStr ++ OptPartition1Str ++ OnOrUsingStr
        , NewCtx7},
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
    {TabStr0, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Tab),
    TabStr = case Tab of
        {select, _} -> "(" ++ string:strip(TabStr0) ++ ")";
        _ -> string:strip(TabStr0)
    end,
    {OnOrUsingStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, OnOrUsing),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {case JoinType of
        join -> " join ";
        join_inner -> " inner join "
    end ++ TabStr ++ OnOrUsingStr
        , NewCtx4},
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
    RT = {"declare " ++ CurName ++ " cursor for " ++ StmtStr ++ OptOrderByStr,
        NewCtx3},
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
    {BodyStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, StmtList),
    RT = {case D of
        'declare begin procedure' -> "declare begin ";
        'begin procedure' -> "begin "
    end ++ BodyStr ++ "; end",
        NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'default', Def} = ST) ->
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
    RT = {lists:flatten([" default ",
        case Def of
            Def when is_binary(Def) -> binary_to_list(Def);
            Def -> DefStr
        end, "\n "])
        , NewCtx2},
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
    RT = {lists:flatten([" ", atom_to_list(TS), " ", binary_to_list(Tab), " "]),
        NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DELETE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'delete', Table, Where, Return} = ST)
    when is_binary(Table) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Table, NewCtx),
    {WhereStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Where),
    {ReturnStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Return),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {"delete from " ++ binary_to_list(Table)
        ++ " " ++ WhereStr ++ ReturnStr
        , NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of aggregate function types 3/4: DISTINCT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {distinct, Column} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    RT = {" distinct " ++ binary_to_list(Column)
        , NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP FUNCTION
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'drop function', FunctionName} = ST)
    when is_binary(FunctionName) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(FunctionName, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {"drop function " ++ binary_to_list(FunctionName)
        , NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP INDEX
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'drop index', Indx, Tbl} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = case FType of
        top_down -> NewCtx;
        bottom_up -> Fun(ST, NewCtx)
    end,
    RT = {"drop index "
        ++ if Indx == {} -> "from ";
        true -> binary_to_list(Indx) ++ " from "
    end
        ++ binary_to_list(Tbl)
        , NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP PROCEDURE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'drop procedure', ProcedureName} = ST)
    when is_binary(ProcedureName) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(ProcedureName, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {"drop procedure " ++ binary_to_list(ProcedureName)
        , NewCtx2},
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
    RT = {"drop role " ++ RoleStr
        , NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'drop table', {tables, Ts}, E, RC, Types} = ST)
    when (is_atom(RC) orelse (RC =:= {})) andalso
    (is_atom(E) orelse (E =:= {})) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(E, NewCtx),
    {Tables, NewCtx2} = lists:foldl(fun(T, {Acc, CtxAcc}) ->
        CtxAcc1 = Fun(T, CtxAcc),
        {Acc ++ [binary_to_list(T)], CtxAcc1}
    end,
        {[], NewCtx1},
        Ts),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    TypesStr = string:join([binary_to_list(T) || {'type', T} <- Types], " "),
    RT = {"drop "
        ++ if length(TypesStr) > 0 -> TypesStr ++ " "; true -> "" end
        ++ "table "
        ++ if E =:= exists -> "if exists "; true -> "" end
        ++ string:join(Tables, ", ")
        ++ if is_atom(RC) -> " " ++ atom_to_list(RC); true -> "" end
        , NewCtx3},
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
    RT = {"drop user " ++ binary_to_list(Usr)
        ++ " " ++ OptsStr
        , NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXISTS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'exists', Sql} = ST) ->
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
    RT = {lists:flatten([" exists (", SqlStr, ")"]), NewCtx2},
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
    RT = {SqlStr ++ if Bin /= <<>> ->
        ";" ++ binary_to_list(Bin);
        true -> ""
    end,
        NewCtx2},
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
    RT = {string:strip("fetch " ++ CurName ++ " " ++ IntoStr), NewCtx2},
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
        case F of
            F when is_binary(F) -> {Acc ++ [binary_to_list(F)], Fun(F, CtxAcc)};
            {'select', _} = F ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ [lists:flatten(["(", SubAcc, ")"])], CtxAcc1};
            Other ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Other),
                {Acc ++ [SubAcc], CtxAcc1}
        end
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
        norm -> " norm_with ";
        filter -> " filter_with "
    end,
    RT = {FunHead ++ binary_to_list(FunBody) ++ " ", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FOREIGN KEY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'foreign key', ClmList, {'ref', Ref}} = ST) ->
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
        {Table, TblClmList} ->
            {TblStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, Table),
            {TblColStrList, NewCtx3} =
                lists:foldl(
                    fun(Clm, {StrList, ICtx}) ->
                        {CStr, ICtx1} = fold(FType, Fun, ICtx, Lvl + 1, Clm),
                        {[CStr | StrList], ICtx1}
                    end, {[], NewCtx2}, TblClmList),
            RefStr = TblStr ++ " (" ++ string:join(lists:reverse(TblColStrList), ", ") ++ ")";
        Table ->
            {RefStr, NewCtx3} = fold(FType, Fun, NewCtx1, Lvl + 1, Table)
    end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {" foreign key (" ++ ClmStr ++ ") references " ++ RefStr, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FROM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {from, Forms} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {FormStr, NewCtx1} = case Forms of
        Forms when is_list(Forms) ->
            {FrmStr, NewCtx2}
                = lists:foldl(
                fun(F, {Acc, CtxAcc}) ->
                    case F of
                        F when is_binary(F) ->
                            {Acc ++ [binary_to_list(F)], Fun(F, CtxAcc)};
                        {'select', _} = F ->
                            {FoldFStr, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                            {Acc ++ [lists:flatten(["(", string:strip(FoldFStr), ")"])],
                                CtxAcc1};
                        Other ->
                            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Other),
                            {Acc ++ [SubAcc], CtxAcc1}
                    end
                end, {[], NewCtx},
                [case Frm of
                    {'as', A, B} -> {'$from_as', A, B};
                    _ -> Frm
                end || Frm <- Forms]),
            {string:join(FrmStr, ", ")
                , NewCtx2};
        Forms ->
            fold(FType, Fun, NewCtx, Lvl + 1, Forms)
    end,
    NewCtx3 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {"from " ++ FormStr, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alias for from list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'$from_as', A, B} = ST)
    when is_binary(B) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {AStr, NewCtx1}
        = case A of
        A when is_binary(A) ->
            {string:strip(binary_to_list(A)), Fun(A, NewCtx)};
        {param, A0} when is_binary(A0) ->
            {string:strip(binary_to_list(A0)), Fun(A, NewCtx)};
        A ->
            {A0, NCtx} = fold(FType, Fun, NewCtx, Lvl + 1, A),
            {lists:flatten(["(", string:strip(A0), ")"]), NCtx}
    end,
    NewCtx2 = Fun(B, NewCtx1),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    RT = {lists:flatten([AStr, " ", binary_to_list(B)]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% funs
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
            A when is_tuple(A) ->
                case lists:member(element(1, A),
                    ['select', 'insert', 'create table',
                        'create user', 'alter user',
                        'truncate table', 'update', 'delete',
                        'grant', 'revoke']) of
                    true ->
                        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, A),
                        {Acc ++ ["(" ++ string:strip(SubAcc) ++ ")"], CtxAcc1};
                    _ ->
                        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, A),
                        {Acc ++ [SubAcc], CtxAcc1}
                end;
            A ->
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
    RT = {binary_to_list(N) ++ "(" ++
        string:join(ArgsStr, ", ")
        ++ ")"
        , NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GRANT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'grant', Objs, {OnTyp, On}, {'to', Tos}, Opts} = ST)
    when is_atom(OnTyp), is_atom(Opts) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [atom_to_list(O)], Fun(O, CtxAcc)}
    end,
        {[], NewCtx},
        Objs),
    NewCtx2 = Fun(OnTyp, NewCtx1),
    NewCtx3 = Fun(On, NewCtx2),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [binary_to_list(O)], Fun(O, CtxAcc)}
    end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    NewCtx6 = case FType of
        top_down -> NewCtx5;
        bottom_up -> Fun(ST, NewCtx5)
    end,
    RT = {"grant "
        ++ string:join(ObjsStr, ",") ++ " "
        ++ if On =/= <<"">> ->
        atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
        ++ if length(Tos) > 0 ->
        "to " ++ string:join(TosStr, ",") ++ " "; true -> "" end
        ++ atom_to_list(Opts)
        , NewCtx6},
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
    Size = length(GroupBy),
    {GroupByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case fold(FType, Fun, CtxAcc, Lvl + 1, F) of
            {F1, CtxAcc1} when is_binary(F1) ->
                {Acc ++ binary_to_list(F1), CtxAcc1};
            {F1, CtxAcc1} when is_list(F1) -> {Acc ++ [F1], CtxAcc1}
        end
    end,
        {[], NewCtx},
        GroupBy),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {if Size > 0 -> " group by " ++ string:join(GroupByStr, ", ");
        true -> ""
    end
        , NewCtx2},
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
    Size = size(Having),
    {HavingStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Having),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {if Size > 0 -> " having " ++ HavingStr;
        true -> ""
    end
        , NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hierarchical query
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, {'hierarchical query', {}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    {""
        , Fun(ST, Ctx)};
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
    RT = {lists:flatten([Part1Str, " ", Part2Str])
        , NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HINTS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% select sub-part patterns
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
    RT = {if Size > 0 -> binary_to_list(Hints);
        true -> ""
    end
        , NewCtx2},
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
    RT = {" identified by " ++ binary_to_list(Pswd), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'identified extern', E} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {IdStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, E),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {" identified externally " ++ IdStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'identified globally', E} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {IdStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, E),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {" identified globally " ++ IdStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In operator
% for right hand non list argument extra parenthesis added
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'in', L, {'list', _} = R} = ST)
    when is_binary(L) ->
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
    RT = {lists:flatten([binary_to_list(L), " in ", RStr])
        , NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {'in', L, R} = ST)
    when is_binary(L), is_tuple(R) ->
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
    RT = {lists:flatten([binary_to_list(L), " in (", RStr, ")"])
        , NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSERT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {insert, Tab, {}, {}, {}} = ST)
    when is_binary(Tab) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    RT = {"insert into " ++ binary_to_list(Tab)
        , NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {insert, Tab, {cols, Cols}, {values, Values}, Return} = ST)
    when is_binary(Tab) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {CStrs, NewCtx1} = lists:foldl(fun(C, {Acc, CtxAcc}) ->
        case C of
            C when is_binary(C) ->
                {Acc ++ [binary_to_list(C)], Fun(C, CtxAcc)};
            C ->
                {CT, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, C),
                {Acc ++ [CT], CtxAcc1}
        end
    end,
        {[], NewCtx},
        Cols),
    {Vals, NewCtx2} = lists:foldl(fun(V, {Acc1, CtxAcc1}) ->
        case V of
            V when is_binary(V) ->
                {Acc1 ++ [binary_to_list(V)], Fun(V, CtxAcc1)};
            V ->
                {VT, CtxAcc2} = fold(FType, Fun, CtxAcc1, Lvl + 1, V),
                {Acc1 ++ [VT], CtxAcc2}
        end
    end,
        {[], NewCtx1},
        Values),
    {Ret, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Return),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {"insert into " ++ binary_to_list(Tab) ++
        case length(CStrs) of
            0 -> "";
            _ -> lists:flatten(["(", string:join(CStrs, ","), ")"])
        end ++ " values (" ++ string:join(Vals, ",") ++ ")" ++ Ret
        , NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTO
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {into, Into} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {IntoStr, NewCtx1} = lists:foldl(fun(I, {Acc, CtxAcc}) ->
        {Acc ++ [binary_to_list(I)], Fun(I, CtxAcc)}
    end,
        {[], NewCtx},
        Into),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {"into " ++ string:join(IntoStr, ",") ++ " ", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Like operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'like', Var, Like, OptEsc} = ST) ->
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
    RT = {VarStr ++ " like " ++ LikeStr ++
        if byte_size(OptEsc) > 0 -> " escape " ++ binary_to_list(OptEsc);
            true -> ""
        end
        , NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIMITED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'limited', Q, T} = ST)
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
    RT = {lists:flatten(["quota ", binary_to_list(Q), " on ", binary_to_list(T),
        " "]),
        NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIST
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'list', Elms} = ST) ->
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
    RT = {"(" ++
        string:join(ElmsStr, ", ")
        ++ ")"
        , NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NATURAL
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, natural = _ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, _ST]),
    RT = {" natural", Fun(natural, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unary - and 'not' operators
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Op, A} = ST)
    when Op =:= '-' orelse Op =:= 'not' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Op, NewCtx),
    {Str, NewCtx3} = case A of
        A when is_binary(A) ->
            NewCtx2 = Fun(A, NewCtx1),
            {lists:flatten([atom_to_list(Op), " (", binary_to_list(A), ")"])
                , NewCtx2};
        A ->
            {As, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl + 1, A),
            {lists:flatten([atom_to_list(Op), " (", As, ")"])
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
    RT = {" on " ++ CondStr, NewCtx2},
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
    Size = byte_size(Opt),
    NewCtx1 = Fun(Opt, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {if Size > 0 -> binary_to_list(Opt) ++ " ";
        true -> ""
    end
        , NewCtx2},
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
            F when is_binary(F) -> {Acc ++ [binary_to_list(F)], Fun(F, CtxAcc)};
            {O, Op} when is_binary(O), is_binary(Op) ->
                CtxAcc1 = Fun(O, CtxAcc),
                CtxAcc2 = Fun(Op, CtxAcc1),
                {Acc ++ [string:strip(lists:flatten([binary_to_list(O), " ", binary_to_list(Op)]))]
                    , CtxAcc2};
            {O, Op} when is_binary(Op) ->
                {Os, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, O),
                CtxAcc2 = Fun(Op, CtxAcc1),
                {Acc ++ [string:strip(lists:flatten([Os, " ", binary_to_list(Op)]))]
                    , CtxAcc2}
        end
    end,
        {[], NewCtx},
        OrderBy),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {if Size > 0 ->
        "order by " ++ string:join(OrderByStr, ", ")
            ++ " ";
        true -> ""
    end
        , NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARAM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'param', P} = ST) ->
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
    RT = case P of
        P when is_binary(P) -> {binary_to_list(P), NewCtx2};
        P -> {P, NewCtx2}
    end,
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARTITION_BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {partition_by, Fields} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        {Acc ++ [binary_to_list(F)], Fun(F, CtxAcc)}
    end,
        {[], NewCtx},
        Fields),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {" partition by (" ++ string:join(FieldsStr, ",") ++ ")", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PASSWORD EXPIRE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'password', 'expire'} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = case FType of
        top_down -> NewCtx;
        bottom_up -> Fun(ST, NewCtx)
    end,
    RT = {" password expire ",
        NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIMARY KEY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'primary key', ClmList} = ST) ->
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
    RT = {" primary key (" ++ ClmStr ++ ")", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIOR
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'prior', Field} = ST) ->
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
    RT = {lists:flatten(["prior ", FieldsStr])
        , NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROFILE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'profile', Profile} = ST) ->
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
    RT = {lists:flatten([" profile ", binary_to_list(Profile), " "]),
        NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% QUOTAS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'quotas', Quotas} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {QuotaStr, NewCtx1}
        = lists:foldl(
        fun(Quota, {Str, AccCtx}) ->
            {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl + 1, Quota),
            {Str ++ NewStr, NewAccCtx}
        end, {"", NewCtx}, Quotas),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {QuotaStr ++ " ",
        NewCtx2},
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
        case S of
            S when is_binary(S) -> {Acc ++ [binary_to_list(S)], Fun(S, CtxAcc)};
            S ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, S),
                {Acc ++ [SubAcc], CtxAcc1}
        end
    end,
        {[], NewCtx1},
        Sel),
    {VarStr, NewCtx3} = lists:foldl(fun({param, V}, {Acc, CtxAcc}) ->
        case V of
            V when is_binary(V) -> {Acc ++ [binary_to_list(V)], Fun(V, CtxAcc)};
            V ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, V),
                {Acc ++ [SubAcc], CtxAcc1}
        end
    end,
        {[], NewCtx2},
        Var),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {" " ++ atom_to_list(R) ++ " " ++ string:join(SelStr, ",")
        ++ " INTO " ++
        string:join(VarStr, ",")
        , NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(_FType, Fun, Ctx, _Lvl, {R, {}} = _ST)
    when R =:= return; R =:= returning ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, _ST]),
    RT = {""
        , Fun(R, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REVOKE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'revoke', Objs, {OnTyp, On}, {'from', Tos}, Opts} = ST)
    when is_atom(OnTyp), is_atom(Opts) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [atom_to_list(O)], Fun(O, CtxAcc)}
    end,
        {[], NewCtx},
        Objs),
    NewCtx2 = Fun(OnTyp, NewCtx1),
    NewCtx3 = Fun(On, NewCtx2),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [binary_to_list(O)], Fun(O, CtxAcc)}
    end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    NewCtx6 = case FType of
        top_down -> NewCtx5;
        bottom_up -> Fun(ST, NewCtx5)
    end,
    RT = {"revoke "
        ++ string:join(ObjsStr, ",") ++ " "
        ++ if On =/= <<"">> ->
        atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
        ++ if length(Tos) > 0 ->
        "from " ++ string:join(TosStr, ",") ++ " "; true -> "" end
        ++ atom_to_list(Opts)
        , NewCtx6},
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
    RT = {lists:flatten([atom_to_list(Role), " ",
        string:join([binary_to_list(R) || R <- Roles], ",")]),
        NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SCOPE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'scope', S} = ST)
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
    RT = {lists:flatten([" ", binary_to_list(S), " "]),
        NewCtx2},
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
    RT = {lists:flatten([" start with ", StartWithStr])
        , NewCtx2},
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
        {Acc ++ [SubAcc], CtxAcc1}
    end,
        {[], NewCtx1},
        Joins),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    RT = {TabStr ++ JoinsStr, NewCtx3},
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
    RT = {"truncate table " ++ binary_to_list(Tbl) ++ " " ++
        case Mvl of
            {} -> "";
            {'materialized view log', T} ->
                lists:flatten([atom_to_list(T), " materialized view log "])
        end
        ++
        case Storage of
            {} -> "";
            {'storage', T} -> lists:flatten([atom_to_list(T), " storage"])
        end,
        NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TYPE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {'type', T} = ST)
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
    RT = {lists:flatten([" ", if is_binary(T) -> binary_to_list(T);
        true -> atom_to_list(T)
    end, " "]),
        NewCtx2},
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
    RT = {lists:flatten(["(", AStr, " union ", BStr, ")"]), NewCtx3},
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
    RT = {"quota unlimited on " ++ binary_to_list(T) ++ " ",
        NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
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
    RT = {"quota unlimited on " ++ binary_to_list(T) ++ " ",
        NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UPDATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'update', Tbl, {set, Set}, Where, Return} = ST)
    when is_binary(Tbl) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Tbl, NewCtx),
    {Sets, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, S),
        {Acc ++ [SubAcc], CtxAcc1}
    end,
        {[], NewCtx1},
        Set),
    {WhereStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
    {ReturnStr, NewCtx4} = fold(FType, Fun, NewCtx3, Lvl + 1, Return),
    NewCtx5 = case FType of
        top_down -> NewCtx4;
        bottom_up -> Fun(ST, NewCtx4)
    end,
    RT = {"update " ++ binary_to_list(Tbl)
        ++ " set " ++ string:join(Sets, ",")
        ++ if length(WhereStr) > 0 orelse length(ReturnStr) > 0 ->
            " " ++ WhereStr ++ ReturnStr;
            true -> ""
        end,
        NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USING
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {using, ColumnList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ColumnListStr, NewCtx1} = lists:foldl(fun(C, {Acc, CtxAcc}) ->
        {Acc ++ [binary_to_list(C)], Fun(C, CtxAcc)}
    end,
        {[], NewCtx},
        ColumnList),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {" using (" ++ string:join(ColumnListStr, ",") ++ ")", NewCtx2},
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
    RT = {" where current of " ++ CurName, NewCtx1},
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
% PL/SQL concatenate operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {'||', Args} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ArgsStr, NewCtx1} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
        case A of
            A when is_binary(A) -> {Acc ++ [binary_to_list(A)], Fun(A, CtxAcc)};
            A ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, A),
                {Acc ++ [SubAcc], CtxAcc1}
        end
    end,
        {[], NewCtx},
        Args),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    RT = {string:join(ArgsStr, " || "), NewCtx2},
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
    RT = {binary_to_list(JPPath), Ctx},
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
    {Fl, NewCtx1} = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' ->
            {Ls, NC1} = fold(FType, Fun, NewCtx, Lvl + 1, L),
            {lists:flatten(["(", Ls, ")"]), NC1};
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' ->
            {Ls, NC1} = fold(FType, Fun, NewCtx, Lvl + 1, L),
            {lists:flatten(["(", Ls, ")"]), NC1};
        {'and', 'or'} -> {Ls, NC1} = fold(FType, Fun, NewCtx, Lvl + 1, L),
            {lists:flatten(["(", Ls, ")"]), NC1};
        _ -> fold(FType, Fun, NewCtx, Lvl + 1, L)
    end,
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' ->
            {Rs, NC2} = fold(FType, Fun, NewCtx2, Lvl + 1, R),
            {lists:flatten(["(", Rs, ")"]), NC2};
        {'/', Or} when Or =:= '-'; Or =:= '+' ->
            {Rs, NC2} = fold(FType, Fun, NewCtx2, Lvl + 1, R),
            {lists:flatten(["(", Rs, ")"]), NC2};
        {'and', 'or'} -> {Rs, NC2} = fold(FType, Fun, NewCtx2, Lvl + 1, R),
            {lists:flatten(["(", Rs, ")"]), NC2};
        _ -> fold(FType, Fun, NewCtx2, Lvl + 1, R)
    end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {lists:flatten([Fl, " ", atom_to_list(Op), " ", Fr])
        , NewCtx4},
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
    {Fr, NewCtx3} = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' ->
            {Rs, NC} = fold(FType, Fun, NewCtx2, Lvl + 1, R),
            {lists:flatten(["(", Rs, ")"]), NC};
        {'/', Or} when Or =:= '-'; Or =:= '+' ->
            {Rs, NC} = fold(FType, Fun, NewCtx2, Lvl + 1, R),
            {lists:flatten(["(", Rs, ")"]), NC};
        _ -> fold(FType, Fun, NewCtx2, Lvl + 1, R)
    end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", Fr])
        , NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_tuple(L), is_binary(R), Op /= 'connect by' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {Fl, NewCtx1} = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' ->
            {Ls, NC} = fold(FType, Fun, NewCtx, Lvl + 1, L),
            {lists:flatten(["(", Ls, ")"]), NC};
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' ->
            {Ls, NC} = fold(FType, Fun, NewCtx, Lvl + 1, L),
            {lists:flatten(["(", Ls, ")"]), NC};
        _ -> fold(FType, Fun, NewCtx, Lvl + 1, L)
    end,
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    RT = {lists:flatten([Fl, " ", atom_to_list(Op), " ", binary_to_list(R)])
        , NewCtx4},
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
    RT = {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", binary_to_list(R)])
        , NewCtx4},
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
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p~n ST: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
