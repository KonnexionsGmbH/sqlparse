%% -----------------------------------------------------------------------------
%%
%% sqlparse_fold.erl: SQL - utilities.
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

-module(sqlparse_fold).

-export([fold/7]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-include("sql_lex.hrl").

%% Allowed values: init_cap, keep_unchanged, lower,upper -----------------------
-define(CASE_IDENTIFIER, list_to_atom(
    string:to_lower(os:getenv("CASE_IDENTIFIER", "keep_unchanged")))).
%% Allowed values: init_cap, lower,upper ---------------------------------------
-define(CASE_KEYWORD, list_to_atom(
    string:to_lower(os:getenv("CASE_KEYWORD", "upper")))).

-define(CHAR_NEWLINE, case os:type() of
                          {unix, _} -> "\n";
                          _ -> "\r\n"
                      end).
-define(CHAR_TAB, "\t").

%% Allowed values: 1 ... n -----------------------------------------------------
-define(CR_LIMIT_ALTER_ROLES, list_to_integer(
    os:getenv("CR_LIMIT_ALTER_ROLES", "3"))).
-define(CR_LIMIT_ALTER_USERS, list_to_integer(
    os:getenv("CR_LIMIT_ALTER_USERS", "3"))).
-define(CR_LIMIT_CREATE_INDEX, list_to_integer(
    os:getenv("CR_LIMIT_CREATE_INDEX", "3"))).
-define(CR_LIMIT_DROP_TABLE, list_to_integer(
    os:getenv("CR_LIMIT_DROP_TABLE", "3"))).
-define(CR_LIMIT_FUNC_ARGS, list_to_integer(
    os:getenv("CR_LIMIT_FUNC_ARGS", "3"))).
-define(CR_LIMIT_GRANT_GRANTEE, list_to_integer(
    os:getenv("CR_LIMIT_GRANT_GRANTEE", "3"))).
-define(CR_LIMIT_GRANT_PRIVILEGE, list_to_integer(
    os:getenv("CR_LIMIT_GRANT_PRIVILEGE", "3"))).
-define(CR_LIMIT_GROUP_BY, list_to_integer(
    os:getenv("CR_LIMIT_GROUP_BY", "3"))).
-define(CR_LIMIT_INSERT, list_to_integer(os:getenv("CR_LIMIT_INSERT", "3"))).
-define(CR_LIMIT_INTO, list_to_integer(os:getenv("CR_LIMIT_INTO", "3"))).
-define(CR_LIMIT_ORDER_BY, list_to_integer(
    os:getenv("CR_LIMIT_ORDER_BY", "3"))).
-define(CR_LIMIT_PARTITION, list_to_integer(
    os:getenv("CR_LIMIT_PARTITION", "3"))).
-define(CR_LIMIT_RETURNING, list_to_integer(
    os:getenv("CR_LIMIT_RETURNING", "3"))).
-define(CR_LIMIT_REVOKE_PRIVILEGE, list_to_integer(
    os:getenv("CR_LIMIT_REVOKE_PRIVILEGE", "3"))).
-define(CR_LIMIT_REVOKE_REVOKEE, list_to_integer(
    os:getenv("CR_LIMIT_REVOKE_REVOKEE", "3"))).
-define(CR_LIMIT_SELECT, list_to_integer(os:getenv("CR_LIMIT_SELECT", "3"))).
-define(CR_LIMIT_USING, list_to_integer(os:getenv("CR_LIMIT_USING", "3"))).
-define(CR_LIMIT_VIEW, list_to_integer(os:getenv("CR_LIMIT_VIEW", "3"))).

-define(DATA_TYPES, [
    "bfile",
    "binary_double",
    "binary_float",
    "blob",
    "clob",
    "char",
    "date",
    "integer",
    "long",
    "nchar",
    "nclob",
    "number",
    "nvarchar2",
    "raw",
    "rowid",
    "timestamp",
    "urowid",
    "varchar2"
]).

%% Allowed values: 1 - 8 -------------------------------------------------------
-define(INDENT_SPACES, list_to_integer(os:getenv("INDENT_SPACES", "4"))).
%% Allowed values: spaces, tab -------------------------------------------------
-define(INDENT_WITH, list_to_atom(
    string:to_lower(os:getenv("INDENT_WITH", "spaces")))).

-define(OBJECT_PRIVILEGES, [
    "all",
    "all privileges",
    "alter",
    "delete",
    "execute",
    "index",
    "insert",
    "references",
    "select",
    "update"
]).

-define(SYSTEM_PRIVILEGES, [
    "admin",
    "alter any index",
    "alter any materialized view",
    "alter any table",
    "alter any view",
    "create any index",
    "create any materialized view",
    "create any table",
    "create any view",
    "create materialized view",
    "create table",
    "create view",
    "delete any table",
    "drop any index",
    "drop any materialized view",
    "drop any table",
    "drop any view",
    "insert any table",
    "select any table",
    "update any table"
]).

-define(TABLE_OPTIONS, [
    "bag",
    "cluster",
    "local",
    "ordered_set",
    "schema",
    "set"
]).

%% Allowed values: false, true -------------------------------------------------
-define(WS_OPERATORS, list_to_atom(
    string:to_lower(os:getenv("WS_OPERATORS", "true")))).

-record(state, {
    function_level,
    indentation_level,
    select_clause,
    statement
}).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of parsetrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, {}, FType, Fun, Ctx, Lvl, [{_, {extra, _}} | _] = STs)
    when is_list(STs) ->
    ?debugFmt(
        ?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n FType: ~p~n",
        [Format, Lvl, {}, STs, FType]),
    NewCtx = case FType of
                 top_down -> Fun(STs, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1}
        = lists:foldl(
        fun(ST, {Sql, AccCtx}) ->
            State = #state{
                function_level = 1,
                indentation_level = 1,
                select_clause = none,
                statement = case ST of
                                {{select, _}, _} -> select;
                                {{Type, _, _}, _} when Type == intersect orelse
                                    Type == minus orelse
                                    Type == union orelse
                                    Type == 'union all' -> Type;
                                _ -> none
                            end
            },
            {NewSql, NewAccCtx} =
                fold(Format, State, FType, Fun, AccCtx, Lvl, ST),
            {lists:append([
                Sql,
                case length(Sql) > 0 of
                    true -> ";" ++ case Format of
                                       true -> ?CHAR_NEWLINE;
                                       _ -> " "
                                   end;
                    _ -> []
                end,
                NewSql
            ]), NewAccCtx}
        end, {[], NewCtx}, STs),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(STs, NewCtx1)
              end,
    RT = {SqlStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Statement list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {stmtList, STs})
    when is_list(STs) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, STs]),
    NewCtx = case FType of
                 top_down -> Fun(STs, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1}
        = lists:foldl(
        fun(ST, {Sql, AccCtx}) ->
            {NewSql, NewAccCtx} =
                fold(Format, State#state{indentation_level =
                State#state.indentation_level + 1},
                    FType, Fun, AccCtx, Lvl, ST),
            {lists:append([
                Sql,
                case length(Sql) > 0 andalso length(NewSql) > 0 of
                    true -> ";";
                    _ -> []
                end,
                case Format of
                    true -> ?CHAR_NEWLINE ++
                    format_column_pos(State#state.indentation_level);
                    _ -> []
                end,
                NewSql
            ]), NewAccCtx}
        end, {[], NewCtx}, STs),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(STs, NewCtx1)
              end,
    RT = {SqlStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other lists
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold(Format, State, FType, Fun, Ctx, Lvl, STs)
    when is_list(STs) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, STs]),
    NewCtx = case FType of
                 top_down -> Fun(STs, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1}
        = lists:foldl(
        fun(ST, {Sql, AccCtx}) ->
            {NewSql, NewAccCtx} = fold(Format,
                case ST of
                    {select, _} ->
                        State#state{indentation_level =
                        State#state.indentation_level +
                            1};
                    _ -> State
                end,
                FType, Fun, AccCtx, Lvl, ST),
            {lists:append([
                Sql,
                case length(Sql) > 0 andalso length(NewSql) > 0 of
                    true -> " ";
                    _ -> []
                end,
                NewSql
            ]), NewAccCtx}
        end, {[], NewCtx}, STs),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(STs, NewCtx1)
              end,
    RT = {SqlStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACCOUNT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl, {account, LockUnlock} = ST)
    when LockUnlock == 'lock'; LockUnlock == 'unlock' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(LockUnlock, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("account "),
                  format_keyword(LockUnlock)
              ]);
              _ -> "account " ++ atom_to_list(LockUnlock)
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of aggregate function types 3/4: ALL / DISTINCT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == all orelse Type == distinct ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueStr, NewCtx1} = case is_binary(Value) of
                              true -> {case Format of
                                           true -> format_identifier(Value);
                                           _ -> binary_to_list(Value)
                                       end, NewCtx};
                              _ -> fold(Format, State, FType, Fun, NewCtx,
                                  Lvl + 1, Value)
                          end,
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([format_keyword(Type), " ", ValueStr]);
              _ -> lists:append(
                  [atom_to_list(Type), " ", case Value of
                                                {select, _} ->
                                                    lists:append(
                                                        ["(", ValueStr, ")"]);
                                                _ -> ValueStr
                                            end])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALL / ANY / SOME
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {N, Args} = ST)
    when (N == any orelse N == all orelse N == some) andalso is_list(Args) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(N, NewCtx),
    {ArgsStr, NewCtx2} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
        case lists:member(element(1, A),
            ['select', 'insert', 'create table',
                'create user', 'alter user',
                'truncate table', 'update', 'delete',
                'grant', 'revoke']) of
            true ->
                {SubAcc, CtxAcc1} =
                    fold(Format, State#state{indentation_level =
                    State#state.indentation_level + 1},
                        FType, Fun, CtxAcc, Lvl + 1, A),
                {Acc ++ case Format of
                            true -> [string:trim(SubAcc)];
                            _ -> [lists:append(
                                ["(", string:trim(SubAcc, both, " "), ")"])]
                        end, CtxAcc1};
            _ ->
                {SubAcc, CtxAcc1} =
                    fold(Format, State, FType, Fun, CtxAcc, Lvl + 1, A),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                     end,
        {[], NewCtx1},
        Args),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:flatten(
                  [format_keyword(N), " ", lists:join(", ", ArgsStr)]);
              _ -> lists:flatten(
                  [atom_to_list(N), " (", string:join(ArgsStr, ", "), ")"])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALTER USER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'alter user', Usr, {spec, Opts}} =
    ST)
    when is_binary(Usr) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptsStr, NewCtx1} =
        lists:foldl(
            fun(Opt, {OptsS, INewCtx}) ->
                {OS, INewCtx1} =
                    fold(Format,
                        State#state{select_clause = none, statement = 'alter user'},
                        FType, Fun, INewCtx, Lvl + 1, Opt),
                {case Format of
                     true -> OptsS ++ OS;
                     _ -> lists:append([
                         OptsS,
                         case length(OptsS) == 0 of
                             true -> [];
                             _ -> " "
                         end,
                         OS
                     ])
                 end, INewCtx1}
            end, {[], NewCtx}, Opts),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("alter user"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_identifier(Usr),
                  OptsStr
              ]);
              _ -> lists:flatten(
                  ["alter user ", binary_to_list(Usr), " ", OptsStr])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, _Lvl,
    {'alter user', [Usr | _] = Users, {Grant, GrantArg}} =
        ST)
    when is_binary(Usr) andalso
    (Grant == 'grant connect' orelse Grant == 'revoke connect') ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    GrantStr = case Format of
                   true -> lists:flatten([
                       ?CHAR_NEWLINE,
                       format_column_pos(State#state.indentation_level - 1),
                       case Grant of
                           'grant connect' -> format_keyword("grant ");
                           _ -> format_keyword("revoke ")
                       end,
                       format_keyword("connect through "),
                       case is_atom(GrantArg) of
                           true -> format_keyword(GrantArg);
                           _ ->
                               case GrantArg of
                                   {W, A} when is_atom(W), is_atom(A) ->
                                       lists:append([
                                           format_keyword(W),
                                           " ",
                                           format_keyword(A)
                                       ]);
                                   {W, Roles} when is_atom(W) ->
                                       lists:append([
                                           format_keyword(W),
                                           case length(Roles) =<
                                               ?CR_LIMIT_ALTER_ROLES of
                                               true -> lists:append([
                                                   ?CHAR_NEWLINE,
                                                   format_column_pos(
                                                       State#state.indentation_level),
                                                   lists:join(", ",
                                                       [format_identifier(
                                                           R) || R <- Roles])
                                               ]);
                                               _ ->
                                                   format_commalist(
                                                       State#state.indentation_level,
                                                       [format_identifier(
                                                           R) || R <- Roles])
                                           end
                                       ]);
                                   {{W, Roles}, Authrec} when is_atom(W) ->
                                       lists:append([
                                           format_keyword(W),
                                           case length(Roles) =<
                                               ?CR_LIMIT_ALTER_ROLES of
                                               true -> lists:append([
                                                   ?CHAR_NEWLINE,
                                                   format_column_pos(
                                                       State#state.indentation_level),
                                                   lists:join(", ",
                                                       [format_identifier(
                                                           R) || R <- Roles])
                                               ]);
                                               _ ->
                                                   format_commalist(
                                                       State#state.indentation_level,
                                                       [format_identifier(
                                                           R) || R <- Roles])
                                           end,
                                           ?CHAR_NEWLINE,
                                           format_column_pos(
                                               State#state.indentation_level -
                                                   1),
                                           format_keyword(Authrec)
                                       ])
                               end
                       end]);
                   _ -> [atom_to_list(Grant), " ", "through", " ",
                       case is_atom(GrantArg) of
                           true -> atom_to_list(GrantArg);
                           _ ->
                               case GrantArg of
                                   {W, A} when is_atom(W), is_atom(A) ->
                                       [atom_to_list(W), " ", atom_to_list(A)];
                                   {W, Roles} when is_atom(W) ->
                                       [atom_to_list(W), " ",
                                           string:join([binary_to_list(
                                               R) || R <- Roles],
                                               ",")];
                                   {{W, Roles}, Authrec} when is_atom(W) ->
                                       [atom_to_list(W), " ",
                                           string:join([binary_to_list(
                                               R) || R <- Roles],
                                               ","), " ", atom_to_list(Authrec)]
                               end
                       end]
               end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("alter user"),
                  case length(Users) =< ?CR_LIMIT_ALTER_USERS of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          lists:join(", ", [format_identifier(U) || U <- Users])
                      ]);
                      _ ->
                          format_commalist(State#state.indentation_level,
                              [format_identifier(U) || U <- Users])
                  end,
                  GrantStr
              ]);
              _ ->
                  lists:flatten(["alter user ", string:join(
                      [binary_to_list(U) || U <- Users],
                      ","), " ", GrantStr])
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AS SELECT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {as, {select, _} = QuerySpec, Check} =
    ST)
    when Check == [];Check == "with check option" ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {QuerySpecStr, NewCtx1} = fold(Format,
        State#state{indentation_level = State#state.indentation_level +
            1, statement = 'create view'}, FType, Fun, NewCtx, Lvl + 1,
        QuerySpec),
    NewCtx2 = case FType of
                  top_down -> Fun(ST, NewCtx1);
                  bottom_up -> NewCtx1
              end,
    RT = {case Format of
              true -> lists:append([
                  format_keyword("as"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  QuerySpecStr,
                  case Check of
                      [] -> [];
                      _ -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          format_keyword(Check)

                      ])
                  end]);
              _ -> lists:append(["as ", QuerySpecStr, case Check of
                                                          [] -> [];
                                                          _ -> " " ++ Check
                                                      end])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All aliases
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {as, L, R} = ST)
    when is_binary(R) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Fl, NewCtx1} = case Format of
                        true -> case is_binary(L) of
                                    true -> {format_identifier(L), NewCtx};
                                    _ -> fold(Format, State, FType, Fun, NewCtx,
                                        Lvl + 1, L)
                                end;
                        _ -> case is_binary(L) of
                                 true -> {binary_to_list(L), NewCtx};
                                 _ ->
                                     {Ls, NC} =
                                         fold(Format, State, FType, Fun, NewCtx,
                                             Lvl + 1, L),
                                     case L of
                                         {select, _} ->
                                             {lists:append(["(", Ls, ")"]), NC};
                                         _ ->
                                             {Ls, NC}
                                     end
                             end
                    end,
    NewCtx2 = Fun(R, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([
        Fl,
        " ",
        case Format of
            true -> format_identifier(R);
            _ -> binary_to_list(R)
        end
    ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl, {as, L, R, {dblink, Dblink}} = ST)
    when is_binary(R) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Fl, NewCtx1} = case is_binary(L) of
                        true -> {case Format of
                                     true -> format_identifier(L);
                                     _ -> binary_to_list(L)
                                 end, NewCtx};
                        _ -> fold(Format, State, FType, Fun, NewCtx, Lvl + 1, L)
                    end,
    NewCtx2 = Fun(R, NewCtx1),
    NewCtx3 = Fun(Dblink, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([
        Fl,
        binary_to_list(Dblink),
        " ",
        case Format of
            true -> format_identifier(R);
            _ -> binary_to_list(R)
        end
    ]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, _State, _FType, Fun, Ctx, _Lvl, Tab = _ST)
    when is_binary(Tab) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, _ST]),
    RT = {case Format of
              true -> format_identifier(Tab);
              _ -> binary_to_list(Tab)
          end, Fun(Tab, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BETWEEN operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {Type, A, B, C} = ST)
    when Type == between ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {A1, NewCtx1} = case is_binary(A) of
                        true -> {case Format of
                                     true -> format_identifier(A);
                                     _ -> binary_to_list(A)
                                 end, NewCtx};
                        _ ->
                            {A1Int, NewCtx1Int} =
                                fold(Format, case A of
                                                 {select, _} ->
                                                     State#state{indentation_level =
                                                     State#state.indentation_level +
                                                         1};
                                                 {TypeA, _, _} when
                                                     TypeA == intersect orelse
                                                         TypeA == minus orelse
                                                         TypeA == union orelse
                                                         TypeA == 'union all' ->
                                                     State#state{indentation_level =
                                                     State#state.indentation_level +
                                                         2};
                                                 _ -> State
                                             end, FType, Fun, NewCtx, Lvl + 1,
                                    A),
                            case A of
                                {select, _} ->
                                    {lists:append(
                                        ["(", A1Int, ")"]), NewCtx1Int};
                                _ ->
                                    {A1Int, NewCtx1Int}
                            end
                    end,
    {B1, NewCtx2} = case is_binary(B) of
                        true -> {case Format of
                                     true -> format_identifier(B);
                                     _ -> binary_to_list(B)
                                 end, NewCtx1};
                        _ ->
                            {B1Int, NewCtx2Int} =
                                fold(Format, case B of
                                                 {select, _} ->
                                                     State#state{indentation_level =
                                                     State#state.indentation_level +
                                                         1};
                                                 {TypeB, _, _} when
                                                     TypeB == intersect orelse
                                                         TypeB == minus orelse
                                                         TypeB == union orelse
                                                         TypeB == 'union all' ->
                                                     State#state{indentation_level =
                                                     State#state.indentation_level +
                                                         2};
                                                 _ -> State
                                             end, FType, Fun, NewCtx1, Lvl + 1,
                                    B),
                            case B of
                                {select, _} ->
                                    {lists:append(
                                        ["(", B1Int, ")"]), NewCtx2Int};
                                _ ->
                                    {B1Int, NewCtx2Int}
                            end
                    end,
    {C1, NewCtx3} = case is_binary(C) of
                        true -> {case Format of
                                     true -> format_identifier(C);
                                     _ -> binary_to_list(C)
                                 end, NewCtx2};
                        _ ->
                            {C1Int, NewCtx3Int} =
                                fold(Format, case C of
                                                 {select, _} ->
                                                     State#state{indentation_level =
                                                     State#state.indentation_level +
                                                         1};
                                                 {TypeC, _, _} when
                                                     TypeC == intersect orelse
                                                         TypeC == minus orelse
                                                         TypeC == union orelse
                                                         TypeC == 'union all' ->
                                                     State#state{indentation_level =
                                                     State#state.indentation_level +
                                                         2};
                                                 _ -> State
                                             end, FType, Fun, NewCtx2, Lvl + 1,
                                    C),
                            case C of
                                {select, _} ->
                                    {lists:append(
                                        ["(", C1Int, ")"]), NewCtx3Int};
                                _ ->
                                    {C1Int, NewCtx3Int}
                            end
                    end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true ->
                  lists:append([
                      A1,
                      " ",
                      format_keyword("between "),
                      B1,
                      " ",
                      format_keyword("and "),
                      C1
                  ]);
              _ -> lists:append([A1, " between ", B1, " and ", C1])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure calls ('call ...')
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'call procedure', Function} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FunctionStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1, Function),
    NewCtx2 = case FType of
                  top_down -> Fun(ST, NewCtx1);
                  bottom_up -> NewCtx1
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("call"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  FunctionStr
              ]);
              _ -> "call " ++ FunctionStr
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'case', Expr, WhenThenList, Else} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExprStr, NewCtx1} = case is_binary(Expr) of
                             true -> {binary_to_list(Expr), NewCtx};
                             _ -> fold(Format, State, FType, Fun, NewCtx,
                                 Lvl + 1, Expr)
                         end,
    {WhenThenStr, NewCtx2}
        = lists:foldl(
        fun({When, Then}, {Sql, AccCtx}) ->
            {WhenStr, AccCtx1} =
                fold(Format, State, FType, Fun, AccCtx, Lvl + 1, When),
            {ThenStr, AccCtx2} =
                fold(Format, State, FType, Fun, AccCtx1, Lvl + 1, Then),
            {case Format of
                 true -> lists:append([
                     Sql,
                     ?CHAR_NEWLINE,
                     format_column_pos(State#state.indentation_level + 1),
                     format_keyword("when "),
                     WhenStr,
                     ?CHAR_NEWLINE,
                     format_column_pos(State#state.indentation_level + 1),
                     format_keyword("then "),
                     ThenStr
                 ]);
                 _ -> lists:append(
                     [Sql, "when ", WhenStr, " then ", ThenStr, " "])
             end, AccCtx2}
        end, {[], NewCtx1}, WhenThenList),
    {ElseStr, NewCtx3} = case Else of
                             {} -> {[], NewCtx2};
                             Else ->
                                 {EStr, NewCtx21} =
                                     fold(Format, State, FType, Fun, NewCtx2,
                                         Lvl + 1, Else),
                                 {case Format of
                                      true -> lists:append([
                                          ?CHAR_NEWLINE,
                                          format_column_pos(
                                              State#state.indentation_level +
                                                  1),
                                          format_keyword("else "),
                                          EStr
                                      ]);
                                      _ -> lists:append(["else ", EStr, " "])
                                  end, NewCtx21}
                         end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  format_keyword("case"),
                  case length(ExprStr) == 0 of
                      true -> [];
                      _ -> " " ++ ExprStr
                  end,
                  WhenThenStr,
                  ElseStr,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_keyword("end")
              ]);
              _ -> lists:flatten([
                  "case ",
                  ExprStr,
                  case length(ExprStr) == 0 of
                      true -> [];
                      _ -> " "
                  end,
                  WhenThenStr,
                  ElseStr,
                  "end"
              ])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHECK
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {check, Condition} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ConditionStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1, Condition),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["check (", ConditionStr, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLOSE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, _State, FType, Fun, Ctx, _Lvl, {close, {cur, CurName}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> format_keyword("close ");
              _ -> "close "
          end ++ CurName, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECT BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'connect by', NoCycle, ConnectBy} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NoCycleStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1, NoCycle),
    {ConnectByStr, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl + 1, ConnectBy),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("connect by"),
                  case byte_size(NoCycle) > 0 of
                      true -> " " ++ format_keyword(NoCycleStr);
                      _ -> []
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  ConnectByStr
              ]);
              _ -> lists:append([
                  "connect by ",
                  case byte_size(NoCycle) > 0 of
                      true -> NoCycleStr ++ " ";
                      _ -> []
                  end,
                  ConnectByStr
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE INDEX
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'create index', Opts, Idx, Table, Spec, Norm, Filter} =
        ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptsStr, NewCtx1} = fold(Format,
        State#state{select_clause = none, statement = 'create index'}, FType,
        Fun, NewCtx, Lvl + 1, Opts),
    {IdxStr, NewCtx2} = fold(Format,
        State#state{select_clause = none, statement = 'create index'}, FType,
        Fun, NewCtx1, Lvl + 1, Idx),
    {TableStr, NewCtx3} =
        fold(Format,
            State#state{select_clause = none, statement = 'create index'},
            FType, Fun, NewCtx2, Lvl + 1, {table, Table}),
    {Specs, NewCtx4} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} = fold(Format,
            State#state{select_clause = none, statement = 'create index'},
            FType, Fun, CtxAcc, Lvl + 1, S),
        {Acc ++ [SubAcc], CtxAcc1}
                                   end,
        {[], NewCtx3},
        Spec),
    {NormStr, NewCtx5} =
        fold(Format,
            State#state{select_clause = none, statement = 'create index'},
            FType, Fun, NewCtx4, Lvl + 1, Norm),
    {FilterStr, NewCtx6} =
        fold(Format,
            State#state{select_clause = none, statement = 'create index'},
            FType, Fun, NewCtx5, Lvl + 1, Filter),
    NewCtx7 = case FType of
                  top_down -> NewCtx6;
                  bottom_up -> Fun(ST, NewCtx6)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("create "),
                  case length(OptsStr) > 0 of
                      true -> format_keyword(OptsStr) ++ " ";
                      _ -> []
                  end,
                  format_keyword("index"),
                  case length(IdxStr) > 0 of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          IdxStr
                      ]);
                      _ -> []
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("on"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  TableStr,
                  case length(Specs) > 0 of
                      true ->
                          case length(Specs) =< ?CR_LIMIT_CREATE_INDEX of
                              true ->
                                  lists:flatten(
                                      [" (", lists:join(", ", Specs), ")"]);
                              _ -> lists:append([
                                  " (",
                                  format_commalist(
                                      State#state.indentation_level + 1, Specs),
                                  ?CHAR_NEWLINE,
                                  format_column_pos(
                                      State#state.indentation_level),
                                  ")"
                              ])
                          end;
                      _ -> []
                  end,
                  case NormStr =/= [] of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          NormStr
                      ]);
                      _ -> []
                  end,
                  case FilterStr =/= [] of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          FilterStr
                      ]);
                      _ -> []
                  end
              ]);
              _ -> lists:append([
                  "create ",
                  OptsStr,
                  case length(OptsStr) > 0 of
                      true -> " ";
                      _ -> []
                  end,
                  "index ",
                  IdxStr,
                  case length(IdxStr) > 0 of
                      true -> " ";
                      _ -> []
                  end,
                  "on ",
                  TableStr,
                  case length(Specs) > 0 of
                      true ->
                          lists:append([" (", string:join(Specs, ", "), ")"]);
                      _ -> []
                  end,
                  case NormStr =/= [] of
                      true -> " ";
                      _ -> []
                  end,
                  NormStr,
                  case FilterStr =/= [] of
                      true -> " ";
                      _ -> []
                  end,
                  FilterStr
              ])
          end, NewCtx7},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE ROLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'create role', Role} = ST)
    when is_binary(Role) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RoleStr, NewCtx1} = fold(Format,
        State#state{select_clause = none, statement = 'create role'}, FType,
        Fun, NewCtx, Lvl + 1, Role),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("create role"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  RoleStr
              ]);
              _ -> "create role " ++ RoleStr
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE SCHEMA AUTHORIZATION
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'create schema authorization', User, SchemaElements} =
        ST)
    when is_list(SchemaElements) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(User, NewCtx),
    {SchemaElementsStr, NewCtx2} = lists:foldl(fun(SE, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} =
            fold(Format,
                State#state{select_clause = none, statement = 'create schema authorization'},
                FType, Fun, CtxAcc, Lvl + 1, SE),
        {lists:append([
            Acc,
            case length(Acc) == 0 of
                true -> [];
                _ -> " "
            end,
            SubAcc
        ]), CtxAcc1}
                                               end,
        {[], NewCtx1},
        SchemaElements),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  "create schema authorization",
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  User,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  SchemaElementsStr
              ]);
              _ -> lists:append([
                  "create schema authorization ",
                  User,
                  " ",
                  SchemaElementsStr
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'create table', Table, Fields, Opts} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptsStr, NewCtx1}
        = lists:foldl(
        fun(Opt, {Str, AccCtx}) ->
            {NewStr, NewAccCtx} =
                fold(Format,
                    State#state{select_clause = none, statement = 'create table'},
                    FType, Fun, AccCtx, Lvl + 1, Opt),
            {case Format of
                 true -> lists:append([
                     case length(Str) == 0 of
                         true -> [];
                         _ -> Str ++ " "
                     end,
                     case lists:member(NewStr, ?TABLE_OPTIONS) of
                         true -> format_keyword(NewStr);
                         _ -> format_identifier(NewStr)
                     end
                 ]);
                 _ -> lists:append([
                     Str,
                     case length(Str) == 0 of
                         true -> [];
                         _ -> " "
                     end,
                     NewStr
                 ])
             end, NewAccCtx}
        end, {[], NewCtx}, Opts),
    NewCtx2 = Fun(Table, NewCtx1),
    {Clms, NewCtx3} = lists:foldl(fun(Clm, {Acc, CtxAcc}) ->
        ?debugFmt(?MODULE_STRING ++ ":fold ===>~n Clm: ~p~n", [Clm]),
        case Clm of
            {C, {T, N, N1}, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                CtxAcc3 = Fun(N, CtxAcc2),
                CtxAcc4 = Fun(N1, CtxAcc3),
                {SubAcc, CtxAcc5} =
                    fold(Format,
                        State#state{select_clause = none, statement = 'create table'},
                        FType, Fun, CtxAcc4, Lvl + 1, O),
                {Acc ++ [binary_to_list(list_to_binary(
                    case Format of
                        true -> [
                            format_identifier(C),
                            " ",
                            format_data_type(T),
                            "(",
                            N,
                            ",",
                            N1,
                            ")",
                            case length(SubAcc) == 0 of
                                true -> [];
                                _ -> " " ++ SubAcc
                            end
                        ];
                        _ -> [C, " ", T, "(", N, ",", N1, ")", SubAcc]
                    end
                ))], CtxAcc5};
            {C, {T, N}, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                CtxAcc3 = Fun(N, CtxAcc2),
                {SubAcc, CtxAcc4} =
                    fold(Format,
                        State#state{select_clause = none, statement = 'create table'},
                        FType, Fun, CtxAcc3, Lvl + 1, O),
                {Acc ++ [binary_to_list(list_to_binary(
                    case Format of
                        true -> [
                            format_identifier(C),
                            " ",
                            format_data_type(T),
                            "(",
                            N,
                            ")",
                            case length(SubAcc) == 0 of
                                true -> [];
                                _ -> " " ++ SubAcc
                            end
                        ];
                        _ -> [C, " ", T, "(", N, ")", SubAcc]
                    end
                ))], CtxAcc4};
            {C, T, O} when is_binary(C) ->
                CtxAcc1 = Fun(C, CtxAcc),
                CtxAcc2 = Fun(T, CtxAcc1),
                {SubAcc, CtxAcc3} =
                    fold(Format,
                        State#state{select_clause = none, statement = 'create table'},
                        FType, Fun, CtxAcc2, Lvl + 1, O),
                {Acc ++ [binary_to_list(list_to_binary(
                    case Format of
                        true -> [
                            format_identifier(C),
                            " ",
                            format_data_type(T),
                            case length(SubAcc) == 0 of
                                true -> [];
                                _ -> " " ++ SubAcc
                            end
                        ];
                        _ -> [C, " ", T, " ", SubAcc]
                    end
                ))], CtxAcc3};
            Clm ->
                {SubAcc, CtxAcc1} = fold(Format,
                    State#state{indentation_level =
                    State#state.indentation_level +
                        1, select_clause = none, statement = 'create table'},
                    FType, Fun, CtxAcc, Lvl + 1,
                    Clm),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                  end,
        {[], NewCtx2},
        Fields),
    {TableStr, NewCtx4} =
        fold(Format,
            State#state{select_clause = none, statement = 'create table'},
            FType, Fun, NewCtx3, Lvl + 1, {table, Table}),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("create "),
                  case length(OptsStr) > 0 of
                      true -> OptsStr ++ " ";
                      _ -> []
                  end,
                  format_keyword("table"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  TableStr,
                  " (",
                  format_commalist(State#state.indentation_level + 1, Clms),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  ")"
              ]);
              _ -> lists:flatten([
                  "create ",
                  OptsStr,
                  case length(OptsStr) > 0 of
                      true -> " ";
                      _ -> []
                  end,
                  "table ",
                  TableStr,
                  " (",
                  string:join(Clms, ", "),
                  ")"
              ])
          end, NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE USER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'create user', Usr, Id, Opts} = ST)
    when is_binary(Usr) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Usr, NewCtx),
    {IdStr, NewCtx2} = fold(Format,
        State#state{select_clause = none, statement = 'create user'}, FType,
        Fun, NewCtx1, Lvl + 1, Id),
    {OptsStr, NewCtx3}
        = case Opts of
              [] -> {[], NewCtx2};
              _ -> lists:foldl(
                  fun(Opt, {Str, AccCtx}) ->
                      {NewStr, NewAccCtx} =
                          fold(Format,
                              State#state{select_clause = none, statement = 'create user'},
                              FType, Fun, AccCtx, Lvl + 1, Opt),
                      {lists:append([
                          Str,
                          case length(Str) == 0 of
                              true -> [];
                              _ -> " "
                          end,
                          NewStr
                      ]), NewAccCtx}
                  end, {[], NewCtx2}, Opts)
          end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("create user"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_identifier(Usr),
                  " ",
                  IdStr,
                  case OptsStr of
                      [] ->
                          [];
                      _ ->
                          " " ++ OptsStr
                  end
              ]);
              _ -> lists:append(["create user ",
                  binary_to_list(Usr),
                  " ",
                  IdStr,
                  case OptsStr of
                      [] ->
                          [];
                      _ ->
                          " " ++ OptsStr
                  end
              ])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE VIEW
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'create view', Table, Columns, QuerySpec} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format,
            State#state{select_clause = none, statement = 'create view'}, FType,
            Fun, NewCtx, Lvl + 1, {table, Table}),
    {Clms, NewCtx2} = case Columns of
                          [] -> {[], NewCtx1};
                          _ -> lists:foldl(fun(Clm, {Acc, CtxAcc}) ->
                              {SubAcc, CtxAcc1} =
                                  fold(Format,
                                      State#state{select_clause = none, statement = 'create view'},
                                      FType, Fun, CtxAcc,
                                      Lvl + 1, Clm),
                              {Acc ++ [SubAcc], CtxAcc1}
                                           end,
                              {[], NewCtx1}, Columns)
                      end,
    {QuerySpecStr, NewCtx3} =
        fold(Format,
            State#state{select_clause = none, statement = 'create view'}, FType,
            Fun, NewCtx2, Lvl + 1, QuerySpec),
    NewCtx4 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("create view"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  TableStr,
                  case length(Clms) == 0 of
                      true -> [];
                      _ -> case length(Clms) =< ?CR_LIMIT_VIEW of
                               true -> lists:append([
                                   lists:flatten(
                                       [" (", lists:join(", ", Clms), ")"])
                               ]);
                               _ -> lists:append([
                                   " (",
                                   format_commalist(
                                       State#state.indentation_level, Clms),
                                   ?CHAR_NEWLINE,
                                   format_column_pos(
                                       State#state.indentation_level),
                                   ")"
                               ])
                           end
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  QuerySpecStr
              ]);
              _ -> lists:append([
                  "create view ",
                  TableStr,
                  case Clms of
                      [] -> [];
                      _ ->
                          lists:append([" (", string:join(Clms, ", "), ")"])
                  end,
                  " ",
                  QuerySpecStr
              ])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CURSOR
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {cursor_def, {cur, CurName}, Stmt} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StmtStr, NewCtx1} = fold(Format,
        State#state{select_clause = none, statement = 'cursor_der'}, FType, Fun,
        NewCtx, Lvl + 1, Stmt),
    NewCtx2 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true ->
                  lists:append([
                      format_keyword("cursor "),
                      CurName,
                      " ",
                      format_keyword("is ("),
                      StmtStr,
                      ")"
                  ]);
              _ -> lists:append(["cursor ", CurName, " is (", StmtStr, ")"])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure calls ('begin procedure')
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {D, StmtList} = ST)
    when D =:= 'begin procedure' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {BodyStr, NewCtx1} =
        fold(Format, State#state{select_clause = none, statement = D}, FType,
            Fun, NewCtx, Lvl + 1,
            {stmtList, StmtList}),
    RT = {case Format of
              true -> lists:append([
                  format_keyword("begin"),
                  BodyStr,
                  ";",
                  ?CHAR_NEWLINE,
                  format_keyword("end")
              ]);
              _ -> lists:append([
                  "begin ",
                  BodyStr,
                  "; end"
              ])
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {default, Def} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {DefStr, NewCtx1} = fold(Format, State, FType, Fun, NewCtx, Lvl + 1, Def),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_keyword("default "),
                  case Def of
                      Def when is_binary(Def) -> format_identifier(Def);
                      _ -> DefStr
                  end
              ]);
              _ -> lists:append([
                  "default ",
                  case Def of
                      Def when is_binary(Def) -> binary_to_list(Def);
                      _ -> DefStr
                  end
              ])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT TABLESPACE / TEMPORARY TABLESPACE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl, {TS, Tab} = ST)
    when TS == 'default tablespace'; TS == 'temporary tablespace' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Tab, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword(TS),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_identifier(Tab)
              ]);
              _ -> lists:append([atom_to_list(TS), " ", binary_to_list(Tab)])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DELETE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {delete, Table, Where, Return} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format, State#state{select_clause = none, statement = delete},
            FType, Fun, NewCtx, Lvl + 1, {table, Table}),
    {WhereStr, NewCtx2} =
        fold(Format, State#state{select_clause = none, statement = delete},
            FType, Fun, NewCtx1, Lvl + 1, Where),
    {ReturnStr, NewCtx3} =
        case Return of
            {_, {}} -> {[], NewCtx2};
            _ -> fold(Format,
                State#state{select_clause = none, statement = delete}, FType,
                Fun, NewCtx2, Lvl + 1, Return)
        end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("delete from"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  TableStr,
                  WhereStr,
                  ReturnStr
              ]);
              _ -> lists:append(["delete from ",
                  TableStr,
                  case WhereStr of
                      [] -> [];
                      _ -> " " ++ WhereStr
                  end,
                  case ReturnStr of
                      [] -> [];
                      _ -> " " ++ ReturnStr
                  end
              ])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP INDEX
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl, {'drop index', Indx, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("drop index"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_identifier(Indx)
              ]);
              _ -> "drop index " ++ binary_to_list(Indx)
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl, {'drop index', Indx, Table} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format,
            State#state{select_clause = none, statement = 'drop index'}, FType,
            Fun, NewCtx, Lvl + 1, {table, Table}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("drop index"),
                  case Indx == {} of
                      true -> [];
                      _ -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          format_identifier(Indx)
                      ])
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("from"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_identifier(TableStr)
              ]);
              _ -> lists:append([
                  "drop index ",
                  case Indx == {} of
                      true -> "from ";
                      _ ->
                          binary_to_list(Indx) ++ " from "
                  end,
                  TableStr
              ])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP ROLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'drop role', Role} = ST)
    when is_binary(Role) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RoleStr, NewCtx1} =
        fold(Format, State#state{select_clause = none, statement = 'drop role'},
            FType, Fun, NewCtx, Lvl + 1, Role),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("drop role"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_identifier(RoleStr)
              ]);
              _ -> "drop role " ++ RoleStr
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'drop table', {tables, Tables}, E, RC, Types} = ST)
    when (is_atom(RC) orelse (RC =:= {})) andalso
    (is_atom(E) orelse (E =:= {})) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(E, NewCtx),
    {TablesList, NewCtx2} = lists:foldl(fun(T, {Acc, CtxAcc}) ->
        CtxAcc1 = Fun(T, CtxAcc),
        {TNew, _} =
            fold(Format,
                State#state{select_clause = none, statement = 'drop table'},
                FType, Fun, NewCtx, Lvl + 1, {table, T}),
        {Acc ++ [TNew], CtxAcc1}
                                        end,
        {[], NewCtx1},
        Tables),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("drop"),
                  case length(Types) > 0 of
                      true -> " " ++ format_identifier(Types);
                      _ -> []
                  end,
                  " ",
                  format_keyword("table"),
                  case E =:= exists of
                      true -> lists:append([
                          " ",
                          format_keyword("if exists")
                      ]);
                      _ -> []
                  end,
                  case length(TablesList) =< ?CR_LIMIT_DROP_TABLE of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          lists:join(", ", TablesList)
                      ]);
                      _ ->
                          format_commalist(State#state.indentation_level,
                              TablesList)
                  end,
                  case is_atom(RC) of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          format_keyword(RC)
                      ]);
                      _ -> []
                  end
              ]);
              _ -> lists:flatten([
                  "drop ",
                  Types,
                  case length(Types) > 0 of
                      true -> " ";
                      _ -> []
                  end,
                  "table ",
                  case E =:= exists of
                      true -> "if exists ";
                      _ -> []
                  end,
                  string:join(TablesList, ", "),
                  case is_atom(RC) of
                      true -> " " ++ atom_to_list(RC);
                      _ -> []
                  end
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP USER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'drop user', Usr, Opts} = ST)
    when is_binary(Usr) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Usr, NewCtx),
    {OptsStr, NewCtx2} =
        fold(Format, State#state{select_clause = none, statement = 'drop user'},
            FType, Fun, NewCtx1, Lvl + 1, Opts),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("drop user"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  format_identifier(Usr),
                  case Opts of
                      [] -> [];
                      _ -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          format_keyword(OptsStr)
                      ])
                  end
              ]);
              _ -> lists:append(
                  ["drop user ", binary_to_list(Usr), " ", OptsStr])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXISTS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {exists, Sql} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1} = case Sql of
                            {select, _} ->
                                fold(Format,
                                    State#state{indentation_level =
                                    State#state.indentation_level + 1}, FType,
                                    Fun,
                                    NewCtx, Lvl + 1, Sql);
                            _ ->
                                {SqlStrInner, NewCtx11} = fold(Format,
                                    State#state{indentation_level =
                                    State#state.indentation_level + 1}, FType,
                                    Fun,
                                    NewCtx, Lvl + 1, Sql),
                                {lists:append(
                                    ["(", SqlStrInner, ")"]), NewCtx11}
                        end,
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> format_keyword("exists ");
              _ -> "exists "
          end ++ SqlStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of Extra part
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {Pt, {extra, Bin}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SqlStr, NewCtx1} = fold(Format, State, FType, Fun, NewCtx, Lvl, Pt),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {SqlStr ++
        case Bin /= <<>> of
            true -> lists:append([
                ";",
                case Format of
                    true -> ?CHAR_NEWLINE ++
                    format_column_pos(State#state.indentation_level - 1);
                    _ -> []
                end,
                binary_to_list(Bin)
            ]);
            _ -> []
        end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cursor statements: FETCH
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {fetch, {cur, CurName}, IntoST} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntoStr, NewCtx1} =
        fold(Format, State#state{select_clause = none, statement = fetch},
            FType, Fun, Ctx, Lvl + 1, IntoST),
    NewCtx2 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(case Format of
                           true -> [format_keyword(
                               "fetch "), CurName, " ", IntoStr];
                           _ -> ["fetch ", CurName, " ", IntoStr]
                       end), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIELDS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {fields, Fields} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        ?debugFmt(?MODULE_STRING ++ ":fold ===>~n F: ~p~n", [F]),
        case F of
            F when is_binary(F) -> {Acc ++ [case Format of
                                                true -> format_identifier(F);
                                                _ -> binary_to_list(F)
                                            end], Fun(F, CtxAcc)};
            {as, {T, _, _}, _} = F when
                T == intersect orelse T == minus orelse T == union orelse
                    T == 'union all' ->
                {SubAcc, CtxAcc1} = fold(Format,
                    State#state{indentation_level =
                    State#state.indentation_level + 1}, FType, Fun, CtxAcc,
                    Lvl + 1, F),
                {Acc ++ [SubAcc], CtxAcc1};
            {T, _, _} = F when
                T == intersect orelse T == minus orelse T == union orelse
                    T == 'union all' ->
                {SubAcc, CtxAcc1} = fold(Format,
                    State#state{indentation_level =
                    State#state.indentation_level + 1}, FType, Fun, CtxAcc,
                    Lvl + 1, F),
                {Acc ++ [SubAcc], CtxAcc1};
            {select, _} ->
                {SubAcc, CtxAcc1} = fold(Format,
                    State#state{indentation_level =
                    State#state.indentation_level + 1}, FType, Fun, CtxAcc,
                    Lvl + 1, F),
                {Acc ++ [SubAcc], CtxAcc1};
            {as, {select, _}, _} ->
                {SubAcc, CtxAcc1} = fold(Format,
                    State#state{indentation_level =
                    State#state.indentation_level + 1}, FType, Fun, CtxAcc,
                    Lvl + 1, F),
                {Acc ++ [SubAcc], CtxAcc1};
            Other ->
                {SubAcc, CtxAcc1} =
                    fold(Format, State, FType, Fun, CtxAcc, Lvl + 1, Other),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                       end,
        {[], NewCtx},
        Fields),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  case is_simple_list(Fields, ?CR_LIMIT_SELECT) of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          lists:join(", ", FieldsStr)
                      ]);
                      _ -> format_commalist(State#state.indentation_level,
                          FieldsStr)
                  end
              ]);
              _ -> columns_join(FieldsStr, ", ", [])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Index norm or filter
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl, {FunType, FunBody} = ST)
    when FunType =:= norm; FunType =:= filter ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    FunHead = case FunType of
                  norm -> "norm_with";
                  filter -> "filter_with"
              end,
    RT = {case Format of
              true -> lists:append([
                  format_keyword(FunHead),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  binary_to_list(FunBody)
              ]);
              _ -> lists:append([FunHead, " ", binary_to_list(FunBody)])
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FOREIGN KEY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'foreign key', ClmList, {ref, Ref}} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ColStrList, NewCtx1} =
        lists:foldl(
            fun(Clm, {StrList, ICtx}) ->
                {CStr, ICtx1} =
                    fold(Format, State, FType, Fun, ICtx, Lvl + 1, Clm),
                {[CStr | StrList], ICtx1}
            end, {[], NewCtx}, ClmList),
    ClmStr = lists:join(", ", lists:reverse(ColStrList)),
    {RefStr, NewCtx4} = case Ref of
                            {Table, TblClmList} when is_list(TblClmList) ->
                                {TableStr, NewCtx2} =
                                    fold(Format, State, FType, Fun, NewCtx1,
                                        Lvl + 1,
                                        {table, Table}),
                                {TblColStrList, NewCtx3} =
                                    lists:foldl(
                                        fun(Clm, {StrList, ICtx}) ->
                                            {CStr, ICtx1} =
                                                fold(Format, State, FType, Fun,
                                                    ICtx, Lvl + 1, Clm),
                                            {[CStr | StrList], ICtx1}
                                        end, {[], NewCtx2}, TblClmList),
                                {lists:flatten([TableStr, " (", lists:join(", ",
                                    lists:reverse(
                                        TblColStrList)), ")"]), NewCtx3};
                            _ ->
                                fold(Format, State, FType, Fun, NewCtx1,
                                    Lvl + 1, {table, Ref})
                        end,
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_keyword("foreign key ("),
                  ClmStr,
                  format_keyword(") references "),
                  RefStr
              ]);
              _ -> lists:append(
                  ["foreign key (", ClmStr, ") references ", RefStr])
          end, NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FROM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {from, Froms} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FrmStr, NewCtx1}
        = lists:foldl(
        fun(F, {Acc, CtxAcc}) ->
            ?debugFmt(?MODULE_STRING ++ ":fold ===>~n F: ~p~n", [F]),
            case F of
                {T, _, _} = F when
                    T == intersect orelse T == minus orelse T == union orelse
                        T == 'union all' ->
                    {FoldFStr, CtxAcc1} = fold(Format,
                        State#state{indentation_level =
                        State#state.indentation_level + 1}, FType, Fun, CtxAcc,
                        Lvl + 1,
                        F),
                    {Acc ++ [FoldFStr], CtxAcc1};
                {select, _} = F ->
                    {FoldFStr, CtxAcc1} = fold(Format,
                        State#state{indentation_level =
                        State#state.indentation_level + 1}, FType, Fun, CtxAcc,
                        Lvl + 1,
                        F),
                    {Acc ++ [FoldFStr], CtxAcc1};
                {T, J} = F when is_binary(T), is_list(J) ->
                    {FoldFStr, CtxAcc1} =
                        fold(Format, State, FType, Fun, CtxAcc, Lvl + 1, F),
                    {Acc ++ case Format of
                                true -> FoldFStr;
                                _ -> [FoldFStr]
                            end, CtxAcc1};
                {{as, _, _}, J} = F when is_list(J) ->
                    {FoldFStr, CtxAcc1} =
                        fold(Format, State, FType, Fun, CtxAcc, Lvl + 1, F),
                    {Acc ++ case Format of
                                true -> FoldFStr;
                                _ -> [FoldFStr]
                            end, CtxAcc1};
                {{as, _, _, _}, J} = F when is_list(J) ->
                    {FoldFStr, CtxAcc1} =
                        fold(Format, State, FType, Fun, CtxAcc, Lvl + 1, F),
                    {Acc ++ case Format of
                                true -> FoldFStr;
                                _ -> [FoldFStr]
                            end, CtxAcc1};
                {{param, _}, J} = F when is_list(J) ->
                    {FoldFStr, CtxAcc1} =
                        fold(Format, State, FType, Fun, CtxAcc, Lvl + 1, F),
                    {Acc ++ case Format of
                                true -> FoldFStr;
                                _ -> [FoldFStr]
                            end, CtxAcc1};
                Other ->
                    {SubAcc, CtxAcc1} =
                        fold(Format, State, FType, Fun, CtxAcc, Lvl + 1,
                            {table, Other}),
                    {Acc ++ case SubAcc of
                                [_, _] -> case F of
                                              {param, _} -> [SubAcc];
                                              _ -> SubAcc
                                          end;
                                [_, _, _] -> case F of
                                                 {{_, _}, [_]} -> SubAcc;
                                                 _ -> [SubAcc]
                                             end;
                                [_, _, _, _] -> case F of
                                                    {{_, _}, [_]} -> SubAcc;
                                                    _ -> [SubAcc]
                                                end;
                                _ -> [SubAcc]
                            end, CtxAcc1}
            end
        end, {[], NewCtx},
        [case Frm of
             {'as', A, B} -> {'$from_as', A, B};
             _ -> Frm
         end || Frm <- Froms]),
    {FromStr, NewCtx2} = {case Format of
                              true -> FrmStr;
                              _ -> lists:flatten(columns_join(FrmStr, ", ", []))
                          end, NewCtx1},
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("from"),
                  format_commalist(State#state.indentation_level, FromStr)
              ]);
              _ -> "from " ++ FromStr
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% Alias for from list
fold(Format, State, FType, Fun, Ctx, Lvl, {'$from_as', A, B} = ST)
    when is_binary(B) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AStr, NewCtx1}
        = case A of
              A when is_binary(A) ->
                  {case Format of
                       true -> string:trim(format_identifier(A));
                       _ -> string:trim(binary_to_list(A), both, " ")
                   end, Fun(A, NewCtx)};
              {param, A0} when is_binary(A0) ->
                  {string:trim(binary_to_list(A0)), Fun(A, NewCtx)};
              A ->
                  {A0, NCtx} = fold(Format,
                      State#state{indentation_level =
                      State#state.indentation_level + 1}, FType, Fun, NewCtx,
                      Lvl + 1,
                      A),
                  {string:trim(A0), NCtx}
          end,
    NewCtx2 = Fun(B, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([AStr, " ", case Format of
                                       true -> format_identifier(B);
                                       _ -> binary_to_list(B)
                                   end
    ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% funs
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'fun', N, Args} = ST)
    when is_binary(N) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(N, NewCtx),
    {ArgsStr, NewCtx2} = case Args of
                             [] -> {[], NewCtx1};
                             _ -> lists:foldl(fun(A, {Acc, CtxAcc}) ->
                                 case A of
                                     A when is_binary(A) ->
                                         {Acc ++ [
                                             case Format of
                                                 true -> format_identifier(A);
                                                 _ -> binary_to_list(A)
                                             end
                                         ], Fun(A, CtxAcc)};
                                     _ ->
                                         case lists:member(element(1, A),
                                             [
                                                 'alter user',
                                                 'create table',
                                                 'create user',
                                                 'delete',
                                                 'grant',
                                                 'insert',
                                                 'revoke',
                                                 'select',
                                                 'truncate table',
                                                 'update'
                                             ]) of
                                             true ->
                                                 {SubAcc, CtxAcc1} =
                                                     fold(Format, State, FType,
                                                         Fun, CtxAcc,
                                                         Lvl + 1, A),
                                                 {Acc ++ [lists:append([
                                                     "(",
                                                     string:trim(SubAcc),
                                                     ")"
                                                 ])], CtxAcc1};
                                             _ ->
                                                 {SubAcc, CtxAcc1} =
                                                     fold(Format,
                                                         State#state{function_level =
                                                         State#state.function_level +
                                                             1}, FType, Fun,
                                                         CtxAcc, Lvl + 1, A),
                                                 {Acc ++ [SubAcc], CtxAcc1}
                                         end
                                 end
                                              end,
                                 {[], NewCtx1}, Args)
                         end,
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> case ArgsStr of
                          [] -> format_identifier(N);
                          _ ->
                              case is_tuple(ST) andalso
                                  is_simple_list(Args, ?CR_LIMIT_FUNC_ARGS) of
                                  true -> lists:flatten([
                                      format_identifier(N),
                                      "(",
                                      columns_join(ArgsStr, ", ", []),
                                      ")"
                                  ]);
                                  _ -> lists:flatten([
                                      format_identifier(N),
                                      "(",
                                      format_commalist(
                                          State#state.indentation_level +
                                              State#state.function_level,
                                          ArgsStr),
                                      ?CHAR_NEWLINE,
                                      format_column_pos(
                                          State#state.indentation_level +
                                              State#state.function_level),
                                      ")"
                                  ])
                              end
                      end;
              _ -> case ArgsStr of
                       [] -> binary_to_list(N);
                       _ ->
                           lists:append(
                               [binary_to_list(N), "(", columns_join(ArgsStr,
                                   ", ", []), ")"])
                   end
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GOTO
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, _State, FType, Fun, Ctx, _Lvl, {goto, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> format_keyword("goto ");
              _ -> "goto "
          end ++ Value, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GRANT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {grant, Objs, {OnTyp, On}, {'to', Tos}, Opts} = ST)
    when is_atom(OnTyp), is_atom(Opts) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [atom_to_list(O)], Fun(O, CtxAcc)}
                                     end,
        {[], NewCtx},
        Objs),
    {OnTypNew, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl + 1, OnTyp),
    {OnNew, NewCtx3} =
        fold(Format, State, FType, Fun, NewCtx2, Lvl + 1, {table, On}),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [case is_binary(O) of
                     true -> binary_to_list(O);
                     _ -> {ONew, _} =
                         fold(Format, State, FType, Fun, NewCtx, Lvl + 1, O),
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
    RT = {case Format of
              true -> lists:flatten([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("grant"),
                  case length(ObjsStr) =< ?CR_LIMIT_GRANT_PRIVILEGE of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          lists:join(", ",
                              [case lists:member(O,
                                  ?OBJECT_PRIVILEGES ++ ?SYSTEM_PRIVILEGES) of
                                   true -> format_keyword(O);
                                   _ -> format_identifier(O)
                               end || O <- ObjsStr])
                      ]);
                      _ ->
                          format_commalist(State#state.indentation_level,
                              [case lists:member(O, ?OBJECT_PRIVILEGES
                              ++ ?SYSTEM_PRIVILEGES) of
                                   true ->
                                       format_keyword(O);
                                   _ ->
                                       format_identifier(O)
                               end || O <- ObjsStr])
                  end,
                  case On =/= <<"">> of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          format_keyword(OnTypNew),
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          OnNew
                      ]);
                      _ -> []
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("to"),
                  case length(TosStr) =< ?CR_LIMIT_GRANT_GRANTEE of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          lists:join(", ",
                              [format_identifier(T) || T <- TosStr])
                      ]);
                      _ ->
                          format_commalist(State#state.indentation_level,
                              [format_identifier(T) || T <- TosStr])
                  end,
                  case format_keyword(Opts) of
                      [] -> [];
                      OptsStr -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          OptsStr
                      ])
                  end
              ]);
              _ -> lists:flatten([
                  "grant ",
                  string:join(ObjsStr, ","),
                  " ",
                  case On =/= <<"">> of
                      true -> lists:append([OnTypNew, " ", OnNew, " "]);
                      _ -> []
                  end,
                  "to ",
                  string:join(TosStr, ","),
                  case atom_to_list(Opts) of
                      [] -> [];
                      OptsStr -> " " ++ OptsStr
                  end
              ])
          end, NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GROUP BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'group by', GroupBy} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {GroupByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        {F1, CtxAcc1} = fold(Format, State, FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ [F1], CtxAcc1}
                                        end,
        {[], NewCtx},
        GroupBy),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case length(GroupByStr) > 0 of
              true -> case Format of
                          true -> lists:append([
                              ?CHAR_NEWLINE,
                              format_column_pos(
                                  State#state.indentation_level - 1),
                              format_keyword("group by"),
                              case is_simple_list(GroupBy,
                                  ?CR_LIMIT_GROUP_BY) of
                                  true -> lists:append([
                                      ?CHAR_NEWLINE,
                                      format_column_pos(
                                          State#state.indentation_level),
                                      columns_join(GroupByStr, ", ", [])
                                  ]);
                                  _ ->
                                      format_commalist(
                                          State#state.indentation_level,
                                          GroupByStr)
                              end
                          ]);
                          _ -> "group by " ++ columns_join(GroupByStr, ", ", [])
                      end;
              _ -> []
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAVING
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {having, Having} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {HavingStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1, Having),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case length(HavingStr) > 0 of
              true -> case Format of
                          true -> lists:append([
                              ?CHAR_NEWLINE,
                              format_column_pos(
                                  State#state.indentation_level - 1),
                              format_keyword("having"),
                              ?CHAR_NEWLINE,
                              format_column_pos(State#state.indentation_level),
                              format_search_condition(
                                  State#state.indentation_level, HavingStr)
                          ]);
                          _ -> "having " ++ HavingStr
                      end;
              _ -> []
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hierarchical query
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, _FType, Fun, Ctx, _Lvl, {'hierarchical query', {}} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
    RT = {[], Fun(ST, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {'hierarchical query', {Part1, Part2}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Part1Str, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1, Part1),
    {Part2Str, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl + 1, Part2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([
        Part1Str,
        case Format of
            true -> [];
            _ -> " "
        end,
        Part2Str]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HINTS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, FType, Fun, Ctx, _Lvl, {hints, Hints} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Hints, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {binary_to_list(Hints), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IDENTIFIED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl, {'identified by', Pswd} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Pswd, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("identified by"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  binary_to_list(Pswd)
              ]);
              _ -> "identified by " ++ binary_to_list(Pswd)
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, _Lvl, {Type, {}} = ST)
    when Type == 'identified extern'; Type == 'identified globally' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("identified "),
                  case Type of
                      'identified extern' -> format_keyword("externally");
                      _ -> format_keyword("globally")
                  end
              ]);
              _ -> case Type of
                       'identified extern' -> "identified externally";
                       _ -> atom_to_list(Type)
                   end
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, _Lvl, {Type, E} = ST)
    when Type == 'identified extern'; Type == 'identified globally' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(E, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("identified "),
                  case Type of
                      'identified extern' -> format_keyword("externally ");
                      _ -> format_keyword("globally ")
                  end,
                  format_keyword("as"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  binary_to_list(E)
              ]);
              _ -> lists:append([case Type of
                                     'identified extern' ->
                                         "identified externally";
                                     _ -> atom_to_list(Type)
                                 end,
                  " as ",
                  binary_to_list(E)])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IN operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {'in', L, R} = ST)
    when is_tuple(R) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {LStr, NewCtx1} = case is_binary(L) of
                          true -> {case Format of
                                       true -> format_identifier(L);
                                       _ -> binary_to_list(L)
                                   end, NewCtx};
                          _ -> fold(Format, case L of
                                                {select, _} ->
                                                    State#state{indentation_level =
                                                    State#state.indentation_level +
                                                        1};
                                                {TypeL, _, _} when
                                                    TypeL == intersect orelse
                                                        TypeL == minus orelse
                                                        TypeL == union orelse
                                                        TypeL == 'union all' ->
                                                    State#state{indentation_level =
                                                    State#state.indentation_level +
                                                        1};
                                                _ -> State
                                            end, FType, Fun, NewCtx, Lvl + 1, L)
                      end,
    {RStr, NewCtx2} = fold(Format, case R of
                                       {select, _} ->
                                           State#state{indentation_level =
                                           State#state.indentation_level + 1};
                                       {TypeR, _, _} when
                                           TypeR == intersect orelse
                                               TypeR == minus orelse
                                               TypeR == union orelse
                                               TypeR == 'union all' ->
                                           State#state{indentation_level =
                                           State#state.indentation_level + 1};
                                       _ -> State
                                   end, FType, Fun, NewCtx1, Lvl + 1, R),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    IsOuterBracket = case string:slice(RStr, 0, 1) == "(" of
                         true -> false;
                         _ -> true
                     end,
    RT = {case Format of
              true -> lists:append([
                  LStr,
                  " ",
                  format_keyword("in "),
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  RStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end
              ]);
              _ -> lists:append([
                  LStr,
                  " in ",
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  RStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSERT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {insert, Table, {}, {}, Return} =
    ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format, State#state{select_clause = none, statement = insert},
            FType, Fun, NewCtx, Lvl + 1, {table, Table}),
    {Ret, NewCtx2} =
        case Return of
            {_, {}} -> {[], NewCtx1};
            _ -> fold(Format,
                State#state{select_clause = none, statement = insert}, FType,
                Fun, NewCtx1, Lvl + 1, Return)
        end,
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("insert into"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  TableStr,
                  Ret
              ]);
              _ -> lists:append([
                  "insert into ",
                  TableStr,
                  case Ret of
                      [] -> [];
                      _ -> " " ++ Ret
                  end
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {insert, Table, {cols, Columns}, {values, Values}, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format, State#state{select_clause = none, statement = insert},
            FType, Fun, NewCtx, Lvl + 1, {table, Table}),
    {CStrs, NewCtx2} = case Columns of
                           [] -> {[], NewCtx1};
                           _ -> lists:foldl(fun(C, {Acc, CtxAcc}) ->
                               {CT, CtxAcc1} =
                                   fold(Format,
                                       State#state{select_clause = none, statement = insert},
                                       FType, Fun, CtxAcc,
                                       Lvl + 1, C),
                               {Acc ++ [CT], CtxAcc1}
                                            end,
                               {[], NewCtx1},
                               Columns)
                       end,
    {Vals, NewCtx3} = lists:foldl(fun(V, {Acc1, CtxAcc1}) ->
        case V of
            V when is_binary(V) ->
                {Acc1 ++ [binary_to_list(V)], Fun(V, CtxAcc1)};
            V ->
                {VT, CtxAcc2} =
                    fold(Format,
                        State#state{select_clause = none, statement = insert},
                        FType, Fun, CtxAcc1, Lvl + 1, V),
                {Acc1 ++ [VT], CtxAcc2}
        end
                                  end,
        {[], NewCtx2},
        Values),
    {Ret, NewCtx4} =
        case Return of
            {_, {}} -> {[], NewCtx3};
            _ -> fold(Format,
                State#state{select_clause = none, statement = insert}, FType,
                Fun, NewCtx3, Lvl + 1, Return)
        end,
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("insert into"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  TableStr,
                  case length(CStrs) == 0 of
                      true -> [];
                      _ -> case length(CStrs) =< ?CR_LIMIT_INSERT of
                               true -> lists:append([
                                   lists:flatten(
                                       [" (", lists:join(", ", CStrs), ")"])
                               ]);
                               _ -> lists:append([
                                   " (",
                                   format_commalist(
                                       State#state.indentation_level, CStrs),
                                   ?CHAR_NEWLINE,
                                   format_column_pos(
                                       State#state.indentation_level),
                                   ")"
                               ])
                           end
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword("values"),
                  case is_simple_list(Values, ?CR_LIMIT_INSERT) of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          lists:flatten(
                              ["(", lists:join(", ",
                                  [format_identifier(V) || V <- Vals]), ")"])
                      ]);
                      _ -> lists:append([
                          " (",
                          format_commalist(State#state.indentation_level,
                              [format_identifier(V) || V <- Vals]),
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level),
                          ")"
                      ])
                  end,
                  Ret
              ]);
              _ -> lists:append([
                  "insert into ",
                  TableStr,
                  case length(CStrs) == 0 of
                      true -> [];
                      _ -> lists:append([" (", string:join(CStrs, ","), ")"])
                  end,
                  " values (",
                  string:join(Vals, ","),
                  ")",
                  case Ret of
                      [] -> [];
                      _ -> " " ++ Ret
                  end
              ])
          end, NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {insert, Table, {cols, Columns}, {select, _} = SubQuery, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format, State#state{select_clause = none, statement = insert},
            FType, Fun, NewCtx, Lvl + 1, {table, Table}),
    {CStrs, NewCtx2} = case Columns of
                           [] -> {[], NewCtx1};
                           _ -> lists:foldl(fun(C, {Acc, CtxAcc}) ->
                               {CT, CtxAcc1} =
                                   fold(Format,
                                       State#state{select_clause = none, statement = insert},
                                       FType, Fun, CtxAcc,
                                       Lvl + 1, C),
                               {Acc ++ [CT], CtxAcc1}
                                            end,
                               {[], NewCtx1},
                               Columns)
                       end,
    {SubQueryStr, NewCtx3} = fold(Format,
        State#state{indentation_level = State#state.indentation_level +
            1, select_clause = none, statement = insert},
        FType, Fun, NewCtx2, Lvl + 1, SubQuery),
    {Ret, NewCtx4} =
        case Return of
            {_, {}} -> {[], NewCtx3};
            _ -> fold(Format,
                State#state{select_clause = none, statement = insert}, FType,
                Fun, NewCtx3, Lvl + 1, Return)
        end,
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(State#state.indentation_level - 2),
                  format_keyword("insert into"),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  TableStr,
                  case length(CStrs) == 0 of
                      true -> [];
                      _ -> case length(CStrs) =< ?CR_LIMIT_INSERT of
                               true -> lists:append([
                                   lists:flatten(
                                       [" (", lists:join(", ", CStrs), ")"])
                               ]);
                               _ -> lists:append([
                                   " (",
                                   format_commalist(
                                       State#state.indentation_level, CStrs),
                                   ?CHAR_NEWLINE,
                                   format_column_pos(
                                       State#state.indentation_level),
                                   ")"
                               ])
                           end
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  SubQueryStr,
                  Ret
              ]);
              _ -> lists:append([
                  "insert into ",
                  TableStr,
                  case length(CStrs) == 0 of
                      true -> [];
                      _ -> lists:append([" (", string:join(CStrs, ","), ")"])
                  end,
                  " ",
                  SubQueryStr,
                  case Ret of
                      [] -> [];
                      _ -> " " ++ Ret
                  end
              ])
          end, NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intersect / Minus / Union
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {Type, {select, _} = A, {select, _} = B} = ST)
    when Type == intersect; Type == minus; Type == union; Type == 'union all' ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    IsOuterBracket = case State#state.indentation_level > 1 of
                         true -> true;
                         _ -> false
                     end,
    {AStr, NewCtx1} = fold(Format, State, FType, Fun, NewCtx, Lvl + 1, A),
    {BStr, NewCtx2} = fold(Format, State, FType, Fun, NewCtx1, Lvl + 1, B),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  AStr,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword(Type),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  BStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end
              ]);
              _ -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  AStr,
                  " ",
                  atom_to_list(Type),
                  " ",
                  BStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

fold(Format, State, FType, Fun, Ctx, Lvl,
    {Type, {select, _} = A, {TypeB, _, _} = B} = ST)
    when (Type == intersect orelse Type == minus orelse Type == union orelse
    Type == 'union all') andalso
    (TypeB == intersect orelse TypeB == minus orelse TypeB == union orelse
        TypeB == 'union all') ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    IsOuterBracket = case State#state.statement of
                         select -> case
                                       State#state.select_clause of
                                       none -> false;
                                       _ -> true
                                   end;
                         UI when UI == intersect;UI == minus;
                             UI == union;UI ==
                                 'union all' ->
                             case
                                 State#state.indentation_level > 1 of
                                 true -> true;
                                 _ -> false
                             end;
                         _ -> true
                     end,
    {AStr, NewCtx1} =
        fold(Format,
            State#state{indentation_level =
            State#state.indentation_level + 1, statement = Type}, FType, Fun,
            NewCtx, Lvl + 1, A),
    {BStr, NewCtx2} =
        fold(Format,
            State#state{indentation_level =
            State#state.indentation_level + 1, statement = Type}, FType, Fun,
            NewCtx1, Lvl + 1, B),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(" ++ ?CHAR_NEWLINE;
                      _ -> []
                  end,
                  format_column_pos(State#state.indentation_level),
                  AStr,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword(Type),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  BStr,
                  case IsOuterBracket of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          ")"
                      ]);
                      _ -> []
                  end
              ]);
              _ -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  AStr,
                  " ",
                  atom_to_list(Type),
                  " ",
                  BStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

fold(Format, State, FType, Fun, Ctx, Lvl,
    {Type, {TypeA, _, _} = A, B} = ST)
    when (Type == intersect orelse Type == minus orelse Type == union orelse
    Type == 'union all') andalso
    (TypeA == intersect orelse TypeA == minus orelse TypeA == union orelse
        TypeA == 'union all') ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    IsOuterBracket = case State#state.statement of
                         select -> case
                                       State#state.select_clause of
                                       none -> false;
                                       _ -> true
                                   end;
                         UI when UI == intersect;UI == minus;
                             UI == union;UI ==
                                 'union all' ->
                             case
                                 State#state.indentation_level > 1 of
                                 true -> true;
                                 _ -> false
                             end;
                         _ -> true
                     end,
    {AStr, NewCtx1} =
        fold(Format,
            State#state{indentation_level =
            State#state.indentation_level + 1, statement = Type}, FType, Fun,
            NewCtx, Lvl + 1, A),
    {BStr, NewCtx2} =
        fold(Format,
            State#state{indentation_level =
            State#state.indentation_level + 1, statement = Type}, FType, Fun,
            NewCtx1, Lvl + 1, B),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(" ++ ?CHAR_NEWLINE;
                      _ -> []
                  end,
                  format_column_pos(State#state.indentation_level),
                  AStr,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword(Type),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  BStr,
                  case IsOuterBracket of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          ")"
                      ]);
                      _ -> []
                  end
              ]);
              _ -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  AStr,
                  " ",
                  atom_to_list(Type),
                  " ",
                  BStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

fold(Format, State, FType, Fun, Ctx, Lvl,
    {Type, {TypeA, _, _} = A, {TypeB, _, _} = B} = ST)
    when (Type == intersect orelse Type == minus orelse Type == union orelse
    Type == 'union all') andalso
    (TypeA == intersect orelse TypeA == minus orelse TypeA == union orelse
        TypeA == 'union all') andalso
    (TypeB == intersect orelse TypeB == minus orelse TypeB == union orelse
        TypeB == 'union all') ->
    ?debugFmt(?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    IsOuterBracket = case State#state.statement of
                         select -> case
                                       State#state.select_clause of
                                       none -> false;
                                       _ -> true
                                   end;
                         UI when UI == intersect;UI == minus;
                             UI == union;UI ==
                                 'union all' ->
                             case
                                 State#state.indentation_level > 1 of
                                 true -> true;
                                 _ -> false
                             end;
                         _ -> true
                     end,
    {AStr, NewCtx1} =
        fold(Format,
            State#state{indentation_level =
            State#state.indentation_level + 1, statement = Type}, FType, Fun,
            NewCtx, Lvl + 1, A),
    {BStr, NewCtx2} =
        fold(Format,
            State#state{indentation_level =
            State#state.indentation_level + 1, statement = Type}, FType, Fun,
            NewCtx1, Lvl + 1, B),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(" ++ ?CHAR_NEWLINE;
                      _ -> []
                  end,
                  format_column_pos(State#state.indentation_level),
                  AStr,
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level - 1),
                  format_keyword(Type),
                  ?CHAR_NEWLINE,
                  format_column_pos(State#state.indentation_level),
                  BStr,
                  case IsOuterBracket of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(State#state.indentation_level - 1),
                          ")"
                      ]);
                      _ -> []
                  end
              ]);
              _ -> lists:flatten([
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  AStr,
                  " ",
                  atom_to_list(Type),
                  " ",
                  BStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end
              ])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTO
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {into, Into} = ST)
    when is_list(Into) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntoStr, NewCtx1} = lists:foldl(fun(I, {Acc, CtxAcc}) ->
        {Acc ++ [case is_binary(I) of
                     true -> case Format of
                                 true -> format_identifier(I);
                                 _ -> binary_to_list(I)
                             end;
                     _ -> {INew, _} =
                         fold(Format, State, FType, Fun,
                             NewCtx, Lvl + 1, I),
                         INew
                 end], Fun(I, CtxAcc)}
                                     end,
        {[], NewCtx},
        Into),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("into"),
                  case length(IntoStr) =< ?CR_LIMIT_INTO of
                      true -> lists:flatten([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          lists:flatten(
                              lists:join(", ", IntoStr))
                      ]);
                      _ -> format_commalist(
                          State#state.indentation_level,
                          IntoStr)
                  end
              ]);
              _ -> "into " ++ string:join(IntoStr, ",")
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% joins
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {JoinType, Tab} = ST)
    when JoinType =:= cross_join;
    JoinType =:= natural_join;
    JoinType =:= natural_inner_join ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl + 1,
            Tab),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {[case Format of
               true -> lists:append([
                   format_keyword(case JoinType of
                                      cross_join ->
                                          "cross join";
                                      natural_join ->
                                          "natural join";
                                      natural_inner_join ->
                                          "natural inner join"
                                  end),
                   " ",
                   TabStr
               ]);
               _ -> case JoinType of
                        cross_join -> "cross join ";
                        natural_join -> "natural join ";
                        natural_inner_join ->
                            "natural inner join "
                    end ++ TabStr
           end], NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {{JoinType, OptPartitionLeft, OptNatural}, Tab, OptPartitionRight, OnOrUsing} =
        ST)
    when JoinType =:= full;
    JoinType =:= left;
    JoinType =:= right;
    JoinType =:= full_outer;
    JoinType =:= left_outer;
    JoinType =:= right_outer ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptPartitionLeftStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1,
            OptPartitionLeft),
    {OptNaturalStr, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl + 1,
            OptNatural),
    NewCtx3 = Fun(JoinType, NewCtx2),
    {TabStr0, NewCtx4} =
        fold(Format, State, FType, Fun, NewCtx3, Lvl + 1,
            Tab),
    TabStr = case Tab of
                 {select, _} -> [string:trim(TabStr0)];
                 _ -> string:trim(TabStr0)
             end,
    {OptPartitionRightStr, NewCtx5} =
        fold(Format, State, FType, Fun, NewCtx4, Lvl + 1,
            OptPartitionRight),
    {OnOrUsingStr, NewCtx6} =
        fold(Format, State, FType, Fun, NewCtx5, Lvl + 1,
            OnOrUsing),

    NewCtx7 = case FType of
                  top_down -> NewCtx6;
                  bottom_up -> Fun(ST, NewCtx6)
              end,
    RT = {case Format of
              true -> lists:append([
                  case length(OptPartitionLeftStr) == 0 of
                      true -> [];
                      _ -> [OptPartitionLeftStr]
                  end,
                  [lists:append([
                      case length(OptNaturalStr) == 0 of
                          true -> [];
                          _ ->
                              format_keyword(OptNaturalStr) ++
                              " "
                      end,
                      format_keyword(case JoinType of
                                         full -> "full join";
                                         left -> "left join";
                                         right ->
                                             "right join";
                                         full_outer ->
                                             "full outer join";
                                         left_outer ->
                                             "left outer join";
                                         right_outer ->
                                             "right outer join"
                                     end),
                      " ",
                      TabStr
                  ])
                  ],
                  case length(OptPartitionRightStr) == 0 of
                      true -> [];
                      _ -> [OptPartitionRightStr]
                  end,
                  case length(OnOrUsingStr) == 0 of
                      true -> [];
                      _ -> [OnOrUsingStr]
                  end
              ]);
              _ -> lists:append([
                  case length(OptPartitionLeftStr) == 0 of
                      true -> [];
                      _ -> OptPartitionLeftStr ++ " "
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
                  case length(OptPartitionRightStr) == 0 of
                      true -> [];
                      _ -> " " ++ OptPartitionRightStr
                  end,
                  case length(OnOrUsingStr) == 0 of
                      true -> [];
                      _ -> " " ++ OnOrUsingStr
                  end
              ])
          end, NewCtx7},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {JoinType, Tab, OnOrUsing} = ST)
    when JoinType =:= join; JoinType =:= join_inner ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr0, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl + 1,
            Tab),
    TabStr = case Tab of
                 {select, _} -> [string:trim(TabStr0)];
                 _ -> string:trim(TabStr0)
             end,
    {OnOrUsingStr, NewCtx3} =
        fold(Format, State, FType, Fun, NewCtx2, Lvl + 1,
            OnOrUsing),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> [lists:append(
                  [format_keyword(case JoinType of
                                      join -> "join";
                                      join_inner ->
                                          "inner join"
                                  end),
                      " ",
                      TabStr])] ++ [OnOrUsingStr];
              _ -> lists:append([
                  case JoinType of
                      join -> "join ";
                      join_inner -> "inner join "
                  end,
                  TabStr,
                  " ",
                  OnOrUsingStr
              ])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIKE operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {like, Var, Like, OptEsc} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VarStr, NewCtx1} = case is_binary(Var) of
                            true -> {case Format of
                                         true ->
                                             format_identifier(
                                                 Var);
                                         _ -> binary_to_list(
                                             Var)
                                     end, NewCtx};
                            _ -> fold(Format, case Var of
                                                  {select, _} ->
                                                      State#state{indentation_level =
                                                      State#state.indentation_level +
                                                          1};
                                                  {TypeV, _, _} when
                                                      TypeV ==
                                                          intersect orelse
                                                          TypeV ==
                                                              minus orelse
                                                          TypeV ==
                                                              union orelse
                                                          TypeV ==
                                                              'union all' ->
                                                      State#state{indentation_level =
                                                      State#state.indentation_level +
                                                          1};
                                                  _ -> State
                                              end, FType, Fun,
                                NewCtx, Lvl + 1,
                                Var)
                        end,
    {LikeStr, NewCtx2} = case is_binary(Like) of
                             true -> {case Format of
                                          true ->
                                              format_identifier(
                                                  Like);
                                          _ -> binary_to_list(
                                              Like)
                                      end, NewCtx1};
                             _ -> fold(Format, case Like of
                                                   {select, _} ->
                                                       State#state{indentation_level =
                                                       State#state.indentation_level +
                                                           1};
                                                   {TypeL, _, _} when
                                                       TypeL ==
                                                           intersect orelse
                                                           TypeL ==
                                                               minus orelse
                                                           TypeL ==
                                                               union orelse
                                                           TypeL ==
                                                               'union all' ->
                                                       State#state{indentation_level =
                                                       State#state.indentation_level +
                                                           1};
                                                   _ -> State
                                               end, FType,
                                 Fun, NewCtx1,
                                 Lvl + 1, Like)
                         end,
    {OptEscStr, NewCtx3} = case is_binary(OptEsc) of
                               true -> case OptEsc of
                                           <<>> ->
                                               {[], NewCtx2};
                                           _ ->
                                               {binary_to_list(
                                                   OptEsc), NewCtx2}
                                       end;
                               _ -> fold(Format, State, FType,
                                   Fun, NewCtx2,
                                   Lvl + 1, OptEsc)
                           end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    IsOuterBracket = case string:slice(LikeStr, 0, 1) == "(" of
                         true -> false;
                         _ -> case Like of
                                  {select, _} -> true;
                                  _ -> false
                              end
                     end,
    RT = {case Format of
              true -> lists:append([
                  VarStr,
                  " ",
                  format_keyword("like "),
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  LikeStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end,
                  case OptEscStr of
                      [] -> [];
                      _ -> lists:append(
                          [" ", format_keyword(
                              "escape "), OptEscStr])
                  end
              ]);
              _ -> lists:append([
                  VarStr,
                  " like ",
                  case IsOuterBracket of
                      true -> "(";
                      _ -> []
                  end,
                  LikeStr,
                  case IsOuterBracket of
                      true -> ")";
                      _ -> []
                  end,
                  case OptEscStr of
                      [] -> [];
                      _ -> " escape " ++ OptEscStr
                  end
              ])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIMITED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl,
    {limited, Q, U, T} = ST)
    when is_binary(Q), is_binary(T) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Q, NewCtx),
    NewCtx2 = Fun(U, NewCtx1),
    NewCtx3 = Fun(T, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("quota"),
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level),
                  binary_to_list(Q),
                  case U =/= <<"">> of
                      true -> " " ++ format_identifier(U);
                      _ -> []
                  end,
                  " ",
                  format_keyword("on "),
                  format_identifier(T)
              ]);
              _ -> lists:append(["quota ",
                  binary_to_list(Q),
                  case U =/= <<"">> of
                      true -> " " ++ binary_to_list(U);
                      _ -> []
                  end,
                  " on ",
                  binary_to_list(T)])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIST
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {list, Elms} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ElmsStr, NewCtx1} = lists:foldl(fun(E, {Acc, CtxAcc}) ->
        case E of
            E when is_binary(E) -> {Acc ++ [case Format of
                                                true ->
                                                    format_identifier(
                                                        E);
                                                _ ->
                                                    binary_to_list(
                                                        E)
                                            end], Fun(E,
                CtxAcc)};
            E ->
                {SubAcc, CtxAcc1} =
                    fold(Format, State, FType, Fun, CtxAcc,
                        Lvl + 1, E),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                     end,
        {[], NewCtx},
        Elms),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(
        ["(", lists:join(", ", ElmsStr), ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NATURAL
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, _State, _FType, Fun, Ctx, _Lvl, natural = _ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, _ST]),
    RT = {case Format of
              true -> format_keyword("natural");
              _ -> "natural"
          end, Fun(natural, Ctx)},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unary - and 'not' operators
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {Op, A} = ST)
    when Op =:= '+' orelse Op =:= '-' orelse Op =:= 'not' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Op, NewCtx),
    {Str, NewCtx3} = case A of
                         A when is_binary(A) ->
                             NewCtx2 = Fun(A, NewCtx1),
                             {case Format of
                                  true -> lists:append([
                                      format_operator(
                                          State#state.indentation_level,
                                          Op,
                                          true),
                                      "(",
                                      format_identifier(A),
                                      ")"
                                  ]);
                                  _ ->
                                      lists:append(
                                          [atom_to_list(
                                              Op), " (", binary_to_list(
                                              A), ")"])
                              end, NewCtx2};
                         A ->
                             {As, NewCtx2} =
                                 fold(Format, State, FType,
                                     Fun, NewCtx1,
                                     Lvl + 1, A),
                             {lists:append([case Format of
                                                true ->
                                                    format_operator(
                                                        State#state.indentation_level,
                                                        Op,
                                                        true);
                                                _ ->
                                                    atom_to_list(
                                                        Op)
                                            end, " (", As, ")"]), NewCtx2}
                     end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Str, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ON
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {on, Condition} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {CondStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1,
            Condition),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> format_keyword("on ");
              _ -> "on "
          end ++ CondStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OPEN
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, _State, FType, Fun, Ctx, _Lvl,
    {open, {cur, CurName}} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> format_keyword("open ");
              _ -> "open "
          end ++ CurName, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OPT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, _State, FType, Fun, Ctx, _Lvl,
    {opt, Opt} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Opt, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> " " ++ format_keyword(Opt);
              _ -> binary_to_list(Opt)
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ORDER BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'order by', OrderBy} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    Size = length(OrderBy),
    {OrderByStr, NewCtx1} =
        lists:foldl(fun(F, {Acc, CtxAcc}) ->
            ?debugFmt(
                ?MODULE_STRING ++ ":fold ===>~n F: ~p~n",
                [F]),
            case F of
                {O, Op} when is_binary(O), is_binary(Op) ->
                    CtxAcc1 = Fun(O, CtxAcc),
                    CtxAcc2 = Fun(Op, CtxAcc1),
                    {Acc ++ case Format of
                                true ->
                                    [string:trim(lists:append(
                                        [format_identifier(
                                            O), " ", format_keyword(
                                            binary_to_list(
                                                Op))]))];
                                _ ->
                                    [string:trim(
                                        lists:append(
                                            [binary_to_list(
                                                O), " ", binary_to_list(
                                                Op)]),
                                        both, " ")]
                            end, CtxAcc2};
                {{select, _} = O, Op} when is_binary(Op) ->
                    {Os, CtxAcc1} = fold(Format,
                        State#state{indentation_level =
                        State#state.indentation_level +
                            1, statement = select}, FType,
                        Fun, CtxAcc, Lvl + 1, O),
                    CtxAcc2 = Fun(Op, CtxAcc1),
                    {Acc ++ case Format of
                                true -> [Os ++ case Op of
                                                   <<>> -> [];
                                                   _ -> " " ++
                                                   format_keyword(
                                                       binary_to_list(
                                                           Op))
                                               end];
                                _ ->
                                    [string:trim(lists:append(
                                        ["(", Os, ") ", binary_to_list(
                                            Op)]),
                                        both, " ")]
                            end, CtxAcc2};
                {O, Op} when is_binary(Op) ->
                    {Os, CtxAcc1} =
                        fold(Format, State, FType, Fun,
                            CtxAcc, Lvl + 1, O),
                    CtxAcc2 = Fun(Op, CtxAcc1),
                    {Acc ++ case Format of
                                true ->
                                    [string:trim(
                                        lists:append(
                                            [Os, " ", format_keyword(
                                                binary_to_list(
                                                    Op))]))];
                                _ ->
                                    [string:trim(
                                        lists:append(
                                            [Os, " ", binary_to_list(
                                                Op)]),
                                        both,
                                        " ")]
                            end, CtxAcc2}
            end
                    end,
            {[], NewCtx},
            OrderBy),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Size > 0 of
              true -> case Format of
                          true -> lists:append([
                              ?CHAR_NEWLINE,
                              format_column_pos(
                                  State#state.indentation_level -
                                      1),
                              format_keyword("order by"),
                              case is_simple_list(OrderBy,
                                  ?CR_LIMIT_ORDER_BY) of
                                  true -> lists:append([
                                      ?CHAR_NEWLINE,
                                      format_column_pos(
                                          State#state.indentation_level),
                                      columns_join(OrderByStr,
                                          ", ", [])
                                  ]);
                                  _ ->
                                      format_commalist(
                                          State#state.indentation_level,
                                          OrderByStr)
                              end
                          ]);
                          _ -> "order by " ++
                          columns_join(OrderByStr, ", ", [])
                      end;
              _ -> []
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARAM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, FType, Fun, Ctx, _Lvl,
    {{param, P1}, {param, P2}} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
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
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(_Format, _State, FType, Fun, Ctx, _Lvl,
    {param, P} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
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
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARTITION_BY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {partition_by, Fields} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FieldsStr, NewCtx1} =
        lists:foldl(fun(F, {Acc, CtxAcc}) ->
            ?debugFmt(
                ?MODULE_STRING ++ ":fold ===>~n F: ~p~n",
                [F]),
            case F of
                F when is_binary(F) -> {Acc ++ [case Format of
                                                    true ->
                                                        format_identifier(
                                                            F);
                                                    _ ->
                                                        binary_to_list(
                                                            F)
                                                end], Fun(F,
                    CtxAcc)};
                _ -> {SubAcc, CtxAcc1} =
                    fold(Format, State, FType, Fun, CtxAcc,
                        Lvl + 1, F),
                    {Acc ++ [SubAcc], CtxAcc1}
            end
                    end,
            {[], NewCtx},
            Fields),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_keyword("partition by"),
                  case is_simple_list(Fields,
                      ?CR_LIMIT_PARTITION) of
                      true ->
                          lists:flatten(
                              [" (", lists:join(", ",
                                  FieldsStr), ")"]);
                      _ -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          "(",
                          format_commalist(
                              State#state.indentation_level +
                                  1,
                              FieldsStr),
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          ")"
                      ])
                  end
              ]);
              _ -> lists:append(
                  ["partition by (", columns_join(FieldsStr,
                      ", ", []), ")"])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PASSWORD EXPIRE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl,
    {password, expire} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("password expire")
              ]);
              _ -> "password expire "
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIMARY KEY / UNIQUE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {Type, ClmList} = ST)
    when Type == 'primary key';
    Type == unique ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ColStrList, NewCtx1} =
        lists:foldl(
            fun(Clm, {StrList, ICtx}) ->
                {CStr, ICtx1} =
                    fold(Format, State, FType, Fun, ICtx,
                        Lvl + 1, Clm),
                {[CStr | StrList], ICtx1}
            end, {[], NewCtx}, ClmList),
    ClmStr = lists:join(", ", lists:reverse(ColStrList)),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten([case Format of
                             true -> format_keyword(Type);
                             _ -> atom_to_list(Type)
                         end, " (", ClmStr, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIOR
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {prior, Field} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FieldsStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1,
            Field),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> format_keyword("prior ");
              _ -> "prior "
          end ++ case string:sub_string(FieldsStr, 1, 7) ==
        "select " of
                     true ->
                         lists:append(["(", FieldsStr, ")"]);
                     _ -> FieldsStr
                 end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROFILE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl,
    {profile, Profile} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(Profile, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("profile"),
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level),
                  format_identifier(Profile)
              ]);
              _ -> lists:append(
                  ["profile ", binary_to_list(Profile)])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% QUOTAS
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {quotas, Quotas} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {QuotaStr, NewCtx1}
        = lists:foldl(
        fun(Quota, {Str, AccCtx}) ->
            {NewStr, NewAccCtx} =
                fold(Format, State, FType, Fun, AccCtx,
                    Lvl + 1, Quota),
            {lists:append([
                Str,
                case length(Str) == 0 of
                    true -> [];
                    _ -> " "
                end,
                NewStr
            ]), NewAccCtx}
        end, {[], NewCtx}, Quotas),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {QuotaStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REFERENCES
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {ref, {Table, Value2}} = ST)
    when is_list(Value2) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1,
            {table, Table}),
    {Value2Str, NewCtx2} =
        lists:foldl(fun(V, {Acc, CtxAcc}) ->
            {VNew, _} =
                fold(Format, State, FType, Fun, NewCtx1,
                    Lvl + 1, V),
            {lists:append([
                Acc,
                case length(Acc) == 0 of
                    true -> [];
                    _ -> ", "
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
    RT = {lists:append([case Format of
                            true ->
                                format_keyword("references ");
                            _ -> "references "
                        end, TableStr, " (", Value2Str, ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {ref, Table} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1,
            {table, Table}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([case Format of
                            true ->
                                format_keyword("references ");
                            _ -> "references "
                        end, TableStr]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Returning phrase
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {R, Sel, Var} = ST)
    when R =:= return; R =:= returning ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(R, NewCtx),
    {SelStr, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} =
            fold(Format, State, FType, Fun, CtxAcc, Lvl + 1,
                S),
        {Acc ++ [SubAcc], CtxAcc1}
                                    end,
        {[], NewCtx1},
        Sel),
    {VarStr, NewCtx3} = lists:foldl(fun(V, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} =
            fold(Format, State, FType, Fun, CtxAcc, Lvl + 1,
                V),
        {Acc ++ [SubAcc], CtxAcc1}
                                    end,
        {[], NewCtx2},
        Var),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword(R),
                  case length(SelStr) =<
                      ?CR_LIMIT_RETURNING of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          lists:join(", ", SelStr)
                      ]);
                      _ ->
                          format_commalist(
                              State#state.indentation_level,
                              SelStr)
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("into"),
                  case length(VarStr) =<
                      ?CR_LIMIT_RETURNING of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          lists:join(", ", VarStr)
                      ]);
                      _ ->
                          format_commalist(
                              State#state.indentation_level,
                              VarStr)
                  end
              ]);
              _ ->
                  lists:flatten(
                      [atom_to_list(R), " ", string:join(
                          SelStr,
                          ", "), " INTO ", string:join(VarStr,
                          ", ")])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REVOKE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {revoke, Objs, {OnTyp, On}, {'from', Tos}, Opts} = ST)
    when is_atom(OnTyp), is_atom(Opts) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [atom_to_list(O)], Fun(O, CtxAcc)}
                                     end,
        {[], NewCtx},
        Objs),
    {OnTypNew, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl + 1,
            OnTyp),
    {OnNew, NewCtx3} =
        fold(Format, State, FType, Fun, NewCtx2, Lvl + 1,
            {table, On}),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
        {Acc ++ [case is_binary(O) of
                     true -> binary_to_list(O);
                     _ -> {ONew, _} =
                         fold(Format, State, FType, Fun,
                             NewCtx, Lvl + 1, O),
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
    RT = {case Format of
              true -> lists:flatten([
                  format_column_pos(
                      State#state.indentation_level - 2),
                  format_keyword("revoke"),
                  case length(ObjsStr) =<
                      ?CR_LIMIT_REVOKE_PRIVILEGE of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          lists:join(", ",
                              [case lists:member(O,
                                  ?OBJECT_PRIVILEGES ++
                                  ?SYSTEM_PRIVILEGES) of
                                   true -> format_keyword(O);
                                   _ -> format_identifier(O)
                               end || O <- ObjsStr])
                      ]);
                      _ ->
                          format_commalist(
                              State#state.indentation_level,
                              [case lists:member(O,
                                  ?OBJECT_PRIVILEGES
                                  ++ ?SYSTEM_PRIVILEGES) of
                                   true ->
                                       format_keyword(O);
                                   _ ->
                                       format_identifier(O)
                               end || O <- ObjsStr])
                  end,
                  case On =/= <<"">> of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level -
                                  1),
                          format_keyword(OnTypNew),
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          OnNew
                      ]);
                      _ -> []
                  end,
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("from"),
                  case length(TosStr) =<
                      ?CR_LIMIT_REVOKE_REVOKEE of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          lists:join(", ",
                              [format_identifier(
                                  T) || T <- TosStr])
                      ]);
                      _ ->
                          format_commalist(
                              State#state.indentation_level,
                              [format_identifier(
                                  T) || T <- TosStr])
                  end,
                  case format_keyword(Opts) of
                      [] -> [];
                      OptsStr -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level -
                                  1),
                          OptsStr
                      ])
                  end
              ]);
              _ -> lists:flatten([
                  "revoke ",
                  string:join(ObjsStr, ", "),
                  " ",
                  case On =/= <<"">> of
                      true -> lists:append(
                          [OnTypNew, " ", OnNew, " "]);
                      _ -> []
                  end,
                  "from ",
                  string:join(TosStr, ", "),
                  case atom_to_list(Opts) of
                      [] -> [];
                      OptsStr -> " " ++ OptsStr
                  end
              ])
          end, NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Role
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl, {Role, Roles} = ST)
    when Role == 'default role'; Role ==
    'default role all except' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> lists:flatten([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("default role"),
                  case Role of
                      'default role' -> [];
                      _ -> " " ++ format_keyword("all except")
                  end,
                  case length(Roles) =<
                      ?CR_LIMIT_ALTER_ROLES of
                      true -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          lists:join(", ", [format_identifier(
                              R) || R <- Roles])
                      ]);
                      _ ->
                          format_commalist(
                              State#state.indentation_level,
                              [format_identifier(
                                  R) || R <- Roles])
                  end
              ]);
              _ -> lists:flatten([
                  atom_to_list(Role),
                  " ",
                  string:join(
                      [binary_to_list(R) || R <- Roles],
                      ", ")
              ])
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, _Lvl, ST)
    when ST == 'default role all'; ST ==
    'default role none' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("default role "),
                  case ST of
                      'default role all' ->
                          format_keyword("all");
                      _ -> format_keyword("none")
                  end
              ]);
              _ -> lists:append([atom_to_list(ST)])
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SCOPE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, FType, Fun, Ctx, _Lvl, {scope, S} = ST)
    when S == <<"local">>; S == <<"cluster">>; S ==
    <<"schema">> ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(S, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([binary_to_list(S)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SELECT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {select, Opts} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NewOs, NewCtx1} =
        lists:foldl(fun({OType, _} = O, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} =
                fold(Format,
                    State#state{select_clause = OType, statement = select},
                    FType, Fun, CtxAcc, Lvl + 1, O),
            case length(SubAcc) > 0 of
                true ->
                    {lists:append([Acc,
                        case Format of
                            true ->
                                case O of
                                    {hints, _} -> " ";
                                    _ -> []
                                end;
                            _ -> " "
                        end, SubAcc
                    ]), CtxAcc1};
                _ ->
                    {Acc, CtxAcc1}
            end
                    end,
            {[], NewCtx},
            Opts),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([
        case State#state.statement =/= select orelse
            State#state.indentation_level == 1 of
            true -> [];
            _ -> "("
        end,
        case Format of
            true -> format_keyword("select") ++ NewOs;
            _ -> "select" ++ lists:flatten(NewOs)
        end,
        case State#state.statement =/= select orelse
            State#state.indentation_level == 1 of
            true -> [];
            _ -> ")"
        end
    ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% START WITH
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'start with', StartWith} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StartWithStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1,
            StartWith),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("start with"),
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level),
                  StartWithStr
              ]);
              _ -> "start with " ++ StartWithStr
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tab
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {Tab, [J | _] = Joins} = ST)
    when is_tuple(J) andalso
    (is_binary(Tab) orelse is_tuple(Tab)) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TabStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1, Tab),
    {JoinsStr, NewCtx2} =
        lists:foldl(fun(Join, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} =
                fold(Format, State, FType, Fun, CtxAcc,
                    Lvl + 1, Join),
            {lists:append([
                Acc,
                case Format of
                    true -> [];
                    _ -> case length(Acc) == 0 of
                             true -> [];
                             _ -> " "
                         end
                end,
                SubAcc
            ]), CtxAcc1}
                    end,
            {[], NewCtx1},
            Joins),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> [TabStr] ++ JoinsStr;
              _ -> lists:flatten([TabStr, " ", JoinsStr])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Table & Dblink & Alias
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {table, {as, Table, Alias, {dblink, Dblink}}} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} = case is_binary(Table) of
                              true -> {binary_to_list(
                                  Table), NewCtx};
                              _ -> fold(Format, State, FType,
                                  Fun, NewCtx,
                                  Lvl + 1, Table)
                          end,
    NewCtx2 = Fun(Alias, NewCtx1),
    NewCtx3 = Fun(Dblink, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true ->
                  lists:append(
                      [format_identifier(
                          TableStr), binary_to_list(
                          Dblink), " ", format_identifier(
                          Alias)]);
              _ -> lists:append(
                  [TableStr, binary_to_list(
                      Dblink), " ", binary_to_list(
                      Alias)])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {table, {as, Table, Alias}} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} = case is_binary(Table) of
                              true -> {binary_to_list(
                                  Table), NewCtx};
                              _ -> fold(Format, State, FType,
                                  Fun, NewCtx,
                                  Lvl + 1, Table)
                          end,
    NewCtx2 = Fun(Alias, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> lists:append(
                  [format_identifier(
                      TableStr), " ", format_identifier(
                      Alias)]);
              _ -> lists:append(
                  [TableStr, " ", binary_to_list(Alias)])
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {table, {param, _} = Table} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl + 1,
            Table),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> format_identifier(TableStr);
              _ -> TableStr
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {table, {Table, {dblink, Dblink}}} =
        ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} = case is_binary(Table) of
                              true -> {binary_to_list(
                                  Table), NewCtx};
                              _ -> fold(Format, State, FType,
                                  Fun, NewCtx,
                                  Lvl + 1, Table)
                          end,
    NewCtx2 = Fun(Dblink, NewCtx1),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {case Format of
              true -> format_identifier(TableStr) ++
              binary_to_list(Dblink);
              _ -> TableStr ++ binary_to_list(Dblink)
          end, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl,
    {table, Table} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} = case is_binary(Table) of
                              true -> {case Format of
                                           true ->
                                               format_identifier(
                                                   Table);
                                           _ ->
                                               binary_to_list(
                                                   Table)
                                       end, NewCtx};
                              _ -> fold(Format, State, FType,
                                  Fun, NewCtx,
                                  Lvl + 1, Table)
                          end,
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {TableStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRUNCATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {'truncate table', Table, Mvl, Storage} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format,
            State#state{select_clause = none, statement = 'truncate table'},
            FType, Fun, NewCtx, Lvl + 1, {table, Table}),
    NewCtx2 = Fun(Mvl, NewCtx1),
    NewCtx3 = Fun(Storage, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(
                      State#state.indentation_level - 2),
                  format_keyword("truncate table"),
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level),
                  TableStr,
                  case Mvl of
                      {} -> [];
                      {'materialized view log', T} ->
                          lists:append([
                              ?CHAR_NEWLINE,
                              format_column_pos(
                                  State#state.indentation_level -
                                      1),
                              format_keyword(T),
                              " ",
                              format_keyword(
                                  "materialized view log")
                          ])
                  end,
                  case Storage of
                      {} -> [];
                      {'storage', T} -> lists:append([
                          case Mvl of
                              {} ->
                                  ?CHAR_NEWLINE ++
                                  format_column_pos(
                                      State#state.indentation_level -
                                          1);
                              _ -> " "
                          end,
                          format_keyword(T),
                          " ",
                          format_keyword("storage")
                      ])
                  end
              ]);
              _ -> lists:append([
                  "truncate table ",
                  TableStr,
                  " ",
                  case Mvl of
                      {} -> [];
                      {'materialized view log', T} ->
                          lists:append(
                              [atom_to_list(
                                  T), " materialized view log "])
                  end,
                  case Storage of
                      {} -> [];
                      {'storage', T} ->
                          lists:append(
                              [atom_to_list(T), " storage"])
                  end
              ])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TYPE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, FType, Fun, Ctx, _Lvl, {type, T} = ST)
    when T == 'set'; T == 'ordered_set'; T ==
    'bag'; is_binary(T) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(T, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([binary_to_list(T)]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNLIMITED ON
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl,
    {'unlimited on', T} = ST)
    when is_binary(T) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(T, NewCtx),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("quota"),
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level),
                  format_keyword("unlimited on "),
                  format_identifier(T)
              ]);
              _ -> "quota unlimited on " ++ binary_to_list(T)
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UPDATE TABLE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {update, Table, {set, Set}, Where, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        fold(Format,
            State#state{select_clause = none, statement = update},
            FType, Fun, NewCtx, Lvl + 1, {table, Table}),
    {Sets, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
        {SubAcc, CtxAcc1} =
            fold(Format,
                State#state{select_clause = none, statement = update},
                FType, Fun, CtxAcc, Lvl + 1, S),
        {Acc ++ [SubAcc], CtxAcc1}
                                  end,
        {[], NewCtx1},
        Set),
    {WhereStr, NewCtx3} =
        fold(Format,
            State#state{select_clause = none, statement = update},
            FType, Fun, NewCtx2, Lvl + 1, Where),
    {ReturnStr, NewCtx4} =
        case Return of
            {_, {}} -> {[], NewCtx3};
            _ -> fold(Format,
                State#state{select_clause = none, statement = update},
                FType,
                Fun, NewCtx3, Lvl + 1, Return)
        end,
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_column_pos(
                      State#state.indentation_level - 2),
                  format_keyword("update"),
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level),
                  TableStr,
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("set"),
                  format_commalist(
                      State#state.indentation_level, Sets),
                  case WhereStr of
                      [] -> [];
                      _ -> WhereStr
                  end,
                  case ReturnStr of
                      [] -> [];
                      _ -> ReturnStr
                  end
              ]);
              _ -> lists:append([
                  "update ",
                  TableStr,
                  " set ",
                  string:join(Sets, ", "),
                  case WhereStr of
                      [] -> [];
                      _ -> " " ++ WhereStr
                  end,
                  case ReturnStr of
                      [] -> [];
                      _ -> " " ++ ReturnStr
                  end
              ])
          end, NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USING
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {using, ColumnList} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ColumnListStr, NewCtx1} =
        lists:foldl(fun(C, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} =
                fold(Format, State, FType, Fun, CtxAcc,
                    Lvl + 1, C),
            {Acc ++ [SubAcc], CtxAcc1}
                    end,
            {[], NewCtx},
            ColumnList),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_keyword("using"),
                  case is_simple_list(ColumnList,
                      ?CR_LIMIT_USING) of
                      true ->
                          lists:flatten(
                              [" (", lists:join(", ",
                                  ColumnListStr), ")"]);
                      _ -> lists:append([
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          "(",
                          format_commalist(
                              State#state.indentation_level +
                                  1,
                              ColumnListStr),
                          ?CHAR_NEWLINE,
                          format_column_pos(
                              State#state.indentation_level),
                          ")"
                      ])
                  end
              ]);
              _ -> lists:append(
                  ["using(", string:join(ColumnListStr,
                      ", "), ")"])
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHENEVER NOT FOUND
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {when_not_found, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> format_keyword("whenever not found ");
              _ -> "whenever not found "
          end ++ ValueStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHENEVER SQLERROR
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {when_sql_err, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Format of
              true -> format_keyword("whenever sqlerror ");
              _ -> "whenever sqlerror "
          end ++ ValueStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All where clauses
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {where, Where} = ST)
    when is_tuple(Where) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {WhereStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl, Where),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case length(WhereStr) > 0 of
              true -> case Format of
                          true -> lists:append([
                              ?CHAR_NEWLINE,
                              format_column_pos(
                                  State#state.indentation_level -
                                      1),
                              format_keyword("where"),
                              ?CHAR_NEWLINE,
                              format_column_pos(
                                  State#state.indentation_level),
                              format_search_condition(
                                  State#state.indentation_level,
                                  WhereStr)
                          ]);
                          _ -> "where " ++ WhereStr
                      end;
              _ -> []
          end, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHERE_CURRENT_OF
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, _Lvl,
    {where_current_of, {cur, CurName}} =
        ST) -> ?debugFmt(
    ?MODULE_STRING ++ ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
    [Format, _Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> lists:append([
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level - 1),
                  format_keyword("where current of"),
                  ?CHAR_NEWLINE,
                  format_column_pos(
                      State#state.indentation_level),
                  format_identifier(CurName)
              ]);
              _ -> "where current of " ++ CurName
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Empty list or tuples
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, _FType, _Fun, Ctx, _Lvl, X = _ST)
    when X =:= {}; X =:= [] ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, _ST]),
    RT = {[], Ctx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JSON parser hooking
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, _FType, _Fun, Ctx, _Lvl,
    {Op, Columns, _} = ST)
    when Op =:= '{}';Op =:= '[]' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
    {ok, JPPath} = jpparse_fold:string(ST),
    JPPathList = binary_to_list(JPPath),
    RT = {case Columns of
              _ when is_tuple(Columns) ->
                  Target = decompose_tuple(Columns),
                  lists:append([
                      string:trim(decompose_tuple(Columns),
                          trailing, "."),
                      "|",
                      string:sub_string(JPPathList,
                          length(Target) + 1),
                      "|"
                  ]);
              empty ->
                  lists:append([
                      "|",
                      JPPathList,
                      "|"
                  ]);
              _ ->
                  Target =
                      string:trim(binary_to_list(Columns),
                          trailing, "."),
                  lists:append([
                      Target,
                      "|",
                      string:sub_string(JPPathList,
                          length(Target) + 1),
                      "|"
                  ])
          end, Ctx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, _State, _FType, _Fun, Ctx, _Lvl, {Op, _, _} = ST)
    when Op =:= ':'; Op =:= '::'; Op =:= '#' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, ST]),
    {ok, JPPath} = jpparse_fold:string(ST),
    JPPathList = binary_to_list(JPPath),
    Others = decompose_tuple(ST),
    RT = {case Format of
              true -> lists:append([
                  format_identifier(
                      string:trim(Others, trailing, ".")),
                  "|",
                  string:sub_string(JPPathList,
                      length(Others) + 1),
                  "|"
              ]);
              _ ->
                  lists:append(
                      [string:trim(Others, trailing,
                          "."), "|", string:sub_string(
                          JPPathList,
                          length(Others) + 1), "|"])
          end, Ctx},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Boolean and arithmetic binary operators handled with precedence
% *,/ > +,- > and > or
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_tuple(L), is_tuple(R), Op /= fetch,
    Op /=
        'connect by' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Fl, NewCtx1} = case {Op, element(1, L)} of
                        {'*', Ol} when Ol =:= '-'; Ol =:=
                            '+' ->
                            {Ls, NC1} =
                                fold(Format, State, FType,
                                    Fun, NewCtx, Lvl + 1,
                                    L),
                            {lists:append(
                                ["(", Ls, ")"]), NC1};
                        {'/', Ol} when Ol =:= '-'; Ol =:=
                            '+' ->
                            {Ls, NC1} =
                                fold(Format, State, FType,
                                    Fun, NewCtx, Lvl + 1,
                                    L),
                            {lists:append(
                                ["(", Ls, ")"]), NC1};
                        {'and', 'or'} ->
                            {Ls, NC1} =
                                fold(Format, State, FType,
                                    Fun, NewCtx, Lvl + 1,
                                    L),
                            {[lists:append(
                                ["(", Ls, ")"])], NC1};
                        {_, select} ->
                            fold(Format,
                                State#state{indentation_level =
                                State#state.indentation_level +
                                    1, statement = select},
                                FType, Fun, NewCtx,
                                Lvl + 1, L);
                        _ -> fold(Format, State, FType, Fun,
                            NewCtx, Lvl + 1, L)
                    end,
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
                        {'*', Or} when Or =:= '-'; Or =:=
                            '+' ->
                            {Rs, NC2} =
                                fold(Format, State, FType,
                                    Fun, NewCtx2,
                                    Lvl + 1, R),
                            {lists:append(
                                ["(", Rs, ")"]), NC2};
                        {'/', Or} when Or =:= '-'; Or =:=
                            '+' ->
                            {Rs, NC2} =
                                fold(Format, State, FType,
                                    Fun, NewCtx2,
                                    Lvl + 1, R),
                            {lists:append(
                                ["(", Rs, ")"]), NC2};
                        {'and', 'or'} ->
                            {Rs, NC2} =
                                fold(Format, State, FType,
                                    Fun, NewCtx2,
                                    Lvl + 1, R),
                            {[lists:append(
                                ["(", Rs, ")"])], NC2};
                        {_, select} ->
                            fold(Format,
                                State#state{indentation_level =
                                State#state.indentation_level +
                                    1, statement = select},
                                FType, Fun, NewCtx2,
                                Lvl + 1, R);
                        _ ->
                            fold(Format, State, FType, Fun,
                                NewCtx2, Lvl + 1, R)
                    end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> case string:slice(Fl, 0, 1) of
                          "(" ->
                              [Fl, format_operator(
                                  State#state.indentation_level,
                                  Op,
                                  false), Fr];
                          _ -> lists:flatten([
                              Fl,
                              format_operator(
                                  State#state.indentation_level,
                                  Op,
                                  false),
                              Fr
                          ])
                      end;
              _ -> lists:flatten(
                  [Fl, " ", atom_to_list(Op), " ", Fr])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_binary(L), is_tuple(R), Op /=
    'connect by' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
                        {'*', Or} when Or =:= '-'; Or =:=
                            '+' ->
                            {Rs, NC} =
                                fold(Format, State, FType,
                                    Fun, NewCtx2,
                                    Lvl + 1, R),
                            {lists:append(
                                ["(", Rs, ")"]), NC};
                        {'/', Or} when Or =:= '-'; Or =:=
                            '+' ->
                            {Rs, NC} =
                                fold(Format, State, FType,
                                    Fun, NewCtx2,
                                    Lvl + 1, R),
                            {lists:append(
                                ["(", Rs, ")"]), NC};
                        {_, select} ->
                            fold(Format,
                                State#state{indentation_level =
                                State#state.indentation_level +
                                    1, statement = select},
                                FType, Fun, NewCtx2,
                                Lvl + 1, R);
                        _ ->
                            fold(Format, State, FType, Fun,
                                NewCtx2, Lvl + 1, R)
                    end,
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:append([
                  format_identifier(L),
                  format_operator(
                      State#state.indentation_level + 1, Op,
                      false),
                  Fr]);
              _ -> lists:append(
                  [binary_to_list(L), " ", atom_to_list(
                      Op), " ", Fr])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_tuple(L), is_binary(R), Op /=
    'connect by' ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Fl, NewCtx1} = case {Op, element(1, L)} of
                        {'*', Ol} when Ol =:= '-'; Ol =:=
                            '+' ->
                            {Ls, NC} =
                                fold(Format, State, FType,
                                    Fun, NewCtx, Lvl + 1,
                                    L),
                            {lists:append(
                                ["(", Ls, ")"]), NC};
                        {'/', Ol} when Ol =:= '-'; Ol =:=
                            '+' ->
                            {Ls, NC} =
                                fold(Format, State, FType,
                                    Fun, NewCtx, Lvl + 1,
                                    L),
                            {lists:append(
                                ["(", Ls, ")"]), NC};
                        {_, select} ->
                            fold(Format,
                                State#state{indentation_level =
                                State#state.indentation_level +
                                    1, statement = select},
                                FType, Fun, NewCtx,
                                Lvl + 1, L);
                        _ -> fold(Format, State, FType, Fun,
                            NewCtx, Lvl + 1, L)
                    end,
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Format of
              true -> lists:append([
                  Fl,
                  format_operator(
                      State#state.indentation_level, Op,
                      false),
                  format_identifier(R)
              ]);
              _ -> lists:append(
                  [Fl, " ", atom_to_list(
                      Op), " ", binary_to_list(R)])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(true = _Format, _State, _FType, Fun, Ctx, _Lvl,
    {is = Op, L, <<"null">> = R} = ST)
    when is_binary(L) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    RT = {lists:append([
        format_identifier(L),
        " ",
        format_keyword(Op),
        " ",
        format_keyword(R)
    ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(Format, State, FType, Fun, Ctx, _Lvl, {Op, L, R} = ST)
    when is_atom(Op), is_binary(L), is_binary(R) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, State#state.indentation_level, ST]),
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
    RT = {case Format of
              true -> lists:append([
                  format_identifier(L),
                  format_operator(
                      State#state.indentation_level + 1, Op,
                      false),
                  format_identifier(R)
              ]);
              _ -> lists:append(
                  [binary_to_list(L), " ", atom_to_list(
                      Op), " ", binary_to_list(R)])
          end, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Index options
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, _State, FType, Fun, Ctx, _Lvl, ST) when is_atom(
    ST) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {case Format of
              true -> format_keyword(ST);
              _ -> atom_to_list(ST)
          end, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;
fold(_Format, _State, FType, Fun, Ctx, _Lvl,
    {Table, {dblink, Dblink}} =
        ST) when is_binary(
    Table) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {TableStr, NewCtx1} =
        {binary_to_list(Table) ++
            binary_to_list(Dblink), NewCtx},
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {TableStr, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JSONPath anchors
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(Format, State, FType, Fun, Ctx, Lvl,
    {Anchor, {Op, _, _} = JSON, Bracket} =
        ST)
    when
    (Op =:= '{}' orelse Op =:= '[]' orelse Op =:= ':' orelse
        Op =:= '::' orelse
        Op =:= '#')
        andalso (Bracket =:= [] orelse Bracket =:= '(') ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [Format, Lvl, State#state.indentation_level, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AnchorStr, NewCtx1} =
        fold(Format, State, FType, Fun, NewCtx, Lvl, Anchor),
    {JSONStr, NewCtx2} =
        fold(Format, State, FType, Fun, NewCtx1, Lvl, JSON),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([
        case Bracket of
            '(' -> "(";
            _ -> []
        end,
        AnchorStr,
        case Bracket of
            '(' -> ")";
            _ -> []
        end,
        JSONStr
    ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":fold ===>~n RT: ~p~n",
        [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_Format, _State, _FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt(?MODULE_STRING ++
    ":fold ===> Start ~p-~p-~p~n ST: ~p~n",
        [_Format, _Lvl, _State#state.indentation_level, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

columns_join([], _Separator, Result) ->
    Result;
columns_join([Head | Tail], Separator, Result) ->
    columns_join(Tail, Separator, lists:append([
        Result,
        case Result of
            [] -> [];
            _ -> Separator
        end,
        case string:sub_string(Head, 1, 7) == "select " of
            true -> lists:append(["(", Head, ")"]);
            _ -> Head
        end
    ])).

decompose_tuple({_, _, X}) when is_tuple(X) ->
    decompose_tuple(X);
decompose_tuple({_, _, X}) when is_binary(X) ->
    binary_to_list(X);
decompose_tuple({_, _, empty}) ->
    [].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determining the current column position.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_column_pos(IndentationLevel) ->
    format_column_pos(IndentationLevel, []).

format_column_pos(IndentationLevel, Acc)
    when IndentationLevel =< 0 ->
    Acc;
format_column_pos(IndentationLevel, Acc) ->
    format_column_pos(IndentationLevel - 1, Acc ++ case ?INDENT_WITH of
                                                       tab -> ?CHAR_TAB;
                                                       _ ->
                                                           case ?INDENT_SPACES of
                                                               2 -> "  ";
                                                               3 -> "   ";
                                                               4 -> "    ";
                                                               5 -> "     ";
                                                               6 -> "      ";
                                                               7 -> "       ";
                                                               8 -> "        ";
                                                               _ -> " "
                                                           end
                                                   end).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting comma separated lists.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_commalist(IndentationLevel, List = _ST) ->
    format_commalist(IndentationLevel, List, []).

format_commalist(_IndentationLevel, [], Acc) ->
    Acc;
format_commalist(IndentationLevel, [Head | Tail], Acc) ->
    format_commalist(IndentationLevel, Tail, lists:append([
        Acc,
        ?CHAR_NEWLINE,
        format_column_pos(IndentationLevel),
        lists:flatten(Head),
        case string:slice(lists:append(Tail), 0, 1) == ")" orelse Tail == [] of
            true -> [];
            _ -> Next = string:casefold(lists:nth(1, Tail)),
                case string:slice(Next, 0, 5) == "full " orelse
                    string:slice(Next, 0, 6) == "cross " orelse
                    string:slice(Next, 0, 6) == "inner " orelse
                    string:slice(Next, 0, 5) == "join " orelse
                    string:slice(Next, 0, 5) == "left " orelse
                    string:slice(Next, 0, 8) == "natural " orelse
                    string:slice(Next, 0, 3) == "on " orelse
                    string:slice(Next, 0, 10) == "partition " orelse
                    string:slice(Next, 0, 6) == "right " orelse
                    string:slice(Next, 0, 6) == "using " orelse
                    string:slice(Next, 0, 6) == "using" ++ ?CHAR_NEWLINE of
                    true -> [];
                    _ -> ","
                end
        end
    ])).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting data types.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_data_type(ST)
    when is_binary(ST) ->
    format_data_type(binary_to_list(ST));
format_data_type(ST) ->
    STLower = string:casefold(ST),
    case lists:member(ST, ?DATA_TYPES) of
        true -> format_keyword(STLower);
        _ -> format_identifier(STLower)
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting identifiers.
% ------------------------------------------------------------------------------
% Allowed values: init_cap, keep_unchanged, lower,upper
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_identifier(Identifier)
    when is_binary(Identifier) ->
    format_identifier(binary_to_list(Identifier));
format_identifier(Identifier = _ST) ->
    case Identifier of
        "*" -> Identifier;
        _ -> Fun_4 = string:slice(Identifier, 0, 4),
            case Fun_4 == "fun " orelse Fun_4 == "fun(" of
                true -> Identifier;
                _ -> I_1 = lists:sublist(Identifier, 1),
                    case I_1 == "'" orelse I_1 == "\"" of
                        true -> Identifier;
                        _ -> case lists:member(string:uppercase(Identifier),
                            get_funs()) of
                                 true -> format_keyword(Identifier);
                                 _ -> case ?CASE_IDENTIFIER of
                                          keep_unchanged -> Identifier;
                                          lower -> string:casefold(Identifier);
                                          upper -> string:uppercase(Identifier);
                                          _ -> format_init_cap(
                                              string:casefold(Identifier), [],
                                              [])
                                      end
                             end
                    end
            end
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting init_cap version.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_init_cap([], _, Acc) ->
    Acc;
format_init_cap([Head | Tail], Previous, Acc) ->
    format_init_cap(Tail, Head,
        Acc ++
        case Previous == [] orelse lists:member([Previous], [" ", "_", "."]) of
            true -> string:uppercase([Head]);
            _ -> [Head]
        end).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting keywords.
% ------------------------------------------------------------------------------
% Allowed values: init_cap, lower,upper
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_keyword(Keyword)
    when is_atom(Keyword) ->
    format_keyword(atom_to_list(Keyword));
format_keyword(Keyword)
    when is_binary(Keyword) ->
    format_keyword(binary_to_list(Keyword));
format_keyword(Keyword) ->
    case ?CASE_KEYWORD of
        lower -> Keyword;
        upper -> string:uppercase(Keyword);
        _ -> format_init_cap(Keyword, [], [])
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting operators.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_operator(IndentationLevel, Op = _ST, IsUnary)
    when is_atom(Op) ->
    format_operator(IndentationLevel, atom_to_list(Op), IsUnary);

format_operator(IndentationLevel, Op = _ST, IsUnary) ->
    case Op == "and" orelse Op == "or" of
        true -> lists:append([
            ?CHAR_NEWLINE,
            format_column_pos(IndentationLevel),
            format_keyword(Op),
            " "
        ]);
        _ -> case Op == "not" of
                 true -> format_keyword(Op);
                 _ -> case IsUnary of
                          true -> case ?WS_OPERATORS of
                                      true -> string:trim(Op, both, " ") ++ " ";
                                      _ -> Op
                                  end;
                          _ ->
                              case ?WS_OPERATORS /= true andalso (
                                  Op == "=" orelse Op == "!=" orelse
                                      Op == "^=" orelse
                                      Op == "<>" orelse Op == "<" orelse
                                      Op == ">" orelse
                                      Op == "<=" orelse Op == ">=") of
                                  true -> Op;
                                  _ -> lists:append(
                                      [" ", string:trim(Op, both, " "), " "])
                              end
                      end
             end
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting search conditions.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_search_condition(IndentationLevel, [Left, Op, Right] = _ST) ->
    lists:append([
        Left,
        format_operator(IndentationLevel, Op, false),
        Right
    ]);
format_search_condition(_IndentationLevel, SearchCondition = _ST) ->
    SearchCondition.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Delivers the standard functions from the lexer.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_funs() ->
    [string:trim(string:trim(F, leading, "^(?i)("), trailing,
        ")$") || {F, 'FUNS'} <- ?TOKENPATTERNS].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checking if a given list is a simple one.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_simple_list(List, Limit) ->
    case length(List) > Limit of
        true -> false;
        _ -> is_simple_list(List)
    end.

is_simple_list([]) ->
    true;
is_simple_list([Head | Tail]) ->
    case Head of
        {as, {'case', _, _, _}, _} -> false;
        {as, {intersect, _, _}, _} -> false;
        {as, {minus, _, _}, _} -> false;
        {as, {select, _}, _} -> false;
        {as, {union, _, _}, _} -> false;
        {as, {'union all', _, _}, _} -> false;
        {'case', _, _, _} -> false;
        {'fun', _, _} -> false;
        {intersect, _, _} -> false;
        {minus, _, _} -> false;
        {select, _} -> false;
        {union, _, _} -> false;
        {'union all', _, _} -> false;
        _ -> is_simple_list(Tail)
    end.
