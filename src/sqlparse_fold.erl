-module(sqlparse_fold).

-export([fold/5]).

% 
% List of parsetrees
%
fold(FType, Fun, Ctx, Lvl, STs) when is_list(STs)->
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
    {SqlStr, NewCtx2};

% 
% Handling of Extra part
%
fold(FType, Fun, Ctx, Lvl, {Pt, {extra, Bin}} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {SqlStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Pt),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {SqlStr ++ if Bin /= <<>> ->
                  ";" ++ binary_to_list(Bin);
                  true -> ""
               end,
     NewCtx2};

%
% SELECT
%
fold(FType, Fun, Ctx, Lvl, {select, Opts} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {NewOs, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, O),
            {Acc++[SubAcc], CtxAcc1}
        end,
        {[], NewCtx},
        Opts),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"select "++lists:flatten(NewOs), NewCtx2};

%
% INSERT
%
fold(FType, Fun, Ctx, _Lvl, {insert, Tab, {}, {}, {}} = ST)
  when is_binary(Tab) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {"insert into " ++ binary_to_list(Tab)
    , NewCtx};
fold(FType, Fun, Ctx, Lvl, {insert, Tab, {cols, Cols}, {values, Values}, Return} = ST)
  when is_binary(Tab) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {CStrs, NewCtx1} = lists:foldl(fun(C, {Acc, CtxAcc}) ->
            case C of
                C when is_binary(C) ->
                    {Acc++[binary_to_list(C)], Fun(C, CtxAcc)};
                C ->
                    {CT, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, C),
                    {Acc++[CT], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Cols),
    {Vals, NewCtx2} = lists:foldl(fun(V, {Acc1, CtxAcc1}) ->
            case V of
                V when is_binary(V) ->
                    {Acc1++[binary_to_list(V)], Fun(V, CtxAcc1)};
                V ->
                    {VT, CtxAcc2} = fold(FType, Fun, CtxAcc1, Lvl+1, V),
                    {Acc1++[VT], CtxAcc2}
            end
        end,
        {[], NewCtx1},
        Values),
    {Ret, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl+1, Return),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {"insert into " ++ binary_to_list(Tab) ++
     case length(CStrs) of
         0 -> "";
         _ -> lists:flatten(["(", string:join(CStrs, ","), ")"])
     end ++ " values (" ++ string:join(Vals, ",") ++ ")" ++ Ret
    , NewCtx4};

%
% CREATE TABLE
%
fold(FType, Fun, Ctx, Lvl, {'create table', Tab, Fields, Opts} = ST)
  when is_binary(Tab) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {OptsStr, NewCtx1}
    = lists:foldl(
        fun(Opt, {Str, AccCtx}) ->
                {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl+1, Opt),
                {Str++NewStr, NewAccCtx}
        end, {"", NewCtx}, Opts),
    NewCtx2 = Fun(Tab, NewCtx1),
    {Clms, NewCtx3} = lists:foldl(fun(Clm, {Acc, CtxAcc}) ->
            case Clm of
                {C, {T, N}, O} when is_binary(C) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    CtxAcc3 = Fun(N, CtxAcc2),
                    {SubAcc, CtxAcc4} = fold(FType, Fun, CtxAcc3, Lvl+1, O),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ",
                                            atom_to_list(T), "(", N, ") ",
                                            SubAcc])]
                    , CtxAcc4};
                {C, {T, N, N1}, O} when is_binary(C) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    CtxAcc3 = Fun(N, CtxAcc2),
                    CtxAcc4 = Fun(N1, CtxAcc3),
                    {SubAcc, CtxAcc5} = fold(FType, Fun, CtxAcc4, Lvl+1, O),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ",
                                            atom_to_list(T), "(",N,",",N1,") ",
                                            SubAcc])]
                    , CtxAcc5};
                {C, T, O} when is_binary(C) and is_binary(T) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    {SubAcc, CtxAcc3} = fold(FType, Fun, CtxAcc2, Lvl+1, O),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ",
                                            binary_to_list(T), " ", SubAcc])]
                    , CtxAcc3};
                {C, T, O} when is_binary(C) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    {SubAcc, CtxAcc3} = fold(FType, Fun, CtxAcc2, Lvl+1, O),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ",
                                            atom_to_list(T), " ", SubAcc])]
                    , CtxAcc3};
                Clm ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, Clm),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx2},
        Fields),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {"create " ++ OptsStr ++ " table " ++ binary_to_list(Tab)
        ++ " (" ++ string:join(Clms, ", ") ++ ")"
    , NewCtx4};

%
% CREATE USER
%
fold(FType, Fun, Ctx, Lvl, {'create user', Usr, Id, Opts} = ST)
  when is_binary(Usr) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Usr, NewCtx),
    {IdStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Id),
    {OptsStr, NewCtx3}
    = lists:foldl(
        fun(Opt, {Str, AccCtx}) ->
                {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl+1, Opt),
                {Str++NewStr, NewAccCtx}
        end, {"", NewCtx2}, Opts),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {"create user " ++ binary_to_list(Usr)
    ++ IdStr ++ " " ++ OptsStr
    , NewCtx4};

%
% CREATE INDEX
%
fold(FType, Fun, Ctx, Lvl, {'create index', Opts, Idx, Table, Spec, Norm, Filter} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {OptsStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Opts),
    {IdxStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Idx),
    {TableStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl+1, Table),
    {Specs, NewCtx4} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, S),
            {Acc ++ [SubAcc], CtxAcc1}
        end,
        {[], NewCtx3},
        Spec),
    {NormStr, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl+1, Norm),
    {FilterStr, NewCtx6} = fold(FType, Fun, NewCtx5, Lvl+1, Filter),
    NewCtx7 = case FType of
        top_down -> NewCtx6;
        bottom_up -> Fun(ST, NewCtx6)
    end,
    {"create " ++ OptsStr
     ++ " index " ++ IdxStr
     ++ " on " ++ TableStr 
     ++ " (" ++ string:join(Specs, "|") ++ ") "
     ++ if NormStr =/= [] -> NormStr; true -> "" end
     ++ if FilterStr =/= [] -> FilterStr; true -> "" end
    , NewCtx7};

%
% CREATE ROLE
%
fold(FType, Fun, Ctx, Lvl, {'create role', Role} = ST)
  when is_binary(Role) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {RoleStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Role),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"create role " ++ RoleStr,
     NewCtx2};

%
% ALTER USER
%
fold(FType, Fun, Ctx, Lvl, {'alter user', Usr, {spec, Opts}} = ST)
  when is_binary(Usr) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Usr, NewCtx),
    {OptsStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Opts),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {lists:flatten(["alter user ", binary_to_list(Usr), " ", OptsStr]),
     NewCtx3};

%
% TRUNCATE TABLE
%
fold(FType, Fun, Ctx, _Lvl, {'truncate table', Tbl, Mvl, Storage} = ST)
  when is_binary(Tbl) ->
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
    {"truncate table " ++ binary_to_list(Tbl) ++ " " ++
    case Mvl of
        {} -> "";
        {'materialized view log', T} -> lists:flatten([atom_to_list(T), " materialized view log "])
    end
    ++
    case Storage of
        {} -> "";
        {'storage', T} -> lists:flatten([atom_to_list(T), " storage"])
    end,
    NewCtx4};

%
% UPDATE TABLE
%
fold(FType, Fun, Ctx, Lvl, {'update', Tbl, {set, Set}, Where, Return} = ST)
  when is_binary(Tbl) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Tbl, NewCtx),
    {Sets, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, S),
            {Acc ++ [SubAcc], CtxAcc1}
        end,
        {[], NewCtx1},
        Set),
    {WhereStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl+1, Where),
    {ReturnStr, NewCtx4} = fold(FType, Fun, NewCtx3, Lvl+1, Return),
    NewCtx5 = case FType of
        top_down -> NewCtx4;
        bottom_up -> Fun(ST, NewCtx4)
    end,
    {"update " ++ binary_to_list(Tbl)
    ++ " set " ++ string:join(Sets, ",")
    ++ if length(WhereStr) > 0 orelse length(ReturnStr) > 0 ->
              " " ++ WhereStr ++ ReturnStr;
          true -> ""
       end,
    NewCtx5};

%
% DROPS
%
fold(FType, Fun, Ctx, Lvl, {'drop user', Usr, Opts} = ST)
  when is_binary(Usr) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Usr, NewCtx),
    {OptsStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Opts),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {"drop user " ++ binary_to_list(Usr)
     ++ " " ++ OptsStr
    , NewCtx3};
fold(FType, Fun, Ctx, _Lvl, {'drop function', FunctionName} = ST)
  when is_binary(FunctionName) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(FunctionName, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"drop function " ++ binary_to_list(FunctionName)
    , NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'drop procedure', ProcedureName} = ST)
  when is_binary(ProcedureName) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(ProcedureName, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"drop procedure " ++ binary_to_list(ProcedureName)
    , NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'drop table', {tables, Ts}, E, RC} = ST)
  when (is_atom(RC) orelse (RC =:= {})) andalso
       (is_atom(E) orelse (E =:= {})) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(E, NewCtx),
    {Tables, NewCtx2} = lists:foldl(fun(T, {Acc, CtxAcc}) ->
            CtxAcc1 = Fun(T, CtxAcc),
            {Acc++[binary_to_list(T)], CtxAcc1}
        end,
        {[], NewCtx1},
        Ts),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {"drop table "
     ++ if E =:= exists -> " if exists "; true -> "" end
     ++ string:join(Tables, ", ")
     ++ if is_atom(RC) -> " " ++ atom_to_list(RC); true -> "" end
    , NewCtx3};
fold(FType, Fun, Ctx, _Lvl, {'drop index', Indx, Tbl} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = case FType of
        top_down -> NewCtx;
        bottom_up -> Fun(ST, NewCtx)
    end,
    {"drop index "
     ++ if Indx == {} -> "from ";
           true -> binary_to_list(Indx) ++ " from "
        end
     ++ binary_to_list(Tbl)
    , NewCtx1};
fold(FType, Fun, Ctx, Lvl, {'drop role', Role} = ST)
  when is_binary(Role) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {RoleStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Role),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"drop role " ++ RoleStr
    , NewCtx2};

%
% DELETE
%
fold(FType, Fun, Ctx, Lvl, {'delete', Table, Where, Return} = ST)
  when is_binary(Table) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Table, NewCtx),
    {WhereStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Where),
    {ReturnStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl+1, Return),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {"delete from " ++ binary_to_list(Table)
     ++ " " ++ WhereStr ++ ReturnStr
    , NewCtx4};

%
% GRANT
%
fold(FType, Fun, Ctx, _Lvl, {'grant', Objs, {OnTyp, On}, {'to', Tos}, Opts} = ST)
  when is_atom(OnTyp), is_atom(Opts) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[atom_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx},
        Objs),
    NewCtx2 = Fun(OnTyp, NewCtx1),
    NewCtx3 = Fun(On, NewCtx2),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    NewCtx6 = case FType of
        top_down -> NewCtx5;
        bottom_up -> Fun(ST, NewCtx5)
    end,
    {"grant "
     ++ string:join(ObjsStr, ",") ++ " "
     ++ if On =/= <<"">> -> atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
     ++ if length(Tos) > 0 -> "to " ++ string:join(TosStr, ",") ++ " "; true -> "" end
     ++ atom_to_list(Opts)
    , NewCtx6};

%
% REVOKE
%
fold(FType, Fun, Ctx, _Lvl, {'revoke', Objs, {OnTyp, On}, {'from', Tos}, Opts} = ST)
  when is_atom(OnTyp), is_atom(Opts) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[atom_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx},
        Objs),
    NewCtx2 = Fun(OnTyp, NewCtx1),
    NewCtx3 = Fun(On, NewCtx2),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    NewCtx6 = case FType of
        top_down -> NewCtx5;
        bottom_up -> Fun(ST, NewCtx5)
    end,
    {"revoke "
     ++ string:join(ObjsStr, ",") ++ " "
     ++ if On =/= <<"">> -> atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
     ++ if length(Tos) > 0 -> "from " ++ string:join(TosStr, ",") ++ " "; true -> "" end
     ++ atom_to_list(Opts)
    , NewCtx6};

%--------------------------------------------------------------------
% component matching patterns
%

% Empty list or tuples
fold(_FType, _Fun, Ctx, _Lvl, X) when X =:= {}; X =:= [] -> {"", Ctx};

% All option and optionlist and its variants
fold(FType, Fun, Ctx, Lvl, {'identified globally', E} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {IdStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, E),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {" identified globally " ++ IdStr,
     NewCtx2};
fold(FType, Fun, Ctx, Lvl, {'identified extern', E} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {IdStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, E),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {" identified externally " ++ IdStr,
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'identified by', Pswd} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Pswd, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {" identified by " ++ binary_to_list(Pswd),
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'account', LockUnlock} = ST)
  when LockUnlock == 'lock'; LockUnlock == 'unlock' ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(LockUnlock, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {" account " ++ atom_to_list(LockUnlock) ++ " ",
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'password', 'expire'} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = case FType of
        top_down -> NewCtx;
        bottom_up -> Fun(ST, NewCtx)
    end,
    {" password expire ",
     NewCtx1};
fold(FType, Fun, Ctx, _Lvl, {'unlimited on', T} = ST)
  when is_binary(T) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(T, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"quota unlimited on " ++ binary_to_list(T) ++ " ",
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'unlimited on', T} = ST)
  when is_binary(T) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(T, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"quota unlimited on " ++ binary_to_list(T) ++ " ",
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'scope', S} = ST)
  when S == 'local'; S == 'cluster'; S == 'schema' ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(S, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten([" ", atom_to_list(S), " "]),
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'type', T} = ST)
  when T == 'set'; T == 'ordered_set'; T == 'bag' ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(T, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten([" ", atom_to_list(T), " "]),
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {TS, Tab} = ST)
  when TS == 'default tablespace'; TS == 'temporary tablespace' ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Tab, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten([" ", atom_to_list(TS), " ", binary_to_list(Tab), " "]),
     NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {'profile', Profile} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(Profile, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten([" profile ", binary_to_list(Profile), " "]),
     NewCtx2};
fold(FType, Fun, Ctx, Lvl, {'quotas', Quotas} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {QuotaStr, NewCtx1}
    = lists:foldl(
        fun(Quota, {Str, AccCtx}) ->
                {NewStr, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl+1, Quota),
                {Str++NewStr, NewAccCtx}
        end, {"", NewCtx}, Quotas),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {QuotaStr ++ " ",
     NewCtx2};

%fold(FType, Fun, Ctx, Lvl, [{'type', T}|Opts] = ST) ->
%    NewCtx = case FType of
%        top_down -> Fun(ST, Ctx);
%        bottom_up -> Ctx
%    end,
%    NewCtx1 = Fun(T, NewCtx),
%    {OptsStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Opts),
%    NewCtx3 = case FType of
%        top_down -> NewCtx2;
%        bottom_up -> Fun(ST, NewCtx2)
%    end,
%    {lists:flatten([" ", atom_to_list(T), " ", OptsStr]),
%     NewCtx3};
fold(FType, Fun, Ctx, Lvl, {'limited', Q, T} = ST)
  when is_binary(Q), is_binary(T) ->
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
    {lists:flatten(["quota ", binary_to_list(Q), " on ", binary_to_list(T),
                    " "]),
     NewCtx3};
fold(FType, Fun, Ctx, Lvl, [cascade|Opts] = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {OptsStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Opts),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten([" cascade ", OptsStr])
    , NewCtx2};
fold(FType, Fun, Ctx, Lvl, {'default', Def} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {DefStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Def),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten([" default ", 
        case Def of
            Def when is_binary(Def) -> binary_to_list(Def);
            Def -> DefStr
        end, "\n "])
    , NewCtx2};

% select sub-part patterns
fold(FType, Fun, Ctx, _Lvl, {hints, Hints} = ST) ->
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
    {if Size > 0 -> binary_to_list(Hints);
     true        -> ""
     end
    , NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {opt, Opt} = ST) ->
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
    {if Size > 0 -> binary_to_list(Opt) ++ " ";
     true        -> ""
     end
    , NewCtx2};
fold(FType, Fun, Ctx, Lvl, {fields, Fields} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            case F of
                F when is_binary(F) -> {Acc++[binary_to_list(F)], Fun(F, CtxAcc)};
                {'select', _} = F   ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, F),
                    {Acc++[lists:flatten(["(", SubAcc, ")"])], CtxAcc1};
                Other ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, Other),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Fields),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {string:join(FieldsStr, ", ")
    , NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {into, Into} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {IntoStr, NewCtx1} = lists:foldl(fun(I, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(I)], Fun(I, CtxAcc)}
        end,
        {[], NewCtx},
        Into),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {string:join(IntoStr, ", ") ++ " "
    , NewCtx2};
fold(FType, Fun, Ctx, Lvl, {from, Forms} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {FormStr, NewCtx1} = case Forms of
        Forms when is_list(Forms) ->
            {FrmStr, NewCtx2} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
                    case F of
                        F when is_binary(F) -> {Acc++[binary_to_list(F)], Fun(F,CtxAcc)};
                        {'select', _} = F   ->
                            {FoldFStr, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, F),
                            {lists:flatten(["(", FoldFStr, ")"])
                            , CtxAcc1};
                        Other               ->
                            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, Other),
                            {Acc++[SubAcc], CtxAcc1}
                    end
                end,
                {[], NewCtx},
                Forms),
            {string:join(FrmStr, ", ")
            , NewCtx2};
        Forms ->
            fold(FType, Fun, NewCtx, Lvl+1, Forms)
    end,
    NewCtx3 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"from " ++FormStr++ " "
    , NewCtx3};
fold(FType, Fun, Ctx, Lvl, {'group by', GroupBy} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    Size = length(GroupBy),
    {GroupByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            case fold(FType, Fun, CtxAcc, Lvl+1, F) of
                {F1, CtxAcc1} when is_binary(F1) -> {Acc++binary_to_list(F1), CtxAcc1};
                {F1, CtxAcc1} when is_list(F1) -> {Acc++[F1], CtxAcc1}
            end
        end,
        {[], NewCtx},
        GroupBy),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {if Size > 0 -> " group by " ++ string:join(GroupByStr, ", ");
        true -> ""
     end
    , NewCtx2};
fold(FType, Fun, Ctx, Lvl, {having, Having} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    Size = size(Having),
    {HavingStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Having),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {if Size > 0 -> " having " ++ HavingStr;
        true -> ""
     end
    , NewCtx2};
fold(FType, Fun, Ctx, Lvl, {'order by', OrderBy} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    Size = length(OrderBy),
    {OrderByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            case F of
                F when is_binary(F) -> {Acc++[binary_to_list(F)], Fun(F, CtxAcc)};
                {O, Op} when is_binary(O), is_binary(Op) ->
                    CtxAcc1 = Fun(O, CtxAcc),
                    CtxAcc2 = Fun(Op, CtxAcc1),
                    {Acc++[string:strip(lists:flatten([binary_to_list(O), " ", binary_to_list(Op)]))]
                    , CtxAcc2};
                {O, Op} when is_binary(Op) ->
                    {Os, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, O),
                    CtxAcc2 = Fun(Op, CtxAcc1),
                    {Acc++[string:strip(lists:flatten([Os, " ", binary_to_list(Op)]))]
                    , CtxAcc2}
            end
        end,
        {[], NewCtx},
        OrderBy),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {if Size > 0 ->
        " order by " ++ string:join(OrderByStr, ", ")
        ++ " ";
        true -> ""
     end
    , NewCtx2};

% joins
fold(FType, Fun, Ctx, Lvl, {JoinType, Tab} = ST)
  when JoinType =:= cross_join;
       JoinType =:= natural_join;
       JoinType =:= natural_inner_join ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Tab),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {case JoinType of
        cross_join          -> " cross join ";
        natural_join        -> " natural join ";
        natural_inner_join  -> " natural inner join "
     end ++ TabStr
    , NewCtx3};
fold(FType, Fun, Ctx, Lvl, {{JoinType,OptPartition,OptNatural},Tab,OptPartition1,OnOrUsing} = ST)
  when JoinType =:= full;       JoinType =:= left;
       JoinType =:= right;      JoinType =:= full_outer;
       JoinType =:= left_outer; JoinType =:= right_outer ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {OptPartitionStr, NewCtx1}  = fold(FType, Fun,   NewCtx, Lvl+1, OptPartition),
    {OptNaturalStr,   NewCtx2} = fold(FType, Fun,     NewCtx1, Lvl+1, OptNatural),
                      NewCtx3  = Fun(JoinType,              NewCtx2),
    {TabStr,          NewCtx4} = fold(FType, Fun,            NewCtx3, Lvl+1, Tab),
    {OptPartition1Str,NewCtx5} = fold(FType, Fun,  NewCtx4, Lvl+1, OptPartition1),
    {OnOrUsingStr,    NewCtx6} = fold(FType, Fun,      NewCtx5, Lvl+1, OnOrUsing),

    NewCtx7 = case FType of
        top_down -> NewCtx6;
        bottom_up -> Fun(ST, NewCtx6)
    end,
    {OptPartitionStr ++ OptNaturalStr ++
    case JoinType of
        full        -> " full join ";
        left        -> " left join ";
        right       -> " right join ";
        full_outer  -> " full outer join ";
        left_outer  -> " left outer join ";
        right_outer -> " right outer join "
    end ++ TabStr ++ OptPartition1Str ++ OnOrUsingStr
    , NewCtx7};
fold(FType, Fun, Ctx, Lvl, {JoinType, Tab, OnOrUsing} = ST)
  when JoinType =:= join; JoinType =:= join_inner ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Tab),
    {OnOrUsingStr, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl+1, OnOrUsing),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {case JoinType of
        join        -> " join ";
        join_inner -> " inner join "
     end ++ TabStr ++ OnOrUsingStr
    , NewCtx4};
fold(FType, Fun, Ctx, _Lvl, {partition_by,Fields} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(F)], Fun(F, CtxAcc)}
        end,
        {[], NewCtx},
        Fields),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {" partition by (" ++ string:join(FieldsStr, ",") ++ ")"
    , NewCtx2};
fold(FType, Fun, Ctx, Lvl, {on, Condition} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {CondStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Condition),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {" on " ++ CondStr
    , NewCtx2};
fold(FType, Fun, Ctx, _Lvl, {using, ColumnList} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ColumnListStr, NewCtx1} = lists:foldl(fun(C, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(C)], Fun(C, CtxAcc)}
        end,
        {[], NewCtx},
        ColumnList),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {" using (" ++ string:join(ColumnListStr, ",") ++ ")"
    , NewCtx2};
fold(_FType, Fun, Ctx, _Lvl, natural) ->
    {" natural", Fun(natural, Ctx)};
fold(FType, Fun, Ctx, Lvl, {Tab, [J|_] = Joins} = ST)
  when is_tuple(J) andalso (is_binary(Tab) orelse is_tuple(Tab)) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {TabStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Tab),
    {JoinsStr, NewCtx2} = lists:foldl(fun(Join, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, Join),
            {Acc++[SubAcc], CtxAcc1}
        end,
        {[], NewCtx1},
        Joins),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {TabStr++JoinsStr
    , NewCtx3};

% betwen operator
fold(FType, Fun, Ctx, Lvl, {'between', A, B, C} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {A1, NewCtx1} = if is_binary(A) -> {binary_to_list(A), NewCtx}; true -> fold(FType, Fun, NewCtx, Lvl+1, A) end,
    {B1, NewCtx2} = if is_binary(B) -> {binary_to_list(B), NewCtx1}; true -> fold(FType, Fun, NewCtx1, Lvl+1, B) end,
    {C1, NewCtx3} = if is_binary(C) -> {binary_to_list(C), NewCtx2}; true -> fold(FType, Fun, NewCtx2, Lvl+1, C) end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {lists:flatten([A1,  " between ", B1, " and ", C1])
    , NewCtx4};

% PL/SQL concatenate operator
fold(FType, Fun, Ctx, Lvl, {'||', Args} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ArgsStr, NewCtx1} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
            case A of
                A when is_binary(A) -> {Acc++[binary_to_list(A)], Fun(A, CtxAcc)};
                A ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, A),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Args),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {string:join(ArgsStr, " || ")
    , NewCtx2};

% All aliases
fold(FType, Fun, Ctx, _Lvl, {as, A, B} = ST)
  when is_binary(A), is_binary(B) ->
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
    {lists:flatten([binary_to_list(A), " ", binary_to_list(B)])
    , NewCtx3};
fold(FType, Fun, Ctx, Lvl, {as, A, B} = ST)
    when is_binary(B) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {AStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, A),
    NewCtx2 = Fun(B, NewCtx1),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {lists:flatten([AStr, " ", binary_to_list(B)])
    , NewCtx3};
fold(FType, Fun, Ctx, _Lvl, {as, A} = ST)
  when is_binary(A) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(A, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten(["as ", binary_to_list(A)])
    , NewCtx2};
fold(_FType, Fun, Ctx, _Lvl, Tab)
  when is_binary(Tab) ->
    {binary_to_list(Tab)
    , Fun(Tab, Ctx)};

% Union
fold(FType, Fun, Ctx, Lvl, {union, A, B} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {AStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1,  A),
    {BStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, B),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {lists:flatten(["(", AStr, " union ", BStr, ")"])
    , NewCtx3};

% All where clauses
fold(_FType, Fun, Ctx, _Lvl, {where, {}} = ST) ->
    {""
    , Fun(ST, Ctx)};
fold(FType, Fun, Ctx, Lvl, {where, Where} = ST)
  when is_tuple(Where) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {WhereStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl, Where),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"where " ++ WhereStr
    , NewCtx2};

% Like operator
fold(FType, Fun, Ctx, Lvl, {'like',Var,Like,OptEsc} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {VarStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Var),
    {LikeStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Like),
    NewCtx3 = Fun(OptEsc, NewCtx2),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {VarStr ++ " like " ++ LikeStr ++
    if byte_size(OptEsc) > 0 -> " escape "++binary_to_list(OptEsc);
       true -> ""
    end
    , NewCtx4};

% In operator
% for right hand non list argument extra parenthesis added
fold(FType, Fun, Ctx, Lvl, {'in', L, {'list', _} = R} = ST)
  when is_binary(L) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(L, NewCtx),
    {RStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, R),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {lists:flatten([binary_to_list(L), " in ", RStr])
    , NewCtx3};
fold(FType, Fun, Ctx, Lvl, {'in', L, R} = ST)
  when is_binary(L), is_tuple(R) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(L, NewCtx),
    {RStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, R),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {lists:flatten([binary_to_list(L), " in (", RStr, ")"])
    , NewCtx3};

% Optional Returning phrase
fold(FType, Fun, Ctx, Lvl, {R, Sel, Var} = ST)
  when R =:= return; R =:= returning ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(R, NewCtx),
    {SelStr, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
            case S of
                S when is_binary(S) -> {Acc++[binary_to_list(S)], Fun(S, CtxAcc)};
                S ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, S),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx1},
        Sel),
    {VarStr, NewCtx3} = lists:foldl(fun({param, V}, {Acc, CtxAcc}) ->
            case V of
                V when is_binary(V) -> {Acc++[binary_to_list(V)], Fun(V, CtxAcc)};
                V ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, V),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx2},
        Var),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {" "++atom_to_list(R)++" "++string:join(SelStr, ",")
    ++ " INTO " ++
    string:join(VarStr, ",")
    , NewCtx4};
fold(_FType, Fun, Ctx, _Lvl, {R, {}})
  when R =:= return; R =:= returning ->
    {""
    , Fun(R, Ctx)};

%%% JSON parser hooking
fold(_FType, _Fun, Ctx, _Lvl, {Op, _, _} = ST)
  when Op =:= ':'; Op =:= '::'; Op =:= '#';
       Op =:= '{}'; Op =:= '[]' ->
    {ok, JPPath} = jpparse:string(ST),
    {binary_to_list(JPPath), Ctx};

% Boolean and arithmetic binary operators handled with precedence
% *,/ > +,- > and > or
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
  when is_atom(Op), is_tuple(L), is_tuple(R) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {Fl, NewCtx1} = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC1} = fold(FType, Fun, NewCtx, Lvl+1, L), {lists:flatten(["(",Ls,")"]), NC1};
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC1} = fold(FType, Fun, NewCtx, Lvl+1, L), {lists:flatten(["(",Ls,")"]), NC1};
        {'and', 'or'}                         -> {Ls, NC1} = fold(FType, Fun, NewCtx, Lvl+1, L), {lists:flatten(["(",Ls,")"]), NC1};
        _ -> fold(FType, Fun, NewCtx, Lvl+1, L)
    end,
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC2} = fold(FType, Fun, NewCtx2, Lvl+1, R), {lists:flatten(["(",Rs,")"]), NC2};
        {'/', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC2} = fold(FType, Fun, NewCtx2, Lvl+1, R), {lists:flatten(["(",Rs,")"]), NC2};
        {'and', 'or'}                         -> {Rs, NC2} = fold(FType, Fun, NewCtx2, Lvl+1, R), {lists:flatten(["(",Rs,")"]), NC2};
        _ -> fold(FType, Fun, NewCtx2, Lvl+1, R)
    end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {lists:flatten([Fl, " ", atom_to_list(Op), " ", Fr])
    , NewCtx4};
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
  when is_atom(Op), is_binary(L), is_tuple(R) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC} = fold(FType, Fun, NewCtx2, Lvl+1, R), {lists:flatten(["(",Rs,")"]), NC};
        {'/', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC} = fold(FType, Fun, NewCtx2, Lvl+1, R), {lists:flatten(["(",Rs,")"]), NC};
        _ -> fold(FType, Fun, NewCtx2, Lvl+1, R)
    end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", Fr])
    , NewCtx4};
fold(FType, Fun, Ctx, Lvl, {Op, L, R} = ST)
  when is_atom(Op), is_tuple(L), is_binary(R) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {Fl, NewCtx1} = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC} = fold(FType, Fun, NewCtx, Lvl+1, L), {lists:flatten(["(",Ls,")"]), NC};
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC} = fold(FType, Fun, NewCtx, Lvl+1, L), {lists:flatten(["(",Ls,")"]), NC};
        _ -> fold(FType, Fun, NewCtx, Lvl+1, L)
    end,
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {lists:flatten([Fl, " ", atom_to_list(Op), " ", binary_to_list(R)])
    , NewCtx4};
fold(FType, Fun, Ctx, _Lvl, {Op, L, R} = ST)
  when is_atom(Op), is_binary(L), is_binary(R) ->
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
    {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", binary_to_list(R)])
    , NewCtx4};

% Unary - and 'not' operators
fold(FType, Fun, Ctx, Lvl, {Op, A} = ST)
  when Op =:= '-' orelse Op =:= 'not' ->
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
            {As, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, A),
            {lists:flatten([atom_to_list(Op)," (", As, ")"])
            , NewCtx2}
    end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {Str, NewCtx4};

% funs
fold(FType, Fun, Ctx, Lvl, {'fun', N, Args} = ST)
  when is_binary(N) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(N, NewCtx),
    {ArgsStr, NewCtx2} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
            case A of
                A when is_binary(A) -> {Acc++[binary_to_list(A)], Fun(A, CtxAcc)};
                A when is_tuple(A) ->
                    case lists:member(element(1, A),
                                      [ 'select', 'insert', 'create table',
                                        'create user', 'alter user',
                                        'truncate table', 'update', 'delete',
                                        'grant', 'revoke']) of
                        true ->
                            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, A),
                            {Acc++["(" ++ string:strip(SubAcc) ++ ")"], CtxAcc1};
                        _ ->
                            {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, A),
                            {Acc++[SubAcc], CtxAcc1}
                    end;
                A ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, A),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx1},
        Args),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {binary_to_list(N) ++ "(" ++
    string:join(ArgsStr, ", ")
    ++ ")"
    , NewCtx3};

% hierarchical query
fold(_FType, Fun, Ctx, _Lvl, {'hierarchical query', {}} = ST) ->
    {""
    , Fun(ST, Ctx)};
fold(FType, Fun, Ctx, Lvl, {'hierarchical query', {Part1, Part2}} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {Part1Str, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Part1),
    {Part2Str, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Part2),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {lists:flatten([Part1Str, " ", Part2Str])
    , NewCtx3};
fold(FType, Fun, Ctx, Lvl, {'start with', StartWith} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {StartWithStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, StartWith),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten([" start with ", StartWithStr])
    , NewCtx2};
fold(FType, Fun, Ctx, Lvl, {'connect by', NoCycle, ConnectBy} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {NoCycleStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, NoCycle),
    {ConnectByStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, ConnectBy),
    NewCtx3 = case FType of
        top_down -> NewCtx2;
        bottom_up -> Fun(ST, NewCtx2)
    end,
    {lists:flatten(["connect by "
                  , if byte_size(NoCycle) > 0 -> NoCycleStr++" "; true -> "" end
                  , ConnectByStr])
    , NewCtx3};
fold(FType, Fun, Ctx, Lvl, {'prior', Field} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {FieldsStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Field),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {lists:flatten(["prior ", FieldsStr])
    , NewCtx2};

% lists
fold(FType, Fun, Ctx, Lvl, {'list', Elms} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {ElmsStr, NewCtx1} = lists:foldl(fun(E, {Acc, CtxAcc}) ->
            case E of
                E when is_binary(E) -> {Acc++[binary_to_list(E)], Fun(E, CtxAcc)};
                E ->
                    {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl+1, E),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Elms),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    {"(" ++
     string:join(ElmsStr, ", ")
     ++ ")"
    , NewCtx2};

fold(FType, Fun, Ctx, _Lvl, {'param', P} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = Fun(P, NewCtx),
    NewCtx2 = case FType of
        top_down -> NewCtx1;
        bottom_up -> Fun(ST, NewCtx1)
    end,
    case P of
        P when is_binary(P) -> {binary_to_list(P), NewCtx2};
        P -> {P, NewCtx2}
    end;

fold(FType, Fun, Ctx, Lvl, {'case', When, Then, Else} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {WhenStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, When),
    {ThenStr, NewCtx2} = fold(FType, Fun, NewCtx1, Lvl+1, Then),
    {ElseStr, NewCtx3} = case Else of
        {} -> {"", NewCtx2};
        Else ->
            {EStr, NewCtx21} = fold(FType, Fun, NewCtx2, Lvl+1, Else),
            {" else " ++ EStr, NewCtx21}
    end,
    NewCtx4 = case FType of
        top_down -> NewCtx3;
        bottom_up -> Fun(ST, NewCtx3)
    end,
    {"case when " ++WhenStr++" then "++ThenStr++ElseStr++" end"
    , NewCtx4};

% procedure calls ('declare begin procedure' or 'begin procedure')
fold(FType, Fun, Ctx, Lvl, {D, StmtList} = ST)
  when D =:= 'declare begin procedure'; D =:= 'begin procedure' ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {BodyStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, StmtList),
    {case D of
        'declare begin procedure' -> "declare begin ";
        'begin procedure' -> "begin "
     end ++ BodyStr ++ "; end",
     NewCtx1};

% procedure calls ('call ...')
fold(FType, Fun, Ctx, Lvl, {'call procedure', Function} = ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    {FunctionStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl+1, Function),
    {"call " ++FunctionStr
    , NewCtx1};

% Index options
fold(FType, Fun, Ctx, _Lvl, ST) when is_atom(ST) ->
    NewCtx = case FType of
        top_down -> Fun(ST, Ctx);
        bottom_up -> Ctx
    end,
    NewCtx1 = case FType of
        top_down -> NewCtx;
        bottom_up -> Fun(ST, NewCtx)
    end,
    {atom_to_list(ST), NewCtx1};

% Index norm or filter
fold(FType, Fun, Ctx, _Lvl, {FunType, FunBody} = ST)
  when FunType =:= norm; FunType =:= filter ->
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
    {FunHead ++ binary_to_list(FunBody) ++ " ", NewCtx1};

%
% UNSUPPORTED
%
fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    Fun(PTree, Ctx),
    io:format(user, "Parse tree not supported ~p~n", [PTree]),
    throw({"Parse tree not supported",PTree}).

