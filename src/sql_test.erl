-module(sql_test).

-ifdef(TEST).
-include("sql_tests.hrl").

-export([parse_groups/2, update_counters/2]).

-define(Log(__F),       io:format(user, __F++"~n", [])).
-define(Log(__F,__A),   io:format(user, __F++"~n", __A)).

print_counters(Ts, Cs) -> print_counters(Ts, Cs, 0).
print_counters([], [], S) -> S;
print_counters([{T,_,_}|R], [C|Cs], S) ->
    ?Log(T++"   : ~p", [C]),
    print_counters(R, Cs, C+S).

parse_groups(T, S) when is_function(T) -> parse_groups(T, S, ?TEST_SQLS, lists:duplicate(length(?TEST_SQLS),0)).
parse_groups(_, _, [], Counters) ->
    ?Log("~n---------------"),
    ?Log("---------------~ntotal    : ~p", [print_counters(?TEST_SQLS, Counters)]),
    ?Log("===============================");
parse_groups(TestFun, ShowParseTree, [{Title, SqlGroup, Limit}|SqlGroups], Counters) ->
    ?Log("+-------------------------------+"),
    ?Log("|\t"++Title++"(~p)\t\t|", [Limit]),
    ?Log("+-------------------------------+"),
    NewCounters = if Limit =:= 0 -> Counters; true -> TestFun(ShowParseTree, SqlGroup, 1, Limit, Counters) end,
    parse_groups(TestFun, ShowParseTree, SqlGroups, NewCounters).

update_counters(ParseTree, Counters) ->
    case element(1, ParseTree) of
        'select'         -> incr(1, Counters);
        'union'          -> incr(1, Counters);
        'union all'      -> incr(1, Counters);
        'minus'          -> incr(1, Counters);
        'intersect'      -> incr(1, Counters);
        'insert'         -> incr(2, Counters);
        'create table'   -> incr(3, Counters);
        'create user'    -> incr(3, Counters);
        'update'         -> incr(4, Counters);
        'alter user'     -> incr(4, Counters);
        'alter table'    -> incr(4, Counters);
        'delete'         -> incr(5, Counters);
        'drop user'      -> incr(5, Counters);
        'drop table'     -> incr(5, Counters);
        'truncate table' -> incr(6, Counters);
        'grant'          -> incr(7, Counters);
        'revoke'         -> incr(8, Counters);
        _Unknown         ->
            io:format(user, "Unknown counter increment for ~p~n", [_Unknown]),
            Counters
    end.

incr(Idx, L) ->
    lists:sublist(L, Idx-1) ++ [lists:nth(Idx, L) + 1] ++ lists:sublist(L, Idx+1, length(L)).
-endif.
