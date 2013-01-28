-module(sql_test).

-include("sql_tests.hrl").

-export([parse_groups/2, update_counters/2]).

parse_groups(T, S) when is_function(T) -> parse_groups(T, S, ?TEST_SQLS, {0,0,0,0,0,0}).
parse_groups(_, _, [], {S,C,I,U,D,T}) ->
    io:format(user, "~n---------------~n", []),
    io:format(user, "select   : ~p~ninsert   : ~p~ncreate   : ~p~nupdate   : ~p~ndelete   : ~p~ntruncate : ~p",[S,I,C,U,D,T]),
    io:format(user, "~n---------------~n", []),
    io:format(user, "total    : ~p~n", [S+I+C+U+D+T]),
    io:format(user, "===============================~n", []);
parse_groups(TestFun, ShowParseTree, [{Title, SqlGroup, Limit}|SqlGroups], Counters) ->
    io:format(user, "+-------------------------------+~n", []),
    io:format(user, "|\t"++Title++"(~p)\t\t|~n", [Limit]),
    io:format(user, "+-------------------------------+~n", []),
    NewCounters = if Limit =:= 0 -> Counters; true -> TestFun(ShowParseTree, SqlGroup, 1, Limit, Counters) end,
    parse_groups(TestFun, ShowParseTree, SqlGroups, NewCounters).

update_counters(ParseTree, {S,C,I,U,D,T}) ->
    case element(1, ParseTree) of
        select           -> {S+1,C,I,U,D,T};
        'create table'   -> {S,C+1,I,U,D,T};
        'create user'    -> {S,C+1,I,U,D,T};
        insert           -> {S,C,I+1,U,D,T};
        update           -> {S,C,I,U+1,D,T};
        'alter user'     -> {S,C,I,U+1,D,T};
        'alter table'    -> {S,C,I,U+1,D,T};
        delete           -> {S,C,I,U,D+1,T};
        'drop user'      -> {S,C,I,U,D+1,T};
        'drop table'     -> {S,C,I,U,D+1,T};
        'truncate table' -> {S,C,I,U,D,T+1};
        _                -> {S,C,I,U,D,T}
    end.
