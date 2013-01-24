-module(sql_test).

-include("sql_tests.hrl").

-export([parse_groups/2, update_counters/2]).

parse_groups(T, S) when is_function(T) -> parse_groups(T, S, ?TEST_SQLS, {0,0,0,0,0}).
parse_groups(_, _, [], {S,C,I,U,D}) ->
    io:format(user, "~n-------------~nselect : ~p~ninsert : ~p~ncreate : ~p~nupdate : ~p~ndelete : ~p~n-------------~ntotal  : ~p~n", [S,I,C,U,D,S+I+C+U+D]),
    io:format(user, "===============================~n", []);
parse_groups(TestFun, ShowParseTree, [{Title, SqlGroup, Limit}|SqlGroups], Counters) ->
    io:format(user, "+-------------------------------+~n", []),
    io:format(user, "|\t"++Title++"(~p)\t\t|~n", [Limit]),
    io:format(user, "+-------------------------------+~n", []),
    NewCounters = if Limit =:= 0 -> Counters; true -> TestFun(ShowParseTree, SqlGroup, 1, Limit, Counters) end,
    parse_groups(TestFun, ShowParseTree, SqlGroups, NewCounters).

update_counters(ParseTree, {S,C,I,U,D}) ->
    case element(1, ParseTree) of
        select          -> {S+1,C,I,U,D};
        'create table'  -> {S,C+1,I,U,D};
        'create user'   -> {S,C+1,I,U,D};
        insert          -> {S,C,I+1,U,D};
        update          -> {S,C,I,U+1,D};
        'alter user'    -> {S,C,I,U+1,D};
        'alter table'   -> {S,C,I,U+1,D};
        delete          -> {S,C,I,U,D+1};
        'drop user'     -> {S,C,I,U,D+1};
        'drop table'    -> {S,C,I,U,D+1};
        _               -> {S,C,I,U,D}
    end.
