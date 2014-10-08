sqlparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://magnum.travis-ci.com/k2informatics/sqlparse.svg?token=jAKQYF1CVGmsnuguN6iU&branch=master" alt="Travis-CI"></a>
========

LALR grammar based SQL Parser

Example use
-----------
Parsing
````erlang
1> {ok, {ParseTree, Tokens}} = sqlparse:parsetree("select * from table_1").
2> ParseTree.
[{select,[{hints,<<>>},
          {opt,<<>>},
          {fields,[<<"*">>]},
          {into,[]},
          {from,[<<"table_1">>]},
          {where,[]},
          {'group by',[]},
          {having,{}},
          {'order by',[]}]}]
3> Tokens.
[{'SELECT',1},
 {'*',1},
 {'FROM',1},
 {'NAME',7,"table_1"},
 {';',1}]
````
Compiling
````erlang
4> sqlparse:fold(ParseTree).
"select * from table_1"
````
