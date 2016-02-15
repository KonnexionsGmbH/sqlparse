sqlparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
========

LALR grammar based SQL Parser

Example use
-----------
Parsing
````erlang
1> {ok, {ParseTree, Tokens}} = sqlparse:parsetree_with_tokens("select * from table_1").
2> ParseTree.
[{{select,[{hints,<<>>},
           {opt,<<>>},
           {fields,[<<"*">>]},
           {into,[]},
           {from,[<<"table_1">>]},
           {where,{}},
           {'hierarchical query',{}},
           {'group by',[]},
           {having,{}},
           {'order by',[]}]},
  {extra,<<>>}}]
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
