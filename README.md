sqlparse
========

LALR grammar based SQL Parser

Example use
-----------
Parsing
````js
1> {ok, {ParseTree, Tokens}} = sql_parse:parsetree("select * from table_1").
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
````js
4> sql_parse:fold(ParseTree).
"select * from table_1 "
````
