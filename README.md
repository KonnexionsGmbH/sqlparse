# sqlparse - LALR grammar based SQL parser

![build](https://github.com/KonnexionsGmbH/sqlparse/workflows/Erlang%20CI/badge.svg)
![Hex.pm](https://img.shields.io/hexpm/v/sqlparse.svg)
![Coveralls github](https://img.shields.io/coveralls/github/KonnexionsGmbH/sqlparse.svg)
![GitHub release](https://img.shields.io/github/release/KonnexionsGmbH/sqlparse.svg)
![GitHub Release Date](https://img.shields.io/github/release-date/KonnexionsGmbH/sqlparse.svg)
![GitHub commits since latest release](https://img.shields.io/github/commits-since/KonnexionsGmbH/sqlparse/4.6.3.svg)

**sqlparse** is a production-ready SQL parser written in pure Erlang. 
**sqlparse** is aligned to the Oracle SQL language and enriched with [imem](https://github.com/KonnexionsGmbH/imem) and [JSONPath](https://github.com/KonnexionsGmbH/jpparse) specific features.

## 1. Usage

### Example code:

```
SELECT column_a|:f()|, column_b 
  FROM table_a
 WHERE column_b = 'test'
 ORDER BY 2 DESC,
          1;
```

### Parsing the example code:

```erlang
1> {ok, {ParseTree, Tokens}} = sqlparse:parsetree_with_tokens("SELECT column_a|:f()|, column_b FROM table_a WHERE column_b = 'test' ORDER BY 2 DESC, 1;").
{ok,{[{{select,[{fields,[{':',{'fun',<<"f">>,[]},
                              <<"column_a">>},
                         <<"column_b">>]},
                {from,[<<"table_a">>]},
                {where,{'=',<<"column_b">>,<<"'test'">>}},
                {'hierarchical query',{}},
                {'group by',[]},
                {having,{}},
                {'order by',[{<<"2">>,<<"desc">>},{<<"1">>,<<>>}]}]},
       {extra,<<>>}}],
     [{'SELECT',1},
      {'NAME',8,"column_a"},
      {'JSON',1,":f()"},
      {',',1},
      {'NAME',8,"column_b"},
      {'FROM',1},
      {'NAME',7,"table_a"},
      {'WHERE',1},
      {'NAME',8,"column_b"},
      {'=',1},
      {'STRING',1,"'test'"},
      {'ORDER',1},
      {'BY',1},
      {'INTNUM',1,"2"},
      {'DESC',1},
      {',',1},
      {'INTNUM',1,"1"},
      {';',1}]}}
```

### Access the parse tree of the example code:

```erlang
2> ParseTree.
[{{select,[{fields,[{':',{'fun',<<"f">>,[]},<<"column_a">>},
                    <<"column_b">>]},
           {from,[<<"table_a">>]},
           {where,{'=',<<"column_b">>,<<"'test'">>}},
           {'hierarchical query',{}},
           {'group by',[]},
           {having,{}},
           {'order by',[{<<"2">>,<<"desc">>},{<<"1">>,<<>>}]}]},
  {extra,<<>>}}]
```

### Access the token list of the example code:

```erlang
3> Tokens.
[{'SELECT',1},
 {'NAME',8,"column_a"},
 {'JSON',1,":f()"},
 {',',1},
 {'NAME',8,"column_b"},
 {'FROM',1},
 {'NAME',7,"table_a"},
 {'WHERE',1},
 {'NAME',8,"column_b"},
 {'=',1},
 {'STRING',1,"'test'"},
 {'ORDER',1},
 {'BY',1},
 {'INTNUM',1,"2"},
 {'DESC',1},
 {',',1},
 {'INTNUM',1,"1"},
 {';',1}]
```

### Compile the code from a parse tree:

```erlang
4> sqlparse_fold:top_down(sqlparse_format_flat, ParseTree, []).
<<"select column_a|:f()|, column_b from table_a where column_b = 'test' order by 2 desc, 1">>
```

## 2. Documentation

The documentation for **sqlparse** is available here: [Wiki](https://github.com/KonnexionsGmbH/sqlparse/wiki).

## 3. Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
