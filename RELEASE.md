# sqlparse - LALR grammar based SQL Parser

[![Build Status](https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg?branch=master)](https://travis-ci.org/K2InformaticsGmbH/sqlparse)

# Release Notes

## Version 3.0.0

Release Date: dd.mm.2017

### Grammar changes

- **APPROXNUM**

```
New: ((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))([eE][+-]?[0-9]+)?[fFdD]?)
 
Old: (([0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)*))
```

- **column_ref**

```
New: column_ref -> NAME     JSON
     column_ref -> NAME '.' NAME     JSON
 
Old: column_ref -> NAME '.' JSON
     column_ref -> NAME '.' NAME '.' JSON
```

- **comparison_predicate**

```
New: n/a
 
Old: comparison_predicate -> scalar_exp COMPARISON subquery
```

- **drop_table_def**

```
New: drop_table_def -> DROP      TABLE opt_exists table_list opt_restrict_cascade
     drop_table_def -> DROP NAME TABLE opt_exists table_list opt_restrict_cascade
 
Old: drop_table_def -> DROP tbl_type TABLE opt_exists table_list opt_restrict_cascade
```

- **db_user_proxy**

```
New: n/a
 
Old: db_user_proxy -> '$empty'
```

- **fun_arg**

```
New: fun_arg -> fun_arg -> fun_arg    NAME 
 
Old: fun_arg -> '+' literal
     fun_arg -> '-' literal
```

- **in_predicate**

```
New: n/a
 
Old: in_predicate -> scalar_exp NOT IN scalar_exp_commalist
     in_predicate -> scalar_exp     IN scalar_exp_commalist
```

- **JSON**

```
New: (\|[:{\[#]([^\|]*)+\|)
 
Old: ([A-Za-z0-9_\.]+([:#\[\{]+|([\s\t\n\r]*[#\[\{]+))[A-Za-z0-9_\.\:\(\)\[\]\{\}\#\,\|\-\+\*\/\\%\s\t\n\r]*)
```

- **scalar_sub_exp**

```
New: n/a
 
Old: scalar_sub_exp -> '+' literal
     scalar_sub_exp -> '-' literal
```

- **table**

```
New: n/a
 
Old: table -> NAME AS NAME
     table -> NAME '.' NAME AS NAME
     table -> parameter AS NAME
```

- **table_ref**

```
New: n/a
 
Old: table_ref -> '(' query_exp ')' AS NAME
```

### Parse tree changes

- **column_ref**

```
New: column_ref -> JSON                                                                              : {jp, jpparse('$1')}.
     column_ref -> NAME     JSON                                                                     : {jp, list_to_binary(unwrap('$1')), jpparse('$2')}.
     column_ref -> NAME '.' NAME     JSON                                                            : {jp, list_to_binary([unwrap('$1'),".",unwrap('$3')]), jpparse('$4')}.
 
Old: column_ref -> JSON                                                                              : jpparse('$1').
     column_ref -> NAME '.' JSON                                                                     : jpparse(list_to_binary([unwrap('$1'),".",unwrap('$3')])).
     column_ref -> NAME '.' NAME '.' JSON                                                            : jpparse(list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')])).

```

- **create_index_spec_items**

```
New: create_index_spec_items -> JSON                                                                 : [{jp, jpparse('$1')}].
     create_index_spec_items -> JSON '|' create_index_spec_items                                     : [{jp, jpparse('$1')} | '$3'].
 
Old: create_index_spec_items -> JSON                                                                 : [jpparse('$1')].
     create_index_spec_items -> JSON '|' create_index_spec_items                                     : [jpparse('$1') | '$3'].
```

- **drop_table_def**

```
New: drop_table_def -> DROP      TABLE opt_exists table_list opt_restrict_cascade                    : {'drop table', {'tables', '$4'}, '$3', '$5', []}.
     drop_table_def -> DROP NAME TABLE opt_exists table_list opt_restrict_cascade                    : {'drop table', {'tables', '$5'}, '$4', '$6', unwrap('$2')}.
 
Old: drop_table_def -> DROP tbl_type TABLE opt_exists table_list opt_restrict_cascade                : {'drop table', {'tables', '$5'}, '$4', '$6', '$2'}.
```

- **opt_on_obj_clause**

```
New: opt_on_obj_clause -> ON JAVA SOURCE   table                                                     : {'on java source',   '$4'}.
     opt_on_obj_clause -> ON JAVA RESOURCE table                                                     : {'on java resource', '$4'}.

Old: opt_on_obj_clause -> ON JAVA SOURCE table                                                       : {'on java source', unwrap_bin('$4')}.
     opt_on_obj_clause -> ON JAVA RESOURCE table                                                     : {'on java resource', unwrap_bin('$4')}.
```

- **schema**

```
New: schema -> CREATE SCHEMA AUTHORIZATION NAME opt_schema_element_list                              : {'create schema authorization', unwrap('$4'), '$5'}.

Old: schema -> CREATE SCHEMA AUTHORIZATION user opt_schema_element_list                              : {'create schema authorization', '$4', '$5'}.
```

### New features

- **BNFC (BNF Converter):** grammar in LBNF (Labelled BNF grammar) format to enable the [BNFC tools](http://bnfc.digitalgrammars.com "BNFC")
- **Generating test data:** module and scripts to generate test data covering the whole grammar definition for common test and eunit test
- **Railroad diagrams:** grammar definition in EBNF format to create railroad diagrams with the online application [Railroad Diagram Generator](http://bottlecaps.de/rr/ui "Railroad Diagram Generator") 
- **Wiki documentation:** extending the documentation via GitHub Wiki

### Modified features

- **Code coverage:** 100% code coverage in folder module sqlparse_fold.erl 
- **Debugging refined:** eunit debugging messages at the start and the end of every fold function 
- **Grammar cleanup:** removing dead grammar rules and reduce/reduce conflicts; reducing the number of shift/reduce conflicts 
- **JSONPath:** embedding JSONPath expressions in SQL grammar rules between two vertical bars 
- **Test driver:** adding common test support and refactoring the eunit support
