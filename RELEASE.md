# sqlparse - LALR grammar based SQL Parser

[![Build Status](https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg?branch=master)](https://travis-ci.org/K2InformaticsGmbH/sqlparse)

# Release Notes

## Version 3.0.0

Release Date: dd.mm.2017

### Grammar changes

- **APPROXNUM**

```
New: ((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))[eE]?[+-]?[0-9]*[fFdD]?)
 
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
New: fun_arg -> unary_add_or_subtract fun_arg
                unary_add_or_subtract -> '+' 
                unary_add_or_subtract -> '-'
 
Old: fun_arg -> '+' fun_arg
     fun_arg -> '-' fun_arg
     fun_arg -> '+' literal
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

### Parse tree changes

- **between_predicate**

```
New: between_predicate -> scalar_exp not_between scalar_exp AND scalar_exp                           : {'not between', '$1', '$3', '$5'}.
 
Old: between_predicate -> scalar_exp NOT BETWEEN scalar_exp AND scalar_exp                           : {'not', {'between', '$1', '$4', '$6'}}.
```

- **case_when_exp**

```
New: case_when_exp -> '(' case_when_exp ')'                                                          : {'$2', "("}.
 
Old: case_when_exp -> '(' case_when_exp ')'                                                          : '$2'.
```

- **case_when_opt_as_exp**

```
New: case_when_opt_as_exp -> case_when_exp    NAME                                                   : {as, '$1', unwrap_bin('$2'), " "}.
     case_when_opt_as_exp -> case_when_exp AS NAME                                                   : {as, '$1', unwrap_bin('$3'), " as "}.
 
Old: case_when_opt_as_exp -> case_when_exp NAME                                                      : {as, '$1', unwrap_bin('$2')}.
     case_when_opt_as_exp -> case_when_exp AS NAME                                                   : {as, '$1', unwrap_bin('$3')}. 
```

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

- **data_type**

```
New: data_type -> NAME '(' opt_sgn_num ')'                                                           : {unwrap_bin('$1'), "(", '$3'}.
     data_type -> NAME '(' opt_sgn_num ',' opt_sgn_num ')'                                           : {unwrap_bin('$1'), "(", '$3', '$5'}.
 
Old: data_type -> NAME '(' opt_sgn_num ')'                                                           : {unwrap_bin('$1'), '$3'}.
     data_type -> NAME '(' opt_sgn_num ',' opt_sgn_num ')'                                           : {unwrap_bin('$1'), '$3', '$5'}.

```

- **drop_table_def**

```
New: drop_table_def -> DROP      TABLE opt_exists table_list opt_restrict_cascade                    : {'drop table', {'tables', '$4'}, '$3', '$5', []}.
     drop_table_def -> DROP NAME TABLE opt_exists table_list opt_restrict_cascade                    : {'drop table', {'tables', '$5'}, '$4', '$6', unwrap('$2')}.
 
Old: drop_table_def -> DROP tbl_type TABLE opt_exists table_list opt_restrict_cascade                : {'drop table', {'tables', '$5'}, '$4', '$6', '$2'}.
```

- **from_column_commalist**

```
New: from_column -> '(' join_clause ')'                                                              : {['$2'], "("}.
 
Old: from_commalist -> '(' join_clause ')'                                                           : ['$2'].
```

- **in_predicate**

```
New: in_predicate -> scalar_exp not_in '(' subquery ')'                                              : {'not in', '$1', '$4'}.
     in_predicate -> scalar_exp not_in '(' scalar_exp_commalist ')'                                  : {'not in', '$1', {list, '$4'}}.
 
Old: in_predicate -> scalar_exp NOT IN '(' subquery ')'                                              : {'not', {'in', '$1', '$5'}}.
     in_predicate -> scalar_exp NOT IN '(' scalar_exp_commalist ')'                                  : {'not', {'in', '$1', {'list', '$5'}}}.
```

- **join_ref**

```
New: join_ref -> '(' query_exp ')'                                                                   : {'$2', "("}.
     join_ref -> '(' query_exp ')' AS NAME                                                           : {as, {'$2', "("}, unwrap_bin('$5'), " as "}.
     join_ref -> '(' query_exp ')'    NAME                                                           : {as, {'$2', "("}, unwrap_bin('$4'), " "}.
 
Old: join_ref -> '(' query_exp ')'                                                                   : '$2'.
     join_ref -> '(' query_exp ')' AS NAME                                                           : {as,'$2',unwrap_bin('$5')}.
     join_ref -> '(' query_exp ')' NAME                                                              : {as,'$2',unwrap_bin('$4')}.
```

- **like_predicate**

```
New: like_predicate -> scalar_exp not_like scalar_exp opt_escape                                     : {'not like', '$1', '$3', '$4'}.

Old: like_predicate -> scalar_exp NOT LIKE scalar_exp opt_escape                                     : {'not', {'like', '$1', '$4', '$5'}}.
```

- **opt_on_obj_clause**

```
New: opt_on_obj_clause -> ON JAVA SOURCE   table                                                     : {'on java source',   '$4'}.
     opt_on_obj_clause -> ON JAVA RESOURCE table                                                     : {'on java resource', '$4'}.

Old: opt_on_obj_clause -> ON JAVA SOURCE table                                                       : {'on java source', unwrap_bin('$4')}.
     opt_on_obj_clause -> ON JAVA RESOURCE table                                                     : {'on java resource', unwrap_bin('$4')}.
```

- **query_partition_clause**

```
New: query_partition_clause -> PARTITION BY '(' scalar_exp_commalist ')'                             : {partition_by, '$4', "("}.
     query_partition_clause -> PARTITION BY     scalar_exp_commalist                                 : {partition_by, '$3', []} .
 
Old: query_partition_clause -> PARTITION BY '(' scalar_exp_commalist ')'                             : {partition_by, '$4'}.
     query_partition_clause -> PARTITION BY scalar_exp_commalist                                     : {partition_by, '$3'}.
```

- **query_term**

```
New: query_term -> '(' query_exp ')'                                                                 : {'$2', "("}.
 
Old: query_term -> '(' query_exp ')'                                                                 : '$2'.
```

- **scalar_opt_as_exp**

```
New: scalar_opt_as_exp -> scalar_exp    NAME                                                         : {as, '$1', unwrap_bin('$2'), " "}.
     scalar_opt_as_exp -> scalar_exp AS NAME                                                         : {as, '$1', unwrap_bin('$3'), " as "}.
 
Old: scalar_opt_as_exp -> scalar_exp NAME                                                            : {as, '$1', unwrap_bin('$2')}.
     scalar_opt_as_exp -> scalar_exp AS NAME                                                         : {as, '$1', unwrap_bin('$3')}. 
```

- **schema**

```
New: schema -> CREATE SCHEMA AUTHORIZATION NAME opt_schema_element_list                              : {'create schema authorization', unwrap('$4'), '$5'}.

Old: schema -> CREATE SCHEMA AUTHORIZATION user opt_schema_element_list                              : {'create schema authorization', '$4', '$5'}.
```

- **search_condition**

```
New: search_condition -> '(' search_condition ')'                                                    : {'$2', "("}.
 
Old: search_condition -> '(' search_condition ')'                                                    : '$2'.
```

- **table**

```
New: table -> NAME AS NAME                                                                           : {as, unwrap_bin('$1'), unwrap_bin('$3'), " as "}.
     table -> NAME    NAME                                                                           : {as, unwrap_bin('$1'), unwrap_bin('$2'), " "}.
     table -> NAME '.' NAME AS NAME                                                                  : {as, list_to_binary([unwrap('$1'),".",unwrap('$3')]), unwrap_bin('$5'), " as "}.
     table -> NAME '.' NAME    NAME                                                                  : {as, list_to_binary([unwrap('$1'),".",unwrap('$3')]), unwrap_bin('$4'), " "}.
     table -> parameter    NAME                                                                      : {as, '$1', unwrap_bin('$2'), " "}.
     table -> parameter AS NAME                                                                      : {as, '$1', unwrap_bin('$3'), " as "}.
 
Old: table -> NAME AS NAME                                                                           : {as, unwrap_bin('$1'), unwrap_bin('$3')}.
     table -> NAME NAME                                                                              : {as, unwrap_bin('$1'), unwrap_bin('$2')}.
     table -> NAME '.' NAME AS NAME                                                                  : {as, list_to_binary([unwrap('$1'),".",unwrap('$3')]), unwrap_bin('$5')}.
     table -> NAME '.' NAME NAME                                                                     : {as, list_to_binary([unwrap('$1'),".",unwrap('$3')]), unwrap_bin('$4')}.
     table -> parameter NAME                                                                         : {as, '$1', unwrap_bin('$2')}.
     table -> parameter AS NAME                                                                      : {as, '$1', unwrap_bin('$3')}.
```

- **table_ref**

```
New: table_ref -> '(' query_exp ')'                                                                  : {'$2', "("}.
     table_ref -> '(' query_exp ')' AS NAME                                                          : {as, {'$2', "("}, unwrap_bin('$5'), " as "}.
     table_ref -> '(' query_exp ')'    NAME                                                          : {as, {'$2', "("}, unwrap_bin('$4'), " "}.
  
Old: table_ref -> '(' query_exp ')'                                                                  : '$2'.
     table_ref -> '(' query_exp ')' AS NAME                                                          : {as,'$2',unwrap_bin('$5')}.
     table_ref -> '(' query_exp ')' NAME                                                             : {as,'$2',unwrap_bin('$4')}.
```

- **test_for_null**

```
New: test_for_null -> scalar_exp is_not_null                                                         : {'is not', '$1', <<"null">>}.
 
Old: test_for_null -> scalar_exp IS NOT NULLX                                                        : {'not', {'is', '$1', <<"null">>}}.
```

- **view_def**

```
New: view_def -> AS query_spec                                                                       : {as, '$2', [],                   "as "}.
     view_def -> AS query_spec WITH CHECK OPTION                                                     : {as, '$2', " with check option", "as "}.
 
Old: view_def -> AS query_spec opt_with_check_option                                                 : {'as', '$2', '$3'}.
        
     opt_with_check_option -> '$empty'                                                               : [].
     opt_with_check_option -> WITH CHECK OPTION                                                      : 'with check option'.
```

### New features

- **BNFC (BNF Converter)**: grammar in LBNF (Labelled BNF grammar) format to enable the [BNFC tools](http://bnfc.digitalgrammars.com "BNFC")
- **Generating test data**: module and scripts to generate test data covering the whole grammar definition for common test and eunit test
- **Railroad diagrams**: grammar definition in EBNF format to create railroad diagrams with the online application [Railroad Diagram Generator](http://bottlecaps.de/rr/ui "Railroad Diagram Generator") 
- **Wiki documentation**: extending the documentation via GitHub Wiki

### Modified features

- **Code coverage**: 100% code coverage in folder module sqlparse_fold.erl 
- **Debugging refined**: eunit debugging messages at the start and the end of every fold function 
- **Grammar cleanup**: removing dead grammar rules and reduce/reduce conflicts; reducing the number of shift/reduce conflicts 
- **JSONPath**: embedding JSONPath expressions in SQL grammar rules between two vertical bars 
- **Test driver**: adding common test support and refactoring of eunit tests
