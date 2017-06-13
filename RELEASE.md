# sqlparse - the SQL parser written in Erlang

[![Build Status](https://travis-ci.org/walter-weinmann/sqlparse.svg?branch=master)](https://travis-ci.org/walter-weinmann/sqlparse)

# Release Notes

## Version x.x.x

Release Date: 03.05.2017 - Grammar as of 30.04.2017

### Pure grammar changes

- **APPROXNUM**

```
New: ((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))[eE]?[+-]?[0-9]*[fFdD]?)
 
Old: (([0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)*))
```

- **JSON**

```
New: (\|[:{\[#]([^\|]*)+\|)
 
Old: ([A-Za-z0-9_\.]+([:#\[\{]+|([\s\t\n\r]*[#\[\{]+))[A-Za-z0-9_\.\:\(\)\[\]\{\}\#\,\|\-\+\*\/\\%\s\t\n\r]*)
```

### Grammar and parse tree changes

- **between_predicate**

```
New: between_predicate -> scalar_exp NOT BETWEEN scalar_exp AND scalar_exp                           : {'not between', '$1', '$4', '$6'}.
 
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

- **comparison_predicate**

```
New: n/a
 
Old: comparison_predicate -> scalar_exp COMPARISON subquery                                          : {unwrap('$2'), '$1', '$3'}.
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
New: drop_table_def -> DROP      TABLE opt_exists table_list opt_restrict_cascade                    : {'drop table', {'tables', '$4'}, '$3', '$5', <<>>}.
     drop_table_def -> DROP NAME TABLE opt_exists table_list opt_restrict_cascade                    : {'drop table', {'tables', '$5'}, '$4', '$6', unwrap_bin('$2')}.
 
Old: drop_table_def -> DROP tbl_type TABLE opt_exists table_list opt_restrict_cascade                : {'drop table', {'tables', '$5'}, '$4', '$6', '$2'}.
```

- **db_user_proxy**

```
New: n/a
 
Old: db_user_proxy -> '$empty'                                                                      : {}.

```

- **from_column_commalist**

```
New: from_column -> table_ref                                                                        : ['$1'].
     from_column -> '(' join_clause ')'                                                              : {['$2'], "("}.
     from_column -> join_clause                                                                      : ['$1'].
 
     from_column_commalist ->                           from_column                                  :        ['$1'].
     from_column_commalist -> from_column_commalist ',' from_column                                  : '$1'++ ['$3'].
 
Old: from_commalist -> table_ref                                                                     : ['$1'].
     from_commalist -> '(' join_clause ')'                                                           : ['$2'].
     from_commalist -> join_clause                                                                   : ['$1'].
     from_commalist -> from_commalist ',' from_commalist                                             : '$1'++'$3'.
```

- **fun_arg**

```
New: n/a
 
Old: fun_arg -> '+' literal                                                                          : '$2'.
     fun_arg -> '-' literal                                                                          : list_to_binary(["-",'$2']).
```

- **identified**

```
New: identified -> IDENTIFIED            BY NAME                                                     : {'identified by',       unwrap_bin('$3')}.
     identified -> IDENTIFIED EXTERNALLY                                                             : {'identified extern',   []}.
     identified -> IDENTIFIED EXTERNALLY AS NAME                                                     : {'identified extern',   unwrap_bin('$4')}.
     identified -> IDENTIFIED GLOBALLY                                                               : {'identified globally', []}.
     identified -> IDENTIFIED GLOBALLY   AS NAME                                                     : {'identified globally', unwrap_bin('$4')}.
 
Old: identified -> IDENTIFIED BY NAME                                                                : {'identified by', unwrap_bin('$3')}.
     identified -> IDENTIFIED EXTERNALLY opt_as                                                      : {'identified extern', '$3'}.
     identified -> IDENTIFIED GLOBALLY opt_as                                                        : {'identified globally', '$3'}.
     
     opt_as -> '$empty'                                                                              : {}.
     opt_as -> AS NAME                                                                               : {'as', unwrap_bin('$2')}.
```

- **in_predicate**

```
New: in_predicate -> scalar_exp NOT IN '(' subquery ')'                                              : {'not in', '$1', '$5'}.
     in_predicate -> scalar_exp NOT IN '(' scalar_exp_commalist ')'                                  : {'not in', '$1', {list, '$5'}}.
     in_predicate -> scalar_exp NOT IN scalar_exp_commalist                                          : {'not in', '$1', {list, '$4'}}.
 
Old: in_predicate -> scalar_exp NOT IN '(' subquery ')'                                              : {'not', {'in', '$1', '$5'}}.
     in_predicate -> scalar_exp NOT IN '(' scalar_exp_commalist ')'                                  : {'not', {'in', '$1', {'list', '$5'}}}.
     in_predicate -> scalar_exp NOT IN scalar_exp_commalist                                          : {'not', {'in', '$1', {'list', '$4'}}}.

```

- **join_list**

```
New: join -> inner_cross_join                                                                        : '$1'.
     join -> outer_join                                                                              : '$1'.
     
     join_list ->           join                                                                     :        ['$1'].
     join_list -> join_list join                                                                     : '$1'++ ['$2'].
 
Old: join_list -> inner_cross_join                                                                   : ['$1'].
     join_list -> outer_join                                                                         : ['$1'].
     join_list -> join_list join_list                                                                : '$1'++'$2'.
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
New: like_predicate -> scalar_exp NOT LIKE scalar_exp ESCAPE atom                                    : {'not like', '$1', '$4', '$6'}.
     like_predicate -> scalar_exp NOT LIKE scalar_exp                                                : {'not like', '$1', '$4', []}.
     like_predicate -> scalar_exp     LIKE scalar_exp ESCAPE atom                                    : {like,       '$1', '$3', '$5'}.
     like_predicate -> scalar_exp     LIKE scalar_exp                                                : {like,       '$1', '$3', []}.
 
Old: like_predicate -> scalar_exp NOT LIKE scalar_exp opt_escape                                     : {'not', {'like', '$1', '$4', '$5'}}.
     like_predicate -> scalar_exp LIKE scalar_exp opt_escape                                         : {'like', '$1', '$3', '$4'}.
     
     opt_escape -> '$empty'                                                                          : <<>>.
     opt_escape -> ESCAPE atom                                                                       : '$2'.
```

- **opt_on_obj_clause**

```
New: opt_on_obj_clause -> ON JAVA SOURCE table                                                       : {'on java source', '$4'}.
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

- **scalar_sub_exp**

```
New: n/a
 
Old: scalar_sub_exp -> '+' literal                                                                   : '$2'.
     scalar_sub_exp -> '-' literal                                                                   : list_to_binary(["-",'$2']).
```

- **search_condition**

```
New: search_condition -> '(' search_condition ')'                                                    : {'$2', "("}.
 
Old: search_condition -> '(' search_condition ')'                                                    : '$2'.
```

- **schema**

```
New: schema -> CREATE SCHEMA AUTHORIZATION NAME opt_schema_element_list                              : {'create schema authorization', unwrap_bin('$4'), '$5'}.
 
Old: schema -> CREATE SCHEMA AUTHORIZATION user opt_schema_element_list                              : {'create schema authorization', '$4', '$5'}.
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
New: test_for_null -> scalar_exp IS NOT NULLX                                                        : {'is not', '$1', <<"null">>}.
 
Old: test_for_null -> scalar_exp IS NOT NULLX                                                        : {'not', {'is', '$1', <<"null">>}}.
```

- **view_def**

```
New: view_def -> CREATE VIEW table opt_column_commalist                                              : {'create view', '$3', '$4'}.
     view_def -> AS query_spec                                                                       : {as, '$2', [],                   "as "}.
     view_def -> AS query_spec WITH CHECK OPTION                                                     : {as, '$2', " with check option", "as "}.
 
Old: view_def -> CREATE VIEW table opt_column_commalist                                              : {'create view', '$3', '$4'}.
     view_def -> AS query_spec opt_with_check_option                                                 : {'as', '$2', '$3'}.
        
     opt_with_check_option -> '$empty'                                                               : [].
     opt_with_check_option -> WITH CHECK OPTION                                                      : 'with check option'.
```

### Features modified

- **ocparse_generator**: checking the result of performance common tests
- **ocparse_test**: comparison of source code removed
- **ocparse_test**: messages improved
