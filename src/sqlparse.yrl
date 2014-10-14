%% -*- erlang -*-
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email bikram.chatterjee@k2informatics.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 sql_list
 schema
 opt_schema_element_list
 schema_element_list
 schema_element
 create_role_def
 create_table_def
 create_user_def
 create_index_def
 create_index_opts
 create_index_spec
 create_index_spec_items
 create_index_opt_norm
 create_index_opt_filter
 index_name
 alter_user_def
 drop_role_def
 drop_table_def
 drop_index_def
 drop_user_def
 base_table_element_commalist
 base_table_element
 column_def
 column_def_opt_list
 column_def_opt
 table_constraint_def
 column_commalist
 view_def
 opt_with_check_option
 opt_column_commalist
 grant_def
 revoke_def
 opt_on_obj_clause
 opt_with_grant_option
 opt_with_revoke_option
 system_priviledge_list
 grantee_commalist
 grantee
 cursor_def
 opt_order_by_clause
 ordering_spec_commalist
 ordering_spec
 opt_asc_desc
 manipulative_statement
 close_statement
 commit_statement
 delete_statement_positioned
 delete_statement_searched
 fetch_statement
 insert_statement
 values_or_query_spec
 insert_atom_commalist
 insert_atom
 open_statement
 rollback_statement
 select_statement
 opt_all_distinct
 update_statement_positioned
 assignment_commalist
 assignment
 update_statement_searched
 target_commalist
 target
 query_exp
 query_term
 query_spec
 opt_into
 selection
 table_exp
 from_clause
 form_commalist
 table_ref
 join_ref
 opt_where_clause
 where_clause
 opt_hierarchical_query_clause
 hierarchical_query_clause
 opt_group_by_clause
 opt_nocycle
 column_ref_commalist
 opt_having_clause
 search_condition
 predicate
 comparison_predicate
 between_predicate
 like_predicate
 opt_escape
 test_for_null
 in_predicate
 all_or_any_predicate
 any_all_some
 existence_test
 subquery
 scalar_exp
 scalar_opt_as_exp
 scalar_sub_exp
 scalar_exp_commalist
 select_field_commalist
 atom
 parameter_ref
 function_ref
 %concat_list
 fun_args
 literal
 table
 column_ref
 data_type
 column
 cursor
 parameter
 range_variable
 user
 sql
 when_action
 opt_hint
 table_list
 opt_exists
 opt_restrict_cascade
 identified
 opt_user_opts_list
 opt_as
 user_opt
 quota_list
 quota
 proxy_clause
 user_list
 spec_list
 role_list
 user_role
 opt_sgn_num
 create_opts
 tbl_scope
 tbl_type
 truncate_table 
 table_name 
 opt_materialized
 opt_storage
 system_priviledge
 extra
 returning
 join_clause
 join_list
 inner_cross_join
 outer_join
 opt_join_on_or_using_clause
 join_on_or_using_clause
 outer_join_type
 query_partition_clause
 case_when_exp
 opt_else
 procedure_call
.

    %% symbolic tokens
    %% literal keyword tokens

%'LANGUAGE' 'PROCEDURE' 'SQLCODE'
Terminals
 NAME
 STRING
 INTNUM
 APPROXNUM
 COMPARISON
 ALL
 FUNS
 AMMSC
 ANY
 AS
 ASC
 AUTHORIZATION
 BETWEEN
 BY
 CHECK
 CLOSE
 COMMIT
 CONTINUE
 CREATE
 CURRENT
 CURSOR
 DECLARE
 BEGIN
 DEFAULT
 DELETE
 DESC
 DISTINCT
 ESCAPE
 EXISTS
 DROP
 IF
 RESTRICT
 CASCADE
 FETCH
 FOR
 FOREIGN
 FOUND
 FROM
 GOTO
 GRANT
 GROUP
 HAVING
 IN
 INDICATOR
 INSERT
 INTO
 IS
 KEY
 LIKE
 NULLX
 OF
 ON
 OPEN
 OPTION
 ORDER
 PARAMETER
 PRIMARY
 PRIVILEGES
 PUBLIC
 REFERENCES
 ROLLBACK
 SCHEMA
 SELECT
 SET
 SOME
 SQLERROR
 TABLE
 TO
 UNION
 INTERSECT
 MINUS
 UNIQUE
 UPDATE
 USER
 VALUES
 VIEW
 WHENEVER
 WHERE
 WITH
 WORK
 HINT
 IDENTIFIED
 EXTERNALLY
 GLOBALLY
 TABLESPACE
 TEMPORARY
 PROFILE
 PRIOR
 QUOTA
 UNLIMITED
 ALTER
 ENTERPRISE
 REVOKE
 THROUGH
 USERS
 ROLE
 EXCEPT
 NONE
 CONNECT
 LOCAL
 CLUSTER
 ORDERED_SET
 BAG
 TRUNCATE
 PRESERVE
 PURGE
 MATERIALIZED
 LOG
 REUSE
 STORAGE
 HIERARCHY
 DIRECTORY
 JAVA
 SOURCE
 RESOURCE
 CONSTRAINS
 FORCE
 RETURNING
 RETURN
 INNER
 OUTER
 LEFT
 RIGHT
 FULL
 CROSS
 NATURAL
 JOIN
 USING
 PARTITION
 START
 NOCYCLE
 CASE
 WHEN
 THEN
 ELSE
 END
 CALL
 JSON
 BITMAP
 KEYLIST
 HASHMAP
 INDEX
 NORM_WITH
 FILTER_WITH
 'AND'
 'NOT'
 'OR'
 '+'
 '-'
 '*'
 '/'
 ';'
 '('
 ')'
 ','
 '||'
 '|'
 '.'
 'div'
.

Rootsymbol sql_list.


    %% operators

Left        110 'OR'.
Left        120 'AND'.
Left        130 'NOT'.
Nonassoc    200 COMPARISON. %% = <> < > <= >=
Left        300 '+' '-'.
Left        400 '*' '/'.
%Unary       500 '-'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sql_list -> sql ';' extra                                                                       : [{'$1','$3'}].
sql_list -> sql_list sql ';' extra                                                              : '$1' ++ [{'$2','$4'}].

extra -> '$empty'                                                                               : {extra, <<>>}.
extra -> NAME  ';'                                                                              : {extra, unwrap_bin('$1')}.

sql -> procedure_call                                                                           : '$1'.
procedure_call -> DECLARE BEGIN function_ref ';' END                                            : {'declare begin procedure', '$3'}.
procedure_call -> BEGIN function_ref ';' END                                                    : {'begin procedure', '$2'}.
procedure_call -> CALL function_ref                                                             : {'call procedure', '$2'}.

    %% schema definition language
sql -> schema                                                                                   : '$1'.
   
schema -> CREATE SCHEMA AUTHORIZATION user opt_schema_element_list                              : {'create schema authorization', '$4', '$5'}.

opt_schema_element_list -> '$empty'                                                             : [].
opt_schema_element_list -> schema_element_list                                                  : '$1'.

schema_element_list -> schema_element                                                           : ['$1'].
schema_element_list -> schema_element_list schema_element                                       : '$1' ++ ['$2'].

schema_element -> create_role_def                                                               : '$1'.
schema_element -> create_table_def                                                              : '$1'.
schema_element -> create_index_def                                                              : '$1'.
schema_element -> create_user_def                                                               : '$1'.
schema_element -> view_def                                                                      : '$1'.

create_role_def -> CREATE ROLE NAME                                                             : {'create role', unwrap_bin('$3')}.
create_table_def -> CREATE create_opts TABLE table '(' base_table_element_commalist ')'         : {'create table', '$4', '$6', '$2'}.
create_user_def -> CREATE USER NAME identified opt_user_opts_list                               : {'create user', unwrap_bin('$3'), '$4', '$5'}.
drop_table_def -> DROP TABLE opt_exists table_list opt_restrict_cascade                         : {'drop table', {'tables', '$4'}, '$3', '$5'}.

drop_role_def -> DROP ROLE NAME                                                                 : {'drop role', unwrap_bin('$3')}.
drop_index_def -> DROP INDEX index_name FROM table                                              : {'drop index', '$3', '$5'}.

create_index_def -> CREATE create_index_opts INDEX index_name ON table create_index_spec
                    create_index_opt_norm create_index_opt_filter                               : {'create index', '$2', '$4', '$6', '$7', '$8', '$9'}.

create_index_opts -> '$empty'                                                                   : {}.
create_index_opts -> BITMAP                                                                     : bitmap.
create_index_opts -> KEYLIST                                                                    : keylist.
create_index_opts -> HASHMAP                                                                    : hashmap.
create_index_opts -> UNIQUE                                                                     : unique.

index_name -> NAME                                                                              : unwrap_bin('$1').
index_name -> NAME '.' NAME                                                                     : list_to_binary([unwrap('$1'), ".", unwrap('$3')]).

create_index_spec -> '(' create_index_spec_items ')'                                            : '$2'.

create_index_spec_items -> NAME                                                                 : [unwrap_bin('$1')].
create_index_spec_items -> NAME '|' create_index_spec_items                                     : [unwrap_bin('$1') | '$3'].
create_index_spec_items -> JSON                                                                 : [jpparse('$1')].
create_index_spec_items -> JSON '|' create_index_spec_items                                     : [jpparse('$1') | '$3'].

create_index_opt_norm -> '$empty'                                                               : {}.
create_index_opt_norm -> NORM_WITH STRING                                                       : {'norm', unwrap_bin('$2')}.

create_index_opt_filter -> '$empty'                                                             : {}.
create_index_opt_filter -> FILTER_WITH STRING                                                   : {'filter', unwrap_bin('$2')}.

create_opts -> tbl_scope tbl_type                                                               : '$1' ++ '$2'.

tbl_scope -> '$empty'                                                                           : [].
tbl_scope -> LOCAL                                                                              : [{'scope', 'local'}].
tbl_scope -> CLUSTER                                                                            : [{'scope', 'cluster'}].
tbl_scope -> SCHEMA                                                                             : [{'scope', 'schema'}].

tbl_type -> '$empty'                                                                            : [].
tbl_type -> SET                                                                                 : [{'type', 'set'}].
tbl_type -> ORDERED_SET                                                                         : [{'type', 'ordered_set'}].
tbl_type -> BAG                                                                                 : [{'type', 'bag'}].

alter_user_def -> ALTER USER user_list proxy_clause                                             : {'alter user', '$3', '$4'}.
alter_user_def -> ALTER USER NAME spec_list                                                     : {'alter user', unwrap_bin('$3'), {'spec', '$4'}}.
alter_user_def -> ALTER USER NAME NAME NAME                                                     :
                  {'alter user', unwrap_bin('$3'),
                   {'spec',
                    [case {string:to_lower(unwrap('$4')), string:to_lower(unwrap('$5'))} of
                         {"account", "lock"} -> {account, lock};
                         {"account", "unlock"} -> {account, unlock};
                         {"password", "expire"} -> {password, expire};
                         Unknown -> exit({invalid_option, Unknown})
                     end]
                   }
                  }.

drop_user_def -> DROP USER NAME                                                                 : {'drop user', unwrap_bin('$3'), []}.
drop_user_def -> DROP USER NAME CASCADE                                                         : {'drop user', unwrap_bin('$3'), ['cascade']}.

user_list -> NAME                                                                               : [unwrap_bin('$1')].
user_list -> NAME user_list                                                                     : [unwrap_bin('$1')] ++ '$2'.

proxy_clause -> GRANT CONNECT THROUGH ENTERPRISE USERS                                          : {'grant connect', 'enterprise users'}.
proxy_clause -> GRANT REVOKE THROUGH ENTERPRISE USERS                                           : {'grant revoke', 'enterprise users'}.
proxy_clause -> GRANT REVOKE THROUGH NAME                                                       : {'grant revoke', unwrap_bin('$4')}.

spec_list -> identified                                                                         : ['$1'].
spec_list -> user_opt                                                                           : '$1'.
spec_list -> user_role                                                                          : ['$1'].
spec_list -> spec_list spec_list                                                                : ['$1'] ++ ['$2'].

user_role -> DEFAULT ROLE ALL                                                                   : 'role all'.
user_role -> DEFAULT ROLE ALL EXCEPT role_list                                                  : {'role except', '$5'}.
user_role -> DEFAULT ROLE NONE                                                                  : 'role none'.
user_role -> DEFAULT ROLE role_list                                                             : {'role', '$3'}.

role_list -> NAME                                                                               : [unwrap_bin('$1')].
role_list -> NAME role_list                                                                     : [unwrap_bin('$1')] ++ '$2'.

identified -> IDENTIFIED BY NAME                                                                : {'identified by', unwrap_bin('$3')}.
identified -> IDENTIFIED EXTERNALLY opt_as                                                      : {'identified extern', '$3'}.
identified -> IDENTIFIED GLOBALLY opt_as                                                        : {'identified globally', '$3'}.

opt_as -> '$empty'                                                                              : {}.
opt_as -> AS NAME                                                                               : {'as', unwrap_bin('$2')}.

opt_user_opts_list -> '$empty'                                                                  : [].
opt_user_opts_list -> user_opt opt_user_opts_list                                               : '$1' ++ '$2'.

user_opt -> DEFAULT TABLESPACE NAME                                                             : [{'default tablespace', unwrap_bin('$3')}].
user_opt -> TEMPORARY TABLESPACE NAME                                                           : [{'temporary tablespace', unwrap_bin('$3')}].
user_opt -> quota_list                                                                          : [{'quotas', '$1'}].
user_opt -> PROFILE NAME                                                                        : [{'profile', unwrap_bin('$2')}].

quota_list -> quota                                                                             : ['$1'].
quota_list -> quota quota_list                                                                  : ['$1'] ++ '$2'.

quota -> QUOTA UNLIMITED ON NAME                                                                : {'unlimited on', unwrap_bin('$4')}.
quota -> QUOTA INTNUM ON NAME                                                                   : {'limited', unwrap_bin('$2'), unwrap_bin('$4')}.
quota -> QUOTA INTNUM NAME ON NAME                                                              : {'limited', list_to_binary([unwrap('$2'),unwrap('$3')])
                                                                                                   , unwrap_bin('$5')}.

table_list -> table                                                                             : ['$1'].
table_list -> table_list ',' table                                                              : '$1' ++ ['$3'].

opt_exists -> '$empty'                                                                          : {}.
opt_exists -> IF EXISTS                                                                         : 'exists'.

opt_restrict_cascade -> '$empty'                                                                : {}.
opt_restrict_cascade -> RESTRICT                                                                : 'restrict'.
opt_restrict_cascade -> CASCADE                                                                 : 'cascade'.

base_table_element_commalist -> base_table_element                                              : ['$1'].
base_table_element_commalist -> base_table_element_commalist ',' base_table_element             : '$1' ++ ['$3'].

base_table_element -> column_def                                                                : '$1'.
base_table_element -> table_constraint_def                                                      : '$1'.

column_def -> column data_type column_def_opt_list                                              : {'$1', '$2', '$3'}.

column_def_opt_list -> '$empty'                                                                 : [].
column_def_opt_list -> column_def_opt_list column_def_opt                                       : '$1' ++ ['$2'].

column_def_opt -> NOT NULLX                                                                     : 'not null'.
column_def_opt -> NOT NULLX UNIQUE                                                              : 'not null unique'.
column_def_opt -> NOT NULLX PRIMARY KEY                                                         : 'not null primary key'.
column_def_opt -> DEFAULT function_ref                                                          : {'default', '$2'}.
column_def_opt -> DEFAULT literal                                                               : {'default', '$2'}.
column_def_opt -> DEFAULT NAME                                                                  : {'default', unwrap_bin('$2')}.
column_def_opt -> DEFAULT NULLX                                                                 : {'default', 'null'}.
column_def_opt -> DEFAULT USER                                                                  : {'default', 'user'}.
column_def_opt -> CHECK '(' search_condition ')'                                                : {'check', '$3'}.
column_def_opt -> REFERENCES table                                                              : {'ref', '$2'}.
column_def_opt -> REFERENCES table '(' column_commalist ')'                                     : {'ref', {'$2', '$4'}}.

table_constraint_def -> UNIQUE '(' column_commalist ')'                                         : {'unique', '$3'}.
table_constraint_def -> PRIMARY KEY '(' column_commalist ')'                                    : {'primary key', '$4'}.
table_constraint_def -> FOREIGN KEY '(' column_commalist ')' REFERENCES table                   : {'foreign key', '$4', {'ref', '$7'}}.
table_constraint_def ->
            FOREIGN KEY '(' column_commalist ')' REFERENCES table '(' column_commalist ')'      : {'foreign key', '$4', {'ref', {'$7', '$9'}}}.
table_constraint_def -> CHECK '(' search_condition ')'                                          : {'check', '$3'}.

column_commalist -> column                                                                      : ['$1'].
column_commalist -> column_commalist ',' column                                                 : '$1' ++ ['$3'].

view_def -> CREATE VIEW table opt_column_commalist                                              : {'create view', '$3', '$4'}.
view_def -> AS query_spec opt_with_check_option                                                 : {'as', '$2', '$3'}.
   
opt_with_check_option -> '$empty'                                                               : [].
opt_with_check_option -> WITH CHECK OPTION                                                      : 'with check option'.

opt_column_commalist -> '$empty'                                                                : [].
opt_column_commalist -> '(' column_commalist ')'                                                : '$2'.

grant_def ->
 GRANT system_priviledge_list opt_on_obj_clause TO grantee_commalist opt_with_grant_option      : {'grant', '$2', '$3', {'to', '$5'}, '$6'}.

revoke_def ->
 REVOKE system_priviledge_list opt_on_obj_clause FROM grantee_commalist opt_with_revoke_option  : {'revoke', '$2', '$3', {'from', '$5'}, '$6'}.

opt_on_obj_clause -> '$empty'                                                                   : {'on', <<"">>}.
opt_on_obj_clause -> ON table                                                                   : {'on', '$2'}.
opt_on_obj_clause -> ON DIRECTORY NAME                                                          : {'on directory', unwrap_bin('$3')}.
opt_on_obj_clause -> ON JAVA SOURCE table                                                       : {'on java source', unwrap_bin('$4')}.
opt_on_obj_clause -> ON JAVA RESOURCE table                                                     : {'on java resource', unwrap_bin('$4')}.

system_priviledge_list -> '$empty'                                                              : [].
system_priviledge_list -> system_priviledge                                                     : ['$1'].
system_priviledge_list -> system_priviledge ',' system_priviledge_list                          : ['$1'|'$3'].
system_priviledge_list -> ALL                                                                   : ['all'].
system_priviledge_list -> ALL PRIVILEGES                                                        : ['all privileges'].

system_priviledge -> SELECT                                                                     : 'select'.
system_priviledge -> UPDATE                                                                     : 'update'.
system_priviledge -> DELETE                                                                     : 'delete'.
system_priviledge -> INSERT                                                                     : 'insert'.
system_priviledge -> DROP                                                                       : 'drop'.
system_priviledge -> NAME                                                                       : strl2atom(['$1']).
system_priviledge -> NAME NAME                                                                  : strl2atom(['$1', '$2']).
system_priviledge -> NAME NAME NAME                                                             : strl2atom(['$1', '$2', '$3']).
system_priviledge -> NAME NAME NAME NAME                                                        : strl2atom(['$1', '$2', '$3', '$4']).
system_priviledge -> NAME NAME NAME NAME NAME                                                   : strl2atom(['$1', '$2', '$3', '$4', '$5']).

opt_with_grant_option -> '$empty'                                                               : ''.
opt_with_grant_option -> WITH GRANT OPTION                                                      : 'with grant option'.
opt_with_grant_option -> WITH NAME OPTION                                                       : strl2atom(["with", '$2', "option"]).
opt_with_grant_option -> WITH HIERARCHY OPTION                                                  : 'with hierarchy option'.

opt_with_revoke_option -> '$empty'                                                              : ''.
opt_with_revoke_option -> CASCADE CONSTRAINS                                                    : 'cascade constrains'.
opt_with_revoke_option -> FORCE                                                                 : 'force'.

grantee_commalist -> grantee                                                                    : ['$1'].
grantee_commalist -> grantee_commalist ',' grantee                                              : '$1' ++ ['$3'].

grantee -> PUBLIC                                                                               : 'public'.
grantee -> NAME                                                                                 : unwrap_bin('$1').
grantee -> NAME IDENTIFIED BY NAME                                                              : {'identified by', unwrap_bin('$1'), unwrap_bin('$4')}.

    %% cursor definition

sql -> cursor_def                                                                               : '$1'.


cursor_def -> DECLARE cursor CURSOR FOR query_exp opt_order_by_clause                           : {'declare', '$2', {'cur_for', '$5'}, '$6'}.

opt_order_by_clause -> '$empty'                                                                 : {'order by', []}.
opt_order_by_clause -> ORDER BY ordering_spec_commalist                                         : {'order by', '$3'}.

ordering_spec_commalist -> ordering_spec                                                        : ['$1'].
ordering_spec_commalist -> ordering_spec_commalist ',' ordering_spec                            : '$1' ++ ['$3'].

ordering_spec -> scalar_sub_exp opt_asc_desc                                                    : {'$1', '$2'}.

opt_asc_desc -> '$empty'                                                                        : <<>>.
opt_asc_desc -> ASC                                                                             : <<"asc">>.
opt_asc_desc -> DESC                                                                            : <<"desc">>.

    %% manipulative statements

sql -> manipulative_statement                                                                   : '$1'.

manipulative_statement -> close_statement                                                       : '$1'.
manipulative_statement -> commit_statement                                                      : '$1'.
manipulative_statement -> delete_statement_positioned                                           : '$1'.
manipulative_statement -> delete_statement_searched                                             : '$1'.
manipulative_statement -> fetch_statement                                                       : '$1'.
manipulative_statement -> insert_statement                                                      : '$1'.
manipulative_statement -> open_statement                                                        : '$1'.
manipulative_statement -> rollback_statement                                                    : '$1'.
manipulative_statement -> select_statement                                                      : '$1'.
manipulative_statement -> update_statement_positioned                                           : '$1'.
manipulative_statement -> update_statement_searched                                             : '$1'.
manipulative_statement -> create_table_def                                                      : '$1'.
manipulative_statement -> create_role_def                                                       : '$1'.
manipulative_statement -> create_index_def                                                      : '$1'.
manipulative_statement -> create_user_def                                                       : '$1'.
manipulative_statement -> drop_role_def                                                         : '$1'.
manipulative_statement -> drop_table_def                                                        : '$1'.
manipulative_statement -> drop_index_def                                                        : '$1'.
manipulative_statement -> alter_user_def                                                        : '$1'.
manipulative_statement -> drop_user_def                                                         : '$1'.
manipulative_statement -> view_def                                                              : '$1'.
manipulative_statement -> truncate_table                                                        : '$1'.
manipulative_statement -> grant_def                                                             : '$1'.
manipulative_statement -> revoke_def                                                            : '$1'.

truncate_table -> TRUNCATE TABLE table_name opt_materialized opt_storage                        : {'truncate table', '$3', '$4', '$5'}.
table_name -> NAME                                                                              : unwrap_bin('$1').
table_name -> NAME '.' NAME                                                                     : list_to_binary([unwrap('$1'),".",unwrap('$3')]).
table_name -> NAME '.' NAME '.' NAME                                                            : list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')]).

opt_materialized -> '$empty'                                                                    : {}.
opt_materialized -> PRESERVE MATERIALIZED VIEW LOG                                              : {'materialized view log', 'preserve'}.
opt_materialized -> PURGE MATERIALIZED VIEW LOG                                                 : {'materialized view log', 'purge'}.

opt_storage ->  '$empty'                                                                        : {}.
opt_storage ->  DROP STORAGE                                                                    : {'storage', 'drop'}.
opt_storage ->  REUSE STORAGE                                                                   : {'storage', 'reuse'}.

close_statement -> CLOSE cursor                                                                 : {'close', '$2'}.

commit_statement -> COMMIT WORK                                                                 : 'commit work'.

delete_statement_positioned -> DELETE FROM table WHERE CURRENT OF cursor returning              : {'delete', '$3',{'where_current_of', '$7'}, '$8'}.

delete_statement_searched -> DELETE FROM table opt_where_clause returning                       : {'delete', '$3', '$4', '$5'}.

fetch_statement -> FETCH cursor INTO target_commalist                                           : {'fetch', '$2', {'into', '$4'}}.

insert_statement -> INSERT INTO table                                                           : {'insert', '$3', {}, {}, {}}.
insert_statement -> INSERT INTO table opt_column_commalist values_or_query_spec returning       : {'insert', '$3', {cols, '$4'}, '$5', '$6'}.

values_or_query_spec -> VALUES '(' insert_atom_commalist ')'                                    : {'values', '$3'}.
values_or_query_spec -> query_spec                                                              : '$1'.

insert_atom_commalist -> insert_atom                                                            : ['$1'].
insert_atom_commalist -> insert_atom_commalist ',' insert_atom                                  : '$1' ++ ['$3'].

insert_atom -> scalar_opt_as_exp                                                                : '$1'.

open_statement -> OPEN cursor                                                                   : {'open', '$2'}.

rollback_statement -> ROLLBACK WORK                                                             : 'rollback work'.

select_statement -> SELECT opt_hint opt_all_distinct selection INTO target_commalist table_exp
                 : case '$2' of
                     {hint, ""} -> list_to_tuple([select, ['$3', '$4', '$6'] ++ '$7']);
                     _          -> list_to_tuple([select, ['$2', '$3', '$4', '$6'] ++ '$7'])
                   end.
select_statement -> query_exp                                                                   : '$1'.

opt_hint -> '$empty'                                                                            : {hints, <<>>}.
opt_hint -> HINT                                                                                : {hints, unwrap_bin('$1')}.

opt_all_distinct -> '$empty'                                                                    : {opt, <<>>}.
opt_all_distinct -> ALL                                                                         : {opt, <<"all">>}.
opt_all_distinct -> DISTINCT                                                                    : {opt, <<"distinct">>}.

update_statement_positioned -> UPDATE table SET assignment_commalist
                                                           WHERE CURRENT OF cursor returning    : {'update', '$2', {'set', '$4'}, {'where_cur_of', '$8'}, '$9'}.

assignment_commalist -> assignment                                                              : ['$1'].
assignment_commalist -> assignment_commalist ',' assignment                                     : '$1' ++ ['$3'].

assignment -> column COMPARISON scalar_opt_as_exp                                               : {'=', '$1', '$3'}.

update_statement_searched -> UPDATE table SET assignment_commalist opt_where_clause returning   : {'update', '$2', {'set', '$4'}, '$5', '$6'}.

target_commalist -> target                                                                      : ['$1'].
target_commalist -> target_commalist ',' target                                                 : '$1' ++ ['$3'].

target -> NAME                                                                                  : unwrap_bin('$1').
target -> parameter_ref                                                                         : '$1'.

opt_where_clause -> '$empty'                                                                    : {'where', {}}.
opt_where_clause -> where_clause                                                                : '$1'.

opt_hierarchical_query_clause -> '$empty'                                                       : {'hierarchical query', {}}.
opt_hierarchical_query_clause -> hierarchical_query_clause                                      : '$1'.

    %% query expressions

query_exp -> query_term                                                                         : '$1'.
query_exp -> query_exp UNION query_term                                                         : {'union', '$1', '$3'}.
query_exp -> query_exp UNION ALL query_term                                                     : {'union all', '$1', '$4'}.
query_exp -> query_exp INTERSECT query_term                                                     : {'intersect', '$1', '$3'}.
query_exp -> query_exp MINUS query_term                                                         : {'minus', '$1', '$3'}.

returning -> '$empty'                                                                           : {returning, {}}.
returning -> RETURNING selection INTO selection                                                 : {returning, '$2', '$4'}.
returning -> RETURN selection INTO selection                                                    : {return, '$2', '$4'}.

query_term -> query_spec                                                                        : '$1'.
query_term -> '(' query_exp ')'                                                                 : '$2'.

query_spec -> SELECT opt_hint opt_all_distinct selection opt_into table_exp
           : case '$2' of
               {hint, ""} -> list_to_tuple([select, ['$3', {fields, '$4'}, {into, '$5'}] ++ '$6']);
               _          -> list_to_tuple([select, ['$2', '$3', {fields, '$4'}, {into, '$5'}] ++ '$6'])
             end.

opt_into -> '$empty'                                                                            : [].
opt_into -> INTO target_commalist                                                               : {'$2', {}}.
opt_into -> INTO target_commalist IN NAME                                                       : {'$2', {'in', unwrap_bin('$4')}}.

selection -> select_field_commalist                                                             : '$1'.

select_field_commalist -> case_when_exp                                                         : ['$1'].
select_field_commalist -> scalar_opt_as_exp                                                     : ['$1'].
%select_field_commalist -> search_condition                                                      : ['$1'].
select_field_commalist -> '*'                                                                   : [<<"*">>].
select_field_commalist -> select_field_commalist ',' select_field_commalist                     : '$1' ++ '$3'.

case_when_exp -> '(' CASE WHEN search_condition THEN scalar_opt_as_exp opt_else END ')'         : {'case', '$4', '$6', '$7'}.
case_when_exp -> CASE WHEN search_condition THEN scalar_opt_as_exp opt_else END                 : {'case', '$3', '$5', '$6'}.

opt_else -> '$empty'                                                                            : {}.
opt_else -> ELSE scalar_opt_as_exp                                                              : '$2'.

table_exp ->
     from_clause opt_where_clause
                 opt_hierarchical_query_clause
                 opt_group_by_clause
                 opt_having_clause
                 opt_order_by_clause                                                            : ['$1', '$2', '$3', '$4', '$5', '$6'].

from_clause -> FROM form_commalist                                                              : {from, '$2'}.

form_commalist -> table_ref                                                                     : ['$1'].
form_commalist -> '(' join_clause ')'                                                           : ['$2'].
form_commalist -> join_clause                                                                   : ['$1'].
form_commalist -> form_commalist ',' form_commalist                                             : '$1'++'$3'.

join_clause -> table_ref join_list                                                              : {'$1', '$2'}.

join_list -> inner_cross_join                                                                   : ['$1'].
join_list -> outer_join                                                                         : ['$1'].
join_list -> join_list join_list                                                                : '$1'++'$2'.

inner_cross_join -> INNER JOIN join_ref join_on_or_using_clause                                 : {join_inner, '$3', '$4'}.
inner_cross_join -> JOIN join_ref join_on_or_using_clause                                       : {join, '$2', '$3'}.

join_on_or_using_clause -> ON search_condition                                                  : {on, '$2'}.
join_on_or_using_clause -> USING '(' select_field_commalist ')'                                 : {using, '$3'}.

opt_join_on_or_using_clause -> '$empty'                                                         : {}.
opt_join_on_or_using_clause -> join_on_or_using_clause                                          : '$1'.

% ----------------------------------------------------------------------------------------------- {{join_type, partition, opt_natural} ... }
outer_join -> NATURAL outer_join_type JOIN join_ref opt_join_on_or_using_clause                 : {{'$2', {}, natural}, '$4', {}, '$5'}.
outer_join -> NATURAL outer_join_type JOIN join_ref query_partition_clause
              opt_join_on_or_using_clause                                                       : {{'$2', {}, natural}, '$4', '$5', '$6'}.

outer_join -> query_partition_clause outer_join_type JOIN join_ref opt_join_on_or_using_clause  : {{'$2', '$1', {}}, '$4', {}, '$5'}.
outer_join -> query_partition_clause outer_join_type JOIN join_ref
              query_partition_clause opt_join_on_or_using_clause                                : {{'$2', '$1', {}}, '$4', '$5', '$6'}.

outer_join -> outer_join_type JOIN join_ref opt_join_on_or_using_clause                         : {{'$1', {}, {}}, '$3', {}, '$4'}.
outer_join -> outer_join_type JOIN join_ref query_partition_clause
              opt_join_on_or_using_clause                                                       : {{'$1', {}, {}}, '$3', '$4', '$5'}.

outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref
              opt_join_on_or_using_clause                                                       : {{'$3', '$1', natural}, '$5', {}, '$6'}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN
              join_ref query_partition_clause opt_join_on_or_using_clause                       : {{'$3', '$1', natural}, '$5', '$6', '$7'}.
% -----------------------------------------------------------------------------------------------

query_partition_clause -> PARTITION BY '(' scalar_exp_commalist ')'                             : {partition_by, '$4'}.
query_partition_clause -> PARTITION BY scalar_exp_commalist                                     : {partition_by, '$3'}.

outer_join_type -> FULL                                                                         : full.
outer_join_type -> LEFT                                                                         : left.
outer_join_type -> RIGHT                                                                        : right.
outer_join_type -> FULL OUTER                                                                   : full_outer.
outer_join_type -> LEFT OUTER                                                                   : left_outer.
outer_join_type -> RIGHT OUTER                                                                  : right_outer.

inner_cross_join -> CROSS JOIN join_ref                                                         : {cross_join, '$3'}.
inner_cross_join -> NATURAL JOIN join_ref                                                       : {natural_join, '$3'}.
inner_cross_join -> NATURAL INNER JOIN join_ref                                                 : {natural_inner_join, '$4'}.

table_ref -> table                                                                              : '$1'.
table_ref -> '(' query_exp ')'                                                                  : '$2'.
table_ref -> '(' query_exp ')' AS NAME                                                          : {as,'$2',unwrap_bin('$5')}.
table_ref -> '(' query_exp ')' NAME                                                             : {as,'$2',unwrap_bin('$4')}.
table_ref -> table range_variable                                                               : {'$1', '$2'}.

join_ref -> table                                                                               : '$1'.
join_ref -> '(' query_exp ')'                                                                   : '$2'.
join_ref -> '(' query_exp ')' AS NAME                                                           : {as,'$2',unwrap_bin('$5')}.
join_ref -> '(' query_exp ')' NAME                                                              : {as,'$2',unwrap_bin('$4')}.

hierarchical_query_clause -> START WITH search_condition CONNECT BY opt_nocycle search_condition: {'hierarchical query', {{'start with', '$3'}, {'connect by', '$6', '$7'}}}.
hierarchical_query_clause -> CONNECT BY opt_nocycle search_condition START WITH search_condition: {'hierarchical query', {{'connect by', '$3', '$4'}, {'start with', '$7'}}}.

opt_nocycle -> '$empty'                                                                         : <<>>.
opt_nocycle -> NOCYCLE                                                                          : <<"nocycle">>.

where_clause -> WHERE search_condition                                                          : {'where', '$2'}.

opt_group_by_clause  -> '$empty'                                                                : {'group by', []}.
opt_group_by_clause  -> GROUP BY column_ref_commalist                                           : {'group by', '$3'}.

column_ref_commalist -> function_ref                                                            : ['$1'].
column_ref_commalist -> column_ref                                                              : ['$1'].
column_ref_commalist -> column_ref_commalist ',' column_ref                                     : '$1' ++ ['$3'].
column_ref_commalist -> column_ref_commalist ',' function_ref                                   : '$1' ++ ['$3'].

opt_having_clause -> '$empty'                                                                   : {'having', {}}.
opt_having_clause -> HAVING search_condition                                                    : {'having', '$2'}.

    %% search conditions

search_condition -> search_condition OR search_condition                                        : {'or', '$1', '$3'}.
search_condition -> search_condition AND search_condition                                       : {'and', '$1', '$3'}.
search_condition -> NOT search_condition                                                        : {'not', '$2'}.
search_condition -> '(' search_condition ')'                                                    : '$2'.
search_condition -> predicate                                                                   : '$1'.

predicate -> comparison_predicate                                                               : '$1'.
predicate -> between_predicate                                                                  : '$1'.
predicate -> like_predicate                                                                     : '$1'.
predicate -> test_for_null                                                                      : '$1'.
predicate -> in_predicate                                                                       : '$1'.
predicate -> all_or_any_predicate                                                               : '$1'.
predicate -> existence_test                                                                     : '$1'.

comparison_predicate -> scalar_opt_as_exp                                                       : '$1'.
comparison_predicate -> scalar_exp COMPARISON scalar_exp                                        : {unwrap('$2'), '$1', '$3'}.
comparison_predicate -> PRIOR scalar_exp COMPARISON scalar_exp                                  : {unwrap('$3'), {prior, '$2'}, '$4'}.
comparison_predicate -> scalar_exp COMPARISON PRIOR scalar_exp                                  : {unwrap('$2'), '$1', {prior, '$4'}}.
comparison_predicate -> scalar_exp COMPARISON subquery                                          : {unwrap('$2'), '$1', '$3'}.

between_predicate -> scalar_exp NOT BETWEEN scalar_exp AND scalar_exp                           : {'not', {'between', '$1', '$4', '$6'}}.
between_predicate -> scalar_exp BETWEEN scalar_exp AND scalar_exp                               : {'between', '$1', '$3', '$5'}.

like_predicate -> scalar_exp NOT LIKE scalar_exp opt_escape                                     : {'not', {'like', '$1', '$4', '$5'}}.
like_predicate -> scalar_exp LIKE scalar_exp opt_escape                                         : {'like', '$1', '$3', '$4'}.

opt_escape -> '$empty'                                                                          : <<>>.
opt_escape -> ESCAPE atom                                                                       : '$2'.

test_for_null -> column_ref IS NOT NULLX                                                        : {'not', {'is', '$1', <<"null">>}}.
test_for_null -> column_ref IS NULLX                                                            : {'is', '$1', <<"null">>}.

in_predicate -> scalar_exp NOT IN '(' subquery ')'                                              : {'not', {'in', '$1', '$5'}}.
in_predicate -> scalar_exp IN '(' subquery ')'                                                  : {'in', '$1', '$4'}.
in_predicate -> scalar_exp NOT IN '(' scalar_exp_commalist ')'                                  : {'not', {'in', '$1', {'list', '$5'}}}.
in_predicate -> scalar_exp IN '(' scalar_exp_commalist ')'                                      : {'in', '$1', {'list', '$4'}}.
in_predicate -> scalar_exp NOT IN scalar_exp_commalist                                          : {'not', {'in', '$1', {'list', '$4'}}}.
in_predicate -> scalar_exp IN scalar_exp_commalist                                              : {'in', '$1', {'list', '$3'}}.

all_or_any_predicate -> scalar_exp COMPARISON any_all_some subquery                             : {unwrap('$2'), '$1', {'$3', '$4'}}.
           
any_all_some -> ANY                                                                             : 'ANY'.
any_all_some -> ALL                                                                             : 'ALL'.
any_all_some -> SOME                                                                            : 'SOME'.

existence_test -> EXISTS subquery                                                               : {exists, '$2'}.

subquery -> query_exp                                                                           : '$1'.

    %% scalar expressions

scalar_opt_as_exp -> scalar_exp                                                                 : '$1'.
scalar_opt_as_exp -> scalar_exp NAME                                                            : {as, '$1', unwrap_bin('$2')}.
scalar_opt_as_exp -> scalar_exp AS NAME                                                         : {as, '$1', unwrap_bin('$3')}. 
scalar_exp -> scalar_sub_exp '||' scalar_exp                                                    : {'||','$1','$3'}.
scalar_exp -> scalar_sub_exp                                                                    : '$1'.

scalar_sub_exp -> scalar_sub_exp '+' scalar_sub_exp                                             : {'+','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '-' scalar_sub_exp                                             : {'-','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '*' scalar_sub_exp                                             : {'*','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '/' scalar_sub_exp                                             : {'/','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp 'div' scalar_sub_exp                                           : {'div','$1','$3'}.
scalar_sub_exp -> '+' scalar_sub_exp                                                            : {'+','$2'}. %prec UMINU
scalar_sub_exp -> '-' scalar_sub_exp                                                            : {'-','$2'}. %prec UMINU
scalar_sub_exp -> '+' literal                                                                   : '$2'.
scalar_sub_exp -> '-' literal                                                                   : list_to_binary(["-",'$2']).
scalar_sub_exp -> NULLX                                                                         : <<"NULL">>.
scalar_sub_exp -> atom                                                                          : '$1'.
scalar_sub_exp -> subquery                                                                      : '$1'.
scalar_sub_exp -> column_ref                                                                    : '$1'.
scalar_sub_exp -> function_ref                                                                  : '$1'.
scalar_sub_exp -> '(' scalar_sub_exp ')'                                                        : '$2'.

scalar_exp_commalist -> scalar_opt_as_exp                                                       : ['$1'].
scalar_exp_commalist -> scalar_exp_commalist ',' scalar_opt_as_exp                              : '$1' ++ ['$3'].

atom -> parameter_ref                                                                           : '$1'.
atom -> literal                                                                                 : '$1'.
atom -> USER                                                                                    : <<"user">>.

parameter_ref -> parameter                                                                      : '$1'.
parameter_ref -> parameter parameter                                                            : {'$1', '$2'}.
parameter_ref -> parameter INDICATOR parameter                                                  : {'indicator', '$1', '$3'}.

function_ref -> NAME '.' NAME '.' NAME '(' fun_args ')'                                         : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3'),"."
                                                                                                                          ,unwrap('$5')]), make_list('$7')}.
function_ref -> NAME '.' NAME '(' fun_args ')'                                                  : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3')])
                                                                                                   ,make_list('$5')}.
function_ref -> NAME  '(' fun_args ')'                                                          : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> FUNS                                                                            : {'fun', unwrap_bin('$1'), []}.
function_ref -> FUNS  '(' fun_args ')'                                                          : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> AMMSC '(' '*' ')'                                                               : {'fun', unwrap_bin('$1'), [<<"*">>]}.
function_ref -> AMMSC '(' DISTINCT column_ref ')'                                               : {'fun', unwrap_bin('$1'), [{'distinct', '$4'}]}.
function_ref -> AMMSC '(' ALL scalar_exp ')'                                                    : {'fun', unwrap_bin('$1'), [{'all', '$4'}]}.
function_ref -> AMMSC '(' scalar_exp ')'                                                        : {'fun', unwrap_bin('$1'), make_list('$3')}.

fun_args -> '(' fun_args ')'                                                                    : '$2'.
fun_args -> function_ref                                                                        : '$1'.
fun_args -> column_ref                                                                          : '$1'.
fun_args -> fun_args '+' fun_args                                                               : {'+','$1','$3'}.
fun_args -> fun_args '-' fun_args                                                               : {'-','$1','$3'}.
fun_args -> fun_args '*' fun_args                                                               : {'*','$1','$3'}.
fun_args -> fun_args '/' fun_args                                                               : {'/','$1','$3'}.
fun_args -> fun_args 'div' fun_args                                                             : {'div','$1','$3'}.
fun_args -> fun_args '||' fun_args                                                              : {'||','$1','$3'}.
fun_args -> '+' fun_args                                                                        : {'+','$2'}. %prec UMINU
fun_args -> '-' fun_args                                                                        : {'-','$2'}. %prec UMINU
fun_args -> '+' literal                                                                         : '$2'.
fun_args -> '-' literal                                                                         : list_to_binary(["-",'$2']).
fun_args -> NULLX                                                                               : <<"NULL">>.
fun_args -> atom                                                                                : '$1'.
fun_args -> subquery                                                                            : '$1'.
fun_args -> fun_args ',' fun_args                                                               : lists:flatten(['$1'] ++ ['$3']).

literal -> STRING                                                                               : unwrap_bin('$1').
literal -> INTNUM                                                                               : unwrap_bin('$1').
literal -> APPROXNUM                                                                            : unwrap_bin('$1').
%literal -> JSON                                                                                 : jpparse('$1').

    %% miscellaneous

table -> NAME                                                                                   : unwrap_bin('$1').
table -> NAME AS NAME                                                                           : {as, unwrap_bin('$1'), unwrap_bin('$3')}.
table -> NAME NAME                                                                              : {as, unwrap_bin('$1'), unwrap_bin('$2')}.
table -> STRING                                                                                 : unwrap_bin('$1').
table -> NAME '.' NAME                                                                          : list_to_binary([unwrap('$1'),".",unwrap('$3')]).
table -> NAME '.' NAME AS NAME                                                                  : {as, list_to_binary([unwrap('$1'),".",unwrap('$3')])
                                                                                                   , unwrap_bin('$5')}.
table -> NAME '.' NAME NAME                                                                     : {as, list_to_binary([unwrap('$1'),".",unwrap('$3')])
                                                                                                   , unwrap_bin('$4')}.

column_ref -> JSON                                                                              : jpparse('$1').
column_ref -> NAME                                                                              : unwrap_bin('$1').
column_ref -> NAME '.' NAME                                                                     : list_to_binary([unwrap('$1'),".",unwrap('$3')]).
column_ref -> NAME '.' NAME '.' NAME                                                            : list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')]).
column_ref -> NAME '(' '+' ')'                                                                  : list_to_binary([unwrap('$1'),"(+)"]).
column_ref -> NAME '.' NAME '(' '+' ')'                                                         : list_to_binary([unwrap('$1'),".",unwrap('$3'),"(+)"]).
column_ref -> NAME '.' NAME '.' NAME '(' '+' ')'                                                : list_to_binary([unwrap('$1'),".",unwrap('$3'),"."
                                                                                                                  ,unwrap('$5'),"(+)"]).
column_ref -> NAME '.' '*'                                                                      : list_to_binary([unwrap('$1'),".*"]).
column_ref -> NAME '.' NAME '.' '*'                                                             : list_to_binary([unwrap('$1'),".",unwrap('$3'),".*"]).

%% data types

data_type -> STRING                                                                             : datatype('$1').
data_type -> NAME                                                                               : datatype('$1').
data_type -> NAME '(' opt_sgn_num ')'                                                           : {datatype('$1'), '$3'}.
data_type -> NAME '(' opt_sgn_num ',' opt_sgn_num ')'                                           : {datatype('$1'), '$3', '$5'}.

opt_sgn_num -> INTNUM                                                                           : unwrap('$1').
opt_sgn_num -> '-' INTNUM                                                                       : "-"++unwrap('$2').

    %% the various things you can name

column -> NAME                                                                                  : unwrap_bin('$1').
column -> STRING                                                                                : unwrap_bin('$1').

cursor -> NAME                                                                                  : {'cur', unwrap('$1')}.

parameter -> PARAMETER                                                                          : {'param', unwrap_bin('$1')}.

range_variable -> NAME                                                                          : unwrap('$1').

user -> NAME                                                                                    : {'user', unwrap('$1')}.

    %% embedded condition things

sql -> WHENEVER NOT FOUND when_action                                                           : {'when_not_found', '$1'}.
sql -> WHENEVER SQLERROR when_action                                                            : {'when_sql_err', '$1'}.

when_action -> GOTO NAME                                                                        : {'goto', unwrap('$2')}.
when_action -> CONTINUE                                                                         : 'continue'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

-behaviour(application).
-behaviour(supervisor).

% application callbacks
-export([start/0, start/2, stop/1, stop/0]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([pt_to_string/1, foldtd/3, foldbu/3
         , parsetree/1
         , parsetree_with_tokens/1
         , is_reserved/1]).

-import(sqlparse_fold, [fold/5]).

-define(Dbg(__Rule, __Production),
begin
    io:format(user, "__ "??__Rule" (~p)~n", [__Production]),
    __Production
end). 

%%-----------------------------------------------------------------------------
%%                          dummy application interface
%%-----------------------------------------------------------------------------

start() ->
    jpparse:start(),
    application:start(?MODULE).
stop() ->
    application:stop(?MODULE),
    jpparse:stop().

start(_Type, _Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State)        -> ok.

init([])            -> {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

jpparse({_,_,X}) -> jpparse(X);
jpparse(X) ->
    {ok, Pt} = jpparse:parsetree(X),
    Pt.

unwrap({_,_,X}) -> X;
unwrap(X) -> X.

unwrap_bin({_,_,X}) when is_list(X) -> list_to_binary([X]);
unwrap_bin({_,_,X}) when is_atom(X) -> atom_to_binary(X, unicode).

strl2atom([]) -> '';
strl2atom(Strs) -> list_to_atom(lists:flatten(string:join([string:to_lower(unwrap(S)) || S <- Strs], " "))).

datatype({_,_,X}) ->
    case string:to_lower(X) of
    "atom"              -> 'atom';
    "float"             -> 'float';
    "fun"               -> 'fun';
    "term"              -> 'term';
    "timestamp"         -> 'timestamp';
    "tuple"             -> 'tuple';
    "ipaddr"            -> 'ipaddr';
    "list"              -> 'list';
    "pid"               -> 'pid';
    "ref"               -> 'ref';
    "binary"            -> 'binary';
    "raw"               -> 'raw';
    "blob"              -> 'blob';
    "rowid"             -> 'rowid';
    "binstr"            -> 'binstr';
    "clob"              -> 'clob';
    "nclob"             -> 'nclob';
    "bool"              -> 'bool';
    "boolean"           -> 'boolean';
    "datetime"          -> 'datetime';
    "date"              -> 'date';
    "decimal"           -> 'decimal';
    "number"            -> 'number';
    "userid"            -> 'userid';
    "integer"           -> 'integer';
    "int"               -> 'int';
    "string"            -> 'string';
    "varchar2"          -> 'varchar2';
    "nvarchar2"         -> 'nvarchar2';
    "char"              -> 'char';
    "nchar"             -> 'nchar';
    S when is_list(S)   -> list_to_binary(S);
    Other               -> throw("unknown datatype " ++ Other)
    end.

make_list(L) when is_list(L) -> L;
make_list(L) -> [L].

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) -> {parse_error, term()} | {lex_error, term()} | {ok, [tuple()]}.
parsetree(Sql) ->
   case parsetree_with_tokens(Sql) of
       {ok, {ParseTree, _Tokens}} -> {ok, ParseTree};
       Error -> Error
   end.

-spec parsetree_with_tokens(binary()|list()) -> {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree_with_tokens(Sql) when is_binary(Sql) -> parsetree_with_tokens(binary_to_list(Sql));
parsetree_with_tokens([]) -> {parse_error, {not_a_valid_sql, []}};
parsetree_with_tokens(Sql) when is_list(Sql) ->
    [C|_] = lists:reverse(string:strip(Sql)),
    NSql = if C =:= $; -> Sql; true -> string:strip(Sql) ++ ";" end,
    case sql_lex:string(NSql) of
        {ok, Toks, _} ->
            case sqlparse:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error,{N,?MODULE,ErrorTerms}} -> {parse_error, {lists:flatten([integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error,Error} -> {parse_error, {Error, Toks}}
            end;
        {error,Error,_} -> {lex_error, Error}
    end;
parsetree_with_tokens(SomethingElse) -> {parse_error, {not_a_valid_sql, SomethingElse}}.

-spec is_reserved(binary() | atom() | list()) -> true | false.
is_reserved(Word) when is_binary(Word)  -> is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word)    -> is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word)    -> lists:member(erlang:list_to_atom(string:to_upper(Word)), sql_lex:reserved_keywords()).

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  COMPILER
%%-----------------------------------------------------------------------------

-spec pt_to_string(tuple()) -> {error, term()} | binary().
pt_to_string(PTree) when is_tuple(PTree) -> foldtd(fun(_,_) -> null_fun end, null_fun, PTree);
pt_to_string(PTrees) when is_list(PTrees) ->
    list_to_binary([string:join([
        binary_to_list(case PTree of
            {Pt, {extra, <<>>}} ->
                foldtd(fun(_,_) -> null_fun end, null_fun, Pt);
            {Pt, {extra, Extra}} ->
                Sql = foldtd(fun(_,_) -> null_fun end, null_fun, Pt),
                << Sql/binary, "; ", Extra/binary >>
        end)
    || PTree <- PTrees], "; "), ";"]).

-spec foldtd(fun(), term(), tuple()) -> {error, term()} | binary().
foldtd(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try fold(top_down, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Sql, null_fun = Ctx} -> list_to_binary(string:strip(Sql));
        {_, NewCtx} -> NewCtx
    catch
        _:Error -> {error, Error}
    end.

-spec foldbu(fun(), term(), tuple()) -> {error, term()} | binary().
foldbu(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try fold(bottom_up, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Sql, null_fun = Ctx} -> list_to_binary(string:strip(Sql));
        {_, NewCtx} -> NewCtx
    catch
        _:Error -> {error, Error}
    end.
