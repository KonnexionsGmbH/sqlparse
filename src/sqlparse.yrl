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
 base_table_def
 drop_table_def
 alter_user_def
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
 opt_where_clause
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
 where_clause
 opt_group_by_clause
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
 EXPIRE
 PASSWORD
 ACCOUNT
 LOCK
 UNLOCK
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

    %% schema definition language
sql -> schema                                                                                   : '$1'.
   
schema -> CREATE SCHEMA AUTHORIZATION user opt_schema_element_list                              : {'create schema authorization', '$4', '$5'}.

opt_schema_element_list -> '$empty'                                                             : [].
opt_schema_element_list -> schema_element_list                                                  : '$1'.

schema_element_list -> schema_element                                                           : ['$1'].
schema_element_list -> schema_element_list schema_element                                       : '$1' ++ ['$2'].

schema_element -> base_table_def                                                                : '$1'.
schema_element -> view_def                                                                      : '$1'.

base_table_def -> CREATE create_opts TABLE table '(' base_table_element_commalist ')'           : {'create table', '$4', '$6', '$2'}.
base_table_def -> CREATE USER NAME identified opt_user_opts_list                                : {'create user', unwrap_bin('$3'), '$4', '$5'}.
drop_table_def -> DROP TABLE opt_exists table_list opt_restrict_cascade                         : list_to_tuple(['drop table', {'tables', '$4'}] ++ '$3' ++ '$5').

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
user_opt -> PASSWORD EXPIRE                                                                     : [{'password','expire'}].
user_opt -> ACCOUNT LOCK                                                                        : [{'account', 'lock'}].
user_opt -> ACCOUNT UNLOCK                                                                      : [{'account', 'unlock'}].

quota_list -> quota                                                                             : ['$1'].
quota_list -> quota quota_list                                                                  : ['$1'] ++ '$2'.

quota -> QUOTA UNLIMITED ON NAME                                                                : {'unlimited on', unwrap_bin('$4')}.
quota -> QUOTA INTNUM ON NAME                                                                   : {'limited', unwrap_bin('$2'), unwrap_bin('$4')}.
quota -> QUOTA INTNUM NAME ON NAME                                                              : {'limited', list_to_binary(unwrap('$2')++unwrap('$3')), unwrap_bin('$5')}.

table_list -> table                                                                             : ['$1'].
table_list -> table_list ',' table                                                              : '$1' ++ ['$3'].

opt_exists -> '$empty'                                                                          : [{'exists', 'false'}].
opt_exists -> IF EXISTS                                                                         : [{'exists', 'true'}].

opt_restrict_cascade -> '$empty'                                                                : [{'opt', 'restrict'}].
opt_restrict_cascade -> RESTRICT                                                                : [{'opt', 'restrict'}].
opt_restrict_cascade -> CASCADE                                                                 : [{'opt', 'cascade'}].

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

ordering_spec -> INTNUM opt_asc_desc                                                            : {unwrap_bin('$1'), '$2'}.
ordering_spec -> column_ref opt_asc_desc                                                        : {'$1', '$2'}.
ordering_spec -> function_ref opt_asc_desc                                                      : {'$1', '$2'}.

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
manipulative_statement -> base_table_def                                                        : '$1'.
manipulative_statement -> drop_table_def                                                        : '$1'.
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

insert_statement -> INSERT INTO table opt_column_commalist values_or_query_spec returning       : {'insert', '$3', {cols, '$4'}, '$5', '$6'}.

values_or_query_spec -> VALUES '(' insert_atom_commalist ')'                                    : {'values', '$3'}.
values_or_query_spec -> query_spec                                                              : '$1'.

insert_atom_commalist -> insert_atom                                                            : ['$1'].
insert_atom_commalist -> insert_atom_commalist ',' insert_atom                                  : '$1' ++ ['$3'].

insert_atom -> scalar_exp                                                                       : '$1'.

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

assignment -> column COMPARISON scalar_exp                                                      : {'=', '$1', '$3'}.

update_statement_searched -> UPDATE table SET assignment_commalist opt_where_clause returning   : {'update', '$2', {'set', '$4'}, '$5', '$6'}.

target_commalist -> target                                                                      : ['$1'].
target_commalist -> target_commalist ',' target                                                 : '$1' ++ ['$3'].

target -> NAME                                                                                  : unwrap_bin('$1').
target -> parameter_ref                                                                         : '$1'.

opt_where_clause -> '$empty'                                                                    : {'where', {}}.
opt_where_clause -> where_clause                                                                : '$1'.

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

select_field_commalist -> scalar_exp                                                            : ['$1'].
select_field_commalist -> '*'                                                                   : [<<"*">>].
select_field_commalist -> select_field_commalist ',' select_field_commalist                     : '$1' ++ '$3'.

table_exp ->
     from_clause opt_where_clause opt_group_by_clause opt_having_clause opt_order_by_clause     : ['$1', '$2', '$3', '$4', '$5'].

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
table_ref -> '(' query_exp ')' AS NAME                                                          : {'as','$2',unwrap_bin('$5')}.
table_ref -> '(' query_exp ')' NAME                                                             : {'as','$2',unwrap_bin('$4')}.
table_ref -> table range_variable                                                               : {'$1', '$2'}.

join_ref -> table                                                                               : '$1'.
join_ref -> '(' query_exp ')'                                                                   : '$2'.
join_ref -> '(' query_exp ')' AS NAME                                                           : {'as','$2',unwrap_bin('$5')}.
join_ref -> '(' query_exp ')' NAME                                                              : {'as','$2',unwrap_bin('$4')}.

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

comparison_predicate -> scalar_exp                                                              : '$1'.
comparison_predicate -> scalar_exp COMPARISON scalar_exp                                        : {unwrap('$2'), '$1', '$3'}.
comparison_predicate -> scalar_exp COMPARISON subquery                                          : {unwrap('$2'), '$1', '$3'}.

between_predicate -> scalar_exp NOT BETWEEN scalar_exp AND scalar_exp                           : {'not', {'between', '$1', '$4', '$6'}}.
between_predicate -> scalar_exp BETWEEN scalar_exp AND scalar_exp                               : {'between', '$1', '$3', '$5'}.

like_predicate -> scalar_exp NOT LIKE atom opt_escape                                           : {'not', {'like', '$1', '$4', '$5'}}.
like_predicate -> scalar_exp LIKE atom opt_escape                                               : {'like', '$1', '$3', '$4'}.

opt_escape -> '$empty'                                                                          : <<>>.
opt_escape -> ESCAPE atom                                                                       : '$2'.

test_for_null -> column_ref IS NOT NULLX                                                        : {'not', {'is', '$1', <<"null">>}}.
test_for_null -> column_ref IS NULLX                                                            : {'is', '$1', <<"null">>}.

in_predicate -> scalar_exp NOT IN '(' subquery ')'                                              : {'not', {'in', '$1', '$5'}}.
in_predicate -> scalar_exp IN '(' subquery ')'                                                  : {'in', '$1', '$4'}.
in_predicate -> scalar_exp NOT IN '(' scalar_exp_commalist ')'                                  : {'not', {'in', '$1', {'list', '$5'}}}.
in_predicate -> scalar_exp IN '(' scalar_exp_commalist ')'                                      : {'in', '$1', {'list', '$4'}}.

all_or_any_predicate -> scalar_exp COMPARISON any_all_some subquery                             : {unwrap('$2'), '$1', {'$3', '$4'}}.
           
any_all_some -> ANY                                                                             : 'ANY'.
any_all_some -> ALL                                                                             : 'ALL'.
any_all_some -> SOME                                                                            : 'SOME'.

existence_test -> EXISTS subquery                                                               : {exists, '$2'}.

subquery -> query_spec                                                                          : '$1'.

    %% scalar expressions

scalar_exp -> scalar_sub_exp '||' scalar_exp                                                    : {'||', lists:flatten(['$1','$3'])}.
scalar_exp -> scalar_sub_exp                                                                    : '$1'.
scalar_exp -> scalar_sub_exp NAME                                                               : {as, '$1', unwrap_bin('$2')}.
scalar_exp -> scalar_sub_exp AS NAME                                                            : {as, '$1', unwrap_bin('$3')}. 

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

scalar_exp_commalist -> scalar_exp                                                              : ['$1'].
scalar_exp_commalist -> scalar_exp_commalist ',' scalar_exp                                     : '$1' ++ ['$3'].

atom -> parameter_ref                                                                           : '$1'.
atom -> literal                                                                                 : '$1'.
atom -> USER                                                                                    : <<"user">>.
atom -> PASSWORD                                                                                : <<"password">>.

parameter_ref -> parameter                                                                      : '$1'.
parameter_ref -> parameter parameter                                                            : {'$1', '$2'}.
parameter_ref -> parameter INDICATOR parameter                                                  : {'indicator', '$1', '$3'}.

function_ref -> NAME '.' NAME '.' NAME '(' fun_args ')'                                         : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')]), make_list('$7')}.
function_ref -> NAME '.' NAME '(' fun_args ')'                                                  : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3')]), make_list('$5')}.
function_ref -> NAME  '(' fun_args ')'                                                          : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> FUNS                                                                            : {'fun', unwrap_bin('$1'), []}.
function_ref -> FUNS  '(' fun_args ')'                                                          : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> AMMSC '(' '*' ')'                                                               : {'fun', unwrap_bin('$1'), [<<"*">>]}.
function_ref -> AMMSC '(' DISTINCT column_ref ')'                                               : {'fun', unwrap_bin('$1'), {'distinct', '$4'}}.
function_ref -> AMMSC '(' ALL scalar_exp ')'                                                    : {'fun', unwrap_bin('$1'), {'all', '$4'}}.
function_ref -> AMMSC '(' scalar_exp ')'                                                        : {'fun', unwrap_bin('$1'), make_list('$3')}.

fun_args -> '(' fun_args ')'                                                                    : '$2'.
fun_args -> function_ref                                                                        : '$1'.
fun_args -> column_ref                                                                          : '$1'.
fun_args -> fun_args '+' fun_args                                                               : {'+','$1','$3'}.
fun_args -> fun_args '-' fun_args                                                               : {'-','$1','$3'}.
fun_args -> fun_args '*' fun_args                                                               : {'*','$1','$3'}.
fun_args -> fun_args '/' fun_args                                                               : {'/','$1','$3'}.
fun_args -> fun_args 'div' fun_args                                                             : {'div','$1','$3'}.
fun_args -> fun_args '||' fun_args                                                              : {'||', lists:flatten(['$1','$3'])}.
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

    %% miscellaneous

table -> NAME                                                                                   : unwrap_bin('$1').
table -> NAME AS NAME                                                                           : {as, unwrap_bin('$1'), unwrap_bin('$3')}.
table -> NAME NAME                                                                              : {as, unwrap_bin('$1'), unwrap_bin('$2')}.
table -> STRING                                                                                 : unwrap_bin('$1').
table -> NAME '.' NAME                                                                          : list_to_binary(unwrap('$1') ++ "." ++ unwrap('$3')).
table -> NAME '.' NAME AS NAME                                                                  : {as, list_to_binary(unwrap('$1') ++ "." ++ unwrap('$3')), unwrap_bin('$5')}.
table -> NAME '.' NAME NAME                                                                     : {as, list_to_binary(unwrap('$1') ++ "." ++ unwrap('$3')), unwrap_bin('$4')}.

column_ref -> NAME                                                                              : unwrap_bin('$1').
column_ref -> NAME '.' NAME                                                                     : list_to_binary([unwrap('$1'),".",unwrap('$3')]).
column_ref -> NAME '.' NAME '.' NAME                                                            : list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')]).
column_ref -> NAME '(' '+' ')'                                                                  : list_to_binary([unwrap('$1'),"(+)"]).
column_ref -> NAME '.' NAME '(' '+' ')'                                                         : list_to_binary([unwrap('$1'),".",unwrap('$3'),"(+)"]).
column_ref -> NAME '.' NAME '.' NAME '(' '+' ')'                                                : list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5'),"(+)"]).
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
-export([start/2, stop/1]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([fold/1, parsetree/1, is_reserved/1]).

-ifdef(TEST).
% eunit helper function
-export([test_parse/5]).
-endif.

-define(PARSETREE, 0).

-define(Dbg(__Rule, __Production),
begin
    io:format(user, "__ "??__Rule" (~p)~n", [__Production]),
    __Production
end). 

%%-----------------------------------------------------------------------------
%%                          dummy application interface
%%-----------------------------------------------------------------------------

start(_Type, _Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State)        -> ok.
init([])            -> {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({_,_,X}) -> X;
unwrap(X) -> X.

unwrap_bin({_,_,X}) when is_list(X) -> list_to_binary(X);
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
-spec parsetree(binary()|list()) -> {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree(Sql) when is_binary(Sql) -> parsetree(binary_to_list(Sql));
parsetree(Sql) when is_list(Sql) ->
    [C|_] = lists:reverse(string:strip(Sql)),
    NSql = if C =:= $; -> Sql; true -> string:strip(Sql) ++ ";" end,
    case sql_lex:string(NSql) of
        {ok, Toks, _} ->
            case sqlparse:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error,Error} -> {parse_error, {Error, Toks}}
            end;
        {error,Error,_} -> {lex_error, Error}
    end;
parsetree(SomethingElse) -> {parse_error, {not_a_valid_sql, SomethingElse}}.

-spec is_reserved(binary() | atom() | list()) -> true | false.
is_reserved(Word) when is_binary(Word)  -> is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word)    -> is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word)    -> lists:member(erlang:list_to_atom(string:to_upper(Word)), sql_lex:reserved_keywords()).

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  COMPILER
%%-----------------------------------------------------------------------------

-spec fold(tuple()) -> {error, term()} | binary().
fold(PTree) ->
    try foldi(PTree) of
        {error,_} = Error -> Error;
        Sql -> list_to_binary(string:strip(Sql))
    catch
        _:Error -> {error, Error}
    end.

%
% SELECT
%
foldi({select, Opts}) ->
    "select "
    ++
    lists:flatten([foldi(O) || O <- Opts]);

%
% INSERT
%
foldi({insert, Tab, {cols, Cols}, {values, Values}, Return}) when is_binary(Tab) ->
    CStrs =
      [case C of
            C when is_binary(C) -> binary_to_list(C);
            C -> foldi(C)
        end
        || C <- Cols],
    ColsStr = case length(CStrs) of
        0 -> "";
        _ -> lists:flatten(["(", string:join(CStrs, ","), ")"])
    end,
    "insert into " ++ binary_to_list(Tab)
    ++ ColsStr
    ++ " values (" ++ string:join(
        [case V of
            V when is_binary(V) -> binary_to_list(V);
            V -> foldi(V)
        end
        || V <- Values], ",") ++ ")"
    ++ foldi(Return);

%
% CREATE TABLE
%
foldi({'create table', Tab, Fields, Opts}) when is_binary(Tab) ->
    "create " ++ foldi(Opts) ++ " table " ++ binary_to_list(Tab)
    ++ " (" ++ string:join(
        [case Clm of
            {C, {T, N}, O} when is_binary(C)                -> lists:flatten([binary_to_list(C), " ", atom_to_list(T), "(", N, ") ", foldi(O)]);
            {C, {T, N, N1}, O} when is_binary(C)            -> lists:flatten([binary_to_list(C), " ", atom_to_list(T), "(",N,",",N1,") ", foldi(O)]);
            {C, T, O} when is_binary(C) and is_binary(T)    -> lists:flatten([binary_to_list(C), " ", binary_to_list(T), " ", foldi(O)]);
            {C, T, O} when is_binary(C)                     -> lists:flatten([binary_to_list(C), " ", atom_to_list(T), " ", foldi(O)]);
            C -> foldi(C)
        end
        || Clm <- Fields], ", ") ++ ")";

%
% CREATE USER
%
foldi({'create user', Usr, Id, Opts}) when is_binary(Usr) ->
    "create user " ++ binary_to_list(Usr)
    ++ foldi(Id) ++ " " ++ foldi(Opts);

%
% ALTER USER
%
foldi({'alter user', Usr, {spec, Opts}}) when is_binary(Usr) ->
    lists:flatten(["alter user ", binary_to_list(Usr), " ", foldi(Opts)]);

%
% TRUNCATE TABLE
%
foldi({'truncate table', Tbl, Mvl, Storage}) when is_binary(Tbl) ->
    "truncate table " ++ binary_to_list(Tbl) ++ " " ++
    case Mvl of
        {} -> "";
        {'materialized view log', T} -> lists:flatten([atom_to_list(T), " materialized view log "])
    end
    ++
    case Storage of
        {} -> "";
        {'storage', T} -> lists:flatten([atom_to_list(T), " storage"])
    end;

%
% UPDATE TABLE
%
foldi({'update', Tbl, {set, Set}, Where, Return}) when is_binary(Tbl) ->
    "update " ++ binary_to_list(Tbl)
    ++ " set " ++ string:join([foldi(S) || S <- Set], ",")
    ++ " " ++foldi(Where) ++ foldi(Return);

%
% DROPS
%
foldi({'drop user', Usr, Opts}) when is_binary(Usr) ->
    "drop user " ++ binary_to_list(Usr)
    ++ " " ++ foldi(Opts);
foldi({'drop table', {tables, Ts}, {exists, E}, {opt, R}}) when is_atom(R) ->
    "drop table "
    ++ if E =:= true -> " if exists "; true -> "" end
    ++ string:join([binary_to_list(T) || T <- Ts], ", ")
    ++ " " ++ atom_to_list(R);

%
% DELETE
%
foldi({'delete', Table, Where, Return}) when is_binary(Table) ->
    "delete from " ++ binary_to_list(Table)
    ++ " " ++ foldi(Where) ++ foldi(Return);

%
% GRANT
%
foldi({'grant', Objs, {OnTyp, On}, {'to', Tos}, Opts}) when is_atom(OnTyp), is_atom(Opts) ->
    "grant "
    ++ string:join([atom_to_list(O)||O<-Objs], ",") ++ " "
    ++ if On =/= <<"">> -> atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
    ++ if length(Tos) > 0 -> "to " ++ string:join([binary_to_list(O)||O<-Tos], ",") ++ " "; true -> "" end
    ++ atom_to_list(Opts);

%
% REVOKE
%
foldi({'revoke', Objs, {OnTyp, On}, {'from', Tos}, Opts}) when is_atom(OnTyp), is_atom(Opts) ->
    "revoke "
    ++ string:join([atom_to_list(O)||O<-Objs], ",") ++ " "
    ++ if On =/= <<"">> -> atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
    ++ if length(Tos) > 0 -> "from " ++ string:join([binary_to_list(O)||O<-Tos], ",") ++ " "; true -> "" end
    ++ atom_to_list(Opts);

%--------------------------------------------------------------------
% component matching patterns
%

% Empty list or tuples
foldi(X) when X =:= {}; X =:= [] -> "";

% All option and optionlist and its variants
foldi({'identified globally', E}) -> " identified globally " ++ foldi(E);
foldi({'identified extern', E}) -> " identified externally " ++ foldi(E);
foldi({'identified by', Pswd}) -> " identified by " ++ binary_to_list(Pswd);
foldi([{'scope', S}|Opts]) -> lists:flatten([" ", atom_to_list(S), " ", foldi(Opts)]);
foldi([{'type', T}|Opts]) -> lists:flatten([" ", atom_to_list(T), " ", foldi(Opts)]);
foldi([{'limited', Q, T}|O]) when is_binary(Q), is_binary(T) -> lists:flatten(["quota ", binary_to_list(Q), " on ", binary_to_list(T), " ", foldi(O)]);
foldi([cascade|Opts]) -> lists:flatten([" cascade ", foldi(Opts)]);
foldi([{Tok, T}|Opts]) ->
    if is_binary(T) andalso (
        Tok =:= 'default tablespace' orelse
        Tok =:= 'temporary tablespace' orelse
        Tok =:= 'profile') -> lists:flatten([atom_to_list(Tok), " ", binary_to_list(T), " ", foldi(Opts)]);
        true ->
            case {Tok, T} of
                {'password', 'expire'}                  -> "password expire ";
                {'account', 'lock'}                     -> "account lock ";
                {'account', 'unlock'}                   -> "account unlock ";
                {'unlimited on', T} when is_binary(T)   -> lists:flatten(["quota unlimited on ", binary_to_list(T)]);
                {'quotas', Qs}                          -> foldi(Qs);
                _                                       -> foldi({Tok, T})
            end
    end;
foldi({'default', Def}) ->
    lists:flatten([" default ", 
        case Def of
            Def when is_binary(Def) -> binary_to_list(Def);
            Def -> foldi(Def)
        end, "\n "]);

% select sub-part patterns
foldi({hints, Hints}) ->
    Size = byte_size(Hints),
    if Size > 0 -> binary_to_list(Hints);
    true        -> ""
    end;
foldi({opt, Opt}) ->
    Size = byte_size(Opt),
    if Size > 0 -> binary_to_list(Opt) ++ " ";
    true        -> ""
    end;
foldi({fields, Fields}) ->
    string:join(
        [case F of
            F when is_binary(F) -> binary_to_list(F);
            {'select', _} = F   -> lists:flatten(["(", foldi(F), ")"]);
            Other               -> foldi(Other)
        end
        || F <- Fields]
    , ", ");
foldi({into, Into}) -> string:join([binary_to_list(I) || I <- Into], ", ") ++ " ";
foldi({from, Forms}) ->
    "from " ++
    case Forms of
        Forms when is_list(Forms) ->
            string:join(
                [case F of
                    F when is_binary(F) -> binary_to_list(F);
                    {'select', _} = F   -> lists:flatten(["(", foldi(F), ")"]);
                    Other               -> foldi(Other)
                end
                || F <- Forms]
            , ", ");
        Forms ->
            foldi(Forms)
    end
    ++ " ";
foldi({'group by', GroupBy}) ->
    Size = length(GroupBy),
    if Size > 0 -> " group by " ++ string:join([case foldi(F) of
                                                    F1 when is_binary(F1) -> binary_to_list(F1);
                                                    F1 when is_list(F1) ->F1
                                                end || F <- GroupBy], ", ");
    true -> ""
    end;
foldi({having, Having}) ->
    Size = size(Having),
    if Size > 0 -> " having " ++ foldi(Having);
    true -> ""
    end;
foldi({'order by', OrderBy}) ->
    Size = length(OrderBy),
    if Size > 0 ->
        " order by " ++
        string:join(
            [case F of
            F when is_binary(F) -> binary_to_list(F);
            {O, Op} when is_binary(O), is_binary(Op) -> string:strip(lists:flatten([binary_to_list(O), " ", binary_to_list(Op)]));
            {O, Op} when is_binary(Op)               -> string:strip(lists:flatten([foldi(O), " ", binary_to_list(Op)]))
            end
            || F <- OrderBy]
        , ", ")
        ++ " ";
    true -> ""
    end;

% joins
foldi({JoinType, Tab1})
when ((JoinType =:= cross_join)
     orelse (JoinType =:= natural_join)
     orelse (JoinType =:= natural_inner_join)) ->
    case JoinType of
        cross_join          -> " cross join ";
        natural_join        -> " natural join ";
        natural_inner_join  -> " natural inner join "
    end ++
    foldi(Tab1);
foldi({{JoinType,OptPartition,OptNatural},Tab1,OptPartition1,OnOrUsing})
when ((JoinType =:= full)
      orelse (JoinType =:= left)
      orelse (JoinType =:= right)
      orelse (JoinType =:= full_outer)
      orelse (JoinType =:= left_outer)
      orelse (JoinType =:= right_outer)) ->
    foldi(OptPartition) ++
    foldi(OptNatural) ++
    case JoinType of
        full        -> " full join ";
        left        -> " left join ";
        right       -> " right join ";
        full_outer  -> " full outer join ";
        left_outer  -> " left outer join ";
        right_outer -> " right outer join "
    end ++
    foldi(Tab1) ++
    foldi(OptPartition1) ++
    foldi(OnOrUsing);
foldi({JoinType, Tab1, OnOrUsing})
when ((JoinType =:= join)
      orelse (JoinType =:= join_inner)) ->
    case JoinType of
        join        -> " join ";
        join_inner -> " inner join "
    end ++
    foldi(Tab1) ++
    foldi(OnOrUsing);
foldi({partition_by,Fields}) ->
    " partition by (" ++ string:join([binary_to_list(F) || F<-Fields], ",") ++ ")";
foldi({on, Condition}) -> " on " ++ foldi(Condition);
foldi({using, ColumnList}) -> " using (" ++ string:join([binary_to_list(C) || C<-ColumnList], ",") ++ ")";
foldi(natural) -> " natural";
foldi({Tab, [J|_] = Joins})
when (is_tuple(J) andalso (is_binary(Tab) orelse is_tuple(Tab))) ->
    foldi(Tab)++[foldi(Join) || Join <- Joins];

% betwen operator
foldi({'between', A, B, C}) ->
    A1 = if is_binary(A) -> binary_to_list(A); true -> foldi(A) end,
    B1 = if is_binary(B) -> binary_to_list(B); true -> foldi(B) end,
    C1 = if is_binary(C) -> binary_to_list(C); true -> foldi(C) end,
    lists:flatten([A1,  " between ", B1, " and ", C1]);

% PL/SQL concatenate operator
foldi({'||', Args}) ->
    string:join(
    [case A of
        A when is_binary(A) -> binary_to_list(A);
        A -> foldi(A)
    end
    || A <- Args], " || ");

% All aliases
foldi({as, A, B}) when is_binary(A), is_binary(B) -> lists:flatten([binary_to_list(A), " ", binary_to_list(B)]);
foldi({as, A, B}) when is_binary(B)               -> lists:flatten([foldi(A), " ", binary_to_list(B)]);
foldi({as, A}) when is_binary(A)                  -> lists:flatten(["as ", binary_to_list(A)]);
foldi(Tab) when is_binary(Tab)                    -> binary_to_list(Tab);

% Union
foldi({union, A, B})                              -> lists:flatten(["(", foldi(A), " union ", foldi(B), ")"]);

% All where clauses
foldi({where, {}}) -> "";
foldi({where, Where}) when is_tuple(Where) -> "where " ++ foldi(Where);

% Like operator
foldi({'like',Var,Like,OptEsc}) when is_binary(Like) andalso is_binary(OptEsc) ->
    foldi(Var) ++
    " like " ++
    binary_to_list(Like) ++
    if byte_size(OptEsc) > 0 -> " escape "++binary_to_list(OptEsc);
       true -> ""
    end;

% In operator
% for right hand non list argument extra parenthesis added
foldi({'in', L, {'list', _} = R}) when is_binary(L) ->
    lists:flatten([binary_to_list(L), " in ", foldi(R)]);
foldi({'in', L, R}) when is_binary(L), is_tuple(R) ->
    lists:flatten([binary_to_list(L), " in (", foldi(R), ")"]);

% Optional Returning phrase
foldi({R, Sel, Var}) when (R =:= return) orelse (R =:= returning) ->
    " " ++ atom_to_list(R)++" "
    ++
    string:join([case S of
            S when is_binary(S) -> binary_to_list(S);
            S -> foldi(S)
        end || S <- Sel], ",")
    ++ " INTO " ++
    string:join([case V of
            V when is_binary(V) -> binary_to_list(V);
            V -> foldi(V)
        end || {param, V} <- Var], ",");
foldi({R, {}}) when (R =:= return) orelse (R =:= returning) -> "";

% Boolean and arithmetic binary operators handled with precedence
% *,/ > +,- > and > or
foldi({Op, L, R}) when is_atom(Op), is_tuple(L), is_tuple(R) ->
    Fl = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' -> lists:flatten(["(", foldi(L), ")"]);
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' -> lists:flatten(["(", foldi(L), ")"]);
        {'and', 'or'}                         -> lists:flatten(["(", foldi(L), ")"]);
        _ -> foldi(L)
    end,
    Fr = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' -> lists:flatten(["(", foldi(R), ")"]);
        {'/', Or} when Or =:= '-'; Or =:= '+' -> lists:flatten(["(", foldi(R), ")"]);
        {'and', 'or'}                         -> lists:flatten(["(", foldi(R), ")"]);
        _ -> foldi(R)
    end,
    lists:flatten([Fl, " ", atom_to_list(Op), " ", Fr]);
foldi({Op, L, R}) when is_atom(Op), is_binary(L), is_tuple(R) ->
    Fr = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' -> lists:flatten(["(", foldi(R), ")"]);
        {'/', Or} when Or =:= '-'; Or =:= '+' -> lists:flatten(["(", foldi(R), ")"]);
        _ -> foldi(R)
    end,
    lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", Fr]);
foldi({Op, L, R}) when is_atom(Op), is_tuple(L), is_binary(R) ->
    Fl = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' -> lists:flatten(["(", foldi(L), ")"]);
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' -> lists:flatten(["(", foldi(L), ")"]);
        _ -> foldi(L)
    end,
    lists:flatten([Fl, " ", atom_to_list(Op), " ", binary_to_list(R)]);
foldi({Op, L, R}) when is_atom(Op), is_binary(L), is_binary(R) ->    lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", binary_to_list(R)]);

% Unary - and 'not' operators
foldi({Op, A}) when Op =:= '-'; Op =:= 'not' ->
    case A of
        A when is_binary(A) -> lists:flatten([atom_to_list(Op), " (", binary_to_list(A), ")"]);
        A                   -> lists:flatten([atom_to_list(Op)," (", foldi(A), ")"])
    end;

% funs
foldi({'fun', N, Args}) when is_binary(N) ->
    binary_to_list(N) ++ "(" ++
    string:join(
        [case A of
            A when is_binary(A) -> binary_to_list(A);
            A -> foldi(A)
        end
        || A <- Args]
    , ", ")
    ++ ")";

% lists
foldi({'list', Elms}) ->
    "(" ++
    string:join(
        [case E of
            E when is_binary(E) -> binary_to_list(E);
            E -> foldi(E)
        end
        || E <- Elms]
    , ", ")
    ++ ")";

foldi({'param', P}) ->
    case P of
        P when is_binary(P) -> binary_to_list(P);
        P -> P
    end;

%
% UNSUPPORTED
%
foldi(PTree) ->
    io:format(user, "Parse tree not supported ~p~n", [PTree]),
    throw({"Parse tree not supported",PTree}).

%%-----------------------------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("sql_tests.hrl").
%%-----------------------------------------------------------------------------
%%                               EUnit test
%%-----------------------------------------------------------------------------

-ifdef(PARSETREE).
parse_test() ->
    io:format(user, "===============================~n", []),
    io:format(user, "|    S Q L   P A R S I N G    |~n", []),
    io:format(user, "===============================~n", []),
    sql_test:parse_groups(fun ?MODULE:test_parse/5, true).
-else.
parse_test() ->
    io:format(user, "===============================~n", []),
    io:format(user, "|    S Q L   P A R S I N G    |~n", []),
    io:format(user, "===============================~n", []),
    sql_test:parse_groups(fun ?MODULE:test_parse/5, false).
-endif.

test_parse(_, [], _, _, Private) -> Private;
test_parse(ShowParseTree, [BinSql|Sqls], N, Limit, Private) ->
    %FlatSql = re:replace(Sql, "([\n\r\t ]+)", " ", [{return, list}, global]),
    Sql = case BinSql of
        BinSql when is_list(BinSql)   -> BinSql;
        BinSql when is_binary(BinSql) -> binary_to_list(BinSql)
    end,
    io:format(user, "[~p]~n"++Sql++"~n", [N]),
    case sqlparse:parsetree(BinSql) of
        {ok, {[{ParseTree,_}|_], Tokens}} -> 
            if ShowParseTree ->
        	    io:format(user, "~p~n", [ParseTree]),
                NSql = foldi(ParseTree),
                io:format(user,  "~n> " ++ NSql ++ "~n", []),
                {ok, {[{NPTree,_}|_], NToks}} = sqlparse:parsetree(NSql),
                try
                    ParseTree = NPTree
                catch
                    _:_ ->
            	    io:format(user, "~n> ~p~n", [NPTree]),
            	    io:format(user, "~n> ~p~n", [Tokens]),
            	    io:format(user, "~n> ~p~n", [NToks])
                end,
                ?assertEqual(ParseTree, NPTree),
                io:format("~p~n", [ParseTree]),
        	    io:format(user, lists:flatten(lists:duplicate(79, "-")) ++ "~n", []);
            true -> ok
            end,
            NewPrivate = sql_test:update_counters(ParseTree, Private),
            if (Limit =:= 1) -> NewPrivate; true ->
            test_parse(ShowParseTree, Sqls, N+1, Limit-1, NewPrivate)
            end;
        {lex_error, Error} ->
            io:format(user, "Failed lexer ~p~n", [Error]),
            ?assertEqual(ok, Error);
        {parse_error, {Error, Tokens}} ->
            io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
            ?assertEqual(ok, Error)
    end.

%%-----------------------------------------------------------------------------
-endif.
