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

function_ref -> NAME '.' NAME '.' NAME '(' fun_args ')'                                         : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')]), make_list('$7')}.
function_ref -> NAME '.' NAME '(' fun_args ')'                                                  : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3')]), make_list('$5')}.
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
-export([fold/1, fold/3, parsetree/1, is_reserved/1]).

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
                {error,{N,?MODULE,ErrorTerms}} -> {parse_error, {lists:flatten([integer_to_list(N), ": ", ErrorTerms]), Toks}};
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
    case fold(PTree, fun(_,_) -> ok end, ok) of
        {SqlBin, ok} when is_binary(SqlBin) -> SqlBin;
        Error -> Error
    end.

-spec fold(tuple(), fun(), term()) -> {error, term()} | binary().
fold(PTree, Fun, Ctx) when is_function(Fun, 2) ->
    try foldi(PTree, Fun, Ctx) of
        {error,_} = Error -> Error;
        {Sql, NewCtx} -> {list_to_binary(string:strip(Sql)), NewCtx}
    catch
        _:Error -> {error, Error}
    end.

%
% SELECT
%
foldi({select, Opts} = ST, Fun, Ctx)
 when is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    {NewOs, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} = foldi(O, Fun, CtxAcc),
            {Acc++[SubAcc], CtxAcc1}
        end,
        {[], NewCtx},
        Opts),
    {"select "++lists:flatten(NewOs), NewCtx1};

%
% INSERT
%
foldi({insert, Tab, {cols, Cols}, {values, Values}, Return} = ST, Fun, Ctx)
 when is_binary(Tab), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    {CStrs, NewCtx1} = lists:foldl(fun(C, {Acc, CtxAcc}) ->
            case C of
                C when is_binary(C) ->
                    {Acc++[binary_to_list(C)], Fun(C, CtxAcc)};
                C ->
                    {CT, CtxAcc1} = foldi(C, Fun, CtxAcc),
                    {Acc++[CT], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Cols),
    {Vals, NewCtx2} = lists:foldl(fun(V, {Acc1, CtxAcc1}) ->
            case V of
                V when is_binary(V) ->
                    {Acc1++[binary_to_list(V)], Fun(V, CtxAcc1)};
                V ->
                    {VT, CtxAcc2} = foldi(V, Fun, CtxAcc1),
                    {Acc1++[VT], CtxAcc2}
            end
        end,
        {[], NewCtx1},
        Values),
    {Ret, NewCtx3} = foldi(Return, Fun, NewCtx2),
    {"insert into " ++ binary_to_list(Tab) ++
     case length(CStrs) of
         0 -> "";
         _ -> lists:flatten(["(", string:join(CStrs, ","), ")"])
     end ++ " values (" ++ string:join(Vals, ",") ++ ")" ++ Ret
    , NewCtx3};

%
% CREATE TABLE
%
foldi({'create table', Tab, Fields, Opts} = ST, Fun, Ctx)
 when is_binary(Tab), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    {OptsStr, NewCtx1} = foldi(Opts, Fun, NewCtx),
    NewCtx2 = Fun(Tab, NewCtx1),
    {Clms, NewCtx3} = lists:foldl(fun(Clm, {Acc, CtxAcc}) ->
            case Clm of
                {C, {T, N}, O} when is_binary(C) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    CtxAcc3 = Fun(N, CtxAcc2),
                    {SubAcc, CtxAcc4} = foldi(O, Fun, CtxAcc3),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ", atom_to_list(T), "(", N, ") ", SubAcc])]
                    , CtxAcc4};
                {C, {T, N, N1}, O} when is_binary(C) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    CtxAcc3 = Fun(N, CtxAcc2),
                    CtxAcc4 = Fun(N1, CtxAcc3),
                    {SubAcc, CtxAcc5} = foldi(O, Fun, CtxAcc4),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ", atom_to_list(T), "(",N,",",N1,") ", SubAcc])]
                    , CtxAcc5};
                {C, T, O} when is_binary(C) and is_binary(T) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    {SubAcc, CtxAcc3} = foldi(O, Fun, CtxAcc2),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ", binary_to_list(T), " ", SubAcc])]
                    , CtxAcc3};
                {C, T, O} when is_binary(C) ->
                    CtxAcc1 = Fun(C, CtxAcc),
                    CtxAcc2 = Fun(T, CtxAcc1),
                    {SubAcc, CtxAcc3} = foldi(O, Fun, CtxAcc2),
                    {Acc ++ [lists:flatten([binary_to_list(C), " ", atom_to_list(T), " ", SubAcc])]
                    , CtxAcc3};
                Clm ->
                    {SubAcc, CtxAcc1} = foldi(Clm, Fun, CtxAcc),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx2},
        Fields),
    {"create " ++ OptsStr ++ " table " ++ binary_to_list(Tab)
        ++ " (" ++ string:join(Clms, ", ") ++ ")"
    , NewCtx3};

%
% CREATE USER
%
foldi({'create user', Usr, Id, Opts} = ST, Fun, Ctx)
 when is_binary(Usr), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Usr, NewCtx),
    {IdStr, NewCtx2} = foldi(Id, Fun, NewCtx1),
    {OptsStr, NewCtx3} = foldi(Opts, Fun, NewCtx2),
    {"create user " ++ binary_to_list(Usr)
    ++ IdStr ++ " " ++ OptsStr
    , NewCtx3};

%
% ALTER USER
%
foldi({'alter user', Usr, {spec, Opts}} = ST, Fun, Ctx)
 when is_binary(Usr), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Usr, NewCtx),
    {OptsStr, NewCtx2} = foldi(Opts, Fun, NewCtx1),
    {lists:flatten(["alter user ", binary_to_list(Usr), " ", OptsStr])
    , NewCtx2};

%
% TRUNCATE TABLE
%
foldi({'truncate table', Tbl, Mvl, Storage} = ST, Fun, Ctx)
 when is_binary(Tbl), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Tbl, NewCtx),
    NewCtx2 = Fun(Mvl, NewCtx1),
    NewCtx3 = Fun(Storage, NewCtx2),
    {"truncate table " ++ binary_to_list(Tbl) ++ " " ++
    case Mvl of
        {} -> "";
        {'materialized view log', T} -> lists:flatten([atom_to_list(T), " materialized view log "])
    end
    ++
    case Storage of
        {} -> "";
        {'storage', T} -> lists:flatten([atom_to_list(T), " storage"])
    end
    , NewCtx3};

%
% UPDATE TABLE
%
foldi({'update', Tbl, {set, Set}, Where, Return} = ST, Fun, Ctx)
 when is_binary(Tbl), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Tbl, NewCtx),
    {Sets, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} = foldi(S, Fun, CtxAcc),
            {Acc ++ [SubAcc], CtxAcc1}
        end,
        {[], NewCtx1},
        Set),
    {WhereStr, NewCtx3} = foldi(Where, Fun, NewCtx2),
    {ReturnStr, NewCtx4} = foldi(Return, Fun, NewCtx3),
    {"update " ++ binary_to_list(Tbl)
    ++ " set " ++ string:join(Sets, ",")
    ++ " " ++ WhereStr ++ ReturnStr
    , NewCtx4};

%
% DROPS
%
foldi({'drop user', Usr, Opts} = ST, Fun, Ctx)
 when is_binary(Usr), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Usr, NewCtx),
    {OptsStr, NewCtx2} = foldi(Opts, Fun, NewCtx1),
    {"drop user " ++ binary_to_list(Usr)
     ++ " " ++ OptsStr
    , NewCtx2};
foldi({'drop table', {tables, Ts}, {exists, E}, {opt, R}} = ST, Fun, Ctx)
 when is_atom(R), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(E, NewCtx),
    {Tables, NewCtx2} = lists:foldl(fun(T, {Acc, CtxAcc}) ->
            CtxAcc1 = Fun(T, CtxAcc),
            {Acc++[binary_to_list(T)], CtxAcc1}
        end,
        {[], NewCtx1},
        Ts),
    NewCtx3 = Fun(R, NewCtx2),
    {"drop table "
     ++ if E =:= true -> " if exists "; true -> "" end
     ++ string:join(Tables, ", ")
     ++ " " ++ atom_to_list(R)
    , NewCtx3};

%
% DELETE
%
foldi({'delete', Table, Where, Return} = ST, Fun, Ctx)
 when is_binary(Table), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Table, NewCtx),
    {WhereStr, NewCtx2} = foldi(Where, Fun, NewCtx1),
    {ReturnStr, NewCtx3} = foldi(Return, Fun, NewCtx2),
    {"delete from " ++ binary_to_list(Table)
     ++ " " ++ WhereStr ++ ReturnStr
    , NewCtx3};

%
% GRANT
%
foldi({'grant', Objs, {OnTyp, On}, {'to', Tos}, Opts} = ST, Fun, Ctx)
 when is_atom(OnTyp), is_atom(Opts), is_function(Fun, 2) ->
    NewCtx = Fun(ST, Ctx),
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[atom_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx},
        Objs),
    NewCtx2 = Fun(OnTyp, NewCtx1),
    NewCtx3 = Fun(On, NewCtx2),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    {"grant "
     ++ string:join(ObjsStr, ",") ++ " "
     ++ if On =/= <<"">> -> atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
     ++ if length(Tos) > 0 -> "to " ++ string:join(TosStr, ",") ++ " "; true -> "" end
     ++ atom_to_list(Opts)
    , NewCtx5};

%
% REVOKE
%
foldi({'revoke', Objs, {OnTyp, On}, {'from', Tos}, Opts} = ST, Fun, Ctx)
 when is_function(Fun, 2), is_atom(OnTyp), is_atom(Opts) ->
    NewCtx = Fun(ST, Ctx),
    {ObjsStr, NewCtx1} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[atom_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx},
        Objs),
    NewCtx2 = Fun(OnTyp, NewCtx1),
    NewCtx3 = Fun(On, NewCtx2),
    {TosStr, NewCtx4} = lists:foldl(fun(O, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(O)], Fun(O, CtxAcc)}
        end,
        {[], NewCtx3},
        Tos),
    NewCtx5 = Fun(Opts, NewCtx4),
    {"revoke "
     ++ string:join(ObjsStr, ",") ++ " "
     ++ if On =/= <<"">> -> atom_to_list(OnTyp) ++ " " ++ binary_to_list(On) ++ " "; true -> "" end
     ++ if length(Tos) > 0 -> "from " ++ string:join(TosStr, ",") ++ " "; true -> "" end
     ++ atom_to_list(Opts)
    , NewCtx5};

%--------------------------------------------------------------------
% component matching patterns
%

% Empty list or tuples
foldi(X, Fun, Ctx) when is_function(Fun, 2) andalso (X =:= {} orelse X =:= []) -> {"", Ctx};

% All option and optionlist and its variants
foldi({'identified globally', E} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {IdStr, NewCtx1} = foldi(E, Fun, NewCtx),
    {" identified globally " ++ IdStr
    , NewCtx1};
foldi({'identified extern', E} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {IdStr, NewCtx1} = foldi(E, Fun, NewCtx),
    {" identified externally " ++ IdStr
    , NewCtx1};
foldi({'identified by', Pswd} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Pswd, NewCtx),
    {" identified by " ++ binary_to_list(Pswd)
    , NewCtx1};
foldi([{'scope', S}|Opts] = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(S, NewCtx),
    {OptsStr, NewCtx2} = foldi(Opts, Fun, NewCtx1),
    {lists:flatten([" ", atom_to_list(S), " ", OptsStr])
    , NewCtx2};
foldi([{'type', T}|Opts] = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(T, NewCtx),
    {OptsStr, NewCtx2} = foldi(Opts, Fun, NewCtx1),
    {lists:flatten([" ", atom_to_list(T), " ", OptsStr])
    , NewCtx2};
foldi([{'limited', Q, T}|O] = ST, Fun, Ctx)
 when is_binary(Q), is_binary(T), is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Q, NewCtx),
    NewCtx2 = Fun(T, NewCtx1),
    {Os, NewCtx3} = foldi(O, Fun, NewCtx2),
    {lists:flatten(["quota ", binary_to_list(Q), " on ", binary_to_list(T), " ", Os])
    , NewCtx3};
foldi([cascade|Opts] = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {OptsStr, NewCtx1} = foldi(Opts, Fun, NewCtx),
    {lists:flatten([" cascade ", OptsStr])
    , NewCtx1};
foldi([{Tok, T}|Opts] = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    if is_binary(T) andalso (Tok =:= 'default tablespace'
                             orelse Tok =:= 'temporary tablespace'
                             orelse Tok =:= 'profile') ->
            NewCtx1 = Fun(Tok, NewCtx),
            NewCtx2 = Fun(T, NewCtx1),
            {OptsStr, NewCtx3} = foldi(Opts, Fun, NewCtx2),
            {lists:flatten([atom_to_list(Tok), " ", binary_to_list(T), " ", OptsStr])
            , NewCtx3};
        true ->
            case {Tok, T} of
                {'password', 'expire'}                  -> {"password expire ", Fun({Tok, T}, NewCtx)};
                {'account', 'lock'}                     -> {"account lock ", Fun({Tok, T}, NewCtx)};
                {'account', 'unlock'}                   -> {"account unlock ", Fun({Tok, T}, NewCtx)};
                {'unlimited on', T} when is_binary(T)   ->
                    {lists:flatten(["quota unlimited on ", binary_to_list(T)])
                    , Fun({Tok, T}, NewCtx)};
                {'quotas', Qs}                          -> foldi(Qs, Fun, NewCtx);
                _                                       -> foldi({Tok, T}, Fun, NewCtx)
            end
    end;
foldi({'default', Def} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {DefStr, NewCtx1} = foldi(Def, Fun, NewCtx),
    {lists:flatten([" default ", 
        case Def of
            Def when is_binary(Def) -> binary_to_list(Def);
            Def -> DefStr
        end, "\n "])
    , NewCtx1};

% select sub-part patterns
foldi({hints, Hints} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    Size = byte_size(Hints),
    NewCtx1 = Fun(Hints, NewCtx),
    {if Size > 0 -> binary_to_list(Hints);
     true        -> ""
     end
    , NewCtx1};
foldi({opt, Opt} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    Size = byte_size(Opt),
    NewCtx1 = Fun(Opt, NewCtx),
    {if Size > 0 -> binary_to_list(Opt) ++ " ";
     true        -> ""
     end
    , NewCtx1};
foldi({fields, Fields} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            case F of
                F when is_binary(F) -> {Acc++[binary_to_list(F)], Fun(F, CtxAcc)};
                {'select', _} = F   ->
                    {SubAcc, CtxAcc1} = foldi(F, Fun, CtxAcc),
                    {Acc++[lists:flatten(["(", SubAcc, ")"])], CtxAcc1};
                Other ->
                    {SubAcc, CtxAcc1} = foldi(Other, Fun, CtxAcc),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Fields),
    {string:join(FieldsStr, ", ")
    , NewCtx1};
foldi({into, Into} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {IntoStr, NewCtx1} = lists:foldl(fun(I, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(I)], Fun(I, CtxAcc)}
        end,
        {[], NewCtx},
        Into),
    {string:join(IntoStr, ", ") ++ " "
    , NewCtx1};
foldi({from, Forms} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {FormStr, NewCtx1} = case Forms of
        Forms when is_list(Forms) ->
            {FrmStr, NewCtx2} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
                    case F of
                        F when is_binary(F) -> {Acc++[binary_to_list(F)], Fun(F,CtxAcc)};
                        {'select', _} = F   ->
                            {FoldFStr, CtxAcc1} = foldi(F, Fun, Ctx),
                            {lists:flatten(["(", FoldFStr, ")"])
                            , CtxAcc1};
                        Other               ->
                            {SubAcc, CtxAcc1} = foldi(Other, Fun, Ctx),
                            {Acc++[SubAcc], CtxAcc1}
                    end
                end,
                {[], NewCtx},
                Forms),
            {string:join(FrmStr, ", ")
            , NewCtx2};
        Forms ->
            foldi(Forms, Fun, NewCtx)
    end,
    {"from " ++FormStr++ " "
    , NewCtx1};
foldi({'group by', GroupBy} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    Size = length(GroupBy),
    {GroupByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            case foldi(F, Fun, CtxAcc) of
                {F1, CtxAcc1} when is_binary(F1) -> {Acc++binary_to_list(F1), CtxAcc1};
                {F1, CtxAcc1} when is_list(F1) -> {Acc++[F1], CtxAcc1}
            end
        end,
        {[], NewCtx},
        GroupBy),
    {if Size > 0 -> " group by " ++ string:join(GroupByStr, ", ");
        true -> ""
     end
    , NewCtx1};
foldi({having, Having} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    Size = size(Having),
    {HavingStr, NewCtx1} = foldi(Having, Fun, NewCtx),
    {if Size > 0 -> " having " ++ HavingStr;
        true -> ""
     end
    , NewCtx1};
foldi({'order by', OrderBy} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    Size = length(OrderBy),
    {OrderByStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            case F of
                F when is_binary(F) -> {Acc++[binary_to_list(F)], Fun(F, CtxAcc)};
                {O, Op} when is_binary(O), is_binary(Op) ->
                    CtxAcc1 = Fun(O, CtxAcc),
                    CtxAcc2 = Fun(Op, CtxAcc1),
                    {Acc++[string:strip(lists:flatten([binary_to_list(O), " ", binary_to_list(Op)]))]
                    , CtxAcc2};
                {O, Op} when is_binary(Op) ->
                    {Os, CtxAcc1} = foldi(O, Fun, CtxAcc),
                    CtxAcc2 = Fun(Op, CtxAcc1),
                    {Acc++[string:strip(lists:flatten([Os, " ", binary_to_list(Op)]))]
                    , CtxAcc2}
            end
        end,
        {[], NewCtx},
        OrderBy),
    {if Size > 0 ->
        " order by " ++ string:join(OrderByStr, ", ")
        ++ " ";
        true -> ""
     end
    , NewCtx1};

% joins
foldi({JoinType, Tab} = ST, Fun, Ctx)
 when (is_function(Fun,2) andalso (
        (JoinType =:= cross_join) orelse
        (JoinType =:= natural_join) orelse
        (JoinType =:= natural_inner_join)
     )) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr, NewCtx2} = foldi(Tab, Fun, NewCtx1),
    {case JoinType of
        cross_join          -> " cross join ";
        natural_join        -> " natural join ";
        natural_inner_join  -> " natural inner join "
     end ++ TabStr
    , NewCtx2};
foldi({{JoinType,OptPartition,OptNatural},Tab,OptPartition1,OnOrUsing} = ST, Fun, Ctx)
 when (is_function(Fun,2) andalso (
        (JoinType =:= full) orelse
        (JoinType =:= left) orelse
        (JoinType =:= right) orelse
        (JoinType =:= full_outer) orelse
        (JoinType =:= left_outer) orelse
        (JoinType =:= right_outer)
      )) ->
    NewCtx = Fun(ST, Ctx),
    {OptPartitionStr, NewCtx1}  = foldi(OptPartition, Fun,   NewCtx),
    {OptNaturalStr,   NewCtx2} = foldi(OptNatural, Fun,     NewCtx1),
                      NewCtx3  = Fun(JoinType,              NewCtx2),
    {TabStr,          NewCtx4} = foldi(Tab, Fun,            NewCtx3),
    {OptPartition1Str,NewCtx5} = foldi(OptPartition1, Fun,  NewCtx4),
    {OnOrUsingStr,    NewCtx6} = foldi(OnOrUsing, Fun,      NewCtx5),

    {OptPartitionStr ++ OptNaturalStr ++
    case JoinType of
        full        -> " full join ";
        left        -> " left join ";
        right       -> " right join ";
        full_outer  -> " full outer join ";
        left_outer  -> " left outer join ";
        right_outer -> " right outer join "
    end ++ TabStr ++ OptPartition1Str ++ OnOrUsingStr
    , NewCtx6};
foldi({JoinType, Tab, OnOrUsing} = ST, Fun, Ctx)
 when (is_function(Fun,2) andalso (
        (JoinType =:= join) orelse
        (JoinType =:= join_inner)
      )) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(JoinType, NewCtx),
    {TabStr, NewCtx2} = foldi(Tab, Fun, NewCtx1),
    {OnOrUsingStr, NewCtx3} = foldi(OnOrUsing, Fun, NewCtx2),
    {case JoinType of
        join        -> " join ";
        join_inner -> " inner join "
     end ++ TabStr ++ OnOrUsingStr
    , NewCtx3};
foldi({partition_by,Fields} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {FieldsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(F)], Fun(F, CtxAcc)}
        end,
        {[], NewCtx},
        Fields),
    {" partition by (" ++ string:join(FieldsStr, ",") ++ ")"
    , NewCtx1};
foldi({on, Condition} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {CondStr, NewCtx1} = foldi(Condition, Fun, NewCtx),
    {" on " ++ CondStr
    , NewCtx1};
foldi({using, ColumnList} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {ColumnListStr, NewCtx1} = lists:foldl(fun(C, {Acc, CtxAcc}) ->
            {Acc++[binary_to_list(C)], Fun(C, CtxAcc)}
        end,
        {[], NewCtx},
        ColumnList),
    {" using (" ++ string:join(ColumnListStr, ",") ++ ")"
    , NewCtx1};
foldi(natural, Fun, Ctx)
 when is_function(Fun,2) ->
    {" natural", Fun(natural, Ctx)};
foldi({Tab, [J|_] = Joins} = ST, Fun, Ctx)
 when is_function(Fun,2) andalso
        (is_tuple(J) andalso (is_binary(Tab) orelse is_tuple(Tab))) ->
    NewCtx = Fun(ST, Ctx),
    {TabStr, NewCtx1} = foldi(Tab, Fun, NewCtx),
    {JoinsStr, NewCtx2} = lists:foldl(fun(Join, {Acc, CtxAcc}) ->
            {SubAcc, CtxAcc1} = foldi(Join, Fun, CtxAcc),
            {Acc++[SubAcc], CtxAcc1}
        end,
        {[], NewCtx1},
        Joins),
    {TabStr++JoinsStr
    , NewCtx2};

% betwen operator
foldi({'between', A, B, C} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {A1, NewCtx1}  = if is_binary(A) -> {binary_to_list(A), NewCtx}; true -> foldi(A, Fun, NewCtx) end,
    {B1, NewCtx2} = if is_binary(B) -> {binary_to_list(B), NewCtx1}; true -> foldi(B, Fun, NewCtx1) end,
    {C1, NewCtx3} = if is_binary(C) -> {binary_to_list(C), NewCtx2}; true -> foldi(C, Fun, NewCtx2) end,
    {lists:flatten([A1,  " between ", B1, " and ", C1])
    , NewCtx3};

% PL/SQL concatenate operator
foldi({'||', Args} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {ArgsStr, NewCtx1} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
            case A of
                A when is_binary(A) -> {Acc++[binary_to_list(A)], Fun(A, CtxAcc)};
                A ->
                    {SubAcc, CtxAcc1} = foldi(A, Fun, CtxAcc),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Args),
    {string:join(ArgsStr, " || ")
    , NewCtx1};

% All aliases
foldi({as, A, B} = ST, Fun, Ctx)
 when is_function(Fun,2), is_binary(A), is_binary(B) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(A, NewCtx),
    NewCtx2 = Fun(B, NewCtx1),
    {lists:flatten([binary_to_list(A), " ", binary_to_list(B)])
    , NewCtx2};
foldi({as, A, B} = ST, Fun, Ctx)
    when is_function(Fun,2), is_binary(B) ->
    NewCtx = Fun(ST, Ctx),
    {AStr, NewCtx1} = foldi(A, Fun, NewCtx), 
    NewCtx2 = Fun(B, NewCtx1),
    {lists:flatten([AStr, " ", binary_to_list(B)])
    , NewCtx2};
foldi({as, A} = ST, Fun, Ctx)
 when is_function(Fun,2), is_binary(A) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(A, NewCtx),
    {lists:flatten(["as ", binary_to_list(A)])
    , NewCtx1};
foldi(Tab, Fun, Ctx)
 when is_function(Fun,2), is_binary(Tab) ->
    NewCtx = Fun(Tab, Ctx),
    {binary_to_list(Tab)
    , NewCtx};

% Union
foldi({union, A, B} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {AStr, NewCtx1} = foldi(A, Fun, NewCtx),
    {BStr, NewCtx2} = foldi(B, Fun, NewCtx1),
    {lists:flatten(["(", AStr, " union ", BStr, ")"])
    , NewCtx2};

% All where clauses
foldi({where, {}} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {""
    , NewCtx};
foldi({where, Where} = ST, Fun, Ctx)
 when is_function(Fun,2), is_tuple(Where) ->
    NewCtx = Fun(ST, Ctx),
    {WhereStr, NewCtx1} = foldi(Where, Fun, NewCtx),
    {"where " ++ WhereStr
    , NewCtx1};

% Like operator
foldi({'like',Var,Like,OptEsc} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {VarStr, NewCtx1} = foldi(Var, Fun, NewCtx),
    {LikeStr, NewCtx2} = foldi(Like, Fun, NewCtx1),
    NewCtx3 = Fun(OptEsc, NewCtx2),
    {VarStr ++ " like " ++ LikeStr ++
    if byte_size(OptEsc) > 0 -> " escape "++binary_to_list(OptEsc);
       true -> ""
    end
    , NewCtx3};

% In operator
% for right hand non list argument extra parenthesis added
foldi({'in', L, {'list', _} = R} = ST, Fun, Ctx)
 when is_function(Fun,2), is_binary(L) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(L, NewCtx),
    {RStr, NewCtx2} = foldi(R, Fun, NewCtx1),
    {lists:flatten([binary_to_list(L), " in ", RStr])
    , NewCtx2};
foldi({'in', L, R} = ST, Fun, Ctx)
 when is_function(Fun,2), is_binary(L), is_tuple(R) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(L, NewCtx),
    {RStr, NewCtx2} = foldi(R, Fun, NewCtx1),
    {lists:flatten([binary_to_list(L), " in (", RStr, ")"])
    , NewCtx2};

% Optional Returning phrase
foldi({R, Sel, Var} = ST, Fun, Ctx)
 when is_function(Fun,2) andalso (
        (R =:= return) orelse
        (R =:= returning)
      ) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(R, NewCtx),
    {SelStr, NewCtx2} = lists:foldl(fun(S, {Acc, CtxAcc}) ->
            case S of
                S when is_binary(S) -> {Acc++[binary_to_list(S)], Fun(S, CtxAcc)};
                S ->
                    {SubAcc, CtxAcc1} = foldi(S, Fun, CtxAcc),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx1},
        Sel),
    {VarStr, NewCtx3} = lists:foldl(fun({param, V}, {Acc, CtxAcc}) ->
            case V of
                V when is_binary(V) -> {Acc++[binary_to_list(V)], Fun(V, CtxAcc)};
                V ->
                    {SubAcc, CtxAcc1} = foldi(V, Fun, CtxAcc),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx2},
        Var),
    {" "++atom_to_list(R)++" "++string:join(SelStr, ",")
    ++ " INTO " ++
    string:join(VarStr, ",")
    , NewCtx3};
foldi({R, {}}, Fun, Ctx)
 when is_function(Fun,2) andalso (
        (R =:= return) orelse
        (R =:= returning)
      ) ->
    {"", Fun(R, Ctx)};

% Boolean and arithmetic binary operators handled with precedence
% *,/ > +,- > and > or
foldi({Op, L, R} = ST, Fun, Ctx)
 when is_function(Fun,2), is_atom(Op), is_tuple(L), is_tuple(R) ->
    NewCtx = Fun(ST, Ctx),
    {Fl, NewCtx1} = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC1} = foldi(L, Fun, NewCtx), {lists:flatten(["(",Ls,")"]), NC1};
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC1} = foldi(L, Fun, NewCtx), {lists:flatten(["(",Ls,")"]), NC1};
        {'and', 'or'}                         -> {Ls, NC1} = foldi(L, Fun, NewCtx), {lists:flatten(["(",Ls,")"]), NC1};
        _ -> foldi(L, Fun, NewCtx)
    end,
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC2} = foldi(R, Fun, NewCtx2), {lists:flatten(["(",Rs,")"]), NC2};
        {'/', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC2} = foldi(R, Fun, NewCtx2), {lists:flatten(["(",Rs,")"]), NC2};
        {'and', 'or'}                         -> {Rs, NC2} = foldi(R, Fun, NewCtx2), {lists:flatten(["(",Rs,")"]), NC2};
        _ -> foldi(R, Fun, NewCtx2)
    end,
    {lists:flatten([Fl, " ", atom_to_list(Op), " ", Fr])
    , NewCtx3};
foldi({Op, L, R} = ST, Fun, Ctx)
 when is_function(Fun,2), is_atom(Op), is_binary(L), is_tuple(R) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    {Fr, NewCtx3} = case {Op, element(1, R)} of
        {'*', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC} = foldi(R, Fun, NewCtx2), {lists:flatten(["(",Rs,")"]), NC};
        {'/', Or} when Or =:= '-'; Or =:= '+' -> {Rs, NC} = foldi(R, Fun, NewCtx2), {lists:flatten(["(",Rs,")"]), NC};
        _ -> foldi(R, Fun, NewCtx2)
    end,
    {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", Fr])
    , NewCtx3};
foldi({Op, L, R} = ST, Fun, Ctx)
 when is_function(Fun,2), is_atom(Op), is_tuple(L), is_binary(R) ->
    NewCtx = Fun(ST, Ctx),
    {Fl, NewCtx1} = case {Op, element(1, L)} of
        {'*', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC} = foldi(L, Fun, NewCtx), {lists:flatten(["(",Ls,")"]), NC};
        {'/', Ol} when Ol =:= '-'; Ol =:= '+' -> {Ls, NC} = foldi(L, Fun, NewCtx), {lists:flatten(["(",Ls,")"]), NC};
        _ -> foldi(L, Fun, NewCtx)
    end,
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    {lists:flatten([Fl, " ", atom_to_list(Op), " ", binary_to_list(R)])
    , NewCtx3};
foldi({Op, L, R} = ST, Fun, Ctx)
 when is_function(Fun,2), is_atom(Op), is_binary(L), is_binary(R) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(L, NewCtx),
    NewCtx2 = Fun(Op, NewCtx1),
    NewCtx3 = Fun(R, NewCtx2),
    {lists:flatten([binary_to_list(L), " ", atom_to_list(Op), " ", binary_to_list(R)])
    , NewCtx3};

% Unary - and 'not' operators
foldi({Op, A} = ST, Fun, Ctx)
 when is_function(Fun,2) andalso (Op =:= '-' orelse Op =:= 'not') ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(Op, NewCtx),
    case A of
        A when is_binary(A) ->
            NewCtx2 = Fun(A, NewCtx1),
            {lists:flatten([atom_to_list(Op), " (", binary_to_list(A), ")"])
            , NewCtx2};
        A ->
            {As, NewCtx2} = foldi(A, Fun, NewCtx1),
            {lists:flatten([atom_to_list(Op)," (", As, ")"])
            , NewCtx2}
    end;

% funs
foldi({'fun', N, Args} = ST, Fun, Ctx)
 when is_function(Fun,2), is_binary(N) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(N, NewCtx),
    {ArgsStr, NewCtx2} = lists:foldl(fun(A, {Acc, CtxAcc}) ->
            case A of
                A when is_binary(A) -> {Acc++[binary_to_list(A)], Fun(A, CtxAcc)};
                A when is_tuple(A) ->
                    case lists:member(element(1, A), [ 'select', 'insert', 'create table'
                                                     , 'create user', 'alter user'
                                                     , 'truncate table', 'update', 'delete'
                                                     , 'grant', 'revoke']) of
                        true ->
                            {SubAcc, CtxAcc1} = foldi(A, Fun, CtxAcc),
                            {Acc++["(" ++ string:strip(SubAcc) ++ ")"], CtxAcc1};
                        _ ->
                            {SubAcc, CtxAcc1} = foldi(A, Fun, CtxAcc),
                            {Acc++[SubAcc], CtxAcc1}
                    end;
                A ->
                    {SubAcc, CtxAcc1} = foldi(A, Fun, CtxAcc),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx1},
        Args),
    {binary_to_list(N) ++ "(" ++
    string:join(ArgsStr, ", ")
    ++ ")"
    , NewCtx2};

% hierarchical query
foldi({'hierarchical query', {}} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {"", NewCtx};
foldi({'hierarchical query', {Part1, Part2}} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {Part1Str, NewCtx1} = foldi(Part1, Fun, NewCtx),
    {Part2Str, NewCtx2} = foldi(Part2, Fun, NewCtx1),
    {lists:flatten([Part1Str, " ", Part2Str])
    , NewCtx2};
foldi({'start with', StartWith} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {StartWithStr, NewCtx1} = foldi(StartWith, Fun, NewCtx),
    {lists:flatten([" start with ", StartWithStr])
    , NewCtx1};
foldi({'connect by', NoCycle, ConnectBy} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {NoCycleStr, NewCtx1} = foldi(NoCycle, Fun, NewCtx),
    {ConnectByStr, NewCtx2} = foldi(ConnectBy, Fun, NewCtx1),
    {lists:flatten(["connect by "
                  , if byte_size(NoCycle) > 0 -> NoCycleStr++" "; true -> "" end
                  , ConnectByStr])
    , NewCtx2};
foldi({'prior', Field} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {FieldsStr, NewCtx1} = foldi(Field, Fun, NewCtx),
    {lists:flatten(["prior ", FieldsStr])
    , NewCtx1};

% lists
foldi({'list', Elms} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {ElmsStr, NewCtx1} = lists:foldl(fun(E, {Acc, CtxAcc}) ->
            case E of
                E when is_binary(E) -> {Acc++[binary_to_list(E)], Fun(E, CtxAcc)};
                E ->
                    {SubAcc, CtxAcc1} = foldi(E, Fun, CtxAcc),
                    {Acc++[SubAcc], CtxAcc1}
            end
        end,
        {[], NewCtx},
        Elms),
    {"(" ++
     string:join(ElmsStr, ", ")
     ++ ")"
    , NewCtx1};

foldi({'param', P} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    NewCtx1 = Fun(P, NewCtx),
    case P of
        P when is_binary(P) -> {binary_to_list(P), NewCtx1};
        P -> {P, NewCtx1}
    end;

foldi({'case', When, Then, Else} = ST, Fun, Ctx)
 when is_function(Fun,2) ->
    NewCtx = Fun(ST, Ctx),
    {WhenStr, NewCtx1} = foldi(When, Fun, NewCtx),
    {ThenStr, NewCtx2} = foldi(Then, Fun, NewCtx1),
    {ElseStr, NewCtx3} = case Else of
        {} -> {"", NewCtx2};
        Else ->
            {EStr, NewCtx21} = foldi(Else, Fun, NewCtx2),
            {" else " ++ EStr, NewCtx21}
    end,
    {"case when " ++WhenStr++" then "++ThenStr++ElseStr++" end"
    , NewCtx3};

%
% UNSUPPORTED
%
foldi(PTree, Fun, Ctx)
 when is_function(Fun,2) ->
    Fun(PTree, Ctx),
    io:format(user, "Parse tree not supported ~p~n", [PTree]),
    throw({"Parse tree not supported",PTree}).

%%-----------------------------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("sql_tests.hrl").

foldi(PTree) -> foldi(PTree, fun(_,_) -> ok end, ok).

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
                {NSql, _} = foldi(ParseTree),
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
            if (Limit =:= 1) -> NewPrivate;
                true -> test_parse(ShowParseTree, Sqls, N+1, Limit-1, NewPrivate)
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
