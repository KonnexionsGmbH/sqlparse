%% -----------------------------------------------------------------------------
%%
%% sqlparse.yrl: SQL - parser definition.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -*- erlang -*-
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email bikram.chatterjee@k2informatics.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 all_distinct
 all_or_any_predicate
 alter_user_def
 any_all_some
 asc_desc
 assignment
 assignment_commalist
 atom
 base_table_element
 base_table_element_commalist
 between_predicate
 case_when_exp
 case_when_opt_as_exp
 case_when_then
 case_when_then_list
 close_statement
 column
 column_commalist
 column_def
 column_def_opt
 column_def_list
 column_ref
 column_ref_commalist
 commit_statement
 comparison_predicate
 create_index_def
 create_index_filter
 create_index_norm
 create_index_opts
 create_index_spec
 create_index_spec_column
 create_index_spec_items
 create_opts
 create_role_def
 create_table_def
 create_user_def
 cursor
 cursor_def
 data_type
 db_user_proxy
 delete_statement_positioned
 delete_statement_searched
 drop_index_def
 drop_role_def
 drop_table_def
 drop_user_def
 else
 escape
 existence_test
 exists
 extra
 fetch_statement
 from_clause
 from_column
 from_column_commalist
 fun_arg
 fun_args
 function_ref
 function_ref_list
 grant_def
 grantee_identified_by
 grantee_revokee
 grantee_revokee_commalist
 group_by_clause
 having_clause
 hierarchical_query_clause
 hint
 identified
 in_predicate
 index_name
 inner_cross_join
 insert_atom
 insert_atom_commalist
 insert_statement
 into
 is_not_null
 is_null
 join
 join_clause
 join_list
 join_on_or_using_clause
 join_ref
 like_predicate
 literal
 manipulative_statement
 materialized
 nocycle
 not_between
 not_in
 not_like
 object_privilege
 object_privilege_list
 object_with_grant_option
 object_with_revoke_option
 on_obj_clause
 open_statement
 order_by_clause
 ordering_spec
 ordering_spec_commalist
 outer_join
 outer_join_type
 parameter
 parameter_ref
 predicate
 procedure_call
 proxy_auth_req
 proxy_clause
 proxy_with
 query_exp
 query_partition_clause
 query_spec
 query_term
 quota
 quota_list
 restrict_cascade
 returning
 revoke_def
 role_list
 rollback_statement
 scalar_exp
 scalar_exp_commalist
 scalar_opt_as_exp
 scalar_sub_exp
 schema
 schema_element
 schema_element_list
 search_condition
 select_field
 select_field_commalist
 select_statement
 selection
 sgn_num
 spec_item
 spec_list
 sql
 sql_list
 storage
 subquery
 system_privilege
 system_privilege_list
 system_with_grant_option
 table
 table_alias
 table_coll_expr
 table_constraint_def
 table_dblink
 table_exp
 table_list
 table_ref
 target
 target_commalist
 tbl_scope
 tbl_type
 test_for_null
 truncate_table
 unary_add_or_subtract
 update_statement_positioned
 update_statement_searched
 user_list
 user_opt
 user_opts_list
 user_role
 values_or_query_spec
 view_def
 when_action
 where_clause
.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% symbolic tokens
%% literal keyword tokens
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Terminals
 ADMIN
 ALL
 ALTER
 AND
 ANY
 APPROXNUM
 AS
 ASC
 AUTHENTICATION
 AUTHORIZATION
 BAG
 BEGIN
 BETWEEN
 BITMAP
 BY
 CALL
 CASCADE
 CASE
 CHECK
 CLOSE
 CLUSTER
 COMMIT
 COMPARISON
 CONNECT
 CONSTRAINT
 CONSTRAINTS
 CONTINUE
 CREATE
 CROSS
 CURRENT
 CURSOR
 DBLINK
 DEFAULT
 DELEGATE
 DELETE
 DESC
 DIRECTORY
 DISTINCT
 DROP
 ELSE
 END
 ENTERPRISE
 ESCAPE
 EXECUTE
 EXCEPT
 EXISTS
 EXTERNALLY
 FETCH
 FILTER_WITH
 FORCE
 FOREIGN
 FOUND
 FROM
 FULL
 FUNS
 GLOBALLY
 GOTO
 GRANT
 GROUP
 HASHMAP
 HAVING
 HIERARCHY
 HINT
 IDENTIFIED
 IF
 IN
 INDEX
 INDICATOR
 INNER
 INSERT
 INTERSECT
 INTNUM
 INTO
 IS
 JOIN
 JSON
 KEY
 KEYLIST
 LEFT
 LIKE
 LOCAL
 LOG
 MATERIALIZED
 MINUS
 NAME
 NATURAL
 NO
 NOCYCLE
 NONE
 NORM_WITH
 NOT
 NULLX
 OF
 ON
 OPEN
 OPTION
 OR
 ORDER
 ORDERED_SET
 OUTER
 PARAMETER
 PARTITION
 PRESERVE
 PRIMARY
 PRIOR
 PRIVILEGES
 PROFILE
 PUBLIC
 PURGE
 QUOTA
 REFERENCES
 REQUIRED
 RESTRICT
 RETURN
 RETURNING
 REUSE
 REVOKE
 RIGHT
 ROLE
 ROLES
 ROLLBACK
 SCHEMA
 SELECT
 SET
 SOME
 SQLERROR
 START
 STORAGE
 STRING
 TABLE
 TABLESPACE
 TEMPORARY
 THEN
 THROUGH
 TO
 TRUNCATE
 UNION
 UNIQUE
 UNLIMITED
 UPDATE
 USER
 USERS
 USING
 VALUES
 VIEW
 WHEN
 WHENEVER
 WHERE
 WITH
 WORK
 '('
 ')'
 '*'
 '+'
 ','
 '-'
 '.'
 '/'
 ';'
 '='
 'div'
 '||'
.

Rootsymbol sql_list.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% precedence
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Left        100 OR.
Left        200 AND.
Left        300 NOT.
Nonassoc    400 '=' COMPARISON.
Left        500 '+' '-' '||'.
Left        600 '*' '/' 'div'.
Left        700 unary_add_or_subtract.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sql_list ->          sql ';'       :         [{'$1',{extra, <<>>}}].
sql_list ->          sql ';' extra :         [{'$1','$3'}].
sql_list -> sql_list sql ';'       : '$1' ++ [{'$2',{extra, <<>>}}].
sql_list -> sql_list sql ';' extra : '$1' ++ [{'$2','$4'}].

extra -> NAME  ';' : {extra, unwrap_bin('$1')}.

%% =============================================================================
%% Helper definitions - test purposes.
%% -----------------------------------------------------------------------------
% sql -> table_ref                      : '$1'.
%% =============================================================================

sql -> cursor_def                     : '$1'.
sql -> manipulative_statement         : '$1'.
sql -> procedure_call                 : '$1'.
sql -> schema                         : '$1'.
sql -> WHENEVER NOT FOUND when_action : {when_not_found, '$4'}.
sql -> WHENEVER SQLERROR  when_action : {when_sql_err, '$3'}.

procedure_call -> BEGIN function_ref_list END : {'begin procedure', '$2'}.
procedure_call -> BEGIN sql_list          END : {'begin procedure', '$2'}.
procedure_call -> CALL  function_ref          : {'call procedure', '$2'}.

function_ref_list -> function_ref ';'                   : ['$1'].
function_ref_list -> function_ref ';' function_ref_list : ['$1' | '$3'].

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% schema definition language
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schema -> CREATE SCHEMA AUTHORIZATION NAME                     : {'create schema authorization', unwrap('$4'), []}.
schema -> CREATE SCHEMA AUTHORIZATION NAME schema_element_list : {'create schema authorization', unwrap('$4'), '$5'}.

schema_element_list ->                     schema_element :         ['$1'].
schema_element_list -> schema_element_list schema_element : '$1' ++ ['$2'].

schema_element -> create_table_def : '$1'.
schema_element -> view_def         : '$1'.
schema_element -> grant_def        : '$1'.

create_role_def -> CREATE ROLE NAME : {'create role', unwrap_bin('$3')}.

create_table_def -> CREATE             TABLE table '('                              ')' : {'create table', '$3', [],   []}.
create_table_def -> CREATE             TABLE table '(' base_table_element_commalist ')' : {'create table', '$3', '$5', []}.
create_table_def -> CREATE create_opts TABLE table '('                              ')' : {'create table', '$4', [],   '$2'}.
create_table_def -> CREATE create_opts TABLE table '(' base_table_element_commalist ')' : {'create table', '$4', '$6', '$2'}.

create_user_def -> CREATE USER NAME identified                : {'create user', unwrap_bin('$3'), '$4', []}.
create_user_def -> CREATE USER NAME identified user_opts_list : {'create user', unwrap_bin('$3'), '$4', '$5'}.

drop_table_def -> DROP      TABLE        table_list                  : {'drop table', {'tables', '$3'}, {},   {},   []}.
drop_table_def -> DROP      TABLE        table_list restrict_cascade : {'drop table', {'tables', '$3'}, {},   '$4', []}.
drop_table_def -> DROP      TABLE exists table_list                  : {'drop table', {'tables', '$4'}, '$3', {},   []}.
drop_table_def -> DROP      TABLE exists table_list restrict_cascade : {'drop table', {'tables', '$4'}, '$3', '$5', []}.
drop_table_def -> DROP NAME TABLE        table_list                  : {'drop table', {'tables', '$4'}, {},   {},   unwrap('$2')}.
drop_table_def -> DROP NAME TABLE        table_list restrict_cascade : {'drop table', {'tables', '$4'}, {},   '$5', unwrap('$2')}.
drop_table_def -> DROP NAME TABLE exists table_list                  : {'drop table', {'tables', '$5'}, '$4', {},   unwrap('$2')}.
drop_table_def -> DROP NAME TABLE exists table_list restrict_cascade : {'drop table', {'tables', '$5'}, '$4', '$6', unwrap('$2')}.

drop_role_def -> DROP ROLE NAME : {'drop role', unwrap_bin('$3')}.

drop_index_def -> DROP INDEX            FROM table : {'drop index', {},   '$4'}.
drop_index_def -> DROP INDEX index_name            : {'drop index', '$3', []}.
drop_index_def -> DROP INDEX index_name FROM table : {'drop index', '$3', '$5'}.

create_index_def -> CREATE                   INDEX            ON table_alias                                                         : {'create index', {},   {},   '$4', [],   {},   {}}.
create_index_def -> CREATE                   INDEX            ON table_alias                                     create_index_filter : {'create index', {},   {},   '$4', [],   {},   '$5'}.
create_index_def -> CREATE                   INDEX            ON table_alias                   create_index_norm                     : {'create index', {},   {},   '$4', [],   '$5', {}}.
create_index_def -> CREATE                   INDEX            ON table_alias                   create_index_norm create_index_filter : {'create index', {},   {},   '$4', [],   '$5', '$6'}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec                                       : {'create index', {},   {},   '$4', '$5', {},   {}}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec                   create_index_filter : {'create index', {},   {},   '$4', '$5', {},   '$6'}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec create_index_norm                     : {'create index', {},   {},   '$4', '$5', '$6', {}}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', {},   {},   '$4', '$5', '$6', '$7'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                                                         : {'create index', {},   '$3', '$5', [],   {},   {}}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                                     create_index_filter : {'create index', {},   '$3', '$5', [],   {},   '$6'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                   create_index_norm                     : {'create index', {},   '$3', '$5', [],   '$6', {}}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                   create_index_norm create_index_filter : {'create index', {},   '$3', '$5', [],   '$6', '$7'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec                                       : {'create index', {},   '$3', '$5', '$6', {},   {}}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec                   create_index_filter : {'create index', {},   '$3', '$5', '$6', {},   '$7'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', {},   '$3', '$5', '$6', '$7', '$8'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec create_index_norm                     : {'create index', {},   '$3', '$5', '$6', '$7', {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                                                         : {'create index', '$2', {},   '$5', [],   {},   {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                                     create_index_filter : {'create index', '$2', {},   '$5', [],   {},   '$6'}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                   create_index_norm                     : {'create index', '$2', {},   '$5', [],   '$6', {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                   create_index_norm create_index_filter : {'create index', '$2', {},   '$5', [],   '$6', '$7'}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec                                       : {'create index', '$2', {},   '$5', '$6', {},   {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec                   create_index_filter : {'create index', '$2', {},   '$5', '$6', {},   '$7'}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec create_index_norm                     : {'create index', '$2', {},   '$5', '$6', '$7', {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', '$2', {},   '$5', '$6', '$7', '$8'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                                                         : {'create index', '$2', '$4', '$6', [],   {},   {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                                     create_index_filter : {'create index', '$2', '$4', '$6', [],   {},   '$7'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                   create_index_norm                     : {'create index', '$2', '$4', '$6', [],   '$7', {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                   create_index_norm create_index_filter : {'create index', '$2', '$4', '$6', [],   '$7', '$8'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec                                       : {'create index', '$2', '$4', '$6', '$7', {},   {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec                   create_index_filter : {'create index', '$2', '$4', '$6', '$7', {},   '$8'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec create_index_norm                     : {'create index', '$2', '$4', '$6', '$7', '$8', {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', '$2', '$4', '$6', '$7', '$8', '$9'}.

create_index_opts -> BITMAP  : bitmap.
create_index_opts -> KEYLIST : keylist.
create_index_opts -> HASHMAP : hashmap.
create_index_opts -> UNIQUE  : unique.

index_name -> NAME          : unwrap_bin('$1').
index_name -> NAME '.' NAME : list_to_binary([unwrap('$1'), ".", unwrap('$3')]).

create_index_spec -> '(' create_index_spec_items ')' : '$2'.

create_index_spec_items -> create_index_spec_column                             : ['$1'].
create_index_spec_items -> create_index_spec_column ',' create_index_spec_items : ['$1' | '$3'].

create_index_spec_column -> NAME      : unwrap_bin('$1').
create_index_spec_column -> NAME JSON : jpparse(list_to_binary([unwrap('$1'),unwrap('$2')])).

create_index_norm -> NORM_WITH STRING : {norm, unwrap_bin('$2')}.

create_index_filter -> FILTER_WITH STRING : {filter, unwrap_bin('$2')}.

create_opts ->           tbl_type : '$1'.
create_opts -> tbl_scope          : '$1'.
create_opts -> tbl_scope tbl_type : '$1' ++ '$2'.

tbl_scope -> LOCAL   : [{scope, <<"local">>}].
tbl_scope -> CLUSTER : [{scope, <<"cluster">>}].
tbl_scope -> SCHEMA  : [{scope, <<"schema">>}].

tbl_type -> SET         : [{type, <<"set">>}].
tbl_type -> ORDERED_SET : [{type, <<"ordered_set">>}].
tbl_type -> BAG         : [{type, <<"bag">>}].
tbl_type -> NAME        : [{type, unwrap_bin('$1')}].

alter_user_def -> ALTER USER user_list proxy_clause : {'alter user', '$3', '$4'}.
alter_user_def -> ALTER USER NAME spec_list         : {'alter user', unwrap_bin('$3'), {spec, '$4'}}.
alter_user_def -> ALTER USER NAME NAME NAME         : {'alter user', unwrap_bin('$3'), {'spec', [case {string:to_lower(unwrap('$4')), string:to_lower(unwrap('$5'))} of
                                                                                                     {"account", "lock"} -> {account, lock};
                                                                                                     {"account", "unlock"} -> {account, unlock};
                                                                                                     {"password", "expire"} -> {password, expire};
                                                                                                     Unknown -> exit({invalid_option, Unknown})
                                                                                                 end]
                                                                                       }
                                                      }.

drop_user_def -> DROP USER NAME         : {'drop user', unwrap_bin('$3'), []}.
drop_user_def -> DROP USER NAME CASCADE : {'drop user', unwrap_bin('$3'), ['cascade']}.

user_list -> NAME               : [unwrap_bin('$1')].
user_list -> NAME ',' user_list : [unwrap_bin('$1') | '$3'].

proxy_clause -> GRANT  CONNECT THROUGH ENTERPRISE USERS : {'grant connect', 'enterprise users'}.
proxy_clause -> GRANT  CONNECT THROUGH db_user_proxy    : {'grant connect', '$4'}.
proxy_clause -> REVOKE CONNECT THROUGH ENTERPRISE USERS : {'revoke connect', 'enterprise users'}.
proxy_clause -> REVOKE CONNECT THROUGH db_user_proxy    : {'revoke connect', '$4'}.

db_user_proxy -> proxy_with                : '$1'.
db_user_proxy ->            proxy_auth_req : '$1'.
db_user_proxy -> proxy_with proxy_auth_req : {'$1', '$2'}.

proxy_with -> WITH NO ROLES                  : 'with no roles'.
proxy_with -> WITH ROLE            role_list : {'with role', '$3'}.
proxy_with -> WITH ROLE ALL EXCEPT role_list : {'with role all except', '$5'}.

proxy_auth_req -> AUTHENTICATION REQUIRED : 'authentication required'.

spec_list -> spec_item           : ['$1'].
spec_list -> spec_item spec_list : ['$1'|'$2'].

spec_item -> identified : '$1'.
spec_item -> user_opt   : '$1'.
spec_item -> user_role  : '$1'.

user_role -> DEFAULT ROLE ALL                  : 'default role all'.
user_role -> DEFAULT ROLE ALL EXCEPT role_list : {'default role all except', '$5'}.
user_role -> DEFAULT ROLE NONE                 : 'default role none'.
user_role -> DEFAULT ROLE            role_list : {'default role', '$3'}.

role_list -> NAME               : [unwrap_bin('$1')].
role_list -> NAME ',' role_list : [unwrap_bin('$1') | '$3'].

identified -> IDENTIFIED            BY NAME : {'identified by',       unwrap_bin('$3')}.
identified -> IDENTIFIED EXTERNALLY         : {'identified extern',   {}}.
identified -> IDENTIFIED EXTERNALLY AS NAME : {'identified extern',   unwrap_bin('$4')}.
identified -> IDENTIFIED GLOBALLY           : {'identified globally', {}}.
identified -> IDENTIFIED GLOBALLY   AS NAME : {'identified globally', unwrap_bin('$4')}.

user_opts_list -> user_opt                : ['$1'].
user_opts_list -> user_opt user_opts_list : ['$1'] ++ '$2'.

user_opt -> DEFAULT   TABLESPACE NAME : [{'default tablespace', unwrap_bin('$3')}].
user_opt -> TEMPORARY TABLESPACE NAME : [{'temporary tablespace', unwrap_bin('$3')}].
user_opt -> quota_list                : [{quotas, '$1'}].
user_opt -> PROFILE NAME              : [{profile, unwrap_bin('$2')}].

quota_list -> quota            : ['$1'].
quota_list -> quota quota_list : ['$1'] ++ '$2'.

quota -> QUOTA UNLIMITED   ON NAME : {'unlimited on', unwrap_bin('$4')}.
quota -> QUOTA INTNUM      ON NAME : {limited, unwrap_bin('$2'), <<"">>,           unwrap_bin('$4')}.
quota -> QUOTA INTNUM NAME ON NAME : {limited, unwrap_bin('$2'), unwrap_bin('$3'), unwrap_bin('$5')}.

table_list ->                table :         ['$1'].
table_list -> table_list ',' table : '$1' ++ ['$3'].

exists -> IF EXISTS : 'exists'.

restrict_cascade -> RESTRICT : 'restrict'.
restrict_cascade -> CASCADE  : 'cascade'.

base_table_element_commalist ->                                  base_table_element :         ['$1'].
base_table_element_commalist -> base_table_element_commalist ',' base_table_element : '$1' ++ ['$3'].

base_table_element -> column_def           : '$1'.
base_table_element -> table_constraint_def : '$1'.

column_def -> column data_type                 : {'$1', '$2', []}.
column_def -> column data_type column_def_list : {'$1', '$2', '$3'}.

column_def_list ->                 column_def_opt : ['$1'].
column_def_list -> column_def_list column_def_opt : '$1' ++ ['$2'].

column_def_opt -> NOT NULLX                                 : 'not null'.
column_def_opt -> NOT NULLX UNIQUE                          : 'not null unique'.
column_def_opt -> NOT NULLX PRIMARY KEY                     : 'not null primary key'.
column_def_opt -> DEFAULT function_ref                      : {default, '$2'}.
column_def_opt -> DEFAULT literal                           : {default, '$2'}.
column_def_opt -> DEFAULT NAME                              : {default, unwrap_bin('$2')}.
column_def_opt -> DEFAULT NULLX                             : {default, 'null'}.
column_def_opt -> DEFAULT USER                              : {default, 'user'}.
column_def_opt -> CHECK '(' search_condition ')'            : {check, '$3'}.
column_def_opt -> REFERENCES table                          : {ref, '$2'}.
column_def_opt -> REFERENCES table '(' column_commalist ')' : {ref, {'$2', '$4'}}.

table_constraint_def ->                 UNIQUE      '(' column_commalist ')'                                           : {unique,        [],               '$3'}.
table_constraint_def ->                 PRIMARY KEY '(' column_commalist ')'                                           : {'primary key', [],               '$4'}.
table_constraint_def ->                 FOREIGN KEY '(' column_commalist ')' REFERENCES table                          : {'foreign key', [],               '$4', {'ref', '$7'}}.
table_constraint_def ->                 FOREIGN KEY '(' column_commalist ')' REFERENCES table '(' column_commalist ')' : {'foreign key', [],               '$4', {'ref', {'$7', '$9'}}}.
table_constraint_def ->                 CHECK '(' search_condition ')'                                                 : {check,         [],               '$3'}.
table_constraint_def -> CONSTRAINT NAME UNIQUE      '(' column_commalist ')'                                           : {unique,        unwrap_bin('$2'), '$5'}.
table_constraint_def -> CONSTRAINT NAME PRIMARY KEY '(' column_commalist ')'                                           : {'primary key', unwrap_bin('$2'), '$6'}.
table_constraint_def -> CONSTRAINT NAME FOREIGN KEY '(' column_commalist ')' REFERENCES table                          : {'foreign key', unwrap_bin('$2'), '$6', {'ref', '$9'}}.
table_constraint_def -> CONSTRAINT NAME FOREIGN KEY '(' column_commalist ')' REFERENCES table '(' column_commalist ')' : {'foreign key', unwrap_bin('$2'), '$6', {'ref', {'$9', '$11'}}}.
table_constraint_def -> CONSTRAINT NAME CHECK '(' search_condition ')'                                                 : {check,         unwrap_bin('$2'), '$5'}.

column_commalist -> column                      : ['$1'].
column_commalist -> column ',' column_commalist : ['$1' | '$3'].

view_def -> CREATE VIEW table                          AS query_spec                   : {'create view', '$3', [],   {as, '$5', []}}.
view_def -> CREATE VIEW table                          AS query_spec WITH CHECK OPTION : {'create view', '$3', [],   {as, '$5', "with check option"}}.
view_def -> CREATE VIEW table '(' column_commalist ')' AS query_spec                   : {'create view', '$3', '$5', {as, '$8', []}}.
view_def -> CREATE VIEW table '(' column_commalist ')' AS query_spec WITH CHECK OPTION : {'create view', '$3', '$5', {as, '$8', "with check option"}}.

grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_revokee_commalist                          : {grant, ['all privileges'], '$4',         {to, '$6'},   ''}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_revokee_commalist                          : {grant, '$2',               '$3',         {to, '$5'},   ''}.
grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_identified_by                              : {grant, ['all privileges'], '$4',         {to, ['$6']}, ''}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_identified_by                              : {grant, '$2',               '$3',         {to, ['$5']}, ''}.
grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_revokee_commalist object_with_grant_option : {grant, ['all privileges'], '$4',         {to, '$6'},   '$7'}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_revokee_commalist object_with_grant_option : {grant, '$2',               '$3',         {to, '$5'},   '$6'}.
grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_identified_by     object_with_grant_option : {grant, ['all privileges'], '$4',         {to, ['$6']}, '$7'}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_identified_by     object_with_grant_option : {grant, '$2',               '$3',         {to, ['$5']}, '$6'}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_revokee_commalist                          : {grant, ['all privileges'], {on, <<"">>}, {to, '$5'},   ''}.
grant_def -> GRANT system_privilege_list               TO grantee_revokee_commalist                          : {grant, '$2',               {on, <<"">>}, {to, '$4'},   ''}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_identified_by                              : {grant, ['all privileges'], {on, <<"">>}, {to, ['$5']}, ''}.
grant_def -> GRANT system_privilege_list               TO grantee_identified_by                              : {grant, '$2',               {on, <<"">>}, {to, ['$4']}, ''}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_revokee_commalist system_with_grant_option : {grant, ['all privileges'], {on, <<"">>}, {to, '$5'},   '$6'}.
grant_def -> GRANT system_privilege_list               TO grantee_revokee_commalist system_with_grant_option : {grant, '$2',               {on, <<"">>}, {to, '$4'},   '$5'}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_identified_by     system_with_grant_option : {grant, ['all privileges'], {on, <<"">>}, {to, ['$5']}, '$6'}.
grant_def -> GRANT system_privilege_list               TO grantee_identified_by     system_with_grant_option : {grant, '$2',               {on, <<"">>}, {to, ['$4']}, '$5'}.

revoke_def -> REVOKE ALL PRIVILEGES        on_obj_clause FROM grantee_revokee_commalist                           : {revoke, ['all privileges'], '$4',         {from, '$6'}, ''}.
revoke_def -> REVOKE object_privilege_list on_obj_clause FROM grantee_revokee_commalist                           : {revoke, '$2',               '$3',         {from, '$5'}, ''}.
revoke_def -> REVOKE ALL PRIVILEGES        on_obj_clause FROM grantee_revokee_commalist object_with_revoke_option : {revoke, ['all privileges'], '$4',         {from, '$6'}, '$7'}.
revoke_def -> REVOKE object_privilege_list on_obj_clause FROM grantee_revokee_commalist object_with_revoke_option : {revoke, '$2',               '$3',         {from, '$5'}, '$6'}.
revoke_def -> REVOKE ALL PRIVILEGES                      FROM grantee_revokee_commalist                           : {revoke, ['all privileges'], {on, <<"">>}, {from, '$5'}, ''}.
revoke_def -> REVOKE system_privilege_list               FROM grantee_revokee_commalist                           : {revoke, '$2',               {on, <<"">>}, {from, '$4'}, ''}.

grantee_identified_by -> NAME IDENTIFIED BY STRING : {'identified by', unwrap_bin('$1'), unwrap_bin('$4')}.

grantee_revokee -> NAME   : unwrap_bin('$1').
grantee_revokee -> PUBLIC : 'public'.

grantee_revokee_commalist ->                               grantee_revokee :         ['$1'].
grantee_revokee_commalist -> grantee_revokee_commalist ',' grantee_revokee : '$1' ++ ['$3'].

object_privilege -> ALL            : 'all'.
object_privilege -> ALTER          : 'alter'.
object_privilege -> DELETE         : 'delete'.
object_privilege -> EXECUTE        : 'execute'.
object_privilege -> INDEX          : 'index'.
object_privilege -> INSERT         : 'insert'.
object_privilege -> REFERENCES     : 'references'.
object_privilege -> SELECT         : 'select'.
object_privilege -> UPDATE         : 'update'.

object_privilege_list -> object_privilege                           : ['$1'].
object_privilege_list -> object_privilege ',' object_privilege_list : ['$1'|'$3'].

object_with_grant_option -> WITH GRANT     OPTION : 'with grant option'.
object_with_grant_option -> WITH HIERARCHY OPTION : 'with hierarchy option'.

object_with_revoke_option -> CASCADE CONSTRAINTS : 'cascade constraints'.
object_with_revoke_option -> FORCE               : 'force'.

on_obj_clause -> ON DIRECTORY NAME : {'on directory', unwrap_bin('$3')}.
on_obj_clause -> ON table          : {on,            '$2'}.

system_privilege -> ADMIN                        : 'admin'.
system_privilege -> ALTER ANY INDEX              : 'alter any index'.
system_privilege -> ALTER ANY MATERIALIZED VIEW  : 'alter any materialized view'.
system_privilege -> ALTER ANY TABLE              : 'alter any table'.
system_privilege -> ALTER ANY VIEW               : 'alter any view'.
system_privilege -> CREATE ANY INDEX             : 'create any index'.
system_privilege -> CREATE ANY MATERIALIZED VIEW : 'create any materialized view'.
system_privilege -> CREATE ANY TABLE             : 'create any table'.
system_privilege -> CREATE ANY VIEW              : 'create any view'.
system_privilege -> CREATE MATERIALIZED VIEW     : 'create materialized view'.
system_privilege -> CREATE TABLE                 : 'create table'.
system_privilege -> CREATE VIEW                  : 'create view'.
system_privilege -> DELETE ANY TABLE             : 'delete any table'.
system_privilege -> DROP ANY INDEX               : 'drop any index'.
system_privilege -> DROP ANY MATERIALIZED VIEW   : 'drop any materialized view'.
system_privilege -> DROP ANY TABLE               : 'drop any table'.
system_privilege -> DROP ANY VIEW                : 'drop any view'.
system_privilege -> INSERT ANY TABLE             : 'insert any table'.
system_privilege -> SELECT ANY TABLE             : 'select any table'.
system_privilege -> UPDATE ANY TABLE             : 'update any table'.
system_privilege -> NAME                         : strl2atom(['$1']).

system_privilege_list -> system_privilege                           : ['$1'].
system_privilege_list -> system_privilege ',' system_privilege_list : ['$1'|'$3'].

system_with_grant_option -> WITH ADMIN    OPTION : 'with admin option'.
system_with_grant_option -> WITH DELEGATE OPTION : 'with delegate option'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cursor definition
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cursor_def -> CURSOR cursor IS query_exp : {cursor_def, '$2', '$4'}.

order_by_clause -> ORDER BY ordering_spec_commalist : {'order by', '$3'}.

ordering_spec_commalist ->                             ordering_spec :         ['$1'].
ordering_spec_commalist -> ordering_spec_commalist ',' ordering_spec : '$1' ++ ['$3'].

ordering_spec -> scalar_exp          : {'$1', <<>>}.
ordering_spec -> scalar_exp asc_desc : {'$1', '$2'}.

asc_desc -> ASC  : <<"asc">>.
asc_desc -> DESC : <<"desc">>.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% manipulative statements
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

manipulative_statement -> close_statement             : '$1'.
manipulative_statement -> commit_statement            : '$1'.
manipulative_statement -> delete_statement_positioned : '$1'.
manipulative_statement -> delete_statement_searched   : '$1'.
manipulative_statement -> fetch_statement             : '$1'.
manipulative_statement -> insert_statement            : '$1'.
manipulative_statement -> open_statement              : '$1'.
manipulative_statement -> rollback_statement          : '$1'.
manipulative_statement -> select_statement            : '$1'.
manipulative_statement -> update_statement_positioned : '$1'.
manipulative_statement -> update_statement_searched   : '$1'.
manipulative_statement -> create_table_def            : '$1'.
manipulative_statement -> create_role_def             : '$1'.
manipulative_statement -> create_index_def            : '$1'.
manipulative_statement -> create_user_def             : '$1'.
manipulative_statement -> drop_role_def               : '$1'.
manipulative_statement -> drop_table_def              : '$1'.
manipulative_statement -> drop_index_def              : '$1'.
manipulative_statement -> alter_user_def              : '$1'.
manipulative_statement -> drop_user_def               : '$1'.
manipulative_statement -> view_def                    : '$1'.
manipulative_statement -> truncate_table              : '$1'.
manipulative_statement -> grant_def                   : '$1'.
manipulative_statement -> revoke_def                  : '$1'.

truncate_table -> TRUNCATE TABLE table                      : {'truncate table', '$3', {},   {}}.
truncate_table -> TRUNCATE TABLE table              storage : {'truncate table', '$3', {},   '$4'}.
truncate_table -> TRUNCATE TABLE table materialized         : {'truncate table', '$3', '$4', {}}.
truncate_table -> TRUNCATE TABLE table materialized storage : {'truncate table', '$3', '$4', '$5'}.

materialized -> PRESERVE MATERIALIZED VIEW LOG : {'materialized view log', preserve}.
materialized -> PURGE    MATERIALIZED VIEW LOG : {'materialized view log', purge}.

storage ->  DROP  STORAGE : {storage, drop}.
storage ->  REUSE STORAGE : {storage, reuse}.

close_statement -> CLOSE cursor : {close, '$2'}.

commit_statement -> COMMIT      : 'commit'.
commit_statement -> COMMIT WORK : 'commit work'.

delete_statement_positioned -> DELETE FROM table_dblink WHERE CURRENT OF cursor           : {delete, '$3',{where_current_of, '$7'}, {returning, {}}}.
delete_statement_positioned -> DELETE FROM table_dblink WHERE CURRENT OF cursor returning : {delete, '$3',{where_current_of, '$7'}, '$8'}.

delete_statement_searched -> DELETE FROM table_dblink                        : {delete, '$3', [],   {returning, {}}}.
delete_statement_searched -> DELETE FROM table_dblink              returning : {delete, '$3', [],   '$4'}.
delete_statement_searched -> DELETE FROM table_dblink where_clause           : {delete, '$3', '$4', {returning, {}}}.
delete_statement_searched -> DELETE FROM table_dblink where_clause returning : {delete, '$3', '$4', '$5'}.

fetch_statement -> FETCH cursor INTO target_commalist : {fetch, '$2', {into, '$4'}}.

insert_statement -> INSERT INTO table_dblink                                                         : {insert, '$3', {},           {},   {returning, {}}}.
insert_statement -> INSERT INTO table_dblink                                               returning : {insert, '$3', {},           {},   '$4'}.
insert_statement -> INSERT INTO table_dblink                          values_or_query_spec           : {insert, '$3', {cols, []},   '$4', {returning, {}}}.
insert_statement -> INSERT INTO table_dblink                          values_or_query_spec returning : {insert, '$3', {cols, []},   '$4', '$5'}.
insert_statement -> INSERT INTO table_dblink '(' column_commalist ')' values_or_query_spec           : {insert, '$3', {cols, '$5'}, '$7', {returning, {}}}.
insert_statement -> INSERT INTO table_dblink '(' column_commalist ')' values_or_query_spec returning : {insert, '$3', {cols, '$5'}, '$7', '$8'}.

values_or_query_spec -> VALUES '(' insert_atom_commalist ')' : {values, '$3'}.
values_or_query_spec -> query_spec                           : '$1'.

insert_atom_commalist ->                           insert_atom :         ['$1'].
insert_atom_commalist -> insert_atom_commalist ',' insert_atom : '$1' ++ ['$3'].

insert_atom -> scalar_opt_as_exp : '$1'.

open_statement -> OPEN cursor : {open, '$2'}.

rollback_statement -> ROLLBACK      : 'rollback'.
rollback_statement -> ROLLBACK WORK : 'rollback work'.

select_statement -> query_exp : '$1'.

hint -> HINT : {hints, unwrap_bin('$1')}.

all_distinct -> ALL      : {opt, <<"all">>}.
all_distinct -> DISTINCT : {opt, <<"distinct">>}.

update_statement_positioned -> UPDATE table_dblink SET assignment_commalist WHERE CURRENT OF cursor           : {update, '$2', {set, '$4'}, {where_current_of, '$8'}, {returning, {}}}.
update_statement_positioned -> UPDATE table_dblink SET assignment_commalist WHERE CURRENT OF cursor returning : {update, '$2', {set, '$4'}, {where_current_of, '$8'}, '$9'}.

assignment_commalist ->                          assignment :         ['$1'].
assignment_commalist -> assignment_commalist ',' assignment : '$1' ++ ['$3'].

assignment -> column '=' scalar_opt_as_exp : {'=', '$1', '$3'}.

update_statement_searched -> UPDATE table_dblink SET assignment_commalist                        : {update, '$2', {set, '$4'}, [],   {returning, {}}}.
update_statement_searched -> UPDATE table_dblink SET assignment_commalist              returning : {update, '$2', {set, '$4'}, [],   '$5'}.
update_statement_searched -> UPDATE table_dblink SET assignment_commalist where_clause           : {update, '$2', {set, '$4'}, '$5', {returning, {}}}.
update_statement_searched -> UPDATE table_dblink SET assignment_commalist where_clause returning : {update, '$2', {set, '$4'}, '$5', '$6'}.

target_commalist ->                      target :         ['$1'].
target_commalist -> target_commalist ',' target : '$1' ++ ['$3'].

target -> NAME          : unwrap_bin('$1').
target -> parameter_ref : '$1'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query expressions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query_exp ->                     query_term : '$1'.
query_exp -> query_exp UNION     query_term : {union,       '$1', '$3'}.
query_exp -> query_exp UNION ALL query_term : {'union all', '$1', '$4'}.
query_exp -> query_exp INTERSECT query_term : {intersect,   '$1', '$3'}.
query_exp -> query_exp MINUS     query_term : {minus,       '$1', '$3'}.

returning -> RETURNING selection INTO selection : {returning, '$2', '$4'}.
returning -> RETURN    selection INTO selection : {return,    '$2', '$4'}.

query_term ->     query_spec          : '$1'.
query_term -> '(' query_exp  ')'      : '$2'.
query_term -> '(' query_exp  ')' JSON : {'$2', jpparse(list_to_binary([unwrap('$4')])), '('}.

query_spec -> SELECT                   selection      table_exp : {select,
                                                                   [{fields, '$2'}] ++
                                                                   '$3'}.
query_spec -> SELECT                   selection into table_exp : {select,
                                                                   [{fields, '$2'}] ++
                                                                   if '$3' == {} -> []; true -> [{into, '$3'}] end ++
                                                                   '$4'}.
query_spec -> SELECT      all_distinct selection      table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   '$4'}.
query_spec -> SELECT      all_distinct selection into table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   if '$4' == {} -> []; true -> [{into, '$4'}] end ++
                                                                   '$5'}.
query_spec -> SELECT hint              selection      table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   '$4'}.
query_spec -> SELECT hint              selection into table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   if '$4' == {} -> []; true -> [{into, '$4'}] end ++
                                                                   '$5'}.
query_spec -> SELECT hint all_distinct selection      table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   if '$3' == {} -> []; true -> ['$3'] end ++
                                                                   [{fields, '$4'}] ++
                                                                   '$5'}.
query_spec -> SELECT hint all_distinct selection into table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   if '$3' == {} -> []; true -> ['$3'] end ++
                                                                   [{fields, '$4'}] ++
                                                                   if '$5' == {} -> []; true -> [{into, '$5'}] end ++
                                                                   '$6'}.

into -> INTO target_commalist : '$2'.

selection -> select_field_commalist : '$1'.

select_field -> case_when_opt_as_exp : ['$1'].
select_field -> scalar_opt_as_exp    : ['$1'].
select_field -> '*'                  : [<<"*">>].

select_field_commalist ->                            select_field :         '$1'.
select_field_commalist -> select_field_commalist ',' select_field : '$1' ++ '$3'.

case_when_opt_as_exp -> case_when_exp         : '$1'.
case_when_opt_as_exp -> case_when_exp    NAME : {as, '$1', unwrap_bin('$2')}.
case_when_opt_as_exp -> case_when_exp AS NAME : {as, '$1', unwrap_bin('$3')}.

case_when_exp -> CASE                   case_when_then_list      END : {'case', <<>>, '$2', {}}.
case_when_exp -> CASE                   case_when_then_list else END : {'case', <<>>, '$2', '$3'}.
case_when_exp -> CASE scalar_opt_as_exp case_when_then_list      END : {'case', '$2', '$3', {}}.
case_when_exp -> CASE scalar_opt_as_exp case_when_then_list else END : {'case', '$2', '$3', '$4'}.
case_when_exp -> '(' case_when_exp ')'                               : '$2'.

case_when_then_list -> case_when_then                     : ['$1'].
case_when_then_list -> case_when_then case_when_then_list : ['$1'|'$2'].

case_when_then -> WHEN search_condition THEN scalar_opt_as_exp : {'$2', '$4'}.

else -> ELSE scalar_opt_as_exp : '$2'.

table_exp -> from_clause                                                                                      : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause                                                                      order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  {having, {}}, '$2'].
table_exp -> from_clause                                                        having_clause                 : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  '$2',         {'order by', []}].
table_exp -> from_clause                                                        having_clause order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  '$2',         '$3'].
table_exp -> from_clause                                        group_by_clause                               : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              {having, {}}, {'order by', []}].
table_exp -> from_clause                                        group_by_clause               order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              {having, {}}, '$3'].
table_exp -> from_clause                                        group_by_clause having_clause                 : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              '$3',         {'order by', []}].
table_exp -> from_clause                                        group_by_clause having_clause order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              '$3',         '$4'].
table_exp -> from_clause              hierarchical_query_clause                                               : ['$1', {where, {}}, '$2',                       {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause                               order_by_clause : ['$1', {where, {}}, '$2',                       {'group by', []},  {having, {}}, '$3'].
table_exp -> from_clause              hierarchical_query_clause                 having_clause                 : ['$1', {where, {}}, '$2',                       {'group by', []},  '$3',         {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause                 having_clause order_by_clause : ['$1', {where, {}}, '$2',                       {'group by', []},  '$3',         '$4'].
table_exp -> from_clause              hierarchical_query_clause group_by_clause                               : ['$1', {where, {}}, '$2',                       '$3',              {having, {}}, {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause group_by_clause               order_by_clause : ['$1', {where, {}}, '$2',                       '$3',              {having, {}}, '$4'].
table_exp -> from_clause              hierarchical_query_clause group_by_clause having_clause                 : ['$1', {where, {}}, '$2',                       '$3',              '$4',         {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause group_by_clause having_clause order_by_clause : ['$1', {where, {}}, '$2',                       '$3',              '$4',         '$5'].
table_exp -> from_clause where_clause                                                                         : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause                                                         order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  {having, {}}, '$3'].
table_exp -> from_clause where_clause                                           having_clause                 : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  '$3', {'order by', []}].
table_exp -> from_clause where_clause                                           having_clause order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  '$3',         '$4'].
table_exp -> from_clause where_clause                           group_by_clause                               : ['$1', '$2',        {'hierarchical query', {}}, '$3',              {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause                           group_by_clause               order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, '$3',              {having, {}}, '$4'].
table_exp -> from_clause where_clause                           group_by_clause having_clause                 : ['$1', '$2',        {'hierarchical query', {}}, '$3',              '$4',         {'order by', []}].
table_exp -> from_clause where_clause                           group_by_clause having_clause order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, '$3',              '$4',         '$5'].
table_exp -> from_clause where_clause hierarchical_query_clause                                               : ['$1', '$2',        '$3',                       {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause                               order_by_clause : ['$1', '$2',        '$3',                       {'group by', []},  {having, {}}, '$4'].
table_exp -> from_clause where_clause hierarchical_query_clause                 having_clause                 : ['$1', '$2',        '$3',                       {'group by', []},  '$4', {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause                 having_clause order_by_clause : ['$1', '$2',        '$3',                       {'group by', []},  '$4',         '$5'].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause                               : ['$1', '$2',        '$3',                       '$4',              {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause               order_by_clause : ['$1', '$2',        '$3',                       '$4',              {having, {}}, '$5'].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause having_clause                 : ['$1', '$2',        '$3',                       '$4',              '$5',         {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause having_clause order_by_clause : ['$1', '$2',        '$3',                       '$4',              '$5',         '$6'].

from_clause -> FROM from_column_commalist : {from, '$2'}.

from_column -> table_ref           : ['$1'].
from_column -> '(' join_clause ')' : ['$2'].
from_column ->     join_clause     : ['$1'].

from_column_commalist ->                           from_column :        '$1'.
from_column_commalist -> from_column_commalist ',' from_column : '$1'++ '$3'.

join_clause -> table_ref join_list : {'$1', '$2'}.

join -> inner_cross_join : '$1'.
join -> outer_join       : '$1'.

join_list ->           join :        ['$1'].
join_list -> join_list join : '$1'++ ['$2'].

inner_cross_join ->               JOIN join_ref join_on_or_using_clause : {join,               '$2', '$3'}.
inner_cross_join -> CROSS         JOIN join_ref                         : {cross_join,         '$3'}.
inner_cross_join -> INNER         JOIN join_ref join_on_or_using_clause : {join_inner,         '$3', '$4'}.
inner_cross_join -> NATURAL       JOIN join_ref                         : {natural_join,       '$3'}.
inner_cross_join -> NATURAL INNER JOIN join_ref                         : {natural_inner_join, '$4'}.

join_on_or_using_clause -> ON search_condition                  : {on,    '$2'}.
join_on_or_using_clause -> USING '(' select_field_commalist ')' : {using, '$3'}.

% ----------------------------------------------------------------------------------------------- {{join_type, partition, opt_natural} ... }
outer_join ->                                outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$1', {},   {}},      '$3', {},   '$4'}.
outer_join ->                                outer_join_type JOIN join_ref                                                : {{'$1', {},   {}},      '$3', {},   {}}.
outer_join ->                                outer_join_type JOIN join_ref query_partition_clause                         : {{'$1', {},   {}},      '$3', '$4', {}}.
outer_join ->                                outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$1', {},   {}},      '$3', '$4', '$5'}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$2', {},   natural}, '$4', {},   '$5'}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref                                                : {{'$2', {},   natural}, '$4', {},   {}}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref query_partition_clause                         : {{'$2', {},   natural}, '$4', '$5', {}}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$2', {},   natural}, '$4', '$5', '$6'}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$2', '$1', {}},      '$4', {},   '$5'}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref                                                : {{'$2', '$1', {}},      '$4', {},   {}}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref query_partition_clause                         : {{'$2', '$1', {}},      '$4', '$5', {}}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$2', '$1', {}},      '$4', '$5', '$6'}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$3', '$1', natural}, '$5', {},   '$6'}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref                                                : {{'$3', '$1', natural}, '$5', {},   {}}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref query_partition_clause                         : {{'$3', '$1', natural}, '$5', '$6', {}}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$3', '$1', natural}, '$5', '$6', '$7'}.
% -----------------------------------------------------------------------------------------------

query_partition_clause -> PARTITION BY     scalar_exp_commalist     : {partition_by, '$3'} .
query_partition_clause -> PARTITION BY '(' scalar_exp_commalist ')' : {partition_by, '$4'}.

outer_join_type -> FULL        : full.
outer_join_type -> FULL  OUTER : full_outer.
outer_join_type -> LEFT        : left.
outer_join_type -> LEFT  OUTER : left_outer.
outer_join_type -> RIGHT       : right.
outer_join_type -> RIGHT OUTER : right_outer.

table_ref -> table_dblink    : '$1'.
table_ref -> query_term      : '$1'.
table_ref -> query_term NAME : {as, '$1', unwrap_bin('$2')}.

join_ref -> table_dblink    : '$1'.
join_ref -> query_term      : '$1'.
join_ref -> query_term NAME : {as, '$1', unwrap_bin('$2')}.

hierarchical_query_clause -> START WITH         search_condition CONNECT BY         search_condition : {'hierarchical query', {{'start with', '$3'},       {'connect by', <<>>, '$6'}}}.
hierarchical_query_clause -> START WITH         search_condition CONNECT BY nocycle search_condition : {'hierarchical query', {{'start with', '$3'},       {'connect by', '$6', '$7'}}}.
hierarchical_query_clause -> CONNECT BY         search_condition START WITH         search_condition : {'hierarchical query', {{'connect by', <<>>, '$3'}, {'start with', '$6'}}}.
hierarchical_query_clause -> CONNECT BY nocycle search_condition START WITH         search_condition : {'hierarchical query', {{'connect by', '$3', '$4'}, {'start with', '$7'}}}.

nocycle -> NOCYCLE : <<"nocycle">>.

where_clause -> WHERE search_condition : {where, '$2'}.

group_by_clause  -> GROUP BY column_ref_commalist : {'group by', '$3'}.

column_ref_commalist ->                          column_ref   :         ['$1'].
column_ref_commalist ->                          function_ref :         ['$1'].
column_ref_commalist -> column_ref_commalist ',' column_ref   : '$1' ++ ['$3'].
column_ref_commalist -> column_ref_commalist ',' function_ref : '$1' ++ ['$3'].

having_clause -> HAVING search_condition : {having, '$2'}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% search conditions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search_condition -> search_condition OR  search_condition : {'or',  '$1', '$3'}.
search_condition -> search_condition AND search_condition : {'and', '$1', '$3'}.
search_condition -> NOT search_condition                  : {'not', '$2'}.
search_condition -> '(' search_condition ')'              : '$2'.
search_condition -> predicate                             : '$1'.

predicate -> comparison_predicate : '$1'.
predicate -> between_predicate    : '$1'.
predicate -> like_predicate       : '$1'.
predicate -> test_for_null        : '$1'.
predicate -> in_predicate         : '$1'.
predicate -> all_or_any_predicate : '$1'.
predicate -> existence_test       : '$1'.

comparison_predicate -> scalar_opt_as_exp                            : '$1'.
comparison_predicate ->       scalar_exp '='        PRIOR scalar_exp : {'=',          '$1',          {prior, '$4'}}.
comparison_predicate ->       scalar_exp COMPARISON PRIOR scalar_exp : {unwrap('$2'), '$1',          {prior, '$4'}}.
comparison_predicate -> PRIOR scalar_exp '='              scalar_exp : {'=',          {prior, '$2'}, '$4'}.
comparison_predicate -> PRIOR scalar_exp COMPARISON       scalar_exp : {unwrap('$3'), {prior, '$2'}, '$4'}.

between_predicate -> scalar_exp     BETWEEN scalar_exp AND scalar_exp:         {between, '$1', '$3', '$5'}.
between_predicate -> scalar_exp not_between scalar_exp AND scalar_exp: {'not', {between, '$1', '$3', '$5'}}.

not_between -> NOT BETWEEN : 'not between'.

like_predicate -> scalar_exp     LIKE scalar_exp        :         {like, '$1', '$3', <<>>}.
like_predicate -> scalar_exp     LIKE scalar_exp escape :         {like, '$1', '$3', '$4'}.
like_predicate -> scalar_exp not_like scalar_exp        : {'not', {like, '$1', '$3', <<>>}}.
like_predicate -> scalar_exp not_like scalar_exp escape : {'not', {like, '$1', '$3', '$4'}}.

not_like -> NOT LIKE  : 'not like'.

escape -> ESCAPE atom : '$2'.

test_for_null -> scalar_exp is_null     :         {'is', '$1', <<"null">>}.
test_for_null -> scalar_exp is_not_null : {'not', {'is', '$1', <<"null">>}}.

is_not_null -> IS NOT NULLX : 'is not'.

is_null -> IS NULLX : is.

in_predicate -> scalar_exp     IN '(' scalar_exp_commalist ')' :         {in, '$1', {list, '$4'}}.
in_predicate -> scalar_exp     IN '(' subquery ')'             :         {in, '$1', '$4'}.
in_predicate -> scalar_exp not_in '(' scalar_exp_commalist ')' : {'not', {in, '$1', {list, '$4'}}}.
in_predicate -> scalar_exp not_in '(' subquery ')'             : {'not', {in, '$1', '$4'}}.

not_in -> NOT IN : 'not in'.

all_or_any_predicate -> scalar_exp '='        any_all_some subquery : {'=',          '$1', {'$3', ['$4']}}.
all_or_any_predicate -> scalar_exp COMPARISON any_all_some subquery : {unwrap('$2'), '$1', {'$3', ['$4']}}.

any_all_some -> ANY  : any.
any_all_some -> ALL  : all.
any_all_some -> SOME : some.

existence_test -> EXISTS subquery : {exists, '$2'}.

% Optional plus (+) is not supported in table_collection_expression:
%
% The optional plus (+) is relevant if you are joining the TABLE collection expression with
% the parent table. The + creates an outer join of the two, so that the query returns rows
% from the outer table even if the collection expression is null.

table_coll_expr -> TABLE '(' column_ref   ')' : {table_coll_expr, '$3'}.
table_coll_expr -> TABLE '(' function_ref ')' : {table_coll_expr, '$3'}.
table_coll_expr -> TABLE '(' subquery     ')' : {table_coll_expr, '$3'}.

subquery -> query_exp : '$1'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scalar expressions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scalar_opt_as_exp -> scalar_exp                       : '$1'.
scalar_opt_as_exp -> scalar_exp '='        scalar_exp : {'=',          '$1', '$3'}.
scalar_opt_as_exp -> scalar_exp COMPARISON scalar_exp : {unwrap('$2'), '$1', '$3'}.
scalar_opt_as_exp -> scalar_exp    NAME               : {as,           '$1', unwrap_bin('$2')}.
scalar_opt_as_exp -> scalar_exp AS NAME               : {explicit_as,  '$1', unwrap_bin('$3')}.

scalar_exp -> scalar_sub_exp '||' scalar_exp : {'||','$1','$3'}.
scalar_exp -> scalar_sub_exp                 : '$1'.

scalar_sub_exp -> scalar_sub_exp '+'    scalar_sub_exp : {'+','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '-'    scalar_sub_exp : {'-','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '*'    scalar_sub_exp : {'*','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '/'    scalar_sub_exp : {'/','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp 'div'  scalar_sub_exp : {'div','$1','$3'}.
scalar_sub_exp -> unary_add_or_subtract scalar_sub_exp : {'$1','$2'}.
scalar_sub_exp -> NULLX                                : <<"NULL">>.
scalar_sub_exp -> atom                                 : '$1'.
scalar_sub_exp -> subquery                             : '$1'.
scalar_sub_exp -> column_ref                           : '$1'.
scalar_sub_exp -> function_ref                         : '$1'.
scalar_sub_exp -> '(' scalar_sub_exp ')'               : '$2'.
scalar_sub_exp -> '(' scalar_sub_exp ')' JSON          : {'$2', jpparse(list_to_binary([unwrap('$4')])), '('}.

unary_add_or_subtract -> '+' : '+'.
unary_add_or_subtract -> '-' : '-'.

scalar_exp_commalist ->                          scalar_opt_as_exp :         ['$1'].
scalar_exp_commalist -> scalar_exp_commalist ',' scalar_opt_as_exp : '$1' ++ ['$3'].

atom -> parameter_ref : '$1'.
atom -> literal       : '$1'.
atom -> USER          : <<"user">>.

parameter_ref -> parameter                     : '$1'.
parameter_ref -> parameter           parameter : {'$1', '$2'}.
parameter_ref -> parameter INDICATOR parameter : {indicator, '$1', '$3'}.

function_ref -> NAME '.' NAME '.' NAME '(' fun_args ')' : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')]), make_list('$7')}.
function_ref -> NAME '.' NAME '(' fun_args ')'          : {'fun', list_to_binary([unwrap('$1'),".",unwrap('$3')]),make_list('$5')}.
function_ref -> NAME '(' fun_args ')'                   : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> FUNS                                    : {'fun', unwrap_bin('$1'), []}.
function_ref -> FUNS '(' fun_args ')'                   : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> FUNS '(' '*' ')'                        : {'fun', unwrap_bin('$1'), [<<"*">>]}.
function_ref -> FUNS '(' DISTINCT column_ref ')'        : {'fun', unwrap_bin('$1'), [{distinct, '$4'}]}.
function_ref -> FUNS '(' ALL      scalar_exp ')'        : {'fun', unwrap_bin('$1'), [{all,      '$4'}]}.
function_ref -> function_ref JSON                       : {'$1', jpparse(list_to_binary([unwrap('$2')])), []}.

fun_args -> fun_arg              : ['$1'].
fun_args -> fun_arg ',' fun_args : ['$1' | '$3'].

fun_arg -> '(' fun_arg ')'               : '$2'.
fun_arg -> function_ref                  : '$1'.
fun_arg -> column_ref                    : '$1'.
fun_arg -> fun_arg '+' fun_arg           : {'+',  '$1','$3'}.
fun_arg -> fun_arg '-' fun_arg           : {'-',  '$1','$3'}.
fun_arg -> fun_arg '*' fun_arg           : {'*',  '$1','$3'}.
fun_arg -> fun_arg '/' fun_arg           : {'/',  '$1','$3'}.
fun_arg -> fun_arg 'div' fun_arg         : {'div','$1','$3'}.
fun_arg -> fun_arg '||' fun_arg          : {'||', '$1','$3'}.
fun_arg -> unary_add_or_subtract fun_arg : {'$1', '$2'}.
fun_arg -> NULLX                         : <<"NULL">>.
fun_arg -> atom                          : '$1'.
fun_arg -> subquery                      : '$1'.
fun_arg -> fun_arg    NAME               : {as, '$1', unwrap_bin('$2')}.
fun_arg -> fun_arg AS NAME               : {as, '$1', unwrap_bin('$3')}.
fun_arg -> fun_arg '='        fun_arg    : {'=',          '$1', '$3'}.
fun_arg -> fun_arg COMPARISON fun_arg    : {unwrap('$2'), '$1', '$3'}.
fun_arg -> case_when_exp                 : '$1'.

literal -> STRING    : unwrap_bin('$1').
literal -> INTNUM    : unwrap_bin('$1').
literal -> APPROXNUM : unwrap_bin('$1').

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% miscellaneous
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table -> NAME          : unwrap_bin('$1').
table -> NAME '.' NAME : list_to_binary([unwrap('$1'), ".", unwrap('$3')]).
table -> parameter     : '$1'.
table -> STRING        : unwrap_bin('$1').

table_alias -> NAME          NAME : {as, unwrap_bin('$1'),                                unwrap_bin('$2')}.
table_alias -> NAME '.' NAME NAME : {as, list_to_binary([unwrap('$1'),".",unwrap('$3')]), unwrap_bin('$4')}.
table_alias -> parameter     NAME : {as, '$1',                                            unwrap_bin('$2')}.
table_alias -> STRING        NAME : {as, unwrap_bin('$1'),                                unwrap_bin('$2')}.
table_alias -> table              : '$1'.

table_dblink -> NAME          DBLINK      : {    unwrap_bin('$1'),                                                    {dblink, unwrap_bin('$2')}}.
table_dblink -> NAME          DBLINK NAME : {as, unwrap_bin('$1'),                                  unwrap_bin('$3'), {dblink, unwrap_bin('$2')}}.
table_dblink -> NAME '.' NAME DBLINK      : {    list_to_binary([unwrap('$1'), ".", unwrap('$3')]),                   {dblink, unwrap_bin('$4')}}.
table_dblink -> NAME '.' NAME DBLINK NAME : {as, list_to_binary([unwrap('$1'), ".", unwrap('$3')]), unwrap_bin('$5'), {dblink, unwrap_bin('$4')}}.
table_dblink -> parameter     DBLINK      : {    '$1',                                                                {dblink, unwrap_bin('$2')}}.
table_dblink -> parameter     DBLINK NAME : {as, '$1',                                              unwrap_bin('$3'), {dblink, unwrap_bin('$2')}}.
table_dblink -> table_alias               : '$1'.
table_dblink -> table_coll_expr           : '$1'.

column_ref -> NAME                   JSON        : jpparse(list_to_binary([unwrap('$1'),unwrap('$2')])).
column_ref -> NAME '.' NAME          JSON        : jpparse(list_to_binary([unwrap('$1'),".",unwrap('$3'),unwrap('$4')])).
column_ref -> NAME '.' NAME '.' NAME JSON        : jpparse(list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5'),unwrap('$6')])).
column_ref -> NAME                               : unwrap_bin('$1').
column_ref -> NAME '.' NAME                      : list_to_binary([unwrap('$1'),".",unwrap('$3')]).
column_ref -> NAME '.' NAME '.' NAME             : list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5')]).
column_ref -> NAME '(' '+' ')'                   : list_to_binary([unwrap('$1'),"(+)"]).
column_ref -> NAME '.' NAME '(' '+' ')'          : list_to_binary([unwrap('$1'),".",unwrap('$3'),"(+)"]).
column_ref -> NAME '.' NAME '.' NAME '(' '+' ')' : list_to_binary([unwrap('$1'),".",unwrap('$3'),".",unwrap('$5'),"(+)"]).
column_ref -> NAME '.' '*'                       : list_to_binary([unwrap('$1'),".*"]).
column_ref -> NAME '.' NAME '.' '*'              : list_to_binary([unwrap('$1'),".",unwrap('$3'),".*"]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data types
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_type -> STRING                           : unwrap_bin('$1').
data_type -> NAME                             : unwrap_bin('$1').
data_type -> NAME '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> NAME '(' sgn_num ',' sgn_num ')' : {unwrap_bin('$1'), '$3', '$5'}.

sgn_num ->     INTNUM : unwrap_bin('$1').
sgn_num -> '-' INTNUM : list_to_binary(["-",unwrap_bin('$2')]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the various things you can name
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

column -> NAME   : unwrap_bin('$1').
column -> STRING : unwrap_bin('$1').

cursor -> NAME : {cur, unwrap('$1')}.

parameter -> PARAMETER : {param, unwrap_bin('$1')}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% embedded condition things
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

when_action -> GOTO NAME : {goto, unwrap('$2')}.
when_action -> CONTINUE  : 'continue'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% sqlparse.erl: SQL - parser.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

% parser and compiler interface
-export([
    is_reserved/1,
    parsetree/1,
    parsetree_with_tokens/1
]).

-define(NODEBUG, true).

-include("sqlparse.hrl").

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

jpparse(X) ->
    {ok, Pt} = jpparse:parsetree(X),
    Pt.

unwrap({_, _, X}) -> X;
unwrap(X) -> X.

unwrap_bin({_, _, X}) when is_list(X) -> list_to_binary([X]);
unwrap_bin({_, _, X}) when is_atom(X) -> atom_to_binary(X, unicode).

strl2atom(Strs) ->
    list_to_atom(lists:flatten(
        string:join([string:to_lower(unwrap(S)) || S <- Strs], " "))).

make_list(L) when is_list(L) -> L;
make_list(L) -> [L].

%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, [tuple()]}.
parsetree(Sql) ->
    ?D("Start~n Sql: ~p~n", [Sql]),
    case parsetree_with_tokens(Sql) of
        {ok, {ParseTree, _Tokens}} ->
            ?D("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, _Tokens]),
            {ok, ParseTree};
        Error -> Error
    end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree_with_tokens([]) -> {parse_error, invalid_string};
parsetree_with_tokens(<<>>) -> {parse_error, invalid_string};
parsetree_with_tokens(Sql0) ->
    Sql = re:replace(Sql0, "(^[ \r\n]+)|([ \r\n]+$)", "",
        [global, {return, list}]),
    ?D("Start~n Sql: ~p~n", [Sql]),
    [C | _] = lists:reverse(Sql),
    NSql = if C =:= $; -> Sql; true -> string:trim(Sql) ++ ";" end,
    case sql_lex:string(NSql) of
        {ok, Toks, _} ->
            case parse(Toks) of
                {ok, PTree} ->
                    ?D("~n ParseTree: ~p~n Tokens: ~p~n", [PTree, Toks]),
                    {ok, {PTree, Toks}};
                {error, {N, ?MODULE, ErrorTerms}} ->
                    {parse_error, {lists:flatten(
                        [integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error, Error} -> {parse_error, {Error, Toks}}
            end;
        {error, Error, _} -> {lex_error, Error}
    end.

-spec is_reserved(binary() | atom() | list()) -> true | false.
is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
        sql_lex:reserved_keywords()).
