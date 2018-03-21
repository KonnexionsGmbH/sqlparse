%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: REVOKE
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revoke_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"REVOKE a,b,c FROM user1,user2".
"REVOKE ADMIN FROM name_user_1".
"revoke admin from public".
"revoke admin,create any table,something from public".
"revoke admin,create any table,something from public,user_1".
"revoke admin,create any table,something from user_1".
"revoke admin,create any table,something from user_1,user_2".
"revoke admin,create any table,something from user_1,user_2,user_3".
"REVOKE ALL ON name_table FROM name_user_1".
"REVOKE ALL ON schema1.ddTable FROM user1,user2".
"REVOKE ALL ON table_1 FROM name_user_1 CASCADE CONSTRAINTS".
"REVOKE ALL PRIVILEGES FROM name_user_1".
"revoke all privileges from public".
"revoke all privileges from user_1".
"revoke all privileges from user_1,user_2".
"revoke all privileges from user_1,user_2,user_3".
"revoke all privileges on table_1 from public force".
"revoke all privileges on table_1 from user_1 cascade constraints".
"revoke all privileges on table_1 from user_1,user_2 force".
"revoke all privileges on table_1 from user_1,user_2,user_3 cascade constraints".
"revoke all,alter,delete,execute,index,insert,references,select,update on directory directory_1 from user_1".
"revoke all,alter,delete,execute,index,insert,references,select,update on directory directory_1 from user_1,user_2,user_3".
"revoke all,alter,delete,execute,index,insert,references,select,update on table_1 from public".
"revoke all,alter,delete,execute,index,insert,references,select,update on table_1 from user_1,user_2".
"revoke alter any index from public".
"revoke alter any materialized view from public".
"revoke alter any table from public".
"revoke alter any view from public".
"revoke create any index from public".
"revoke create any materialized view from public".
"revoke create any table from public".
"revoke create any view from public".
"revoke create materialized view from public".
"REVOKE CREATE TABLE FROM name_user_1, name_user_2".
"revoke create table from public".
"revoke create view from public".
"revoke delete any table from public".
"Revoke Delete On :param_1 From user_1".
"Revoke Delete On \"^&()\" From user_1".
"Revoke Delete On schema_1.table_1 From user_1".
"REVOKE DELETE ON table_1 FROM name_user_1".
"Revoke Delete On table_1 From user_1".
"REVOKE DELETE, INSERT, SELECT, UPDATE ON table_1 FROM name_user_1".
"revoke drop any index from public".
"revoke drop any materialized view from public".
"REVOKE DROP ANY TABLE FROM name_user_1".
"revoke drop any table from public".
"REVOKE DROP ANY VIEW FROM name_user_1".
"revoke drop any view from public".
"REVOKE EXECUTE ON module1 FROM user1".
"revoke insert any table from public".
"REVOKE INSERT ON table_1 FROM name_user_1".
"REVOKE manage_system FROM admin1".
"REVOKE name_privilege_1 FROM name_user_1".
"REVOKE name_privilege_1, name_privilege_2 FROM name_user_1".
"REVOKE name_privilege_1, name_privilege_2, name_privilege_3, name_privilege_4, name_privilege_5 FROM name_user_1".
"REVOKE role_1 FROM name_user_1".
"revoke select any table from public".
"REVOKE SELECT ON ddTable FROM user_1".
"REVOKE SELECT ON table_1 FROM name_user_1".
"revoke something from public".
"revoke update any table from public".
"REVOKE UPDATE ON table_1 FROM name_user_1".
"REVOKE UPDATE ON table_1 FROM name_user_1, name_user_2 FORCE".
"revoke update, delete on ddTable from test_user_1".

%% -----------------------------------------------------------------------------
%% TESTS: REVOKE
%% =============================================================================
