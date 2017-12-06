%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%%
%% TESTS
%%

"REVOKE ALL ON table_1 FROM name_user_1 CASCADE CONSTRAINTS".
"REVOKE UPDATE ON table_1 FROM name_user_1, name_user_2 FORCE".

"REVOKE DROP ANY TABLE FROM name_user_1".
"REVOKE CREATE TABLE FROM name_user_1, name_user_2".

"REVOKE ADMIN FROM name_user_1".
"REVOKE role_1 FROM name_user_1".

"REVOKE ALL ON name_table FROM name_user_1".
% ? "REVOKE ON DIRECTORY name_directory FROM name_user_1".
% ? "REVOKE ON JAVA SOURCE name_table FROM name_user_1".
% ? "REVOKE ON JAVA RESOURCE name_table FROM name_user_1".

"REVOKE ALL PRIVILEGES FROM name_user_1".
"REVOKE DELETE ON table_1 FROM name_user_1".
"REVOKE DROP ANY VIEW FROM name_user_1".
"REVOKE INSERT ON table_1 FROM name_user_1".
"REVOKE SELECT ON table_1 FROM name_user_1".
"REVOKE UPDATE ON table_1 FROM name_user_1".
"REVOKE DELETE, INSERT, SELECT, UPDATE ON table_1 FROM name_user_1".
"REVOKE name_privilege_1 FROM name_user_1".
"REVOKE name_privilege_1, name_privilege_2 FROM name_user_1".
"REVOKE name_privilege_1, name_privilege_2, name_privilege_3, name_privilege_4, name_privilege_5 FROM name_user_1".

"REVOKE manage_system FROM admin1".
"REVOKE a,b,c FROM user1,user2".
"REVOKE SELECT ON ddTable FROM user_1".
"REVOKE ALL ON schema1.ddTable FROM user1,user2".
"REVOKE EXECUTE ON module1 FROM user1".
"revoke update, delete on ddTable from test_user_1".

% table reference -------------------------------------------------------------

"Revoke Delete On table_1 From user_1".
"Revoke Delete On schema_1.table_1 From user_1".
"Revoke Delete On :param_1 From user_1".
"Revoke Delete On \"^&()\" From user_1".
