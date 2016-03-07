%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"REVOKE FROM name_user_1".
"REVOKE FROM name_user_1, name_user_2".

"REVOKE FROM name_user_1 CASCADE CONSTRAINS".
"REVOKE FROM name_user_1 FORCE".

"REVOKE ON name_table FROM name_user_1".
% ? "REVOKE ON DIRECTORY name_directory FROM name_user_1".
% ? "REVOKE ON JAVA SOURCE name_table FROM name_user_1".
% ? "REVOKE ON JAVA RESOURCE name_table FROM name_user_1".

"REVOKE ALL FROM name_user_1".
"REVOKE ALL PRIVILEGES FROM name_user_1".
"REVOKE DELETE FROM name_user_1".
"REVOKE DROP FROM name_user_1".
"REVOKE INSERT FROM name_user_1".
"REVOKE SELECT FROM name_user_1".
"REVOKE UPDATE FROM name_user_1".
"REVOKE DELETE, DROP, INSERT, SELECT, UPDATE FROM name_user_1".
"REVOKE name_privilege_1 FROM name_user_1".
"REVOKE DELETE, name_privilege_1, name_privilege_2 FROM name_user_1".
"REVOKE name_privilege_1, name_privilege_2, DELETE FROM name_user_1".
"REVOKE name_privilege_1 name_privilege_2 name_privilege_3 name_privilege_4 name_privilege_5 FROM name_user_1".

"REVOKE manage_system FROM admin".
"REVOKE a,b,c FROM user1,user2".
"REVOKE SELECT ON ddTable FROM user_1".
"REVOKE ALL ON schema1.ddTable FROM user1,user2".
"REVOKE EXECUTE ON module1 FROM user1".
"revoke update, delete on ddTable from test_user_1".
