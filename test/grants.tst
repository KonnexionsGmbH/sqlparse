%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS
%%

"GRANT CREATE TABLE TO name_user_1".
"GRANT DROP ANY TABLE TO name_user_1, name_user_2".

"GRANT super_role TO name_user_1 WITH DELEGATE OPTION".
"GRANT ADMIN TO name_user_1 WITH ADMIN OPTION".
"GRANT SELECT ON table_1 TO name_user_1 WITH GRANT OPTION".
"GRANT REFERENCES ON table_1 TO name_user_1 WITH HIERARCHY OPTION".

"GRANT ALL ON name_table TO name_user_1".
% ? "GRANT ON DIRECTORY name_directory TO name_user_1".
% ? "GRANT ON JAVA SOURCE name_table TO name_user_1".
% ? "GRANT ON JAVA RESOURCE name_table TO name_user_1".

"GRANT ALL PRIVILEGES TO name_user_1".
"GRANT DELETE ON table_1 TO name_user_1".
"GRANT INSERT ON table_1 TO name_user_1".
"GRANT SELECT ON table_1 TO name_user_1".
"GRANT UPDATE ON table_1 TO name_user_1".
"GRANT DELETE, INSERT, SELECT, UPDATE ON table_1 TO name_user_1".
"GRANT name_role TO name_user_1".
"GRANT name_privilege_1, name_privilege_2 TO name_user_1".
"GRANT name_privilege_1, name_privilege_2, name_privilege_3, name_privilege_4, name_privilege_5 TO name_user_1".

"GRANT manage_system TO user_1".
"GRANT a,b,c TO user2".
"GRANT SELECT ON ddTable TO user_1".
"GRANT SELECT ON schema1.ddTable TO user_1".
"GRANT ALL ON ddTable TO user1,user2".
"GRANT EXECUTE ON module1 TO user1".
"GRANT all privileges ON schema1.ddTable TO role2".
"grant update, delete on ddTable to test_user_1".
"grant insert on ddTable to test_user_1 WITH GRANT OPTION".
"GRANT manage_system TO test_user_1 with admin option".

% table reference -------------------------------------------------------------

"Grant Delete On table_1 To user_1".
"Grant Delete On schema_1.table_1 To user_1".
"Grant Delete On :param_1 To user_1".
"Grant Delete On \"^&()\" To user_1".
