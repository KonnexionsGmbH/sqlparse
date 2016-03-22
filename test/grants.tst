%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS
%%

"GRANT TO name_user_1".
"GRANT TO name_user_1, name_user_2".

"GRANT TO name_user_1 WITH GRANT OPTION".
"GRANT TO name_user_1 WITH HIERARCHY OPTION".
"GRANT TO name_user_1 WITH name OPTION".

"GRANT ON name_table TO name_user_1".
% ? "GRANT ON DIRECTORY name_directory TO name_user_1".
% ? "GRANT ON JAVA SOURCE name_table TO name_user_1".
% ? "GRANT ON JAVA RESOURCE name_table TO name_user_1".

"GRANT ALL TO name_user_1".
"GRANT ALL PRIVILEGES TO name_user_1".
"GRANT DELETE TO name_user_1".
"GRANT DROP TO name_user_1".
"GRANT INSERT TO name_user_1".
"GRANT SELECT TO name_user_1".
"GRANT UPDATE TO name_user_1".
"GRANT DELETE, DROP, INSERT, SELECT, UPDATE TO name_user_1".
"GRANT name_privilege_1 TO name_user_1".
"GRANT DELETE, name_privilege_1, name_privilege_2 TO name_user_1".
"GRANT name_privilege_1, name_privilege_2, DELETE TO name_user_1".
"GRANT name_privilege_1 name_privilege_2 name_privilege_3 name_privilege_4 name_privilege_5 TO name_user_1".

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
