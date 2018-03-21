%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: GRANT
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grant_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"GRANT a,b,c TO user2".
"GRANT ADMIN TO name_user_1 WITH ADMIN OPTION".
"grant admin to public".
"grant admin,create any table,something to public".
"grant admin,create any table,something to public,user_1".
"grant admin,create any table,something to user_1 identified by 'password_1'".
"grant admin,create any table,something to user_1".
"grant admin,create any table,something to user_1,user_2".
"grant admin,create any table,something to user_1,user_2,user_3".
"grant admin,something to public with delegate option".
"grant admin,something to user_1 identified by 'password_1' with admin option".
"grant admin,something to user_1 with delegate option".
"grant admin,something to user_1,user_2 with admin option".
"grant admin,something to user_1,user_2,user_3 with delegate option".
"GRANT ALL ON ddTable TO user1,user2".
"GRANT ALL ON name_table TO name_user_1".
"GRANT all privileges ON schema1.ddTable TO role2".
"grant all privileges on table_1 to public with grant option".
"grant all privileges on table_1 to user_1 identified by 'password_1' with hierarchy option".
"grant all privileges on table_1 to user_1 with grant option".
"grant all privileges on table_1 to user_1,user_2 with hierarchy option".
"grant all privileges on table_1 to user_1,user_2,user_3 with grant option".
"GRANT ALL PRIVILEGES TO name_user_1".
"grant all privileges to public with admin option".
"grant all privileges to public".
"grant all privileges to user_1 identified by 'password_1' with delegate option".
"grant all privileges to user_1 identified by 'password_1'".
"grant all privileges to user_1 with admin option".
"grant all privileges to user_1".
"grant all privileges to user_1,user_2 with delegate option".
"grant all privileges to user_1,user_2".
"grant all privileges to user_1,user_2,user_3 with admin option".
"grant all privileges to user_1,user_2,user_3".
"grant all,alter,delete,execute,index,insert,references,select,update on directory directory_1 to user_1 identified by 'password_1'".
"grant all,alter,delete,execute,index,insert,references,select,update on directory directory_1 to user_1,user_2".
"grant all,alter,delete,execute,index,insert,references,select,update on table_1 to public".
"grant all,alter,delete,execute,index,insert,references,select,update on table_1 to user_1".
"grant all,alter,delete,execute,index,insert,references,select,update on table_1 to user_1,user_2,user_3".
"grant alter any index to public".
"grant alter any materialized view to public".
"grant alter any table to public".
"grant alter any view to public".
"grant create any index to public".
"grant create any materialized view to public".
"grant create any table to public".
"grant create any view to public".
"grant create materialized view to public".
"GRANT CREATE TABLE TO name_user_1".
"grant create table to public".
"grant create view to public".
"grant delete any table to public".
"Grant Delete On :param_1 To user_1".
"Grant Delete On \"^&()\" To user_1".
"Grant Delete On schema_1.table_1 To user_1".
"GRANT DELETE ON table_1 TO name_user_1".
"Grant Delete On table_1 To user_1".
"GRANT DELETE, INSERT, SELECT, UPDATE ON table_1 TO name_user_1".
"grant drop any index to public".
"grant drop any materialized view to public".
"GRANT DROP ANY TABLE TO name_user_1, name_user_2".
"grant drop any table to public".
"grant drop any view to public".
"GRANT EXECUTE ON module1 TO user1".
"grant insert any table to public".
"grant insert on ddTable to test_user_1 WITH GRANT OPTION".
"GRANT INSERT ON table_1 TO name_user_1".
"GRANT manage_system TO test_user_1 with admin option".
"GRANT manage_system TO user_1".
"GRANT name_privilege_1, name_privilege_2 TO name_user_1".
"GRANT name_privilege_1, name_privilege_2, name_privilege_3, name_privilege_4, name_privilege_5 TO name_user_1".
"GRANT name_role TO name_user_1".
"GRANT REFERENCES ON table_1 TO name_user_1 WITH HIERARCHY OPTION".
"grant select any table to public".
"GRANT SELECT ON ddTable TO user_1".
"GRANT SELECT ON schema1.ddTable TO user_1".
"GRANT SELECT ON table_1 TO name_user_1 WITH GRANT OPTION".
"GRANT SELECT ON table_1 TO name_user_1".
"grant something to public".
"GRANT super_role TO name_user_1 WITH DELEGATE OPTION".
"grant update any table to public".
"GRANT UPDATE ON table_1 TO name_user_1".
"grant update, delete on ddTable to test_user_1".

%% -----------------------------------------------------------------------------
%% TESTS: GRANT
%% =============================================================================
