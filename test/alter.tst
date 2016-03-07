%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS ALTER USER
%%

% ? "ALTER USER name_user_1 GRANT CONNECT THROUGH ENTERPRISE USERS".
% ? "ALTER USER name_user_1 name_user_2 GRANT CONNECT THROUGH ENTERPRISE USERS".
% ? "ALTER USER name_user_1 GRANT REVOKE THROUGH ENTERPRISE USERS".
% ? "ALTER USER name_user_1 REVOKE THROUGH name_user_2".

"ALTER USER test_user_123 IDENTIFIED BY new_password".
% ? "ALTER USER name_user_1 IDENTIFIED BY name_password_1 IDENTIFIED BY name_password_2".
"ALTER USER name_user_1 IDENTIFIED EXTERNALLY".
"ALTER USER name_user_1 IDENTIFIED EXTERNALLY AS name_external_1".
"ALTER USER name_user_1 IDENTIFIED GLOBALLY".
"ALTER USER name_user_1 IDENTIFIED GLOBALLY AS name_external_1".

"ALTER USER name_user_1 DEFAULT TABLESPACE name_tablespace_1".
"ALTER USER name_user_1 TEMPORARY TABLESPACE name_tablespace_1".

"ALTER USER name_user_1 QUOTA UNLIMITED ON name_tablespace_1".
"ALTER USER name_user_1 QUOTA 1024 ON name_tablespace_1".
"ALTER USER name_user_1 QUOTA 1024 name_unit_1 ON name_tablespace_1".

"ALTER USER name_user_1 PROFILE name_profile_1".

% ? "ALTER USER name_user_1 DEFAULT ROLE ALL".
% ? "ALTER USER name_user_1 DEFAULT ROLE ALL EXCEPT name_role_1".
% ? "ALTER USER name_user_1 DEFAULT ROLE ALL EXCEPT name_role_1 name_role_2".
% ? "ALTER USER name_user_1 DEFAULT ROLE NONE".
% ? "ALTER USER name_user_1 DEFAULT ROLE name_role_1".
% ? "ALTER USER name_user_1 DEFAULT ROLE name_role_1 name_role_2".

"ALTER USER test_user_123 ACCOUNT LOCK".
"ALTER USER test_user_123 ACCOUNT UNLOCK".
"ALTER USER test_user_123 PASSWORD EXPIRE".
