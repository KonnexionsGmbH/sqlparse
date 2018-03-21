%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: ALTER_USER
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alter_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"ALTER USER name_user_1 DEFAULT ROLE ALL DEFAULT ROLE NONE".
"ALTER USER name_user_1 DEFAULT ROLE ALL EXCEPT name_role_1".
"ALTER USER name_user_1 DEFAULT ROLE ALL EXCEPT name_role_1,name_role_2".
"ALTER USER name_user_1 DEFAULT ROLE ALL".
"ALTER USER name_user_1 DEFAULT ROLE name_role_1".
"ALTER USER name_user_1 DEFAULT ROLE name_role_1,name_role_2".
"ALTER USER name_user_1 DEFAULT ROLE NONE".
"ALTER USER name_user_1 DEFAULT TABLESPACE name_tablespace_1".
"ALTER USER name_user_1 GRANT CONNECT THROUGH ENTERPRISE USERS".
"ALTER USER name_user_1 GRANT CONNECT THROUGH WITH NO ROLES".
"ALTER USER name_user_1 IDENTIFIED BY name_password_1 IDENTIFIED BY name_password_2".
"ALTER USER name_user_1 IDENTIFIED EXTERNALLY AS name_external_1".
"ALTER USER name_user_1 IDENTIFIED EXTERNALLY".
"ALTER USER name_user_1 IDENTIFIED GLOBALLY AS name_external_1".
"ALTER USER name_user_1 IDENTIFIED GLOBALLY".
"ALTER USER name_user_1 PROFILE name_profile_1".
"ALTER USER name_user_1 QUOTA 1024 name_unit_1 ON name_tablespace_1".
"ALTER USER name_user_1 QUOTA 1024 ON name_tablespace_1".
"ALTER USER name_user_1 QUOTA UNLIMITED ON name_tablespace_1".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH AUTHENTICATION REQUIRED".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH ENTERPRISE USERS".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH WITH NO ROLES AUTHENTICATION REQUIRED".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH WITH NO ROLES".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT role1,role2 AUTHENTICATION REQUIRED".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT role1,role2".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH WITH ROLE role1,role2 AUTHENTICATION REQUIRED".
"ALTER USER name_user_1 REVOKE CONNECT THROUGH WITH ROLE role1,role2".
"ALTER USER name_user_1 TEMPORARY TABLESPACE name_tablespace_1".
"ALTER USER name_user_1,name_user_2 GRANT CONNECT THROUGH ENTERPRISE USERS".
"ALTER USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 DEFAULT TABLESPACE table_2".
"ALTER USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 TEMPORARY TABLESPACE table_2".
"ALTER USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1".
"ALTER USER test_user_1 IDENTIFIED BY a_password".
"ALTER USER test_user_1 IDENTIFIED EXTERNALLY AS test_usr_2_extern TEMPORARY TABLESPACE table_1".
"ALTER USER test_user_123 ACCOUNT LOCK".
"ALTER USER test_user_123 ACCOUNT UNLOCK".
"ALTER USER test_user_123 IDENTIFIED BY new_password".
"ALTER USER test_user_123 PASSWORD EXPIRE".
"ALTER USER test_user_2 IDENTIFIED EXTERNALLY AS test_usr_2_extern".
"ALTER USER test_user_3 IDENTIFIED EXTERNALLY PROFILE user_profile".
"ALTER USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10 ON table_3 QUOTA 10M ON table_4 QUOTA UNLIMITED ON table_1".
"ALTER USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_2".
"ALTER USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_3 QUOTA UNLIMITED ON table_1".
"ALTER USER test_user_3 IDENTIFIED EXTERNALLY QUOTA UNLIMITED ON table_1".
"ALTER USER test_user_3 IDENTIFIED EXTERNALLY".
"ALTER USER test_user_4 IDENTIFIED EXTERNALLY IDENTIFIED GLOBALLY".
"ALTER USER test_user_4 IDENTIFIED GLOBALLY AS test_usr_2_extern".
"ALTER USER test_user_4 IDENTIFIED GLOBALLY".
"alter user user_1 grant connect through authentication required".
"alter user user_1 grant connect through enterprise users".
"alter user user_1 grant connect through with no roles authentication required".
"alter user user_1 grant connect through with no roles".
"alter user user_1 grant connect through with role all except role_1 authentication required".
"alter user user_1 grant connect through with role all except role_1".
"alter user user_1 grant connect through with role all except role_1,role_2 authentication required".
"alter user user_1 grant connect through with role all except role_1,role_2".
"alter user user_1 grant connect through with role role_1 authentication required".
"alter user user_1 grant connect through with role role_1".
"alter user user_1 grant connect through with role role_1,role_2 authentication required".
"alter user user_1 grant connect through with role role_1,role_2".
"alter user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3 quota unlimited on name_4".
"alter user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3".
"alter user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3;\"extra remark\"".
"alter user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1 quota unlimited on name_1".
"alter user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1".
"alter user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1;extra_1".
"alter user user_1 identified by name_1 default tablespace tablespace_1".
"alter user user_1 identified by name_1 default tablespace tablespace_1;\"extra remark\"".
"alter user user_1 identified by name_1 default tablespace tablespace_1;extra_1".
"alter user user_1 identified by name_1 profile profile_1 profile profile_2".
"alter user user_1 identified by name_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3".
"alter user user_1 identified by name_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3;extra_1".
"alter user user_1 identified by name_1 quota 1024 mb on name_1 quota 1024 on name_2 quota unlimited on name_3".
"alter user user_1 identified by name_1 quota 1024 mb on name_1".
"alter user user_1 identified by name_1 quota 1024 on name_1".
"alter user user_1 identified by name_1 quota unlimited on name_1".
"alter user user_1 identified by name_1 temporary tablespace tablespace_1".
"alter user user_1 identified by name_1".
"alter user user_1 identified by name_1;\"user remark\"".
"alter user user_1 identified by name_1;extra_1".
"alter user user_1 identified by name_1;extra_1;alter user user_2 identified by name_2;extra_2".
"alter user user_1 identified externally as name_1 identified globally as name_1".
"alter user user_1 identified externally as name_1".
"alter user user_1 identified externally".
"alter user user_1 identified globally as name_1".
"alter user user_1 identified globally".
"alter user user_1 revoke connect through authentication required".
"alter user user_1 revoke connect through enterprise users".
"alter user user_1 revoke connect through with no roles authentication required".
"alter user user_1 revoke connect through with no roles".
"alter user user_1 revoke connect through with role all except role1,role2 authentication required".
"alter user user_1 revoke connect through with role all except role1,role2".
"alter user user_1 revoke connect through with role all except role_1 authentication required".
"alter user user_1 revoke connect through with role all except role_1".
"alter user user_1 revoke connect through with role all except role_1,role_2 authentication required".
"alter user user_1 revoke connect through with role all except role_1,role_2".
"alter user user_1 revoke connect through with role role1,role2 authentication required".
"alter user user_1 revoke connect through with role role1,role2".
"alter user user_1 revoke connect through with role role_1 authentication required".
"alter user user_1 revoke connect through with role role_1".
"alter user user_1 revoke connect through with role role_1,role_2 authentication required".
"alter user user_1 revoke connect through with role role_1,role_2".
"alter user user_1,user_2,user_3 grant connect through authentication required".
"alter user user_1,user_2,user_3 grant connect through enterprise users".
"alter user user_1,user_2,user_3 grant connect through with no roles authentication required".
"alter user user_1,user_2,user_3 grant connect through with no roles".
"alter user user_1,user_2,user_3 grant connect through with role all except role_1 authentication required".
"alter user user_1,user_2,user_3 grant connect through with role all except role_1".
"alter user user_1,user_2,user_3 grant connect through with role all except role_1,role_2 authentication required".
"alter user user_1,user_2,user_3 grant connect through with role all except role_1,role_2".
"alter user user_1,user_2,user_3 grant connect through with role role_1 authentication required".
"alter user user_1,user_2,user_3 grant connect through with role role_1".
"alter user user_1,user_2,user_3 grant connect through with role role_1,role_2 authentication required".
"alter user user_1,user_2,user_3 grant connect through with role role_1,role_2".
"alter user user_1,user_2,user_3 revoke connect through authentication required".
"alter user user_1,user_2,user_3 revoke connect through enterprise users".
"alter user user_1,user_2,user_3 revoke connect through with no roles authentication required".
"alter user user_1,user_2,user_3 revoke connect through with no roles".
"alter user user_1,user_2,user_3 revoke connect through with role all except role1,role2 authentication required".
"alter user user_1,user_2,user_3 revoke connect through with role all except role1,role2".
"alter user user_1,user_2,user_3 revoke connect through with role all except role_1 authentication required".
"alter user user_1,user_2,user_3 revoke connect through with role all except role_1".
"alter user user_1,user_2,user_3 revoke connect through with role all except role_1,role_2 authentication required".
"alter user user_1,user_2,user_3 revoke connect through with role all except role_1,role_2".
"alter user user_1,user_2,user_3 revoke connect through with role role1,role2 authentication required".
"alter user user_1,user_2,user_3 revoke connect through with role role1,role2".
"alter user user_1,user_2,user_3 revoke connect through with role role_1 authentication required".
"alter user user_1,user_2,user_3 revoke connect through with role role_1".
"alter user user_1,user_2,user_3 revoke connect through with role role_1,role_2 authentication required".
"alter user user_1,user_2,user_3 revoke connect through with role role_1,role_2".

%% -----------------------------------------------------------------------------
%% TESTS: ALTER_USER
%% =============================================================================
