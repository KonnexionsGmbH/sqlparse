%% -----------------------------------------------------------------------------
%%
%% sqlpartse_pretty_test.hrl: SQL - pretty format test driver.
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

-ifndef(SQLPARSE_PRETTY_TEST_HRL).
-define(SQLPARSE_PRETTY_TEST_HRL, true).

-include_lib("eunit/include/eunit.hrl").
-include("sqlparse_fold.hrl").
-include("sqlparse_test.hrl").

%%------------------------------------------------------------------------------
%% ALTER_USER 01 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_01, "
alter user user_1
grant connect through enterprise users").

-define(ALTER_USER_01_RESULT_DEFAULT, "ALTER USER
    User_1
GRANT CONNECT THROUGH ENTERPRISE USERS").

%%------------------------------------------------------------------------------
%% ALTER_USER 02 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_02, "
alter user user_1
grant connect through with no roles").

-define(ALTER_USER_02_RESULT_DEFAULT, "ALTER USER
    User_1
GRANT CONNECT THROUGH WITH NO ROLES").

%%------------------------------------------------------------------------------
%% ALTER_USER 03 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_03, "
alter user user_1
revoke connect through authentication required").

-define(ALTER_USER_03_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH AUTHENTICATION REQUIRED").

%%------------------------------------------------------------------------------
%% ALTER_USER 04 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_04, "
alter user user_1
revoke connect through with no roles authentication required").

-define(ALTER_USER_04_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH NO ROLES AUTHENTICATION REQUIRED").

%%------------------------------------------------------------------------------
%% ALTER_USER 05 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_05, "
alter user user_1
revoke connect through with role role_1, role_2").

-define(ALTER_USER_05_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE
    Role_1, Role_2").

%%------------------------------------------------------------------------------
%% ALTER_USER 06 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_06, "
alter user user_1
revoke connect through with role role_1, role_2 authentication required").

-define(ALTER_USER_06_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE
    Role_1, Role_2
AUTHENTICATION REQUIRED").

%%------------------------------------------------------------------------------
%% ALTER_USER 07 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_07, "
alter user user_1
revoke connect through with role all except role_1, role_2").

-define(ALTER_USER_07_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT
    Role_1, Role_2").

%%------------------------------------------------------------------------------
%% ALTER_USER 08 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_08, "
alter user user_1
revoke connect through with role all except role_1, role_2 authentication required").

-define(ALTER_USER_08_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT
    Role_1, Role_2
AUTHENTICATION REQUIRED").

%%------------------------------------------------------------------------------
%% ALTER_USER 09 - IDENTIFIED BY.
%%------------------------------------------------------------------------------

-define(ALTER_USER_09, "
alter user test_user_123
identified by new_password").

-define(ALTER_USER_09_RESULT_DEFAULT, "ALTER USER
    Test_User_123
IDENTIFIED BY
    new_password").

%%------------------------------------------------------------------------------
%% ALTER_USER 10 - IDENTIFIED BY.
%%------------------------------------------------------------------------------

-define(ALTER_USER_10, "
alter user user_1
identified by name_password_1 identified by name_password_2").

-define(ALTER_USER_10_RESULT_DEFAULT, "ALTER USER
    User_1
IDENTIFIED BY
    name_password_1
IDENTIFIED BY
    name_password_2").

%%------------------------------------------------------------------------------
%% ALTER_USER 11 - IDENTIFIED EXTERNALLY.
%%------------------------------------------------------------------------------

-define(ALTER_USER_11, "
alter user user_1
identified externally").

-define(ALTER_USER_11_RESULT_DEFAULT, "ALTER USER
    User_1
IDENTIFIED EXTERNALLY").

%%------------------------------------------------------------------------------
%% ALTER_USER 12 - IDENTIFIED GLOBALLY.
%%------------------------------------------------------------------------------

-define(ALTER_USER_12, "
alter user user_1
identified globally as name_external_1").

-define(ALTER_USER_12_RESULT_DEFAULT, "ALTER USER
    User_1
IDENTIFIED GLOBALLY AS
    name_external_1").

%%------------------------------------------------------------------------------
%% ALTER_USER 13 - DEFAULT TABLESPACE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_13, "
alter user user_1
default tablespace tablespace_1").

-define(ALTER_USER_13_RESULT_DEFAULT, "ALTER USER
    User_1
DEFAULT TABLESPACE
    Tablespace_1").

%%------------------------------------------------------------------------------
%% ALTER_USER 14 - QUOTA.
%%------------------------------------------------------------------------------

-define(ALTER_USER_14, "
alter user user_1
quota unlimited on tablespace_1").

-define(ALTER_USER_14_RESULT_DEFAULT, "ALTER USER
    User_1
QUOTA
    UNLIMITED ON Tablespace_1").

%%------------------------------------------------------------------------------
%% ALTER_USER 15 - QUOTA.
%%------------------------------------------------------------------------------

-define(ALTER_USER_15, "
alter user user_1
quota 1024 on tablespace_1").

-define(ALTER_USER_15_RESULT_DEFAULT, "ALTER USER
    User_1
QUOTA
    1024 ON Tablespace_1").

%%------------------------------------------------------------------------------
%% ALTER_USER 16 - QUOTA.
%%------------------------------------------------------------------------------

-define(ALTER_USER_16, "
alter user user_1
quota 1024 unit_1 on tablespace_1").

-define(ALTER_USER_16_RESULT_DEFAULT, "ALTER USER
    User_1
QUOTA
    1024 Unit_1 ON Tablespace_1").

%%------------------------------------------------------------------------------
%% ALTER_USER 17 - PROFILE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_17, "
alter user user_1
profile profile_1").

-define(ALTER_USER_17_RESULT_DEFAULT, "ALTER USER
    User_1
PROFILE
    Profile_1").

%%------------------------------------------------------------------------------
%% ALTER_USER 18 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_18, "
alter user user_1
default role all").

-define(ALTER_USER_18_RESULT_DEFAULT, "ALTER USER
    User_1
DEFAULT ROLE ALL").

%%------------------------------------------------------------------------------
%% ALTER_USER 19 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_19, "
alter user user_1
default role role_1,role_2").

-define(ALTER_USER_19_RESULT_DEFAULT, "ALTER USER
    User_1
DEFAULT ROLE
    Role_1, Role_2").

%%------------------------------------------------------------------------------
%% ALTER_USER 20 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_20, "
alter user user_1
default role all except role_1").

-define(ALTER_USER_20_RESULT_DEFAULT, "ALTER USER
    User_1
DEFAULT ROLE ALL EXCEPT
    Role_1").

%%------------------------------------------------------------------------------
%% ALTER_USER 21 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_21, "
alter user user_1
default role all except role_1,role_2").

-define(ALTER_USER_21_RESULT_DEFAULT, "ALTER USER
    User_1
DEFAULT ROLE ALL EXCEPT
    Role_1, Role_2").

%%------------------------------------------------------------------------------
%% ALTER_USER 22 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_22, "
alter user user_1
default role none").

-define(ALTER_USER_22_RESULT_DEFAULT, "ALTER USER
    User_1
DEFAULT ROLE NONE").

%%------------------------------------------------------------------------------
%% ALTER_USER 23 - ACCOUNT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_23, "
alter user test_user_123
account lock").

-define(ALTER_USER_23_RESULT_DEFAULT, "ALTER USER
    Test_User_123
ACCOUNT LOCK").

%%------------------------------------------------------------------------------
%% ALTER_USER 24 - ACCOUNT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_24, "
alter user test_user_123
account unlock").

-define(ALTER_USER_24_RESULT_DEFAULT, "ALTER USER
    Test_User_123
ACCOUNT UNLOCK").

%%------------------------------------------------------------------------------
%% ALTER_USER 25 - PASSWORD.
%%------------------------------------------------------------------------------

-define(ALTER_USER_25, "
alter user test_user_123
password expire").

-define(ALTER_USER_25_RESULT_DEFAULT, "ALTER USER
    Test_User_123
PASSWORD EXPIRE").

%%------------------------------------------------------------------------------
%% ALTER_USER 26 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_26, "
alter user user_1,user_2,user_3
grant connect through enterprise users").

-define(ALTER_USER_26_RESULT_DEFAULT, "ALTER USER
    User_1, User_2, User_3
GRANT CONNECT THROUGH ENTERPRISE USERS").

%%------------------------------------------------------------------------------
%% ALTER_USER 27 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_27, "
alter user user_1,user_2,user_3,user_4
grant connect through enterprise users").

-define(ALTER_USER_27_RESULT_DEFAULT, "ALTER USER
    User_1, User_2, User_3, User_4
GRANT CONNECT THROUGH ENTERPRISE USERS").

%%------------------------------------------------------------------------------
%% ALTER_USER 28 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_28, "
alter user user_1
revoke connect through with role role_1, role_2,role_3,role_4").

-define(ALTER_USER_28_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE
    Role_1, Role_2, Role_3, Role_4").

%%------------------------------------------------------------------------------
%% ALTER_USER 29 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_29, "
alter user user_1
revoke connect through with role role_1, role_2,role_3,role_4 authentication required").

-define(ALTER_USER_29_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE
    Role_1, Role_2, Role_3, Role_4
AUTHENTICATION REQUIRED").

%%------------------------------------------------------------------------------
%% ALTER_USER 30 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_30, "
alter user user_1
revoke connect through with role all except role_1, role_2,role_3,role_4").

-define(ALTER_USER_30_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT
    Role_1, Role_2, Role_3, Role_4").

%%------------------------------------------------------------------------------
%% ALTER_USER 31 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_31, "
alter user user_1
revoke connect through with role all except role_1, role_2,role_3,role_4 authentication required").

-define(ALTER_USER_31_RESULT_DEFAULT, "ALTER USER
    User_1
REVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT
    Role_1, Role_2, Role_3, Role_4
AUTHENTICATION REQUIRED").

%%------------------------------------------------------------------------------
%% CREATE 01 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_01, "
create index on table_1").

-define(CREATE_01_RESULT_DEFAULT, "CREATE INDEX
ON
    Table_1").

%%------------------------------------------------------------------------------
%% CREATE 02 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_02, "
create index on table_1 norm_with 'n' filter_with 'f'").

-define(CREATE_02_RESULT_DEFAULT, "CREATE INDEX
ON
    Table_1
NORM_WITH
    'n'
FILTER_WITH
    'f'").

%%------------------------------------------------------------------------------
%% CREATE 03 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_03, "
create index on table_1 (column_1,column_2,column_3)").

-define(CREATE_03_RESULT_DEFAULT, "CREATE INDEX
ON
    Table_1 (Column_1, Column_2, Column_3)").

%%------------------------------------------------------------------------------
%% CREATE 04 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_04, "
create index on table_1 (column_1,column_2,column_3) norm_with 'n' filter_with 'f'").

-define(CREATE_04_RESULT_DEFAULT, "CREATE INDEX
ON
    Table_1 (Column_1, Column_2, Column_3)
NORM_WITH
    'n'
FILTER_WITH
    'f'").

%%------------------------------------------------------------------------------
%% CREATE 05 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_05, "
create index index_1 on table_1").

-define(CREATE_05_RESULT_DEFAULT, "CREATE INDEX
    Index_1
ON
    Table_1").

%%------------------------------------------------------------------------------
%% CREATE 06 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_06, "
create index index_1 on table_1 norm_with 'n' filter_with 'f'").

-define(CREATE_06_RESULT_DEFAULT, "CREATE INDEX
    Index_1
ON
    Table_1
NORM_WITH
    'n'
FILTER_WITH
    'f'").

%%------------------------------------------------------------------------------
%% CREATE 07 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_07, "
create unique index index_1 on table_1 (column_1,column_2,column_3) norm_with 'n' filter_with 'f'").

-define(CREATE_07_RESULT_DEFAULT, "CREATE UNIQUE INDEX
    Index_1
ON
    Table_1 (Column_1, Column_2, Column_3)
NORM_WITH
    'n'
FILTER_WITH
    'f'").

%%------------------------------------------------------------------------------
%% CREATE 08 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_08, "
create bitmap index index_1 on table_1 (column_1,column_2,column_3,column_4) norm_with 'n' filter_with 'f'").

-define(CREATE_08_RESULT_DEFAULT, "CREATE BITMAP INDEX
    Index_1
ON
    Table_1 (Column_1, Column_2, Column_3, Column_4)
NORM_WITH
    'n'
FILTER_WITH
    'f'").

%%------------------------------------------------------------------------------
%% CREATE 09 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_09, "
create index index_1 on table_1 alias_1").

-define(CREATE_09_RESULT_DEFAULT, "CREATE INDEX
    Index_1
ON
    Table_1 Alias_1").

%%------------------------------------------------------------------------------
%% CREATE 10 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_10, "
Create Index index_1 On schema_1.table_1 alias_1").

-define(CREATE_10_RESULT_DEFAULT, "CREATE INDEX
    Index_1
ON
    Schema_1.Table_1 Alias_1").

%%------------------------------------------------------------------------------
%% CREATE 11 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_11, "
Create Index index_1 On :param_1 alias_1").

-define(CREATE_11_RESULT_DEFAULT, "CREATE INDEX
    Index_1
ON
    :param_1 Alias_1").

%%------------------------------------------------------------------------------
%% CREATE 12 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_12, "
create index index_1 on \"^&()\" alias_1").

-define(CREATE_12_RESULT_DEFAULT, "CREATE INDEX
    Index_1
ON
    \"^&()\" Alias_1").

%%------------------------------------------------------------------------------
%% CREATE 13 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_13, "
create table table_1 (column_1 date)").

-define(CREATE_13_RESULT_DEFAULT, "CREATE TABLE
    Table_1 (
        Column_1 DATE
    )").

%%------------------------------------------------------------------------------
%% CREATE 14 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_14, "
create table schema_1.table_1 (column_1 date)").

-define(CREATE_14_RESULT_DEFAULT, "CREATE TABLE
    Schema_1.Table_1 (
        Column_1 DATE
    )").

%%------------------------------------------------------------------------------
%% CREATE 15 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_15, "
create table :param_1 (column_1 date)").

-define(CREATE_15_RESULT_DEFAULT, "CREATE TABLE
    :param_1 (
        Column_1 DATE
    )").

%%------------------------------------------------------------------------------
%% CREATE 16 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_16, "
create table \"^&()\" (column_1 date)").

-define(CREATE_16_RESULT_DEFAULT, "CREATE TABLE
    \"^&()\" (
        Column_1 DATE
    )").

%%------------------------------------------------------------------------------
%% CREATE 17 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_17, "
create table table_1 (column_1 date, column_2 date, foreign key (fkey_1,fkey_2) references table_2)").

-define(CREATE_17_RESULT_DEFAULT, "CREATE TABLE
    Table_1 (
        Column_1 DATE,
        Column_2 DATE,
        FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES Table_2
    )").

%%------------------------------------------------------------------------------
%% CREATE 18 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_18, "
create table schema_1.table_1 (column_1 date, column_2 date, unique (ukey_1,ukey_2))
").

-define(CREATE_18_RESULT_DEFAULT, "CREATE TABLE
    Schema_1.Table_1 (
        Column_1 DATE,
        Column_2 DATE,
        UNIQUE (Ukey_1, Ukey_2)
    )").

%%------------------------------------------------------------------------------
%% CREATE 19 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_19, "
create table schema_1.table_1 (column_1 date, column_2 date, primary key (pkey_1,pkey_2))").

-define(CREATE_19_RESULT_DEFAULT, "CREATE TABLE
    Schema_1.Table_1 (
        Column_1 DATE,
        Column_2 DATE,
        PRIMARY KEY (Pkey_1, Pkey_2)
    )").

%%------------------------------------------------------------------------------
%% CREATE 20 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_20, "
create table schema_1.table_1 (column_1 date, column_2 date, foreign key (fkey_1,fkey_2) references schema_1.table_2 (column_8,column_9))").

-define(CREATE_20_RESULT_DEFAULT, "CREATE TABLE
    Schema_1.Table_1 (
        Column_1 DATE,
        Column_2 DATE,
        FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES Schema_1.Table_2 (Column_8,
        Column_9)
    )").

%%------------------------------------------------------------------------------
%% CREATE 21 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_21, "
create local ordered_set table table_1
(column_1 date,
column_2 varchar2(30),
column_3 number(5,2))").

-define(CREATE_21_RESULT_DEFAULT, "CREATE LOCAL ORDERED_SET TABLE
    Table_1 (
        Column_1 DATE,
        Column_2 VARCHAR2(30),
        Column_3 NUMBER(5,2)
    )").

%%------------------------------------------------------------------------------
%% CREATE 22 - INDEX & FUN.
%%------------------------------------------------------------------------------

-define(CREATE_22, "
create index a on b (f) norm_with fun() -> norm end.").

-define(CREATE_22_RESULT_DEFAULT, "CREATE INDEX
    A
ON
    B (F)
NORM_WITH
    fun() -> norm end.").

%%------------------------------------------------------------------------------
%% CREATE 23 - INDEX & FUN.
%%------------------------------------------------------------------------------

-define(CREATE_23, "
create index a on b (a|:d{}|) norm_with fun() -> norm end. filter_with fun mod:modfun/5.").

-define(CREATE_23_RESULT_DEFAULT, "CREATE INDEX
    A
ON
    B (A|:d{}|)
NORM_WITH
    fun() -> norm end.
FILTER_WITH
    fun mod:modfun/5.").

%%------------------------------------------------------------------------------
%% CREATE 24 - INDEX & FUN.
%%------------------------------------------------------------------------------

-define(CREATE_24, "
create index name_sort on skvhaccount (cvalue|:name|) norm_with fun imem_index:vnf_lcase_ascii/1. filter_with fun imem_index:iff_binterm_list_1/1.").

-define(CREATE_24_RESULT_DEFAULT, "CREATE INDEX
    Name_Sort
ON
    Skvhaccount (Cvalue|:name|)
NORM_WITH
    fun imem_index:vnf_lcase_ascii/1.
FILTER_WITH
    fun imem_index:iff_binterm_list_1/1.").

%%------------------------------------------------------------------------------
%% DELETE 01 - SIMPLE.
%%------------------------------------------------------------------------------

-define(DELETE_01, "
delete from table_1").

-define(DELETE_01_RESULT_DEFAULT, "DELETE FROM
    Table_1").

%%------------------------------------------------------------------------------
%% DELETE 03 - SIMPLE RETURN.
%%------------------------------------------------------------------------------

-define(DELETE_03, "
delete from table_1 return column_3,column_4 into column_5,column_6").

-define(DELETE_03_RESULT_DEFAULT, "DELETE FROM
    Table_1
RETURN
    Column_3, Column_4
INTO
    Column_5, Column_6").

%%------------------------------------------------------------------------------
%% DELETE 04 - SIMPLE RETURN COLUMNS.
%%------------------------------------------------------------------------------

-define(DELETE_04, "
delete from table_1 return column_3,column_4,column_5,column_6 into column_7,column_8,column_9,column_10").

-define(DELETE_04_RESULT_DEFAULT, "DELETE FROM
    Table_1
RETURN
    Column_3, Column_4, Column_5, Column_6
INTO
    Column_7, Column_8, Column_9, Column_10").

%%------------------------------------------------------------------------------
%% DELETE 05 - CONDITIONED.
%%------------------------------------------------------------------------------

-define(DELETE_05, "
delete from table_1 where column_1=value_1 and column_2 = value_2").

-define(DELETE_05_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    Column_1 = Value_1
    AND Column_2 = Value_2").

%%------------------------------------------------------------------------------
%% DELETE 06 - CONDITIONED RETURN.
%%------------------------------------------------------------------------------

-define(DELETE_06, "
delete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4 into column_5,column_6").

-define(DELETE_06_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    Column_1 = Value_1
    AND Column_2 = Value_2
RETURN
    Column_3, Column_4
INTO
    Column_5, Column_6").

%%------------------------------------------------------------------------------
%% DELETE 07 - CONDITIONED RETURN COLUMNS.
%%------------------------------------------------------------------------------

-define(DELETE_07, "
delete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4,column_5,column_6 into column_7,column_8,column_9,column_10").

-define(DELETE_07_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    Column_1 = Value_1
    AND Column_2 = Value_2
RETURN
    Column_3, Column_4, Column_5, Column_6
INTO
    Column_7, Column_8, Column_9, Column_10").

%%------------------------------------------------------------------------------
%% DROP 01 - INDEX.
%%------------------------------------------------------------------------------

-define(DROP_01, "
drop index from table_1").

-define(DROP_01_RESULT_DEFAULT, "DROP INDEX
FROM
    Table_1").

%%------------------------------------------------------------------------------
%% DROP 02 - INDEX.
%%------------------------------------------------------------------------------

-define(DROP_02, "
drop index schema_1.index_1 from table_1").

-define(DROP_02_RESULT_DEFAULT, "DROP INDEX
    Schema_1.Index_1
FROM
    Table_1").

%%------------------------------------------------------------------------------
%% DROP 03 - ROLE.
%%------------------------------------------------------------------------------

-define(DROP_03, "
drop role role_1").

-define(DROP_03_RESULT_DEFAULT, "DROP ROLE
    Role_1").

%%------------------------------------------------------------------------------
%% DROP 04 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_04, "
drop table table_1, table_2").

-define(DROP_04_RESULT_DEFAULT, "DROP TABLE
    Table_1, Table_2").

%%------------------------------------------------------------------------------
%% DROP 05 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_05, "
drop table table_1, table_2 cascade").

-define(DROP_05_RESULT_DEFAULT, "DROP TABLE
    Table_1, Table_2
CASCADE").

%%------------------------------------------------------------------------------
%% DROP 06 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_06, "
drop table table_1 restrict").

-define(DROP_06_RESULT_DEFAULT, "DROP TABLE
    Table_1
RESTRICT").

%%------------------------------------------------------------------------------
%% DROP 07 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_07, "
drop table if exists table_1,table_2 cascade").

-define(DROP_07_RESULT_DEFAULT, "DROP TABLE IF EXISTS
    Table_1, Table_2
CASCADE").

%%------------------------------------------------------------------------------
%% DROP 08 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_08, "
drop table if exists table_1, table_2,table_3,table_4 restrict").

-define(DROP_08_RESULT_DEFAULT, "DROP TABLE IF EXISTS
    Table_1, Table_2, Table_3, Table_4
RESTRICT").

%%------------------------------------------------------------------------------
%% DROP 09 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_09, "
drop imem_dal_skvh table skvhtest").

-define(DROP_09_RESULT_DEFAULT, "DROP Imem_Dal_Skvh TABLE
    Skvhtest").

%%------------------------------------------------------------------------------
%% DROP 10 - USER.
%%------------------------------------------------------------------------------

-define(DROP_10, "
drop user user_1").

-define(DROP_10_RESULT_DEFAULT, "DROP USER
    User_1").

%%------------------------------------------------------------------------------
%% DROP 11 - USER.
%%------------------------------------------------------------------------------

-define(DROP_11, "
drop user user_1 cascade").

-define(DROP_11_RESULT_DEFAULT, "DROP USER
    User_1
CASCADE").

%%------------------------------------------------------------------------------
%% DROP 12 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_12, "
drop table schema_1.table_1").

-define(DROP_12_RESULT_DEFAULT, "DROP TABLE
    Schema_1.Table_1").

%%------------------------------------------------------------------------------
%% FROM 01 - ALIAS table: param & table.
%%------------------------------------------------------------------------------

-define(FROM_01, "
select *
from dual,dual alias_1,:param_2,:param_3 alias_3").

-define(FROM_01_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual,
    Dual Alias_1,
    :param_2,
    :param_3 Alias_3").

%%------------------------------------------------------------------------------
%% FROM 02 - ALIAS table with SCHEMA.
%%------------------------------------------------------------------------------

-define(FROM_02, "
select *
from schema_0.dual,schema_1.dual alias_1").

-define(FROM_02_RESULT_DEFAULT, "SELECT
    *
FROM
    Schema_0.Dual,
    Schema_1.Dual Alias_1").

%%------------------------------------------------------------------------------
%% FROM 03 -ALIAS table with DBLINK.
%%------------------------------------------------------------------------------

-define(FROM_03, "
select *
from dual\"@link_0\",dual\"@link_1\" alias_1").

-define(FROM_03_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual\"@link_0\",
    Dual\"@link_1\" Alias_1").

%%------------------------------------------------------------------------------
%% FROM 04 - ALIAS table with SCHEMA and DBLINK.
%%------------------------------------------------------------------------------

-define(FROM_04, "
select *
from schema_0.dual\"@link_0\",schema_1.dual\"@link_1\" alias_1").

-define(FROM_04_RESULT_DEFAULT, "SELECT
    *
FROM
    Schema_0.Dual\"@link_0\",
    Schema_1.Dual\"@link_1\" Alias_1").

%%------------------------------------------------------------------------------
%% FROM 05 - FROM table.
%%------------------------------------------------------------------------------

-define(FROM_05, "
select *
from table_1,table_2 alias_2,table_3").

-define(FROM_05_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1,
    Table_2 Alias_2,
    Table_3").

%%------------------------------------------------------------------------------
%% FROM 06 - FROM nested subqueries.
%%------------------------------------------------------------------------------

-define(FROM_06, "
select column_1
from (select column_2 from (select column_3 from dual))").

-define(FROM_06_RESULT_DEFAULT, "SELECT
    Column_1
FROM
    (SELECT
        Column_2
    FROM
        (SELECT
            Column_3
        FROM
            Dual))").

%%------------------------------------------------------------------------------
%% FROM 07 - FROM subquery.
%%------------------------------------------------------------------------------

-define(FROM_07, "
select *
from (select column_1 from table_1),(select column_2 from table_2) alias_2,(select column_3 from table_3)").

-define(FROM_07_RESULT_DEFAULT, "SELECT
    *
FROM
    (SELECT
        Column_1
    FROM
        Table_1),
    (SELECT
        Column_2
    FROM
        Table_2) Alias_2,
    (SELECT
        Column_3
    FROM
        Table_3)").

%%------------------------------------------------------------------------------
%% FROM 08 - FROM DBLINK.
%%------------------------------------------------------------------------------

-define(FROM_08, "
select *
from (schema_1.table_1\"@dblink_1\" full join schema_2.table_2\"@dblink_2\" )").

-define(FROM_08_RESULT_DEFAULT, "SELECT
    *
FROM
    Schema_1.Table_1\"@dblink_1\"
    FULL JOIN
    Schema_2.Table_2\"@dblink_2\"").

%%------------------------------------------------------------------------------
%% FROM 09 - PARAM.
%%------------------------------------------------------------------------------

-define(FROM_09, "
select *
from :t").

-define(FROM_09_RESULT_DEFAULT, "SELECT
    *
FROM
    :t").

%%------------------------------------------------------------------------------
%% FROM 10 - JOIN.
%%------------------------------------------------------------------------------

-define(FROM_10, "
select *
from :param_1 alias_1 join :param_2 alias_2 on column_1 = column_2").

-define(FROM_10_RESULT_DEFAULT, "SELECT
    *
FROM
    :param_1 Alias_1
    JOIN
    :param_2 Alias_2
    ON Column_1 = Column_2").

%%------------------------------------------------------------------------------
%% GRANT 01 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_01, "
grant create table to user_1").

-define(GRANT_01_RESULT_DEFAULT, "GRANT
    CREATE TABLE
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 02 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_02, "
grant drop any table to user_1, user_2").

-define(GRANT_02_RESULT_DEFAULT, "GRANT
    DROP ANY TABLE
TO
    User_1, User_2").

%%------------------------------------------------------------------------------
%% GRANT 03 - TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_03, "
grant super_role to user_1 with delegate option").

-define(GRANT_03_RESULT_DEFAULT, "GRANT
    Super_Role
TO
    User_1
WITH DELEGATE OPTION").

%%------------------------------------------------------------------------------
%% GRANT 04 - TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_04, "
grant admin to user_1 with admin option").

-define(GRANT_04_RESULT_DEFAULT, "GRANT
    ADMIN
TO
    User_1
WITH ADMIN OPTION").

%%------------------------------------------------------------------------------
%% GRANT 05 - ON & TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_05, "
grant select on table_1 to user_1 with grant option").

-define(GRANT_05_RESULT_DEFAULT, "GRANT
    SELECT
ON
    Table_1
TO
    User_1
WITH GRANT OPTION").

%%------------------------------------------------------------------------------
%% GRANT 06 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_06, "
grant all on table_1 to user_1").

-define(GRANT_06_RESULT_DEFAULT, "GRANT
    ALL
ON
    Table_1
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 07 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_07, "
grant delete, insert, select, update on table_1 to user_1").

-define(GRANT_07_RESULT_DEFAULT, "GRANT
    DELETE, INSERT, SELECT, UPDATE
ON
    Table_1
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 08 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_08, "
grant role_1 to user_1").

-define(GRANT_08_RESULT_DEFAULT, "GRANT
    Role_1
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 09 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_09, "
grant privilege_1, privilege_2 to user_1").

-define(GRANT_09_RESULT_DEFAULT, "GRANT
    Privilege_1, Privilege_2
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 10 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_10, "
grant privilege_1, privilege_2, privilege_3, privilege_4, privilege_5 to user_1").

-define(GRANT_10_RESULT_DEFAULT, "GRANT
    Privilege_1, Privilege_2, Privilege_3, Privilege_4, Privilege_5
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 11 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_11, "
grant select on ddtable to user_1").

-define(GRANT_11_RESULT_DEFAULT, "GRANT
    SELECT
ON
    Ddtable
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 12 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_12, "
grant select on schema1.ddtable to user_1").

-define(GRANT_12_RESULT_DEFAULT, "GRANT
    SELECT
ON
    Schema1.Ddtable
TO
    User_1").

%%------------------------------------------------------------------------------
%% GRANT 13 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_13, "
grant all privileges on schema1.ddtable to role_2").

-define(GRANT_13_RESULT_DEFAULT, "GRANT
    ALL PRIVILEGES
ON
    Schema1.Ddtable
TO
    Role_2").

%%------------------------------------------------------------------------------
%% GRANT 14 - TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_14, "
grant manage_system to test_user_1 with admin option").

-define(GRANT_14_RESULT_DEFAULT, "GRANT
    Manage_System
TO
    Test_User_1
WITH ADMIN OPTION").

%%------------------------------------------------------------------------------
%% GRANT 15 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_15, "
grant create table to user_1,user_2,user_3,user_4").

-define(GRANT_15_RESULT_DEFAULT, "GRANT
    CREATE TABLE
TO
    User_1, User_2, User_3, User_4").

%%------------------------------------------------------------------------------
%% GROUP BY 01 - very simple.
%%------------------------------------------------------------------------------

-define(GROUP_BY_01, "
select * from dual
group by column_1, column_2").

-define(GROUP_BY_01_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    Column_1, Column_2").

%%------------------------------------------------------------------------------
%% GROUP BY 02 - column_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_02, "
select * from dual
group by column_1,table_1.column_1,schema_1.table_1.column_1,column_1|:x:y|,table_1.column_1|:x:y|,schema_1.table_1.column_1|:x:y|").

-define(GROUP_BY_02_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    Column_1, Table_1.Column_1, Schema_1.Table_1.Column_1, Column_1|:x:y|,
    Table_1.Column_1|:x:y|, Schema_1.Table_1.Column_1|:x:y|").

%%------------------------------------------------------------------------------
%% GROUP BY 03 - column_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_03, "
select * from dual
group by column_1(+),table_1.column_1(+),schema_1.table_1.column_1(+),table_1.*,schema_1.table_1.*").

-define(GROUP_BY_03_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    Column_1(+), Table_1.Column_1(+), Schema_1.Table_1.Column_1(+), Table_1.*,
    Schema_1.Table_1.*").

%%------------------------------------------------------------------------------
%% GROUP BY 04 - function_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_04, "
select * from dual
group by function_1(param_1)|:b[f(p:q)]|").

-define(GROUP_BY_04_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    Function_1(Param_1)|:b[f(p:q)]|").

%%------------------------------------------------------------------------------
%% GROUP BY 05 - function_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_05, "
select * from dual
group by function_1(param_1,param_2),package_1.function_1(param_1,param_2),schema_1.package_1.function_1(param_1,param_2),function_1(param_1,param_2)|:b[f(p:q)]|,package_1.function_1(param_1,param_2)|:b[f(p:q)]|,schema_1.package_1.function_1(param_1,param_2)|:b[f(p:q)]|").

-define(GROUP_BY_05_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    Function_1(Param_1, Param_2), Package_1.Function_1(Param_1, Param_2),
    Schema_1.Package_1.Function_1(Param_1, Param_2), Function_1(Param_1, Param_2
    )|:b[f(p:q)]|, Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|,
    Schema_1.Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|").

%%------------------------------------------------------------------------------
%% GROUP BY 06 - function_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_06, "
select * from dual
group by decode(distinct column_1)|:b[f(p:q)]|,decode(all 6)").

-define(GROUP_BY_06_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    DECODE(DISTINCT Column_1)|:b[f(p:q)]|, DECODE(ALL 6)").

%%------------------------------------------------------------------------------
%% GROUP BY 07 - function_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_07, "
select * from dual
group by decode,decode(param_1,param_2),decode(*),decode(distinct column_1),decode(all 6)").

-define(GROUP_BY_07_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    DECODE, DECODE(Param_1, Param_2), DECODE(*), DECODE(DISTINCT Column_1),
    DECODE(ALL 6)").

%%------------------------------------------------------------------------------
%% GROUP BY 08 - nested function_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_08, "
select * from dual
group by function_1(param_11,function_21(param_21,function_31(param_31,param_32,param_33),param_23),param_13)").

-define(GROUP_BY_08_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    Function_1(Param_11, Function_21(Param_21, Function_31(Param_31, Param_32,
    Param_33), Param_23), Param_13)").

%%------------------------------------------------------------------------------
%% GROUP BY 09 - COLUMN.
%%------------------------------------------------------------------------------

-define(GROUP_BY_09, "
select * from dual
group by column_1, column_2,column_3,column_4").

-define(GROUP_BY_09_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
GROUP BY
    Column_1, Column_2, Column_3, Column_4").

%%------------------------------------------------------------------------------
%% HAVING 01 - very simple.
%%------------------------------------------------------------------------------

-define(HAVING_01, "
select * from dual
having column_1 = column_2").

-define(HAVING_01_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
HAVING
    Column_1 = Column_2").

%%------------------------------------------------------------------------------
%% HAVING 02 - simple.
%%------------------------------------------------------------------------------

-define(HAVING_02, "
select * from dual
having column_1 = column_2 and column_3 = column_4").

-define(HAVING_02_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
HAVING
    Column_1 = Column_2
    AND Column_3 = Column_4").

%%------------------------------------------------------------------------------
%% INSERT 01 - COLUMNS & SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_01, "
insert into table_1 (column_1,column_2)
select column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6
returning column_1,column_2 into :a,:b").

-define(INSERT_01_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2)
    SELECT
        Column_1, Column_2
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6
RETURNING
    Column_1, Column_2
INTO
    :a, :b").

%%------------------------------------------------------------------------------
%% INSERT 02 - COLUMNS & SELECT.
%%------------------------------------------------------------------------------

-define(INSERT_02, "
insert into table_1 (column_1,column_2)
select column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6").

-define(INSERT_02_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2)
    SELECT
        Column_1, Column_2
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6").

%%------------------------------------------------------------------------------
%% INSERT 03 - COLUMNS & VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_03, "
insert into table_1 (column_1,column_2)
values (value_1,value_2)
returning column_1,column_2 into :a,:b").

-define(INSERT_03_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2)
VALUES
    (Value_1, Value_2)
RETURNING
    Column_1, Column_2
INTO
    :a, :b").

%%------------------------------------------------------------------------------
%% INSERT 04 - COLUMNS & VALUES.
%%------------------------------------------------------------------------------

-define(INSERT_04, "
insert into table_1 (column_1,column_2)
values (value_1,value_2)").

-define(INSERT_04_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2)
VALUES
    (Value_1, Value_2)").

%%------------------------------------------------------------------------------
%% INSERT 05 - COLUMNS & SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_05, "
insert into table_1 (column_1,column_2,column_3,column_4)
select column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6
returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d").

-define(INSERT_05_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2, Column_3, Column_4)
    SELECT
        Column_1, Column_2, Column_3, Column_4
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6
RETURNING
    Column_1, Column_2, Column_3, Column_4
INTO
    :a, :b, :c, :d").

%%------------------------------------------------------------------------------
%% INSERT 06 - COLUMNS & SELECT.
%%------------------------------------------------------------------------------

-define(INSERT_06, "
insert into table_1 (column_1,column_2,column_3,column_4)
select column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6").

-define(INSERT_06_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2, Column_3, Column_4)
    SELECT
        Column_1, Column_2, Column_3, Column_4
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6").

%%------------------------------------------------------------------------------
%% INSERT 07 - COLUMNS & VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_07, "
insert into table_1 (column_1,column_2,column_3,column_4)
values (value_1,value_2,value_3,value_4)
returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d").

-define(INSERT_07_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2, Column_3, Column_4)
VALUES
    (Value_1, Value_2, Value_3, Value_4)
RETURNING
    Column_1, Column_2, Column_3, Column_4
INTO
    :a, :b, :c, :d").

%%------------------------------------------------------------------------------
%% INSERT 08 - COLUMNS & VALUES.
%%------------------------------------------------------------------------------

-define(INSERT_08, "
insert into table_1 (column_1,column_2,column_3,column_4)
values (value_1,value_2,value_3,value_4)").

-define(INSERT_08_RESULT_DEFAULT, "INSERT INTO
    Table_1 (
        Column_1, Column_2, Column_3, Column_4)
VALUES
    (Value_1, Value_2, Value_3, Value_4)").

%%------------------------------------------------------------------------------
%% INSERT 09 - RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_09, "
insert into table_1
returning column_1,column_2 into :a,:b").

-define(INSERT_09_RESULT_DEFAULT, "INSERT INTO
    Table_1
RETURNING
    Column_1, Column_2
INTO
    :a, :b").

%%------------------------------------------------------------------------------
%% INSERT 10 - SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_10, "
insert into table_1
select column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6
returning column_1,column_2 into :a,:b").

-define(INSERT_10_RESULT_DEFAULT, "INSERT INTO
    Table_1
    SELECT
        Column_1, Column_2
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6
RETURNING
    Column_1, Column_2
INTO
    :a, :b").

%%------------------------------------------------------------------------------
%% INSERT 11 - SELECT.
%%------------------------------------------------------------------------------

-define(INSERT_11, "
insert into table_1
select column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6").

-define(INSERT_11_RESULT_DEFAULT, "INSERT INTO
    Table_1
    SELECT
        Column_1, Column_2
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6").

%%------------------------------------------------------------------------------
%% INSERT 12 - SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_12, "
insert into table_1
select column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6
returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d").

-define(INSERT_12_RESULT_DEFAULT, "INSERT INTO
    Table_1
    SELECT
        Column_1, Column_2, Column_3, Column_4
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6
RETURNING
    Column_1, Column_2, Column_3, Column_4
INTO
    :a, :b, :c, :d").

%%------------------------------------------------------------------------------
%% INSERT 13 - SELECT.
%%------------------------------------------------------------------------------

-define(INSERT_13, "
insert into table_1
select column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6").

-define(INSERT_13_RESULT_DEFAULT, "INSERT INTO
    Table_1
    SELECT
        Column_1, Column_2, Column_3, Column_4
    FROM
        Table_2
    WHERE
        Column_3 = Column_4
        AND Column_5 = Column_6").

%%------------------------------------------------------------------------------
%% INSERT 14 - VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_14, "
insert into table_1
values (value_1,value_2)
returning column_1,column_2 into :a,:b").

-define(INSERT_14_RESULT_DEFAULT, "INSERT INTO
    Table_1
VALUES
    (Value_1, Value_2)
RETURNING
    Column_1, Column_2
INTO
    :a, :b").

%%------------------------------------------------------------------------------
%% INSERT 15 - VALUES.
%%------------------------------------------------------------------------------

-define(INSERT_15, "
insert into table_1
values (value_1,value_2)").

-define(INSERT_15_RESULT_DEFAULT, "INSERT INTO
    Table_1
VALUES
    (Value_1, Value_2)").

%%------------------------------------------------------------------------------
%% INSERT 16 - VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_16, "
insert into table_1
values (value_1,value_2,value_3,value_4)
returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d").

-define(INSERT_16_RESULT_DEFAULT, "INSERT INTO
    Table_1
VALUES
    (Value_1, Value_2, Value_3, Value_4)
RETURNING
    Column_1, Column_2, Column_3, Column_4
INTO
    :a, :b, :c, :d").

%%------------------------------------------------------------------------------
%% INSERT 17 - VALUES.
%%------------------------------------------------------------------------------

-define(INSERT_17, "
insert into table_1
values (value_1,value_2,value_3,value_4)").

-define(INSERT_17_RESULT_DEFAULT, "INSERT INTO
    Table_1
VALUES
    (Value_1, Value_2, Value_3, Value_4)").

%%------------------------------------------------------------------------------
%% INSERT 18 - NONE.
%%------------------------------------------------------------------------------

-define(INSERT_18, "
insert into table_1").

-define(INSERT_18_RESULT_DEFAULT, "INSERT INTO
    Table_1").

%%------------------------------------------------------------------------------
%% JOIN 01 - JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_01, "
select *
from table_1 join table_2 using (column_1)").

-define(JOIN_01_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    JOIN
    Table_2
    USING (Column_1)").

%%------------------------------------------------------------------------------
%% JOIN 02 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_02, "
select *
from table_1 inner join table_2 using (column_1)").

-define(JOIN_02_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    INNER JOIN
    Table_2
    USING (Column_1)").

%%------------------------------------------------------------------------------
%% JOIN 03 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_03, "
select *
from table_1 inner join table_2 using (column_1,column_2,column_3)").

-define(JOIN_03_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    INNER JOIN
    Table_2
    USING (Column_1, Column_2, Column_3)").

%%------------------------------------------------------------------------------
%% JOIN 04 - CROSS.
%%------------------------------------------------------------------------------

-define(JOIN_04, "
select *
from table_1 cross join table_2").

-define(JOIN_04_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    CROSS JOIN
    Table_2").

%%------------------------------------------------------------------------------
%% JOIN 05 - NATURAL JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_05, "
select *
from table_1 natural join table_2").

-define(JOIN_05_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    NATURAL JOIN
    Table_2").

%%------------------------------------------------------------------------------
%% JOIN 06 - NATURAL INNER JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_06, "
select *
from table_1 natural inner join table_2").

-define(JOIN_06_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    NATURAL INNER JOIN
    Table_2").

%%------------------------------------------------------------------------------
%% JOIN 07 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_07, "
select *
from table_1 inner join table_2 using (column_1,column_2,column_3,column_4)").

-define(JOIN_07_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    INNER JOIN
    Table_2
    USING (Column_1, Column_2, Column_3, Column_4)").

%%------------------------------------------------------------------------------
%% JOIN 08 - JOIN ON.
%%------------------------------------------------------------------------------

-define(JOIN_08, "
select *
from table_1 join table_2 on table_1.column_1 = table_2.column_2").

-define(JOIN_08_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    JOIN
    Table_2
    ON Table_1.Column_1 = Table_2.Column_2").

%%------------------------------------------------------------------------------
%% JOIN 09 - INNER JOIN ON.
%%------------------------------------------------------------------------------

-define(JOIN_09, "
select *
from table_1 inner join table_2 on table_1.column_1 = table_2.column_2 or table_1.column_3 = table_2.column_4").

-define(JOIN_09_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    INNER JOIN
    Table_2
    ON Table_1.Column_1 = Table_2.Column_2
    OR Table_1.Column_3 = Table_2.Column_4").

%%------------------------------------------------------------------------------
%% JOIN 10 - INNER JOIN ON.
%%------------------------------------------------------------------------------

-define(JOIN_10, "
select *
from table_1 inner join table_2
on table_1.column_1 = table_2.column_2
or table_1.column_3 = table_2.column_4
and table_1.column_5 = table_2.column_6").

-define(JOIN_10_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    INNER JOIN
    Table_2
    ON Table_1.Column_1 = Table_2.Column_2
    OR Table_1.Column_3 = Table_2.Column_4
    AND Table_1.Column_5 = Table_2.Column_6").

%%------------------------------------------------------------------------------
%% JOIN 11 - OUTER JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_11, "
select *
from table_1 left outer join table_2").

-define(JOIN_11_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    LEFT OUTER JOIN
    Table_2").

%%------------------------------------------------------------------------------
%% JOIN 12 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_12, "
select *
from table_1 left outer join table_2 using (column_1)").

-define(JOIN_12_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    LEFT OUTER JOIN
    Table_2
    USING (Column_1)").

%%------------------------------------------------------------------------------
%% JOIN 13 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_13, "
select *
from table_1 left outer join table_2 on column_1 <> column_2").

-define(JOIN_13_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    LEFT OUTER JOIN
    Table_2
    ON Column_1 <> Column_2").

%%------------------------------------------------------------------------------
%% JOIN 14 - CROSS.
%%------------------------------------------------------------------------------

-define(JOIN_14, "
select *
from table_1 partition by column_1 left outer join table_2").

-define(JOIN_14_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    PARTITION BY (Column_1)
    LEFT OUTER JOIN
    Table_2").

%%------------------------------------------------------------------------------
%% JOIN 15 - NATURAL JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_15, "
select *
from table_1 natural left outer join table_2").

-define(JOIN_15_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    NATURAL LEFT OUTER JOIN
    Table_2").

%%------------------------------------------------------------------------------
%% JOIN 16 - NATURAL INNER JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_16, "
select *
from table_1 partition by column_1 natural left outer join table_2").

-define(JOIN_16_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    PARTITION BY (Column_1)
    NATURAL LEFT OUTER JOIN
    Table_2").

%%------------------------------------------------------------------------------
%% JOIN 17 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_17, "
select *
from table_1 natural left outer join table_2 partition by column_1").

-define(JOIN_17_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    NATURAL LEFT OUTER JOIN
    Table_2
    PARTITION BY (Column_1)").

%%------------------------------------------------------------------------------
%% JOIN 18 - JOIN ON.
%%------------------------------------------------------------------------------

-define(JOIN_18, "
select *
from table_1 partition by column_1 natural left outer join table_2 partition by column_2 on column_1 = column_2").

-define(JOIN_18_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    PARTITION BY (Column_1)
    NATURAL LEFT OUTER JOIN
    Table_2
    PARTITION BY (Column_2)
    ON Column_1 = Column_2").

%%------------------------------------------------------------------------------
%% JOIN 19 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_19, "
select *
from table_1 left outer join table_2 using (column_1,column_2,column_3)").

-define(JOIN_19_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    LEFT OUTER JOIN
    Table_2
    USING (Column_1, Column_2, Column_3)").

%%------------------------------------------------------------------------------
%% JOIN 20 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_20, "
select *
from table_1 left outer join table_2 using (column_1,column_2,column_3,column_4)").

-define(JOIN_20_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    LEFT OUTER JOIN
    Table_2
    USING (Column_1, Column_2, Column_3, Column_4)").

%%------------------------------------------------------------------------------
%% JOIN 21 - MIXED JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_21, "
select *
from table_1 natural join table_2,
table_3,table_5,(select * from dual) alias_1,
table_6 join table_7 using (column_1) left outer join table_8").

-define(JOIN_21_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
    NATURAL JOIN
    Table_2,
    Table_3,
    Table_5,
    (SELECT
        *
    FROM
        Dual) Alias_1,
    Table_6
    JOIN
    Table_7
    USING (Column_1)
    LEFT OUTER JOIN
    Table_8").

%%------------------------------------------------------------------------------
%% JOIN 22 - MIXED JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_22, "
select *
from table_1,table_2 natural join table_3,table_4").

-define(JOIN_22_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1,
    Table_2
    NATURAL JOIN
    Table_3,
    Table_4").

%%------------------------------------------------------------------------------
%% JOIN 23 - DBLINK.
%%------------------------------------------------------------------------------

-define(JOIN_23, "
select *
from schema_1.table_1\"@dblink_1\" natural full join schema_2.table_2\"@dblink_2\" on column_1 = column_2").

-define(JOIN_23_RESULT_DEFAULT, "SELECT
    *
FROM
    Schema_1.Table_1\"@dblink_1\"
    NATURAL FULL JOIN
    Schema_2.Table_2\"@dblink_2\"
    ON Column_1 = Column_2").

%%------------------------------------------------------------------------------
%% JOIN 24 - DBLINK.
%%------------------------------------------------------------------------------

-define(JOIN_24, "
select *
from schema_1.table_1\"@dblink_1\" full join schema_2.table_2\"@dblink_2\" partition by column_1 on column_1 = column_2").

-define(JOIN_24_RESULT_DEFAULT, "SELECT
    *
FROM
    Schema_1.Table_1\"@dblink_1\"
    FULL JOIN
    Schema_2.Table_2\"@dblink_2\"
    PARTITION BY (Column_1)
    ON Column_1 = Column_2").

%%------------------------------------------------------------------------------
%% JOIN 25 - DBLINK.
%%------------------------------------------------------------------------------

-define(JOIN_25, "
select *
from :param_1\"@link_1\" alias_1 join :param_1\"@link_1\" alias_1 on column_1 = column_2").

-define(JOIN_25_RESULT_DEFAULT, "SELECT
    *
FROM
    :param_1\"@link_1\" Alias_1
    JOIN
    :param_1\"@link_1\" Alias_1
    ON Column_1 = Column_2").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 02.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_02, "
select column_11,column_12
from table_11
where column_13 <> column_14
and column_15 <> column_16
group by column_17, column_18
having column_19 = column_20
or column_21 = column_22
order by column_23, column_24").

-define(MISCELLANEOUS_02_RESULT_DEFAULT, "SELECT
    Column_11, Column_12
FROM
    Table_11
WHERE
    Column_13 <> Column_14
    AND Column_15 <> Column_16
GROUP BY
    Column_17, Column_18
HAVING
    Column_19 = Column_20
    OR Column_21 = Column_22
ORDER BY
    Column_23, Column_24").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 03.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_03, "
select *
from table_11
where (select column_31 from table_31) <> column_14").

-define(MISCELLANEOUS_03_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_11
WHERE
    (SELECT
        Column_31
    FROM
        Table_31) <> Column_14").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 04.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_04, "
select (select column_21 from table_21)
from table_11
where (select column_31 from table_31) <> column_14
having (select column_51 from table_51) = column_20
order by (select column_61 from table_61)").

-define(MISCELLANEOUS_04_RESULT_DEFAULT, "SELECT
    (SELECT
        Column_21
    FROM
        Table_21)
FROM
    Table_11
WHERE
    (SELECT
        Column_31
    FROM
        Table_31) <> Column_14
HAVING
    (SELECT
        Column_51
    FROM
        Table_51) = Column_20
ORDER BY
    (SELECT
        Column_61
    FROM
        Table_61)").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 05.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_05, "
select *
from table_11
where column_14 <> (select column_31 from table_31)").

-define(MISCELLANEOUS_05_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_11
WHERE
    Column_14 <> (SELECT
        Column_31
    FROM
        Table_31)").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 06.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_06, "
select *
from table_11
where (select column_31 from table_31) <> (select column_41 from table_41)").

-define(MISCELLANEOUS_06_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_11
WHERE
    (SELECT
        Column_31
    FROM
        Table_31) <> (SELECT
        Column_41
    FROM
        Table_41)").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 07.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_07, "
select 'S' || ',' || to_char(BD_DATETIME, 'YYYYMMDDHH24MISS') || ',' || 'I' || ',' || BD_IW_SCENARIO || ',' || DECODE(IS_NUMERIC(BD_MSISDN_A), 0, BD_ORIGSCA, BD_MSISDN_A) || ',' || BD_ORIGSCA || ',' || BD_MSISDN_B || ',' || '41794999021' || ',' || LTRIM(TO_CHAR(NVL(BD_IW_AMOUNT, 0.075) * 10000, '00000')) || ',' || BD_IMSI CSV from SBS1_ADMIN.BDETAIL2_HR_SEG_MASTER_09, SBS1_ADMIN.ENUM256 where to_number(ENUM_ID) <= BD_SEG_COUNT - IS_COUNT and not (BD_MSISDN_A is null) and IS_COUNT < BD_SEG_COUNT and BD_SEG_ID = BD_SEG_COUNT and BD_IW_APMN = 'CHEOR' and BD_DATETIME >= to_date('11.09.2017') and BD_DATETIME < to_date('21.09.2017') order by 1 asc").

-define(MISCELLANEOUS_07_RESULT_DEFAULT, "SELECT
    'S' || ',' || TO_CHAR(Bd_Datetime, 'YYYYMMDDHH24MISS') || ',' || 'I' || ','
    || Bd_Iw_Scenario || ',' || DECODE(Is_Numeric(Bd_Msisdn_A), 0, Bd_Origsca,
    Bd_Msisdn_A) || ',' || Bd_Origsca || ',' || Bd_Msisdn_B || ',' ||
    '41794999021' || ',' || LTRIM(TO_CHAR(NVL(Bd_Iw_Amount, 0.075) * 10000,
    '00000')) || ',' || Bd_Imsi Csv
FROM
    Sbs1_Admin.Bdetail2_Hr_Seg_Master_09,
    Sbs1_Admin.Enum256
WHERE
    To_Number(Enum_Id) <= Bd_Seg_Count - Is_Count
    AND NOT (Bd_Msisdn_A IS NULL)
    AND Is_Count < Bd_Seg_Count
    AND Bd_Seg_Id = Bd_Seg_Count
    AND Bd_Iw_Apmn = 'CHEOR'
    AND Bd_Datetime >= TO_DATE('11.09.2017')
    AND Bd_Datetime < TO_DATE('21.09.2017')
ORDER BY
    1 ASC").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 08.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_08, "
select SUBSTR(SED_ORDER, 1, 10) Day, SUM(SED_COUNT1) Mt_SMS, 10 * SUM(SED_COUNT2) Mo_SMSx10, SED_ETID from SETDETAIL where SED_ETID = :SQLT_STR_CDR_TYPE and not (SED_TARID in ('X', 'V', 'S', 'P', 'T')) and SED_ORDER like to_char(sysdate, 'YYYY-MM') || '%' group by SUBSTR(SED_ORDER, 1, 10), SED_ETID order by SUBSTR(SED_ORDER, 1, 10) asc, 2 asc").

-define(MISCELLANEOUS_08_RESULT_DEFAULT, "SELECT
    Substr(Sed_Order, 1, 10) Day, SUM(Sed_Count1) Mt_Sms, 10 * SUM(Sed_Count2)
    Mo_Smsx10, Sed_Etid
FROM
    Setdetail
WHERE
    Sed_Etid = :SQLT_STR_CDR_TYPE
    AND NOT (Sed_Tarid IN ('X', 'V', 'S', 'P', 'T'))
    AND Sed_Order LIKE TO_CHAR(Sysdate, 'YYYY-MM') || '%'
GROUP BY
    Substr(Sed_Order, 1, 10), Sed_Etid
ORDER BY
    Substr(Sed_Order, 1, 10) ASC, 2 ASC").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 09.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_09, "
select substr(sed_order, 1, 10) day, sum(sed_total) / sum(sed_count1) avg_charge, sum(sed_total) total_charge, sum(sed_count1) mt_count, sum(sed_count2) mo_count, sed_etid from setdetail where sed_etid = :sqlt_str_cdr_type and not (sed_tarid in ('x', 'v', 's', 'p', 't')) and sed_order like to_char(sysdate, 'yyyy-mm') || '%' group by substr(sed_order, 1, 10), sed_etid order by substr(sed_order, 1, 10) asc, 2 asc").

-define(MISCELLANEOUS_09_RESULT_DEFAULT, "SELECT
    Substr(Sed_Order, 1, 10) Day, SUM(Sed_Total) / SUM(Sed_Count1) Avg_Charge,
    SUM(Sed_Total) Total_Charge, SUM(Sed_Count1) Mt_Count, SUM(Sed_Count2)
    Mo_Count, Sed_Etid
FROM
    Setdetail
WHERE
    Sed_Etid = :sqlt_str_cdr_type
    AND NOT (Sed_Tarid IN ('x', 'v', 's', 'p', 't'))
    AND Sed_Order LIKE TO_CHAR(Sysdate, 'yyyy-mm') || '%'
GROUP BY
    Substr(Sed_Order, 1, 10), Sed_Etid
ORDER BY
    Substr(Sed_Order, 1, 10) ASC, 2 ASC").

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 10.
%%------------------------------------------------------------------------------

-define(MISCELLANEOUS_10, "
select /*+ NO_INDEX(BDETAIL) */ BD_ID, BD_SRCTYPE, BD_DATETIME, BD_DEMO, BD_BIHID, BD_MAPSID, BD_BIRECNO, BD_BOHID1, BD_PACSID1, BD_BORECNO1, BD_BOHID2, BD_PACSID2, BD_BORECNO2, BD_BOHID3, BD_PACSID3, BD_BORECNO3, BD_CONID, BD_PMVID, BD_TARID, BD_NPI_A, BD_TON_A, BD_PID_A, BD_MSISDN_A, BD_TON_B, BD_NPI_B, BD_PID_B, BD_MSISDN_B, BD_IMSI, BD_LENGTH, BD_PREPAID, BD_NODENAME, BD_SERVICE, BD_SHORTID, BD_ITEMNO, BD_KEYWORD, BD_LANGID, BD_REQTYPE, BD_BILLRATE, BD_AMOUNTCU, BD_RETSHAREPV, BD_RETSHAREMO, BD_VSMSCID, BD_CONSOLIDATION, BD_STATUS, BD_TERMDATE, BD_SETTLING, BD_SETID, BD_STORAGE, BD_DBIDSTORE, BD_DATESTORE, BD_ARCHIVE, BD_DBIDARCH, BD_DATEARCH, BD_RECTYPE, BD_IDENTITY, BD_REQSERV, BD_SUBSERV, BD_OPERATION, BD_MULTSM, BD_SMSEQ, BD_ANSTYPE, BD_RETCODE, BD_RESPMEDIA, BD_SMSCRESP, BD_TMINODE, BD_REQID, BD_SERVKEY, BD_BILLTEXT, BD_LOCATION, BD_INFO, BD_ZNID, BD_NETWORK, BD_TPID, BD_STYPE, BD_BILLTIME, BD_MSGID, BD_TRCLASS, BD_AMOUNTTR, BD_TOCID, BD_CDRTID, BD_INT, BD_IW, BD_AAATS, BD_CONSTID, BD_ERRID, BD_BILLED, BD_RATED, BD_PACIDHB, BD_OUTPUTHB, BD_BILLINGDOMAIN, BD_TRANSPORTMEDIUM, BD_CATEGORY, BD_TAXRATE, BD_TESTMODE, BD_REQUESTID, BD_COUNTTR, BD_VSPRCID, BD_ORIGSUBMIT, BD_ONLINECHARGE, BD_LTTREQRECCOUNT, BD_LTTDNRECCOUNT, BD_LTTDNDATETIME, BD_GART, BD_SHOW, BD_CAMPAIGN from BDETAIL where BD_DATETIME >= :SQLT_DAT_FROM and BD_DATETIME < :SQLT_DAT_UNTIL and DECODE(BD_AMOUNTCU, 0.0, '0', '1') || BD_BILLED || DECODE(UPPER(BD_PACIDHB), '<REVAH_PACID>', '1', '0') || BD_MAPSID || DECODE(SUBSTR(BD_MSISDN_A, 1, 5), '42377', 'F', 'S') || BD_PREPAID like :SQLT_STR_CODE").

-define(MISCELLANEOUS_10_RESULT_DEFAULT, "SELECT /*+ NO_INDEX(BDETAIL) */
    Bd_Id, Bd_Srctype, Bd_Datetime, Bd_Demo, Bd_Bihid, Bd_Mapsid, Bd_Birecno,
    Bd_Bohid1, Bd_Pacsid1, Bd_Borecno1, Bd_Bohid2, Bd_Pacsid2, Bd_Borecno2,
    Bd_Bohid3, Bd_Pacsid3, Bd_Borecno3, Bd_Conid, Bd_Pmvid, Bd_Tarid, Bd_Npi_A,
    Bd_Ton_A, Bd_Pid_A, Bd_Msisdn_A, Bd_Ton_B, Bd_Npi_B, Bd_Pid_B, Bd_Msisdn_B,
    Bd_Imsi, Bd_Length, Bd_Prepaid, Bd_Nodename, Bd_Service, Bd_Shortid,
    Bd_Itemno, Bd_Keyword, Bd_Langid, Bd_Reqtype, Bd_Billrate, Bd_Amountcu,
    Bd_Retsharepv, Bd_Retsharemo, Bd_Vsmscid, Bd_Consolidation, Bd_Status,
    Bd_Termdate, Bd_Settling, Bd_Setid, Bd_Storage, Bd_Dbidstore, Bd_Datestore,
    Bd_Archive, Bd_Dbidarch, Bd_Datearch, Bd_Rectype, Bd_Identity, Bd_Reqserv,
    Bd_Subserv, Bd_Operation, Bd_Multsm, Bd_Smseq, Bd_Anstype, Bd_Retcode,
    Bd_Respmedia, Bd_Smscresp, Bd_Tminode, Bd_Reqid, Bd_Servkey, Bd_Billtext,
    Bd_Location, Bd_Info, Bd_Znid, Bd_Network, Bd_Tpid, Bd_Stype, Bd_Billtime,
    Bd_Msgid, Bd_Trclass, Bd_Amounttr, Bd_Tocid, Bd_Cdrtid, Bd_Int, Bd_Iw,
    Bd_Aaats, Bd_Constid, Bd_Errid, Bd_Billed, Bd_Rated, Bd_Pacidhb, Bd_Outputhb
    , Bd_Billingdomain, Bd_Transportmedium, Bd_Category, Bd_Taxrate, Bd_Testmode
    , Bd_Requestid, Bd_Counttr, Bd_Vsprcid, Bd_Origsubmit, Bd_Onlinecharge,
    Bd_Lttreqreccount, Bd_Lttdnreccount, Bd_Lttdndatetime, Bd_Gart, Bd_Show,
    Bd_Campaign
FROM
    Bdetail
WHERE
    Bd_Datetime >= :SQLT_DAT_FROM
    AND Bd_Datetime < :SQLT_DAT_UNTIL
    AND DECODE(Bd_Amountcu, 0.0, '0', '1') || Bd_Billed ||
    DECODE(UPPER(Bd_Pacidhb), '<REVAH_PACID>', '1', '0') || Bd_Mapsid ||
    DECODE(Substr(Bd_Msisdn_A, 1, 5), '42377', 'F', 'S') || Bd_Prepaid LIKE
    :SQLT_STR_CODE").

%%------------------------------------------------------------------------------
%% MULTIPLE 01 - CREATE.
%%------------------------------------------------------------------------------

-define(MULTIPLE_01, "
create table hr_regions
(
    region_id   integer          not null primary key,
    region_name varchar2(25)
);
create table hr_jobs
(
    job_id     varchar2(10)      not null primary key,
    job_title  varchar2(35)      not null,
    min_salary number(6,0),
    max_salary number(6,0)
);").

-define(MULTIPLE_01_RESULT_DEFAULT, "CREATE TABLE
    Hr_Regions (
        Region_Id INTEGER NOT NULL PRIMARY KEY,
        Region_Name VARCHAR2(25)
    );
CREATE TABLE
    Hr_Jobs (
        Job_Id VARCHAR2(10) NOT NULL PRIMARY KEY,
        Job_Title VARCHAR2(35) NOT NULL,
        Min_Salary NUMBER(6,0),
        Max_Salary NUMBER(6,0)
    )").

%%------------------------------------------------------------------------------
%% MULTIPLE 02 - INSERT.
%%------------------------------------------------------------------------------

-define(MULTIPLE_02, "
insert into hr_regions (region_id,region_name) values (1,'europe');
insert into hr_regions (region_id,region_name) values (2,'americas');
insert into hr_regions (region_id,region_name) values (3,'asia');
insert into hr_regions (region_id,region_name) values (4,'middle east and africa');").

-define(MULTIPLE_02_RESULT_DEFAULT, "INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (1, 'europe');
INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (2, 'americas');
INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (3, 'asia');
INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (4, 'middle east and africa')").

%%------------------------------------------------------------------------------
%% MULTIPLE 03 - INSERT.
%%------------------------------------------------------------------------------

-define(MULTIPLE_03, "
insert into hr_regions (region_id,region_name) values (1,'europe');
name_label_1;
insert into hr_regions (region_id,region_name) values (2,'americas');
insert into hr_regions (region_id,region_name) values (3,'asia');
name_label_2;
insert into hr_regions (region_id,region_name) values (4,'middle east and africa');
name_label_3").

-define(MULTIPLE_03_RESULT_DEFAULT, "INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (1, 'europe');
name_label_1;
INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (2, 'americas');
INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (3, 'asia');
name_label_2;
INSERT INTO
    Hr_Regions (
        Region_Id, Region_Name)
VALUES
    (4, 'middle east and africa');
name_label_3").

%%------------------------------------------------------------------------------
%% OPTION 01.
%%------------------------------------------------------------------------------

-define(OPTION_01, "
select columN_1
from tablE_1
where columN_3 <> columN_4
group by columN_5
having columN_6 = columN_7
order by columN_8").

-define(OPTION_01_RESULT_DEFAULT, "SELECT
    Column_1
FROM
    Table_1
WHERE
    Column_3 <> Column_4
GROUP BY
    Column_5
HAVING
    Column_6 = Column_7
ORDER BY
    Column_8").

-define(OPTION_01_RESULT_K_I_4_S_T, "Select
    columN_1
From
    tablE_1
Where
    columN_3 <> columN_4
Group By
    columN_5
Having
    columN_6 = columN_7
Order By
    columN_8").

-define(OPTION_01_RESULT_K_L_4_S_F, "select
    columN_1
from
    tablE_1
where
    columN_3<>columN_4
group by
    columN_5
having
    columN_6=columN_7
order by
    columN_8").

-define(OPTION_01_RESULT_L_U_4_S_T, "SELECT
    column_1
FROM
    table_1
WHERE
    column_3 <> column_4
GROUP BY
    column_5
HAVING
    column_6 = column_7
ORDER BY
    column_8").

-define(OPTION_01_RESULT_U_L_1_S_T, "select
 COLUMN_1
from
 TABLE_1
where
 COLUMN_3 <> COLUMN_4
group by
 COLUMN_5
having
 COLUMN_6 = COLUMN_7
order by
 COLUMN_8").

-define(OPTION_01_RESULT_U_L_2_S_T, "select
  COLUMN_1
from
  TABLE_1
where
  COLUMN_3 <> COLUMN_4
group by
  COLUMN_5
having
  COLUMN_6 = COLUMN_7
order by
  COLUMN_8").

-define(OPTION_01_RESULT_U_L_3_S_T, "select
   COLUMN_1
from
   TABLE_1
where
   COLUMN_3 <> COLUMN_4
group by
   COLUMN_5
having
   COLUMN_6 = COLUMN_7
order by
   COLUMN_8").

-define(OPTION_01_RESULT_U_L_4_S_T, "select
    COLUMN_1
from
    TABLE_1
where
    COLUMN_3 <> COLUMN_4
group by
    COLUMN_5
having
    COLUMN_6 = COLUMN_7
order by
    COLUMN_8").

-define(OPTION_01_RESULT_U_L_5_S_T, "select
     COLUMN_1
from
     TABLE_1
where
     COLUMN_3 <> COLUMN_4
group by
     COLUMN_5
having
     COLUMN_6 = COLUMN_7
order by
     COLUMN_8").

-define(OPTION_01_RESULT_U_L_6_S_T, "select
      COLUMN_1
from
      TABLE_1
where
      COLUMN_3 <> COLUMN_4
group by
      COLUMN_5
having
      COLUMN_6 = COLUMN_7
order by
      COLUMN_8").

-define(OPTION_01_RESULT_U_L_7_S_T, "select
       COLUMN_1
from
       TABLE_1
where
       COLUMN_3 <> COLUMN_4
group by
       COLUMN_5
having
       COLUMN_6 = COLUMN_7
order by
       COLUMN_8").

-define(OPTION_01_RESULT_U_L_8_S_T, "select
        COLUMN_1
from
        TABLE_1
where
        COLUMN_3 <> COLUMN_4
group by
        COLUMN_5
having
        COLUMN_6 = COLUMN_7
order by
        COLUMN_8").

-define(OPTION_01_RESULT_U_L___T_T, "select
\tCOLUMN_1
from
\tTABLE_1
where
\tCOLUMN_3 <> COLUMN_4
group by
\tCOLUMN_5
having
\tCOLUMN_6 = COLUMN_7
order by
\tCOLUMN_8").

%%------------------------------------------------------------------------------
%% OPTION 02.
%%------------------------------------------------------------------------------

-define(OPTION_02, "
select columN_1,+columN_2
from tablE_1
where columN_3_1 <> columN_4_1
and columN_3_2 > columN_4_2
and columN_3_3 >= columN_4_3
and columN_3_4 <= columN_4_4
group by columN_5
having columN_6 = columN_7
order by columN_8").

-define(OPTION_02_RESULT_K_L_4_S_F, "select
    columN_1, +columN_2
from
    tablE_1
where
    columN_3_1<>columN_4_1
    and columN_3_2>columN_4_2
    and columN_3_3>=columN_4_3
    and columN_3_4<=columN_4_4
group by
    columN_5
having
    columN_6=columN_7
order by
    columN_8").

%%------------------------------------------------------------------------------
%% ORDER BY 01 - very simple.
%%------------------------------------------------------------------------------

-define(ORDER_BY_01, "
select * from dual
order by column_1, column_2").

-define(ORDER_BY_01_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
ORDER BY
    Column_1, Column_2").

%%------------------------------------------------------------------------------
%% ORDER BY 02 - COLUMNS.
%%------------------------------------------------------------------------------

-define(ORDER_BY_02, "
select * from dual
order by column_1,column_2,column_3").

-define(ORDER_BY_02_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
ORDER BY
    Column_1, Column_2, Column_3").

%%------------------------------------------------------------------------------
%% ORDER BY 03 - COLUMNS.
%%------------------------------------------------------------------------------

-define(ORDER_BY_03, "
select * from dual
order by column_1,column_2,column_3,column_4").

-define(ORDER_BY_03_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
ORDER BY
    Column_1, Column_2, Column_3, Column_4").

%%------------------------------------------------------------------------------
%% PARENTHESES 01 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_01, "
select 1-3+5 from dual").

-define(PARENTHESES_01_RESULT_DEFAULT, "SELECT
    (1 - 3) + 5
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 02 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_02, "
select (1-3)+5 from dual").

-define(PARENTHESES_02_RESULT_DEFAULT, "SELECT
    (1 - 3) + 5
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 03 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_03, "
select 1-(3+5) from dual").

-define(PARENTHESES_03_RESULT_DEFAULT, "SELECT
    1 - (3 + 5)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 04 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_04, "
select 1-2+3-4 from dual").

-define(PARENTHESES_04_RESULT_DEFAULT, "SELECT
    ((1 - 2) + 3) - 4
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 05 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_05, "
select 1-(2+3-4) from dual").

-define(PARENTHESES_05_RESULT_DEFAULT, "SELECT
    1 - ((2 + 3) - 4)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 06 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_06, "
select 1-2+(3-4) from dual").

-define(PARENTHESES_06_RESULT_DEFAULT, "SELECT
    (1 - 2) + (3 - 4)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 07 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_07, "
select 1-(2+3)-4 from dual").

-define(PARENTHESES_07_RESULT_DEFAULT, "SELECT
    (1 - (2 + 3)) - 4
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 11 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_11, "
select * from dual where a=b and c=d or e=f and g=e").

-define(PARENTHESES_11_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    A = B
    AND C = D
    OR E = F
    AND G = E").

%%------------------------------------------------------------------------------
%% PARENTHESES 12 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_12, "
select * from dual where a=b and (c=d or e=f) and g=e").

-define(PARENTHESES_12_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    A = B
    AND (C = D
    OR E = F)
    AND G = E").

%%------------------------------------------------------------------------------
%% PARENTHESES 13 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_13, "
select * from dual where not a=b and (c=d or e=f) and g=e").

-define(PARENTHESES_13_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    NOT (A = B)
    AND (C = D
    OR E = F)
    AND G = E").

%%------------------------------------------------------------------------------
%% PARENTHESES 14 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_14, "
select * from dual where not a=b and not (c=d or e=f) and g=e").

-define(PARENTHESES_14_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    NOT (A = B)
    AND NOT (C = D
    OR E = F)
    AND G = E").

%%------------------------------------------------------------------------------
%% PARENTHESES 21 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_21, "
select column_1 from table_1
intersect
select column_2 from table_2
").

-define(PARENTHESES_21_RESULT_DEFAULT, "    (SELECT
        Column_1
    FROM
        Table_1)
INTERSECT
    (SELECT
        Column_2
    FROM
        Table_2)").

%%------------------------------------------------------------------------------
%% PARENTHESES 22 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_22, "
select column_1 from table_1
intersect
select column_2 from table_2
minus
select column_3 from table_3
").

-define(PARENTHESES_22_RESULT_DEFAULT, "        ((SELECT
            Column_1
        FROM
            Table_1)
    INTERSECT
        (SELECT
            Column_2
        FROM
            Table_2))
MINUS
    (SELECT
        Column_3
    FROM
        Table_3)").

%%------------------------------------------------------------------------------
%% PARENTHESES 23 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_23, "
select column_1 from table_1
intersect
select column_2 from table_2
minus
select column_3 from table_3
union
select column_4 from table_4
").

-define(PARENTHESES_23_RESULT_DEFAULT, "            (((SELECT
                Column_1
            FROM
                Table_1)
        INTERSECT
            (SELECT
                Column_2
            FROM
                Table_2))
    MINUS
        (SELECT
            Column_3
        FROM
            Table_3))
UNION
    (SELECT
        Column_4
    FROM
        Table_4)").

%%------------------------------------------------------------------------------
%% PARENTHESES 24 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_24, "
select column_1 from table_1
intersect
select column_2 from table_2
minus
select column_3 from table_3
union
select column_4 from table_4
union all
select column_5 from table_5
").

-define(PARENTHESES_24_RESULT_DEFAULT, "                ((((SELECT
                    Column_1
                FROM
                    Table_1)
            INTERSECT
                (SELECT
                    Column_2
                FROM
                    Table_2))
        MINUS
            (SELECT
                Column_3
            FROM
                Table_3))
    UNION
        (SELECT
            Column_4
        FROM
            Table_4))
UNION ALL
    (SELECT
        Column_5
    FROM
        Table_5)").

%%------------------------------------------------------------------------------
%% PARENTHESES 25 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_25, "
(select column_1 from table_1
intersect
select column_2 from table_2)
minus
select column_3 from table_3
union
select column_4 from table_4
union all
select column_5 from table_5
").

-define(PARENTHESES_25_RESULT_DEFAULT, "                ((((SELECT
                    Column_1
                FROM
                    Table_1)
            INTERSECT
                (SELECT
                    Column_2
                FROM
                    Table_2))
        MINUS
            (SELECT
                Column_3
            FROM
                Table_3))
    UNION
        (SELECT
            Column_4
        FROM
            Table_4))
UNION ALL
    (SELECT
        Column_5
    FROM
        Table_5)").

%%------------------------------------------------------------------------------
%% PARENTHESES 26 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_26, "
select column_1 from table_1
intersect
(select column_2 from table_2
minus
select column_3 from table_3)
union
select column_4 from table_4
union all
select column_5 from table_5
").

-define(PARENTHESES_26_RESULT_DEFAULT, "            (((SELECT
                Column_1
            FROM
                Table_1)
        INTERSECT
                ((SELECT
                    Column_2
                FROM
                    Table_2)
            MINUS
                (SELECT
                    Column_3
                FROM
                    Table_3)))
    UNION
        (SELECT
            Column_4
        FROM
            Table_4))
UNION ALL
    (SELECT
        Column_5
    FROM
        Table_5)").

%%------------------------------------------------------------------------------
%% PARENTHESES 27 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_27, "
select column_1 from table_1
intersect
select column_2 from table_2
minus
(select column_3 from table_3
union
select column_4 from table_4)
union all
select column_5 from table_5
").

-define(PARENTHESES_27_RESULT_DEFAULT, "            (((SELECT
                Column_1
            FROM
                Table_1)
        INTERSECT
            (SELECT
                Column_2
            FROM
                Table_2))
    MINUS
            ((SELECT
                Column_3
            FROM
                Table_3)
        UNION
            (SELECT
                Column_4
            FROM
                Table_4)))
UNION ALL
    (SELECT
        Column_5
    FROM
        Table_5)").

%%------------------------------------------------------------------------------
%% PARENTHESES 28 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_28, "
select column_1 from table_1
intersect
select column_2 from table_2
minus
select column_3 from table_3
union
(select column_4 from table_4
union all
select column_5 from table_5)
").

-define(PARENTHESES_28_RESULT_DEFAULT, "            (((SELECT
                Column_1
            FROM
                Table_1)
        INTERSECT
            (SELECT
                Column_2
            FROM
                Table_2))
    MINUS
        (SELECT
            Column_3
        FROM
            Table_3))
UNION
        ((SELECT
            Column_4
        FROM
            Table_4)
    UNION ALL
        (SELECT
            Column_5
        FROM
            Table_5))").

%%------------------------------------------------------------------------------
%% PARENTHESES 29 - MINIMAL.
%%------------------------------------------------------------------------------

-define(PARENTHESES_29, "
select 1 * 2 + 3 * 4 from dual where a and b or c and d;
select 1 * (2 + 3) * 4 from dual where a and (b or c) and d;
select 1 + 2 * 3 + 4 from dual where a or b and c or d;
select (1 + 2) * (3 + 4) from dual where (a or b) and (c or d)
").

-define(PARENTHESES_29_RESULT_DEFAULT, "SELECT
    1 * 2 + 3 * 4
FROM
    Dual
WHERE
    A
    AND B
    OR C
    AND D;
SELECT
    1 * (2 + 3) * 4
FROM
    Dual
WHERE
    A
    AND (B
    OR C)
    AND D;
SELECT
    1 + 2 * 3 + 4
FROM
    Dual
WHERE
    A
    OR B
    AND C
    OR D;
SELECT
    (1 + 2) * (3 + 4)
FROM
    Dual
WHERE
    (A
    OR B)
    AND (C
    OR D)").

%%------------------------------------------------------------------------------
%% PARENTHESES 30 - MINIMAL.
%%------------------------------------------------------------------------------

-define(PARENTHESES_30, "
select 1-(3+5) from dual;
select 1-(3-5) from dual
").

-define(PARENTHESES_30_RESULT_DEFAULT, "SELECT
    1 - (3 + 5)
FROM
    Dual;
SELECT
    1 - (3 - 5)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 31 - MINIMAL.
%%------------------------------------------------------------------------------

-define(PARENTHESES_31, "
select 1-2+3-4 from dual;
select (1-2)+3-4 from dual;
select 1-2+(3-4) from dual;
select (1-2)+(3-4) from dual
").

-define(PARENTHESES_31_RESULT_DEFAULT, "SELECT
    ((1 - 2) + 3) - 4
FROM
    Dual;
SELECT
    ((1 - 2) + 3) - 4
FROM
    Dual;
SELECT
    (1 - 2) + (3 - 4)
FROM
    Dual;
SELECT
    (1 - 2) + (3 - 4)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% PARENTHESES 32 - SET.
%%------------------------------------------------------------------------------

-define(PARENTHESES_32, "
select * from table_1 union select * from table_2 intersect select * from table_3 union select * from table_4;
select * from table_1 union all select * from table_2 union select * from table_3 union all select * from table_4;
select * from table_1 minus select * from table_2 union select * from table_3 union all select * from table_4;
select * from table_1 union all select * from table_2 minus select * from table_3 union all select * from table_4;
select * from table_1 union all (select * from table_2 minus select * from table_3) union all select * from table_4;
select * from table_1 minus select * from table_2 minus select * from table_3 minus select * from table_4;
select * from table_1 minus (select * from table_2 union select * from table_3 union all select * from table_4);
(select * from table_1 union all select * from table_2) minus (select * from table_3 union all select * from table_4);
select * from table_1 minus (select * from table_2 minus select * from table_3 minus select * from table_4);
").

-define(PARENTHESES_32_RESULT_DEFAULT, "            (((SELECT
                *
            FROM
                Table_1)
        UNION
            (SELECT
                *
            FROM
                Table_2))
    INTERSECT
        (SELECT
            *
        FROM
            Table_3))
UNION
    (SELECT
        *
    FROM
        Table_4);
            (((SELECT
                *
            FROM
                Table_1)
        UNION ALL
            (SELECT
                *
            FROM
                Table_2))
    UNION
        (SELECT
            *
        FROM
            Table_3))
UNION ALL
    (SELECT
        *
    FROM
        Table_4);
            (((SELECT
                *
            FROM
                Table_1)
        MINUS
            (SELECT
                *
            FROM
                Table_2))
    UNION
        (SELECT
            *
        FROM
            Table_3))
UNION ALL
    (SELECT
        *
    FROM
        Table_4);
            (((SELECT
                *
            FROM
                Table_1)
        UNION ALL
            (SELECT
                *
            FROM
                Table_2))
    MINUS
        (SELECT
            *
        FROM
            Table_3))
UNION ALL
    (SELECT
        *
    FROM
        Table_4);
        ((SELECT
            *
        FROM
            Table_1)
    UNION ALL
            ((SELECT
                *
            FROM
                Table_2)
        MINUS
            (SELECT
                *
            FROM
                Table_3)))
UNION ALL
    (SELECT
        *
    FROM
        Table_4);
            (((SELECT
                *
            FROM
                Table_1)
        MINUS
            (SELECT
                *
            FROM
                Table_2))
    MINUS
        (SELECT
            *
        FROM
            Table_3))
MINUS
    (SELECT
        *
    FROM
        Table_4);
    (SELECT
        *
    FROM
        Table_1)
MINUS
            (((SELECT
                *
            FROM
                Table_2)
        UNION
            (SELECT
                *
            FROM
                Table_3))
    UNION ALL
        (SELECT
            *
        FROM
            Table_4));
        ((SELECT
            *
        FROM
            Table_1)
    UNION ALL
        (SELECT
            *
        FROM
            Table_2))
MINUS
        ((SELECT
            *
        FROM
            Table_3)
    UNION ALL
        (SELECT
            *
        FROM
            Table_4));
    (SELECT
        *
    FROM
        Table_1)
MINUS
            (((SELECT
                *
            FROM
                Table_2)
        MINUS
            (SELECT
                *
            FROM
                Table_3))
    MINUS
        (SELECT
            *
        FROM
            Table_4))").

%%------------------------------------------------------------------------------
%% PARENTHESES 40 - SET.
%%------------------------------------------------------------------------------

-define(PARENTHESES_40, "
select * from table_1 minus select * from table_2;").

-define(PARENTHESES_40_RESULT_DEFAULT, "    (SELECT
        *
    FROM
        Table_1)
MINUS
    (SELECT
        *
    FROM
        Table_2)").

%%------------------------------------------------------------------------------
%% PARENTHESES 41 - SET.
%%------------------------------------------------------------------------------

-define(PARENTHESES_41, "
select * from table_1 union select * from table_2;").

-define(PARENTHESES_41_RESULT_DEFAULT, "    (SELECT
        *
    FROM
        Table_1)
UNION
    (SELECT
        *
    FROM
        Table_2)").

%%------------------------------------------------------------------------------
%% PARENTHESES 42 - SET.
%%------------------------------------------------------------------------------

-define(PARENTHESES_42, "
select * from table_1 minus select * from table_2
minus
select * from table_3 minus select * from table_4").

-define(PARENTHESES_42_RESULT_DEFAULT, "            (((SELECT
                *
            FROM
                Table_1)
        MINUS
            (SELECT
                *
            FROM
                Table_2))
    MINUS
        (SELECT
            *
        FROM
            Table_3))
MINUS
    (SELECT
        *
    FROM
        Table_4)").

%%------------------------------------------------------------------------------
%% PARENTHESES 43 - SET.
%%------------------------------------------------------------------------------

-define(PARENTHESES_43, "
select * from table_1 minus
(select * from table_2 minus select * from table_3)
minus select * from table_4;").

-define(PARENTHESES_43_RESULT_DEFAULT, "        ((SELECT
            *
        FROM
            Table_1)
    MINUS
            ((SELECT
                *
            FROM
                Table_2)
        MINUS
            (SELECT
                *
            FROM
                Table_3)))
MINUS
    (SELECT
        *
    FROM
        Table_4)").

%%------------------------------------------------------------------------------
%% PLSQL 01 - ALTER.
%%------------------------------------------------------------------------------

-define(PLSQL_01, "
begin
alter user user_1 revoke connect through with role role_1, role_2,role_3,role_4;
alter user test_user_123 password expire;
alter user test_user_123 account lock;
alter user user_1 default role role_1,role_2;
alter user user_1 profile profile_1;
alter user user_1 revoke connect through with role role_1, role_2 authentication required;
alter user user_1 quota unlimited on tablespace_1;
alter user user_1 default tablespace tablespace_1;
alter user test_user_123 identified by new_password;
alter user user_1 revoke connect through with role role_1, role_2 authentication required;
alter user user_1 revoke connect through with role role_1, role_2;
alter user user_1 grant connect through enterprise users;
end;").

-define(PLSQL_01_RESULT_DEFAULT, "BEGIN
    ALTER USER
        User_1
    REVOKE CONNECT THROUGH WITH ROLE
        Role_1, Role_2, Role_3, Role_4;
    ALTER USER
        Test_User_123
    PASSWORD EXPIRE;
    ALTER USER
        Test_User_123
    ACCOUNT LOCK;
    ALTER USER
        User_1
    DEFAULT ROLE
        Role_1, Role_2;
    ALTER USER
        User_1
    PROFILE
        Profile_1;
    ALTER USER
        User_1
    REVOKE CONNECT THROUGH WITH ROLE
        Role_1, Role_2
    AUTHENTICATION REQUIRED;
    ALTER USER
        User_1
    QUOTA
        UNLIMITED ON Tablespace_1;
    ALTER USER
        User_1
    DEFAULT TABLESPACE
        Tablespace_1;
    ALTER USER
        Test_User_123
    IDENTIFIED BY
        new_password;
    ALTER USER
        User_1
    REVOKE CONNECT THROUGH WITH ROLE
        Role_1, Role_2
    AUTHENTICATION REQUIRED;
    ALTER USER
        User_1
    REVOKE CONNECT THROUGH WITH ROLE
        Role_1, Role_2;
    ALTER USER
        User_1
    GRANT CONNECT THROUGH ENTERPRISE USERS;
END").

%%------------------------------------------------------------------------------
%% PLSQL 02 - CREATE.
%%------------------------------------------------------------------------------

-define(PLSQL_02, "
begin
create index index_1 on table_1 alias_1;
create bitmap index index_1 on table_1 (column_1,column_2,column_3,column_4) norm_with 'n' filter_with 'f';
create unique index index_1 on table_1 (column_1,column_2,column_3) norm_with 'n' filter_with 'f';
create table table_1 (column_1 date, column_2 date, foreign key (fkey_1,fkey_2) references table_2);
create table schema_1.table_1 (column_1 date, column_2 date, foreign key (fkey_1,fkey_2) references schema_1.table_2 (column_8,column_9));
create index a on b (a|:d{}|) norm_with fun() -> norm end. filter_with fun mod:modfun/5.;
create index name_sort on skvhaccount (cvalue|:name|) norm_with fun imem_index:vnf_lcase_ascii/1. filter_with fun imem_index:iff_binterm_list_1/1.;
end;").

-define(PLSQL_02_RESULT_DEFAULT, "BEGIN
    CREATE INDEX
        Index_1
    ON
        Table_1 Alias_1;
    CREATE BITMAP INDEX
        Index_1
    ON
        Table_1 (Column_1, Column_2, Column_3, Column_4)
    NORM_WITH
        'n'
    FILTER_WITH
        'f';
    CREATE UNIQUE INDEX
        Index_1
    ON
        Table_1 (Column_1, Column_2, Column_3)
    NORM_WITH
        'n'
    FILTER_WITH
        'f';
    CREATE TABLE
        Table_1 (
            Column_1 DATE,
            Column_2 DATE,
            FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES Table_2
        );
    CREATE TABLE
        Schema_1.Table_1 (
            Column_1 DATE,
            Column_2 DATE,
            FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES Schema_1.Table_2 (Column_8,
            Column_9)
        );
    CREATE INDEX
        A
    ON
        B (A|:d{}|)
    NORM_WITH
        fun() -> norm end.
    FILTER_WITH
        fun mod:modfun/5.;
    CREATE INDEX
        Name_Sort
    ON
        Skvhaccount (Cvalue|:name|)
    NORM_WITH
        fun imem_index:vnf_lcase_ascii/1.
    FILTER_WITH
        fun imem_index:iff_binterm_list_1/1.;
END").

%%------------------------------------------------------------------------------
%% PLSQL 03 - DELETE.
%%------------------------------------------------------------------------------

-define(PLSQL_03, "
begin
delete from table_1 return column_3,column_4 into column_5,column_6;
delete from table_1 where column_1=value_1 and column_2 = value_2;
delete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4 into column_5,column_6;
delete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4,column_5,column_6 into column_7,column_8,column_9,column_10;
end;").

-define(PLSQL_03_RESULT_DEFAULT, "BEGIN
    DELETE FROM
        Table_1
    RETURN
        Column_3, Column_4
    INTO
        Column_5, Column_6;
    DELETE FROM
        Table_1
    WHERE
        Column_1 = Value_1
        AND Column_2 = Value_2;
    DELETE FROM
        Table_1
    WHERE
        Column_1 = Value_1
        AND Column_2 = Value_2
    RETURN
        Column_3, Column_4
    INTO
        Column_5, Column_6;
    DELETE FROM
        Table_1
    WHERE
        Column_1 = Value_1
        AND Column_2 = Value_2
    RETURN
        Column_3, Column_4, Column_5, Column_6
    INTO
        Column_7, Column_8, Column_9, Column_10;
END").

%%------------------------------------------------------------------------------
%% PLSQL 04 - DROP.
%%------------------------------------------------------------------------------

-define(PLSQL_04, "
begin
drop index schema_1.index_1 from table_1;
drop role role_1;
drop table table_1, table_2 cascade;
drop table if exists table_1, table_2,table_3,table_4 restrict;
drop imem_dal_skvh table skvhtest;
drop user user_1 cascade;
end;").

-define(PLSQL_04_RESULT_DEFAULT, "BEGIN
    DROP INDEX
        Schema_1.Index_1
    FROM
        Table_1;
    DROP ROLE
        Role_1;
    DROP TABLE
        Table_1, Table_2
    CASCADE;
    DROP TABLE IF EXISTS
        Table_1, Table_2, Table_3, Table_4
    RESTRICT;
    DROP Imem_Dal_Skvh TABLE
        Skvhtest;
    DROP USER
        User_1
    CASCADE;
END").

%%------------------------------------------------------------------------------
%% PLSQL 05 - SELECT.
%%------------------------------------------------------------------------------

-define(PLSQL_05, "
begin
select * from dual,dual alias_1,:param_2,:param_3 alias_3;
select column_1 from (select column_2 from (select column_3 from dual));
select * from (select column_1 from table_1),(select column_2 from table_2) alias_2,(select column_3 from table_3);
select * from :param_1 alias_1 join :param_2 alias_2 on column_1 = column_2;
end;").

-define(PLSQL_05_RESULT_DEFAULT, "BEGIN
    SELECT
        *
    FROM
        Dual,
        Dual Alias_1,
        :param_2,
        :param_3 Alias_3;
    SELECT
        Column_1
    FROM
        (SELECT
            Column_2
        FROM
            (SELECT
                Column_3
            FROM
                Dual));
    SELECT
        *
    FROM
        (SELECT
            Column_1
        FROM
            Table_1),
        (SELECT
            Column_2
        FROM
            Table_2) Alias_2,
        (SELECT
            Column_3
        FROM
            Table_3);
    SELECT
        *
    FROM
        :param_1 Alias_1
        JOIN
        :param_2 Alias_2
        ON Column_1 = Column_2;
END").

%%------------------------------------------------------------------------------
%% PLSQL 06 - GRANT.
%%------------------------------------------------------------------------------

-define(PLSQL_06, "
begin
grant drop any table to user_1, user_2;
grant super_role to user_1 with delegate option;
grant select on table_1 to user_1 with grant option;
grant delete, insert, select, update on table_1 to user_1;
grant privilege_1, privilege_2, privilege_3, privilege_4, privilege_5 to user_1;
grant manage_system to test_user_1 with admin option;
end;").

-define(PLSQL_06_RESULT_DEFAULT, "BEGIN
    GRANT
        DROP ANY TABLE
    TO
        User_1, User_2;
    GRANT
        Super_Role
    TO
        User_1
    WITH DELEGATE OPTION;
    GRANT
        SELECT
    ON
        Table_1
    TO
        User_1
    WITH GRANT OPTION;
    GRANT
        DELETE, INSERT, SELECT, UPDATE
    ON
        Table_1
    TO
        User_1;
    GRANT
        Privilege_1, Privilege_2, Privilege_3, Privilege_4, Privilege_5
    TO
        User_1;
    GRANT
        Manage_System
    TO
        Test_User_1
    WITH ADMIN OPTION;
END").

%%------------------------------------------------------------------------------
%% PLSQL 07 - GROUP BY.
%%------------------------------------------------------------------------------

-define(PLSQL_07, "
begin
select * from dual group by column_1,table_1.column_1,schema_1.table_1.column_1,column_1|:x:y|,table_1.column_1|:x:y|,schema_1.table_1.column_1|:x:y|;
select * from dual group by function_1(param_1,param_2),package_1.function_1(param_1,param_2),schema_1.package_1.function_1(param_1,param_2),function_1(param_1,param_2)|:b[f(p:q)]|,package_1.function_1(param_1,param_2)|:b[f(p:q)]|,schema_1.package_1.function_1(param_1,param_2)|:b[f(p:q)]|;
select * from dual group by decode,decode(param_1,param_2),decode(*),decode(distinct column_1),decode(all 6);
select * from dual group by function_1(param_11,function_21(param_21,function_31(param_31,param_32,param_33),param_23),param_13);
end;").

-define(PLSQL_07_RESULT_DEFAULT, "BEGIN
    SELECT
        *
    FROM
        Dual
    GROUP BY
        Column_1, Table_1.Column_1, Schema_1.Table_1.Column_1, Column_1|:x:y|,
        Table_1.Column_1|:x:y|, Schema_1.Table_1.Column_1|:x:y|;
    SELECT
        *
    FROM
        Dual
    GROUP BY
        Function_1(Param_1, Param_2), Package_1.Function_1(Param_1, Param_2),
        Schema_1.Package_1.Function_1(Param_1, Param_2), Function_1(Param_1,
        Param_2)|:b[f(p:q)]|, Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|
        , Schema_1.Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|;
    SELECT
        *
    FROM
        Dual
    GROUP BY
        DECODE, DECODE(Param_1, Param_2), DECODE(*), DECODE(DISTINCT Column_1),
        DECODE(ALL 6);
    SELECT
        *
    FROM
        Dual
    GROUP BY
        Function_1(Param_11, Function_21(Param_21, Function_31(Param_31,
        Param_32, Param_33), Param_23), Param_13);
END").

%%------------------------------------------------------------------------------
%% PLSQL 08 - HAVING.
%%------------------------------------------------------------------------------

-define(PLSQL_08, "
begin
select * from dual having column_1 = column_2;
select * from dual having column_1 = column_2 and column_3 = column_4;
end;").

-define(PLSQL_08_RESULT_DEFAULT, "BEGIN
    SELECT
        *
    FROM
        Dual
    HAVING
        Column_1 = Column_2;
    SELECT
        *
    FROM
        Dual
    HAVING
        Column_1 = Column_2
        AND Column_3 = Column_4;
END").

%%------------------------------------------------------------------------------
%% PLSQL 09 - INSERT.
%%------------------------------------------------------------------------------

-define(PLSQL_09, "
begin
insert into table_1 (column_1,column_2) select column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6 returning column_1,column_2 into :a,:b;
insert into table_1 (column_1,column_2,column_3,column_4) values (value_1,value_2,value_3,value_4) returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;
insert into table_1 values (value_1,value_2,value_3,value_4) returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;
end;").

-define(PLSQL_09_RESULT_DEFAULT, "BEGIN
    INSERT INTO
        Table_1 (
            Column_1, Column_2)
        SELECT
            Column_1, Column_2
        FROM
            Table_2
        WHERE
            Column_3 = Column_4
            AND Column_5 = Column_6
    RETURNING
        Column_1, Column_2
    INTO
        :a, :b;
    INSERT INTO
        Table_1 (
            Column_1, Column_2, Column_3, Column_4)
    VALUES
        (Value_1, Value_2, Value_3, Value_4)
    RETURNING
        Column_1, Column_2, Column_3, Column_4
    INTO
        :a, :b, :c, :d;
    INSERT INTO
        Table_1
    VALUES
        (Value_1, Value_2, Value_3, Value_4)
    RETURNING
        Column_1, Column_2, Column_3, Column_4
    INTO
        :a, :b, :c, :d;
END").

%%------------------------------------------------------------------------------
%% PLSQL 10 - JOIN.
%%------------------------------------------------------------------------------

-define(PLSQL_10, "
begin
select * from table_1 inner join table_2 using (column_1,column_2,column_3);
select * from table_1 cross join table_2;
select * from table_1 inner join table_2 using (column_1,column_2,column_3,column_4);
select * from table_1 inner join table_2 on table_1.column_1 = table_2.column_2 or table_1.column_3 = table_2.column_4;
select * from table_1 left outer join table_2 on column_1 <> column_2;
select * from table_1 partition by column_1 natural left outer join table_2;
select * from table_1 partition by column_1 natural left outer join table_2 partition by column_2 on column_1 = column_2;
select * from table_1 natural join table_2,table_3,table_5,(select * from dual) alias_1,table_6 join table_7 using (column_1) left outer join table_8;
select * from :param_1\"@link_1\" alias_1 join :param_1\"@link_1\" alias_1 on column_1 = column_2;
end;").

-define(PLSQL_10_RESULT_DEFAULT, "BEGIN
    SELECT
        *
    FROM
        Table_1
        INNER JOIN
        Table_2
        USING (Column_1, Column_2, Column_3);
    SELECT
        *
    FROM
        Table_1
        CROSS JOIN
        Table_2;
    SELECT
        *
    FROM
        Table_1
        INNER JOIN
        Table_2
        USING (Column_1, Column_2, Column_3, Column_4);
    SELECT
        *
    FROM
        Table_1
        INNER JOIN
        Table_2
        ON Table_1.Column_1 = Table_2.Column_2
        OR Table_1.Column_3 = Table_2.Column_4;
    SELECT
        *
    FROM
        Table_1
        LEFT OUTER JOIN
        Table_2
        ON Column_1 <> Column_2;
    SELECT
        *
    FROM
        Table_1
        PARTITION BY (Column_1)
        NATURAL LEFT OUTER JOIN
        Table_2;
    SELECT
        *
    FROM
        Table_1
        PARTITION BY (Column_1)
        NATURAL LEFT OUTER JOIN
        Table_2
        PARTITION BY (Column_2)
        ON Column_1 = Column_2;
    SELECT
        *
    FROM
        Table_1
        NATURAL JOIN
        Table_2,
        Table_3,
        Table_5,
        (SELECT
            *
        FROM
            Dual) Alias_1,
        Table_6
        JOIN
        Table_7
        USING (Column_1)
        LEFT OUTER JOIN
        Table_8;
    SELECT
        *
    FROM
        :param_1\"@link_1\" Alias_1
        JOIN
        :param_1\"@link_1\" Alias_1
        ON Column_1 = Column_2;
END").

%%------------------------------------------------------------------------------
%% PLSQL 11 - MULTIPLE.
%%------------------------------------------------------------------------------

-define(PLSQL_11, "
begin
create table hr_regions(region_id integer not null primary key,region_name varchar2(25));
create table hr_jobs(
    job_id     varchar2(10)      not null primary key,
    job_title  varchar2(35)      not null,
    min_salary number(6,0),
    max_salary number(6,0)
);
insert into hr_regions (region_id,region_name) values (1,'europe');
insert into hr_regions (region_id,region_name) values (2,'americas');
insert into hr_regions (region_id,region_name) values (3,'asia');
insert into hr_regions (region_id,region_name) values (4,'middle east and africa');
insert into hr_regions (region_id,region_name) values (1,'europe');
name_label_1;
insert into hr_regions (region_id,region_name) values (2,'americas');
insert into hr_regions (region_id,region_name) values (3,'asia');
name_label_2;
insert into hr_regions (region_id,region_name) values (4,'middle east and africa');
name_label_3;
end;").

-define(PLSQL_11_RESULT_DEFAULT, "BEGIN
    CREATE TABLE
        Hr_Regions (
            Region_Id INTEGER NOT NULL PRIMARY KEY,
            Region_Name VARCHAR2(25)
        );
    CREATE TABLE
        Hr_Jobs (
            Job_Id VARCHAR2(10) NOT NULL PRIMARY KEY,
            Job_Title VARCHAR2(35) NOT NULL,
            Min_Salary NUMBER(6,0),
            Max_Salary NUMBER(6,0)
        );
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (1, 'europe');
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (2, 'americas');
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (3, 'asia');
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (4, 'middle east and africa');
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (1, 'europe');
    name_label_1;
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (2, 'americas');
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (3, 'asia');
    name_label_2;
    INSERT INTO
        Hr_Regions (
            Region_Id, Region_Name)
    VALUES
        (4, 'middle east and africa');
    name_label_3;
END").

%%------------------------------------------------------------------------------
%% PLSQL 12 - ORDER BY.
%%------------------------------------------------------------------------------

-define(PLSQL_12, "
begin
select * from dual order by column_1, column_2;
select * from dual order by column_1,column_2,column_3,column_4;
end;").

-define(PLSQL_12_RESULT_DEFAULT, "BEGIN
    SELECT
        *
    FROM
        Dual
    ORDER BY
        Column_1, Column_2;
    SELECT
        *
    FROM
        Dual
    ORDER BY
        Column_1, Column_2, Column_3, Column_4;
END").

%%------------------------------------------------------------------------------
%% PLSQL 13 - REVOKE.
%%------------------------------------------------------------------------------

-define(PLSQL_13, "
begin
revoke drop any table from user_1, user_2;
revoke all on table_1 from user_1 cascade constraints;
revoke delete, insert, select, update on table_1 from user_1;
revoke role_1 from user_1;
revoke privilege_1, privilege_2 from user_1;
revoke privilege_1, privilege_2, privilege_3, privilege_4, privilege_5 from user_1;
revoke create table from user_1,user_2,user_3,user_4;
end;").

-define(PLSQL_13_RESULT_DEFAULT, "BEGIN
    REVOKE
        DROP ANY TABLE
    FROM
        User_1, User_2;
    REVOKE
        ALL
    ON
        Table_1
    FROM
        User_1
    CASCADE CONSTRAINTS;
    REVOKE
        DELETE, INSERT, SELECT, UPDATE
    ON
        Table_1
    FROM
        User_1;
    REVOKE
        Role_1
    FROM
        User_1;
    REVOKE
        Privilege_1, Privilege_2
    FROM
        User_1;
    REVOKE
        Privilege_1, Privilege_2, Privilege_3, Privilege_4, Privilege_5
    FROM
        User_1;
    REVOKE
        CREATE TABLE
    FROM
        User_1, User_2, User_3, User_4;
END").

%%------------------------------------------------------------------------------
%% PLSQL 14 - SELECT.
%%------------------------------------------------------------------------------

-define(PLSQL_14, "
begin
select (select * from table_1),column_1,(select * from table_2) from dual;
select column_1 alias_1,(select * from table_1) alias_2,column_3 alias_3 from dual;
select distinct column_1,column_2 from dual;
select sum(column_1) / sum(column_2) avg_charge from dual;
select avg(sum(min(1))) from dual;
select column_1,(select * from table_1,table_2 natural join table_3,table_4) column_2,column_3 from dual;
select * into column_1,column_2,column_3,column_4 from dual;
select
case column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end from dual;
select column_11, case column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end as column_12, column_13 from dual;
select (+ column_2)|:b| from table_1;
end;").

-define(PLSQL_14_RESULT_DEFAULT, "BEGIN
    SELECT
        (SELECT
            *
        FROM
            Table_1), Column_1, " ++ "
        (SELECT
            *
        FROM
            Table_2)
    FROM
        Dual;
    SELECT
        Column_1 Alias_1, " ++ "
        (SELECT
            *
        FROM
            Table_1) Alias_2, Column_3 Alias_3
    FROM
        Dual;
    SELECT DISTINCT
        Column_1, Column_2
    FROM
        Dual;
    SELECT
        SUM(Column_1) / SUM(Column_2) Avg_Charge
    FROM
        Dual;
    SELECT
        AVG(SUM(MIN(1)))
    FROM
        Dual;
    SELECT
        Column_1, " ++ "
        (SELECT
            *
        FROM
            Table_1,
            Table_2
            NATURAL JOIN
            Table_3,
            Table_4) Column_2, Column_3
    FROM
        Dual;
    SELECT
        *
    INTO
        Column_1, Column_2, Column_3, Column_4
    FROM
        Dual;
    SELECT
        CASE Column_0
            WHEN Column_1
            OR Column_2
            AND Column_3
            THEN Column_4
            ELSE Column_5
        END
    FROM
        Dual;
    SELECT
        Column_11, " ++ "
        CASE Column_0
            WHEN Column_1
            OR Column_2
            AND Column_3
            THEN Column_4
            ELSE Column_5
        END Column_12, Column_13
    FROM
        Dual;
    SELECT
        (+ Column_2)|:b|
    FROM
        Table_1;
END").

%%------------------------------------------------------------------------------
%% PLSQL 15 - STRUCTURE.
%%------------------------------------------------------------------------------

-define(PLSQL_15, "
begin
select (select * from table_1) from dual;
select (select (select * from table_2) from table_1) from dual;
select (select (select (select * from table_2) from table_2) from table_1) from dual;
end;").

-define(PLSQL_15_RESULT_DEFAULT, "BEGIN
    SELECT
        (SELECT
            *
        FROM
            Table_1)
    FROM
        Dual;
    SELECT
        (SELECT
            (SELECT
                *
            FROM
                Table_2)
        FROM
            Table_1)
    FROM
        Dual;
    SELECT
        (SELECT
            (SELECT
                (SELECT
                    *
                FROM
                    Table_2)
            FROM
                Table_2)
        FROM
            Table_1)
    FROM
        Dual;
END").

%%------------------------------------------------------------------------------
%% PLSQL 16 - UNION.
%%------------------------------------------------------------------------------

-define(PLSQL_16, "
begin
select * from table_1 union select * from table_2;
select * from (select * from table_11 intersect select * from table_12) union all select * from (select * from table_21 minus select * from table_22);
select * from (select * from table_11 intersect select * from table_12) union all select * from (select * from table_21 minus select * from (select * from table_31 union select * from table_32));
select column_1,(select * from dual) union (select * from dual) from dual;
select column_1,(select * from dual) union (select * from dual),column_2 from dual;
select column_1,(select * from dual) union (select * from dual) as column_2,column_3 from dual;
end;").

-define(PLSQL_16_RESULT_DEFAULT, "BEGIN
        ((SELECT
            *
        FROM
            Table_1)
    UNION
        (SELECT
            *
        FROM
            Table_2));
        ((SELECT
            *
        FROM
                ((SELECT
                    *
                FROM
                    Table_11)
            INTERSECT
                (SELECT
                    *
                FROM
                    Table_12)))
    UNION ALL
        (SELECT
            *
        FROM
                ((SELECT
                    *
                FROM
                    Table_21)
            MINUS
                (SELECT
                    *
                FROM
                    Table_22))));
        ((SELECT
            *
        FROM
                ((SELECT
                    *
                FROM
                    Table_11)
            INTERSECT
                (SELECT
                    *
                FROM
                    Table_12)))
    UNION ALL
        (SELECT
            *
        FROM
                ((SELECT
                    *
                FROM
                    Table_21)
            MINUS
                (SELECT
                    *
                FROM
                        ((SELECT
                            *
                        FROM
                            Table_31)
                    UNION
                        (SELECT
                            *
                        FROM
                            Table_32))))));
    SELECT
        Column_1, " ++ "
            ((SELECT
                *
            FROM
                Dual)
        UNION
            (SELECT
                *
            FROM
                Dual))
    FROM
        Dual;
    SELECT
        Column_1, " ++ "
            ((SELECT
                *
            FROM
                Dual)
        UNION
            (SELECT
                *
            FROM
                Dual)), Column_2
    FROM
        Dual;
    SELECT
        Column_1, " ++ "
            ((SELECT
                *
            FROM
                Dual)
        UNION
            (SELECT
                *
            FROM
                Dual)) Column_2, Column_3
    FROM
        Dual;
END").

%%------------------------------------------------------------------------------
%% PLSQL 17 - WHERE.
%%------------------------------------------------------------------------------

-define(PLSQL_17, "
begin
select * from dual where column_1 = column_2;
select * from dual
where column_11 <> column_12
and column_21 != column_22
or column_31 <> column_32
and column_41 != column_42;
select * from dual where (select column_31 from table_31) <> column_14;
select * from dual where column_1 = column_2 and not column_3 = column_4;
select * from dual where column_1 > all (select * from table_1);
select * from dual start with column_1 is null and column_2 < 0 connect by nocycle column_2 = column_3 or column_4 <> column_5;
end;").

-define(PLSQL_17_RESULT_DEFAULT, "BEGIN
    SELECT
        *
    FROM
        Dual
    WHERE
        Column_1 = Column_2;
    SELECT
        *
    FROM
        Dual
    WHERE
        Column_11 <> Column_12
        AND Column_21 != Column_22
        OR Column_31 <> Column_32
        AND Column_41 != Column_42;
    SELECT
        *
    FROM
        Dual
    WHERE
        (SELECT
            Column_31
        FROM
            Table_31) <> Column_14;
    SELECT
        *
    FROM
        Dual
    WHERE
        Column_1 = Column_2
        AND NOT (Column_3 = Column_4);
    SELECT
        *
    FROM
        Dual
    WHERE
        Column_1 > ALL
        (SELECT
            *
        FROM
            Table_1);
    SELECT
        *
    FROM
        Dual
    START WITH
        Column_1 IS NULL
        AND Column_2 < 0
    CONNECT BY NOCYCLE
        Column_2 = Column_3
        OR Column_4 <> Column_5;
END").

%%------------------------------------------------------------------------------
%% PLSQL 18 - simple.
%%------------------------------------------------------------------------------

-define(PLSQL_18, "
begin schm.proc(:p_first,:p_second,:p_result); end").

-define(PLSQL_18_RESULT_DEFAULT, "BEGIN
    Schm.Proc(:p_first, :p_second, :p_result);
END").

%%------------------------------------------------------------------------------
%% PLSQL 19 - CALL.
%%------------------------------------------------------------------------------

-define(PLSQL_19, "
call proc(:p_first,:p_second,:p_result)").

-define(PLSQL_19_RESULT_DEFAULT, "CALL
    Proc(:p_first, :p_second, :p_result)").

%%------------------------------------------------------------------------------
%% PLSQL 20 - simple.
%%------------------------------------------------------------------------------

-define(PLSQL_20, "
begin schm.proc(:p_first,:p_second,:p_result); dbms_output.put_line('Goodbye cruel World!'); end").

-define(PLSQL_20_RESULT_DEFAULT, "BEGIN
    Schm.Proc(:p_first, :p_second, :p_result);
    Dbms_Output.Put_Line('Goodbye cruel World!');
END").

%%------------------------------------------------------------------------------
%% PLSQL 21 - ROLE.
%%------------------------------------------------------------------------------

-define(PLSQL_21, "
begin
create role role_1;
drop role role_1;
end").

-define(PLSQL_21_RESULT_DEFAULT, "BEGIN
    CREATE ROLE
        Role_1;
    DROP ROLE
        Role_1;
END").

%%------------------------------------------------------------------------------
%% PLSQL 22 - TRANSACTION.
%%------------------------------------------------------------------------------

-define(PLSQL_22, "
begin
commit;
commit work;
rollback;
rollback work;
end").

-define(PLSQL_22_RESULT_DEFAULT, "BEGIN
    COMMIT;
    COMMIT WORK;
    ROLLBACK;
    ROLLBACK WORK;
END").

%%------------------------------------------------------------------------------
%% PLSQL 23 - TRUNCATE.
%%------------------------------------------------------------------------------

-define(PLSQL_23, "
begin
truncate table tbl;
truncate table name_schema.name_table;
truncate table tbl preserve materialized view log;
truncate table tbl purge materialized view log;
truncate table tbl drop storage;
truncate table tbl reuse storage;
truncate table tbl preserve materialized view log drop storage;
truncate table tbl preserve materialized view log reuse storage;
truncate table tbl purge materialized view log drop storage;
truncate table tbl purge materialized view log reuse storage;
truncate table table_1 drop storage;
truncate table schema_1.table_1 drop storage;
truncate table :param_1 drop storage;
truncate table \"^&()\" drop storage;
end").

-define(PLSQL_23_RESULT_DEFAULT, "BEGIN
    TRUNCATE TABLE
        Tbl;
    TRUNCATE TABLE
        Name_Schema.Name_Table;
    TRUNCATE TABLE
        Tbl
    PRESERVE MATERIALIZED VIEW LOG;
    TRUNCATE TABLE
        Tbl
    PURGE MATERIALIZED VIEW LOG;
    TRUNCATE TABLE
        Tbl
    DROP STORAGE;
    TRUNCATE TABLE
        Tbl
    REUSE STORAGE;
    TRUNCATE TABLE
        Tbl
    PRESERVE MATERIALIZED VIEW LOG
    DROP STORAGE;
    TRUNCATE TABLE
        Tbl
    PRESERVE MATERIALIZED VIEW LOG
    REUSE STORAGE;
    TRUNCATE TABLE
        Tbl
    PURGE MATERIALIZED VIEW LOG
    DROP STORAGE;
    TRUNCATE TABLE
        Tbl
    PURGE MATERIALIZED VIEW LOG
    REUSE STORAGE;
    TRUNCATE TABLE
        Table_1
    DROP STORAGE;
    TRUNCATE TABLE
        Schema_1.Table_1
    DROP STORAGE;
    TRUNCATE TABLE
        :param_1
    DROP STORAGE;
    TRUNCATE TABLE
        \"^&()\"
    DROP STORAGE;
END").

%%------------------------------------------------------------------------------
%% PLSQL 24 - UPDATE.
%%------------------------------------------------------------------------------

-define(PLSQL_24, "
begin
update name_table set name_column_1 = :value_1;
update name_table set name_column_1 = :value_1, name_column_2 = :value_2;
update name_table set name_column_1 = :value_1, name_column_2 = :value_2 where company_id = :id1 and employee_id = :id2;
update employees set salary = :sal where employee_id = :id;
update employees set salary = :sal where employee_id = :id returning c,d into :c, :d;
update employees set salary = :sal where company_id = :id1 and employee_id2 = :id returning lob_column into :out_locator;
end").

-define(PLSQL_24_RESULT_DEFAULT, "BEGIN
    UPDATE
        Name_Table
    SET
        Name_Column_1 = :value_1;
    UPDATE
        Name_Table
    SET
        Name_Column_1 = :value_1,
        Name_Column_2 = :value_2;
    UPDATE
        Name_Table
    SET
        Name_Column_1 = :value_1,
        Name_Column_2 = :value_2
    WHERE
        Company_Id = :id1
        AND Employee_Id = :id2;
    UPDATE
        Employees
    SET
        Salary = :sal
    WHERE
        Employee_Id = :id;
    UPDATE
        Employees
    SET
        Salary = :sal
    WHERE
        Employee_Id = :id
    RETURNING
        C, D
    INTO
        :c, :d;
    UPDATE
        Employees
    SET
        Salary = :sal
    WHERE
        Company_Id = :id1
        AND Employee_Id2 = :id
    RETURNING
        Lob_Column
    INTO
        :out_locator;
END").

%%------------------------------------------------------------------------------
%% PLSQL 25 - VIEW.
%%------------------------------------------------------------------------------

-define(PLSQL_25, "
begin
create view table_1 as select * from dual;
create view table_1 as select * from dual with check option;
create view table_1 (column_1, column_2) as select * from dual;
create view table_1 (column_1, column_2) as select * from dual with check option;
create view table_1 (column_1, column_2, column_3, column_4) as select * from dual;
create view table_1 (column_1, column_2, column_3, column_4) as select * from dual with check option;
end").

-define(PLSQL_25_RESULT_DEFAULT, "BEGIN
    CREATE VIEW
        Table_1
    AS
        SELECT
            *
        FROM
            Dual;
    CREATE VIEW
        Table_1
    AS
        SELECT
            *
        FROM
            Dual
    WITH CHECK OPTION;
    CREATE VIEW
        Table_1 (Column_1, Column_2)
    AS
        SELECT
            *
        FROM
            Dual;
    CREATE VIEW
        Table_1 (Column_1, Column_2)
    AS
        SELECT
            *
        FROM
            Dual
    WITH CHECK OPTION;
    CREATE VIEW
        Table_1 (Column_1, Column_2, Column_3, Column_4)
    AS
        SELECT
            *
        FROM
            Dual;
    CREATE VIEW
        Table_1 (Column_1, Column_2, Column_3, Column_4)
    AS
        SELECT
            *
        FROM
            Dual
    WITH CHECK OPTION;
END").

%%------------------------------------------------------------------------------
%% PLSQL 26 - CALL.
%%------------------------------------------------------------------------------

-define(PLSQL_26, "
Call function_1
(select column_1 from table_1 union select column_2 from table_2)").

-define(PLSQL_26_RESULT_DEFAULT, "CALL
    Function_1(
            ((SELECT
                Column_1
            FROM
                Table_1)
        UNION
            (SELECT
                Column_2
            FROM
                Table_2)))").

%%------------------------------------------------------------------------------
%% PLSQL 27 - CALL.
%%------------------------------------------------------------------------------

-define(PLSQL_27, "
Call function_1
(parameter_1,(select column_1 from table_1 union select column_2 from table_2),parameter_3)").

-define(PLSQL_27_RESULT_DEFAULT, "CALL
    Function_1(Parameter_1, " ++ "
            ((SELECT
                Column_1
            FROM
                Table_1)
        UNION
            (SELECT
                Column_2
            FROM
                Table_2)), Parameter_3)").

%%------------------------------------------------------------------------------
%% PLSQL 28 - CALL.
%%------------------------------------------------------------------------------

-define(PLSQL_28, "
call i1ident_8 ((select * from table_1),(select * from table_2),(select * from table_3));").

-define(PLSQL_28_RESULT_DEFAULT, "CALL
    I1ident_8(
        (SELECT
            *
        FROM
            Table_1), " ++ "
        (SELECT
            *
        FROM
            Table_2), " ++ "
        (SELECT
            *
        FROM
            Table_3))").

%%------------------------------------------------------------------------------
%% PLSQL 29 - CALL.
%%------------------------------------------------------------------------------

-define(PLSQL_29, "
begin i1ident_5.m@oney~~$tree_ident ((select * from table_1),(select * from table_2) minus (select * from table_3),(select * from table_4),6.34e8f);end;").

-define(PLSQL_29_RESULT_DEFAULT, "BEGIN
    I1ident_5.M@oney~~$tree_Ident(
        (SELECT
            *
        FROM
            Table_1), " ++ "
            ((SELECT
                *
            FROM
                Table_2)
        MINUS
            (SELECT
                *
            FROM
                Table_3)), " ++ "
        (SELECT
            *
        FROM
            Table_4), 6.34e8f);
END").

%%------------------------------------------------------------------------------
%% UNION 25 - SELECT FROM.
%%------------------------------------------------------------------------------

-define(PROBLEM_01, "
select *
from (select * from table_1 union select * from table_2)").

% line 04

-define(PROBLEM_01_RESULT_DEFAULT, "SELECT
    *
FROM
            ((SELECT
            *
        FROM
            Table_1)
    UNION
        (SELECT
            *
        FROM
            Table_2))").

%%------------------------------------------------------------------------------
%% UNION 28 - UNION FROM.
%%------------------------------------------------------------------------------

-define(PROBLEM_02, "
select *
from ((select * from table_11 intersect select * from table_12) union (select * from table_21 minus select * from table_22))").

% line 04

-define(PROBLEM_02_RESULT_DEFAULT, "SELECT
    *
FROM
                (((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12))
    UNION
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                Table_22)))").

%%------------------------------------------------------------------------------
%% UNION 31 - SELECT & UNION FROM.
%%------------------------------------------------------------------------------

-define(PROBLEM_03, "
select *
from (select * from table_1 union (select * from table_21 minus select * from table_22))").

% line 04

-define(PROBLEM_03_RESULT_DEFAULT, "SELECT
    *
FROM
            ((SELECT
            *
        FROM
            Table_1)
    UNION
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                Table_22)))").

%%------------------------------------------------------------------------------
%% UNION 34 - UNION & SELECT FROM.
%%------------------------------------------------------------------------------

-define(PROBLEM_04, "
select *
from ((select * from table_11 intersect select * from table_12) union select * from table_2)").

% line 04

-define(PROBLEM_04_RESULT_DEFAULT, "SELECT
    *
FROM
                (((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12))
    UNION
        (SELECT
            *
        FROM
            Table_2))").

%%------------------------------------------------------------------------------
%% UNION 36 - MINUS & INTERSECT.
%%------------------------------------------------------------------------------

-define(PROBLEM_05, "
(select * from table_1)|:_a::b::c|
Minus ((select * from table_2) Intersect (select * from table_3));").

% first SELECT

-define(PROBLEM_05_RESULT_DEFAULT, "((SELECT
    *
FROM
    Table_1))|:_a::b::c|
MINUS
        ((SELECT
            *
        FROM
            Table_2)
    INTERSECT
        (SELECT
            *
        FROM
            Table_3))").

%%------------------------------------------------------------------------------
%% UNION 37 - INTERSECT & MINUS.
%%------------------------------------------------------------------------------

% lasr SELECT

-define(PROBLEM_06, "
((select * from table_2)
Intersect (select * from table_3)) Minus (select * from table_1)|:_a::b::c|;").

-define(PROBLEM_06_RESULT_DEFAULT, "        ((SELECT
            *
        FROM
            Table_2)
    INTERSECT
        (SELECT
            *
        FROM
            Table_3))
MINUS
((SELECT
    *
FROM
    Table_1))|:_a::b::c|").

%%------------------------------------------------------------------------------
%% REVOKE 01 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_01, "
revoke create table from user_1").

-define(REVOKE_01_RESULT_DEFAULT, "REVOKE
    CREATE TABLE
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 02 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_02, "
revoke drop any table from user_1, user_2").

-define(REVOKE_02_RESULT_DEFAULT, "REVOKE
    DROP ANY TABLE
FROM
    User_1, User_2").

%%------------------------------------------------------------------------------
%% REVOKE 03 - FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_03, "
revoke all on table_1 from user_1 cascade constraints").

-define(REVOKE_03_RESULT_DEFAULT, "REVOKE
    ALL
ON
    Table_1
FROM
    User_1
CASCADE CONSTRAINTS").

%%------------------------------------------------------------------------------
%% REVOKE 04 - FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_04, "
revoke update on table_1 from user_1, user_2 force").

-define(REVOKE_04_RESULT_DEFAULT, "REVOKE
    UPDATE
ON
    Table_1
FROM
    User_1, User_2
FORCE").

%%------------------------------------------------------------------------------
%% REVOKE 05 - ON & FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_05, "
revoke select on table_1 from user_1 force").

-define(REVOKE_05_RESULT_DEFAULT, "REVOKE
    SELECT
ON
    Table_1
FROM
    User_1
FORCE").

%%------------------------------------------------------------------------------
%% REVOKE 06 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_06, "
revoke all on table_1 from user_1").

-define(REVOKE_06_RESULT_DEFAULT, "REVOKE
    ALL
ON
    Table_1
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 07 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_07, "
revoke delete, insert, select, update on table_1 from user_1").

-define(REVOKE_07_RESULT_DEFAULT, "REVOKE
    DELETE, INSERT, SELECT, UPDATE
ON
    Table_1
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 08 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_08, "
revoke role_1 from user_1").

-define(REVOKE_08_RESULT_DEFAULT, "REVOKE
    Role_1
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 09 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_09, "
revoke privilege_1, privilege_2 from user_1").

-define(REVOKE_09_RESULT_DEFAULT, "REVOKE
    Privilege_1, Privilege_2
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 10 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_10, "
revoke privilege_1, privilege_2, privilege_3, privilege_4, privilege_5 from user_1").

-define(REVOKE_10_RESULT_DEFAULT, "REVOKE
    Privilege_1, Privilege_2, Privilege_3, Privilege_4, Privilege_5
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 11 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_11, "
revoke select on ddtable from user_1").

-define(REVOKE_11_RESULT_DEFAULT, "REVOKE
    SELECT
ON
    Ddtable
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 12 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_12, "
revoke select on schema1.ddtable from user_1").

-define(REVOKE_12_RESULT_DEFAULT, "REVOKE
    SELECT
ON
    Schema1.Ddtable
FROM
    User_1").

%%------------------------------------------------------------------------------
%% REVOKE 13 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_13, "
revoke all privileges on schema1.ddtable from role_2").

-define(REVOKE_13_RESULT_DEFAULT, "REVOKE
    ALL PRIVILEGES
ON
    Schema1.Ddtable
FROM
    Role_2").

%%------------------------------------------------------------------------------
%% REVOKE 14 - FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_14, "
revoke manage_system from test_user_1").

-define(REVOKE_14_RESULT_DEFAULT, "REVOKE
    Manage_System
FROM
    Test_User_1").

%%------------------------------------------------------------------------------
%% REVOKE 15 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_15, "
revoke create table from user_1,user_2,user_3,user_4").

-define(REVOKE_15_RESULT_DEFAULT, "REVOKE
    CREATE TABLE
FROM
    User_1, User_2, User_3, User_4").

%%------------------------------------------------------------------------------
%% ROLE 01 - CREATE.
%%------------------------------------------------------------------------------

-define(ROLE_01, "
create role role_1").

-define(ROLE_01_RESULT_DEFAULT, "CREATE ROLE
    Role_1").

%%------------------------------------------------------------------------------
%% ROLE 02 - DROP.
%%------------------------------------------------------------------------------

-define(ROLE_02, "
drop role role_1").

-define(ROLE_02_RESULT_DEFAULT, "DROP ROLE
    Role_1").

%%------------------------------------------------------------------------------
%% SELECT 01 - very simple.
%%------------------------------------------------------------------------------

-define(SELECT_01, "
select *
from dual").

-define(SELECT_01_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 02 - column and param.
%%------------------------------------------------------------------------------

-define(SELECT_02, "
select column_1, :param_1
from dual").

-define(SELECT_02_RESULT_DEFAULT, "SELECT
    Column_1, :param_1
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 03 - subquery.
%%------------------------------------------------------------------------------

-define(SELECT_03, "
select (select * from table_1)
from dual").

-define(SELECT_03_RESULT_DEFAULT, "SELECT
    (SELECT
        *
    FROM
        Table_1)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 04 - columns.
%%------------------------------------------------------------------------------

-define(SELECT_04, "
select column_1,column_2
from dual").

-define(SELECT_04_RESULT_DEFAULT, "SELECT
    Column_1, Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 05 - subqueries.
%%------------------------------------------------------------------------------

-define(SELECT_05, "
select (select * from table_1),(select * from table_2)
from dual").

-define(SELECT_05_RESULT_DEFAULT, "SELECT
    (SELECT
        *
    FROM
        Table_1), " ++ "
    (SELECT
        *
    FROM
        Table_2)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 06 - column, subquery, column.
%%------------------------------------------------------------------------------

-define(SELECT_06, "
select column_1,(select * from table_1),column_2
from dual").

-define(SELECT_06_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
    (SELECT
        *
    FROM
        Table_1), Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 07 - subquery, column, subquery.
%%------------------------------------------------------------------------------

-define(SELECT_07, "
select (select * from table_1),column_1,(select * from table_2)
from dual").

-define(SELECT_07_RESULT_DEFAULT, "SELECT
    (SELECT
        *
    FROM
        Table_1), Column_1, " ++ "
    (SELECT
        *
    FROM
        Table_2)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 08 - ALIAS column: column & param.
%%------------------------------------------------------------------------------

-define(SELECT_08, "
select column_1 alias_1,:param_2 alias_2
from dual").

-define(SELECT_08_RESULT_DEFAULT, "SELECT
    Column_1 Alias_1, :param_2 Alias_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 09 - ALIAS column: subquery.
%%------------------------------------------------------------------------------

-define(SELECT_09, "
select (select * from table_1) alias_1
from dual").

-define(SELECT_09_RESULT_DEFAULT, "SELECT
    (SELECT
        *
    FROM
        Table_1) Alias_1
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 10 - ALIAS column: column & subquery.
%%------------------------------------------------------------------------------

-define(SELECT_10, "
select column_1 alias_1,(select * from table_1) alias_2,column_3 alias_3
from dual").

-define(SELECT_10_RESULT_DEFAULT, "SELECT
    Column_1 Alias_1, " ++ "
    (SELECT
        *
    FROM
        Table_1) Alias_2, Column_3 Alias_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 11 - ALL.
%%------------------------------------------------------------------------------

-define(SELECT_11, "
select all column_1,column_2
from dual").

-define(SELECT_11_RESULT_DEFAULT, "SELECT ALL
    Column_1, Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 12 - DISTINCT.
%%------------------------------------------------------------------------------

-define(SELECT_12, "
select distinct column_1,column_2
from dual").

-define(SELECT_12_RESULT_DEFAULT, "SELECT DISTINCT
    Column_1, Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 13 - FUN OP FUN.
%%------------------------------------------------------------------------------

-define(SELECT_13, "
select sum(column_1) / sum(column_2) avg_charge
from dual").

-define(SELECT_13_RESULT_DEFAULT, "SELECT
    SUM(Column_1) / SUM(Column_2) Avg_Charge
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 14 - FUN complex.
%%------------------------------------------------------------------------------

-define(SELECT_14, "
select sum(sum(1))
from dual").

-define(SELECT_14_RESULT_DEFAULT, "SELECT
    SUM(SUM(1))
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 15 - FUN simple.
%%------------------------------------------------------------------------------

-define(SELECT_15, "
select sum(1,2,3)
from dual").

-define(SELECT_15_RESULT_DEFAULT, "SELECT
    SUM(1, 2, 3)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 16 - FUN complex.
%%------------------------------------------------------------------------------

-define(SELECT_16, "
select sum(1,2,3,4)
from dual").

-define(SELECT_16_RESULT_DEFAULT, "SELECT
    SUM(1, 2, 3, 4)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 17 - FUN mixed.
%%------------------------------------------------------------------------------

-define(SELECT_17, "
select avg(sum(min(1)))
from dual").

-define(SELECT_17_RESULT_DEFAULT, "SELECT
    AVG(SUM(MIN(1)))
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 18 - HINTS.
%%------------------------------------------------------------------------------

-define(SELECT_18, "
select /* hint */ *
from dual").

-define(SELECT_18_RESULT_DEFAULT, "SELECT /* hint */
    *
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 19 - JOIN column.
%%------------------------------------------------------------------------------

-define(SELECT_19, "
select column_1,(select * from table_1,table_2 natural join table_3,table_4) column_2,column_3
from dual").

-define(SELECT_19_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
    (SELECT
        *
    FROM
        Table_1,
        Table_2
        NATURAL JOIN
        Table_3,
        Table_4) Column_2, Column_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 20 - INTO.
%%------------------------------------------------------------------------------

-define(SELECT_20, "
select * into column_1
from dual").

-define(SELECT_20_RESULT_DEFAULT, "SELECT
    *
INTO
    Column_1
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 21 - INTO.
%%------------------------------------------------------------------------------

-define(SELECT_21, "
select * into column_1,column_2,column_3
from dual").

-define(SELECT_21_RESULT_DEFAULT, "SELECT
    *
INTO
    Column_1, Column_2, Column_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 22 - INTO.
%%------------------------------------------------------------------------------

-define(SELECT_22, "
select * into column_1,column_2,column_3,column_4
from dual").

-define(SELECT_22_RESULT_DEFAULT, "SELECT
    *
INTO
    Column_1, Column_2, Column_3, Column_4
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 23 - COLUMNS.
%%------------------------------------------------------------------------------

-define(SELECT_23, "
select column_1,column_2,column_3
from dual").

-define(SELECT_23_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 24 - COLUMNS.
%%------------------------------------------------------------------------------

-define(SELECT_24, "
select column_1,column_2,column_3,column_4
from dual").

-define(SELECT_24_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 25 - CASE.
%%------------------------------------------------------------------------------

-define(SELECT_25, "
select
case column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end
from dual").

-define(SELECT_25_RESULT_DEFAULT, "SELECT
    CASE Column_0
        WHEN Column_1
        OR Column_2
        AND Column_3
        THEN Column_4
        ELSE Column_5
    END
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 26 - CASE.
%%------------------------------------------------------------------------------

-define(SELECT_26, "
select
column_11, case column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end as column_12, column_13
from dual").

-define(SELECT_26_RESULT_DEFAULT, "SELECT
    Column_11, " ++ "
    CASE Column_0
        WHEN Column_1
        OR Column_2
        AND Column_3
        THEN Column_4
        ELSE Column_5
    END Column_12, Column_13
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 27 - CASE.
%%------------------------------------------------------------------------------

-define(SELECT_27, "
select column_1,column_2,column_3,column_4
from dual").

-define(SELECT_27_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4
FROM
    Dual").

%%------------------------------------------------------------------------------
%% SELECT 28 - UNARY.
%%------------------------------------------------------------------------------

-define(SELECT_28, "
select (+ column_2)|:b| from table_1").

-define(SELECT_28_RESULT_DEFAULT, "SELECT
    (+ Column_2)|:b|
FROM
    Table_1").

%%------------------------------------------------------------------------------
%% SELECT 29 - STRING.
%%------------------------------------------------------------------------------

-define(SELECT_29, "
select \"columN_1\" into \"columN_2\" from \"tablE_1\" where \"columN_3\" = \"columN_4\" group by \"columN_5\" having \"columN_6\" = \"columN_7\" order by \"columN_8\"").

-define(SELECT_29_RESULT_DEFAULT, "SELECT
    \"columN_1\"
INTO
    \"columN_2\"
FROM
    \"tablE_1\"
WHERE
    \"columN_3\" = \"columN_4\"
GROUP BY
    \"columN_5\"
HAVING
    \"columN_6\" = \"columN_7\"
ORDER BY
    \"columN_8\"").

%%------------------------------------------------------------------------------
%% SELECT 30 - STRING.
%%------------------------------------------------------------------------------

-define(SELECT_30, "
select 'columN_1' from 'tablE_1' where 'columN_3' = 'columN_4' having 'columN_6' = 'columN_7' order by 'columN_8'").

-define(SELECT_30_RESULT_DEFAULT, "SELECT
    'columN_1'
FROM
    'tablE_1'
WHERE
    'columN_3' = 'columN_4'
HAVING
    'columN_6' = 'columN_7'
ORDER BY
    'columN_8'").

%%------------------------------------------------------------------------------
%% SELECT 31 - ANCHOR.
%%------------------------------------------------------------------------------

-define(SELECT_31, "
(select * from table_1 group by column_1)|:_a::b::c|").

-define(SELECT_31_RESULT_DEFAULT, "(SELECT
    *
FROM
    Table_1
GROUP BY
    Column_1)|:_a::b::c|").

%%------------------------------------------------------------------------------
%% STRUCTURE 01 - sinple subquery.
%%------------------------------------------------------------------------------

-define(STRUCTURE_01, "
select (select * from table_1)
from dual").

-define(STRUCTURE_01_RESULT_DEFAULT, "SELECT
    (SELECT
        *
    FROM
        Table_1)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% STRUCTURE 02 - double subquery.
%%------------------------------------------------------------------------------

-define(STRUCTURE_02, "
select (select (select * from table_2) from table_1)
from dual").

-define(STRUCTURE_02_RESULT_DEFAULT, "SELECT
    (SELECT
        (SELECT
            *
        FROM
            Table_2)
    FROM
        Table_1)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% STRUCTURE 03 - triple subquery.
%%------------------------------------------------------------------------------

-define(STRUCTURE_03, "
select (select (select (select * from table_2) from table_2) from table_1)
from dual").

-define(STRUCTURE_03_RESULT_DEFAULT, "SELECT
    (SELECT
        (SELECT
            (SELECT
                *
            FROM
                Table_2)
        FROM
            Table_2)
    FROM
        Table_1)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% TRANSACTION 01 - COMMIT.
%%------------------------------------------------------------------------------

-define(TRANSACTION_01, "
commit").

-define(TRANSACTION_01_RESULT_DEFAULT, "COMMIT").

%%------------------------------------------------------------------------------
%% TRANSACTION 02 - COMMIT.
%%------------------------------------------------------------------------------

-define(TRANSACTION_02, "
commit work").

-define(TRANSACTION_02_RESULT_DEFAULT, "COMMIT WORK").

%%------------------------------------------------------------------------------
%% TRANSACTION 03 - ROLLBACK.
%%------------------------------------------------------------------------------

-define(TRANSACTION_03, "
rollback").

-define(TRANSACTION_03_RESULT_DEFAULT, "ROLLBACK").

%%------------------------------------------------------------------------------
%% TRANSACTION 04 - ROLLBACK.
%%------------------------------------------------------------------------------

-define(TRANSACTION_04, "
rollback work").

-define(TRANSACTION_04_RESULT_DEFAULT, "ROLLBACK WORK").

%%------------------------------------------------------------------------------
%% TRUNCATE 01 - simple.
%%------------------------------------------------------------------------------

-define(TRUNCATE_01, "
truncate table name_schema.name_table").

-define(TRUNCATE_01_RESULT_DEFAULT, "TRUNCATE TABLE
    Name_Schema.Name_Table").

%%------------------------------------------------------------------------------
%% TRUNCATE 02 - PRESERVE.
%%------------------------------------------------------------------------------

-define(TRUNCATE_02, "
truncate table tbl preserve materialized view log").

-define(TRUNCATE_02_RESULT_DEFAULT, "TRUNCATE TABLE
    Tbl
PRESERVE MATERIALIZED VIEW LOG").

%%------------------------------------------------------------------------------
%% TRUNCATE 03 - PURGE.
%%------------------------------------------------------------------------------

-define(TRUNCATE_03, "
truncate table tbl purge materialized view log").

-define(TRUNCATE_03_RESULT_DEFAULT, "TRUNCATE TABLE
    Tbl
PURGE MATERIALIZED VIEW LOG").

%%------------------------------------------------------------------------------
%% TRUNCATE 04 - DROP.
%%------------------------------------------------------------------------------

-define(TRUNCATE_04, "
truncate table tbl drop storage").

-define(TRUNCATE_04_RESULT_DEFAULT, "TRUNCATE TABLE
    Tbl
DROP STORAGE").

%%------------------------------------------------------------------------------
%% TRUNCATE 05 - REUSE.
%%------------------------------------------------------------------------------

-define(TRUNCATE_05, "
truncate table tbl reuse storage").

-define(TRUNCATE_05_RESULT_DEFAULT, "TRUNCATE TABLE
    Tbl
REUSE STORAGE").

%%------------------------------------------------------------------------------
%% TRUNCATE 06 - PRESEREVE & DROP.
%%------------------------------------------------------------------------------

-define(TRUNCATE_06, "
truncate table tbl preserve materialized view log drop storage").

-define(TRUNCATE_06_RESULT_DEFAULT, "TRUNCATE TABLE
    Tbl
PRESERVE MATERIALIZED VIEW LOG
DROP STORAGE").

%%------------------------------------------------------------------------------
%% TRUNCATE 07 - PURGE & DROP.
%%------------------------------------------------------------------------------

-define(TRUNCATE_07, "
truncate table tbl purge materialized view log drop storage").

-define(TRUNCATE_07_RESULT_DEFAULT, "TRUNCATE TABLE
    Tbl
PURGE MATERIALIZED VIEW LOG
DROP STORAGE").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 01 - |.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_01, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7,
c_8|:abc defghijkl|
from dual").

-define(UNBREAKABLE_01_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, C_8
    |:abc| Defghijkl
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 02 - '.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_02, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7,
'abc\"de f|gh,i\*jkl'
from dual").

-define(UNBREAKABLE_02_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7,
    'abc\"de f|gh,i*jkl'
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 03 - ".
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_03, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7,
\"ab c'de,f|ghi\*jkl\"
from dual").

-define(UNBREAKABLE_03_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7,
    \"ab c'de,f|ghi*jkl\"
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 04 - /* */.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_04, "
select /* xxaxx xxbxx xxcxx ' xxdxx xxexx , xxfxx | xxgxx xxhxx xxixx \" xxjxxkxxlxx*/
column_1,column_2,column_3,column_4,column_5,column_6,column_7, column_8
from dual").

-define(UNBREAKABLE_04_RESULT_DEFAULT, "SELECT /* xxaxx xxbxx xxcxx ' xxdxx xxexx , xxfxx | xxgxx xxhxx xxixx \" xxjxxkxxlxx*/
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7,
    Column_8
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 05 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_05, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7, Colu_8, column_9
from dual").

-define(UNBREAKABLE_05_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, Colu_8
    , Column_9
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 06 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_06, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7, Col_8, column_9
from dual").

-define(UNBREAKABLE_06_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, Col_8,
    Column_9
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 07 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_07, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7, \"Col8\" column_8
from dual").

-define(UNBREAKABLE_07_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, \"Col8\"
    Column_8
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 08 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_08, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7, colum8(+) column_8
from dual").

-define(UNBREAKABLE_08_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, Colum8
    (+) Column_8
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 09 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_09, "
select column_1,column_2,column_3,column_4,(column_51+column_62+column_73+colum84(+)) column_5_8
from dual").

-define(UNBREAKABLE_09_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_51 + Column_62 + Column_73 +
    Colum84(+) Column_5_8
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 10 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_10, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7|:a_obj:x|,colum8(+)
from dual").

-define(UNBREAKABLE_10_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7
    |:a_obj:x|, Colum8(+)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 11 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_11, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7|:a_o:x|,colum8(+)
from dual").

-define(UNBREAKABLE_11_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7|:a_o:x|
    , Colum8(+)
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNBREAKABLE 12 - limit.
%%------------------------------------------------------------------------------

-define(UNBREAKABLE_12, "
select column_1,column_2,column_3,column_4,column_5,column_6,column_7, 'Col8' column_8
from dual").

-define(UNBREAKABLE_12_RESULT_DEFAULT, "SELECT
    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, 'Col8'
    Column_8
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 01 - very simple.
%%------------------------------------------------------------------------------

-define(UNION_01, "
select *
from table_1 union select * from table_2").

-define(UNION_01_RESULT_DEFAULT, "    (SELECT
        *
    FROM
        Table_1)
UNION
    (SELECT
        *
    FROM
        Table_2)").

%%------------------------------------------------------------------------------
%% UNION 02 - nested.
%%------------------------------------------------------------------------------

-define(UNION_02, "
select * from (select * from table_11 intersect select * from table_12)
union all select * from (select * from table_21 minus select * from table_22)").

-define(UNION_02_RESULT_DEFAULT, "    (SELECT
        *
    FROM
            ((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12)))
UNION ALL
    (SELECT
        *
    FROM
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                Table_22)))").

%%------------------------------------------------------------------------------
%% UNION 03 - nested.
%%------------------------------------------------------------------------------

-define(UNION_03, "
select * from (select * from table_11 intersect select * from table_12)
union all select * from (select * from table_21 minus select * from (select * from table_31 union select * from table_32))").

-define(UNION_03_RESULT_DEFAULT, "    (SELECT
        *
    FROM
            ((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12)))
UNION ALL
    (SELECT
        *
    FROM
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                    ((SELECT
                        *
                    FROM
                        Table_31)
                UNION
                    (SELECT
                        *
                    FROM
                        Table_32)))))").

%%------------------------------------------------------------------------------
%% UNION 04 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_04, "
select column_1,(select * from dual) union (select * from dual)
from dual").

-define(UNION_04_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION
        (SELECT
            *
        FROM
            Dual))
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 05 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_05, "
select column_1,(select * from dual) union (select * from dual),column_2
from dual").

-define(UNION_05_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION
        (SELECT
            *
        FROM
            Dual)), Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 06 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_06, "
select column_1,(select * from dual) union all (select * from dual)
from dual").

-define(UNION_06_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION ALL
        (SELECT
            *
        FROM
            Dual))
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 07 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_07, "
select column_1,(select * from dual) union all (select * from dual),column_2
from dual").

-define(UNION_07_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION ALL
        (SELECT
            *
        FROM
            Dual)), Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 08 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_08, "
select column_1,(select * from dual) minus (select * from dual)
from dual").

-define(UNION_08_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    MINUS
        (SELECT
            *
        FROM
            Dual))
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 09 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_09, "
select column_1,(select * from dual) minus (select * from dual),column_2
from dual").

-define(UNION_09_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    MINUS
        (SELECT
            *
        FROM
            Dual)), Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 10 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_10, "
select column_1,(select * from dual) intersect (select * from dual)
from dual").

-define(UNION_10_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    INTERSECT
        (SELECT
            *
        FROM
            Dual))
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 11 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_11, "
select column_1,(select * from dual) intersect (select * from dual),column_2
from dual").

-define(UNION_11_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    INTERSECT
        (SELECT
            *
        FROM
            Dual)), Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 12 - ALIAS.
%%------------------------------------------------------------------------------

-define(UNION_12, "
select (select * from dual) union (select * from dual) as column_2
from dual").

-define(UNION_12_RESULT_DEFAULT, "SELECT
        ((SELECT
            *
        FROM
            Dual)
    UNION
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 13 - ALIAS.
%%------------------------------------------------------------------------------

-define(UNION_13, "
select column_1,(select * from dual) union (select * from dual) as column_2
from dual").

-define(UNION_13_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 14 - ALIAS.
%%------------------------------------------------------------------------------

-define(UNION_14, "
select column_1,(select * from dual) union (select * from dual) as column_2,column_3
from dual").

-define(UNION_14_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION
        (SELECT
            *
        FROM
            Dual)) Column_2, Column_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 15 - ALIAS.
%%------------------------------------------------------------------------------

-define(UNION_15, "
select (select * from dual) union all (select * from dual) as column_2
from dual").

-define(UNION_15_RESULT_DEFAULT, "SELECT
        ((SELECT
            *
        FROM
            Dual)
    UNION ALL
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 16 - ALIAS.
%%------------------------------------------------------------------------------

-define(UNION_16, "
select column_1,(select * from dual) union all (select * from dual) as column_2
from dual").

-define(UNION_16_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION ALL
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 17 - ALIAS.
%%------------------------------------------------------------------------------

-define(UNION_17, "
select column_1,(select * from dual) union all (select * from dual) as column_2,column_3
from dual").

-define(UNION_17_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    UNION ALL
        (SELECT
            *
        FROM
            Dual)) Column_2, Column_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 18 - ALIAS & MINUS.
%%------------------------------------------------------------------------------

-define(UNION_18, "
select (select * from dual) minus (select * from dual) as column_2
from dual").

-define(UNION_18_RESULT_DEFAULT, "SELECT
        ((SELECT
            *
        FROM
            Dual)
    MINUS
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 19 - ALIAS & MINUS.
%%------------------------------------------------------------------------------

-define(UNION_19, "
select column_1,(select * from dual) minus (select * from dual) as column_2
from dual").

-define(UNION_19_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    MINUS
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 20 - ALIAS & MINUS.
%%------------------------------------------------------------------------------

-define(UNION_20, "
select column_1,(select * from dual) minus (select * from dual) as column_2,column_3
from dual").

-define(UNION_20_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    MINUS
        (SELECT
            *
        FROM
            Dual)) Column_2, Column_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 21 - ALIAS & INTERSECT.
%%------------------------------------------------------------------------------

-define(UNION_21, "
select (select * from dual) intersect (select * from dual) as column_2
from dual").

-define(UNION_21_RESULT_DEFAULT, "SELECT
        ((SELECT
            *
        FROM
            Dual)
    INTERSECT
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 22 - ALIAS & INTERSECT.
%%------------------------------------------------------------------------------

-define(UNION_22, "
select column_1,(select * from dual) intersect (select * from dual) as column_2
from dual").

-define(UNION_22_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    INTERSECT
        (SELECT
            *
        FROM
            Dual)) Column_2
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 23 - ALIAS & INTERSECT.
%%------------------------------------------------------------------------------

-define(UNION_23, "
select column_1,(select * from dual) intersect (select * from dual) as column_2,column_3
from dual").

-define(UNION_23_RESULT_DEFAULT, "SELECT
    Column_1, " ++ "
        ((SELECT
            *
        FROM
            Dual)
    INTERSECT
        (SELECT
            *
        FROM
            Dual)) Column_2, Column_3
FROM
    Dual").

%%------------------------------------------------------------------------------
%% UNION 24 - SELECT.
%%------------------------------------------------------------------------------

-define(UNION_24, "
select *
from table_1 union select * from table_2").

-define(UNION_24_RESULT_DEFAULT, "    (SELECT
        *
    FROM
        Table_1)
UNION
    (SELECT
        *
    FROM
        Table_2)").

%%------------------------------------------------------------------------------
%% UNION 25 - SELECT FROM.
%%------------------------------------------------------------------------------

-define(UNION_25, "
select *
from (select * from table_1 union select * from table_2)").

-define(UNION_25_RESULT_DEFAULT, "SELECT
    *
FROM
          ((SELECT
            *
        FROM
            Table_1)
    UNION
        (SELECT
            *
        FROM
            Table_2))").

%%------------------------------------------------------------------------------
%% UNION 26 - SELECT WHERE.
%%------------------------------------------------------------------------------

-define(UNION_26, "
select *
from table_1 where column_1 in (select * from table_1 union select * from table_2)").

-define(UNION_26_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
WHERE
    Column_1 IN (((SELECT
            *
        FROM
            Table_1)
    UNION
        (SELECT
            *
        FROM
            Table_2)))").

%%------------------------------------------------------------------------------
%% UNION 27 - UNION.
%%------------------------------------------------------------------------------

-define(UNION_27, "
(select * from table_11 intersect select * from table_12)
union
(select * from table_21 minus select * from table_22)").

-define(UNION_27_RESULT_DEFAULT, "        ((SELECT
            *
        FROM
            Table_11)
    INTERSECT
        (SELECT
            *
        FROM
            Table_12))
UNION
        ((SELECT
            *
        FROM
            Table_21)
    MINUS
        (SELECT
            *
        FROM
            Table_22))").

%%------------------------------------------------------------------------------
%% UNION 28 - UNION FROM.
%%------------------------------------------------------------------------------

-define(UNION_28, "
select *
from ((select * from table_11 intersect select * from table_12) union (select * from table_21 minus select * from table_22))").

-define(UNION_28_RESULT_DEFAULT, "SELECT
    *
FROM
            (((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12))
    UNION
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                Table_22)))").

%%------------------------------------------------------------------------------
%% UNION 29 - UNION WHERE.
%%------------------------------------------------------------------------------

-define(UNION_29, "
select *
from table_1 where column_1 in ((select * from table_11 intersect select * from table_12) union (select * from table_21 minus select * from table_22))").

-define(UNION_29_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
WHERE
    Column_1 IN ((((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12))
    UNION
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                Table_22))))").

%%------------------------------------------------------------------------------
%% UNION 30 - SELECT & UNION.
%%------------------------------------------------------------------------------

-define(UNION_30, "
select *
from table_1 union (select * from table_21 minus select * from table_22)").

-define(UNION_30_RESULT_DEFAULT, "    (SELECT
        *
    FROM
        Table_1)
UNION
        ((SELECT
            *
        FROM
            Table_21)
    MINUS
        (SELECT
            *
        FROM
            Table_22))").

%%------------------------------------------------------------------------------
%% UNION 31 - SELECT & UNION FROM.
%%------------------------------------------------------------------------------

-define(UNION_31, "
select *
from (select * from table_1 union (select * from table_21 minus select * from table_22))").

-define(UNION_31_RESULT_DEFAULT, "SELECT
    *
FROM
        ((SELECT
            *
        FROM
            Table_1)
    UNION
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                Table_22)))").

%%------------------------------------------------------------------------------
%% UNION 32 - SELECT & UNION WHERE.
%%------------------------------------------------------------------------------

-define(UNION_32, "
select *
from table_1 where column_1 in (select * from table_1 union (select * from table_21 minus select * from table_22))").

-define(UNION_32_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
WHERE
    Column_1 IN (((SELECT
            *
        FROM
            Table_1)
    UNION
            ((SELECT
                *
            FROM
                Table_21)
        MINUS
            (SELECT
                *
            FROM
                Table_22))))").

%%------------------------------------------------------------------------------
%% UNION 33 - UNION & SELECT.
%%------------------------------------------------------------------------------

-define(UNION_33, "
(select *
from table_11 intersect select * from table_12) union select * from table_2").

-define(UNION_33_RESULT_DEFAULT, "        ((SELECT
            *
        FROM
            Table_11)
    INTERSECT
        (SELECT
            *
        FROM
            Table_12))
UNION
    (SELECT
        *
    FROM
        Table_2)").

%%------------------------------------------------------------------------------
%% UNION 34 - UNION & SELECT FROM.
%%------------------------------------------------------------------------------

-define(UNION_34, "
select *
from ((select * from table_11 intersect select * from table_12) union select * from table_2)").

-define(UNION_34_RESULT_DEFAULT, "SELECT
    *
FROM
            (((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12))
    UNION
        (SELECT
            *
        FROM
            Table_2))").

%%------------------------------------------------------------------------------
%% UNION 35 - UNION & SELECT WHERE.
%%------------------------------------------------------------------------------

-define(UNION_35, "
select *
from table_1 where column_1 in ((select * from table_11 intersect select * from table_12) union select * from table_2)").

-define(UNION_35_RESULT_DEFAULT, "SELECT
    *
FROM
    Table_1
WHERE
    Column_1 IN ((((SELECT
                *
            FROM
                Table_11)
        INTERSECT
            (SELECT
                *
            FROM
                Table_12))
    UNION
        (SELECT
            *
        FROM
            Table_2)))").

%%------------------------------------------------------------------------------
%% UNION 36 - MINUS & INTERSECT.
%%------------------------------------------------------------------------------

-define(UNION_36, "
(select * from table_1)|:_a::b::c|
Minus ((select * from table_2) Intersect (select * from table_3));").

-define(UNION_36_RESULT_DEFAULT, "((SELECT
        *
    FROM
        Table_1))|:_A::b::c|
MINUS
        ((SELECT
            *
        FROM
            Table_2)
    INTERSECT
        (SELECT
            *
        FROM
            Table_3))").

%%------------------------------------------------------------------------------
%% UNION 37 - INTERSECT & MINUS.
%%------------------------------------------------------------------------------

-define(UNION_37, "
((select * from table_2)
Intersect (select * from table_3)) Minus (select * from table_1)|:_a::b::c|;").

-define(UNION_37_RESULT_DEFAULT, "        ((SELECT
            *
        FROM
            Table_2)
    INTERSECT
        (SELECT
            *
        FROM
            Table_3))
MINUS
    ((SELECT
        *
    FROM
        Table_1))|:_A::b::c|").

%%------------------------------------------------------------------------------
%% UPDATE 01 - simple.
%%------------------------------------------------------------------------------

-define(UPDATE_01, "
update name_table set name_column_1 = :value_1").

-define(UPDATE_01_RESULT_DEFAULT, "UPDATE
    Name_Table
SET
    Name_Column_1 = :value_1").

%%------------------------------------------------------------------------------
%% UPDATE 02 - simple.
%%------------------------------------------------------------------------------

-define(UPDATE_02, "
update name_table set name_column_1 = :value_1, name_column_2 = :value_2").

-define(UPDATE_02_RESULT_DEFAULT, "UPDATE
    Name_Table
SET
    Name_Column_1 = :value_1,
    Name_Column_2 = :value_2").

%%------------------------------------------------------------------------------
%% UPDATE 03 - WHERE.
%%------------------------------------------------------------------------------

-define(UPDATE_03, "
update name_table set name_column_1 = :value_1, name_column_2 = :value_2
where company_id = :id1 and employee_id = :id2").

-define(UPDATE_03_RESULT_DEFAULT, "UPDATE
    Name_Table
SET
    Name_Column_1 = :value_1,
    Name_Column_2 = :value_2
WHERE
    Company_Id = :id1
    AND Employee_Id = :id2").

%%------------------------------------------------------------------------------
%% UPDATE 04 - WHERE.
%%------------------------------------------------------------------------------

-define(UPDATE_04, "
update employees set salary = :sal
where employee_id = :id").

-define(UPDATE_04_RESULT_DEFAULT, "UPDATE
    Employees
SET
    Salary = :sal
WHERE
    Employee_Id = :id").

%%------------------------------------------------------------------------------
%% UPDATE 05 - WHERE & RETURNING.
%%------------------------------------------------------------------------------

-define(UPDATE_05, "
update employees set salary = :sal
where employee_id = :id
returning c,d into :c, :d").

-define(UPDATE_05_RESULT_DEFAULT, "UPDATE
    Employees
SET
    Salary = :sal
WHERE
    Employee_Id = :id
RETURNING
    C, D
INTO
    :c, :d").

%%------------------------------------------------------------------------------
%% UPDATE 06 - WHERE & RETURNING.
%%------------------------------------------------------------------------------

-define(UPDATE_06, "
update employees set salary = :sal
where company_id = :id1 and employee_id2 = :id
returning lob_column into :out_locator").

-define(UPDATE_06_RESULT_DEFAULT, "UPDATE
    Employees
SET
    Salary = :sal
WHERE
    Company_Id = :id1
    AND Employee_Id2 = :id
RETURNING
    Lob_Column
INTO
    :out_locator").

%%------------------------------------------------------------------------------
%% UPDATE 07 - SET.
%%------------------------------------------------------------------------------

-define(UPDATE_07, "
Update table_1 Set
column_1 = (Select * from dual) alias_1,
column_2 = column_3").

-define(UPDATE_07_RESULT_DEFAULT, "UPDATE
    Table_1
SET
    Column_1 = (SELECT
        *
    FROM
        Dual) Alias_1,
    Column_2 = Column_3").

%%------------------------------------------------------------------------------
%% VIEW 01 - simple.
%%------------------------------------------------------------------------------

-define(VIEW_01, "
create view table_1
as select * from dual").

-define(VIEW_01_RESULT_DEFAULT, "CREATE VIEW
    Table_1
AS
    SELECT
        *
    FROM
        Dual").

%%------------------------------------------------------------------------------
%% VIEW 02 - simple CHECK.
%%------------------------------------------------------------------------------

-define(VIEW_02, "
create view table_1
as select * from dual
with check option").

-define(VIEW_02_RESULT_DEFAULT, "CREATE VIEW
    Table_1
AS
    SELECT
        *
    FROM
        Dual
WITH CHECK OPTION").

%%------------------------------------------------------------------------------
%% VIEW 03 - COLUMNS.
%%------------------------------------------------------------------------------

-define(VIEW_03, "
create view table_1
(column_1, column_2)
as select * from dual").

-define(VIEW_03_RESULT_DEFAULT, "CREATE VIEW
    Table_1 (Column_1, Column_2)
AS
    SELECT
        *
    FROM
        Dual").

%%------------------------------------------------------------------------------
%% VIEW 04 - COLUMNS & CHECK.
%%------------------------------------------------------------------------------

-define(VIEW_04, "
create view table_1
(column_1, column_2)
as select * from dual
with check option").

-define(VIEW_04_RESULT_DEFAULT, "CREATE VIEW
    Table_1 (Column_1, Column_2)
AS
    SELECT
        *
    FROM
        Dual
WITH CHECK OPTION").

%%------------------------------------------------------------------------------
%% VIEW 05 - COLUMNS.
%%------------------------------------------------------------------------------

-define(VIEW_05, "
create view table_1
(column_1, column_2, column_3, column_4)
as select * from dual").

-define(VIEW_05_RESULT_DEFAULT, "CREATE VIEW
    Table_1 (Column_1, Column_2, Column_3, Column_4)
AS
    SELECT
        *
    FROM
        Dual").

%%------------------------------------------------------------------------------
%% VIEW 06 - COLUMNS & CHECK.
%%------------------------------------------------------------------------------

-define(VIEW_06, "
create view table_1
(column_1, column_2, column_3, column_4)
as select * from dual
with check option").

-define(VIEW_06_RESULT_DEFAULT, "CREATE VIEW
    Table_1 (Column_1, Column_2, Column_3, Column_4)
AS
    SELECT
        *
    FROM
        Dual
WITH CHECK OPTION").

%%------------------------------------------------------------------------------
%% WHERE 01 - very simple.
%%------------------------------------------------------------------------------

-define(WHERE_01, "
select * from dual
where column_1 = column_2").

-define(WHERE_01_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 = Column_2").

%%------------------------------------------------------------------------------
%% WHERE 02 - AND.
%%------------------------------------------------------------------------------

-define(WHERE_02, "
select * from dual
where column_11 <> column_12
and column_21 != column_22").

-define(WHERE_02_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_11 <> Column_12
    AND Column_21 != Column_22").

%%------------------------------------------------------------------------------
%% WHERE 03 - AND.
%%------------------------------------------------------------------------------

-define(WHERE_03, "
select * from dual
where column_11 <> column_12
and column_21 != column_22
or column_31 <> column_32
and column_41 != column_42").

-define(WHERE_03_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_11 <> Column_12
    AND Column_21 != Column_22
    OR Column_31 <> Column_32
    AND Column_41 != Column_42").

%%------------------------------------------------------------------------------
%% WHERE 04 - AND.
%%------------------------------------------------------------------------------

-define(WHERE_04, "
select * from dual
where column_11 <> column_12
and column_21 != column_22
or column_31 <> column_32
and column_41 != column_42
or column_51 <> column_52
and column_61 != column_62").

-define(WHERE_04_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_11 <> Column_12
    AND Column_21 != Column_22
    OR Column_31 <> Column_32
    AND Column_41 != Column_42
    OR Column_51 <> Column_52
    AND Column_61 != Column_62").

%%------------------------------------------------------------------------------
%% WHERE 05 - left subquery.
%%------------------------------------------------------------------------------

-define(WHERE_05, "
select * from dual
where (select column_31 from table_31) <> column_14").

-define(WHERE_05_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    (SELECT
        Column_31
    FROM
        Table_31) <> Column_14").

%%------------------------------------------------------------------------------
%% WHERE 06 - right subquery.
%%------------------------------------------------------------------------------

-define(WHERE_06, "
select * from dual
where column_11 <> (select column_41 from table_41)").

-define(WHERE_06_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_11 <> (SELECT
        Column_41
    FROM
        Table_41)").

%%------------------------------------------------------------------------------
%% WHERE 07 - both subqueries.
%%------------------------------------------------------------------------------

-define(WHERE_07, "
select * from dual
where (select column_31 from table_31) <> (select column_41 from table_41)").

-define(WHERE_07_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    (SELECT
        Column_31
    FROM
        Table_31) <> (SELECT
        Column_41
    FROM
        Table_41)").

%%------------------------------------------------------------------------------
%% WHERE 08 - AND NOT.
%%------------------------------------------------------------------------------

-define(WHERE_08, "
select * from dual
where column_1 = column_2 and not column_3 = column_4").

-define(WHERE_08_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 = Column_2
    AND NOT (Column_3 = Column_4)").

%%------------------------------------------------------------------------------
%% WHERE 09 - OR NOT.
%%------------------------------------------------------------------------------

-define(WHERE_09, "
select * from dual
where column_1 = column_2 or not column_3 = column_4").

-define(WHERE_09_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 = Column_2
    OR NOT (Column_3 = Column_4)").

%%------------------------------------------------------------------------------
%% WHERE 10 - AND & OR NOT.
%%------------------------------------------------------------------------------

-define(WHERE_10, "
select * from dual
where column_1 = column_2 and not column_3 = column_4 and column_5 = column_6 and not column_7 = column_8").

-define(WHERE_10_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 = Column_2
    AND NOT (Column_3 = Column_4)
    AND Column_5 = Column_6
    AND NOT (Column_7 = Column_8)").

%%------------------------------------------------------------------------------
%% WHERE 11 - BETWEEN, IS NOT, LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_11, "
select * from dual
where column_1 is null and column_2 is not null and column_3 between column_4 and column_6 and column_7 like column_8").

-define(WHERE_11_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 IS NULL
    AND NOT (Column_2 IS NULL)
    AND Column_3 BETWEEN Column_4 AND Column_6
    AND Column_7 LIKE Column_8").

%%------------------------------------------------------------------------------
%% WHERE 12 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_12, "
select * from dual
where column_1 in (1,2,3,4)").

-define(WHERE_12_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 IN (1, 2, 3, 4)").

%%------------------------------------------------------------------------------
%% WHERE 13 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_13, "
select * from dual
where column_1 in (column_2)").

-define(WHERE_13_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 IN (Column_2)").

%%------------------------------------------------------------------------------
%% WHERE 14 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_14, "
select * from dual
where column_1 in (select * from table_2)").

-define(WHERE_14_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 IN (SELECT
        *
    FROM
        Table_2)").

%%------------------------------------------------------------------------------
%% WHERE 15 - ALL.
%%------------------------------------------------------------------------------

-define(WHERE_15, "
select * from dual
where column_1 > all (select * from table_1)").

-define(WHERE_15_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 > ALL
    (SELECT
        *
    FROM
        Table_1)").

%%------------------------------------------------------------------------------
%% WHERE 16 - ANY.
%%------------------------------------------------------------------------------

-define(WHERE_16, "
select * from dual
where column_1 > any (select * from table_1)").

-define(WHERE_16_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 > ANY
    (SELECT
        *
    FROM
        Table_1)").

%%------------------------------------------------------------------------------
%% WHERE 17 - EXISTS.
%%------------------------------------------------------------------------------

-define(WHERE_17, "
select * from dual
where exists (select * from table_1)").

-define(WHERE_17_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    EXISTS
    (SELECT
        *
    FROM
        Table_1)").

%%------------------------------------------------------------------------------
%% WHERE 18 - NOT EXISTS.
%%------------------------------------------------------------------------------

-define(WHERE_18, "
select * from dual
where not exists (select * from table_1)").

-define(WHERE_18_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    NOT (EXISTS
    (SELECT
        *
    FROM
        Table_1))").

%%------------------------------------------------------------------------------
%% WHERE 19 - START WITH & CONNECT BY.
%%------------------------------------------------------------------------------

-define(WHERE_19, "
select * from dual
start with column_1 is null and column_2 < 0 connect by column_2 = column_3 or column_4 <> column_5").

-define(WHERE_19_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
START WITH
    Column_1 IS NULL
    AND Column_2 < 0
CONNECT BY
    Column_2 = Column_3
    OR Column_4 <> Column_5").

%%------------------------------------------------------------------------------
%% WHERE 20 - START WITH & CONNECT BY.
%%------------------------------------------------------------------------------

-define(WHERE_20, "
select * from dual
start with column_1 is null and column_2 < 0 connect by nocycle column_2 = column_3 or column_4 <> column_5").

-define(WHERE_20_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
START WITH
    Column_1 IS NULL
    AND Column_2 < 0
CONNECT BY NOCYCLE
    Column_2 = Column_3
    OR Column_4 <> Column_5").

%%------------------------------------------------------------------------------
%% WHERE 21 - START WITH & CONNECT BY.
%%------------------------------------------------------------------------------

-define(WHERE_21, "
select * from dual
connect by nocycle column_2 = column_3 or column_4 <> column_5 start with column_1 is null and column_2 < 0").

-define(WHERE_21_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
CONNECT BY NOCYCLE
    Column_2 = Column_3
    OR Column_4 <> Column_5
START WITH
    Column_1 IS NULL
    AND Column_2 < 0").

%%------------------------------------------------------------------------------
%% WHERE 22 - SELECT = SELECT.
%%------------------------------------------------------------------------------

-define(WHERE_22, "
select * from dual
where (select column_1 from dual) = (select column_2 from dual)").

-define(WHERE_22_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    (SELECT
        Column_1
    FROM
        Dual) = (SELECT
        Column_2
    FROM
        Dual)").

%%------------------------------------------------------------------------------
%% WHERE 23 - = SELECT.
%%------------------------------------------------------------------------------

-define(WHERE_23, "
select * from dual
where column_1 = (select column_2 from dual)").

-define(WHERE_23_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 = (SELECT
        Column_2
    FROM
        Dual)").

%%------------------------------------------------------------------------------
%% WHERE 24 - SELECT =.
%%------------------------------------------------------------------------------

-define(WHERE_24, "
select * from dual
where (select column_1 from dual) = column_2").

-define(WHERE_24_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    (SELECT
        Column_1
    FROM
        Dual) = Column_2").

%%------------------------------------------------------------------------------
%% WHERE 25 - SELECT =.
%%------------------------------------------------------------------------------

-define(WHERE_25, "
select * from dual
where column_1 = column_2 and column_3 = column_4").

-define(WHERE_25_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 = Column_2
    AND Column_3 = Column_4").

%%------------------------------------------------------------------------------
%% WHERE 26 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_26, "
select * from dual
where column_1 like (3 + 5)").

-define(WHERE_26_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 LIKE 3 + 5").

%%------------------------------------------------------------------------------
%% WHERE 27 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_27, "
select * from dual
where column_1 like (select * from dual)").

-define(WHERE_27_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 LIKE (SELECT
        *
    FROM
        Dual)").

%%------------------------------------------------------------------------------
%% WHERE 28 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_28, "
select * from dual
where column_1 like select * from dual").

-define(WHERE_28_RESULT_DEFAULT, "SELECT
    *
FROM
    Dual
WHERE
    Column_1 LIKE (SELECT
        *
    FROM
        Dual)").

%%------------------------------------------------------------------------------
%% WHERE 29 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_29, "
Delete From table_1 Where
column_1 Not In
(1,2,3)").

-define(WHERE_29_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT (Column_1 IN (1, 2, 3))").

%%------------------------------------------------------------------------------
%% WHERE 30 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_30, "
Delete From table_1 Where
(Select column_1 from table_1) Not In
(Select column_2 from table_2)").

-define(WHERE_30_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT ((SELECT
        Column_1
    FROM
        Table_1) IN (SELECT
        Column_2
    FROM
        Table_2))").

%%------------------------------------------------------------------------------
%% WHERE 31 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_31, "
Delete From table_1 Where
((Select column_2 from table_2) union (Select column_3 from table_3)) Not In
((Select column_4 from table_4) union (Select column_5 from table_5))").

-define(WHERE_31_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT (((SELECT
            Column_2
        FROM
            Table_2)
    UNION
        (SELECT
            Column_3
        FROM
            Table_3)) IN (((SELECT
            Column_4
        FROM
            Table_4)
    UNION
        (SELECT
            Column_5
        FROM
            Table_5))))").

%%------------------------------------------------------------------------------
%% WHERE 32 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_32, "
Delete From table_1
Where column_1 Not Like
column_2").

-define(WHERE_32_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT (Column_1 LIKE Column_2)").

%%------------------------------------------------------------------------------
%% WHERE 33 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_33, "
Delete From table_1
Where (Select column_1 from table_1) Not Like
(Select column_2 from table_2)").

-define(WHERE_33_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT ((SELECT
        Column_1
    FROM
        Table_1) LIKE (SELECT
        Column_2
    FROM
        Table_2))").

%%------------------------------------------------------------------------------
%% WHERE 34 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_34, "
Delete From table_1
Where ((Select column_2 from table_2) union (Select column_3 from table_3)) Not Like
((Select column_4 from table_4) union (Select column_5 from table_5))").

-define(WHERE_34_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT (((SELECT
            Column_2
        FROM
            Table_2)
    UNION
        (SELECT
            Column_3
        FROM
            Table_3)) LIKE (((SELECT
            Column_4
        FROM
            Table_4)
    UNION
        (SELECT
            Column_5
        FROM
            Table_5))))").

%%------------------------------------------------------------------------------
%% WHERE 35 - BETWEEN.
%%------------------------------------------------------------------------------

-define(WHERE_35, "
Delete From table_1
Where column_1 Not between
column_2 and column_3").

-define(WHERE_35_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT (Column_1 BETWEEN Column_2 AND Column_3)").

%%------------------------------------------------------------------------------
%% WHERE 36 - BETWEEN.
%%------------------------------------------------------------------------------

-define(WHERE_36, "
Delete From table_1
Where (Select column_1 from table_1) Not between
(Select column_2 from table_2)
and
(Select column_4 from table_4)").

-define(WHERE_36_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT ((SELECT
        Column_1
    FROM
        Table_1) BETWEEN (SELECT
        Column_2
    FROM
        Table_2) AND (SELECT
        Column_4
    FROM
        Table_4))").

%%------------------------------------------------------------------------------
%% WHERE 37 - BETWEEN.
%%------------------------------------------------------------------------------

-define(WHERE_37, "
Delete From table_1
Where ((Select column_2 from table_2) intersect (Select column_2 from table_2)) Not between
((Select column_3 from table_3) minus (Select column_4 from table_4))
and
((Select column_5 from table_5) union (Select column_6 from table_6))").

-define(WHERE_37_RESULT_DEFAULT, "DELETE FROM
    Table_1
WHERE
    NOT (((SELECT
            Column_2
        FROM
            Table_2)
    INTERSECT
        (SELECT
            Column_2
        FROM
            Table_2)) BETWEEN ((SELECT
            Column_3
        FROM
            Table_3)
    MINUS
        (SELECT
            Column_4
        FROM
            Table_4)) AND ((SELECT
            Column_5
        FROM
            Table_5)
    UNION
        (SELECT
            Column_6
        FROM
            Table_6)))").

-endif.
