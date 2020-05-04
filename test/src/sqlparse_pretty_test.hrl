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

-define(ALTER_USER_01, "\nalter user user_1\ngrant connect through enterprise users;").
-define(
  ALTER_USER_01_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nGRANT CONNECT THROUGH ENTERPRISE USERS;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 02 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_02, "\nalter user user_1\ngrant connect through with no roles;").
-define(
  ALTER_USER_02_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nGRANT CONNECT THROUGH WITH NO ROLES;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 03 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_03, "\nalter user user_1\nrevoke connect through authentication required;").
-define(
  ALTER_USER_03_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH AUTHENTICATION REQUIRED;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 04 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_04,
  "\nalter user user_1\nrevoke connect through with no roles authentication required;"
).
-define(
  ALTER_USER_04_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH NO ROLES AUTHENTICATION REQUIRED;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 05 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_05, "\nalter user user_1\nrevoke connect through with role role_1, role_2;").
-define(
  ALTER_USER_05_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE\n    Role_1, Role_2;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 06 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_06,
  "\nalter user user_1\nrevoke connect through with role role_1, role_2 authentication required;"
).
-define(
  ALTER_USER_06_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE\n    Role_1, Role_2\nAUTHENTICATION REQUIRED;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 07 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_07,
  "\nalter user user_1\nrevoke connect through with role all except role_1, role_2;"
).
-define(
  ALTER_USER_07_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT\n    Role_1, Role_2;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 08 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_08,
  "\nalter user user_1\nrevoke connect through with role all except role_1, role_2 authentication required;"
).
-define(
  ALTER_USER_08_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT\n    Role_1, Role_2\nAUTHENTICATION REQUIRED;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 09 - IDENTIFIED BY.
%%------------------------------------------------------------------------------

-define(ALTER_USER_09, "\nalter user test_user_123\nidentified by new_password;").
-define(
  ALTER_USER_09_RESULT_DEFAULT,
  "ALTER USER\n    Test_User_123\nIDENTIFIED BY\n    new_password;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 10 - IDENTIFIED BY.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_10,
  "\nalter user user_1\nidentified by name_password_1 identified by name_password_2;"
).
-define(
  ALTER_USER_10_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nIDENTIFIED BY\n    name_password_1\nIDENTIFIED BY\n    name_password_2;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 11 - IDENTIFIED EXTERNALLY.
%%------------------------------------------------------------------------------

-define(ALTER_USER_11, "\nalter user user_1\nidentified externally;").
-define(ALTER_USER_11_RESULT_DEFAULT, "ALTER USER\n    User_1\nIDENTIFIED EXTERNALLY;").

%%------------------------------------------------------------------------------
%% ALTER_USER 12 - IDENTIFIED GLOBALLY.
%%------------------------------------------------------------------------------

-define(ALTER_USER_12, "\nalter user user_1\nidentified globally as name_external_1;").
-define(
  ALTER_USER_12_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nIDENTIFIED GLOBALLY AS\n    name_external_1;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 13 - DEFAULT TABLESPACE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_13, "\nalter user user_1\ndefault tablespace tablespace_1;").
-define(
  ALTER_USER_13_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nDEFAULT TABLESPACE\n    Tablespace_1;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 14 - QUOTA.
%%------------------------------------------------------------------------------

-define(ALTER_USER_14, "\nalter user user_1\nquota unlimited on tablespace_1;").
-define(
  ALTER_USER_14_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nQUOTA\n    UNLIMITED ON Tablespace_1;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 15 - QUOTA.
%%------------------------------------------------------------------------------

-define(ALTER_USER_15, "\nalter user user_1\nquota 1024 on tablespace_1;").
-define(ALTER_USER_15_RESULT_DEFAULT, "ALTER USER\n    User_1\nQUOTA\n    1024 ON Tablespace_1;").

%%------------------------------------------------------------------------------
%% ALTER_USER 16 - QUOTA.
%%------------------------------------------------------------------------------

-define(ALTER_USER_16, "\nalter user user_1\nquota 1024 unit_1 on tablespace_1;").
-define(
  ALTER_USER_16_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nQUOTA\n    1024 Unit_1 ON Tablespace_1;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 17 - PROFILE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_17, "\nalter user user_1\nprofile profile_1;").
-define(ALTER_USER_17_RESULT_DEFAULT, "ALTER USER\n    User_1\nPROFILE\n    Profile_1;").

%%------------------------------------------------------------------------------
%% ALTER_USER 18 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_18, "\nalter user user_1\ndefault role all;").
-define(ALTER_USER_18_RESULT_DEFAULT, "ALTER USER\n    User_1\nDEFAULT ROLE ALL;").

%%------------------------------------------------------------------------------
%% ALTER_USER 19 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_19, "\nalter user user_1\ndefault role role_1,role_2;").
-define(ALTER_USER_19_RESULT_DEFAULT, "ALTER USER\n    User_1\nDEFAULT ROLE\n    Role_1, Role_2;").

%%------------------------------------------------------------------------------
%% ALTER_USER 20 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_20, "\nalter user user_1\ndefault role all except role_1;").
-define(
  ALTER_USER_20_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nDEFAULT ROLE ALL EXCEPT\n    Role_1;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 21 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_21, "\nalter user user_1\ndefault role all except role_1,role_2;").
-define(
  ALTER_USER_21_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nDEFAULT ROLE ALL EXCEPT\n    Role_1, Role_2;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 22 - DEFAULT ROLE.
%%------------------------------------------------------------------------------

-define(ALTER_USER_22, "\nalter user user_1\ndefault role none;").
-define(ALTER_USER_22_RESULT_DEFAULT, "ALTER USER\n    User_1\nDEFAULT ROLE NONE;").

%%------------------------------------------------------------------------------
%% ALTER_USER 23 - ACCOUNT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_23, "\nalter user test_user_123\naccount lock;").
-define(ALTER_USER_23_RESULT_DEFAULT, "ALTER USER\n    Test_User_123\nACCOUNT LOCK;").

%%------------------------------------------------------------------------------
%% ALTER_USER 24 - ACCOUNT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_24, "\nalter user test_user_123\naccount unlock;").
-define(ALTER_USER_24_RESULT_DEFAULT, "ALTER USER\n    Test_User_123\nACCOUNT UNLOCK;").

%%------------------------------------------------------------------------------
%% ALTER_USER 25 - PASSWORD.
%%------------------------------------------------------------------------------

-define(ALTER_USER_25, "\nalter user test_user_123\npassword expire;").
-define(ALTER_USER_25_RESULT_DEFAULT, "ALTER USER\n    Test_User_123\nPASSWORD EXPIRE;").

%%------------------------------------------------------------------------------
%% ALTER_USER 26 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(ALTER_USER_26, "\nalter user user_1,user_2,user_3\ngrant connect through enterprise users;").
-define(
  ALTER_USER_26_RESULT_DEFAULT,
  "ALTER USER\n    User_1, User_2, User_3\nGRANT CONNECT THROUGH ENTERPRISE USERS;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 27 - GRANT CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_27,
  "\nalter user user_1,user_2,user_3,user_4\ngrant connect through enterprise users;"
).
-define(
  ALTER_USER_27_RESULT_DEFAULT,
  "ALTER USER\n    User_1, User_2, User_3, User_4\nGRANT CONNECT THROUGH ENTERPRISE USERS;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 28 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_28,
  "\nalter user user_1\nrevoke connect through with role role_1, role_2,role_3,role_4;"
).
-define(
  ALTER_USER_28_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE\n    Role_1, Role_2, Role_3, Role_4;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 29 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_29,
  "\nalter user user_1\nrevoke connect through with role role_1, role_2,role_3,role_4 authentication required;"
).
-define(
  ALTER_USER_29_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE\n    Role_1, Role_2, Role_3, Role_4\nAUTHENTICATION REQUIRED;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 30 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_30,
  "\nalter user user_1\nrevoke connect through with role all except role_1, role_2,role_3,role_4;"
).
-define(
  ALTER_USER_30_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT\n    Role_1, Role_2, Role_3, Role_4;"
).

%%------------------------------------------------------------------------------
%% ALTER_USER 31 - REVOKE CONNECT.
%%------------------------------------------------------------------------------

-define(
  ALTER_USER_31,
  "\nalter user user_1\nrevoke connect through with role all except role_1, role_2,role_3,role_4 authentication required;"
).
-define(
  ALTER_USER_31_RESULT_DEFAULT,
  "ALTER USER\n    User_1\nREVOKE CONNECT THROUGH WITH ROLE ALL EXCEPT\n    Role_1, Role_2, Role_3, Role_4\nAUTHENTICATION REQUIRED;"
).

%%------------------------------------------------------------------------------
%% CREATE 01 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_01, "\ncreate index on table_1;").
-define(CREATE_01_RESULT_DEFAULT, "CREATE INDEX\nON\n    Table_1;").

%%------------------------------------------------------------------------------
%% CREATE 02 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_02, "\ncreate index on table_1 norm_with 'n' filter_with 'f';").
-define(
  CREATE_02_RESULT_DEFAULT,
  "CREATE INDEX\nON\n    Table_1\nNORM_WITH\n    'n'\nFILTER_WITH\n    'f';"
).

%%------------------------------------------------------------------------------
%% CREATE 03 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_03, "\ncreate index on table_1 (column_1,column_2,column_3);").
-define(CREATE_03_RESULT_DEFAULT, "CREATE INDEX\nON\n    Table_1 (Column_1, Column_2, Column_3);").

%%------------------------------------------------------------------------------
%% CREATE 04 - INDEX.
%%------------------------------------------------------------------------------

-define(
  CREATE_04,
  "\ncreate index on table_1 (column_1,column_2,column_3) norm_with 'n' filter_with 'f';"
).
-define(
  CREATE_04_RESULT_DEFAULT,
  "CREATE INDEX\nON\n    Table_1 (Column_1, Column_2, Column_3)\nNORM_WITH\n    'n'\nFILTER_WITH\n    'f';"
).

%%------------------------------------------------------------------------------
%% CREATE 05 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_05, "\ncreate index index_1 on table_1;").
-define(CREATE_05_RESULT_DEFAULT, "CREATE INDEX\n    Index_1\nON\n    Table_1;").

%%------------------------------------------------------------------------------
%% CREATE 06 - INDEX.
%%------------------------------------------------------------------------------

-define(CREATE_06, "\ncreate index index_1 on table_1 norm_with 'n' filter_with 'f';").
-define(
  CREATE_06_RESULT_DEFAULT,
  "CREATE INDEX\n    Index_1\nON\n    Table_1\nNORM_WITH\n    'n'\nFILTER_WITH\n    'f';"
).

%%------------------------------------------------------------------------------
%% CREATE 07 - INDEX.
%%------------------------------------------------------------------------------

-define(
  CREATE_07,
  "\ncreate unique index index_1 on table_1 (column_1,column_2,column_3) norm_with 'n' filter_with 'f';"
).
-define(
  CREATE_07_RESULT_DEFAULT,
  "CREATE UNIQUE INDEX\n    Index_1\nON\n    Table_1 (Column_1, Column_2, Column_3)\nNORM_WITH\n    'n'\nFILTER_WITH\n    'f';"
).

%%------------------------------------------------------------------------------
%% CREATE 08 - INDEX.
%%------------------------------------------------------------------------------

-define(
  CREATE_08,
  "\ncreate bitmap index index_1 on table_1 (column_1,column_2,column_3,column_4) norm_with 'n' filter_with 'f';"
).
-define(
  CREATE_08_RESULT_DEFAULT,
  "CREATE BITMAP INDEX\n    Index_1\nON\n    Table_1 (Column_1, Column_2, Column_3, Column_4)\nNORM_WITH\n    'n'\nFILTER_WITH\n    'f';"
).

%%------------------------------------------------------------------------------
%% CREATE 09 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_09, "\ncreate index index_1 on table_1 alias_1;").
-define(CREATE_09_RESULT_DEFAULT, "CREATE INDEX\n    Index_1\nON\n    Table_1 Alias_1;").

%%------------------------------------------------------------------------------
%% CREATE 10 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_10, "\nCreate Index index_1 On schema_1.table_1 alias_1;").
-define(CREATE_10_RESULT_DEFAULT, "CREATE INDEX\n    Index_1\nON\n    Schema_1.Table_1 Alias_1;").

%%------------------------------------------------------------------------------
%% CREATE 11 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_11, "\nCreate Index index_1 On :param_1 alias_1;").
-define(CREATE_11_RESULT_DEFAULT, "CREATE INDEX\n    Index_1\nON\n    :param_1 Alias_1;").

%%------------------------------------------------------------------------------
%% CREATE 12 - INDEX ALIAS.
%%------------------------------------------------------------------------------

-define(CREATE_12, "\ncreate index index_1 on \"^&()\" alias_1;").
-define(CREATE_12_RESULT_DEFAULT, "CREATE INDEX\n    Index_1\nON\n    \"^&()\" Alias_1;").

%%------------------------------------------------------------------------------
%% CREATE 13 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_13, "\ncreate table table_1 (column_1 date);").
-define(CREATE_13_RESULT_DEFAULT, "CREATE TABLE\n    Table_1 (\n        Column_1 DATE\n    );").

%%------------------------------------------------------------------------------
%% CREATE 14 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_14, "\ncreate table schema_1.table_1 (column_1 date);").
-define(
  CREATE_14_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 15 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_15, "\ncreate table :param_1 (column_1 date);").
-define(CREATE_15_RESULT_DEFAULT, "CREATE TABLE\n    :param_1 (\n        Column_1 DATE\n    );").

%%------------------------------------------------------------------------------
%% CREATE 16 - TABLE.
%%------------------------------------------------------------------------------

-define(CREATE_16, "\ncreate table \"^&()\" (column_1 date);").
-define(CREATE_16_RESULT_DEFAULT, "CREATE TABLE\n    \"^&()\" (\n        Column_1 DATE\n    );").

%%------------------------------------------------------------------------------
%% CREATE 17 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_17,
  "\ncreate table table_1 (column_1 date, column_2 date, foreign key (fkey_1,fkey_2) references table_2);"
).
-define(
  CREATE_17_RESULT_DEFAULT,
  "CREATE TABLE\n    Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES Table_2\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 18 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_18,
  "\ncreate table schema_1.table_1 (column_1 date, column_2 date, unique (ukey_1,ukey_2))\n"
).
-define(
  CREATE_18_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        UNIQUE (Ukey_1, Ukey_2)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 19 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_19,
  "\ncreate table schema_1.table_1 (column_1 date, column_2 date, primary key (pkey_1,pkey_2));"
).
-define(
  CREATE_19_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        PRIMARY KEY (Pkey_1, Pkey_2)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 20 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_20,
  "\ncreate table schema_1.table_1 (column_1 date, column_2 date, foreign key (fkey_1,fkey_2) references schema_1.table_2 (column_8,column_9));"
).
-define(
  CREATE_20_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES Schema_1.Table_2 (Column_8,\n        Column_9)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 21 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_21,
  "\ncreate local ordered_set table table_1\n(column_1 date,\ncolumn_2 varchar2(30),\ncolumn_3 number(5,2));"
).
-define(
  CREATE_21_RESULT_DEFAULT,
  "CREATE LOCAL ORDERED_SET TABLE\n    Table_1 (\n        Column_1 DATE,\n        Column_2 VARCHAR2(30),\n        Column_3 NUMBER(5,2)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 22 - INDEX & FUN.
%%------------------------------------------------------------------------------

-define(CREATE_22, "\ncreate index a on b (f) norm_with fun() -> norm end.;").
-define(
  CREATE_22_RESULT_DEFAULT,
  "CREATE INDEX\n    A\nON\n    B (F)\nNORM_WITH\n    fun() -> norm end.;"
).

%%------------------------------------------------------------------------------
%% CREATE 23 - INDEX & FUN.
%%------------------------------------------------------------------------------

-define(
  CREATE_23,
  "\ncreate index a on b (a|:d{}|) norm_with fun() -> norm end. filter_with fun mod:modfun/5.;"
).
-define(
  CREATE_23_RESULT_DEFAULT,
  "CREATE INDEX\n    A\nON\n    B (A|:d{}|)\nNORM_WITH\n    fun() -> norm end.\nFILTER_WITH\n    fun mod:modfun/5.;"
).

%%------------------------------------------------------------------------------
%% CREATE 24 - INDEX & FUN.
%%------------------------------------------------------------------------------

-define(
  CREATE_24,
  "\ncreate index name_sort on skvhaccount (cvalue|:name|) norm_with fun imem_index:vnf_lcase_ascii/1. filter_with fun imem_index:iff_binterm_list_1/1.;"
).
-define(
  CREATE_24_RESULT_DEFAULT,
  "CREATE INDEX\n    Name_Sort\nON\n    Skvhaccount (Cvalue|:name|)\nNORM_WITH\n    fun imem_index:vnf_lcase_ascii/1.\nFILTER_WITH\n    fun imem_index:iff_binterm_list_1/1.;"
).

%%------------------------------------------------------------------------------
%% CREATE 25 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_25,
  "\nCREATE LOCAL ORDERED_SET TABLE TABLE_1\n(COLUMN_1 DATE,\nCOLUMN_2 VARCHAR2(30),\nCOLUMN_3 NUMBER(5,2));"
).
-define(
  CREATE_25_RESULT_DEFAULT,
  "CREATE LOCAL ORDERED_SET TABLE\n    Table_1 (\n        Column_1 DATE,\n        Column_2 VARCHAR2(30),\n        Column_3 NUMBER(5,2)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 26 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_26,
  "\ncreate table schema_1.table_1 (column_1 date, column_2 date, constraint constraint_1 unique (ukey_1,ukey_2))\n"
).
-define(
  CREATE_26_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        CONSTRAINT Constraint_1 UNIQUE (Ukey_1, Ukey_2)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 27 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_27,
  "\ncreate table table_1 (column_1 date, column_2 date, constraint constraint_1 foreign key (fkey_1,fkey_2) references table_2);"
).
-define(
  CREATE_27_RESULT_DEFAULT,
  "CREATE TABLE\n    Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        CONSTRAINT Constraint_1 FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES Table_2\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 28 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_28,
  "\ncreate table schema_1.table_1 (column_1 date, column_2 date, constraint constraint_1 foreign key (fkey_1,fkey_2) references schema_1.table_2 (column_8,column_9));"
).
-define(
  CREATE_28_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        CONSTRAINT Constraint_1 FOREIGN KEY (Fkey_1, Fkey_2) REFERENCES\n        Schema_1.Table_2 (Column_8, Column_9)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 29 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_29,
  "\ncreate table schema_1.table_1 (column_1 date, column_2 date, check (column_8 = column_9));"
).
-define(
  CREATE_29_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        CHECK (Column_8 = Column_9)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 30 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_30,
  "\ncreate table schema_1.table_1 (column_1 date, column_2 date, constraint constraint_1 check (column_8 = column_9));"
).
-define(
  CREATE_30_RESULT_DEFAULT,
  "CREATE TABLE\n    Schema_1.Table_1 (\n        Column_1 DATE,\n        Column_2 DATE,\n        CONSTRAINT Constraint_1 CHECK (Column_8 = Column_9)\n    );"
).

%%------------------------------------------------------------------------------
%% CREATE 31 - TABLE.
%%------------------------------------------------------------------------------

-define(
  CREATE_31,
  "\nCREATE TABLE\n    TPAC_UMJTT (\n        cko VARCHAR2(4000),\n        chs VARCHAR2(10),\n        jpo VARCHAR2(4000),\n        tsp DATE,\n        ckt VARCHAR2(100),\n        cvt VARCHAR2(10),\n        cvl VARCHAR2(4000),\n        jky VARCHAR2(100),\n        rdi CHAR(1),\n        hnt VARCHAR2(100),\n        CONSTRAINT t_pk PRIMARY KEY (cko, chs, jpo)\n    )\n"
).
-define(
  CREATE_31_RESULT_DEFAULT,
  "CREATE TABLE\n    Tpac_Umjtt (\n        Cko VARCHAR2(4000),\n        Chs VARCHAR2(10),\n        Jpo VARCHAR2(4000),\n        Tsp DATE,\n        Ckt VARCHAR2(100),\n        Cvt VARCHAR2(10),\n        Cvl VARCHAR2(4000),\n        Jky VARCHAR2(100),\n        Rdi CHAR(1),\n        Hnt VARCHAR2(100),\n        CONSTRAINT T_Pk PRIMARY KEY (Cko, Chs, Jpo)\n    );"
).

%%------------------------------------------------------------------------------
%% DELETE 01 - SIMPLE.
%%------------------------------------------------------------------------------

-define(DELETE_01, "\ndelete from table_1;").
-define(DELETE_01_RESULT_DEFAULT, "DELETE FROM\n    Table_1;").

%%------------------------------------------------------------------------------
%% DELETE 03 - SIMPLE RETURN.
%%------------------------------------------------------------------------------

-define(DELETE_03, "\ndelete from table_1 return column_3,column_4 into column_5,column_6;").
-define(
  DELETE_03_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nRETURN\n    Column_3, Column_4\nINTO\n    Column_5, Column_6;"
).

%%------------------------------------------------------------------------------
%% DELETE 04 - SIMPLE RETURN COLUMNS.
%%------------------------------------------------------------------------------

-define(
  DELETE_04,
  "\ndelete from table_1 return column_3,column_4,column_5,column_6 into column_7,column_8,column_9,column_10;"
).
-define(
  DELETE_04_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nRETURN\n    Column_3, Column_4, Column_5, Column_6\nINTO\n    Column_7, Column_8, Column_9, Column_10;"
).

%%------------------------------------------------------------------------------
%% DELETE 05 - CONDITIONED.
%%------------------------------------------------------------------------------

-define(DELETE_05, "\ndelete from table_1 where column_1=value_1 and column_2 = value_2;").
-define(
  DELETE_05_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    Column_1 = Value_1\n    AND Column_2 = Value_2;"
).

%%------------------------------------------------------------------------------
%% DELETE 06 - CONDITIONED RETURN.
%%------------------------------------------------------------------------------

-define(
  DELETE_06,
  "\ndelete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4 into column_5,column_6;"
).
-define(
  DELETE_06_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    Column_1 = Value_1\n    AND Column_2 = Value_2\nRETURN\n    Column_3, Column_4\nINTO\n    Column_5, Column_6;"
).

%%------------------------------------------------------------------------------
%% DELETE 07 - CONDITIONED RETURN COLUMNS.
%%------------------------------------------------------------------------------

-define(
  DELETE_07,
  "\ndelete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4,column_5,column_6 into column_7,column_8,column_9,column_10;"
).
-define(
  DELETE_07_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    Column_1 = Value_1\n    AND Column_2 = Value_2\nRETURN\n    Column_3, Column_4, Column_5, Column_6\nINTO\n    Column_7, Column_8, Column_9, Column_10;"
).

%%------------------------------------------------------------------------------
%% DROP 01 - INDEX.
%%------------------------------------------------------------------------------

-define(DROP_01, "\ndrop index from table_1;").
-define(DROP_01_RESULT_DEFAULT, "DROP INDEX\nFROM\n    Table_1;").

%%------------------------------------------------------------------------------
%% DROP 02 - INDEX.
%%------------------------------------------------------------------------------

-define(DROP_02, "\ndrop index schema_1.index_1 from table_1;").
-define(DROP_02_RESULT_DEFAULT, "DROP INDEX\n    Schema_1.Index_1\nFROM\n    Table_1;").

%%------------------------------------------------------------------------------
%% DROP 03 - ROLE.
%%------------------------------------------------------------------------------

-define(DROP_03, "\ndrop role role_1;").
-define(DROP_03_RESULT_DEFAULT, "DROP ROLE\n    Role_1;").

%%------------------------------------------------------------------------------
%% DROP 04 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_04, "\ndrop table table_1, table_2;").
-define(DROP_04_RESULT_DEFAULT, "DROP TABLE\n    Table_1, Table_2;").

%%------------------------------------------------------------------------------
%% DROP 05 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_05, "\ndrop table table_1, table_2 cascade constraints;").
-define(DROP_05_RESULT_DEFAULT, "DROP TABLE\n    Table_1, Table_2 CASCADE CONSTRAINTS;").

%%------------------------------------------------------------------------------
%% DROP 07 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_07, "\ndrop table if exists table_1,table_2 cascade constraints;").
-define(DROP_07_RESULT_DEFAULT, "DROP TABLE IF EXISTS\n    Table_1, Table_2 CASCADE CONSTRAINTS;").

%%------------------------------------------------------------------------------
%% DROP 09 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_09, "\ndrop imem_dal_skvh table skvhtest;").
-define(DROP_09_RESULT_DEFAULT, "DROP Imem_Dal_Skvh TABLE\n    Skvhtest;").

%%------------------------------------------------------------------------------
%% DROP 10 - USER.
%%------------------------------------------------------------------------------

-define(DROP_10, "\ndrop user user_1;").
-define(DROP_10_RESULT_DEFAULT, "DROP USER User_1;").

%%------------------------------------------------------------------------------
%% DROP 11 - USER.
%%------------------------------------------------------------------------------

-define(DROP_11, "\ndrop user user_1 cascade;").
-define(DROP_11_RESULT_DEFAULT, "DROP USER User_1 CASCADE;").

%%------------------------------------------------------------------------------
%% DROP 12 - TABLE.
%%------------------------------------------------------------------------------

-define(DROP_12, "\ndrop table schema_1.table_1;").
-define(DROP_12_RESULT_DEFAULT, "DROP TABLE\n    Schema_1.Table_1;").

%%------------------------------------------------------------------------------
%% FROM 01 - ALIAS table: param & table.
%%------------------------------------------------------------------------------

-define(FROM_01, "\nselect *\nfrom dual,dual alias_1,:param_2,:param_3 alias_3;").
-define(
  FROM_01_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual,\n    Dual Alias_1,\n    :param_2,\n    :param_3 Alias_3;"
).

%%------------------------------------------------------------------------------
%% FROM 02 - ALIAS table with SCHEMA.
%%------------------------------------------------------------------------------

-define(FROM_02, "\nselect *\nfrom schema_0.dual,schema_1.dual alias_1;").
-define(
  FROM_02_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Schema_0.Dual,\n    Schema_1.Dual Alias_1;"
).

%%------------------------------------------------------------------------------
%% FROM 03 -ALIAS table with DBLINK.
%%------------------------------------------------------------------------------

-define(FROM_03, "\nselect *\nfrom dual\"@link_0\",dual\"@link_1\" alias_1;").
-define(
  FROM_03_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\"@link_0\",\n    Dual\"@link_1\" Alias_1;"
).

%%------------------------------------------------------------------------------
%% FROM 04 - ALIAS table with SCHEMA and DBLINK.
%%------------------------------------------------------------------------------

-define(FROM_04, "\nselect *\nfrom schema_0.dual\"@link_0\",schema_1.dual\"@link_1\" alias_1;").
-define(
  FROM_04_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Schema_0.Dual\"@link_0\",\n    Schema_1.Dual\"@link_1\" Alias_1;"
).

%%------------------------------------------------------------------------------
%% FROM 05 - FROM table.
%%------------------------------------------------------------------------------

-define(FROM_05, "\nselect *\nfrom table_1,table_2 alias_2,table_3;").
-define(
  FROM_05_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1,\n    Table_2 Alias_2,\n    Table_3;"
).

%%------------------------------------------------------------------------------
%% FROM 06 - FROM nested subqueries.
%%------------------------------------------------------------------------------

-define(FROM_06, "\nselect column_1\nfrom (select column_2 from (select column_3 from dual));").
-define(
  FROM_06_RESULT_DEFAULT,
  "SELECT\n    Column_1\nFROM\n    (SELECT\n        Column_2\n    FROM\n        (SELECT\n            Column_3\n        FROM\n            Dual));"
).

%%------------------------------------------------------------------------------
%% FROM 07 - FROM subquery.
%%------------------------------------------------------------------------------

-define(
  FROM_07,
  "\nselect *\nfrom (select column_1 from table_1),(select column_2 from table_2) alias_2,(select column_3 from table_3);"
).
-define(
  FROM_07_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    (SELECT\n        Column_1\n    FROM\n        Table_1),\n    (SELECT\n        Column_2\n    FROM\n        Table_2) Alias_2,\n    (SELECT\n        Column_3\n    FROM\n        Table_3);"
).

%%------------------------------------------------------------------------------
%% FROM 08 - FROM DBLINK.
%%------------------------------------------------------------------------------

-define(
  FROM_08,
  "\nselect *\nfrom (schema_1.table_1\"@dblink_1\" full join schema_2.table_2\"@dblink_2\" );"
).
-define(
  FROM_08_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Schema_1.Table_1\"@dblink_1\"\n    FULL JOIN\n    Schema_2.Table_2\"@dblink_2\";"
).

%%------------------------------------------------------------------------------
%% FROM 09 - PARAM.
%%------------------------------------------------------------------------------

-define(FROM_09, "\nselect *\nfrom :t;").
-define(FROM_09_RESULT_DEFAULT, "SELECT\n    *\nFROM\n    :t;").

%%------------------------------------------------------------------------------
%% FROM 10 - JOIN.
%%------------------------------------------------------------------------------

-define(FROM_10, "\nselect *\nfrom :param_1 alias_1 join :param_2 alias_2 on column_1 = column_2;").
-define(
  FROM_10_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    :param_1 Alias_1\n    JOIN\n    :param_2 Alias_2\n    ON Column_1 = Column_2;"
).

%%------------------------------------------------------------------------------
%% GRANT 01 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_01, "\ngrant create table to user_1;").
-define(GRANT_01_RESULT_DEFAULT, "GRANT\n    CREATE TABLE\nTO\n    User_1;").

%%------------------------------------------------------------------------------
%% GRANT 02 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_02, "\ngrant drop any table to user_1, user_2;").
-define(GRANT_02_RESULT_DEFAULT, "GRANT\n    DROP ANY TABLE\nTO\n    User_1, User_2;").

%%------------------------------------------------------------------------------
%% GRANT 03 - TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_03, "\ngrant super_role to user_1 with delegate option;").
-define(GRANT_03_RESULT_DEFAULT, "GRANT\n    Super_Role\nTO\n    User_1\nWITH DELEGATE OPTION;").

%%------------------------------------------------------------------------------
%% GRANT 04 - TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_04, "\ngrant admin to user_1 with admin option;").
-define(GRANT_04_RESULT_DEFAULT, "GRANT\n    ADMIN\nTO\n    User_1\nWITH ADMIN OPTION;").

%%------------------------------------------------------------------------------
%% GRANT 05 - ON & TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_05, "\ngrant select on table_1 to user_1 with grant option;").
-define(
  GRANT_05_RESULT_DEFAULT,
  "GRANT\n    SELECT\nON\n    Table_1\nTO\n    User_1\nWITH GRANT OPTION;"
).

%%------------------------------------------------------------------------------
%% GRANT 06 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_06, "\ngrant all on table_1 to user_1;").
-define(GRANT_06_RESULT_DEFAULT, "GRANT\n    ALL\nON\n    Table_1\nTO\n    User_1;").

%%------------------------------------------------------------------------------
%% GRANT 07 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_07, "\ngrant delete, insert, select, update on table_1 to user_1;").
-define(
  GRANT_07_RESULT_DEFAULT,
  "GRANT\n    DELETE, INSERT, SELECT, UPDATE\nON\n    Table_1\nTO\n    User_1;"
).

%%------------------------------------------------------------------------------
%% GRANT 08 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_08, "\ngrant role_1 to user_1;").
-define(GRANT_08_RESULT_DEFAULT, "GRANT\n    Role_1\nTO\n    User_1;").

%%------------------------------------------------------------------------------
%% GRANT 09 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_09, "\ngrant privilege_1, privilege_2 to user_1;").
-define(GRANT_09_RESULT_DEFAULT, "GRANT\n    Privilege_1, Privilege_2\nTO\n    User_1;").

%%------------------------------------------------------------------------------
%% GRANT 10 - TO.
%%------------------------------------------------------------------------------

-define(
  GRANT_10,
  "\ngrant privilege_1, privilege_2, privilege_3, privilege_4, privilege_5 to user_1;"
).
-define(
  GRANT_10_RESULT_DEFAULT,
  "GRANT\n    Privilege_1, Privilege_2, Privilege_3, Privilege_4, Privilege_5\nTO\n    User_1;"
).

%%------------------------------------------------------------------------------
%% GRANT 11 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_11, "\ngrant select on ddtable to user_1;").
-define(GRANT_11_RESULT_DEFAULT, "GRANT\n    SELECT\nON\n    Ddtable\nTO\n    User_1;").

%%------------------------------------------------------------------------------
%% GRANT 12 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_12, "\ngrant select on schema1.ddtable to user_1;").
-define(GRANT_12_RESULT_DEFAULT, "GRANT\n    SELECT\nON\n    Schema1.Ddtable\nTO\n    User_1;").

%%------------------------------------------------------------------------------
%% GRANT 13 - ON & TO.
%%------------------------------------------------------------------------------

-define(GRANT_13, "\ngrant all privileges on schema1.ddtable to role_2;").
-define(
  GRANT_13_RESULT_DEFAULT,
  "GRANT\n    ALL PRIVILEGES\nON\n    Schema1.Ddtable\nTO\n    Role_2;"
).

%%------------------------------------------------------------------------------
%% GRANT 14 - TO & addon.
%%------------------------------------------------------------------------------

-define(GRANT_14, "\ngrant manage_system to test_user_1 with admin option;").
-define(
  GRANT_14_RESULT_DEFAULT,
  "GRANT\n    Manage_System\nTO\n    Test_User_1\nWITH ADMIN OPTION;"
).

%%------------------------------------------------------------------------------
%% GRANT 15 - TO.
%%------------------------------------------------------------------------------

-define(GRANT_15, "\ngrant create table to user_1,user_2,user_3,user_4;").
-define(GRANT_15_RESULT_DEFAULT, "GRANT\n    CREATE TABLE\nTO\n    User_1, User_2, User_3, User_4;").

%%------------------------------------------------------------------------------
%% GROUP BY 01 - very simple.
%%------------------------------------------------------------------------------

-define(GROUP_BY_01, "\nselect * from dual\ngroup by column_1, column_2;").
-define(
  GROUP_BY_01_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    Column_1, Column_2;"
).

%%------------------------------------------------------------------------------
%% GROUP BY 02 - column_ref.
%%------------------------------------------------------------------------------

-define(
  GROUP_BY_02,
  "\nselect * from dual\ngroup by column_1,table_1.column_1,schema_1.table_1.column_1,column_1|:x:y|,table_1.column_1|:x:y|,schema_1.table_1.column_1|:x:y|;"
).
-define(
  GROUP_BY_02_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    Column_1, Table_1.Column_1, Schema_1.Table_1.Column_1, Column_1|:x:y|,\n    Table_1.Column_1|:x:y|, Schema_1.Table_1.Column_1|:x:y|;"
).

%%------------------------------------------------------------------------------
%% GROUP BY 03 - column_ref.
%%------------------------------------------------------------------------------

-define(
  GROUP_BY_03,
  "\nselect * from dual\ngroup by column_1(+),table_1.column_1(+),schema_1.table_1.column_1(+),table_1.*,schema_1.table_1.*;"
).
-define(
  GROUP_BY_03_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    Column_1(+), Table_1.Column_1(+), Schema_1.Table_1.Column_1(+), Table_1.*,\n    Schema_1.Table_1.*;"
).

%%------------------------------------------------------------------------------
%% GROUP BY 04 - function_ref.
%%------------------------------------------------------------------------------

-define(GROUP_BY_04, "\nselect * from dual\ngroup by function_1(param_1)|:b[f(p:q)]|;").
-define(
  GROUP_BY_04_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    Function_1(Param_1)|:b[f(p:q)]|;"
).

%%------------------------------------------------------------------------------
%% GROUP BY 05 - function_ref.
%%------------------------------------------------------------------------------

-define(
  GROUP_BY_05,
  "\nselect * from dual\ngroup by function_1(param_1,param_2),package_1.function_1(param_1,param_2),schema_1.package_1.function_1(param_1,param_2),function_1(param_1,param_2)|:b[f(p:q)]|,package_1.function_1(param_1,param_2)|:b[f(p:q)]|,schema_1.package_1.function_1(param_1,param_2)|:b[f(p:q)]|;"
).
-define(
  GROUP_BY_05_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    Function_1(Param_1, Param_2), Package_1.Function_1(Param_1, Param_2),\n    Schema_1.Package_1.Function_1(Param_1, Param_2), Function_1(Param_1, Param_2\n    )|:b[f(p:q)]|, Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|,\n    Schema_1.Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|;"
).

%%------------------------------------------------------------------------------
%% GROUP BY 06 - function_ref.
%%------------------------------------------------------------------------------

-define(
  GROUP_BY_06,
  "\nselect * from dual\ngroup by decode(distinct column_1)|:b[f(p:q)]|,decode(all 6);"
).
-define(
  GROUP_BY_06_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    DECODE(DISTINCT Column_1)|:b[f(p:q)]|, DECODE(ALL 6);"
).

%%------------------------------------------------------------------------------
%% GROUP BY 07 - function_ref.
%%------------------------------------------------------------------------------

-define(
  GROUP_BY_07,
  "\nselect * from dual\ngroup by decode,decode(param_1,param_2),decode(*),decode(distinct column_1),decode(all 6);"
).
-define(
  GROUP_BY_07_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    DECODE(), DECODE(Param_1, Param_2), DECODE(*), DECODE(DISTINCT Column_1),\n    DECODE(ALL 6);"
).

%%------------------------------------------------------------------------------
%% GROUP BY 08 - nested function_ref.
%%------------------------------------------------------------------------------

-define(
  GROUP_BY_08,
  "\nselect * from dual\ngroup by function_1(param_11,function_21(param_21,function_31(param_31,param_32,param_33),param_23),param_13);"
).
-define(
  GROUP_BY_08_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    Function_1(Param_11, Function_21(Param_21, Function_31(Param_31, Param_32,\n    Param_33), Param_23), Param_13);"
).

%%------------------------------------------------------------------------------
%% GROUP BY 09 - COLUMN.
%%------------------------------------------------------------------------------

-define(GROUP_BY_09, "\nselect * from dual\ngroup by column_1, column_2,column_3,column_4;").
-define(
  GROUP_BY_09_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nGROUP BY\n    Column_1, Column_2, Column_3, Column_4;"
).

%%------------------------------------------------------------------------------
%% HAVING 01 - very simple.
%%------------------------------------------------------------------------------

-define(HAVING_01, "\nselect * from dual\nhaving column_1 = column_2;").
-define(HAVING_01_RESULT_DEFAULT, "SELECT\n    *\nFROM\n    Dual\nHAVING\n    Column_1 = Column_2;").

%%------------------------------------------------------------------------------
%% HAVING 02 - simple.
%%------------------------------------------------------------------------------

-define(HAVING_02, "\nselect * from dual\nhaving column_1 = column_2 and column_3 = column_4;").
-define(
  HAVING_02_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nHAVING\n    Column_1 = Column_2\n    AND Column_3 = Column_4;"
).

%%------------------------------------------------------------------------------
%% INSERT 01 - COLUMNS & SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_01,
  "\ninsert into table_1 (column_1,column_2)\nselect column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6\nreturning column_1,column_2 into :a,:b;"
).
-define(
  INSERT_01_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2)\n    SELECT\n        Column_1, Column_2\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6\nRETURNING\n    Column_1, Column_2\nINTO\n    :a, :b;"
).

%%------------------------------------------------------------------------------
%% INSERT 02 - COLUMNS & SELECT.
%%------------------------------------------------------------------------------

-define(
  INSERT_02,
  "\ninsert into table_1 (column_1,column_2)\nselect column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6;"
).
-define(
  INSERT_02_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2)\n    SELECT\n        Column_1, Column_2\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6;"
).

%%------------------------------------------------------------------------------
%% INSERT 03 - COLUMNS & VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_03,
  "\ninsert into table_1 (column_1,column_2)\nvalues (value_1,value_2)\nreturning column_1,column_2 into :a,:b;"
).
-define(
  INSERT_03_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2)\nVALUES\n    (Value_1, Value_2)\nRETURNING\n    Column_1, Column_2\nINTO\n    :a, :b;"
).

%%------------------------------------------------------------------------------
%% INSERT 04 - COLUMNS & VALUES.
%%------------------------------------------------------------------------------

-define(INSERT_04, "\ninsert into table_1 (column_1,column_2)\nvalues (value_1,value_2);").
-define(
  INSERT_04_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2)\nVALUES\n    (Value_1, Value_2);"
).

%%------------------------------------------------------------------------------
%% INSERT 05 - COLUMNS & SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_05,
  "\ninsert into table_1 (column_1,column_2,column_3,column_4)\nselect column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6\nreturning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;"
).
-define(
  INSERT_05_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2, Column_3, Column_4)\n    SELECT\n        Column_1, Column_2, Column_3, Column_4\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6\nRETURNING\n    Column_1, Column_2, Column_3, Column_4\nINTO\n    :a, :b, :c, :d;"
).

%%------------------------------------------------------------------------------
%% INSERT 06 - COLUMNS & SELECT.
%%------------------------------------------------------------------------------

-define(
  INSERT_06,
  "\ninsert into table_1 (column_1,column_2,column_3,column_4)\nselect column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6;"
).
-define(
  INSERT_06_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2, Column_3, Column_4)\n    SELECT\n        Column_1, Column_2, Column_3, Column_4\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6;"
).

%%------------------------------------------------------------------------------
%% INSERT 07 - COLUMNS & VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_07,
  "\ninsert into table_1 (column_1,column_2,column_3,column_4)\nvalues (value_1,value_2,value_3,value_4)\nreturning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;"
).
-define(
  INSERT_07_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2, Column_3, Column_4)\nVALUES\n    (Value_1, Value_2, Value_3, Value_4)\nRETURNING\n    Column_1, Column_2, Column_3, Column_4\nINTO\n    :a, :b, :c, :d;"
).

%%------------------------------------------------------------------------------
%% INSERT 08 - COLUMNS & VALUES.
%%------------------------------------------------------------------------------

-define(
  INSERT_08,
  "\ninsert into table_1 (column_1,column_2,column_3,column_4)\nvalues (value_1,value_2,value_3,value_4);"
).
-define(
  INSERT_08_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1 (\n        Column_1, Column_2, Column_3, Column_4)\nVALUES\n    (Value_1, Value_2, Value_3, Value_4);"
).

%%------------------------------------------------------------------------------
%% INSERT 09 - RETURNING.
%%------------------------------------------------------------------------------

-define(INSERT_09, "\ninsert into table_1\nreturning column_1,column_2 into :a,:b;").
-define(
  INSERT_09_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\nRETURNING\n    Column_1, Column_2\nINTO\n    :a, :b;"
).

%%------------------------------------------------------------------------------
%% INSERT 10 - SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_10,
  "\ninsert into table_1\nselect column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6\nreturning column_1,column_2 into :a,:b;"
).
-define(
  INSERT_10_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\n    SELECT\n        Column_1, Column_2\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6\nRETURNING\n    Column_1, Column_2\nINTO\n    :a, :b;"
).

%%------------------------------------------------------------------------------
%% INSERT 11 - SELECT.
%%------------------------------------------------------------------------------

-define(
  INSERT_11,
  "\ninsert into table_1\nselect column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6;"
).
-define(
  INSERT_11_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\n    SELECT\n        Column_1, Column_2\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6;"
).

%%------------------------------------------------------------------------------
%% INSERT 12 - SELECT & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_12,
  "\ninsert into table_1\nselect column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6\nreturning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;"
).
-define(
  INSERT_12_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\n    SELECT\n        Column_1, Column_2, Column_3, Column_4\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6\nRETURNING\n    Column_1, Column_2, Column_3, Column_4\nINTO\n    :a, :b, :c, :d;"
).

%%------------------------------------------------------------------------------
%% INSERT 13 - SELECT.
%%------------------------------------------------------------------------------

-define(
  INSERT_13,
  "\ninsert into table_1\nselect column_1,column_2,column_3,column_4 from table_2 where column_3 = column_4 and column_5 = column_6;"
).
-define(
  INSERT_13_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\n    SELECT\n        Column_1, Column_2, Column_3, Column_4\n    FROM\n        Table_2\n    WHERE\n        Column_3 = Column_4\n        AND Column_5 = Column_6;"
).

%%------------------------------------------------------------------------------
%% INSERT 14 - VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_14,
  "\ninsert into table_1\nvalues (value_1,value_2)\nreturning column_1,column_2 into :a,:b;"
).
-define(
  INSERT_14_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\nVALUES\n    (Value_1, Value_2)\nRETURNING\n    Column_1, Column_2\nINTO\n    :a, :b;"
).

%%------------------------------------------------------------------------------
%% INSERT 15 - VALUES.
%%------------------------------------------------------------------------------

-define(INSERT_15, "\ninsert into table_1\nvalues (value_1,value_2);").
-define(INSERT_15_RESULT_DEFAULT, "INSERT INTO\n    Table_1\nVALUES\n    (Value_1, Value_2);").

%%------------------------------------------------------------------------------
%% INSERT 16 - VALUES & RETURNING.
%%------------------------------------------------------------------------------

-define(
  INSERT_16,
  "\ninsert into table_1\nvalues (value_1,value_2,value_3,value_4)\nreturning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;"
).
-define(
  INSERT_16_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\nVALUES\n    (Value_1, Value_2, Value_3, Value_4)\nRETURNING\n    Column_1, Column_2, Column_3, Column_4\nINTO\n    :a, :b, :c, :d;"
).

%%------------------------------------------------------------------------------
%% INSERT 17 - VALUES.
%%------------------------------------------------------------------------------

-define(INSERT_17, "\ninsert into table_1\nvalues (value_1,value_2,value_3,value_4);").
-define(
  INSERT_17_RESULT_DEFAULT,
  "INSERT INTO\n    Table_1\nVALUES\n    (Value_1, Value_2, Value_3, Value_4);"
).

%%------------------------------------------------------------------------------
%% INSERT 18 - NONE.
%%------------------------------------------------------------------------------

-define(INSERT_18, "\ninsert into table_1;").
-define(INSERT_18_RESULT_DEFAULT, "INSERT INTO\n    Table_1;").

%%------------------------------------------------------------------------------
%% JOIN 01 - JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_01, "\nselect *\nfrom table_1 join table_2 using (column_1);").
-define(
  JOIN_01_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    JOIN\n    Table_2\n    USING (Column_1);"
).

%%------------------------------------------------------------------------------
%% JOIN 02 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_02, "\nselect *\nfrom table_1 inner join table_2 using (column_1);").
-define(
  JOIN_02_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    INNER JOIN\n    Table_2\n    USING (Column_1);"
).

%%------------------------------------------------------------------------------
%% JOIN 03 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_03, "\nselect *\nfrom table_1 inner join table_2 using (column_1,column_2,column_3);").
-define(
  JOIN_03_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    INNER JOIN\n    Table_2\n    USING (Column_1, Column_2, Column_3);"
).

%%------------------------------------------------------------------------------
%% JOIN 04 - CROSS.
%%------------------------------------------------------------------------------

-define(JOIN_04, "\nselect *\nfrom table_1 cross join table_2;").
-define(JOIN_04_RESULT_DEFAULT, "SELECT\n    *\nFROM\n    Table_1\n    CROSS JOIN\n    Table_2;").

%%------------------------------------------------------------------------------
%% JOIN 05 - NATURAL JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_05, "\nselect *\nfrom table_1 natural join table_2;").
-define(JOIN_05_RESULT_DEFAULT, "SELECT\n    *\nFROM\n    Table_1\n    NATURAL JOIN\n    Table_2;").

%%------------------------------------------------------------------------------
%% JOIN 06 - NATURAL INNER JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_06, "\nselect *\nfrom table_1 natural inner join table_2;").
-define(
  JOIN_06_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    NATURAL INNER JOIN\n    Table_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 07 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(
  JOIN_07,
  "\nselect *\nfrom table_1 inner join table_2 using (column_1,column_2,column_3,column_4);"
).
-define(
  JOIN_07_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    INNER JOIN\n    Table_2\n    USING (Column_1, Column_2, Column_3, Column_4);"
).

%%------------------------------------------------------------------------------
%% JOIN 08 - JOIN ON.
%%------------------------------------------------------------------------------

-define(JOIN_08, "\nselect *\nfrom table_1 join table_2 on table_1.column_1 = table_2.column_2;").
-define(
  JOIN_08_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    JOIN\n    Table_2\n    ON Table_1.Column_1 = Table_2.Column_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 09 - INNER JOIN ON.
%%------------------------------------------------------------------------------

-define(
  JOIN_09,
  "\nselect *\nfrom table_1 inner join table_2 on table_1.column_1 = table_2.column_2 or table_1.column_3 = table_2.column_4;"
).
-define(
  JOIN_09_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    INNER JOIN\n    Table_2\n    ON Table_1.Column_1 = Table_2.Column_2\n    OR Table_1.Column_3 = Table_2.Column_4;"
).

%%------------------------------------------------------------------------------
%% JOIN 10 - INNER JOIN ON.
%%------------------------------------------------------------------------------

-define(
  JOIN_10,
  "\nselect *\nfrom table_1 inner join table_2\non table_1.column_1 = table_2.column_2\nor table_1.column_3 = table_2.column_4\nand table_1.column_5 = table_2.column_6;"
).
-define(
  JOIN_10_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    INNER JOIN\n    Table_2\n    ON Table_1.Column_1 = Table_2.Column_2\n    OR Table_1.Column_3 = Table_2.Column_4\n    AND Table_1.Column_5 = Table_2.Column_6;"
).

%%------------------------------------------------------------------------------
%% JOIN 11 - OUTER JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_11, "\nselect *\nfrom table_1 left outer join table_2;").
-define(
  JOIN_11_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    LEFT OUTER JOIN\n    Table_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 12 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_12, "\nselect *\nfrom table_1 left outer join table_2 using (column_1);").
-define(
  JOIN_12_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    LEFT OUTER JOIN\n    Table_2\n    USING (Column_1);"
).

%%------------------------------------------------------------------------------
%% JOIN 13 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_13, "\nselect *\nfrom table_1 left outer join table_2 on column_1 <> column_2;").
-define(
  JOIN_13_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    LEFT OUTER JOIN\n    Table_2\n    ON Column_1 <> Column_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 14 - CROSS.
%%------------------------------------------------------------------------------

-define(JOIN_14, "\nselect *\nfrom table_1 partition by column_1 left outer join table_2;").
-define(
  JOIN_14_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    PARTITION BY (Column_1)\n    LEFT OUTER JOIN\n    Table_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 15 - NATURAL JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_15, "\nselect *\nfrom table_1 natural left outer join table_2;").
-define(
  JOIN_15_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    NATURAL LEFT OUTER JOIN\n    Table_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 16 - NATURAL INNER JOIN.
%%------------------------------------------------------------------------------

-define(JOIN_16, "\nselect *\nfrom table_1 partition by column_1 natural left outer join table_2;").
-define(
  JOIN_16_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    PARTITION BY (Column_1)\n    NATURAL LEFT OUTER JOIN\n    Table_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 17 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_17, "\nselect *\nfrom table_1 natural left outer join table_2 partition by column_1;").
-define(
  JOIN_17_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    NATURAL LEFT OUTER JOIN\n    Table_2\n    PARTITION BY (Column_1);"
).

%%------------------------------------------------------------------------------
%% JOIN 18 - JOIN ON.
%%------------------------------------------------------------------------------

-define(
  JOIN_18,
  "\nselect *\nfrom table_1 partition by column_1 natural left outer join table_2 partition by column_2 on column_1 = column_2;"
).
-define(
  JOIN_18_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    PARTITION BY (Column_1)\n    NATURAL LEFT OUTER JOIN\n    Table_2\n    PARTITION BY (Column_2)\n    ON Column_1 = Column_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 19 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(
  JOIN_19,
  "\nselect *\nfrom table_1 left outer join table_2 using (column_1,column_2,column_3);"
).
-define(
  JOIN_19_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    LEFT OUTER JOIN\n    Table_2\n    USING (Column_1, Column_2, Column_3);"
).

%%------------------------------------------------------------------------------
%% JOIN 20 - INNER JOIN USING.
%%------------------------------------------------------------------------------

-define(
  JOIN_20,
  "\nselect *\nfrom table_1 left outer join table_2 using (column_1,column_2,column_3,column_4);"
).
-define(
  JOIN_20_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    LEFT OUTER JOIN\n    Table_2\n    USING (Column_1, Column_2, Column_3, Column_4);"
).

%%------------------------------------------------------------------------------
%% JOIN 21 - MIXED JOIN USING.
%%------------------------------------------------------------------------------

-define(
  JOIN_21,
  "\nselect *\nfrom table_1 natural join table_2,\ntable_3,table_5,(select * from dual) alias_1,\ntable_6 join table_7 using (column_1) left outer join table_8;"
).
-define(
  JOIN_21_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\n    NATURAL JOIN\n    Table_2,\n    Table_3,\n    Table_5,\n    (SELECT\n        *\n    FROM\n        Dual) Alias_1,\n    Table_6\n    JOIN\n    Table_7\n    USING (Column_1)\n    LEFT OUTER JOIN\n    Table_8;"
).

%%------------------------------------------------------------------------------
%% JOIN 22 - MIXED JOIN USING.
%%------------------------------------------------------------------------------

-define(JOIN_22, "\nselect *\nfrom table_1,table_2 natural join table_3,table_4;").
-define(
  JOIN_22_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1,\n    Table_2\n    NATURAL JOIN\n    Table_3,\n    Table_4;"
).

%%------------------------------------------------------------------------------
%% JOIN 23 - DBLINK.
%%------------------------------------------------------------------------------

-define(
  JOIN_23,
  "\nselect *\nfrom schema_1.table_1\"@dblink_1\" natural full join schema_2.table_2\"@dblink_2\" on column_1 = column_2;"
).
-define(
  JOIN_23_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Schema_1.Table_1\"@dblink_1\"\n    NATURAL FULL JOIN\n    Schema_2.Table_2\"@dblink_2\"\n    ON Column_1 = Column_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 24 - DBLINK.
%%------------------------------------------------------------------------------

-define(
  JOIN_24,
  "\nselect *\nfrom schema_1.table_1\"@dblink_1\" full join schema_2.table_2\"@dblink_2\" partition by column_1 on column_1 = column_2;"
).
-define(
  JOIN_24_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Schema_1.Table_1\"@dblink_1\"\n    FULL JOIN\n    Schema_2.Table_2\"@dblink_2\"\n    PARTITION BY (Column_1)\n    ON Column_1 = Column_2;"
).

%%------------------------------------------------------------------------------
%% JOIN 25 - DBLINK.
%%------------------------------------------------------------------------------

-define(
  JOIN_25,
  "\nselect *\nfrom :param_1\"@link_1\" alias_1 join :param_1\"@link_1\" alias_1 on column_1 = column_2;"
).
-define(
  JOIN_25_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    :param_1\"@link_1\" Alias_1\n    JOIN\n    :param_1\"@link_1\" Alias_1\n    ON Column_1 = Column_2;"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 02.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_02,
  "\nselect column_11,column_12\nfrom table_11\nwhere column_13 <> column_14\nand column_15 <> column_16\ngroup by column_17, column_18\nhaving column_19 = column_20\nor column_21 = column_22\norder by column_23, column_24;"
).
-define(
  MISCELLANEOUS_02_RESULT_DEFAULT,
  "SELECT\n    Column_11, Column_12\nFROM\n    Table_11\nWHERE\n    Column_13 <> Column_14\n    AND Column_15 <> Column_16\nGROUP BY\n    Column_17, Column_18\nHAVING\n    Column_19 = Column_20\n    OR Column_21 = Column_22\nORDER BY\n    Column_23, Column_24;"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 03.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_03,
  "\nselect *\nfrom table_11\nwhere (select column_31 from table_31) <> column_14;"
).
-define(
  MISCELLANEOUS_03_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_11\nWHERE\n    (SELECT\n        Column_31\n    FROM\n        Table_31) <> Column_14;"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 04.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_04,
  "\nselect (select column_21 from table_21)\nfrom table_11\nwhere (select column_31 from table_31) <> column_14\nhaving (select column_51 from table_51) = column_20\norder by (select column_61 from table_61);"
).
-define(
  MISCELLANEOUS_04_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        Column_21\n    FROM\n        Table_21)\nFROM\n    Table_11\nWHERE\n    (SELECT\n        Column_31\n    FROM\n        Table_31) <> Column_14\nHAVING\n    (SELECT\n        Column_51\n    FROM\n        Table_51) = Column_20\nORDER BY\n    (SELECT\n        Column_61\n    FROM\n        Table_61);"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 05.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_05,
  "\nselect *\nfrom table_11\nwhere column_14 <> (select column_31 from table_31);"
).
-define(
  MISCELLANEOUS_05_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_11\nWHERE\n    Column_14 <> (SELECT\n        Column_31\n    FROM\n        Table_31);"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 06.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_06,
  "\nselect *\nfrom table_11\nwhere (select column_31 from table_31) <> (select column_41 from table_41);"
).
-define(
  MISCELLANEOUS_06_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_11\nWHERE\n    (SELECT\n        Column_31\n    FROM\n        Table_31) <> (SELECT\n        Column_41\n    FROM\n        Table_41);"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 07.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_07,
  "\nselect 'S' || ',' || to_char(BD_DATETIME, 'YYYYMMDDHH24MISS') || ',' || 'I' || ',' || BD_IW_SCENARIO || ',' || DECODE(IS_NUMERIC(BD_MSISDN_A), 0, BD_ORIGSCA, BD_MSISDN_A) || ',' || BD_ORIGSCA || ',' || BD_MSISDN_B || ',' || '41794999021' || ',' || LTRIM(TO_CHAR(NVL(BD_IW_AMOUNT, 0.075) * 10000, '00000')) || ',' || BD_IMSI CSV from SBS1_ADMIN.BDETAIL2_HR_SEG_MASTER_09, SBS1_ADMIN.ENUM256 where to_number(ENUM_ID) <= BD_SEG_COUNT - IS_COUNT and not (BD_MSISDN_A is null) and IS_COUNT < BD_SEG_COUNT and BD_SEG_ID = BD_SEG_COUNT and BD_IW_APMN = 'CHEOR' and BD_DATETIME >= to_date('11.09.2017') and BD_DATETIME < to_date('21.09.2017') order by 1 asc;"
).
-define(
  MISCELLANEOUS_07_RESULT_DEFAULT,
  "SELECT\n    'S' || ',' || TO_CHAR(Bd_Datetime, 'YYYYMMDDHH24MISS') || ',' || 'I' || ','\n    || Bd_Iw_Scenario || ',' || DECODE(Is_Numeric(Bd_Msisdn_A), 0, Bd_Origsca,\n    Bd_Msisdn_A) || ',' || Bd_Origsca || ',' || Bd_Msisdn_B || ',' ||\n    '41794999021' || ',' || LTRIM(TO_CHAR(NVL(Bd_Iw_Amount, 0.075) * 10000,\n    '00000')) || ',' || Bd_Imsi Csv\nFROM\n    Sbs1_Admin.Bdetail2_Hr_Seg_Master_09,\n    Sbs1_Admin.Enum256\nWHERE\n    To_Number(Enum_Id) <= Bd_Seg_Count - Is_Count\n    AND NOT (Bd_Msisdn_A IS NULL)\n    AND Is_Count < Bd_Seg_Count\n    AND Bd_Seg_Id = Bd_Seg_Count\n    AND Bd_Iw_Apmn = 'CHEOR'\n    AND Bd_Datetime >= TO_DATE('11.09.2017')\n    AND Bd_Datetime < TO_DATE('21.09.2017')\nORDER BY\n    1 ASC;"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 08.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_08,
  "\nselect SUBSTR(SED_ORDER, 1, 10) Day, SUM(SED_COUNT1) Mt_SMS, 10 * SUM(SED_COUNT2) Mo_SMSx10, SED_ETID from SETDETAIL where SED_ETID = :SQLT_STR_CDR_TYPE and not (SED_TARID in ('X', 'V', 'S', 'P', 'T')) and SED_ORDER like to_char(sysdate, 'YYYY-MM') || '%' group by SUBSTR(SED_ORDER, 1, 10), SED_ETID order by SUBSTR(SED_ORDER, 1, 10) asc, 2 asc;"
).
-define(
  MISCELLANEOUS_08_RESULT_DEFAULT,
  "SELECT\n    Substr(Sed_Order, 1, 10) Day, SUM(Sed_Count1) Mt_Sms, 10 * SUM(Sed_Count2)\n    Mo_Smsx10, Sed_Etid\nFROM\n    Setdetail\nWHERE\n    Sed_Etid = :SQLT_STR_CDR_TYPE\n    AND NOT (Sed_Tarid IN ('X', 'V', 'S', 'P', 'T'))\n    AND Sed_Order LIKE TO_CHAR(Sysdate, 'YYYY-MM') || '%'\nGROUP BY\n    Substr(Sed_Order, 1, 10), Sed_Etid\nORDER BY\n    Substr(Sed_Order, 1, 10) ASC, 2 ASC;"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 09.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_09,
  "\nselect substr(sed_order, 1, 10) day, sum(sed_total) / sum(sed_count1) avg_charge, sum(sed_total) total_charge, sum(sed_count1) mt_count, sum(sed_count2) mo_count, sed_etid from setdetail where sed_etid = :sqlt_str_cdr_type and not (sed_tarid in ('x', 'v', 's', 'p', 't')) and sed_order like to_char(sysdate, 'yyyy-mm') || '%' group by substr(sed_order, 1, 10), sed_etid order by substr(sed_order, 1, 10) asc, 2 asc;"
).
-define(
  MISCELLANEOUS_09_RESULT_DEFAULT,
  "SELECT\n    Substr(Sed_Order, 1, 10) Day, SUM(Sed_Total) / SUM(Sed_Count1) Avg_Charge,\n    SUM(Sed_Total) Total_Charge, SUM(Sed_Count1) Mt_Count, SUM(Sed_Count2)\n    Mo_Count, Sed_Etid\nFROM\n    Setdetail\nWHERE\n    Sed_Etid = :sqlt_str_cdr_type\n    AND NOT (Sed_Tarid IN ('x', 'v', 's', 'p', 't'))\n    AND Sed_Order LIKE TO_CHAR(Sysdate, 'yyyy-mm') || '%'\nGROUP BY\n    Substr(Sed_Order, 1, 10), Sed_Etid\nORDER BY\n    Substr(Sed_Order, 1, 10) ASC, 2 ASC;"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 10.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_10,
  "\nselect /*+ NO_INDEX(BDETAIL) */ BD_ID, BD_SRCTYPE, BD_DATETIME, BD_DEMO, BD_BIHID, BD_MAPSID, BD_BIRECNO, BD_BOHID1, BD_PACSID1, BD_BORECNO1, BD_BOHID2, BD_PACSID2, BD_BORECNO2, BD_BOHID3, BD_PACSID3, BD_BORECNO3, BD_CONID, BD_PMVID, BD_TARID, BD_NPI_A, BD_TON_A, BD_PID_A, BD_MSISDN_A, BD_TON_B, BD_NPI_B, BD_PID_B, BD_MSISDN_B, BD_IMSI, BD_LENGTH, BD_PREPAID, BD_NODENAME, BD_SERVICE, BD_SHORTID, BD_ITEMNO, BD_KEYWORD, BD_LANGID, BD_REQTYPE, BD_BILLRATE, BD_AMOUNTCU, BD_RETSHAREPV, BD_RETSHAREMO, BD_VSMSCID, BD_CONSOLIDATION, BD_STATUS, BD_TERMDATE, BD_SETTLING, BD_SETID, BD_STORAGE, BD_DBIDSTORE, BD_DATESTORE, BD_ARCHIVE, BD_DBIDARCH, BD_DATEARCH, BD_RECTYPE, BD_IDENTITY, BD_REQSERV, BD_SUBSERV, BD_OPERATION, BD_MULTSM, BD_SMSEQ, BD_ANSTYPE, BD_RETCODE, BD_RESPMEDIA, BD_SMSCRESP, BD_TMINODE, BD_REQID, BD_SERVKEY, BD_BILLTEXT, BD_LOCATION, BD_INFO, BD_ZNID, BD_NETWORK, BD_TPID, BD_STYPE, BD_BILLTIME, BD_MSGID, BD_TRCLASS, BD_AMOUNTTR, BD_TOCID, BD_CDRTID, BD_INT, BD_IW, BD_AAATS, BD_CONSTID, BD_ERRID, BD_BILLED, BD_RATED, BD_PACIDHB, BD_OUTPUTHB, BD_BILLINGDOMAIN, BD_TRANSPORTMEDIUM, BD_CATEGORY, BD_TAXRATE, BD_TESTMODE, BD_REQUESTID, BD_COUNTTR, BD_VSPRCID, BD_ORIGSUBMIT, BD_ONLINECHARGE, BD_LTTREQRECCOUNT, BD_LTTDNRECCOUNT, BD_LTTDNDATETIME, BD_GART, BD_SHOW, BD_CAMPAIGN from BDETAIL where BD_DATETIME >= :SQLT_DAT_FROM and BD_DATETIME < :SQLT_DAT_UNTIL and DECODE(BD_AMOUNTCU, 0.0, '0', '1') || BD_BILLED || DECODE(UPPER(BD_PACIDHB), '<REVAH_PACID>', '1', '0') || BD_MAPSID || DECODE(SUBSTR(BD_MSISDN_A, 1, 5), '42377', 'F', 'S') || BD_PREPAID like :SQLT_STR_CODE;"
).
-define(
  MISCELLANEOUS_10_RESULT_DEFAULT,
  "SELECT /*+ NO_INDEX(BDETAIL) */\n    Bd_Id, Bd_Srctype, Bd_Datetime, Bd_Demo, Bd_Bihid, Bd_Mapsid, Bd_Birecno,\n    Bd_Bohid1, Bd_Pacsid1, Bd_Borecno1, Bd_Bohid2, Bd_Pacsid2, Bd_Borecno2,\n    Bd_Bohid3, Bd_Pacsid3, Bd_Borecno3, Bd_Conid, Bd_Pmvid, Bd_Tarid, Bd_Npi_A,\n    Bd_Ton_A, Bd_Pid_A, Bd_Msisdn_A, Bd_Ton_B, Bd_Npi_B, Bd_Pid_B, Bd_Msisdn_B,\n    Bd_Imsi, Bd_Length, Bd_Prepaid, Bd_Nodename, Bd_Service, Bd_Shortid,\n    Bd_Itemno, Bd_Keyword, Bd_Langid, Bd_Reqtype, Bd_Billrate, Bd_Amountcu,\n    Bd_Retsharepv, Bd_Retsharemo, Bd_Vsmscid, Bd_Consolidation, Bd_Status,\n    Bd_Termdate, Bd_Settling, Bd_Setid, Bd_Storage, Bd_Dbidstore, Bd_Datestore,\n    Bd_Archive, Bd_Dbidarch, Bd_Datearch, Bd_Rectype, Bd_Identity, Bd_Reqserv,\n    Bd_Subserv, Bd_Operation, Bd_Multsm, Bd_Smseq, Bd_Anstype, Bd_Retcode,\n    Bd_Respmedia, Bd_Smscresp, Bd_Tminode, Bd_Reqid, Bd_Servkey, Bd_Billtext,\n    Bd_Location, Bd_Info, Bd_Znid, Bd_Network, Bd_Tpid, Bd_Stype, Bd_Billtime,\n    Bd_Msgid, Bd_Trclass, Bd_Amounttr, Bd_Tocid, Bd_Cdrtid, Bd_Int, Bd_Iw,\n    Bd_Aaats, Bd_Constid, Bd_Errid, Bd_Billed, Bd_Rated, Bd_Pacidhb, Bd_Outputhb\n    , Bd_Billingdomain, Bd_Transportmedium, Bd_Category, Bd_Taxrate, Bd_Testmode\n    , Bd_Requestid, Bd_Counttr, Bd_Vsprcid, Bd_Origsubmit, Bd_Onlinecharge,\n    Bd_Lttreqreccount, Bd_Lttdnreccount, Bd_Lttdndatetime, Bd_Gart, Bd_Show,\n    Bd_Campaign\nFROM\n    Bdetail\nWHERE\n    Bd_Datetime >= :SQLT_DAT_FROM\n    AND Bd_Datetime < :SQLT_DAT_UNTIL\n    AND DECODE(Bd_Amountcu, 0.0, '0', '1') || Bd_Billed ||\n    DECODE(UPPER(Bd_Pacidhb), '<REVAH_PACID>', '1', '0') || Bd_Mapsid ||\n    DECODE(Substr(Bd_Msisdn_A, 1, 5), '42377', 'F', 'S') || Bd_Prepaid LIKE\n    :SQLT_STR_CODE;"
).

%%------------------------------------------------------------------------------
%% MISCELLANEOUS 11.
%%------------------------------------------------------------------------------

-define(
  MISCELLANEOUS_11,
  "\nselect ckey, cvalue from tpac where dderlstag.tpac.cvalue = '{\"NAME\":\"MMS-LA Tariff Search\",\"DESC\":\"Find MMS-LA contracts with given tariff(s). Use single tariff ''1..9'' or ''S/V/X'' as parameter (case sensitive) or pipe-separated list of tariffs ''1|2|3|4|5|6|7|8|9|S|V|X''\",\"PATTERN\":\"\\\"mlprc\\\",\\\"[<*>]\\\"\",\"KEYPATTERN\":[\"tpac\",\"_\",\"mlcon\",\"_\",\"mlprc\"],\"DEFAULT\":\"1\"}' and hd(ckey) = to_string('raw_search');"
).
-define(
  MISCELLANEOUS_11_RESULT_DEFAULT,
  "SELECT\n    Ckey, Cvalue\nFROM\n    Tpac\nWHERE\n    Dderlstag.Tpac.Cvalue =\n    '{\"NAME\":\"MMS-LA Tariff Search\",\"DESC\":\"Find MMS-LA contracts with given tariff(s). Use single tariff ''1..9'' or ''S/V/X'' as parameter (case sensitive) or pipe-separated list of tariffs ''1|2|3|4|5|6|7|8|9|S|V|X''\",\"PATTERN\":\"\\\"mlprc\\\",\\\"[<*>]\\\"\",\"KEYPATTERN\":[\"tpac\",\"_\",\"mlcon\",\"_\",\"mlprc\"],\"DEFAULT\":\"1\"}'\n    AND Hd(Ckey) = To_String('raw_search');"
).

%%------------------------------------------------------------------------------
%% MULTIPLE 01 - CREATE.
%%------------------------------------------------------------------------------

-define(
  MULTIPLE_01,
  "\ncreate table hr_regions\n(\n    region_id   integer          not null primary key,\n    region_name varchar2(25)\n);\ncreate table hr_jobs\n(\n    job_id     varchar2(10)      not null primary key,\n    job_title  varchar2(35)      not null,\n    min_salary number(6,0),\n    max_salary number(6,0)\n);"
).
-define(
  MULTIPLE_01_RESULT_DEFAULT,
  "CREATE TABLE\n    Hr_Regions (\n        Region_Id INTEGER NOT NULL PRIMARY KEY,\n        Region_Name VARCHAR2(25)\n    );\nCREATE TABLE\n    Hr_Jobs (\n        Job_Id VARCHAR2(10) NOT NULL PRIMARY KEY,\n        Job_Title VARCHAR2(35) NOT NULL,\n        Min_Salary NUMBER(6,0),\n        Max_Salary NUMBER(6,0)\n    );"
).

%%------------------------------------------------------------------------------
%% MULTIPLE 02 - INSERT.
%%------------------------------------------------------------------------------

-define(
  MULTIPLE_02,
  "\ninsert into hr_regions (region_id,region_name) values (1,'europe');\ninsert into hr_regions (region_id,region_name) values (2,'americas');\ninsert into hr_regions (region_id,region_name) values (3,'asia');\ninsert into hr_regions (region_id,region_name) values (4,'middle east and africa');"
).
-define(
  MULTIPLE_02_RESULT_DEFAULT,
  "INSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (1, 'europe');\nINSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (2, 'americas');\nINSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (3, 'asia');\nINSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (4, 'middle east and africa');"
).

%%------------------------------------------------------------------------------
%% MULTIPLE 03 - INSERT.
%%------------------------------------------------------------------------------

-define(
  MULTIPLE_03,
  "\ninsert into hr_regions (region_id,region_name) values (1,'europe');\nname_label_1;\ninsert into hr_regions (region_id,region_name) values (2,'americas');\ninsert into hr_regions (region_id,region_name) values (3,'asia');\nname_label_2;\ninsert into hr_regions (region_id,region_name) values (4,'middle east and africa');\nname_label_3;"
).
-define(
  MULTIPLE_03_RESULT_DEFAULT,
  "INSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (1, 'europe');\nname_label_1;\nINSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (2, 'americas');\nINSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (3, 'asia');\nname_label_2;\nINSERT INTO\n    Hr_Regions (\n        Region_Id, Region_Name)\nVALUES\n    (4, 'middle east and africa');\nname_label_3;"
).

%%------------------------------------------------------------------------------
%% OPTION 01.
%%------------------------------------------------------------------------------

-define(
  OPTION_01,
  "\nselect columN_1\nfrom tablE_1\nwhere columN_3 <> columN_4\ngroup by columN_5\nhaving columN_6 = columN_7\norder by columN_8;"
).
-define(
  OPTION_01_RESULT_DEFAULT,
  "SELECT\n    Column_1\nFROM\n    Table_1\nWHERE\n    Column_3 <> Column_4\nGROUP BY\n    Column_5\nHAVING\n    Column_6 = Column_7\nORDER BY\n    Column_8;"
).
-define(
  OPTION_01_RESULT_K_I_4_S_T,
  "Select\n    columN_1\nFrom\n    tablE_1\nWhere\n    columN_3 <> columN_4\nGroup By\n    columN_5\nHaving\n    columN_6 = columN_7\nOrder By\n    columN_8;"
).
-define(
  OPTION_01_RESULT_K_L_4_S_F,
  "select\n    columN_1\nfrom\n    tablE_1\nwhere\n    columN_3<>columN_4\ngroup by\n    columN_5\nhaving\n    columN_6=columN_7\norder by\n    columN_8;"
).
-define(
  OPTION_01_RESULT_L_U_4_S_T,
  "SELECT\n    column_1\nFROM\n    table_1\nWHERE\n    column_3 <> column_4\nGROUP BY\n    column_5\nHAVING\n    column_6 = column_7\nORDER BY\n    column_8;"
).
-define(
  OPTION_01_RESULT_U_L_1_S_T,
  "select\n COLUMN_1\nfrom\n TABLE_1\nwhere\n COLUMN_3 <> COLUMN_4\ngroup by\n COLUMN_5\nhaving\n COLUMN_6 = COLUMN_7\norder by\n COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L_2_S_T,
  "select\n  COLUMN_1\nfrom\n  TABLE_1\nwhere\n  COLUMN_3 <> COLUMN_4\ngroup by\n  COLUMN_5\nhaving\n  COLUMN_6 = COLUMN_7\norder by\n  COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L_3_S_T,
  "select\n   COLUMN_1\nfrom\n   TABLE_1\nwhere\n   COLUMN_3 <> COLUMN_4\ngroup by\n   COLUMN_5\nhaving\n   COLUMN_6 = COLUMN_7\norder by\n   COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L_4_S_T,
  "select\n    COLUMN_1\nfrom\n    TABLE_1\nwhere\n    COLUMN_3 <> COLUMN_4\ngroup by\n    COLUMN_5\nhaving\n    COLUMN_6 = COLUMN_7\norder by\n    COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L_5_S_T,
  "select\n     COLUMN_1\nfrom\n     TABLE_1\nwhere\n     COLUMN_3 <> COLUMN_4\ngroup by\n     COLUMN_5\nhaving\n     COLUMN_6 = COLUMN_7\norder by\n     COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L_6_S_T,
  "select\n      COLUMN_1\nfrom\n      TABLE_1\nwhere\n      COLUMN_3 <> COLUMN_4\ngroup by\n      COLUMN_5\nhaving\n      COLUMN_6 = COLUMN_7\norder by\n      COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L_7_S_T,
  "select\n       COLUMN_1\nfrom\n       TABLE_1\nwhere\n       COLUMN_3 <> COLUMN_4\ngroup by\n       COLUMN_5\nhaving\n       COLUMN_6 = COLUMN_7\norder by\n       COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L_8_S_T,
  "select\n        COLUMN_1\nfrom\n        TABLE_1\nwhere\n        COLUMN_3 <> COLUMN_4\ngroup by\n        COLUMN_5\nhaving\n        COLUMN_6 = COLUMN_7\norder by\n        COLUMN_8;"
).
-define(
  OPTION_01_RESULT_U_L___T_T,
  "select\n\tCOLUMN_1\nfrom\n\tTABLE_1\nwhere\n\tCOLUMN_3 <> COLUMN_4\ngroup by\n\tCOLUMN_5\nhaving\n\tCOLUMN_6 = COLUMN_7\norder by\n\tCOLUMN_8;"
).

%%------------------------------------------------------------------------------
%% OPTION 02.
%%------------------------------------------------------------------------------

-define(
  OPTION_02,
  "\nselect columN_1,+columN_2\nfrom tablE_1\nwhere columN_3_1 <> columN_4_1\nand columN_3_2 > columN_4_2\nand columN_3_3 >= columN_4_3\nand columN_3_4 <= columN_4_4\ngroup by columN_5\nhaving columN_6 = columN_7\norder by columN_8;"
).
-define(
  OPTION_02_RESULT_K_L_4_S_F,
  "select\n    columN_1, +columN_2\nfrom\n    tablE_1\nwhere\n    columN_3_1<>columN_4_1\n    and columN_3_2>columN_4_2\n    and columN_3_3>=columN_4_3\n    and columN_3_4<=columN_4_4\ngroup by\n    columN_5\nhaving\n    columN_6=columN_7\norder by\n    columN_8;"
).

%%------------------------------------------------------------------------------
%% ORDER BY 01 - very simple.
%%------------------------------------------------------------------------------

-define(ORDER_BY_01, "\nselect * from dual\norder by column_1, column_2;").
-define(
  ORDER_BY_01_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nORDER BY\n    Column_1, Column_2;"
).

%%------------------------------------------------------------------------------
%% ORDER BY 02 - COLUMNS.
%%------------------------------------------------------------------------------

-define(ORDER_BY_02, "\nselect * from dual\norder by column_1,column_2,column_3;").
-define(
  ORDER_BY_02_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nORDER BY\n    Column_1, Column_2, Column_3;"
).

%%------------------------------------------------------------------------------
%% ORDER BY 03 - COLUMNS.
%%------------------------------------------------------------------------------

-define(ORDER_BY_03, "\nselect * from dual\norder by column_1,column_2,column_3,column_4;").
-define(
  ORDER_BY_03_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nORDER BY\n    Column_1, Column_2, Column_3, Column_4;"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 01 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_01, "\nselect 1-3+5 from dual;").
-define(PARENTHESES_01_RESULT_DEFAULT, "SELECT\n    (1 - 3) + 5\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% PARENTHESES 02 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_02, "\nselect (1-3)+5 from dual;").
-define(PARENTHESES_02_RESULT_DEFAULT, "SELECT\n    (1 - 3) + 5\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% PARENTHESES 03 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_03, "\nselect 1-(3+5) from dual;").
-define(PARENTHESES_03_RESULT_DEFAULT, "SELECT\n    1 - (3 + 5)\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% PARENTHESES 04 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_04, "\nselect 1-2+3-4 from dual;").
-define(PARENTHESES_04_RESULT_DEFAULT, "SELECT\n    ((1 - 2) + 3) - 4\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% PARENTHESES 05 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_05, "\nselect 1-(2+3-4) from dual;").
-define(PARENTHESES_05_RESULT_DEFAULT, "SELECT\n    1 - ((2 + 3) - 4)\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% PARENTHESES 06 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_06, "\nselect 1-2+(3-4) from dual;").
-define(PARENTHESES_06_RESULT_DEFAULT, "SELECT\n    (1 - 2) + (3 - 4)\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% PARENTHESES 07 - Arithmetic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_07, "\nselect 1-(2+3)-4 from dual;").
-define(PARENTHESES_07_RESULT_DEFAULT, "SELECT\n    (1 - (2 + 3)) - 4\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% PARENTHESES 11 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_11, "\nselect * from dual where a=b and c=d or e=f and g=e;").
-define(
  PARENTHESES_11_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    A = B\n    AND C = D\n    OR E = F\n    AND G = E;"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 12 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_12, "\nselect * from dual where a=b and (c=d or e=f) and g=e;").
-define(
  PARENTHESES_12_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    A = B\n    AND (C = D\n    OR E = F)\n    AND G = E;"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 13 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_13, "\nselect * from dual where not a=b and (c=d or e=f) and g=e;").
-define(
  PARENTHESES_13_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    NOT (A = B)\n    AND (C = D\n    OR E = F)\n    AND G = E;"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 14 - Logic operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_14, "\nselect * from dual where not a=b and not (c=d or e=f) and g=e;").
-define(
  PARENTHESES_14_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    NOT (A = B)\n    AND NOT (C = D\n    OR E = F)\n    AND G = E;"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 21 - Set operators.
%%------------------------------------------------------------------------------

-define(PARENTHESES_21, "\nselect column_1 from table_1\nintersect\nselect column_2 from table_2\n").
-define(
  PARENTHESES_21_RESULT_DEFAULT,
  "    (SELECT\n        Column_1\n    FROM\n        Table_1)\nINTERSECT\n    (SELECT\n        Column_2\n    FROM\n        Table_2);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 22 - Set operators.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_22,
  "\nselect column_1 from table_1\nintersect\nselect column_2 from table_2\nminus\nselect column_3 from table_3\n"
).
-define(
  PARENTHESES_22_RESULT_DEFAULT,
  "        ((SELECT\n            Column_1\n        FROM\n            Table_1)\n    INTERSECT\n        (SELECT\n            Column_2\n        FROM\n            Table_2))\nMINUS\n    (SELECT\n        Column_3\n    FROM\n        Table_3);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 23 - Set operators.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_23,
  "\nselect column_1 from table_1\nintersect\nselect column_2 from table_2\nminus\nselect column_3 from table_3\nunion\nselect column_4 from table_4\n"
).
-define(
  PARENTHESES_23_RESULT_DEFAULT,
  "            (((SELECT\n                Column_1\n            FROM\n                Table_1)\n        INTERSECT\n            (SELECT\n                Column_2\n            FROM\n                Table_2))\n    MINUS\n        (SELECT\n            Column_3\n        FROM\n            Table_3))\nUNION\n    (SELECT\n        Column_4\n    FROM\n        Table_4);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 24 - Set operators.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_24,
  "\nselect column_1 from table_1\nintersect\nselect column_2 from table_2\nminus\nselect column_3 from table_3\nunion\nselect column_4 from table_4\nunion all\nselect column_5 from table_5\n"
).
-define(
  PARENTHESES_24_RESULT_DEFAULT,
  "                ((((SELECT\n                    Column_1\n                FROM\n                    Table_1)\n            INTERSECT\n                (SELECT\n                    Column_2\n                FROM\n                    Table_2))\n        MINUS\n            (SELECT\n                Column_3\n            FROM\n                Table_3))\n    UNION\n        (SELECT\n            Column_4\n        FROM\n            Table_4))\nUNION ALL\n    (SELECT\n        Column_5\n    FROM\n        Table_5);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 25 - Set operators.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_25,
  "\n(select column_1 from table_1\nintersect\nselect column_2 from table_2)\nminus\nselect column_3 from table_3\nunion\nselect column_4 from table_4\nunion all\nselect column_5 from table_5\n"
).
-define(
  PARENTHESES_25_RESULT_DEFAULT,
  "                ((((SELECT\n                    Column_1\n                FROM\n                    Table_1)\n            INTERSECT\n                (SELECT\n                    Column_2\n                FROM\n                    Table_2))\n        MINUS\n            (SELECT\n                Column_3\n            FROM\n                Table_3))\n    UNION\n        (SELECT\n            Column_4\n        FROM\n            Table_4))\nUNION ALL\n    (SELECT\n        Column_5\n    FROM\n        Table_5);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 26 - Set operators.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_26,
  "\nselect column_1 from table_1\nintersect\n(select column_2 from table_2\nminus\nselect column_3 from table_3)\nunion\nselect column_4 from table_4\nunion all\nselect column_5 from table_5\n"
).
-define(
  PARENTHESES_26_RESULT_DEFAULT,
  "            (((SELECT\n                Column_1\n            FROM\n                Table_1)\n        INTERSECT\n                ((SELECT\n                    Column_2\n                FROM\n                    Table_2)\n            MINUS\n                (SELECT\n                    Column_3\n                FROM\n                    Table_3)))\n    UNION\n        (SELECT\n            Column_4\n        FROM\n            Table_4))\nUNION ALL\n    (SELECT\n        Column_5\n    FROM\n        Table_5);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 27 - Set operators.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_27,
  "\nselect column_1 from table_1\nintersect\nselect column_2 from table_2\nminus\n(select column_3 from table_3\nunion\nselect column_4 from table_4)\nunion all\nselect column_5 from table_5\n"
).
-define(
  PARENTHESES_27_RESULT_DEFAULT,
  "            (((SELECT\n                Column_1\n            FROM\n                Table_1)\n        INTERSECT\n            (SELECT\n                Column_2\n            FROM\n                Table_2))\n    MINUS\n            ((SELECT\n                Column_3\n            FROM\n                Table_3)\n        UNION\n            (SELECT\n                Column_4\n            FROM\n                Table_4)))\nUNION ALL\n    (SELECT\n        Column_5\n    FROM\n        Table_5);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 28 - Set operators.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_28,
  "\nselect column_1 from table_1\nintersect\nselect column_2 from table_2\nminus\nselect column_3 from table_3\nunion\n(select column_4 from table_4\nunion all\nselect column_5 from table_5)\n"
).
-define(
  PARENTHESES_28_RESULT_DEFAULT,
  "            (((SELECT\n                Column_1\n            FROM\n                Table_1)\n        INTERSECT\n            (SELECT\n                Column_2\n            FROM\n                Table_2))\n    MINUS\n        (SELECT\n            Column_3\n        FROM\n            Table_3))\nUNION\n        ((SELECT\n            Column_4\n        FROM\n            Table_4)\n    UNION ALL\n        (SELECT\n            Column_5\n        FROM\n            Table_5));"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 29 - MINIMAL.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_29,
  "\nselect 1 * 2 + 3 * 4 from dual where a and b or c and d;\nselect 1 * (2 + 3) * 4 from dual where a and (b or c) and d;\nselect 1 + 2 * 3 + 4 from dual where a or b and c or d;\nselect (1 + 2) * (3 + 4) from dual where (a or b) and (c or d)\n"
).
-define(
  PARENTHESES_29_RESULT_DEFAULT,
  "SELECT\n    1 * 2 + 3 * 4\nFROM\n    Dual\nWHERE\n    A\n    AND B\n    OR C\n    AND D;\nSELECT\n    1 * (2 + 3) * 4\nFROM\n    Dual\nWHERE\n    A\n    AND (B\n    OR C)\n    AND D;\nSELECT\n    1 + 2 * 3 + 4\nFROM\n    Dual\nWHERE\n    A\n    OR B\n    AND C\n    OR D;\nSELECT\n    (1 + 2) * (3 + 4)\nFROM\n    Dual\nWHERE\n    (A\n    OR B)\n    AND (C\n    OR D);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 30 - MINIMAL.
%%------------------------------------------------------------------------------

-define(PARENTHESES_30, "\nselect 1-(3+5) from dual;\nselect 1-(3-5) from dual\n").
-define(
  PARENTHESES_30_RESULT_DEFAULT,
  "SELECT\n    1 - (3 + 5)\nFROM\n    Dual;\nSELECT\n    1 - (3 - 5)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 31 - MINIMAL.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_31,
  "\nselect 1-2+3-4 from dual;\nselect (1-2)+3-4 from dual;\nselect 1-2+(3-4) from dual;\nselect (1-2)+(3-4) from dual\n"
).
-define(
  PARENTHESES_31_RESULT_DEFAULT,
  "SELECT\n    ((1 - 2) + 3) - 4\nFROM\n    Dual;\nSELECT\n    ((1 - 2) + 3) - 4\nFROM\n    Dual;\nSELECT\n    (1 - 2) + (3 - 4)\nFROM\n    Dual;\nSELECT\n    (1 - 2) + (3 - 4)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 32 - SET.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_32,
  "\nselect * from table_1 union select * from table_2 intersect select * from table_3 union select * from table_4;\nselect * from table_1 union all select * from table_2 union select * from table_3 union all select * from table_4;\nselect * from table_1 minus select * from table_2 union select * from table_3 union all select * from table_4;\nselect * from table_1 union all select * from table_2 minus select * from table_3 union all select * from table_4;\nselect * from table_1 union all (select * from table_2 minus select * from table_3) union all select * from table_4;\nselect * from table_1 minus select * from table_2 minus select * from table_3 minus select * from table_4;\nselect * from table_1 minus (select * from table_2 union select * from table_3 union all select * from table_4);\n(select * from table_1 union all select * from table_2) minus (select * from table_3 union all select * from table_4);\nselect * from table_1 minus (select * from table_2 minus select * from table_3 minus select * from table_4);\n"
).
-define(
  PARENTHESES_32_RESULT_DEFAULT,
  "            (((SELECT\n                *\n            FROM\n                Table_1)\n        UNION\n            (SELECT\n                *\n            FROM\n                Table_2))\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Table_3))\nUNION\n    (SELECT\n        *\n    FROM\n        Table_4);\n            (((SELECT\n                *\n            FROM\n                Table_1)\n        UNION ALL\n            (SELECT\n                *\n            FROM\n                Table_2))\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_3))\nUNION ALL\n    (SELECT\n        *\n    FROM\n        Table_4);\n            (((SELECT\n                *\n            FROM\n                Table_1)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_2))\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_3))\nUNION ALL\n    (SELECT\n        *\n    FROM\n        Table_4);\n            (((SELECT\n                *\n            FROM\n                Table_1)\n        UNION ALL\n            (SELECT\n                *\n            FROM\n                Table_2))\n    MINUS\n        (SELECT\n            *\n        FROM\n            Table_3))\nUNION ALL\n    (SELECT\n        *\n    FROM\n        Table_4);\n        ((SELECT\n            *\n        FROM\n            Table_1)\n    UNION ALL\n            ((SELECT\n                *\n            FROM\n                Table_2)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_3)))\nUNION ALL\n    (SELECT\n        *\n    FROM\n        Table_4);\n            (((SELECT\n                *\n            FROM\n                Table_1)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_2))\n    MINUS\n        (SELECT\n            *\n        FROM\n            Table_3))\nMINUS\n    (SELECT\n        *\n    FROM\n        Table_4);\n    (SELECT\n        *\n    FROM\n        Table_1)\nMINUS\n            (((SELECT\n                *\n            FROM\n                Table_2)\n        UNION\n            (SELECT\n                *\n            FROM\n                Table_3))\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Table_4));\n        ((SELECT\n            *\n        FROM\n            Table_1)\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Table_2))\nMINUS\n        ((SELECT\n            *\n        FROM\n            Table_3)\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Table_4));\n    (SELECT\n        *\n    FROM\n        Table_1)\nMINUS\n            (((SELECT\n                *\n            FROM\n                Table_2)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_3))\n    MINUS\n        (SELECT\n            *\n        FROM\n            Table_4));"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 40 - SET.
%%------------------------------------------------------------------------------

-define(PARENTHESES_40, "\nselect * from table_1 minus select * from table_2;").
-define(
  PARENTHESES_40_RESULT_DEFAULT,
  "    (SELECT\n        *\n    FROM\n        Table_1)\nMINUS\n    (SELECT\n        *\n    FROM\n        Table_2);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 41 - SET.
%%------------------------------------------------------------------------------

-define(PARENTHESES_41, "\nselect * from table_1 union select * from table_2;").
-define(
  PARENTHESES_41_RESULT_DEFAULT,
  "    (SELECT\n        *\n    FROM\n        Table_1)\nUNION\n    (SELECT\n        *\n    FROM\n        Table_2);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 42 - SET.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_42,
  "\nselect * from table_1 minus select * from table_2\nminus\nselect * from table_3 minus select * from table_4;"
).
-define(
  PARENTHESES_42_RESULT_DEFAULT,
  "            (((SELECT\n                *\n            FROM\n                Table_1)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_2))\n    MINUS\n        (SELECT\n            *\n        FROM\n            Table_3))\nMINUS\n    (SELECT\n        *\n    FROM\n        Table_4);"
).

%%------------------------------------------------------------------------------
%% PARENTHESES 43 - SET.
%%------------------------------------------------------------------------------

-define(
  PARENTHESES_43,
  "\nselect * from table_1 minus\n(select * from table_2 minus select * from table_3)\nminus select * from table_4;"
).
-define(
  PARENTHESES_43_RESULT_DEFAULT,
  "        ((SELECT\n            *\n        FROM\n            Table_1)\n    MINUS\n            ((SELECT\n                *\n            FROM\n                Table_2)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_3)))\nMINUS\n    (SELECT\n        *\n    FROM\n        Table_4);"
).

%%------------------------------------------------------------------------------
%% PLSQL 03 - DELETE.
%%------------------------------------------------------------------------------

-define(
  PLSQL_03,
  "\nbegin\ndelete from table_1 return column_3,column_4 into column_5,column_6;\ndelete from table_1 where column_1=value_1 and column_2 = value_2;\ndelete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4 into column_5,column_6;\ndelete from table_1 where column_1=value_1 and column_2 = value_2 return column_3,column_4,column_5,column_6 into column_7,column_8,column_9,column_10;\nend;"
).
-define(
  PLSQL_03_RESULT_DEFAULT,
  "BEGIN\n    DELETE FROM\n        Table_1\n    RETURN\n        Column_3, Column_4\n    INTO\n        Column_5, Column_6;\n    DELETE FROM\n        Table_1\n    WHERE\n        Column_1 = Value_1\n        AND Column_2 = Value_2;\n    DELETE FROM\n        Table_1\n    WHERE\n        Column_1 = Value_1\n        AND Column_2 = Value_2\n    RETURN\n        Column_3, Column_4\n    INTO\n        Column_5, Column_6;\n    DELETE FROM\n        Table_1\n    WHERE\n        Column_1 = Value_1\n        AND Column_2 = Value_2\n    RETURN\n        Column_3, Column_4, Column_5, Column_6\n    INTO\n        Column_7, Column_8, Column_9, Column_10;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 05 - SELECT.
%%------------------------------------------------------------------------------

-define(
  PLSQL_05,
  "\nbegin\nselect * from dual,dual alias_1,:param_2,:param_3 alias_3;\nselect column_1 from (select column_2 from (select column_3 from dual));\nselect * from (select column_1 from table_1),(select column_2 from table_2) alias_2,(select column_3 from table_3);\nselect * from :param_1 alias_1 join :param_2 alias_2 on column_1 = column_2;\nend;"
).
-define(
  PLSQL_05_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        *\n    FROM\n        Dual,\n        Dual Alias_1,\n        :param_2,\n        :param_3 Alias_3;\n    SELECT\n        Column_1\n    FROM\n        (SELECT\n            Column_2\n        FROM\n            (SELECT\n                Column_3\n            FROM\n                Dual));\n    SELECT\n        *\n    FROM\n        (SELECT\n            Column_1\n        FROM\n            Table_1),\n        (SELECT\n            Column_2\n        FROM\n            Table_2) Alias_2,\n        (SELECT\n            Column_3\n        FROM\n            Table_3);\n    SELECT\n        *\n    FROM\n        :param_1 Alias_1\n        JOIN\n        :param_2 Alias_2\n        ON Column_1 = Column_2;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 07 - GROUP BY.
%%------------------------------------------------------------------------------

-define(
  PLSQL_07,
  "\nbegin\nselect * from dual group by column_1,table_1.column_1,schema_1.table_1.column_1,column_1|:x:y|,table_1.column_1|:x:y|,schema_1.table_1.column_1|:x:y|;\nselect * from dual group by function_1(param_1,param_2),package_1.function_1(param_1,param_2),schema_1.package_1.function_1(param_1,param_2),function_1(param_1,param_2)|:b[f(p:q)]|,package_1.function_1(param_1,param_2)|:b[f(p:q)]|,schema_1.package_1.function_1(param_1,param_2)|:b[f(p:q)]|;\nselect * from dual group by decode,decode(param_1,param_2),decode(*),decode(distinct column_1),decode(all 6);\nselect * from dual group by function_1(param_11,function_21(param_21,function_31(param_31,param_32,param_33),param_23),param_13);\nend;"
).
-define(
  PLSQL_07_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        *\n    FROM\n        Dual\n    GROUP BY\n        Column_1, Table_1.Column_1, Schema_1.Table_1.Column_1, Column_1|:x:y|,\n        Table_1.Column_1|:x:y|, Schema_1.Table_1.Column_1|:x:y|;\n    SELECT\n        *\n    FROM\n        Dual\n    GROUP BY\n        Function_1(Param_1, Param_2), Package_1.Function_1(Param_1, Param_2),\n        Schema_1.Package_1.Function_1(Param_1, Param_2), Function_1(Param_1,\n        Param_2)|:b[f(p:q)]|, Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|\n        , Schema_1.Package_1.Function_1(Param_1, Param_2)|:b[f(p:q)]|;\n    SELECT\n        *\n    FROM\n        Dual\n    GROUP BY\n        DECODE(), DECODE(Param_1, Param_2), DECODE(*), DECODE(DISTINCT Column_1)\n        , DECODE(ALL 6);\n    SELECT\n        *\n    FROM\n        Dual\n    GROUP BY\n        Function_1(Param_11, Function_21(Param_21, Function_31(Param_31,\n        Param_32, Param_33), Param_23), Param_13);\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 08 - HAVING.
%%------------------------------------------------------------------------------

-define(
  PLSQL_08,
  "\nbegin\nselect * from dual having column_1 = column_2;\nselect * from dual having column_1 = column_2 and column_3 = column_4;\nend;"
).
-define(
  PLSQL_08_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        *\n    FROM\n        Dual\n    HAVING\n        Column_1 = Column_2;\n    SELECT\n        *\n    FROM\n        Dual\n    HAVING\n        Column_1 = Column_2\n        AND Column_3 = Column_4;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 09 - INSERT.
%%------------------------------------------------------------------------------

-define(
  PLSQL_09,
  "\nbegin\ninsert into table_1 (column_1,column_2) select column_1,column_2 from table_2 where column_3 = column_4 and column_5 = column_6 returning column_1,column_2 into :a,:b;\ninsert into table_1 (column_1,column_2,column_3,column_4) values (value_1,value_2,value_3,value_4) returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;\ninsert into table_1 values (value_1,value_2,value_3,value_4) returning column_1,column_2,column_3,column_4 into :a,:b,:c,:d;\nend;"
).
-define(
  PLSQL_09_RESULT_DEFAULT,
  "BEGIN\n    INSERT INTO\n        Table_1 (\n            Column_1, Column_2)\n        SELECT\n            Column_1, Column_2\n        FROM\n            Table_2\n        WHERE\n            Column_3 = Column_4\n            AND Column_5 = Column_6\n    RETURNING\n        Column_1, Column_2\n    INTO\n        :a, :b;\n    INSERT INTO\n        Table_1 (\n            Column_1, Column_2, Column_3, Column_4)\n    VALUES\n        (Value_1, Value_2, Value_3, Value_4)\n    RETURNING\n        Column_1, Column_2, Column_3, Column_4\n    INTO\n        :a, :b, :c, :d;\n    INSERT INTO\n        Table_1\n    VALUES\n        (Value_1, Value_2, Value_3, Value_4)\n    RETURNING\n        Column_1, Column_2, Column_3, Column_4\n    INTO\n        :a, :b, :c, :d;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 10 - JOIN.
%%------------------------------------------------------------------------------

-define(
  PLSQL_10,
  "\nbegin\nselect * from table_1 inner join table_2 using (column_1,column_2,column_3);\nselect * from table_1 cross join table_2;\nselect * from table_1 inner join table_2 using (column_1,column_2,column_3,column_4);\nselect * from table_1 inner join table_2 on table_1.column_1 = table_2.column_2 or table_1.column_3 = table_2.column_4;\nselect * from table_1 left outer join table_2 on column_1 <> column_2;\nselect * from table_1 partition by column_1 natural left outer join table_2;\nselect * from table_1 partition by column_1 natural left outer join table_2 partition by column_2 on column_1 = column_2;\nselect * from table_1 natural join table_2,table_3,table_5,(select * from dual) alias_1,table_6 join table_7 using (column_1) left outer join table_8;\nselect * from :param_1\"@link_1\" alias_1 join :param_1\"@link_1\" alias_1 on column_1 = column_2;\nend;"
).
-define(
  PLSQL_10_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        *\n    FROM\n        Table_1\n        INNER JOIN\n        Table_2\n        USING (Column_1, Column_2, Column_3);\n    SELECT\n        *\n    FROM\n        Table_1\n        CROSS JOIN\n        Table_2;\n    SELECT\n        *\n    FROM\n        Table_1\n        INNER JOIN\n        Table_2\n        USING (Column_1, Column_2, Column_3, Column_4);\n    SELECT\n        *\n    FROM\n        Table_1\n        INNER JOIN\n        Table_2\n        ON Table_1.Column_1 = Table_2.Column_2\n        OR Table_1.Column_3 = Table_2.Column_4;\n    SELECT\n        *\n    FROM\n        Table_1\n        LEFT OUTER JOIN\n        Table_2\n        ON Column_1 <> Column_2;\n    SELECT\n        *\n    FROM\n        Table_1\n        PARTITION BY (Column_1)\n        NATURAL LEFT OUTER JOIN\n        Table_2;\n    SELECT\n        *\n    FROM\n        Table_1\n        PARTITION BY (Column_1)\n        NATURAL LEFT OUTER JOIN\n        Table_2\n        PARTITION BY (Column_2)\n        ON Column_1 = Column_2;\n    SELECT\n        *\n    FROM\n        Table_1\n        NATURAL JOIN\n        Table_2,\n        Table_3,\n        Table_5,\n        (SELECT\n            *\n        FROM\n            Dual) Alias_1,\n        Table_6\n        JOIN\n        Table_7\n        USING (Column_1)\n        LEFT OUTER JOIN\n        Table_8;\n    SELECT\n        *\n    FROM\n        :param_1\"@link_1\" Alias_1\n        JOIN\n        :param_1\"@link_1\" Alias_1\n        ON Column_1 = Column_2;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 11 - MULTIPLE.
%%------------------------------------------------------------------------------

-define(
  PLSQL_11,
  "\nbegin\ninsert into hr_regions (region_id,region_name) values (1,'europe');\ninsert into hr_regions (region_id,region_name) values (2,'americas');\ninsert into hr_regions (region_id,region_name) values (3,'asia');\ninsert into hr_regions (region_id,region_name) values (4,'middle east and africa');\ninsert into hr_regions (region_id,region_name) values (1,'europe');\ninsert into hr_regions (region_id,region_name) values (2,'americas');\ninsert into hr_regions (region_id,region_name) values (3,'asia');\ninsert into hr_regions (region_id,region_name) values (4,'middle east and africa');\nend;"
).
-define(
  PLSQL_11_RESULT_DEFAULT,
  "BEGIN\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (1, 'europe');\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (2, 'americas');\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (3, 'asia');\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (4, 'middle east and africa');\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (1, 'europe');\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (2, 'americas');\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (3, 'asia');\n    INSERT INTO\n        Hr_Regions (\n            Region_Id, Region_Name)\n    VALUES\n        (4, 'middle east and africa');\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 12 - ORDER BY.
%%------------------------------------------------------------------------------

-define(
  PLSQL_12,
  "\nbegin\nselect * from dual order by column_1, column_2;\nselect * from dual order by column_1,column_2,column_3,column_4;\nend;"
).
-define(
  PLSQL_12_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        *\n    FROM\n        Dual\n    ORDER BY\n        Column_1, Column_2;\n    SELECT\n        *\n    FROM\n        Dual\n    ORDER BY\n        Column_1, Column_2, Column_3, Column_4;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 14 - SELECT.
%%------------------------------------------------------------------------------

-define(
  PLSQL_14,
  "\nbegin\nselect (select * from table_1),column_1,(select * from table_2) from dual;\nselect column_1 alias_1,(select * from table_1) alias_2,column_3 alias_3 from dual;\nselect distinct column_1,column_2 from dual;\nselect sum(column_1) / sum(column_2) avg_charge from dual;\nselect avg(sum(min(1))) from dual;\nselect column_1,(select * from table_1,table_2 natural join table_3,table_4) column_2,column_3 from dual;\nselect * into column_1,column_2,column_3,column_4 from dual;\nselect\ncase column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end from dual;\nselect column_11, case column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end as column_12, column_13 from dual;\nselect (+ column_2)|:b| from table_1;\nend;"
).
-define(
  PLSQL_14_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        (SELECT\n            *\n        FROM\n            Table_1), Column_1, "
  ++
  "\n        (SELECT\n            *\n        FROM\n            Table_2)\n    FROM\n        Dual;\n    SELECT\n        Column_1 Alias_1, "
  ++
  "\n        (SELECT\n            *\n        FROM\n            Table_1) Alias_2, Column_3 Alias_3\n    FROM\n        Dual;\n    SELECT DISTINCT\n        Column_1, Column_2\n    FROM\n        Dual;\n    SELECT\n        SUM(Column_1) / SUM(Column_2) Avg_Charge\n    FROM\n        Dual;\n    SELECT\n        AVG(SUM(MIN(1)))\n    FROM\n        Dual;\n    SELECT\n        Column_1, "
  ++
  "\n        (SELECT\n            *\n        FROM\n            Table_1,\n            Table_2\n            NATURAL JOIN\n            Table_3,\n            Table_4) Column_2, Column_3\n    FROM\n        Dual;\n    SELECT\n        *\n    INTO\n        Column_1, Column_2, Column_3, Column_4\n    FROM\n        Dual;\n    SELECT\n        CASE Column_0\n            WHEN Column_1\n            OR Column_2\n            AND Column_3\n            THEN Column_4\n            ELSE Column_5\n        END\n    FROM\n        Dual;\n    SELECT\n        Column_11, "
  ++
  "\n        CASE Column_0\n            WHEN Column_1\n            OR Column_2\n            AND Column_3\n            THEN Column_4\n            ELSE Column_5\n        END Column_12, Column_13\n    FROM\n        Dual;\n    SELECT\n        (+ Column_2)|:b|\n    FROM\n        Table_1;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 15 - STRUCTURE.
%%------------------------------------------------------------------------------

-define(
  PLSQL_15,
  "\nbegin\nselect (select * from table_1) from dual;\nselect (select (select * from table_2) from table_1) from dual;\nselect (select (select (select * from table_2) from table_2) from table_1) from dual;\nend;"
).
-define(
  PLSQL_15_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        (SELECT\n            *\n        FROM\n            Table_1)\n    FROM\n        Dual;\n    SELECT\n        (SELECT\n            (SELECT\n                *\n            FROM\n                Table_2)\n        FROM\n            Table_1)\n    FROM\n        Dual;\n    SELECT\n        (SELECT\n            (SELECT\n                (SELECT\n                    *\n                FROM\n                    Table_2)\n            FROM\n                Table_2)\n        FROM\n            Table_1)\n    FROM\n        Dual;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 16 - UNION.
%%------------------------------------------------------------------------------

-define(
  PLSQL_16,
  "\nbegin\nselect * from table_1 union select * from table_2;\nselect * from (select * from table_11 intersect select * from table_12) union all select * from (select * from table_21 minus select * from table_22);\nselect * from (select * from table_11 intersect select * from table_12) union all select * from (select * from table_21 minus select * from (select * from table_31 union select * from table_32));\nselect column_1,(select * from dual) union (select * from dual) from dual;\nselect column_1,(select * from dual) union (select * from dual),column_2 from dual;\nselect column_1,(select * from dual) union (select * from dual) as column_2,column_3 from dual;\nend;"
).
-define(
  PLSQL_16_RESULT_DEFAULT,
  "BEGIN\n        ((SELECT\n            *\n        FROM\n            Table_1)\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_2));\n        ((SELECT\n            *\n        FROM\n                ((SELECT\n                    *\n                FROM\n                    Table_11)\n            INTERSECT\n                (SELECT\n                    *\n                FROM\n                    Table_12)))\n    UNION ALL\n        (SELECT\n            *\n        FROM\n                ((SELECT\n                    *\n                FROM\n                    Table_21)\n            MINUS\n                (SELECT\n                    *\n                FROM\n                    Table_22))));\n        ((SELECT\n            *\n        FROM\n                ((SELECT\n                    *\n                FROM\n                    Table_11)\n            INTERSECT\n                (SELECT\n                    *\n                FROM\n                    Table_12)))\n    UNION ALL\n        (SELECT\n            *\n        FROM\n                ((SELECT\n                    *\n                FROM\n                    Table_21)\n            MINUS\n                (SELECT\n                    *\n                FROM\n                        ((SELECT\n                            *\n                        FROM\n                            Table_31)\n                    UNION\n                        (SELECT\n                            *\n                        FROM\n                            Table_32))))));\n    SELECT\n        Column_1, "
  ++
  "\n            ((SELECT\n                *\n            FROM\n                Dual)\n        UNION\n            (SELECT\n                *\n            FROM\n                Dual))\n    FROM\n        Dual;\n    SELECT\n        Column_1, "
  ++
  "\n            ((SELECT\n                *\n            FROM\n                Dual)\n        UNION\n            (SELECT\n                *\n            FROM\n                Dual)), Column_2\n    FROM\n        Dual;\n    SELECT\n        Column_1, "
  ++
  "\n            ((SELECT\n                *\n            FROM\n                Dual)\n        UNION\n            (SELECT\n                *\n            FROM\n                Dual)) Column_2, Column_3\n    FROM\n        Dual;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 17 - WHERE.
%%------------------------------------------------------------------------------

-define(
  PLSQL_17,
  "\nbegin\nselect * from dual where column_1 = column_2;\nselect * from dual\nwhere column_11 <> column_12\nand column_21 != column_22\nor column_31 <> column_32\nand column_41 != column_42;\nselect * from dual where (select column_31 from table_31) <> column_14;\nselect * from dual where column_1 = column_2 and not column_3 = column_4;\nselect * from dual where column_1 > all (select * from table_1);\nselect * from dual start with column_1 is null and column_2 < 0 connect by nocycle column_2 = column_3 or column_4 <> column_5;\nend;"
).
-define(
  PLSQL_17_RESULT_DEFAULT,
  "BEGIN\n    SELECT\n        *\n    FROM\n        Dual\n    WHERE\n        Column_1 = Column_2;\n    SELECT\n        *\n    FROM\n        Dual\n    WHERE\n        Column_11 <> Column_12\n        AND Column_21 != Column_22\n        OR Column_31 <> Column_32\n        AND Column_41 != Column_42;\n    SELECT\n        *\n    FROM\n        Dual\n    WHERE\n        (SELECT\n            Column_31\n        FROM\n            Table_31) <> Column_14;\n    SELECT\n        *\n    FROM\n        Dual\n    WHERE\n        Column_1 = Column_2\n        AND NOT (Column_3 = Column_4);\n    SELECT\n        *\n    FROM\n        Dual\n    WHERE\n        Column_1 > ALL\n        (SELECT\n            *\n        FROM\n            Table_1);\n    SELECT\n        *\n    FROM\n        Dual\n    START WITH\n        Column_1 IS NULL\n        AND Column_2 < 0\n    CONNECT BY NOCYCLE\n        Column_2 = Column_3\n        OR Column_4 <> Column_5;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 18 - simple.
%%------------------------------------------------------------------------------

-define(PLSQL_18, "\nbegin schm.proc(:p_first,:p_second,:p_result); end;").
-define(PLSQL_18_RESULT_DEFAULT, "BEGIN\n    Schm.Proc(:p_first, :p_second, :p_result);\nEND;").

%%------------------------------------------------------------------------------
%% PLSQL 19 - CALL.
%%------------------------------------------------------------------------------

-define(PLSQL_19, "\nbegin call proc(:p_first,:p_second,:p_result); end;").
-define(
  PLSQL_19_RESULT_DEFAULT,
  "BEGIN\n    CALL\n        Proc(:p_first, :p_second, :p_result);\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 20 - simple.
%%------------------------------------------------------------------------------

-define(
  PLSQL_20,
  "\nbegin schm.proc(:p_first,:p_second,:p_result); dbms_output.put_line('Goodbye cruel World!'); end;"
).
-define(
  PLSQL_20_RESULT_DEFAULT,
  "BEGIN\n    Schm.Proc(:p_first, :p_second, :p_result);\n    Dbms_Output.Put_Line('Goodbye cruel World!');\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 22 - TRANSACTION.
%%------------------------------------------------------------------------------

-define(PLSQL_22, "\nbegin\ncommit;\ncommit work;\nrollback;\nrollback work;\nend;").
-define(
  PLSQL_22_RESULT_DEFAULT,
  "BEGIN\n    COMMIT;\n    COMMIT WORK;\n    ROLLBACK;\n    ROLLBACK WORK;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 24 - UPDATE.
%%------------------------------------------------------------------------------

-define(
  PLSQL_24,
  "\nbegin\nupdate name_table set name_column_1 = :value_1;\nupdate name_table set name_column_1 = :value_1, name_column_2 = :value_2;\nupdate name_table set name_column_1 = :value_1, name_column_2 = :value_2 where company_id = :id1 and employee_id = :id2;\nupdate employees set salary = :sal where employee_id = :id;\nupdate employees set salary = :sal where employee_id = :id returning c,d into :c, :d;\nupdate employees set salary = :sal where company_id = :id1 and employee_id2 = :id returning lob_column into :out_locator;\nend;"
).
-define(
  PLSQL_24_RESULT_DEFAULT,
  "BEGIN\n    UPDATE\n        Name_Table\n    SET\n        Name_Column_1 = :value_1;\n    UPDATE\n        Name_Table\n    SET\n        Name_Column_1 = :value_1,\n        Name_Column_2 = :value_2;\n    UPDATE\n        Name_Table\n    SET\n        Name_Column_1 = :value_1,\n        Name_Column_2 = :value_2\n    WHERE\n        Company_Id = :id1\n        AND Employee_Id = :id2;\n    UPDATE\n        Employees\n    SET\n        Salary = :sal\n    WHERE\n        Employee_Id = :id;\n    UPDATE\n        Employees\n    SET\n        Salary = :sal\n    WHERE\n        Employee_Id = :id\n    RETURNING\n        C, D\n    INTO\n        :c, :d;\n    UPDATE\n        Employees\n    SET\n        Salary = :sal\n    WHERE\n        Company_Id = :id1\n        AND Employee_Id2 = :id\n    RETURNING\n        Lob_Column\n    INTO\n        :out_locator;\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 26 - CALL.
%%------------------------------------------------------------------------------

-define(
  PLSQL_26,
  "\nbegin Call function_1\n(select column_1 from table_1 union select column_2 from table_2); end;"
).
-define(
  PLSQL_26_RESULT_DEFAULT,
  "BEGIN\n    CALL\n        Function_1(\n                ((SELECT\n                    Column_1\n                FROM\n                    Table_1)\n            UNION\n                (SELECT\n                    Column_2\n                FROM\n                    Table_2)));\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 27 - CALL.
%%------------------------------------------------------------------------------

-define(
  PLSQL_27,
  "\nbegin Call function_1\n(parameter_1,(select column_1 from table_1 union select column_2 from table_2),parameter_3); end;"
).
-define(
  PLSQL_27_RESULT_DEFAULT,
  "BEGIN\n    CALL\n        Function_1(Parameter_1, "
  ++
  "\n                ((SELECT\n                    Column_1\n                FROM\n                    Table_1)\n            UNION\n                (SELECT\n                    Column_2\n                FROM\n                    Table_2)), Parameter_3);\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 28 - CALL.
%%------------------------------------------------------------------------------

-define(
  PLSQL_28,
  "\nbegin call i1ident_8 ((select * from table_1),(select * from table_2),(select * from table_3));end;"
).
-define(
  PLSQL_28_RESULT_DEFAULT,
  "BEGIN\n    CALL\n        I1ident_8(\n            (SELECT\n                *\n            FROM\n                Table_1), "
  ++
  "\n            (SELECT\n                *\n            FROM\n                Table_2), "
  ++
  "\n            (SELECT\n                *\n            FROM\n                Table_3));\nEND;"
).

%%------------------------------------------------------------------------------
%% PLSQL 29 - CALL.
%%------------------------------------------------------------------------------

-define(
  PLSQL_29,
  "\nbegin i1ident_5.m@oney~~$tree_ident ((select * from table_1),(select * from table_2) minus (select * from table_3),(select * from table_4),6.34e8f);end;"
).
-define(
  PLSQL_29_RESULT_DEFAULT,
  "BEGIN\n    I1ident_5.M@oney~~$tree_Ident(\n        (SELECT\n            *\n        FROM\n            Table_1), "
  ++
  "\n            ((SELECT\n                *\n            FROM\n                Table_2)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_3)), "
  ++
  "\n        (SELECT\n            *\n        FROM\n            Table_4), 6.34e8f);\nEND;"
).

%%------------------------------------------------------------------------------
%% UNION 25 - SELECT FROM.
%%------------------------------------------------------------------------------

-define(PROBLEM_01, "\nselect *\nfrom (select * from table_1 union select * from table_2);").

% line 04
-define(
  PROBLEM_01_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n            ((SELECT\n            *\n        FROM\n            Table_1)\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_2));"
).

%%------------------------------------------------------------------------------
%% UNION 28 - UNION FROM.
%%------------------------------------------------------------------------------

-define(
  PROBLEM_02,
  "\nselect *\nfrom ((select * from table_11 intersect select * from table_12) union (select * from table_21 minus select * from table_22));"
).

% line 04
-define(
  PROBLEM_02_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n                (((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12))\n    UNION\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_22)));"
).

%%------------------------------------------------------------------------------
%% UNION 31 - SELECT & UNION FROM.
%%------------------------------------------------------------------------------

-define(
  PROBLEM_03,
  "\nselect *\nfrom (select * from table_1 union (select * from table_21 minus select * from table_22));"
).

% line 04
-define(
  PROBLEM_03_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n            ((SELECT\n            *\n        FROM\n            Table_1)\n    UNION\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_22)));"
).

%%------------------------------------------------------------------------------
%% UNION 34 - UNION & SELECT FROM.
%%------------------------------------------------------------------------------

-define(
  PROBLEM_04,
  "\nselect *\nfrom ((select * from table_11 intersect select * from table_12) union select * from table_2);"
).

% line 04
-define(
  PROBLEM_04_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n                (((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12))\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_2));"
).

%%------------------------------------------------------------------------------
%% UNION 36 - MINUS & INTERSECT.
%%------------------------------------------------------------------------------

-define(
  PROBLEM_05,
  "\n(select * from table_1)|:_a::b::c|\nMinus ((select * from table_2) Intersect (select * from table_3));"
).

% first SELECT
-define(
  PROBLEM_05_RESULT_DEFAULT,
  "((SELECT\n    *\nFROM\n    Table_1))|:_a::b::c|\nMINUS\n        ((SELECT\n            *\n        FROM\n            Table_2)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Table_3));"
).

%%------------------------------------------------------------------------------
%% UNION 37 - INTERSECT & MINUS.
%%------------------------------------------------------------------------------

% lasr SELECT
-define(
  PROBLEM_06,
  "\n((select * from table_2)\nIntersect (select * from table_3)) Minus (select * from table_1)|:_a::b::c|;"
).
-define(
  PROBLEM_06_RESULT_DEFAULT,
  "        ((SELECT\n            *\n        FROM\n            Table_2)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Table_3))\nMINUS\n((SELECT\n    *\nFROM\n    Table_1))|:_a::b::c|;"
).

%%------------------------------------------------------------------------------
%% REVOKE 01 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_01, "\nrevoke create table from user_1;").
-define(REVOKE_01_RESULT_DEFAULT, "REVOKE\n    CREATE TABLE\nFROM\n    User_1;").

%%------------------------------------------------------------------------------
%% REVOKE 02 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_02, "\nrevoke drop any table from user_1, user_2;").
-define(REVOKE_02_RESULT_DEFAULT, "REVOKE\n    DROP ANY TABLE\nFROM\n    User_1, User_2;").

%%------------------------------------------------------------------------------
%% REVOKE 03 - FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_03, "\nrevoke all on table_1 from user_1 cascade constraints;").
-define(
  REVOKE_03_RESULT_DEFAULT,
  "REVOKE\n    ALL\nON\n    Table_1\nFROM\n    User_1\nCASCADE CONSTRAINTS;"
).

%%------------------------------------------------------------------------------
%% REVOKE 04 - FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_04, "\nrevoke update on table_1 from user_1, user_2 force;").
-define(
  REVOKE_04_RESULT_DEFAULT,
  "REVOKE\n    UPDATE\nON\n    Table_1\nFROM\n    User_1, User_2\nFORCE;"
).

%%------------------------------------------------------------------------------
%% REVOKE 05 - ON & FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_05, "\nrevoke select on table_1 from user_1 force;").
-define(REVOKE_05_RESULT_DEFAULT, "REVOKE\n    SELECT\nON\n    Table_1\nFROM\n    User_1\nFORCE;").

%%------------------------------------------------------------------------------
%% REVOKE 06 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_06, "\nrevoke all on table_1 from user_1;").
-define(REVOKE_06_RESULT_DEFAULT, "REVOKE\n    ALL\nON\n    Table_1\nFROM\n    User_1;").

%%------------------------------------------------------------------------------
%% REVOKE 07 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_07, "\nrevoke delete, insert, select, update on table_1 from user_1;").
-define(
  REVOKE_07_RESULT_DEFAULT,
  "REVOKE\n    DELETE, INSERT, SELECT, UPDATE\nON\n    Table_1\nFROM\n    User_1;"
).

%%------------------------------------------------------------------------------
%% REVOKE 08 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_08, "\nrevoke role_1 from user_1;").
-define(REVOKE_08_RESULT_DEFAULT, "REVOKE\n    Role_1\nFROM\n    User_1;").

%%------------------------------------------------------------------------------
%% REVOKE 09 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_09, "\nrevoke privilege_1, privilege_2 from user_1;").
-define(REVOKE_09_RESULT_DEFAULT, "REVOKE\n    Privilege_1, Privilege_2\nFROM\n    User_1;").

%%------------------------------------------------------------------------------
%% REVOKE 10 - FROM.
%%------------------------------------------------------------------------------

-define(
  REVOKE_10,
  "\nrevoke privilege_1, privilege_2, privilege_3, privilege_4, privilege_5 from user_1;"
).
-define(
  REVOKE_10_RESULT_DEFAULT,
  "REVOKE\n    Privilege_1, Privilege_2, Privilege_3, Privilege_4, Privilege_5\nFROM\n    User_1;"
).

%%------------------------------------------------------------------------------
%% REVOKE 11 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_11, "\nrevoke select on ddtable from user_1;").
-define(REVOKE_11_RESULT_DEFAULT, "REVOKE\n    SELECT\nON\n    Ddtable\nFROM\n    User_1;").

%%------------------------------------------------------------------------------
%% REVOKE 12 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_12, "\nrevoke select on schema1.ddtable from user_1;").
-define(REVOKE_12_RESULT_DEFAULT, "REVOKE\n    SELECT\nON\n    Schema1.Ddtable\nFROM\n    User_1;").

%%------------------------------------------------------------------------------
%% REVOKE 13 - ON & FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_13, "\nrevoke all privileges on schema1.ddtable from role_2;").
-define(
  REVOKE_13_RESULT_DEFAULT,
  "REVOKE\n    ALL PRIVILEGES\nON\n    Schema1.Ddtable\nFROM\n    Role_2;"
).

%%------------------------------------------------------------------------------
%% REVOKE 14 - FROM & addon.
%%------------------------------------------------------------------------------

-define(REVOKE_14, "\nrevoke manage_system from test_user_1;").
-define(REVOKE_14_RESULT_DEFAULT, "REVOKE\n    Manage_System\nFROM\n    Test_User_1;").

%%------------------------------------------------------------------------------
%% REVOKE 15 - FROM.
%%------------------------------------------------------------------------------

-define(REVOKE_15, "\nrevoke create table from user_1,user_2,user_3,user_4;").
-define(
  REVOKE_15_RESULT_DEFAULT,
  "REVOKE\n    CREATE TABLE\nFROM\n    User_1, User_2, User_3, User_4;"
).

%%------------------------------------------------------------------------------
%% ROLE 01 - CREATE.
%%------------------------------------------------------------------------------

-define(ROLE_01, "\ncreate role role_1;").
-define(ROLE_01_RESULT_DEFAULT, "CREATE ROLE\n    Role_1;").

%%------------------------------------------------------------------------------
%% ROLE 02 - DROP.
%%------------------------------------------------------------------------------

-define(ROLE_02, "\ndrop role role_1;").
-define(ROLE_02_RESULT_DEFAULT, "DROP ROLE\n    Role_1;").

%%------------------------------------------------------------------------------
%% SELECT 01 - very simple.
%%------------------------------------------------------------------------------

-define(SELECT_01, "\nselect *\nfrom dual;").
-define(SELECT_01_RESULT_DEFAULT, "SELECT\n    *\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 02 - column and param.
%%------------------------------------------------------------------------------

-define(SELECT_02, "\nselect column_1, :param_1\nfrom dual;").
-define(SELECT_02_RESULT_DEFAULT, "SELECT\n    Column_1, :param_1\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 03 - subquery.
%%------------------------------------------------------------------------------

-define(SELECT_03, "\nselect (select * from table_1)\nfrom dual;").
-define(
  SELECT_03_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        *\n    FROM\n        Table_1)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 04 - columns.
%%------------------------------------------------------------------------------

-define(SELECT_04, "\nselect column_1,column_2\nfrom dual;").
-define(SELECT_04_RESULT_DEFAULT, "SELECT\n    Column_1, Column_2\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 05 - subqueries.
%%------------------------------------------------------------------------------

-define(SELECT_05, "\nselect (select * from table_1),(select * from table_2)\nfrom dual;").
-define(
  SELECT_05_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        *\n    FROM\n        Table_1), "
  ++
  "\n    (SELECT\n        *\n    FROM\n        Table_2)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 06 - column, subquery, column.
%%------------------------------------------------------------------------------

-define(SELECT_06, "\nselect column_1,(select * from table_1),column_2\nfrom dual;").
-define(
  SELECT_06_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n    (SELECT\n        *\n    FROM\n        Table_1), Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 07 - subquery, column, subquery.
%%------------------------------------------------------------------------------

-define(SELECT_07, "\nselect (select * from table_1),column_1,(select * from table_2)\nfrom dual;").
-define(
  SELECT_07_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        *\n    FROM\n        Table_1), Column_1, "
  ++
  "\n    (SELECT\n        *\n    FROM\n        Table_2)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 08 - ALIAS column: column & param.
%%------------------------------------------------------------------------------

-define(SELECT_08, "\nselect column_1 alias_1,:param_2 alias_2\nfrom dual;").
-define(SELECT_08_RESULT_DEFAULT, "SELECT\n    Column_1 Alias_1, :param_2 Alias_2\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 09 - ALIAS column: subquery.
%%------------------------------------------------------------------------------

-define(SELECT_09, "\nselect (select * from table_1) alias_1\nfrom dual;").
-define(
  SELECT_09_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        *\n    FROM\n        Table_1) Alias_1\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 10 - ALIAS column: column & subquery.
%%------------------------------------------------------------------------------

-define(
  SELECT_10,
  "\nselect column_1 alias_1,(select * from table_1) alias_2,column_3 alias_3\nfrom dual;"
).
-define(
  SELECT_10_RESULT_DEFAULT,
  "SELECT\n    Column_1 Alias_1, "
  ++
  "\n    (SELECT\n        *\n    FROM\n        Table_1) Alias_2, Column_3 Alias_3\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 11 - ALL.
%%------------------------------------------------------------------------------

-define(SELECT_11, "\nselect all column_1,column_2\nfrom dual;").
-define(SELECT_11_RESULT_DEFAULT, "SELECT ALL\n    Column_1, Column_2\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 12 - DISTINCT.
%%------------------------------------------------------------------------------

-define(SELECT_12, "\nselect distinct column_1,column_2\nfrom dual;").
-define(SELECT_12_RESULT_DEFAULT, "SELECT DISTINCT\n    Column_1, Column_2\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 13 - FUN OP FUN.
%%------------------------------------------------------------------------------

-define(SELECT_13, "\nselect sum(column_1) / sum(column_2) avg_charge\nfrom dual;").
-define(
  SELECT_13_RESULT_DEFAULT,
  "SELECT\n    SUM(Column_1) / SUM(Column_2) Avg_Charge\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 14 - FUN complex.
%%------------------------------------------------------------------------------

-define(SELECT_14, "\nselect sum(sum(1))\nfrom dual;").
-define(SELECT_14_RESULT_DEFAULT, "SELECT\n    SUM(SUM(1))\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 15 - FUN simple.
%%------------------------------------------------------------------------------

-define(SELECT_15, "\nselect sum(1,2,3)\nfrom dual;").
-define(SELECT_15_RESULT_DEFAULT, "SELECT\n    SUM(1, 2, 3)\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 16 - FUN complex.
%%------------------------------------------------------------------------------

-define(SELECT_16, "\nselect sum(1,2,3,4)\nfrom dual;").
-define(SELECT_16_RESULT_DEFAULT, "SELECT\n    SUM(1, 2, 3, 4)\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 17 - FUN mixed.
%%------------------------------------------------------------------------------

-define(SELECT_17, "\nselect avg(sum(min(1)))\nfrom dual;").
-define(SELECT_17_RESULT_DEFAULT, "SELECT\n    AVG(SUM(MIN(1)))\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 18 - HINTS.
%%------------------------------------------------------------------------------

-define(SELECT_18, "\nselect /* hint */ *\nfrom dual;").
-define(SELECT_18_RESULT_DEFAULT, "SELECT /* hint */\n    *\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 19 - JOIN column.
%%------------------------------------------------------------------------------

-define(
  SELECT_19,
  "\nselect column_1,(select * from table_1,table_2 natural join table_3,table_4) column_2,column_3\nfrom dual;"
).
-define(
  SELECT_19_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n    (SELECT\n        *\n    FROM\n        Table_1,\n        Table_2\n        NATURAL JOIN\n        Table_3,\n        Table_4) Column_2, Column_3\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 20 - INTO.
%%------------------------------------------------------------------------------

-define(SELECT_20, "\nselect * into column_1\nfrom dual;").
-define(SELECT_20_RESULT_DEFAULT, "SELECT\n    *\nINTO\n    Column_1\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 21 - INTO.
%%------------------------------------------------------------------------------

-define(SELECT_21, "\nselect * into column_1,column_2,column_3\nfrom dual;").
-define(
  SELECT_21_RESULT_DEFAULT,
  "SELECT\n    *\nINTO\n    Column_1, Column_2, Column_3\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 22 - INTO.
%%------------------------------------------------------------------------------

-define(SELECT_22, "\nselect * into column_1,column_2,column_3,column_4\nfrom dual;").
-define(
  SELECT_22_RESULT_DEFAULT,
  "SELECT\n    *\nINTO\n    Column_1, Column_2, Column_3, Column_4\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 23 - COLUMNS.
%%------------------------------------------------------------------------------

-define(SELECT_23, "\nselect column_1,column_2,column_3\nfrom dual;").
-define(SELECT_23_RESULT_DEFAULT, "SELECT\n    Column_1, Column_2, Column_3\nFROM\n    Dual;").

%%------------------------------------------------------------------------------
%% SELECT 24 - COLUMNS.
%%------------------------------------------------------------------------------

-define(SELECT_24, "\nselect column_1,column_2,column_3,column_4\nfrom dual;").
-define(
  SELECT_24_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 25 - CASE.
%%------------------------------------------------------------------------------

-define(
  SELECT_25,
  "\nselect\ncase column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end\nfrom dual;"
).
-define(
  SELECT_25_RESULT_DEFAULT,
  "SELECT\n    CASE Column_0\n        WHEN Column_1\n        OR Column_2\n        AND Column_3\n        THEN Column_4\n        ELSE Column_5\n    END\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 26 - CASE.
%%------------------------------------------------------------------------------

-define(
  SELECT_26,
  "\nselect\ncolumn_11, case column_0 when column_1 or (column_2 and column_3) then column_4 else column_5 end as column_12, column_13\nfrom dual;"
).
-define(
  SELECT_26_RESULT_DEFAULT,
  "SELECT\n    Column_11, "
  ++
  "\n    CASE Column_0\n        WHEN Column_1\n        OR Column_2\n        AND Column_3\n        THEN Column_4\n        ELSE Column_5\n    END Column_12, Column_13\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 27 - CASE.
%%------------------------------------------------------------------------------

-define(SELECT_27, "\nselect column_1,column_2,column_3,column_4\nfrom dual;").
-define(
  SELECT_27_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% SELECT 28 - UNARY.
%%------------------------------------------------------------------------------

-define(SELECT_28, "\nselect (+ column_2)|:b| from table_1;").
-define(SELECT_28_RESULT_DEFAULT, "SELECT\n    (+ Column_2)|:b|\nFROM\n    Table_1;").

%%------------------------------------------------------------------------------
%% SELECT 29 - STRING.
%%------------------------------------------------------------------------------

-define(
  SELECT_29,
  "\nselect \"columN_1\" into \"columN_2\" from \"tablE_1\" where \"columN_3\" = \"columN_4\" group by \"columN_5\" having \"columN_6\" = \"columN_7\" order by \"columN_8\";"
).
-define(
  SELECT_29_RESULT_DEFAULT,
  "SELECT\n    \"columN_1\"\nINTO\n    \"columN_2\"\nFROM\n    \"tablE_1\"\nWHERE\n    \"columN_3\" = \"columN_4\"\nGROUP BY\n    \"columN_5\"\nHAVING\n    \"columN_6\" = \"columN_7\"\nORDER BY\n    \"columN_8\";"
).

%%------------------------------------------------------------------------------
%% SELECT 30 - STRING.
%%------------------------------------------------------------------------------

-define(
  SELECT_30,
  "\nselect \"columN_1\" from \"tablE_1\" where \"columN_3\" = \"columN_4\" having \"columN_6\" = \"columN_7\" order by \"columN_8\";"
).
-define(
  SELECT_30_RESULT_DEFAULT,
  "SELECT\n    \"columN_1\"\nFROM\n    \"tablE_1\"\nWHERE\n    \"columN_3\" = \"columN_4\"\nHAVING\n    \"columN_6\" = \"columN_7\"\nORDER BY\n    \"columN_8\";"
).

%%------------------------------------------------------------------------------
%% SELECT 31 - ANCHOR.
%%------------------------------------------------------------------------------

-define(SELECT_31, "\n(select * from table_1 group by column_1)|:_a::b::c|;").
-define(
  SELECT_31_RESULT_DEFAULT,
  "(SELECT\n    *\nFROM\n    Table_1\nGROUP BY\n    Column_1)|:_a::b::c|;"
).

%%------------------------------------------------------------------------------
%% SELECT 32 - ALIAS.
%%------------------------------------------------------------------------------

-define(
  SELECT_32,
  "\nSELECT\n    ID, cast(time_stamp as datex), LOGGER_LEVEL, TEXT, SCOPE, MODULE, ACTION,\n    USER_NAME, CLIENT_IDENTIFIER, CALL_STACK, UNIT_NAME, LINE_NO, SCN, SID,\n    CLIENT_INFO\nFROM\n    DBSSX.LOGGER_LOGS;"
).
-define(
  SELECT_32_RESULT_DEFAULT,
  "SELECT\n    Id, Cast(Time_Stamp AS Datex), Logger_Level, Text, Scope, Module, Action,\n    User_Name, Client_Identifier, Call_Stack, Unit_Name, Line_No, Scn, Sid,\n    Client_Info\nFROM\n    Dbssx.Logger_Logs;"
).

%%------------------------------------------------------------------------------
%% STRUCTURE 01 - sinple subquery.
%%------------------------------------------------------------------------------

-define(STRUCTURE_01, "\nselect (select * from table_1)\nfrom dual;").
-define(
  STRUCTURE_01_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        *\n    FROM\n        Table_1)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% STRUCTURE 02 - double subquery.
%%------------------------------------------------------------------------------

-define(STRUCTURE_02, "\nselect (select (select * from table_2) from table_1)\nfrom dual;").
-define(
  STRUCTURE_02_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        (SELECT\n            *\n        FROM\n            Table_2)\n    FROM\n        Table_1)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% STRUCTURE 03 - triple subquery.
%%------------------------------------------------------------------------------

-define(
  STRUCTURE_03,
  "\nselect (select (select (select * from table_2) from table_2) from table_1)\nfrom dual;"
).
-define(
  STRUCTURE_03_RESULT_DEFAULT,
  "SELECT\n    (SELECT\n        (SELECT\n            (SELECT\n                *\n            FROM\n                Table_2)\n        FROM\n            Table_2)\n    FROM\n        Table_1)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% TRANSACTION 01 - COMMIT.
%%------------------------------------------------------------------------------

-define(TRANSACTION_01, "\nbegin commit; end;").
-define(TRANSACTION_01_RESULT_DEFAULT, "BEGIN\n    COMMIT;\nEND;").

%%------------------------------------------------------------------------------
%% TRANSACTION 02 - COMMIT.
%%------------------------------------------------------------------------------

-define(TRANSACTION_02, "\nbegin commit work; end;").
-define(TRANSACTION_02_RESULT_DEFAULT, "BEGIN\n    COMMIT WORK;\nEND;").

%%------------------------------------------------------------------------------
%% TRANSACTION 03 - ROLLBACK.
%%------------------------------------------------------------------------------

-define(TRANSACTION_03, "\nbegin rollback; end;").
-define(TRANSACTION_03_RESULT_DEFAULT, "BEGIN\n    ROLLBACK;\nEND;").

%%------------------------------------------------------------------------------
%% TRANSACTION 04 - ROLLBACK.
%%------------------------------------------------------------------------------

-define(TRANSACTION_04, "\nbegin rollback work; end;").
-define(TRANSACTION_04_RESULT_DEFAULT, "BEGIN\n    ROLLBACK WORK;\nEND;").

%%------------------------------------------------------------------------------
%% TRUNCATE 01 - simple.
%%------------------------------------------------------------------------------

-define(TRUNCATE_01, "\ntruncate table name_schema.name_table;").
-define(TRUNCATE_01_RESULT_DEFAULT, "TRUNCATE TABLE\n    Name_Schema.Name_Table;").

%%------------------------------------------------------------------------------
%% TRUNCATE 02 - PRESERVE.
%%------------------------------------------------------------------------------

-define(TRUNCATE_02, "\ntruncate table tbl preserve materialized view log;").
-define(
  TRUNCATE_02_RESULT_DEFAULT,
  "TRUNCATE TABLE\n    Tbl\n        PRESERVE MATERIALIZED VIEW LOG;"
).

%%------------------------------------------------------------------------------
%% TRUNCATE 03 - PURGE.
%%------------------------------------------------------------------------------

-define(TRUNCATE_03, "\ntruncate table tbl purge materialized view log;").
-define(TRUNCATE_03_RESULT_DEFAULT, "TRUNCATE TABLE\n    Tbl\n        PURGE MATERIALIZED VIEW LOG;").

%%------------------------------------------------------------------------------
%% TRUNCATE 04 - DROP.
%%------------------------------------------------------------------------------

-define(TRUNCATE_04, "\ntruncate table tbl drop storage;").
-define(TRUNCATE_04_RESULT_DEFAULT, "TRUNCATE TABLE\n    Tbl\n        DROP STORAGE;").

%%------------------------------------------------------------------------------
%% TRUNCATE 05 - REUSE.
%%------------------------------------------------------------------------------

-define(TRUNCATE_05, "\ntruncate table tbl reuse storage;").
-define(TRUNCATE_05_RESULT_DEFAULT, "TRUNCATE TABLE\n    Tbl\n        REUSE STORAGE;").

%%------------------------------------------------------------------------------
%% TRUNCATE 06 - PRESEREVE & DROP.
%%------------------------------------------------------------------------------

-define(TRUNCATE_06, "\ntruncate table tbl preserve materialized view log drop storage;").
-define(
  TRUNCATE_06_RESULT_DEFAULT,
  "TRUNCATE TABLE\n    Tbl\n        PRESERVE MATERIALIZED VIEW LOG\n        DROP STORAGE;"
).

%%------------------------------------------------------------------------------
%% TRUNCATE 07 - PURGE & DROP.
%%------------------------------------------------------------------------------

-define(TRUNCATE_07, "\ntruncate table tbl purge materialized view log drop storage;").
-define(
  TRUNCATE_07_RESULT_DEFAULT,
  "TRUNCATE TABLE\n    Tbl\n        PURGE MATERIALIZED VIEW LOG\n        DROP STORAGE;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 01 - |.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_01,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7,\nc_8|:abc defghijkl|\nfrom dual;"
).
-define(
  UNBREAKABLE_01_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, C_8\n    |:abc| Defghijkl\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 02 - '.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_02,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7,\n'abc\"de f|gh,i*jkl'\nfrom dual;"
).
-define(
  UNBREAKABLE_02_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7,\n    'abc\"de f|gh,i*jkl'\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 03 - ".
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_03,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7,\n\"ab c'de,f|ghi*jkl\"\nfrom dual;"
).
-define(
  UNBREAKABLE_03_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7,\n    \"ab c'de,f|ghi*jkl\"\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 04 - /* */.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_04,
  "\nselect /* xxaxx xxbxx xxcxx ' xxdxx xxexx , xxfxx | xxgxx xxhxx xxixx \" xxjxxkxxlxx*/\ncolumn_1,column_2,column_3,column_4,column_5,column_6,column_7, column_8\nfrom dual;"
).
-define(
  UNBREAKABLE_04_RESULT_DEFAULT,
  "SELECT /* xxaxx xxbxx xxcxx ' xxdxx xxexx , xxfxx | xxgxx xxhxx xxixx \" xxjxxkxxlxx*/\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7,\n    Column_8\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 05 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_05,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7, Colu_8, column_9\nfrom dual;"
).
-define(
  UNBREAKABLE_05_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, Colu_8\n    , Column_9\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 06 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_06,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7, Col_8, column_9\nfrom dual;"
).
-define(
  UNBREAKABLE_06_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, Col_8,\n    Column_9\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 07 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_07,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7, \"Col8\" column_8\nfrom dual;"
).
-define(
  UNBREAKABLE_07_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, \"Col8\"\n    Column_8\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 08 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_08,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7, colum8(+) column_8\nfrom dual;"
).
-define(
  UNBREAKABLE_08_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, Colum8\n    (+) Column_8\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 09 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_09,
  "\nselect column_1,column_2,column_3,column_4,(column_51+column_62+column_73+colum84(+)) column_5_8\nfrom dual;"
).
-define(
  UNBREAKABLE_09_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_51 + Column_62 + Column_73 +\n    Colum84(+) Column_5_8\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 10 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_10,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7|:a_obj:x|,colum8(+)\nfrom dual;"
).
-define(
  UNBREAKABLE_10_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7\n    |:a_obj:x|, Colum8(+)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 11 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_11,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7|:a_o:x|,colum8(+)\nfrom dual;"
).
-define(
  UNBREAKABLE_11_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7|:a_o:x|\n    , Colum8(+)\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNBREAKABLE 12 - limit.
%%------------------------------------------------------------------------------

-define(
  UNBREAKABLE_12,
  "\nselect column_1,column_2,column_3,column_4,column_5,column_6,column_7, 'Col8' column_8\nfrom dual;"
).
-define(
  UNBREAKABLE_12_RESULT_DEFAULT,
  "SELECT\n    Column_1, Column_2, Column_3, Column_4, Column_5, Column_6, Column_7, 'Col8'\n    Column_8\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 01 - very simple.
%%------------------------------------------------------------------------------

-define(UNION_01, "\nselect *\nfrom table_1 union select * from table_2;").
-define(
  UNION_01_RESULT_DEFAULT,
  "    (SELECT\n        *\n    FROM\n        Table_1)\nUNION\n    (SELECT\n        *\n    FROM\n        Table_2);"
).

%%------------------------------------------------------------------------------
%% UNION 02 - nested.
%%------------------------------------------------------------------------------

-define(
  UNION_02,
  "\nselect * from (select * from table_11 intersect select * from table_12)\nunion all select * from (select * from table_21 minus select * from table_22);"
).
-define(
  UNION_02_RESULT_DEFAULT,
  "    (SELECT\n        *\n    FROM\n            ((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12)))\nUNION ALL\n    (SELECT\n        *\n    FROM\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_22)));"
).

%%------------------------------------------------------------------------------
%% UNION 03 - nested.
%%------------------------------------------------------------------------------

-define(
  UNION_03,
  "\nselect * from (select * from table_11 intersect select * from table_12)\nunion all select * from (select * from table_21 minus select * from (select * from table_31 union select * from table_32));"
).
-define(
  UNION_03_RESULT_DEFAULT,
  "    (SELECT\n        *\n    FROM\n            ((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12)))\nUNION ALL\n    (SELECT\n        *\n    FROM\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                    ((SELECT\n                        *\n                    FROM\n                        Table_31)\n                UNION\n                    (SELECT\n                        *\n                    FROM\n                        Table_32)))));"
).

%%------------------------------------------------------------------------------
%% UNION 04 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_04, "\nselect column_1,(select * from dual) union (select * from dual)\nfrom dual;").
-define(
  UNION_04_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION\n        (SELECT\n            *\n        FROM\n            Dual))\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 05 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_05,
  "\nselect column_1,(select * from dual) union (select * from dual),column_2\nfrom dual;"
).
-define(
  UNION_05_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION\n        (SELECT\n            *\n        FROM\n            Dual)), Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 06 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_06,
  "\nselect column_1,(select * from dual) union all (select * from dual)\nfrom dual;"
).
-define(
  UNION_06_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Dual))\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 07 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_07,
  "\nselect column_1,(select * from dual) union all (select * from dual),column_2\nfrom dual;"
).
-define(
  UNION_07_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Dual)), Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 08 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(UNION_08, "\nselect column_1,(select * from dual) minus (select * from dual)\nfrom dual;").
-define(
  UNION_08_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    MINUS\n        (SELECT\n            *\n        FROM\n            Dual))\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 09 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_09,
  "\nselect column_1,(select * from dual) minus (select * from dual),column_2\nfrom dual;"
).
-define(
  UNION_09_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    MINUS\n        (SELECT\n            *\n        FROM\n            Dual)), Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 10 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_10,
  "\nselect column_1,(select * from dual) intersect (select * from dual)\nfrom dual;"
).
-define(
  UNION_10_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Dual))\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 11 - COLUMN & INTERSECT / MINUS / UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_11,
  "\nselect column_1,(select * from dual) intersect (select * from dual),column_2\nfrom dual;"
).
-define(
  UNION_11_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Dual)), Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 12 - ALIAS.
%%------------------------------------------------------------------------------

-define(
  UNION_12,
  "\nselect (select * from dual) union (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_12_RESULT_DEFAULT,
  "SELECT\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 13 - ALIAS.
%%------------------------------------------------------------------------------

-define(
  UNION_13,
  "\nselect column_1,(select * from dual) union (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_13_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 14 - ALIAS.
%%------------------------------------------------------------------------------

-define(
  UNION_14,
  "\nselect column_1,(select * from dual) union (select * from dual) as column_2,column_3\nfrom dual;"
).
-define(
  UNION_14_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2, Column_3\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 15 - ALIAS.
%%------------------------------------------------------------------------------

-define(
  UNION_15,
  "\nselect (select * from dual) union all (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_15_RESULT_DEFAULT,
  "SELECT\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 16 - ALIAS.
%%------------------------------------------------------------------------------

-define(
  UNION_16,
  "\nselect column_1,(select * from dual) union all (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_16_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 17 - ALIAS.
%%------------------------------------------------------------------------------

-define(
  UNION_17,
  "\nselect column_1,(select * from dual) union all (select * from dual) as column_2,column_3\nfrom dual;"
).
-define(
  UNION_17_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    UNION ALL\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2, Column_3\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 18 - ALIAS & MINUS.
%%------------------------------------------------------------------------------

-define(
  UNION_18,
  "\nselect (select * from dual) minus (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_18_RESULT_DEFAULT,
  "SELECT\n        ((SELECT\n            *\n        FROM\n            Dual)\n    MINUS\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 19 - ALIAS & MINUS.
%%------------------------------------------------------------------------------

-define(
  UNION_19,
  "\nselect column_1,(select * from dual) minus (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_19_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    MINUS\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 20 - ALIAS & MINUS.
%%------------------------------------------------------------------------------

-define(
  UNION_20,
  "\nselect column_1,(select * from dual) minus (select * from dual) as column_2,column_3\nfrom dual;"
).
-define(
  UNION_20_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    MINUS\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2, Column_3\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 21 - ALIAS & INTERSECT.
%%------------------------------------------------------------------------------

-define(
  UNION_21,
  "\nselect (select * from dual) intersect (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_21_RESULT_DEFAULT,
  "SELECT\n        ((SELECT\n            *\n        FROM\n            Dual)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 22 - ALIAS & INTERSECT.
%%------------------------------------------------------------------------------

-define(
  UNION_22,
  "\nselect column_1,(select * from dual) intersect (select * from dual) as column_2\nfrom dual;"
).
-define(
  UNION_22_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 23 - ALIAS & INTERSECT.
%%------------------------------------------------------------------------------

-define(
  UNION_23,
  "\nselect column_1,(select * from dual) intersect (select * from dual) as column_2,column_3\nfrom dual;"
).
-define(
  UNION_23_RESULT_DEFAULT,
  "SELECT\n    Column_1, "
  ++
  "\n        ((SELECT\n            *\n        FROM\n            Dual)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Dual)) Column_2, Column_3\nFROM\n    Dual;"
).

%%------------------------------------------------------------------------------
%% UNION 24 - SELECT.
%%------------------------------------------------------------------------------

-define(UNION_24, "\nselect *\nfrom table_1 union select * from table_2;").
-define(
  UNION_24_RESULT_DEFAULT,
  "    (SELECT\n        *\n    FROM\n        Table_1)\nUNION\n    (SELECT\n        *\n    FROM\n        Table_2);"
).

%%------------------------------------------------------------------------------
%% UNION 25 - SELECT FROM.
%%------------------------------------------------------------------------------

-define(UNION_25, "\nselect *\nfrom (select * from table_1 union select * from table_2);").
-define(
  UNION_25_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n          ((SELECT\n            *\n        FROM\n            Table_1)\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_2));"
).

%%------------------------------------------------------------------------------
%% UNION 26 - SELECT WHERE.
%%------------------------------------------------------------------------------

-define(
  UNION_26,
  "\nselect *\nfrom table_1 where column_1 in (select * from table_1 union select * from table_2);"
).
-define(
  UNION_26_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\nWHERE\n    Column_1 IN (((SELECT\n            *\n        FROM\n            Table_1)\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_2)));"
).

%%------------------------------------------------------------------------------
%% UNION 27 - UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_27,
  "\n(select * from table_11 intersect select * from table_12)\nunion\n(select * from table_21 minus select * from table_22);"
).
-define(
  UNION_27_RESULT_DEFAULT,
  "        ((SELECT\n            *\n        FROM\n            Table_11)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Table_12))\nUNION\n        ((SELECT\n            *\n        FROM\n            Table_21)\n    MINUS\n        (SELECT\n            *\n        FROM\n            Table_22));"
).

%%------------------------------------------------------------------------------
%% UNION 28 - UNION FROM.
%%------------------------------------------------------------------------------

-define(
  UNION_28,
  "\nselect *\nfrom ((select * from table_11 intersect select * from table_12) union (select * from table_21 minus select * from table_22));"
).
-define(
  UNION_28_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n            (((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12))\n    UNION\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_22)));"
).

%%------------------------------------------------------------------------------
%% UNION 29 - UNION WHERE.
%%------------------------------------------------------------------------------

-define(
  UNION_29,
  "\nselect *\nfrom table_1 where column_1 in ((select * from table_11 intersect select * from table_12) union (select * from table_21 minus select * from table_22));"
).
-define(
  UNION_29_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\nWHERE\n    Column_1 IN ((((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12))\n    UNION\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_22))));"
).

%%------------------------------------------------------------------------------
%% UNION 30 - SELECT & UNION.
%%------------------------------------------------------------------------------

-define(
  UNION_30,
  "\nselect *\nfrom table_1 union (select * from table_21 minus select * from table_22);"
).
-define(
  UNION_30_RESULT_DEFAULT,
  "    (SELECT\n        *\n    FROM\n        Table_1)\nUNION\n        ((SELECT\n            *\n        FROM\n            Table_21)\n    MINUS\n        (SELECT\n            *\n        FROM\n            Table_22));"
).

%%------------------------------------------------------------------------------
%% UNION 31 - SELECT & UNION FROM.
%%------------------------------------------------------------------------------

-define(
  UNION_31,
  "\nselect *\nfrom (select * from table_1 union (select * from table_21 minus select * from table_22));"
).
-define(
  UNION_31_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n        ((SELECT\n            *\n        FROM\n            Table_1)\n    UNION\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_22)));"
).

%%------------------------------------------------------------------------------
%% UNION 32 - SELECT & UNION WHERE.
%%------------------------------------------------------------------------------

-define(
  UNION_32,
  "\nselect *\nfrom table_1 where column_1 in (select * from table_1 union (select * from table_21 minus select * from table_22));"
).
-define(
  UNION_32_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\nWHERE\n    Column_1 IN (((SELECT\n            *\n        FROM\n            Table_1)\n    UNION\n            ((SELECT\n                *\n            FROM\n                Table_21)\n        MINUS\n            (SELECT\n                *\n            FROM\n                Table_22))));"
).

%%------------------------------------------------------------------------------
%% UNION 33 - UNION & SELECT.
%%------------------------------------------------------------------------------

-define(
  UNION_33,
  "\n(select *\nfrom table_11 intersect select * from table_12) union select * from table_2;"
).
-define(
  UNION_33_RESULT_DEFAULT,
  "        ((SELECT\n            *\n        FROM\n            Table_11)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Table_12))\nUNION\n    (SELECT\n        *\n    FROM\n        Table_2);"
).

%%------------------------------------------------------------------------------
%% UNION 34 - UNION & SELECT FROM.
%%------------------------------------------------------------------------------

-define(
  UNION_34,
  "\nselect *\nfrom ((select * from table_11 intersect select * from table_12) union select * from table_2);"
).
-define(
  UNION_34_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n            (((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12))\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_2));"
).

%%------------------------------------------------------------------------------
%% UNION 35 - UNION & SELECT WHERE.
%%------------------------------------------------------------------------------

-define(
  UNION_35,
  "\nselect *\nfrom table_1 where column_1 in ((select * from table_11 intersect select * from table_12) union select * from table_2);"
).
-define(
  UNION_35_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Table_1\nWHERE\n    Column_1 IN ((((SELECT\n                *\n            FROM\n                Table_11)\n        INTERSECT\n            (SELECT\n                *\n            FROM\n                Table_12))\n    UNION\n        (SELECT\n            *\n        FROM\n            Table_2)));"
).

%%------------------------------------------------------------------------------
%% UNION 36 - MINUS & INTERSECT.
%%------------------------------------------------------------------------------

-define(
  UNION_36,
  "\n(select * from table_1)|:_a::b::c|\nMinus ((select * from table_2) Intersect (select * from table_3));"
).
-define(
  UNION_36_RESULT_DEFAULT,
  "((SELECT\n        *\n    FROM\n        Table_1))|:_A::b::c|\nMINUS\n        ((SELECT\n            *\n        FROM\n            Table_2)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Table_3));"
).

%%------------------------------------------------------------------------------
%% UNION 37 - INTERSECT & MINUS.
%%------------------------------------------------------------------------------

-define(
  UNION_37,
  "\n((select * from table_2)\nIntersect (select * from table_3)) Minus (select * from table_1)|:_a::b::c|;"
).
-define(
  UNION_37_RESULT_DEFAULT,
  "        ((SELECT\n            *\n        FROM\n            Table_2)\n    INTERSECT\n        (SELECT\n            *\n        FROM\n            Table_3))\nMINUS\n    ((SELECT\n        *\n    FROM\n        Table_1))|:_A::b::c|;"
).

%%------------------------------------------------------------------------------
%% UPDATE 01 - simple.
%%------------------------------------------------------------------------------

-define(UPDATE_01, "\nupdate name_table set name_column_1 = :value_1;").
-define(UPDATE_01_RESULT_DEFAULT, "UPDATE\n    Name_Table\nSET\n    Name_Column_1 = :value_1;").

%%------------------------------------------------------------------------------
%% UPDATE 02 - simple.
%%------------------------------------------------------------------------------

-define(UPDATE_02, "\nupdate name_table set name_column_1 = :value_1, name_column_2 = :value_2;").
-define(
  UPDATE_02_RESULT_DEFAULT,
  "UPDATE\n    Name_Table\nSET\n    Name_Column_1 = :value_1,\n    Name_Column_2 = :value_2;"
).

%%------------------------------------------------------------------------------
%% UPDATE 03 - WHERE.
%%------------------------------------------------------------------------------

-define(
  UPDATE_03,
  "\nupdate name_table set name_column_1 = :value_1, name_column_2 = :value_2\nwhere company_id = :id1 and employee_id = :id2;"
).
-define(
  UPDATE_03_RESULT_DEFAULT,
  "UPDATE\n    Name_Table\nSET\n    Name_Column_1 = :value_1,\n    Name_Column_2 = :value_2\nWHERE\n    Company_Id = :id1\n    AND Employee_Id = :id2;"
).

%%------------------------------------------------------------------------------
%% UPDATE 04 - WHERE.
%%------------------------------------------------------------------------------

-define(UPDATE_04, "\nupdate employees set salary = :sal\nwhere employee_id = :id;").
-define(
  UPDATE_04_RESULT_DEFAULT,
  "UPDATE\n    Employees\nSET\n    Salary = :sal\nWHERE\n    Employee_Id = :id;"
).

%%------------------------------------------------------------------------------
%% UPDATE 05 - WHERE & RETURNING.
%%------------------------------------------------------------------------------

-define(
  UPDATE_05,
  "\nupdate employees set salary = :sal\nwhere employee_id = :id\nreturning c,d into :c, :d;"
).
-define(
  UPDATE_05_RESULT_DEFAULT,
  "UPDATE\n    Employees\nSET\n    Salary = :sal\nWHERE\n    Employee_Id = :id\nRETURNING\n    C, D\nINTO\n    :c, :d;"
).

%%------------------------------------------------------------------------------
%% UPDATE 06 - WHERE & RETURNING.
%%------------------------------------------------------------------------------

-define(
  UPDATE_06,
  "\nupdate employees set salary = :sal\nwhere company_id = :id1 and employee_id2 = :id\nreturning lob_column into :out_locator;"
).
-define(
  UPDATE_06_RESULT_DEFAULT,
  "UPDATE\n    Employees\nSET\n    Salary = :sal\nWHERE\n    Company_Id = :id1\n    AND Employee_Id2 = :id\nRETURNING\n    Lob_Column\nINTO\n    :out_locator;"
).

%%------------------------------------------------------------------------------
%% UPDATE 07 - SET.
%%------------------------------------------------------------------------------

-define(
  UPDATE_07,
  "\nUpdate table_1 Set\ncolumn_1 = (Select * from dual) alias_1,\ncolumn_2 = column_3;"
).
-define(
  UPDATE_07_RESULT_DEFAULT,
  "UPDATE\n    Table_1\nSET\n    Column_1 = (SELECT\n        *\n    FROM\n        Dual) Alias_1,\n    Column_2 = Column_3;"
).

%%------------------------------------------------------------------------------
%% VIEW 01 - simple.
%%------------------------------------------------------------------------------

-define(VIEW_01, "\ncreate view table_1\nas select * from dual;").
-define(
  VIEW_01_RESULT_DEFAULT,
  "CREATE VIEW\n    Table_1\nAS\n    SELECT\n        *\n    FROM\n        Dual;"
).

%%------------------------------------------------------------------------------
%% VIEW 02 - simple CHECK.
%%------------------------------------------------------------------------------

-define(VIEW_02, "\ncreate view table_1\nas select * from dual\nwith check option;").
-define(
  VIEW_02_RESULT_DEFAULT,
  "CREATE VIEW\n    Table_1\nAS\n    SELECT\n        *\n    FROM\n        Dual\nWITH CHECK OPTION;"
).

%%------------------------------------------------------------------------------
%% VIEW 03 - COLUMNS.
%%------------------------------------------------------------------------------

-define(VIEW_03, "\ncreate view table_1\n(column_1, column_2)\nas select * from dual;").
-define(
  VIEW_03_RESULT_DEFAULT,
  "CREATE VIEW\n    Table_1 (Column_1, Column_2)\nAS\n    SELECT\n        *\n    FROM\n        Dual;"
).

%%------------------------------------------------------------------------------
%% VIEW 04 - COLUMNS & CHECK.
%%------------------------------------------------------------------------------

-define(
  VIEW_04,
  "\ncreate view table_1\n(column_1, column_2)\nas select * from dual\nwith check option;"
).
-define(
  VIEW_04_RESULT_DEFAULT,
  "CREATE VIEW\n    Table_1 (Column_1, Column_2)\nAS\n    SELECT\n        *\n    FROM\n        Dual\nWITH CHECK OPTION;"
).

%%------------------------------------------------------------------------------
%% VIEW 05 - COLUMNS.
%%------------------------------------------------------------------------------

-define(
  VIEW_05,
  "\ncreate view table_1\n(column_1, column_2, column_3, column_4)\nas select * from dual;"
).
-define(
  VIEW_05_RESULT_DEFAULT,
  "CREATE VIEW\n    Table_1 (Column_1, Column_2, Column_3, Column_4)\nAS\n    SELECT\n        *\n    FROM\n        Dual;"
).

%%------------------------------------------------------------------------------
%% VIEW 06 - COLUMNS & CHECK.
%%------------------------------------------------------------------------------

-define(
  VIEW_06,
  "\ncreate view table_1\n(column_1, column_2, column_3, column_4)\nas select * from dual\nwith check option;"
).
-define(
  VIEW_06_RESULT_DEFAULT,
  "CREATE VIEW\n    Table_1 (Column_1, Column_2, Column_3, Column_4)\nAS\n    SELECT\n        *\n    FROM\n        Dual\nWITH CHECK OPTION;"
).

%%------------------------------------------------------------------------------
%% WHERE 01 - very simple.
%%------------------------------------------------------------------------------

-define(WHERE_01, "\nselect * from dual\nwhere column_1 = column_2;").
-define(WHERE_01_RESULT_DEFAULT, "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 = Column_2;").

%%------------------------------------------------------------------------------
%% WHERE 02 - AND.
%%------------------------------------------------------------------------------

-define(WHERE_02, "\nselect * from dual\nwhere column_11 <> column_12\nand column_21 != column_22;").
-define(
  WHERE_02_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_11 <> Column_12\n    AND Column_21 != Column_22;"
).

%%------------------------------------------------------------------------------
%% WHERE 03 - AND.
%%------------------------------------------------------------------------------

-define(
  WHERE_03,
  "\nselect * from dual\nwhere column_11 <> column_12\nand column_21 != column_22\nor column_31 <> column_32\nand column_41 != column_42;"
).
-define(
  WHERE_03_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_11 <> Column_12\n    AND Column_21 != Column_22\n    OR Column_31 <> Column_32\n    AND Column_41 != Column_42;"
).

%%------------------------------------------------------------------------------
%% WHERE 04 - AND.
%%------------------------------------------------------------------------------

-define(
  WHERE_04,
  "\nselect * from dual\nwhere column_11 <> column_12\nand column_21 != column_22\nor column_31 <> column_32\nand column_41 != column_42\nor column_51 <> column_52\nand column_61 != column_62;"
).
-define(
  WHERE_04_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_11 <> Column_12\n    AND Column_21 != Column_22\n    OR Column_31 <> Column_32\n    AND Column_41 != Column_42\n    OR Column_51 <> Column_52\n    AND Column_61 != Column_62;"
).

%%------------------------------------------------------------------------------
%% WHERE 05 - left subquery.
%%------------------------------------------------------------------------------

-define(WHERE_05, "\nselect * from dual\nwhere (select column_31 from table_31) <> column_14;").
-define(
  WHERE_05_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    (SELECT\n        Column_31\n    FROM\n        Table_31) <> Column_14;"
).

%%------------------------------------------------------------------------------
%% WHERE 06 - right subquery.
%%------------------------------------------------------------------------------

-define(WHERE_06, "\nselect * from dual\nwhere column_11 <> (select column_41 from table_41);").
-define(
  WHERE_06_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_11 <> (SELECT\n        Column_41\n    FROM\n        Table_41);"
).

%%------------------------------------------------------------------------------
%% WHERE 07 - both subqueries.
%%------------------------------------------------------------------------------

-define(
  WHERE_07,
  "\nselect * from dual\nwhere (select column_31 from table_31) <> (select column_41 from table_41);"
).
-define(
  WHERE_07_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    (SELECT\n        Column_31\n    FROM\n        Table_31) <> (SELECT\n        Column_41\n    FROM\n        Table_41);"
).

%%------------------------------------------------------------------------------
%% WHERE 08 - AND NOT.
%%------------------------------------------------------------------------------

-define(WHERE_08, "\nselect * from dual\nwhere column_1 = column_2 and not column_3 = column_4;").
-define(
  WHERE_08_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 = Column_2\n    AND NOT (Column_3 = Column_4);"
).

%%------------------------------------------------------------------------------
%% WHERE 09 - OR NOT.
%%------------------------------------------------------------------------------

-define(WHERE_09, "\nselect * from dual\nwhere column_1 = column_2 or not column_3 = column_4;").
-define(
  WHERE_09_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 = Column_2\n    OR NOT (Column_3 = Column_4);"
).

%%------------------------------------------------------------------------------
%% WHERE 10 - AND & OR NOT.
%%------------------------------------------------------------------------------

-define(
  WHERE_10,
  "\nselect * from dual\nwhere column_1 = column_2 and not column_3 = column_4 and column_5 = column_6 and not column_7 = column_8;"
).
-define(
  WHERE_10_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 = Column_2\n    AND NOT (Column_3 = Column_4)\n    AND Column_5 = Column_6\n    AND NOT (Column_7 = Column_8);"
).

%%------------------------------------------------------------------------------
%% WHERE 11 - BETWEEN, IS NOT, LIKE.
%%------------------------------------------------------------------------------

-define(
  WHERE_11,
  "\nselect * from dual\nwhere column_1 is null and column_2 is not null and column_3 between column_4 and column_6 and column_7 like column_8;"
).
-define(
  WHERE_11_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 IS NULL\n    AND NOT (Column_2 IS NULL)\n    AND Column_3 BETWEEN Column_4 AND Column_6\n    AND Column_7 LIKE Column_8;"
).

%%------------------------------------------------------------------------------
%% WHERE 12 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_12, "\nselect * from dual\nwhere column_1 in (1,2,3,4);").
-define(
  WHERE_12_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 IN (1, 2, 3, 4);"
).

%%------------------------------------------------------------------------------
%% WHERE 13 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_13, "\nselect * from dual\nwhere column_1 in (column_2);").
-define(
  WHERE_13_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 IN (Column_2);"
).

%%------------------------------------------------------------------------------
%% WHERE 14 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_14, "\nselect * from dual\nwhere column_1 in (select * from table_2);").
-define(
  WHERE_14_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 IN (SELECT\n        *\n    FROM\n        Table_2);"
).

%%------------------------------------------------------------------------------
%% WHERE 15 - ALL.
%%------------------------------------------------------------------------------

-define(WHERE_15, "\nselect * from dual\nwhere column_1 > all (select * from table_1);").
-define(
  WHERE_15_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 > ALL\n    (SELECT\n        *\n    FROM\n        Table_1);"
).

%%------------------------------------------------------------------------------
%% WHERE 16 - ANY.
%%------------------------------------------------------------------------------

-define(WHERE_16, "\nselect * from dual\nwhere column_1 > any (select * from table_1);").
-define(
  WHERE_16_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 > ANY\n    (SELECT\n        *\n    FROM\n        Table_1);"
).

%%------------------------------------------------------------------------------
%% WHERE 17 - EXISTS.
%%------------------------------------------------------------------------------

-define(WHERE_17, "\nselect * from dual\nwhere exists (select * from table_1);").
-define(
  WHERE_17_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    EXISTS\n    (SELECT\n        *\n    FROM\n        Table_1);"
).

%%------------------------------------------------------------------------------
%% WHERE 18 - NOT EXISTS.
%%------------------------------------------------------------------------------

-define(WHERE_18, "\nselect * from dual\nwhere not exists (select * from table_1);").
-define(
  WHERE_18_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    NOT (EXISTS\n    (SELECT\n        *\n    FROM\n        Table_1));"
).

%%------------------------------------------------------------------------------
%% WHERE 19 - START WITH & CONNECT BY.
%%------------------------------------------------------------------------------

-define(
  WHERE_19,
  "\nselect * from dual\nstart with column_1 is null and column_2 < 0 connect by column_2 = column_3 or column_4 <> column_5;"
).
-define(
  WHERE_19_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nSTART WITH\n    Column_1 IS NULL\n    AND Column_2 < 0\nCONNECT BY\n    Column_2 = Column_3\n    OR Column_4 <> Column_5;"
).

%%------------------------------------------------------------------------------
%% WHERE 20 - START WITH & CONNECT BY.
%%------------------------------------------------------------------------------

-define(
  WHERE_20,
  "\nselect * from dual\nstart with column_1 is null and column_2 < 0 connect by nocycle column_2 = column_3 or column_4 <> column_5;"
).
-define(
  WHERE_20_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nSTART WITH\n    Column_1 IS NULL\n    AND Column_2 < 0\nCONNECT BY NOCYCLE\n    Column_2 = Column_3\n    OR Column_4 <> Column_5;"
).

%%------------------------------------------------------------------------------
%% WHERE 21 - START WITH & CONNECT BY.
%%------------------------------------------------------------------------------

-define(
  WHERE_21,
  "\nselect * from dual\nconnect by nocycle column_2 = column_3 or column_4 <> column_5 start with column_1 is null and column_2 < 0;"
).
-define(
  WHERE_21_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nCONNECT BY NOCYCLE\n    Column_2 = Column_3\n    OR Column_4 <> Column_5\nSTART WITH\n    Column_1 IS NULL\n    AND Column_2 < 0;"
).

%%------------------------------------------------------------------------------
%% WHERE 22 - SELECT = SELECT.
%%------------------------------------------------------------------------------

-define(
  WHERE_22,
  "\nselect * from dual\nwhere (select column_1 from dual) = (select column_2 from dual);"
).
-define(
  WHERE_22_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    (SELECT\n        Column_1\n    FROM\n        Dual) = (SELECT\n        Column_2\n    FROM\n        Dual);"
).

%%------------------------------------------------------------------------------
%% WHERE 23 - = SELECT.
%%------------------------------------------------------------------------------

-define(WHERE_23, "\nselect * from dual\nwhere column_1 = (select column_2 from dual);").
-define(
  WHERE_23_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 = (SELECT\n        Column_2\n    FROM\n        Dual);"
).

%%------------------------------------------------------------------------------
%% WHERE 24 - SELECT =.
%%------------------------------------------------------------------------------

-define(WHERE_24, "\nselect * from dual\nwhere (select column_1 from dual) = column_2;").
-define(
  WHERE_24_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    (SELECT\n        Column_1\n    FROM\n        Dual) = Column_2;"
).

%%------------------------------------------------------------------------------
%% WHERE 25 - SELECT =.
%%------------------------------------------------------------------------------

-define(WHERE_25, "\nselect * from dual\nwhere column_1 = column_2 and column_3 = column_4;").
-define(
  WHERE_25_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 = Column_2\n    AND Column_3 = Column_4;"
).

%%------------------------------------------------------------------------------
%% WHERE 26 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_26, "\nselect * from dual\nwhere column_1 like (3 + 5);").
-define(WHERE_26_RESULT_DEFAULT, "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 LIKE 3 + 5;").

%%------------------------------------------------------------------------------
%% WHERE 27 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_27, "\nselect * from dual\nwhere column_1 like (select * from dual);").
-define(
  WHERE_27_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 LIKE (SELECT\n        *\n    FROM\n        Dual);"
).

%%------------------------------------------------------------------------------
%% WHERE 28 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_28, "\nselect * from dual\nwhere column_1 like select * from dual;").
-define(
  WHERE_28_RESULT_DEFAULT,
  "SELECT\n    *\nFROM\n    Dual\nWHERE\n    Column_1 LIKE (SELECT\n        *\n    FROM\n        Dual);"
).

%%------------------------------------------------------------------------------
%% WHERE 29 - IN.
%%------------------------------------------------------------------------------

-define(WHERE_29, "\nDelete From table_1 Where\ncolumn_1 Not In\n(1,2,3);").
-define(
  WHERE_29_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT (Column_1 IN (1, 2, 3));"
).

%%------------------------------------------------------------------------------
%% WHERE 30 - IN.
%%------------------------------------------------------------------------------

-define(
  WHERE_30,
  "\nDelete From table_1 Where\n(Select column_1 from table_1) Not In\n(Select column_2 from table_2);"
).
-define(
  WHERE_30_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT ((SELECT\n        Column_1\n    FROM\n        Table_1) IN (SELECT\n        Column_2\n    FROM\n        Table_2));"
).

%%------------------------------------------------------------------------------
%% WHERE 31 - IN.
%%------------------------------------------------------------------------------

-define(
  WHERE_31,
  "\nDelete From table_1 Where\n((Select column_2 from table_2) union (Select column_3 from table_3)) Not In\n((Select column_4 from table_4) union (Select column_5 from table_5));"
).
-define(
  WHERE_31_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT (((SELECT\n            Column_2\n        FROM\n            Table_2)\n    UNION\n        (SELECT\n            Column_3\n        FROM\n            Table_3)) IN (((SELECT\n            Column_4\n        FROM\n            Table_4)\n    UNION\n        (SELECT\n            Column_5\n        FROM\n            Table_5))));"
).

%%------------------------------------------------------------------------------
%% WHERE 32 - LIKE.
%%------------------------------------------------------------------------------

-define(WHERE_32, "\nDelete From table_1\nWhere column_1 Not Like\ncolumn_2;").
-define(
  WHERE_32_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT (Column_1 LIKE Column_2);"
).

%%------------------------------------------------------------------------------
%% WHERE 33 - LIKE.
%%------------------------------------------------------------------------------

-define(
  WHERE_33,
  "\nDelete From table_1\nWhere (Select column_1 from table_1) Not Like\n(Select column_2 from table_2);"
).
-define(
  WHERE_33_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT ((SELECT\n        Column_1\n    FROM\n        Table_1) LIKE (SELECT\n        Column_2\n    FROM\n        Table_2));"
).

%%------------------------------------------------------------------------------
%% WHERE 34 - LIKE.
%%------------------------------------------------------------------------------

-define(
  WHERE_34,
  "\nDelete From table_1\nWhere ((Select column_2 from table_2) union (Select column_3 from table_3)) Not Like\n((Select column_4 from table_4) union (Select column_5 from table_5));"
).
-define(
  WHERE_34_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT (((SELECT\n            Column_2\n        FROM\n            Table_2)\n    UNION\n        (SELECT\n            Column_3\n        FROM\n            Table_3)) LIKE (((SELECT\n            Column_4\n        FROM\n            Table_4)\n    UNION\n        (SELECT\n            Column_5\n        FROM\n            Table_5))));"
).

%%------------------------------------------------------------------------------
%% WHERE 35 - BETWEEN.
%%------------------------------------------------------------------------------

-define(WHERE_35, "\nDelete From table_1\nWhere column_1 Not between\ncolumn_2 and column_3;").
-define(
  WHERE_35_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT (Column_1 BETWEEN Column_2 AND Column_3);"
).

%%------------------------------------------------------------------------------
%% WHERE 36 - BETWEEN.
%%------------------------------------------------------------------------------

-define(
  WHERE_36,
  "\nDelete From table_1\nWhere (Select column_1 from table_1) Not between\n(Select column_2 from table_2)\nand\n(Select column_4 from table_4);"
).
-define(
  WHERE_36_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT ((SELECT\n        Column_1\n    FROM\n        Table_1) BETWEEN (SELECT\n        Column_2\n    FROM\n        Table_2) AND (SELECT\n        Column_4\n    FROM\n        Table_4));"
).

%%------------------------------------------------------------------------------
%% WHERE 37 - BETWEEN.
%%------------------------------------------------------------------------------

-define(
  WHERE_37,
  "\nDelete From table_1\nWhere ((Select column_2 from table_2) intersect (Select column_2 from table_2)) Not between\n((Select column_3 from table_3) minus (Select column_4 from table_4))\nand\n((Select column_5 from table_5) union (Select column_6 from table_6));"
).
-define(
  WHERE_37_RESULT_DEFAULT,
  "DELETE FROM\n    Table_1\nWHERE\n    NOT (((SELECT\n            Column_2\n        FROM\n            Table_2)\n    INTERSECT\n        (SELECT\n            Column_2\n        FROM\n            Table_2)) BETWEEN ((SELECT\n            Column_3\n        FROM\n            Table_3)\n    MINUS\n        (SELECT\n            Column_4\n        FROM\n            Table_4)) AND ((SELECT\n            Column_5\n        FROM\n            Table_5)\n    UNION\n        (SELECT\n            Column_6\n        FROM\n            Table_6)));"
).
-endif.
