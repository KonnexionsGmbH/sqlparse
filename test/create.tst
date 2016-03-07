%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS CREATE TABLE
%%

"create table key_test (col1 '{atom,integer}', col2 '{string,binstr}')".

"CREATE TABLE Persons
(
P_Id int,
LastName varchar2,
LastName varchar2(255),
FirstName varchar2(255),
Address varchar2(255),
City varchar2(255),
testfield varchar(10)
)".

"create table table_1 (
owner userid,
private term,
field_e date default fun()-> calendar:localtime() end.,
field_t decimal default fun mod:fun/1.,
field_t1 bool,
field_t1 boolean,
field_e date1 default fun(C,D)-> C+D end.,
field_t1 number,
field_t1 number(1),
field_t1 number(1,2),
field_a atom default 'undefined',
field_b list,
'field_c' string default 'NULL',
'field_d' tuple default erl(\"{1,2}\")
)".

"CREATE LOCAL BAG TABLE test (fld CHAR)".

"CREATE CLUSTER SET TABLE test (fld CHAR)".

"CREATE CLUSTER annything_random TABLE test ()".

"CREATE CLUSTER annything_random TABLE test (a int)".

"CREATE SCHEMA ORDERED_SET TABLE test (fld CHAR)".

"CREATE ORDERED_SET TABLE test (fld CHAR)".

"CREATE SCHEMA TABLE test (fld CHAR)".

"CREATE TABLE test (fld CHAR)".

"CREATE LOCAL BAG TABLE test
(
fld CHAR
, fld VARCHAR2(13) DEFAULT '123'
, fld BLOB(2000)
, fld INT DEFAULT 99999999999999999
, fld FLOAT(-3) DEFAULT 123456
, fld DECIMAL(10,3) DEFAULT 1.1234
, fld DATE DEFAULT SYSDATE
, fld DATETIME
, fld TIMESTAMP
, fld INT
)".

"CREATE LOCAL TABLE fun_test
(
fld TUPLE(0) default fun(_D) -> {} end.
, fld BINARY(1000)
, fld atom
, fld ipaddr(4) default fun() -> {0,0,0,0} end. 
, fld LIST(0) default fun() -> [] end.
, fld BINSTR(1000) default fun(C,D) -> <<\"no_value\">> end.
, fld PID
, fld ref
, fld datetime default fun() -> calendar:local_time() end.
, fld timestamp(3) default fun(A,B) -> erlang:now() end.
, fld INTEGER(10,-3)
)".

%%
%% TESTS CREATE USER
%%

"CREATE USER test_user_1 IDENTIFIED BY a_password".

"CREATE USER test_user_3 IDENTIFIED EXTERNALLY".

"CREATE USER test_user_2 IDENTIFIED EXTERNALLY AS test_usr_2_extern".

"CREATE USER test_user_4 IDENTIFIED GLOBALLY".

"CREATE USER test_user_4 IDENTIFIED GLOBALLY AS test_usr_2_extern".

"CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1".

"CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 DEFAULT TABLESPACE table_2".

"CREATE USER test_user_1 IDENTIFIED EXTERNALLY AS test_usr_2_extern TEMPORARY TABLESPACE table_1".

"CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 TEMPORARY TABLESPACE table_2".

"CREATE USER test_user_3 IDENTIFIED EXTERNALLY PROFILE user_profile".

% PASSWORD not supported as keyword
%"CREATE USER test_user_3 IDENTIFIED EXTERNALLY PASSWORD EXPIRE".

% ACCOUNT not supported as keyword
%"CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT LOCK".
%"CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT UNLOCK".

"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA UNLIMITED ON table_1".

"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_2".

"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_3 QUOTA UNLIMITED ON table_1".

"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10 ON table_3 QUOTA 10M ON table_4 QUOTA UNLIMITED
 ON table_1".

%%
%% TESTS CREATE VIEW
%%

% ? "CREATE VIEW name_table_1".
