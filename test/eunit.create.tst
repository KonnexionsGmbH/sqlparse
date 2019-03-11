%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: CREATE INDEX & CREATE ROLE & CREATE TABLE & CREATE USER
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"create bitmap index index_1 on :param_1 alias_1".
"create bitmap index index_1 on :param_1".
"create bitmap index index_1 on \"%%__xxx__%%\" alias_1".
"create bitmap index index_1 on \"%%__xxx__%%\"".
"create bitmap index index_1 on schema_1.table_1 alias_1".
"create bitmap index index_1 on table_1 alias_1".
"create bitmap index index_1 on table_1".
"create bitmap index on :param_1 alias_1".
"create bitmap index on :param_1".
"create bitmap index on \"%%__xxx__%%\" alias_1".
"create bitmap index on \"%%__xxx__%%\"".
"create bitmap index on schema_1.table_1 alias_1".
"CREATE BITMAP INDEX ON tab".
"create bitmap index on table_1 alias_1".
"create bitmap index on table_1".
"create bitmap index s.a on s.d (f)".
"create bitmap index s.a on s.d (f, g)".
"create bitmap index table_1.index_1 on schema_1.table_1 alias_1".
"create bitmap index table_1.index_1 on table_1 alias_1".
"create bitmap index table_1.index_1 on table_1".
"create hashmap index index_1 on :param_1 alias_1".
"create hashmap index index_1 on :param_1".
"create hashmap index index_1 on \"%%__xxx__%%\" alias_1".
"create hashmap index index_1 on \"%%__xxx__%%\"".
"create hashmap index index_1 on schema_1.table_1 alias_1".
"create hashmap index index_1 on table_1 alias_1".
"create hashmap index index_1 on table_1".
"create hashmap index on :param_1 alias_1".
"create hashmap index on :param_1".
"create hashmap index on \"%%__xxx__%%\" alias_1".
"create hashmap index on \"%%__xxx__%%\"".
"create hashmap index on schema_1.table_1 alias_1".
"create hashmap index on table_1 alias_1".
"create hashmap index on table_1".
"create hashmap index s.a on s.d (f)".
"create hashmap index table_1.index_1 on schema_1.table_1 alias_1".
"create hashmap index table_1.index_1 on table_1 alias_1".
"create hashmap index table_1.index_1 on table_1".
"create index a on b (a|:d{}|) norm_with fun() -> norm end. filter_with fun mod:modfun/5.".
"create index a on b (a|:d|)".
"create index a on b (a|:d|, b|:e|)".
"create index a on b (a|:d|,e|:f|)".
"create index a on b (f) norm_with fun() -> norm end.".
"create Index index_1 On :param_1 (column_1)".
"create Index index_1 On :param_1 alias_1 (column_1)".
"Create Index index_1 On :param_1 alias_1".
"create index index_1 on :param_1 alias_1".
"create index index_1 on :param_1 filter_with'filter_string'".
"create index index_1 on :param_1 norm_with'norm_string' filter_with'filter_string'".
"create index index_1 on :param_1 norm_with'norm_string'".
"create index index_1 on :param_1".
"Create Index index_1 On :param_1".
"create index index_1 on \"%%__xxx__%%\" alias_1".
"create index index_1 on \"%%__xxx__%%\"".
"create Index index_1 On \"^&()\" (column_1)".
"create Index index_1 On \"^&()\" alias_1 (column_1)".
"Create Index index_1 On \"^&()\" alias_1".
"Create Index index_1 On \"^&()\"".
"create Index index_1 On schema_1.table_1 (column_1)".
"create Index index_1 On schema_1.table_1 alias_1 (column_1)".
"Create Index index_1 On schema_1.table_1 alias_1".
"create index index_1 on schema_1.table_1 alias_1".
"Create Index index_1 On schema_1.table_1".
"create Index index_1 On table_1 (column_1)".
"create Index index_1 On table_1 alias_1 (column_1)".
"Create Index index_1 On table_1 alias_1".
"create index index_1 on table_1 alias_1".
"create index index_1 on table_1 filter_with'filter_string'".
"create index index_1 on table_1 norm_with'norm_string' filter_with'filter_string'".
"create index index_1 on table_1 norm_with'norm_string'".
"create index index_1 on table_1".
"Create Index index_1 On table_1".
"create index index_1 on table_1(column_1,column_2)".
"create index index_1 on table_1(column_1,column_2)filter_with'filter_string'".
"create index index_1 on table_1(column_1,column_2)norm_with'norm_string' filter_with'filter_string'".
"create index index_1 on table_1(column_1,column_2)norm_with'norm_string'".
"create index index_1 on table_1(column_1|:f()|,column_2|:f()|)".
"create index index_1 on table_1(column_1|:f()|,column_2|:f()|)filter_with'filter_string'".
"create index index_1 on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string' filter_with'filter_string'".
"create index index_1 on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string'".
"create index name_sort on skvhACCOUNT (cvalue|:NAME|) norm_with fun imem_index:vnf_lcase_ascii/1. filter_with fun imem_index:iff_binterm_list_1/1.".
"create index on :param_1 alias_1".
"create index on :param_1 filter_with'filter_string'".
"create index on :param_1 norm_with'norm_string' filter_with'filter_string'".
"create index on :param_1 norm_with'norm_string'".
"create index on :param_1".
"create index on \"%%__xxx__%%\" alias_1".
"create index on \"%%__xxx__%%\"".
"create index on \"table_string\" alias_1".
"create index on \"table_string\"".
"create index on schema_1.table_1 alias_1".
"create index on schema_1.table_1".
"create index on tab".
"create index on table_1 alias_1".
"create index on table_1 filter_with'filter_string'".
"create index on table_1 norm_with'norm_string' filter_with'filter_string'".
"create index on table_1 norm_with'norm_string'".
"create index on table_1".
"create index on table_1(column_1)".
"create index on table_1(column_1,column_2)".
"create index on table_1(column_1,column_2)filter_with'filter_string'".
"create index on table_1(column_1,column_2)norm_with'norm_string' filter_with'filter_string'".
"create index on table_1(column_1,column_2)norm_with'norm_string'".
"create index on table_1(column_1|:f()|)".
"create index on table_1(column_1|:f()|,column_2)".
"create index on table_1(column_1|:f()|,column_2|:f()|)".
"create index on table_1(column_1|:f()|,column_2|:f()|)filter_with'filter_string'".
"create index on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string' filter_with'filter_string'".
"create index on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string'".
"create index table_1.index_1 on schema_1.table_1 alias_1".
"create index table_1.index_1 on table_1 alias_1".
"create index table_1.index_1 on table_1".
"create keylist index index_1 on :param_1 alias_1".
"create keylist index index_1 on :param_1".
"create keylist index index_1 on \"%%__xxx__%%\" alias_1".
"create keylist index index_1 on \"%%__xxx__%%\"".
"create keylist index index_1 on schema_1.table_1 alias_1".
"create keylist index index_1 on table_1 alias_1".
"create keylist index index_1 on table_1".
"create keylist index on :param_1 alias_1".
"create keylist index on :param_1".
"create keylist index on \"%%__xxx__%%\" alias_1".
"create keylist index on \"%%__xxx__%%\"".
"create keylist index on schema_1.table_1 alias_1".
"create keylist index on table_1 alias_1".
"create keylist index on table_1".
"create keylist index s.a on s.d (f)".
"create keylist index table_1.index_1 on table_1".
"create unique index index_1 on :param_1 alias_1".
"create unique index index_1 on :param_1 filter_with'filter_string'".
"create unique index index_1 on :param_1 norm_with'norm_string' filter_with'filter_string'".
"create unique index index_1 on :param_1 norm_with'norm_string'".
"create unique index index_1 on :param_1".
"create unique index index_1 on \"%%__xxx__%%\" alias_1".
"create unique index index_1 on \"%%__xxx__%%\"".
"create unique index index_1 on schema_1.table_1 alias_1".
"create unique index index_1 on table_1 alias_1".
"create unique index index_1 on table_1 filter_with'filter_string'".
"create unique index index_1 on table_1 norm_with'norm_string' filter_with'filter_string'".
"create unique index index_1 on table_1 norm_with'norm_string'".
"create unique index index_1 on table_1".
"create unique index index_1 on table_1(column_1,column_2)filter_with'filter_string'".
"create unique index index_1 on table_1(column_1,column_2)norm_with'norm_string' filter_with'filter_string'".
"create unique index index_1 on table_1(column_1,column_2)norm_with'norm_string'".
"create unique index index_1 on table_1(column_1|:f()|,column_2|:f()|)filter_with'filter_string'".
"create unique index index_1 on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string' filter_with'filter_string'".
"create unique index index_1 on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string'".
"create unique index on :param_1 alias_1".
"create unique index on :param_1 filter_with'filter_string'".
"create unique index on :param_1 norm_with'norm_string' filter_with'filter_string'".
"create unique index on :param_1 norm_with'norm_string'".
"create unique index on :param_1".
"create unique index on \"%%__xxx__%%\" alias_1".
"create unique index on \"%%__xxx__%%\"".
"create unique index on schema_1.table_1 alias_1".
"create unique index on table_1 alias_1".
"create unique index on table_1 filter_with'filter_string'".
"create unique index on table_1 norm_with'norm_string' filter_with'filter_string'".
"create unique index on table_1 norm_with'norm_string'".
"create unique index on table_1".
"create unique index on table_1(column_1,column_2)".
"create unique index on table_1(column_1,column_2)filter_with'filter_string'".
"create unique index on table_1(column_1,column_2)norm_with'norm_string' filter_with'filter_string'".
"create unique index on table_1(column_1,column_2)norm_with'norm_string'".
"create unique index on table_1(column_1|:f()|,column_2|:f()|)".
"create unique index on table_1(column_1|:f()|,column_2|:f()|)filter_with'filter_string'".
"create unique index on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string' filter_with'filter_string'".
"create unique index on table_1(column_1|:f()|,column_2|:f()|)norm_with'norm_string'".
"create unique index s.a on s.d (f)".
"create unique index table_1.index_1 on table_1".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"create role role_1".
"create role role_1;create role role_2".
"create role role_1;extra_1".
"create role role_1;extra_1;create role role_2;extra_2".
"create role role_1;extra_1;create role role_2;extra_2;create role role_3;extra_3".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CLUSTER anything_random TABLE test()".
"CREATE CLUSTER anything_random TABLE test(a int)".
"CREATE CLUSTER SET TABLE test(fld CHAR)".
"CREATE imem_dal_skvh TABLE test()".
"CREATE LOCAL BAG TABLE test(fld CHAR)".
"CREATE LOCAL BAG TABLE test(fld CHAR,fld VARCHAR2(13)DEFAULT '123',fld BLOB(2000),fld INT DEFAULT 99999999999999999,fld FLOAT(-3)DEFAULT 123456,fld DECIMAL(10,3)DEFAULT 1.1234,fld DATE DEFAULT SYSDATE,fld DATETIME,fld TIMESTAMP,fld INT)".
"create local set table schema_1.table_1(column_1 char,column_2 number(10,2),column_3 varchar_2(10) not null primary key,column_4 lob not null default avg(column_1,column_2),column_5 lob not null default my_sum(column_1,column_2)|:b|,column_6 char(10) references table_2(column_3,column_4))".
"create local set table table_1(column_1 date)".
"create local set table table_1(column_1 date);test".
"CREATE LOCAL TABLE fun_test(fld TUPLE(0)default fun(_D)-> {} end.,fld BINARY(1000),fld atom,fld ipaddr(4)default fun()-> {0,0,0,0} end. ,fld LIST(0)default fun()-> [] end.,fld BINSTR(1000)default fun(C,D)-> <<\"no_value\">> end.,fld PID,fld ref,fld datetime default fun()-> calendar:local_time()end.,fld timestamp(3)default fun(A,B)-> erlang:now()end.,fld INTEGER(10,-3))".
"create local table table_1(column_1 date)".
"CREATE ORDERED_SET TABLE test(fld CHAR)".
"CREATE SCHEMA ORDERED_SET TABLE test(fld CHAR)".
"CREATE SCHEMA TABLE test(fld CHAR)".
"create set table table_1(column_1 date)".
"create set table table_1(column_1 number(1),column_2 number(2,7))".
"create set table table_1(column_1 number(1,8),column_2 number(2))".
"create table :param_1(column_1 date)".
"Create Table :param_1(column_1 date)".
"Create Table :param_1(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES :param_2(column_9))".
"Create Table :param_1(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES :param_2)".
"create table \"^&()\"(column_1 date)".
"Create Table \"^&()\"(column_1 date)".
"Create Table \"^&()_1\"(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES \"^&()_2\"(column_9))".
"Create Table \"^&()_1\"(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES \"^&()_2\")".
"CREATE TABLE hr_countries( country_id char(2)NOT NULL PRIMARY KEY,country_name varchar2(40),region_id integer)".
"CREATE TABLE hr_countries( country_id char(2)NOT NULL PRIMARY KEY,country_name varchar2(40),region_id integer,FOREIGN KEY(r_id1,r_id2,r_id3)REFERENCES hr_regions(id1,id2,id3))".
"CREATE TABLE hr_countries( country_id char(2)NOT NULL PRIMARY KEY,country_name varchar2(40),region_id integer,FOREIGN KEY(region_id)REFERENCES hr_regions(region_id))".
"CREATE TABLE hr_countries( country_id char(2)NOT NULL PRIMARY KEY,country_name varchar2(40),region_id integer,FOREIGN KEY(region_id)REFERENCES hr_regions)".
"CREATE TABLE hr_countries( country_id char(2)NOT NULL PRIMARY KEY,country_name varchar2(40),region_id integer,FOREIGN KEY(region_id1,region_id2,region_id3)REFERENCES hr_regions)".
"CREATE TABLE hr_job_history( employee_id number(6,0)NOT NULL,start_date date NOT NULL,end_date date NOT NULL,job_id varchar2(10)NOT NULL,department_id number(4,0));".
"CREATE TABLE hr_job_history( employee_id number(6,0)NOT NULL,start_date date NOT NULL,end_date date NOT NULL,job_id varchar2(10)NOT NULL,department_id number(4,0),CHECK(end_date > start_date));".
"CREATE TABLE hr_job_history( employee_id number(6,0)NOT NULL,start_date date NOT NULL,end_date date NOT NULL,job_id varchar2(10)NOT NULL,department_id number(4,0),PRIMARY KEY(employee_id,start_date));".
"create table key_test(col1 '{atom,integer}',col2 '{string,binstr}')".
"CREATE TABLE Persons(P_Id int,LastName varchar2,LastName varchar2(255),FirstName varchar2(255),Address varchar2(255),City varchar2(255),testfield varchar(10))".
"create table schema_1.table_1(column_1 date,column_2 date,foreign key(fkey_1,fkey_2) references schema_1.table_2(column_8,column_9))".
"create table schema_1.table_1(column_1 date,column_2 date,constraint constraint_1 foreign key(fkey_1,fkey_2) references schema_1.table_2(column_8,column_9))".
"create table schema_1.table_1(column_1 date,column_2 date,primary key(pkey_1,pkey_2))".
"create table schema_1.table_1(column_1 date,column_2 date,constraint constraint_1 primary key(pkey_1,pkey_2))".
"create table schema_1.table_1(column_1 date,column_2 date,unique(ukey_1,ukey_2))".
"create table schema_1.table_1(column_1 date,column_2 date,constraint constraint_1 unique(ukey_1,ukey_2))".
"create table schema_1.table_1(column_1 char(1),column_2 date,foreign key(column_1)references schema_2.table_2(column_3))".
"create table schema_1.table_1(column_1 char(1),column_2 date,foreign key(column_1)references schema_2.table_2(column_3),foreign key(column_1)references schema_2.table_3(column_3))".
"create table schema_1.table_1(column_1 char(1),column_2 date,foreign key(column_1)references schema_2.table_2)".
"create table schema_1.table_1(column_1 char(1),column_2 date,foreign key(column_1,column_2)references schema_2.table_2(column_3,column_4))".
"create table schema_1.table_1(column_1 char(1),column_2 date,foreign key(column_1,column_2)references schema_2.table_2)".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 foreign key(column_1)references schema_2.table_2(column_3))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 foreign key(column_1)references schema_2.table_2(column_3),constraint constraint_2 foreign key(column_1)references schema_2.table_3(column_3))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 foreign key(column_1)references schema_2.table_2)".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 foreign key(column_1,column_2)references schema_2.table_2(column_3,column_4))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 foreign key(column_1,column_2)references schema_2.table_2)".
"create table schema_1.table_1(column_1 char(1),column_2 date,primary key(column_1))".
"create table schema_1.table_1(column_1 char(1),column_2 date,primary key(column_1,column_2))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 primary key(column_1))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 primary key(column_1,column_2))".
"create table schema_1.table_1(column_1 char(1),column_2 date,unique(column_1))".
"create table schema_1.table_1(column_1 char(1),column_2 date,unique(column_1,column_2))".
"create table schema_1.table_1(column_1 char(1),column_2 date,unique(column_1,column_2),foreign key(column_1,column_2)references schema_2.table_2(column_3,column_4))".
"create table schema_1.table_1(column_1 char(1),column_2 date,unique(column_1,column_2),constraint constraint_1 foreign key(column_1,column_2)references schema_2.table_2(column_3,column_4))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 unique(column_1))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 unique(column_1,column_2))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 unique(column_1,column_2),foreign key(column_1,column_2)references schema_2.table_2(column_3,column_4))".
"create table schema_1.table_1(column_1 char(1),column_2 date,constraint constraint_1 unique(column_1,column_2),constraint constraint_1 foreign key(column_1,column_2)references schema_2.table_2(column_3,column_4))".
"create table schema_1.table_1(column_1 char)".
"Create Table schema_1.table_1(column_1 date)".
"Create Table schema_1.table_1(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES schema_1.table_2(column_9))".
"Create Table schema_1.table_1(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES schema_1.table_2)".
"create table table_1(column_1 date,column_2 date,foreign key(fkey_1,fkey_2) references table_2)".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 foreign key(fkey_1,fkey_2) references table_2)".
"create table table_1(check((1 or 4711)and(2 or 4712)))".
"create table table_1(check((4711)))".
"create table table_1(check(1 + 4711 * 2 + 4712))".
"create table table_1(check(1 + 4711 or 2 + 4712))".
"create table table_1(check(1 + 4711))".
"create table table_1(check(1 and 4711 or 2 and 4712))".
"create table table_1(check(1 and 4711))".
"create table table_1(check(1 or 4711 + 2 or 4712))".
"create table table_1(check(1 or 4711 and 2 or 4712))".
"create table table_1(check(1 or 4711))".
"create table table_1(check(4711 as test))".
"create table table_1(check(4711 test))".
"create table table_1(check(4711=4712))".
"create table table_1(check(4711||4712))".
"create table table_1(check(function_1((case when 1 then 2 end))))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 else 3 end)))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 else 4711 as wwe end)))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 else 4712 end)))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 else column_1 end)))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 else column_1=column_2 end)))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 end)))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(check(function_1(case 4711 as wwe when 1 then 2 when 3 then 4 end)))".
"create table table_1(check(function_1(case 4711 when 1 then 2 else 3 end)))".
"create table table_1(check(function_1(case 4711 when 1 then 2 else 4711 as wwe end)))".
"create table table_1(check(function_1(case 4711 when 1 then 2 else column_1=column_2 end)))".
"create table table_1(check(function_1(case 4711 when 1 then 2 end)))".
"create table table_1(check(function_1(case 4711 when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(check(function_1(case 4711 when 1 then 2 when 3 then 4 end)))".
"create table table_1(check(function_1(case column_1=column_2 when 1 then 2 else 3 end)))".
"create table table_1(check(function_1(case column_1=column_2 when 1 then 2 else 4711 as wwe end)))".
"create table table_1(check(function_1(case column_1=column_2 when 1 then 2 else 4711 end)))".
"create table table_1(check(function_1(case column_1=column_2 when 1 then 2 else column_3=column_4 end)))".
"create table table_1(check(function_1(case column_1=column_2 when 1 then 2 end)))".
"create table table_1(check(function_1(case column_1=column_2 when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(check(function_1(case column_1=column_2 when 1 then 2 when 3 then 4 end)))".
"create table table_1(check(function_1(case when 1 then 2 else 3 end)))".
"create table table_1(check(function_1(case when 1 then 2 else 4711 as wwe end)))".
"create table table_1(check(function_1(case when 1 then 2 else column_1=column_2 end)))".
"create table table_1(check(function_1(case when 1 then 2 end)))".
"create table table_1(check(function_1(case when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(check(function_1(case when 1 then 2 when 3 then 4 end)))".
"create table table_1(check(function_1(column_1)))".
"create table table_1(check(function_1(select * from dual)))".
"create table table_1(check(not 1 or 4711))".
"create table table_1(check(not 4711))".
"create table table_1(check(not(1 or 4711)))".
"create table table_1(constraint constraint_1 check((1 or 4711)and(2 or 4712)))".
"create table table_1(constraint constraint_1 check((4711)))".
"create table table_1(constraint constraint_1 check(1 + 4711 * 2 + 4712))".
"create table table_1(constraint constraint_1 check(1 + 4711 or 2 + 4712))".
"create table table_1(constraint constraint_1 check(1 + 4711))".
"create table table_1(constraint constraint_1 check(1 and 4711 or 2 and 4712))".
"create table table_1(constraint constraint_1 check(1 and 4711))".
"create table table_1(constraint constraint_1 check(1 or 4711 + 2 or 4712))".
"create table table_1(constraint constraint_1 check(1 or 4711 and 2 or 4712))".
"create table table_1(constraint constraint_1 check(1 or 4711))".
"create table table_1(constraint constraint_1 check(4711 as test))".
"create table table_1(constraint constraint_1 check(4711 test))".
"create table table_1(constraint constraint_1 check(4711=4712))".
"create table table_1(constraint constraint_1 check(4711||4712))".
"create table table_1(constraint constraint_1 check(function_1((case when 1 then 2 end))))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 else 3 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 else 4711 as wwe end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 else 4712 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 else column_1 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 else column_1=column_2 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 as wwe when 1 then 2 when 3 then 4 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 when 1 then 2 else 3 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 when 1 then 2 else 4711 as wwe end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 when 1 then 2 else column_1=column_2 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 when 1 then 2 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(constraint constraint_1 check(function_1(case 4711 when 1 then 2 when 3 then 4 end)))".
"create table table_1(constraint constraint_1 check(function_1(case column_1=column_2 when 1 then 2 else 3 end)))".
"create table table_1(constraint constraint_1 check(function_1(case column_1=column_2 when 1 then 2 else 4711 as wwe end)))".
"create table table_1(constraint constraint_1 check(function_1(case column_1=column_2 when 1 then 2 else 4711 end)))".
"create table table_1(constraint constraint_1 check(function_1(case column_1=column_2 when 1 then 2 else column_3=column_4 end)))".
"create table table_1(constraint constraint_1 check(function_1(case column_1=column_2 when 1 then 2 end)))".
"create table table_1(constraint constraint_1 check(function_1(case column_1=column_2 when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(constraint constraint_1 check(function_1(case column_1=column_2 when 1 then 2 when 3 then 4 end)))".
"create table table_1(constraint constraint_1 check(function_1(case when 1 then 2 else 3 end)))".
"create table table_1(constraint constraint_1 check(function_1(case when 1 then 2 else 4711 as wwe end)))".
"create table table_1(constraint constraint_1 check(function_1(case when 1 then 2 else column_1=column_2 end)))".
"create table table_1(constraint constraint_1 check(function_1(case when 1 then 2 end)))".
"create table table_1(constraint constraint_1 check(function_1(case when 1 then 2 when 3 then 4 else 5 end)))".
"create table table_1(constraint constraint_1 check(function_1(case when 1 then 2 when 3 then 4 end)))".
"create table table_1(constraint constraint_1 check(function_1(column_1)))".
"create table table_1(constraint constraint_1 check(function_1(select * from dual)))".
"create table table_1(constraint constraint_1 check(not 1 or 4711))".
"create table table_1(constraint constraint_1 check(not 4711))".
"create table table_1(constraint constraint_1 check(not(1 or 4711)))".
"create table table_1(column_1 char)".
"create table table_1(column_1 date references :param_1)".
"create table table_1(column_1 date references \"__..__\")".
"create table table_1(column_1 date references table_2)".
"create table table_1(column_1 date)".
"Create Table table_1(column_1 date)".
"create table table_1(column_1 date,column_2 char(10)references table_2(column_3,column_4))".
"create table table_1(column_1 date,column_2 char(10)references table_2)".
"create table table_1(column_1 date,column_2 date check('a'!=prior 'b'))".
"create table table_1(column_1 date,column_2 date check('a'!=prior :param_2))".
"create table table_1(column_1 date,column_2 date check('a*' between column_1 and :param_1))".
"create table table_1(column_1 date,column_2 date check('a*' is not null))".
"create table table_1(column_1 date,column_2 date check('a*' is null))".
"create table table_1(column_1 date,column_2 date check('a*' like column_1 escape :param_1))".
"create table table_1(column_1 date,column_2 date check('a*' like column_1))".
"create table table_1(column_1 date,column_2 date check('a*' not between column_1 and :param_1))".
"create table table_1(column_1 date,column_2 date check('a*' not like column_1 escape :param_1))".
"create table table_1(column_1 date,column_2 date check('a*' not like column_1))".
"create table table_1(column_1 date,column_2 date check(1 between column_1 and 1))".
"create table table_1(column_1 date,column_2 date check(1 is not null))".
"create table table_1(column_1 date,column_2 date check(1 is null))".
"create table table_1(column_1 date,column_2 date check(1 like column_1 escape 1))".
"create table table_1(column_1 date,column_2 date check(1 like column_1))".
"create table table_1(column_1 date,column_2 date check(1 not between column_1 and 1))".
"create table table_1(column_1 date,column_2 date check(1 not like column_1 escape 1))".
"create table table_1(column_1 date,column_2 date check(1 not like column_1))".
"create table table_1(column_1 date,column_2 date check(1=prior 2))".
"create table table_1(column_1 date,column_2 date check(1=prior :param_2))".
"create table table_1(column_1 date,column_2 date check(4711))".
"create table table_1(column_1 date,column_2 date check(7 in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(7 not in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(7<any select * from dual))".
"create table table_1(column_1 date,column_2 date check(7=all select * from dual))".
"create table table_1(column_1 date,column_2 date check(:param_1 between 1 and :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 between 1 and user))".
"create table table_1(column_1 date,column_2 date check(:param_1 between :param_2 and :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 between :param_2 and user))".
"create table table_1(column_1 date,column_2 date check(:param_1 in('a')))".
"create table table_1(column_1 date,column_2 date check(:param_1 in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(:param_1 is not null))".
"create table table_1(column_1 date,column_2 date check(:param_1 is null))".
"create table table_1(column_1 date,column_2 date check(:param_1 like 1 escape :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 like 1 escape user))".
"create table table_1(column_1 date,column_2 date check(:param_1 like 1))".
"create table table_1(column_1 date,column_2 date check(:param_1 like :param_2 escape :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 like :param_2 escape user))".
"create table table_1(column_1 date,column_2 date check(:param_1 like :param_2))".
"create table table_1(column_1 date,column_2 date check(:param_1 not between 1 and :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 not between 1 and user))".
"create table table_1(column_1 date,column_2 date check(:param_1 not between :param_2 and :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 not between :param_2 and user))".
"create table table_1(column_1 date,column_2 date check(:param_1 not in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(:param_1 not like 1 escape :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 not like 1 escape user))".
"create table table_1(column_1 date,column_2 date check(:param_1 not like 1))".
"create table table_1(column_1 date,column_2 date check(:param_1 not like :param_2 escape :param_3))".
"create table table_1(column_1 date,column_2 date check(:param_1 not like :param_2 escape user))".
"create table table_1(column_1 date,column_2 date check(:param_1 not like :param_2))".
"create table table_1(column_1 date,column_2 date check(:param_1))".
"create table table_1(column_1 date,column_2 date check(:param_1<>all select * from dual))".
"create table table_1(column_1 date,column_2 date check(:param_1<>prior :param_2))".
"create table table_1(column_1 date,column_2 date check(:param_1>= any select * from dual))".
"create table table_1(column_1 date,column_2 date check(column_1 between 'a*' and 'a'))".
"create table table_1(column_1 date,column_2 date check(column_1 between 1 and user))".
"create table table_1(column_1 date,column_2 date check(column_1 between :param_1 and :param_3))".
"create table table_1(column_1 date,column_2 date check(column_1 between :param_1 and user))".
"create table table_1(column_1 date,column_2 date check(column_1 between column_1 and user))".
"create table table_1(column_1 date,column_2 date check(column_1 in('a')))".
"create table table_1(column_1 date,column_2 date check(column_1 in('a','b','c')))".
"create table table_1(column_1 date,column_2 date check(column_1 in(1)))".
"create table table_1(column_1 date,column_2 date check(column_1 in(1,2,3)))".
"create table table_1(column_1 date,column_2 date check(column_1 in(:param_1)))".
"create table table_1(column_1 date,column_2 date check(column_1 in(:param_1,:param_2,:param_3)))".
"create table table_1(column_1 date,column_2 date check(column_1 in(column_1)))".
"create table table_1(column_1 date,column_2 date check(column_1 in(column_1,column_2,column_3)))".
"create table table_1(column_1 date,column_2 date check(column_1 in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(column_1 is not null))".
"create table table_1(column_1 date,column_2 date check(column_1 is null))".
"create table table_1(column_1 date,column_2 date check(column_1 like 'a*' escape 'a'))".
"create table table_1(column_1 date,column_2 date check(column_1 like 'a*'))".
"create table table_1(column_1 date,column_2 date check(column_1 like 1 escape user))".
"create table table_1(column_1 date,column_2 date check(column_1 like 1))".
"create table table_1(column_1 date,column_2 date check(column_1 like :param_1 escape :param_3))".
"create table table_1(column_1 date,column_2 date check(column_1 like :param_1 escape user))".
"create table table_1(column_1 date,column_2 date check(column_1 like :param_1))".
"create table table_1(column_1 date,column_2 date check(column_1 like column_1 escape user))".
"create table table_1(column_1 date,column_2 date check(column_1 like column_1))".
"create table table_1(column_1 date,column_2 date check(column_1 not in('a')))".
"create table table_1(column_1 date,column_2 date check(column_1 not in('a','b','c')))".
"create table table_1(column_1 date,column_2 date check(column_1 not in(1)))".
"create table table_1(column_1 date,column_2 date check(column_1 not in(1,2,3)))".
"create table table_1(column_1 date,column_2 date check(column_1 not in(:param_1)))".
"create table table_1(column_1 date,column_2 date check(column_1 not in(:param_1,:param_2,:param_3)))".
"create table table_1(column_1 date,column_2 date check(column_1 not in(column_1)))".
"create table table_1(column_1 date,column_2 date check(column_1 not in(column_1,column_2,column_3)))".
"create table table_1(column_1 date,column_2 date check(column_1 not in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(column_1 not like 'a*' escape 'a'))".
"create table table_1(column_1 date,column_2 date check(column_1 not like 'a*'))".
"create table table_1(column_1 date,column_2 date check(column_1 not like 1 escape user))".
"create table table_1(column_1 date,column_2 date check(column_1 not like 1))".
"create table table_1(column_1 date,column_2 date check(column_1 not like column_1 escape user))".
"create table table_1(column_1 date,column_2 date check(column_1 not like column_1))".
"create table table_1(column_1 date,column_2 date check(column_1(+)))".
"create table table_1(column_1 date,column_2 date check(column_1))".
"create table table_1(column_1 date,column_2 date check(column_1.*))".
"create table table_1(column_1 date,column_2 date check(column_1<= all select * from dual))".
"create table table_1(column_1 date,column_2 date check(column_1^=prior :param_2))".
"create table table_1(column_1 date,column_2 date check(column_1^=prior column_2))".
"create table table_1(column_1 date,column_2 date check(column_1^=some select * from dual))".
"create table table_1(column_1 date,column_2 date check(column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date check(exists select * from dual))".
"create table table_1(column_1 date,column_2 date check(exists(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(function_1((+column_2))))".
"create table table_1(column_1 date,column_2 date check(function_1((max)))check(function_1((sum))))".
"create table table_1(column_1 date,column_2 date check(function_1((sum))))".
"create table table_1(column_1 date,column_2 date check(function_1(+column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(-column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1 div column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1!=column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1*column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1+column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1-column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1/column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1<=column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1<>column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1<column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1=column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1>=column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1>column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1^=column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(column_1||column_2)))".
"create table table_1(column_1 date,column_2 date check(function_1(max as maximum)))".
"create table table_1(column_1 date,column_2 date check(function_1(max as maximum,sum as summe)))".
"create table table_1(column_1 date,column_2 date check(function_1(max)))".
"create table table_1(column_1 date,column_2 date check(function_1(max,sum)))".
"create table table_1(column_1 date,column_2 date check(function_1(null)))".
"create table table_1(column_1 date,column_2 date check(function_1(null,null)))".
"create table table_1(column_1 date,column_2 date check(function_1(wwe)))".
"create table table_1(column_1 date,column_2 date check(function_1(wwe1,wwe2)))".
"create table table_1(column_1 date,column_2 date check(max))".
"create table table_1(column_1 date,column_2 date check(max)check(sum))".
"create table table_1(column_1 date,column_2 date check(not exists select * from dual))".
"create table table_1(column_1 date,column_2 date check(not exists(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(null))".
"create table table_1(column_1 date,column_2 date check(package_1.function_1(column_1)))".
"create table table_1(column_1 date,column_2 date check(package_1.function_1(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date check(prior 'a'>'b'))".
"create table table_1(column_1 date,column_2 date check(prior 1<2))".
"create table table_1(column_1 date,column_2 date check(prior :param_1<2))".
"create table table_1(column_1 date,column_2 date check(prior :param_1<=column_2))".
"create table table_1(column_1 date,column_2 date check(prior :param_1>'b'))".
"create table table_1(column_1 date,column_2 date check(prior :param_1>=:param_2))".
"create table table_1(column_1 date,column_2 date check(prior column_1^=column_2))".
"create table table_1(column_1 date,column_2 date check(schema_1.table_1.column_1(+)))".
"create table table_1(column_1 date,column_2 date check(schema_1.table_1.column_1))".
"create table table_1(column_1 date,column_2 date check(schema_1.table_1.column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date check(sum(*)))".
"create table table_1(column_1 date,column_2 date check(sum(all 4711)))".
"create table table_1(column_1 date,column_2 date check(sum(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date check(sum(distinct column_1)))".
"create table table_1(column_1 date,column_2 date check(sum|:x:y|summe))".
"create table table_1(column_1 date,column_2 date check(table_1.column_1(+)))".
"create table table_1(column_1 date,column_2 date check(table_1.column_1))".
"create table table_1(column_1 date,column_2 date check(table_1.column_1.*))".
"create table table_1(column_1 date,column_2 date check(table_1.column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date check(user in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(user not in(select * from dual)))".
"create table table_1(column_1 date,column_2 date check(user!=any select * from dual))".
"create table table_1(column_1 date,column_2 date check(user))".
"create table table_1(column_1 date,column_2 date check(user>some select * from dual))".
"create table table_1(column_1 date,column_2 date,check(4711))".
"create table table_1(column_1 date,column_2 date,check(:param_1))".
"create table table_1(column_1 date,column_2 date,check(column_1(+)))".
"create table table_1(column_1 date,column_2 date,check(column_1))".
"create table table_1(column_1 date,column_2 date,check(column_1.*))".
"create table table_1(column_1 date,column_2 date,check(column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date,check(function_1((+column_2))))".
"create table table_1(column_1 date,column_2 date,check(function_1((max))),check(function_1((sum))))".
"create table table_1(column_1 date,column_2 date,check(function_1((sum))))".
"create table table_1(column_1 date,column_2 date,check(function_1(+column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(-column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1 div column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1!=column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1*column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1+column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1-column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1/column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1<=column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1<>column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1<column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1=column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1>=column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1>column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1^=column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1||column_2)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1||column_2*column_3)))".
"create table table_1(column_1 date,column_2 date,check(function_1(column_1||column_2*column_3/column_4)))".
"create table table_1(column_1 date,column_2 date,check(function_1(max as maximum)))".
"create table table_1(column_1 date,column_2 date,check(function_1(max as maximum,sum as summe)))".
"create table table_1(column_1 date,column_2 date,check(function_1(max)))".
"create table table_1(column_1 date,column_2 date,check(function_1(max,sum)))".
"create table table_1(column_1 date,column_2 date,check(function_1(null)))".
"create table table_1(column_1 date,column_2 date,check(function_1(null,null)))".
"create table table_1(column_1 date,column_2 date,check(function_1(wwe)))".
"create table table_1(column_1 date,column_2 date,check(function_1(wwe1,wwe2)))".
"create table table_1(column_1 date,column_2 date,check(max))".
"create table table_1(column_1 date,column_2 date,check(max),check(sum))".
"create table table_1(column_1 date,column_2 date,check(null))".
"create table table_1(column_1 date,column_2 date,check(package_1.function_1(column_1)))".
"create table table_1(column_1 date,column_2 date,check(package_1.function_1(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date,check(schema_1.table_1.column_1(+)))".
"create table table_1(column_1 date,column_2 date,check(schema_1.table_1.column_1))".
"create table table_1(column_1 date,column_2 date,check(schema_1.table_1.column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date,check(sum(*)))".
"create table table_1(column_1 date,column_2 date,check(sum(all 4711)))".
"create table table_1(column_1 date,column_2 date,check(sum(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date,check(sum(distinct column_1)))".
"create table table_1(column_1 date,column_2 date,check(sum|:x:y|summe))".
"create table table_1(column_1 date,column_2 date,check(table_1.column_1(+)))".
"create table table_1(column_1 date,column_2 date,check(table_1.column_1))".
"create table table_1(column_1 date,column_2 date,check(table_1.column_1.*))".
"create table table_1(column_1 date,column_2 date,check(table_1.column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date,check(user))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(4711))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(:param_1))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(column_1(+)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(column_1))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(column_1.*))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1((+column_2))))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1((max))),constraint constraint_1 check(function_1((sum))))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1((sum))))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(+column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(-column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1 div column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1!=column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1*column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1+column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1-column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1/column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1<=column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1<>column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1<column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1=column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1>=column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1>column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1^=column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1||column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1||column_2*column_3)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(column_1||column_2*column_3/column_4)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(max as maximum)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(max as maximum,sum as summe)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(max)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(max,sum)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(null)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(null,null)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(wwe)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(function_1(wwe1,wwe2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(max))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(max),constraint constraint_1 check(sum))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(null))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(package_1.function_1(column_1)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(package_1.function_1(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(schema_1.table_1.column_1(+)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(schema_1.table_1.column_1))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(schema_1.table_1.column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(sum(*)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(sum(all 4711)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(sum(column_1,column_2)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(sum(distinct column_1)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(sum|:x:y|summe))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(table_1.column_1(+)))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(table_1.column_1))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(table_1.column_1.*))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(table_1.column_1|:x:y|))".
"create table table_1(column_1 date,column_2 date,constraint constraint_1 check(user))".
"Create Table table_1(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES table_2(column_9))".
"Create Table table_1(column_1 date,column_2 date,FOREIGN KEY(fkey_1)REFERENCES table_2)".
"create table table_1(column_1 date,foreign key(column_1)references :param_1)".
"create table table_1(column_1 date,foreign key(column_1)references \"__..__\")".
"create table table_1(column_1 date,foreign key(column_1)references table_2)".
"create table table_1(column_1 date,constraint constraint_1 foreign key(column_1)references :param_1)".
"create table table_1(column_1 date,constraint constraint_1 foreign key(column_1)references \"__..__\")".
"create table table_1(column_1 date,constraint constraint_1 foreign key(column_1)references table_2)".
"create table table_1(column_1 number(1,2)not null default 0)".
"create table table_1(column_1 number(1,2)not null default date)".
"create table table_1(column_1 number(1,2)not null default function_1(column_1,column_2))".
"create table table_1(column_1 number(1,2)not null default null)".
"create table table_1(column_1 number(1,2)not null default package_1.function_1(column_1,column_2))".
"create table table_1(column_1 number(1,2)not null default schema_1.package_1.function_1(column_1,column_2))".
"create table table_1(column_1 number(1,2)not null default sum(*))".
"create table table_1(column_1 number(1,2)not null default sum(*)|:b|)".
"create table table_1(column_1 number(1,2)not null default sum(all column_1))".
"create table table_1(column_1 number(1,2)not null default sum(column_1,column_2))".
"create table table_1(column_1 number(1,2)not null default sum(distinct column_1))".
"create table table_1(column_1 number(1,2)not null default user)".
"create table table_1(column_1 number(1,2)not null default wwe)".
"create table table_1(column_1 number(1,2)not null primary key)".
"create table table_1(column_1 number(1,2)not null unique)".
"create table table_1(column_1 number(1,2)not null)".
"
create table table_1 (
                      owner userid,
                      private term,field_e date default fun()-> calendar:localtime()end.,
                      field_t decimal default fun mod:fun/1.,
                      field_t1 bool,field_t1 boolean,
                      field_e date1 default fun(C,D)-> C+D end.,
                      field_t1 number,
                      field_t1 number(1),
                      field_t1 number(1,2),
                      field_a atom default 'undefined',
                      field_b list,
                      \"field_c\" string default 'NULL',
                      \"field_d\" tuple default erl(\"{1,2}\")
                     )
".
"CREATE TABLE test(fld CHAR)".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 DEFAULT TABLESPACE table_2".
"CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 TEMPORARY TABLESPACE table_2".
"CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1".
"CREATE USER test_user_1 IDENTIFIED BY a_password".
"CREATE USER test_user_1 IDENTIFIED EXTERNALLY AS test_usr_2_extern TEMPORARY TABLESPACE table_1".
"CREATE USER test_user_2 IDENTIFIED EXTERNALLY AS test_usr_2_extern".
"CREATE USER test_user_3 IDENTIFIED EXTERNALLY PROFILE user_profile".
"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10 ON table_3 QUOTA 10M ON table_4 QUOTA UNLIMITED ON table_1".
"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_2".
"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_3 QUOTA UNLIMITED ON table_1".
"CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA UNLIMITED ON table_1".
"CREATE USER test_user_3 IDENTIFIED EXTERNALLY".
"CREATE USER test_user_4 IDENTIFIED GLOBALLY AS test_usr_2_extern".
"CREATE USER test_user_4 IDENTIFIED GLOBALLY".
"create user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3".
"create user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3;\"extra remark\"".
"create user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1 quota unlimited on name_1".
"create user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1".
"create user user_1 identified by name_1 default tablespace tablespace_1 profile profile_1;extra_1".
"create user user_1 identified by name_1 default tablespace tablespace_1".
"create user user_1 identified by name_1 default tablespace tablespace_1;\"extra remark\"".
"create user user_1 identified by name_1 default tablespace tablespace_1;extra_1".
"create user user_1 identified by name_1 profile profile_1".
"create user user_1 identified by name_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3".
"create user user_1 identified by name_1 quota 100 on name_1 quota 200 mb on name_2 quota unlimited on name_3;extra_1".
"create user user_1 identified by name_1 quota 1024 mb on name_1 quota 1024 on name_2 quota unlimited on name_3".
"create user user_1 identified by name_1 quota 1024 mb on name_1".
"create user user_1 identified by name_1 quota 1024 on name_1".
"create user user_1 identified by name_1 quota unlimited on name_1".
"create user user_1 identified by name_1 temporary tablespace tablespace_1".
"create user user_1 identified by name_1".
"create user user_1 identified by name_1;\"user remark\";create role role_1;\"role remark\"".
"create user user_1 identified by name_1;extra_1".
"create user user_1 identified by name_1;extra_1;create user user_2 identified by name_2;extra_2".
"create user user_1 identified externally as name_1".
"create user user_1 identified externally".
"create user user_1 identified globally as name_1".
"create user user_1 identified globally".

%% -----------------------------------------------------------------------------
%% TESTS: CREATE INDEX & CREATE ROLE & CREATE TABLE & CREATE USER
%% =============================================================================
