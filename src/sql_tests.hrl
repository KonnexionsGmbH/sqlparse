%% -*- coding: utf-8 -*-
-ifdef(TEST).

-define (TEST_SELECT,[
%<<"SELECT * from tab1 INNER JOIN tab2 tab3 tab4 on a = b">>, %% INVALID - negative test
%<<"select * from ALL_USERS where order by user_id asc">>, %% INVALID
%<<"select a < 0 from abc">>, %% SUPPORT IN PROGRESS
<<"select 'öüäéèà', 'شلاؤيثبلتهتنمةىخ','นี่คือการทดสอบ' from dual"/utf8>>,
<<"sElect
    /*+004*/
    distinct
    - a.b,
    a || fun(a,b) d,
    fun(a,b) || e as d,
    (CASE WHEN CNT < 10 and (c = d or e = f) THEN '1' ELSE 2 END),
    m.\"'$_'\" + 1 d,
    \"'$_'\".m + 2 as e,
    'aa' || 'b\nb' || \"c\r\nc\" || USERNAME || (A + account) || a.fun('a' || fld3 || fun('a' || fld3)),
    fun1(arg3) \"alias2\",
    fun(arg1-arg2, a.b.fun1(arg3)) \"alias1\",
    (SELECT c FROM d UNION SELECT id FROM b),
    NVL((SELECT * FROM tableinner),'some string') alias
   FROM account@1234@_no#de:@nohost,
        account,
        schm.account INNER JOIN tab2 on a = b INNER JOIN tab3 on c = d,
        tab1 PARTITION BY col1 NATURAL FULL outer join tab2 PARTITION BY col2 using (a),
        tab1 INNER JOIN tab2 using (a, b, c),
        tab1 NATURAL INNER JOIN tab2,
        tab1 PARTITION BY (a, b) right outer join times on (a = b and c = d),
        tab2 CROSS JOIN a,
        tab2 NATURAL JOIN a,
        tab2 LEFT OUTER JOIN a ON b.c = e.f,
        tab2 RIGHT OUTER JOIN a ON b.c = e.f,
        tab2 FULL OUTER JOIN a ON b.c = e.f,
        tab2 alias INNER JOIN a alias1 ON b.c = c.d
   WHERE field1 IN (to_timestamp('2001-05-23 00:56:00.000000'))
         and employee_id = :EMPNO
         or a = \"üèéöäà\"
         and f2 Like fun(arg1) || '%'
         and column(+) = 1
         or (field2 = field4 || fun('a' || fld3 || fun('a' || fld3)) and 's' || fld2 = 'd' || fld4)
         or not (username Like '%YS' ESCAPE 'abcd')
         and g between h and i
         or not a between b and c
         and s > sysdate -1.1574074074074073e-4
   START WITH a = 'd' CONNECT BY PRIOR a = d
   GROUP BY f
   HAVING SUM(d) < 2
          and a = abs(abs(1-2+(3)))
          or (((a = a1 or b = b1)) and (((w=(3)))))
   ORDER BY fun2(arg1) desc, 2 + A*B asc
UNION
  SELECT /*+002*/ c FROM d where regexp_like(a, '([aeiou])\1', 'i')
INTERSECT
	SELECT erl(\"['a',b]\"), erl(\"{field_a,b}\") FROM f
MINUS
	SELECT * FROM h
UNION ALL
	SELECT i FROM j
">>
]).

-define (TEST_UPDATE,[
                                        "UPDATE employees set salary = :sal where employee_id = :id",
                                        "UPDATE employees set salary = :sal where employee_id = :id RETURNING c,d INTO :c, :d",
                                        "UPDATE employees set salary = :sal where employee_id = :id RETURNING lob_column INTO :out_locator",
                                        "ALTER USER test_user_123 IDENTIFIED BY new_password",
%% UNSUPPORTED
%                                        "ALTER USER test_user_123 ACCOUNT LOCK",
%                                        "ALTER USER test_user_123 ACCOUNT UNLOCK",
%                                        "ALTER USER test_user_123 PASSWORD EXPIRE",
                                        "UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a is NULL",
                                        "UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a || b = 'c' || 'd'"
]).

-define (TEST_DELETE,[
                                        "DELETE FROM table_name WHERE some_column=some_value",
                                        "DELETE FROM table_name WHERE some_column=some_value RETURNING c,d INTO :c, :d",
                                        "DELETE FROM table_name WHERE some_column=some_value RETURNING lob_column INTO :out_locator",
                                        "DROP USER test_user_123",
                                        "DROP USER test_user_123 CASCADE",
                                        "DROP TABLE table_name",
                                        "DROP TABLE IF EXISTS table_name RESTRICT",
                                        "drop function funny",
                                        "drop function schm.funny",
                                        "drop procedure proc",
                                        "drop procedure schem.proc"
]).

-define (TEST_CREATE, [
%% UNSUPPORTED
%                                        "create or replace procedure
%                                         proc(p_cur out sys_refcursor)
%                                         is
%                                         begin
%                                            open p_cur for select * from test;
%                                         end proc",
                                        "create table key_test (col1 '{atom,integer}', col2 '{string,binstr}')",
                                        "CREATE TABLE Persons
                                        (
                                        P_Id int,
                                        LastName varchar2,
                                        LastName varchar2(255),
                                        FirstName varchar2(255),
                                        Address varchar2(255),
                                        City varchar2(255)
                                        )",
                                        "create table table_1 (
                                                       owner userid,
                                                       private term,
                                                       field_t decimal,
                                                       field_t1 bool,
                                                       field_t1 boolean,
                                                       field_t1 number,
                                                       field_t1 number(1),
                                                       field_t1 number(1,2),
                                                       field_a atom default 'undefined',
                                                       field_b list,
                                                       'field_c' string default 'NULL',
                                                       'field_d' tuple default erl(\"{1,2}\"),
                                                       field_e date default fun()-> calendar:localtime() end.
                                                       )",
                                        "CREATE LOCAL BAG TABLE test (fld CHAR)",
                                        "CREATE CLUSTER SET TABLE test (fld CHAR)",
                                        "CREATE SCHEMA ORDERED_SET TABLE test (fld CHAR)",
                                        "CREATE ORDERED_SET TABLE test (fld CHAR)",
                                        "CREATE SCHEMA TABLE test (fld CHAR)",
                                        "CREATE TABLE test (fld CHAR)",
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
                                        )",
                                        "CREATE LOCAL TABLE test
                                        (
                                        fld TUPLE(0) default fun() -> {} end.
                                        , fld BINARY(1000)
                                        , fld atom
                                        , fld ipaddr(4) default fun() -> {0,0,0,0} end. 
                                        , fld LIST(0) default fun() -> [] end.
                                        , fld BINSTR(1000) default fun() -> <<\"no_value\">> end.
                                        , fld PID
                                        , fld ref
                                        , fld FUN(1)
                                        , fld datetime default fun() -> calendar:local_time() end.
                                        , fld timestamp(3) default fun() -> erlang:now() end.
                                        , fld INTEGER(10,-3)
                                        )",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY",
                                        "CREATE USER test_user_2 IDENTIFIED EXTERNALLY AS test_usr_2_extern",
                                        "CREATE USER test_user_4 IDENTIFIED GLOBALLY",
                                        "CREATE USER test_user_4 IDENTIFIED GLOBALLY AS test_usr_2_extern",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 DEFAULT TABLESPACE table_2",
                                        "CREATE USER test_user_1 IDENTIFIED EXTERNALLY AS test_usr_2_extern TEMPORARY TABLESPACE table_1",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 TEMPORARY TABLESPACE table_2",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY PROFILE user_profile",
%% UNSUPPORTED
%                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY PASSWORD EXPIRE",
%                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT LOCK",
%                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT UNLOCK",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA UNLIMITED ON table_1",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_2",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_3 QUOTA UNLIMITED ON table_1",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10 ON table_3 QUOTA 10M ON table_4 QUOTA UNLIMITED ON table_1"
]).

-define (TEST_INSERT,[
                                        "INSERT INTO some_table VALUES (:a, :b) RETURNING c,d INTO :c, :d",
                                        "INSERT INTO some_table VALUES (:in_locator) RETURNING lob_column INTO :out_locator",
                                        "insert into number (float,integer) values ('C', \"undefined\")",
                                        "insert into def (col1,col2) values ('C', \"undefined\")",
                                        "insert into def (col1,col2) values ('C', 5+1)",
                                        "insert into table_1 (field_a, field_b) values ('first','Stefan''s choice.')",
                                        "insert into table_1 (field_a, field_c) values ('second','Double quote \" in string')",
                                        "insert into table_1 (field_a, field_c) values ('second','Single quote '' in string')",
                                        "insert into table_1 (field_a, field_d) values ('third',erl(\"{a,b,c}\"))",
                                        "insert into table_1 (field_a, field_3) values ('third','31.12.2012 23:59:59')",
                                        "insert into abc values (1, 'a', 'b', 'c' || 'd')",
                                        "INSERT INTO Persons VALUES (4,'Nilsen', 'Johan', 'Bakken 2', 'Stavanger')"
]).

-define (TEST_TRUNCT,[
                                        "truncate table tbl",
                                        "truncate table tbl",
                                        "truncate table tbl preserve materialized view log",
                                        "truncate table tbl purge materialized view log",
                                        "truncate table tbl drop storage",
                                        "truncate table tbl reuse storage",
                                        "truncate table tbl purge materialized view log drop storage",
                                        "truncate table tbl purge materialized view log drop storage"
]).

-define (TEST_GRANTS,[
										"GRANT manage_system TO user_1",
										"GRANT a,b,c TO user2",
										"GRANT SELECT ON ddTable TO user_1",
										"GRANT SELECT ON schema1.ddTable TO user_1",
										"GRANT ALL ON ddTable TO user1,user2",
										"GRANT EXECUTE ON module1 TO user1",
										"GRANT all privileges ON schema1.ddTable TO role2",
										"grant update, delete on ddTable to test_user_1",
										"grant insert on ddTable to test_user_1 WITH GRANT OPTION",
                                        "GRANT manage_system TO test_user_1 with admin option"
]).

-define (TEST_REVOKE,[
										"REVOKE manage_system FROM admin",
										"REVOKE a,b,c FROM user1,user2",
										"REVOKE SELECT ON ddTable FROM user_1",
										"REVOKE ALL ON schema1.ddTable FROM user1,user2",
										"REVOKE EXECUTE ON module1 FROM user1",
										"revoke update, delete on ddTable from test_user_1"
]).

-define (TEST_PLSQLS,[
										"declare begin schm.proc(:p_first,:p_second,:p_result); end",
										"declare begin proc(:p_first,:p_second,:p_result); end",
										"call proc(:p_first,:p_second,:p_result)",
										"call schm.proc(:p_first,:p_second,:p_result)",
										"begin schm.proc(:p_first,:p_second,:p_result); end",
										"begin proc(:p_first,:p_second,:p_result); end"
]).

-define(TEST_SQLS, [
      {"SELECT", ?TEST_SELECT, -1} % 1
    , {"INSERT", ?TEST_INSERT, -1} % 2
    , {"CREATE", ?TEST_CREATE, -1} % 3
    , {"UPDATE", ?TEST_UPDATE, -1} % 4
    , {"DELETE", ?TEST_DELETE, -1} % 5
    , {"TRUNCT", ?TEST_TRUNCT, -1} % 6
    , {"GRANTS", ?TEST_GRANTS, -1} % 7
    , {"REVOKE", ?TEST_REVOKE, -1} % 8
    , {"PLSQLS", ?TEST_PLSQLS, -1} % 8
]).
-endif.
