%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

% Negative tests (no support from framework)
%<<"SELECT * from tab1 INNER JOIN tab2 tab3 tab4 on a = b">>.
%<<"select * from ALL_USERS where order by user_id asc">>.

% Boolean expression in select list not supported
%<<"select a < 0 from abc">>.

<<"select 'öüäéèà', 'شلاؤيثبلتهتنمةىخ','นี่คือการทดสอบ' from dual"/utf8>>.

<<"sElect
    /*+004*/
    distinct
    - a.b,
    fun(a || b, c), fun2(e, a || b), fun2(d, a || b, c),
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
         and :abc is null
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
   ORDER BY fun2(arg1) desc, 2 + A*B asc, P || '(' || R || ')'
UNION
  SELECT /*+002*/ c FROM d where regexp_like(a, '([aeiou])\1', 'i')
INTERSECT
	SELECT erl(\"['a',b]\"), erl(\"{field_a,b}\") FROM f
MINUS
	SELECT * FROM h
UNION ALL
	SELECT i FROM j
"/utf8>>.
