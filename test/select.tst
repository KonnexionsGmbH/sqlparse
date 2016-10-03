%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS
%%

% Negative tests (no support from framework)
%<<"SELECT * from tab1 INNER JOIN tab2 tab3 tab4 on a = b">>.
%<<"select * from ALL_USERS where order by user_id asc">>.

"select * from :t".
"select * from :t tst1".
"select * from :t as tst2".

<<"select a < 0, f(a+2 as d), f2(a < 3 as f) from abc">>.

<<"select 'öüäéèà', 'شلاؤيثبلتهتنمةىخ','นี่คือการทดสอบ' from dual"/utf8>>.

<<"sElect
    /*+004*/
    distinct
    - a.b,
    fun(a || b, c), fun2(e, a || b), fun2(d, a || b, c),
    a || fun(a,b) d,
    REPLACE(REPLACE(MAX(a),'s',''),'q','') || e as d,
    (CASE WHEN CNT < 10 THEN '1' WHEN c = d or e = f THEN '2' ELSE 2 END) as t1,
    CASE a WHEN CNT < 10 and (c = d or e = f) THEN '1' WHEN 100 THEN '2' ELSE 2 END as t2,
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

<<"select * from tab t, (select * from tab) sql">>.

% minimum ----------------------------------------------------------------------

"SELECT * FROM name_table".
"SELECT name_column_1 FROM name_table".
"SELECT name_column_1, name_column_2 FROM name_table".

% hint, all, distinct ----------------------------------------------------------

"SELECT /*hint*/ * FROM name_table".
"SELECT /*hint*/ name_column_1 FROM name_table".
"SELECT /*hint*/ name_column_1, name_column_2 FROM name_table".

"SELECT /*hint*/ ALL * FROM name_table".
"SELECT /*hint*/ ALL name_column_1 FROM name_table".
"SELECT /*hint*/ ALL name_column_1, name_column_2 FROM name_table".

"SELECT /*hint*/ DISTINCT * FROM name_table".
"SELECT /*hint*/ DISTINCT name_column_1 FROM name_table".
"SELECT /*hint*/ DISTINCT name_column_1, name_column_2 FROM name_table".

"SELECT ALL * FROM name_table".
"SELECT ALL name_column_1 FROM name_table".
"SELECT ALL name_column_1, name_column_2 FROM name_table".

"SELECT DISTINCT * FROM name_table".
"SELECT DISTINCT name_column_1 FROM name_table".
"SELECT DISTINCT name_column_1, name_column_2 FROM name_table".

% into -------------------------------------------------------------------------

"SELECT * INTO name_variable_1 FROM name_table".
"SELECT name_column_1 INTO name_variable_1 FROM name_table".
"SELECT name_column_1, name_column_2 INTO name_variable_1, name_variable_2 FROM name_table".

% join -------------------------------------------------------------------------

"SELECT DISTINCT * FROM name_table_1, name_table_2".

% inner join -------------------------------------------------------------------

"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 USING (name_column_1, name_column_2)".

"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) USING (name_column_1, name_column_2)".


"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 name_alias ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 name_alias USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 AS name_alias ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 AS name_alias USING (name_column_1)".

"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 USING (name_column_1, name_column_2)".

% outer join -------------------------------------------------------------------

"SELECT DISTINCT * FROM name_table_1 FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 FULL OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 LEFT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 LEFT OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 RIGHT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 RIGHT OUTER JOIN name_table_2".

"SELECT DISTINCT * FROM name_table_1 FULL JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 FULL OUTER JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 LEFT JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 LEFT OUTER JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 RIGHT JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 RIGHT OUTER JOIN (SELECT * FROM name_table_2)".

"SELECT DISTINCT * FROM name_table_1 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL FULL OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL LEFT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL LEFT OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL RIGHT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL RIGHT OUTER JOIN name_table_2".

"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1, 2 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY (1) NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY (1, 2) NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY (1), 2 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1, (2) NATURAL FULL JOIN name_table_2".

"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 AS name_alias".

"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 name_alias ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 AS name_alias ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 name_alias ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 AS name_alias ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".

"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 name_alias USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 AS name_alias USING (name_column_1, name_column_2)".

% where ------------------------------------------------------------------------

"SELECT * FROM name_table WHERE name_column_1 = name_column_2".
"SELECT * FROM name_table WHERE name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT * FROM name_table WHERE name_column_1 = name_column_2 OR name_column_3 = name_column_4".
"SELECT * FROM name_table WHERE NOT name_column_1 = name_column_2".

"SELECT * FROM name_table WHERE (name_column_1 = name_column_2)".
"SELECT * FROM name_table WHERE ((name_column_1 = name_column_2) AND (name_column_3 = name_column_4))".
"SELECT * FROM name_table WHERE ((name_column_1 = name_column_2) OR (name_column_3 = name_column_4))".
"SELECT * FROM name_table WHERE (NOT (name_column_1 = name_column_2))".

% predicate comparison ---------------------------------------------------------

"SELECT * FROM name_table WHERE name_column_1 = 50".
"SELECT * FROM name_table WHERE PRIOR name_column_1 = 50".
"SELECT * FROM name_table WHERE name_column_1 = PRIOR 50".

% predicate between ------------------------------------------------------------

"SELECT * FROM name_table WHERE name_column_1 BETWEEN 50 AND 100".
"SELECT * FROM name_table WHERE name_column_1 BETWEEN name_column_2 AND name_column_3".
"SELECT * FROM name_table WHERE name_column_1 NOT BETWEEN 50 AND 100".
"SELECT * FROM name_table WHERE name_column_1 NOT BETWEEN name_column_2 AND name_column_3".

% predicate like ---------------------------------------------------------------

"SELECT * FROM name_table WHERE name_column_1 LIKE 50".
"SELECT * FROM name_table WHERE name_column_1 LIKE name_column_2".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE 50".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE name_column_2".

"SELECT * FROM name_table WHERE name_column_1 LIKE 50 ESCAPE 'name_atom'".
"SELECT * FROM name_table WHERE name_column_1 LIKE name_column_2 ESCAPE 'name_atom'".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE 50 ESCAPE 'Name_atom'".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE name_column_2 ESCAPE 'name_atom'".

% predicate null ---------------------------------------------------------------

"SELECT * FROM name_table WHERE name_column_1 IS NULL".
"SELECT * FROM name_table WHERE name_column_1 IS NOT NULL".

% predicate in -----------------------------------------------------------------

"SELECT * FROM name_table WHERE name_column_1 IN name_expr_1".
"SELECT * FROM name_table WHERE name_column_1 IN name_expr_1, name_expr_1".

"SELECT * FROM name_table WHERE name_column_1 IN (name_expr_1)".
"SELECT * FROM name_table WHERE name_column_1 IN (name_expr_1, name_expr_1)".

"SELECT * FROM name_table WHERE name_column_1 NOT IN name_expr_1".
% ? "SELECT * FROM name_table WHERE name_column_1 NOT IN name_expr_1, name_expr_1".

"SELECT * FROM name_table WHERE name_column_1 NOT IN (name_expr_1)".
"SELECT * FROM name_table WHERE name_column_1 NOT IN (name_expr_1, name_expr_1)".

"SELECT * FROM name_table_1 WHERE name_column_1 IN (SELECT * FROM name_table_2)".

% predicate all / any / some ---------------------------------------------------

"SELECT * FROM name_table_1 WHERE name_column_1 > ALL (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE name_column_1 > ANY (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE name_column_1 > SOME (SELECT * FROM name_table_2)".

% predicate exists -------------------------------------------------------------

"SELECT * FROM name_table_1 WHERE EXISTS (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE NOT EXISTS (SELECT * FROM name_table_2)".

% hierarchical query clause ----------------------------------------------------

"SELECT * FROM name_table_1 START WITH name_column_1 IS NULL CONNECT BY name_column_2 = name_column_3".
"SELECT * FROM name_table_1 START WITH name_column_1 IS NULL CONNECT BY NOCYCLE name_column_2 = name_column_3".
"SELECT * FROM name_table_1 CONNECT BY name_column_2 = name_column_3 START WITH name_column_1 IS NULL".
"SELECT * FROM name_table_1 CONNECT BY NOCYCLE name_column_2 = name_column_3 START WITH name_column_1 IS NULL".

% group by ---------------------------------------------------------------------

"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4, name_column_5".

% having -----------------------------------------------------------------------

"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 HAVING name_column_5 = name_column_6".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4 HAVING name_column_5 = name_column_6".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 HAVING name_column_5 = name_column_6".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4, name_column_5 HAVING name_column_5 = name_column_6".

% order by ---------------------------------------------------------------------

"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 ASC".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 DESC".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4, name_column_5".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 ASC, name_column_5".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 ASC, name_column_5 DESC".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 DESC, name_column_5 ASC".

% aggregate functions ----------------------------------------------------------

"SELECT AVG(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT AVG(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT AVG(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT CORR(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(*) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COVAR_POP(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COVAR_SAMP(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MAX(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MAX(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MAX(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MEDIAN(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MIN(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MIN(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MIN(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_AVGX(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_AVGY(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_COUNT(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_INTERCEPT(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_R2(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SLOPE(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SXX(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SXY(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SYY(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV_POP(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV_SAMP(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT SUM(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT SUM(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT SUM(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VAR_POP(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VAR_SAMPLE(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VARIANCE(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VARIANCE(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VARIANCE(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".

"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 AND COUNT(name_column_3)".

"SELECT column_1,column_2,COUNT(column_3) 
   FROM name_table_1 
  WHERE name_column_1 = name_column_2 
  GROUP BY column_1,column_2,REGR_COUNT(name_column_3,name_column_4)".

"SELECT column_1,column_2,COUNT(column_3) 
   FROM name_table_1 
  WHERE name_column_1 = name_column_2 
  GROUP BY column_1,column_2,column_3 
 HAVING COUNT(name_column_3) > 5".

"SELECT column_1,column_2,COUNT(column_3) 
   FROM name_table_1 
  WHERE name_column_1 = name_column_2 
  ORDER BY column_1,column_2,REGR_COUNT(name_column_3,name_column_4)".
