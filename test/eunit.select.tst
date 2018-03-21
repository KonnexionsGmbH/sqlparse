%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: SELECT
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fields
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select 'my_test' alias_1 from dual".
"select 'my_test' from dual".
"select 'wwe' from dual".
"select ('my_test') from dual".
"select ('my_test')|:b| from dual".
"select ('wwe') from dual".
"select ('wwe')|:b| from dual".
"select (select column_1 from table_1) from dual".
"select (select column_1 from table_1)|:b| from dual".
"select (+ column_2) from dual".
"select (+ column_2)|:b| from dual".
"select (- column_2) from dual".
"select (- column_2)|:b| from dual".
"select (4711) from dual".
"select (4711)|:b| from dual".
"select (5.3) from dual".
"select (5.3)|:b| from dual".
"select (:param_1) from dual".
"select (:param_1)|:b| from dual".
"select (case 1 when 2 then 3 else 4 end) from dual".
"select (case column_1 when 2 then 3 else 4 end) from dual".
"select (column_1 * column_2) from dual".
"select (column_1 * column_2)|:b| from dual".
"select (column_1 + column_2) from dual".
"select (column_1 + column_2)|:b| from dual".
"select (column_1 - column_2) from dual".
"select (column_1 - column_2)|:b| from dual".
"select (column_1 / column_2) from dual".
"select (column_1 / column_2)|:b| from dual".
"select (column_1 div column_2) from dual".
"select (column_1 div column_2)|:b| from dual".
"select (column_1(+)) from dual".
"select (column_1(+))|:b| from dual".
"select (column_1) from dual".
"select (column_1)|:b| from dual".
"select (max(*)) from dual".
"select (max(*))|:b| from dual".
"select (max(all :param_1)) from dual".
"select (max(all column_1)) from dual".
"select (max(all column_1))|:b| from dual".
"select (max(distinct column_1)) from dual".
"select (max(distinct column_1))|:b| from dual".
"select (max) from dual".
"select (max)|:b| from dual".
"select (nullx) from dual".
"select (nullx)|:b| from dual".
"select (schema_1.table_1.*) from dual".
"select (schema_1.table_1.*)|:b| from dual".
"select (schema_1.table_1.column_1(+)) from dual".
"select (schema_1.table_1.column_1(+))|:b| from dual".
"select (schema_1.table_1.column_1) from dual".
"select (schema_1.table_1.column_1)|:b| from dual".
"select (select column_1 from table_1) alias_1 from dual".
"select (select column_1 from table_1) alias_1,(select column_2 from table_2) alias_2,(select column_3 from table_3) alias_3 from dual".
"select (select column_1 from table_1) from dual".
"select (select column_1 from table_1),(select column_2 from table_2),(select column_3 from table_3) from dual".
"select (table_1.*) from dual".
"select (table_1.*)|:b| from dual".
"select (table_1.column_1(+)) from dual".
"select (table_1.column_1(+))|:b| from dual".
"select (table_1.column_1) from dual".
"select (table_1.column_1)|:b| from dual".
"select (wwe) from dual".
"select (wwe)|:b| from dual".
"select * from dual".
"select + column_2 alias_1 from dual".
"select + column_2 from dual".
"select - column_2 alias_1 from dual".
"select - column_2 from dual".
"select 4711 alias_1 from dual".
"select 4711 from dual".
"select 5.3 alias_1 from dual".
"select 5.3 from dual".
"select :param_1 alias_1 from dual".
"select :param_1 alias_1,:param_2 alias_2,:param_3 alias_3 from dual".
"select :param_1 from dual".
"select :param_1,:param_2,:param_3 from dual".
"select case 1 when 2 then 3 else 4 end alias_1 from dual".
"select case 1 when 2 then 3 else 4 end from dual".
"select case 11 when 12 then 13 else 14 end alias_1,case 21 when 22 then 23 else 24 end alias_2,case 31 when 32 then 33 else 34 alias_3 end from dual".
"select case 11 when 12 then 13 else 14 end,case 21 when 22 then 23 else 24 end,case 31 when 32 then 33 else 34 end from dual".
"select column_1 * column_2 alias_1 from dual".
"select column_1 * column_2 from dual".
"select column_1 + column_2 alias_1 from dual".
"select column_1 + column_2 from dual".
"select column_1 - column_2 alias_1 from dual".
"select column_1 - column_2 from dual".
"select column_1 / column_2 alias_1 from dual".
"select column_1 / column_2 from dual".
"select column_1 <= column_2 from dual".
"select column_1 = column_2 from dual".
"select column_1 alias_1 from dual".
"select column_1 alias_1,column_2 alias_2,column_3 alias_3 from dual".
"select column_1 div column_2 alias_1 from dual".
"select column_1 div column_2 from dual".
"select column_1 from dual".
"select column_1 || column_2 alias_1 from dual".
"select column_1 || column_2 from dual".
"select column_1(+) from dual".
"select column_1,column_2,column_3 from dual".
"select column_1|:b| from dual".
"select function_1(:param_1,:param_2) from dual".
"select function_1(:param_1,:param_2)|:b| from dual".
"select max from dual".
"select max(*) from dual".
"select max(*)|:b| from dual".
"select max(:param_1,:param_2) from dual".
"select max(:param_1,:param_2)|:b| from dual".
"select max(all column_1) from dual".
"select max(all column_1)|:b| from dual".
"select max(distinct column_1) from dual".
"select max(distinct column_1)|:b| from dual".
"select max|:b| from dual".
"select nullx alias_1 from dual".
"select nullx from dual".
"select package_1.function_1(:param_1,:param_2) from dual".
"select package_1.function_1(:param_1,:param_2)|:b| from dual".
"select schema_1.package_1.function_1(:param_1,:param_2) from dual".
"select schema_1.package_1.function_1(:param_1,:param_2)|:b| from dual".
"select schema_1.table_1.* from dual".
"select schema_1.table_1.column_1 from dual".
"select schema_1.table_1.column_1(+) from dual".
"select schema_1.table_1.column_1|:b| from dual".
"select table_1.* from dual".
"select table_1.column_1 from dual".
"select table_1.column_1(+) from dual".
"select table_1.column_1|:b| from dual".
"select wwe from dual".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hint & fields
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select /* my_hint */ (select column_1 from table_1) alias_1 from dual".
"select /* my_hint */ (select column_1 from table_1) alias_1,(select column_2 from table_2) alias_2,(select column_3 from table_3) alias_3 from dual".
"select /* my_hint */ (select column_1 from table_1) from dual".
"select /* my_hint */ (select column_1 from table_1),(select column_2 from table_2),(select column_3 from table_3) from dual".
"select /* my_hint */ * from dual".
"select /* my_hint */ :param_1 alias_1 from dual".
"select /* my_hint */ :param_1 alias_1,:param_2 alias_2,:param_3 alias_3 from dual".
"select /* my_hint */ :param_1 from dual".
"select /* my_hint */ :param_1,:param_2,:param_3 from dual".
"select /* my_hint */ case 1 when 2 then 3 else 4 end alias_1 from dual".
"select /* my_hint */ case 1 when 2 then 3 else 4 end from dual".
"select /* my_hint */ case 11 when 12 then 13 else 14 end alias_1,case 21 when 22 then 23 else 24 end alias_2,case 31 when 32 then 33 else 34 alias_3 end from dual".
"select /* my_hint */ case 11 when 12 then 13 else 14 end,case 21 when 22 then 23 else 24 end,case 31 when 32 then 33 else 34 end from dual".
"select /* my_hint */ column_1 alias_1 from dual".
"select /* my_hint */ column_1 alias_1,column_2 alias_2,column_3 alias_3 from dual".
"select /* my_hint */ column_1 from dual".
"select /* my_hint */ column_1,column_2,column_3 from dual".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hint & opt & fields
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select /* my_hint */ all (select column_1 from table_1) alias_1 from dual".
"select /* my_hint */ all (select column_1 from table_1) alias_1,(select column_2 from table_2) alias_2,(select column_3 from table_3) alias_3 from dual".
"select /* my_hint */ all (select column_1 from table_1) from dual".
"select /* my_hint */ all (select column_1 from table_1),(select column_2 from table_2),(select column_3 from table_3) from dual".
"select /* my_hint */ all :param_1 alias_1 from dual".
"select /* my_hint */ all :param_1 alias_1,:param_2 alias_2,:param_3 alias_3 from dual".
"select /* my_hint */ all :param_1 from dual".
"select /* my_hint */ all :param_1,:param_2,:param_3 from dual".
"select /* my_hint */ all case 1 when 2 then 3 else 4 end alias_1 from dual".
"select /* my_hint */ all case 1 when 2 then 3 else 4 end from dual".
"select /* my_hint */ all case 11 when 12 then 13 else 14 end alias_1,case 21 when 22 then 23 else 24 end alias_2,case 31 when 32 then 33 else 34 alias_3 end from dual".
"select /* my_hint */ all case 11 when 12 then 13 else 14 end,case 21 when 22 then 23 else 24 end,case 31 when 32 then 33 else 34 end from dual".
"select /* my_hint */ all column_1 alias_1 from dual".
"select /* my_hint */ all column_1 alias_1,column_2 alias_2,column_3 alias_3 from dual".
"select /* my_hint */ all column_1 from dual".
"select /* my_hint */ all column_1,column_2,column_3 from dual".
"select /* my_hint */ distinct (select column_1 from table_1) alias_1 from dual".
"select /* my_hint */ distinct (select column_1 from table_1) alias_1,(select column_2 from table_2) alias_2,(select column_3 from table_3) alias_3 from dual".
"select /* my_hint */ distinct (select column_1 from table_1) from dual".
"select /* my_hint */ distinct (select column_1 from table_1),(select column_2 from table_2),(select column_3 from table_3) from dual".
"select /* my_hint */ distinct :param_1 alias_1 from dual".
"select /* my_hint */ distinct :param_1 alias_1,:param_2 alias_2,:param_3 alias_3 from dual".
"select /* my_hint */ distinct :param_1 from dual".
"select /* my_hint */ distinct :param_1,:param_2,:param_3 from dual".
"select /* my_hint */ distinct case 1 when 2 then 3 else 4 end alias_1 from dual".
"select /* my_hint */ distinct case 1 when 2 then 3 else 4 end from dual".
"select /* my_hint */ distinct case 11 when 12 then 13 else 14 end alias_1,case 21 when 22 then 23 else 24 end alias_2,case 31 when 32 then 33 else 34 alias_3 end from dual".
"select /* my_hint */ distinct case 11 when 12 then 13 else 14 end,case 21 when 22 then 23 else 24 end,case 31 when 32 then 33 else 34 end from dual".
"select /* my_hint */ distinct column_1 alias_1 from dual".
"select /* my_hint */ distinct column_1 alias_1,column_2 alias_2,column_3 alias_3 from dual".
"select /* my_hint */ distinct column_1 from dual".
"select /* my_hint */ distinct column_1,column_2,column_3 from dual".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% opt & fields
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select all (select column_1 from table_1) alias_1 from dual".
"select all (select column_1 from table_1) alias_1,(select column_2 from table_2) alias_2,(select column_3 from table_3) alias_3 from dual".
"select all (select column_1 from table_1) from dual".
"select all (select column_1 from table_1),(select column_2 from table_2),(select column_3 from table_3) from dual".
"select all :param_1 alias_1 from dual".
"select all :param_1 alias_1,:param_2 alias_2,:param_3 alias_3 from dual".
"select all :param_1 from dual".
"select all :param_1,:param_2,:param_3 from dual".
"select all case 1 when 2 then 3 else 4 end alias_1 from dual".
"select all case 1 when 2 then 3 else 4 end from dual".
"select all case 11 when 12 then 13 else 14 end alias_1,case 21 when 22 then 23 else 24 end alias_2,case 31 when 32 then 33 else 34 alias_3 end from dual".
"select all case 11 when 12 then 13 else 14 end,case 21 when 22 then 23 else 24 end,case 31 when 32 then 33 else 34 end from dual".
"select all column_1 alias_1 from dual".
"select all column_1 alias_1,column_2 alias_2,column_3 alias_3 from dual".
"select all column_1 from dual".
"select all column_1,column_2,column_3 from dual".
"select distinct (select column_1 from table_1) alias_1 from dual".
"select distinct (select column_1 from table_1) alias_1,(select column_2 from table_2) alias_2,(select column_3 from table_3) alias_3 from dual".
"select distinct (select column_1 from table_1) from dual".
"select distinct (select column_1 from table_1),(select column_2 from table_2),(select column_3 from table_3) from dual".
"select distinct :param_1 alias_1 from dual".
"select distinct :param_1 alias_1,:param_2 alias_2,:param_3 alias_3 from dual".
"select distinct :param_1 from dual".
"select distinct :param_1,:param_2,:param_3 from dual".
"select distinct case 1 when 2 then 3 else 4 end alias_1 from dual".
"select distinct case 1 when 2 then 3 else 4 end from dual".
"select distinct case 11 when 12 then 13 else 14 end alias_1,case 21 when 22 then 23 else 24 end alias_2,case 31 when 32 then 33 else 34 alias_3 end from dual".
"select distinct case 11 when 12 then 13 else 14 end,case 21 when 22 then 23 else 24 end,case 31 when 32 then 33 else 34 end from dual".
"select distinct column_1 alias_1 from dual".
"select distinct column_1 alias_1,column_2 alias_2,column_3 alias_3 from dual".
"select distinct column_1 from dual".
"select distinct column_1,column_2,column_3 from dual".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% into
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select * into :param_1 :param_2 from dual".
"select * into :param_1 :param_2,:param_3 :param_4,:param_5 :param_6 from dual".
"select * into :param_1 from dual".
"select * into :param_1 indicator :param_2 from dual".
"select * into :param_1 indicator :param_2,:param_3 indicator :param_4,:param_5 indicator :param_6 from dual".
"select * into :param_1,:param_2,:param_3 from dual".
"select * into column_1 from dual".
"select * into column_1,column_2,column_3 from dual".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select * from :param_1 alias_1".
"select * from :param_1 alias_1,:param_2 alias_2,:param_3 alias_3".
"select * from :param_1".
"select * from :param_1,:param_2,:param_3".
"select * from :param_1\"@link_1\" alias_1".
"select * from :param_1\"@link_1\" alias_1,:param_2\"@link_2\" alias_2,:param_3\"@link_3\" alias_3".
"select * from :param_1\"@link_1\"".
"select * from :param_1\"@link_1\",:param_2\"@link_2\",:param_3\"@link_3\"".
"select * from schema_1.table_1 alias_1".
"select * from schema_1.table_1 alias_1,schema_2.table_2 alias_2,schema_3.table_3 alias_3".
"select * from schema_1.table_1".
"select * from schema_1.table_1,schema_2.table_2,schema_3.table_3".
"select * from schema_1.table_1\"@dblink_1\" partition by column_1 full join schema_2.table_2\"@dblink_2\" partition by column_1 using(column_1,column_2,column_3)".
"select * from schema_1.table_1\"@dblink_1\" partition by column_1 full join schema_2.table_2\"@dblink_2\" using(column_1,column_2,column_3)".
"select * from schema_1.table_1\"@dblink_1\" partition by column_1 natural full join schema_2.table_2\"@dblink_2\" partition by column_1 using(column_1,column_2,column_3)".
"select * from schema_1.table_1\"@dblink_1\" partition by column_1 natural full join schema_2.table_2\"@dblink_2\" using(column_1,column_2,column_3)".
"select * from schema_1.table_1\"@link_1\" alias_1".
"select * from schema_1.table_1\"@link_1\" alias_1,schema_2.table_2\"@link_2\" alias_2,schema_3.table_3\"@link_3\" alias_3".
"select * from schema_1.table_1\"@link_1\"".
"select * from schema_1.table_1\"@link_1\",schema_2.table_2\"@link_2\",schema_3.table_3\"@link_3\"".
"select * from select * from dual alias_1".
"select * from table_1 alias_1".
"select * from table_1 alias_1,table_2 alias_2,table_3 alias_3".
"select * from table_1 cross join :param_2 natural join :param_3 natural inner join :param_3".
"select * from table_1 cross join :param_2".
"select * from table_1 cross join table_2".
"select * from table_1 cross join(select column_2 from table_2)".
"select * from table_1 cross join(select column_2 from table_2)alias_1".
"select * from table_1 cross join(select column_2 from table_2)|:b.f[f(p.r:q)]| alias_1".
"select * from table_1 cross join(select column_2 from table_2)|:b.f[f(p.r:q)]|".
"select * from table_1 full join :param_2".
"select * from table_1 full join table_2".
"select * from table_1 full join(select column_2 from table_2)".
"select * from table_1 full outer join table_2".
"select * from table_1 inner join :param_2 on column1 is not null".
"select * from table_1 inner join :param_2 on column1".
"select * from table_1 inner join :param_2 using(column_1,column_2,column_3)".
"select * from table_1 inner join table_2 on column1 is not null".
"select * from table_1 inner join table_2 using(column_1,column_2,column_3)".
"select * from table_1 inner join(select column_2 from table_2)alias_1 on column1 is not null".
"select * from table_1 inner join(select column_2 from table_2)on column1 is not null".
"select * from table_1 inner join(select column_2 from table_2)using(column_1,column_2,column_3)".
"select * from table_1 inner join(select column_2 from table_2)|:b.f[f(p.r:q)]| alias_1 on column1 is not null".
"select * from table_1 inner join(select column_2 from table_2)|:b.f[f(p.r:q)]| on column1 is not null".
"select * from table_1 join :param_2 on column1 is not null".
"select * from table_1 join :param_2 using(column_1,column_2,column_3)".
"select * from table_1 join table_2 on column1 is not null".
"select * from table_1 join table_2 using(column_1,column_2,column_3)".
"select * from table_1 join(select column_2 from table_2)on column1 is not null".
"select * from table_1 join(select column_2 from table_2)using(column_1,column_2,column_3)".
"select * from table_1 left join table_2".
"select * from table_1 left outer join table_2".
"select * from table_1 natural inner join :param_2".
"select * from table_1 natural inner join table_2".
"select * from table_1 natural inner join(select column_2 from table_2)".
"select * from table_1 natural join :param_2".
"select * from table_1 natural join table_2".
"select * from table_1 natural join(select column_2 from table_2)".
"select * from table_1 right join table_2".
"select * from table_1 right outer join table_2".
"select * from table_1".
"select * from table_1,table_2,table_3".
"select * from table_1\"@link_1\" alias_1".
"select * from table_1\"@link_1\" alias_1,table_2\"@link_2\" alias_2,table_3\"@link_3\" alias_3".
"select * from table_1\"@link_1\"".
"select * from table_1\"@link_1\",table_2\"@link_2\",table_3\"@link_3\"".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 full join schema_2.table_2\"@dblink_2\" )".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 full join schema_2.table_2\"@dblink_2\" on column_1 = column_2)".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 full join schema_2.table_2\"@dblink_2\" partition by column_1 )".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 full join schema_2.table_2\"@dblink_2\" partition by column_1 on column_1 = column_2)".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 natural full join schema_2.table_2\"@dblink_2\" )".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 natural full join schema_2.table_2\"@dblink_2\" on column_1 = column_2)".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 natural full join schema_2.table_2\"@dblink_2\" partition by column_1 )".
"select * from(schema_1.table_1\"@dblink_1\" partition by column_1 natural full join schema_2.table_2\"@dblink_2\" partition by column_1 on column_1 = column_2)".
"select * from(select * from dual)".
"select * from(select * from dual)alias_1".
"select * from(select * from dual)|:b.f[f(p.r:q)]| alias_1".
"select * from(select * from dual)|:b.f[f(p.r:q)]|".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select * from dual where column_1 != some select column_2 from table_2".
"select * from dual where column_1 < any(select column_2 from table_2)".
"select * from dual where column_1 <= any(select column_2 from table_2)".
"select * from dual where column_1 <> all(select column_2 from table_2)".
"select * from dual where column_1 = any select column_2 from table_2".
"select * from dual where column_1 = prior column_2".
"select * from dual where column_1 > all select column_2 from table_2".
"select * from dual where column_1 >= 10 and column_2 <= 20 or column_1 >= 30 and column_2 <= 39".
"select * from dual where column_1 >= 10 and column_2 <= 20".
"select * from dual where column_1 >= all select column_2 from table_2".
"select * from dual where column_1 ^= some(select column_2 from table_2)".
"select * from dual where column_1 between 1 and 10 or column_2 not between column_3 and :param_4".
"select * from dual where column_1 between 1 and 10".
"select * from dual where column_1 in (4711,4712,4713)".
"select * from dual where column_1 in (:param_4711,:param_4712,:param_4713)".
"select * from dual where column_1 in (column_4711,column_4712,column_4713)".
"select * from dual where column_1 in (select column_2 from table_2)".
"select * from dual where column_1 is not null".
"select * from dual where column_1 is null".
"select * from dual where column_1 like 'wwe*'".
"select * from dual where column_1 like column_2 escape 'test'".
"select * from dual where column_1 like column_2 escape 1.5".
"select * from dual where column_1 like column_2 escape 4711".
"select * from dual where column_1 like column_2 escape user".
"select * from dual where column_1 like column_2".
"select * from dual where column_1 not between column_2 and :param_3".
"select * from dual where column_1 not in (4711,4712,4713)".
"select * from dual where column_1 not in (:param_4711,:param_4712,:param_4713)".
"select * from dual where column_1 not in (column_4711,column_4712,column_4713)".
"select * from dual where column_1 not in (select column_2 from table_2)".
"select * from dual where column_1 not like 'wwe*'".
"select * from dual where column_1 not like column_2 escape :param_1".
"select * from dual where column_1 not like column_2".
"select * from dual where exists (select column_2 from table_2)".
"select * from dual where exists select column_2 from table_2".
"select * from dual where not column_1 >= 10 and column_2 <= 20".
"select * from dual where not exists (select column_2 from table_2)".
"select * from dual where not exists select column_2 from table_2".
"select * from dual where prior column_1 != column_2".
"select * from dual where prior column_1 < column_2".
"select * from dual where prior column_1 <= column_2".
"select * from dual where prior column_1 <> column_2".
"select * from dual where prior column_1 = column_2".
"select * from dual where prior column_1 > column_2".
"select * from dual where prior column_1 >= column_2".
"select * from dual where prior column_1 ^= column_2".
"select * from dual where sum(column_1,column_2)>4711".
"select * from dual where(column_1 >= 10 and column_2 <= 20 or column_1 >= 30 and column_2 <= 39)".
"select * from dual where(column_1 >= 10 and column_2 <= 20)".
"select * from dual where(not column_1 >= 10 and column_2 <= 20)".
"select * from dual where(sum(column_1,column_2)>4711)".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hierarchical_query_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select * from dual connect by exists (select column_2 from table_2) start with (not column_1 >= 10 and column_2 <= 20)".
"select * from dual connect by exists (select column_2 from table_2) start with not exists select column_2 from table_2".
"select * from dual connect by exists (select column_2 from table_2) start with sum(column_1,column_2)>4711".
"select * from dual connect by nocycle exists (select column_2 from table_2) start with (column_1 >= 10 and column_2 <= 20 or column_1 >= 30 and column_2 <= 39)".
"select * from dual connect by nocycle exists (select column_2 from table_2) start with (sum(column_1,column_2)>4711)".
"select * from dual connect by nocycle exists (select column_2 from table_2) start with not column_1 >= 10 and column_2 <= 20".
"select * from dual connect by nocycle exists (select column_2 from table_2) start with prior column_1 != column_2".
"select * from dual start with column_1 connect by column_1 != some select column_2 from table_2".
"select * from dual start with exists (select column_2 from table_2) connect by column_1 != some select column_2 from table_2".
"select * from dual start with exists (select column_2 from table_2) connect by column_1 > all select column_2 from table_2".
"select * from dual start with exists (select column_2 from table_2) connect by column_1 between 1 and 10 or column_2 not between column_3 and :param_4".
"select * from dual start with exists (select column_2 from table_2) connect by column_1 in (select column_2 from table_2)".
"select * from dual start with exists (select column_2 from table_2) connect by column_1 like column_2 escape 'test'".
"select * from dual start with exists (select column_2 from table_2) connect by column_1 not in (select column_2 from table_2)".
"select * from dual start with exists (select column_2 from table_2) connect by column_1".
"select * from dual start with exists (select column_2 from table_2) connect by exists (select column_2 from table_2)".
"select * from dual start with exists (select column_2 from table_2) connect by nocycle column_1 = prior column_2".
"select * from dual start with exists (select column_2 from table_2) connect by nocycle column_1 >= 10 and column_2 <= 20 or column_1 >= 30 and column_2 <= 39".
"select * from dual start with exists (select column_2 from table_2) connect by nocycle column_1 in (:param_4711,:param_4712,:param_4713)".
"select * from dual start with exists (select column_2 from table_2) connect by nocycle column_1 is not null".
"select * from dual start with exists (select column_2 from table_2) connect by nocycle column_1 not in (4711,4712,4713)".
"select * from dual start with exists (select column_2 from table_2) connect by nocycle column_1".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% group_by_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select * from dual group by column_1".
"select * from dual group by column_1(+)".
"select * from dual group by column_1,column_1(+),column_1|:b.f[f(p.r:q)]|".
"select * from dual group by column_1|:b.f[f(p.r:q)]|".
"select * from dual group by function_1(column_1,column_2,column_3)".
"select * from dual group by function_1(column_1,column_2,column_3),function_1(column_1,column_2,column_3)|:b.f[f(p.r:q)]|".
"select * from dual group by function_1(column_1,column_2,column_3)|:b.f[f(p.r:q)]|".
"select * from dual group by max".
"select * from dual group by max(*)".
"select * from dual group by max(*)|:b.f[f(p.r:q)]|".
"select * from dual group by max(all column_1)|:b.f[f(p.r:q)]|,max(column_1,colum_2,100)".
"select * from dual group by max(all column_2)".
"select * from dual group by max(all column_2)|:b.f[f(p.r:q)]|".
"select * from dual group by max(all column_2)|:b.f[f(p.r:q)]|,max(column_1,colum_2,100)".
"select * from dual group by max(column_1,colum_2,100)".
"select * from dual group by max(column_1,colum_2,100)|:b.f[f(p.r:q)]|".
"select * from dual group by max(column_1,colum_2,100)|:b.f[f(p.r:q)]|,max(distinct column_1)".
"select * from dual group by max(distinct column_1)".
"select * from dual group by max(distinct column_1)|:b.f[f(p.r:q)]|".
"select * from dual group by max(distinct column_1)|:b.f[f(p.r:q)]|,max|:b.f[f(p.r:q)]|".
"select * from dual group by max,max(*),max(*)|:b.f[f(p.r:q)]|,max(all column_1)".
"select * from dual group by max,max(*),max(*)|:b.f[f(p.r:q)]|,max(all column_2)".
"select * from dual group by max|:b.f[f(p.r:q)]|".
"select * from dual group by package_1.function_1(column_1,column_2,column_3)".
"select * from dual group by package_1.function_1(column_1,column_2,column_3),package_1.function_1(column_1,column_2,column_3)|:b.f[f(p.r:q)]|".
"select * from dual group by package_1.function_1(column_1,column_2,column_3)|:b.f[f(p.r:q)]|".
"select * from dual group by package_1.table_1.*".
"select * from dual group by package_1.table_1.*,package_1.table_1.column_1,package_1.table_1.column_1(+)".
"select * from dual group by package_1.table_1.column_1".
"select * from dual group by package_1.table_1.column_1(+)".
"select * from dual group by package_1.table_1.column_1|:b.f[f(p.r:q)]|".
"select * from dual group by package_1.table_1.column_1|:b.f[f(p.r:q)]|,schema_1.package_1.function_1(column_1,column_2,column_3)".
"select * from dual group by schema_1.package_1.function_1(column_1,column_2,column_3)".
"select * from dual group by schema_1.package_1.function_1(column_1,column_2,column_3)|:b.f[f(p.r:q)]|".
"select * from dual group by schema_1.package_1.function_1(column_1,column_2,column_3)|:b.f[f(p.r:q)]|,table_1.*".
"select * from dual group by table_1.*".
"select * from dual group by table_1.column_1".
"select * from dual group by table_1.column_1(+)".
"select * from dual group by table_1.column_1,table_1.column_1(+),table_1.column_1|:b.f[f(p.r:q)]|".
"select * from dual group by table_1.column_1|:b.f[f(p.r:q)]|".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% having_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select * from dual having column_1 != some select column_2 from table_2".
"select * from dual having column_1 < any(select column_2 from table_2)".
"select * from dual having column_1 <= any(select column_2 from table_2)".
"select * from dual having column_1 <> all(select column_2 from table_2)".
"select * from dual having column_1 = any select column_2 from table_2".
"select * from dual having column_1 = prior column_2".
"select * from dual having column_1 > all select column_2 from table_2".
"select * from dual having column_1 >= 10 and column_2 <= 20 or column_1 >= 30 and column_2 <= 39".
"select * from dual having column_1 >= 10 and column_2 <= 20".
"select * from dual having column_1 >= all select column_2 from table_2".
"select * from dual having column_1 ^= some(select column_2 from table_2)".
"select * from dual having column_1 between 1 and 10 or column_2 not between column_3 and :param_4".
"select * from dual having column_1 between 1 and 10".
"select * from dual having column_1 in (4711,4712,4713)".
"select * from dual having column_1 in (:param_4711,:param_4712,:param_4713)".
"select * from dual having column_1 in (column_4711,column_4712,column_4713)".
"select * from dual having column_1 in (select column_2 from table_2)".
"select * from dual having column_1 is not null".
"select * from dual having column_1 is null".
"select * from dual having column_1 like 'wwe*'".
"select * from dual having column_1 like column_2 escape 'test'".
"select * from dual having column_1 like column_2 escape 1.5".
"select * from dual having column_1 like column_2 escape 4711".
"select * from dual having column_1 like column_2 escape user".
"select * from dual having column_1 like column_2".
"select * from dual having column_1 not between column_2 and :param_3".
"select * from dual having column_1 not in (4711,4712,4713)".
"select * from dual having column_1 not in (:param_4711,:param_4712,:param_4713)".
"select * from dual having column_1 not in (column_4711,column_4712,column_4713)".
"select * from dual having column_1 not in (select column_2 from table_2)".
"select * from dual having column_1 not like 'wwe*'".
"select * from dual having column_1 not like column_2 escape :param_1".
"select * from dual having column_1 not like column_2".
"select * from dual having column_1".
"select * from dual having exists (select column_2 from table_2)".
"select * from dual having exists select column_2 from table_2".
"select * from dual having not column_1 >= 10 and column_2 <= 20".
"select * from dual having not exists (select column_2 from table_2)".
"select * from dual having not exists select column_2 from table_2".
"select * from dual having prior column_1 != column_2".
"select * from dual having prior column_1 < column_2".
"select * from dual having prior column_1 <= column_2".
"select * from dual having prior column_1 <> column_2".
"select * from dual having prior column_1 = column_2".
"select * from dual having prior column_1 > column_2".
"select * from dual having prior column_1 >= column_2".
"select * from dual having prior column_1 ^= column_2".
"select * from dual having sum(column_1,column_2)>4711".
"select * from dual having(column_1 >= 10 and column_2 <= 20 or column_1 >= 30 and column_2 <= 39)".
"select * from dual having(column_1 >= 10 and column_2 <= 20)".
"select * from dual having(not column_1 >= 10 and column_2 <= 20)".
"select * from dual having(sum(column_1,column_2)>4711)".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% order_by_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select * from dual order by (select column_2 from table_2)".
"select * from dual order by - column_2".
"select * from dual order by :param_1".
"select * from dual order by column_1".
"select * from dual order by (column_1(+))|:b.f[f(p.r:q)]| asc".
"select * from dual order by column_1,column_1(+),(column_1)|:b.f[f(p.r:q)]|".
"select * from dual order by (column_1)|:b.f[f(p.r:q)]|".
"select * from dual order by function_1(column_1,column_2,column_3)".
"select * from dual order by function_1(column_1,column_2,column_3),(function_1(column_1,column_2,column_3))|:b.f[f(p.r:q)]|".
"select * from dual order by (function_1(column_1,column_2,column_3))|:b.f[f(p.r:q)]|".
"select * from dual order by max".
"select * from dual order by max(*)".
"select * from dual order by (max(*))|:b.f[f(p.r:q)]|".
"select * from dual order by (max(all column_1))|:b.f[f(p.r:q)]| asc,max(column_1,colum_2,100)".
"select * from dual order by max(all column_2)".
"select * from dual order by (max(all column_2))|:b.f[f(p.r:q)]| asc,max(column_1,colum_2,100)".
"select * from dual order by (max(all column_2))|:b.f[f(p.r:q)]|".
"select * from dual order by max(column_1,colum_2,100)".
"select * from dual order by (max(column_1,colum_2,100))|:b.f[f(p.r:q)]| asc,max(distinct column_1)".
"select * from dual order by (max(column_1,colum_2,100))|:b.f[f(p.r:q)]|".
"select * from dual order by max(distinct column_1)".
"select * from dual order by (max(distinct column_1))|:b.f[f(p.r:q)]|".
"select * from dual order by (max(distinct column_1))|:b.f[f(p.r:q)]|,(max)|:b.f[f(p.r:q)]|".
"select * from dual order by max,max(*),(max(*))|:b.f[f(p.r:q)]|,max(all column_1)".
"select * from dual order by max,max(*),(max(*))|:b.f[f(p.r:q)]|,max(all column_2)".
"select * from dual order by max|:b.f[f(p.r:q)]|".
"select * from dual order by package_1.function_1(column_1,column_2,column_3) desc,(package_1.function_1(column_1,column_2,column_3))|:b.f[f(p.r:q)]|".
"select * from dual order by package_1.function_1(column_1,column_2,column_3)".
"select * from dual order by (package_1.function_1(column_1,column_2,column_3))|:b.f[f(p.r:q)]|".
"select * from dual order by package_1.table_1.* desc,package_1.table_1.column_1 desc,package_1.table_1.column_1(+)".
"select * from dual order by package_1.table_1.*".
"select * from dual order by package_1.table_1.column_1".
"select * from dual order by package_1.table_1.column_1(+)".
"select * from dual order by (package_1.table_1.column_1)|:b.f[f(p.r:q)]| desc,schema_1.package_1.function_1(column_1,column_2,column_3)".
"select * from dual order by (package_1.table_1.column_1)|:b.f[f(p.r:q)]|".
"select * from dual order by schema_1.package_1.function_1(column_1,column_2,column_3)".
"select * from dual order by (schema_1.package_1.function_1(column_1,column_2,column_3))|:b.f[f(p.r:q)]| desc,table_1.*".
"select * from dual order by (schema_1.package_1.function_1(column_1,column_2,column_3))|:b.f[f(p.r:q)]|".
"select * from dual order by table_1.*".
"select * from dual order by (table_1.column_1 + column_2)|:b.f[f(p.r:q)]|".
"select * from dual order by table_1.column_1 div column_2".
"select * from dual order by table_1.column_1".
"select * from dual order by table_1.column_1(+)".
"select * from dual order by table_1.column_1,table_1.column_1(+),(table_1.column_1)|:b.f[f(p.r:q)]|".
"select * from dual order by (table_1.column_1)|:b.f[f(p.r:q)]|".
"select * from table_1 order by (select * from table_2) asc".
"select * from table_1 order by (select * from table_2) asc,(select * from table_3) desc".
"select * from table_1 order by (select * from table_2)".
"select * from table_1 order by column_1 asc".
"select * from table_1 order by column_1 asc,column_2 desc".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% legacy tests
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"Select * From  :param_1 alias_1 Join :param_2 alias_2 On column_1 = column_2".
"Select * From  :param_1\"@link_1\" alias_1 Join :param_1\"@link_1\" alias_1 On column_1 = column_2".
"Select * From (:param_1 alias_1 Join :param_2 alias_2 On column_1 = column_2)".
"Select * From (:param_1\"@link_1\" alias_1 Join :param_1\"@link_1\" alias_1 On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\"                               Full Join schema_2.table_2\"@dblink_2\"                                             )".
"Select * From (schema_1.table_1\"@dblink_1\"                               Full Join schema_2.table_2\"@dblink_2\"                       On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\"                               Full Join schema_2.table_2\"@dblink_2\" Partition By column_1                       )".
"Select * From (schema_1.table_1\"@dblink_1\"                               Full Join schema_2.table_2\"@dblink_2\" Partition By column_1 On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\"                       Natural Full Join schema_2.table_2\"@dblink_2\"                                             )".
"Select * From (schema_1.table_1\"@dblink_1\"                       Natural Full Join schema_2.table_2\"@dblink_2\"                       On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\"                       Natural Full Join schema_2.table_2\"@dblink_2\" Partition By column_1                       )".
"Select * From (schema_1.table_1\"@dblink_1\"                       Natural Full Join schema_2.table_2\"@dblink_2\" Partition By column_1 On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\"         Full Join schema_2.table_2\"@dblink_2\" Partition By column_1 On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Cross Join schema_2.table_2\"@dblink_2\")".
"Select * From (schema_1.table_1\"@dblink_1\" Inner         Join schema_2.table_2\"@dblink_2\" On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Inner Join schema_2.table_2\"@dblink_2\" On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Join schema_2.table_2\"@dblink_2\" On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Natural Full Join schema_2.table_2\"@dblink_2\"                       On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Natural Inner Join schema_2.table_2\"@dblink_2\")".
"Select * From (schema_1.table_1\"@dblink_1\" Natural Join schema_2.table_2\"@dblink_2\")".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1         Full Join schema_2.table_2\"@dblink_2\"                                             )".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1         Full Join schema_2.table_2\"@dblink_2\"                       On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1         Full Join schema_2.table_2\"@dblink_2\" Partition By column_1                       )".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1         Full Join schema_2.table_2\"@dblink_2\" Partition By column_1 On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1 Natural Full Join schema_2.table_2\"@dblink_2\"                                             )".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1 Natural Full Join schema_2.table_2\"@dblink_2\"                       On column_1 = column_2)".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1 Natural Full Join schema_2.table_2\"@dblink_2\" Partition By column_1                       )".
"Select * From (schema_1.table_1\"@dblink_1\" Partition By column_1 Natural Full Join schema_2.table_2\"@dblink_2\" Partition By column_1 On column_1 = column_2)".
"Select * From (Select * From dual Where column_1 = column_2)".
"Select * From :param_1 alias_1".
"Select * From :param_1".
"Select * From :param_1\"@link_1\" alias_1".
"Select * From :param_1\"@link_1\"".
"select * from :t tst1".
"select * from :t tst2".
"select * from :t".
"Select * From \"^&()\" alias_1".
"Select * From \"^&()\"".
"select * from dual where column_1 like (3 + 5)".
"select * from dual where column_1 like (select * from dual)".
"select * from dual where column_1 like select * from dual".
"select * from dual".
"SELECT * FROM name_table WHERE ((name_column_1 = name_column_2) AND (name_column_3 = name_column_4))".
"SELECT * FROM name_table WHERE ((name_column_1 = name_column_2) OR (name_column_3 = name_column_4))".
"SELECT * FROM name_table WHERE (name_column_1 = name_column_2)".
"SELECT * FROM name_table WHERE (NOT (name_column_1 = name_column_2))".
"SELECT * FROM name_table WHERE name_column_1 = 50".
"SELECT * FROM name_table WHERE name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT * FROM name_table WHERE name_column_1 = name_column_2 OR name_column_3 = name_column_4".
"SELECT * FROM name_table WHERE name_column_1 = name_column_2".
"SELECT * FROM name_table WHERE name_column_1 = PRIOR 50".
"SELECT * FROM name_table WHERE name_column_1 BETWEEN 50 AND 100".
"SELECT * FROM name_table WHERE name_column_1 BETWEEN name_column_2 AND name_column_3".
"SELECT * FROM name_table WHERE name_column_1 IN (name_expr_1)".
"SELECT * FROM name_table WHERE name_column_1 IN (name_expr_1, name_expr_1)".
"SELECT * FROM name_table WHERE name_column_1 IS NOT NULL".
"SELECT * FROM name_table WHERE name_column_1 IS NULL".
"SELECT * FROM name_table WHERE name_column_1 LIKE 50 ESCAPE 'name_atom'".
"SELECT * FROM name_table WHERE name_column_1 LIKE 50".
"SELECT * FROM name_table WHERE name_column_1 LIKE name_column_2 ESCAPE 'name_atom'".
"SELECT * FROM name_table WHERE name_column_1 LIKE name_column_2".
"SELECT * FROM name_table WHERE name_column_1 NOT BETWEEN 50 AND 100".
"SELECT * FROM name_table WHERE name_column_1 NOT BETWEEN name_column_2 AND name_column_3".
"SELECT * FROM name_table WHERE name_column_1 NOT IN (name_expr_1)".
"SELECT * FROM name_table WHERE name_column_1 NOT IN (name_expr_1, name_expr_1)".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE 50 ESCAPE 'Name_atom'".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE 50".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE name_column_2 ESCAPE 'name_atom'".
"SELECT * FROM name_table WHERE name_column_1 NOT LIKE name_column_2".
"SELECT * FROM name_table WHERE NOT name_column_1 = name_column_2".
"SELECT * FROM name_table WHERE PRIOR name_column_1 = 50".
"SELECT * FROM name_table".
"SELECT * FROM name_table_1 CONNECT BY name_column_2 = name_column_3 START WITH name_column_1 IS NULL".
"SELECT * FROM name_table_1 CONNECT BY NOCYCLE name_column_2 = name_column_3 START WITH name_column_1 IS NULL".
"SELECT * FROM name_table_1 START WITH name_column_1 IS NULL CONNECT BY name_column_2 = name_column_3".
"SELECT * FROM name_table_1 START WITH name_column_1 IS NULL CONNECT BY NOCYCLE name_column_2 = name_column_3".
"SELECT * FROM name_table_1 WHERE EXISTS (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 AND COUNT(name_column_3)".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4 HAVING name_column_5 = name_column_6".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4, name_column_5 HAVING name_column_5 = name_column_6".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY name_column_4, name_column_5".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 HAVING name_column_5 = name_column_6".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 ASC".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 ASC, name_column_5 DESC".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 ASC, name_column_5".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 DESC".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4 DESC, name_column_5 ASC".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4".
"SELECT * FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY name_column_4, name_column_5".
"SELECT * FROM name_table_1 WHERE name_column_1 > ALL (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE name_column_1 > ANY (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE name_column_1 > SOME (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE name_column_1 IN (SELECT * FROM name_table_2)".
"SELECT * FROM name_table_1 WHERE NOT EXISTS (SELECT * FROM name_table_2)".
"Select * From schema_1.table_1 alias_1".
"Select * From schema_1.table_1".
"Select * From schema_1.table_1\"@dblink_1\"         Full Join schema_2.table_2\"@dblink_2\" Partition By column_1 On column_1 = column_2".
"Select * From schema_1.table_1\"@dblink_1\" Inner         Join schema_2.table_2\"@dblink_2\" On column_1 = column_2".
"Select * From schema_1.table_1\"@dblink_1\" Natural Full Join schema_2.table_2\"@dblink_2\"                       On column_1 = column_2".
"Select * From schema_1.table_1\"@dblink_1\" Natural Inner Join schema_2.table_2\"@dblink_2\"".
"Select * From schema_1.table_1\"@link_1\" alias_1".
"Select * From schema_1.table_1\"@link_1\"".
"Select * From Select * From dual Where column_1 = column_2".
"Select * From table_1 alias_1".
"Select * From table_1 Partition By (1, 2, 3, 4, 5) Natural Full Join table_2".
"Select * From table_1 Partition By 1, 2, 3, 4, 5 Natural Full Join table_2".
"Select * From table_1".
"Select * From table_1\"@link_1\" alias_1".
"Select * From table_1\"@link_1\"".
"SELECT * INTO name_variable_1 FROM name_table".
"SELECT /* */ DISTINCT * FROM name_table".
"SELECT /**/ DISTINCT * FROM name_table".
"SELECT /*hint*/ * FROM name_table".
"SELECT /*hint*/ ALL * FROM name_table".
"SELECT /*hint*/ ALL name_column_1 FROM name_table".
"SELECT /*hint*/ ALL name_column_1, name_column_2 FROM name_table".
"SELECT /*hint*/ DISTINCT * FROM name_table".
"SELECT /*hint*/ DISTINCT name_column_1 FROM name_table".
"SELECT /*hint*/ DISTINCT name_column_1, name_column_2 FROM name_table".
"SELECT /*hint*/ name_column_1 FROM name_table".
"SELECT /*hint*/ name_column_1, name_column_2 FROM name_table".
"Select a From tab Having a or (b and c)".
"Select a From tab Where a or (b and c)".
"SELECT ALL * FROM name_table".
"SELECT ALL name_column_1 FROM name_table".
"SELECT ALL name_column_1, name_column_2 FROM name_table".
"SELECT AVG(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT AVG(CASE WHEN e.salary > 2000 THEN e.salary ELSE 2000 END) Average_Salary FROM employees e;".
"SELECT AVG(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT AVG(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"Select Case When a or (b and c) Then d End From tab".
"SELECT column_1, column_2 FROM table_~_1, table_0000_2 WHERE column_3 IS NULL".
"SELECT column_1,column_2,COUNT(column_3) FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY column_1,column_2,column_3 HAVING COUNT(name_column_3) > 5".
"SELECT column_1,column_2,COUNT(column_3) FROM name_table_1 WHERE name_column_1 = name_column_2 GROUP BY column_1,column_2,REGR_COUNT(name_column_3,name_column_4)".
"SELECT column_1,column_2,COUNT(column_3) FROM name_table_1 WHERE name_column_1 = name_column_2 ORDER BY column_1,column_2,REGR_COUNT(name_column_3,name_column_4)".
"SELECT column~1, column~2 FROM table_~_1, table_0000_2 WHERE column_3 IS NULL".
"SELECT CORR(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(*) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COUNT(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COVAR_POP(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT COVAR_SAMP(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table".
"SELECT DISTINCT * FROM name_table_1 FULL JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 FULL OUTER JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 FULL OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 INNER JOIN name_table_2 USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 JOIN (SELECT * FROM name_table_2) USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 name_alias ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 name_alias USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 JOIN name_table_2 USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 LEFT JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 LEFT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 LEFT OUTER JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 LEFT OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL FULL OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL LEFT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL LEFT OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL RIGHT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 NATURAL RIGHT OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY (1) NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY (1), 2 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY (1, 2) NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY (Select * From dual) NATURAL FULL JOIN name_table_2 name_alias USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 name_alias ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 name_alias ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 name_alias USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 name_alias".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 ON name_column_1 = name_column_2 AND name_column_3 = name_column_4".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 ON name_column_1 = name_column_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 USING (name_column_1)".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2 USING (name_column_1, name_column_2)".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1, (2) NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 PARTITION BY 1, 2 NATURAL FULL JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 RIGHT JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 RIGHT JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1 RIGHT OUTER JOIN (SELECT * FROM name_table_2)".
"SELECT DISTINCT * FROM name_table_1 RIGHT OUTER JOIN name_table_2".
"SELECT DISTINCT * FROM name_table_1, name_table_2".
"SELECT DISTINCT name_column_1 FROM name_table".
"SELECT DISTINCT name_column_1, name_column_2 FROM name_table".
"SELECT MAX(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MAX(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MAX(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MEDIAN(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MIN(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MIN(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT MIN(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT name_column_1 FROM name_table".
"SELECT name_column_1 INTO name_variable_1 FROM name_table".
"SELECT name_column_1, name_column_2 FROM name_table".
"SELECT name_column_1, name_column_2 INTO name_variable_1, name_variable_2 FROM name_table".
"SELECT REGR_AVGX(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_AVGY(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_COUNT(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_INTERCEPT(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_R2(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SLOPE(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SXX(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SXY(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT REGR_SYY(name_column_3,name_column_4) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV_POP(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT STDDEV_SAMP(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT SUM(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT SUM(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT SUM(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VAR_POP(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VAR_SAMPLE(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VARIANCE(ALL name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VARIANCE(DISTINCT name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
"SELECT VARIANCE(name_column_3) FROM name_table_1 WHERE name_column_1 = name_column_2".
% <<"select 'öüäéèà', 'شلاؤيثبلتهتنمةىخ','นี่คือการทดสอบ' from dual"/utf8>>.
% <<"select * from tab t, (select * from tab) sql">>.
% <<"select a < 0, f(a+2 as d), f2(a < 3 as f) from abc">>.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% very complex test
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
   FROM account@1234@_no_de_@nohost,
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

%% -----------------------------------------------------------------------------
%% TESTS: SELECT
%% =============================================================================
