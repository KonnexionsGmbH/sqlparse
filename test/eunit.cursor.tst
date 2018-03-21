%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: CURSOR
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"cursor cursor_1 is (select * from dual)|:b.f[f(p.r:q)]|".
"cursor cursor_1 is (select * from table_1 intersect select * from table_2) union (select * from table_3 minus select * from table_4)".
"cursor cursor_1 is (select * from table_1 intersect select * from table_2) union select * from table_3".
"cursor cursor_1 is (select * from table_1 order by (select * from table_2) asc)".
"cursor cursor_1 is (select * from table_1 order by (select * from table_2) asc,(select * from table_3) desc)".
"cursor cursor_1 is (select * from table_1 order by (select * from table_2))".
"cursor cursor_1 is (select * from table_1 order by column_1 asc)".
"cursor cursor_1 is (select * from table_1 order by column_1 asc,column_2 desc)".
"CURSOR cursor_1 IS (SELECT * FROM table_1 Order By column_1)".
"CURSOR cursor_1 IS (SELECT * FROM table_1)".
"cursor cursor_1 is (select * from dual)|:b.f[f(p.r:q)]|".
"cursor cursor_1 is select * from dual".
"cursor cursor_1 is select * from table_1 intersect select * from table_2".
"cursor cursor_1 is select * from table_1 minus select * from table_2".
"CURSOR cursor_1 IS SELECT * FROM table_1 Order By column_1".
"cursor cursor_1 is select * from table_1 union (select * from table_2 minus select * from table_3)".
"cursor cursor_1 is select * from table_1 union select * from table_2".
"CURSOR cursor_1 IS SELECT * FROM table_1".

%% -----------------------------------------------------------------------------
%% TESTS: CURSOR
%% =============================================================================
