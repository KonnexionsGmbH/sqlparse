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

"begin cursor cursor_1 is (select * from dual)|:b.f[f(p.r:q)]|;end;".
"begin cursor cursor_1 is (select * from table_1 intersect select * from table_2) union (select * from table_3 minus select * from table_4);end;".
"begin cursor cursor_1 is (select * from table_1 intersect select * from table_2) union select * from table_3;end;".
"begin cursor cursor_1 is (select * from table_1 order by (select * from table_2) asc);end;".
"begin cursor cursor_1 is (select * from table_1 order by (select * from table_2) asc,(select * from table_3) desc);end;".
"begin cursor cursor_1 is (select * from table_1 order by (select * from table_2));end;".
"begin cursor cursor_1 is (select * from table_1 order by column_1 asc);end;".
"begin cursor cursor_1 is (select * from table_1 order by column_1 asc,column_2 desc);end;".
"begin cursor cursor_1 IS (SELECT * FROM table_1 Order By column_1);end;".
"begin cursor cursor_1 IS (SELECT * FROM table_1);end;".
"begin cursor cursor_1 is (select * from dual)|:b.f[f(p.r:q)]|;end;".
"begin cursor cursor_1 is select * from dual;end;".
"begin cursor cursor_1 is select * from table_1 intersect select * from table_2;end;".
"begin cursor cursor_1 is select * from table_1 minus select * from table_2;end;".
"begin cursor cursor_1 IS SELECT * FROM table_1 Order By column_1;end;".
"begin cursor cursor_1 is select * from table_1 union (select * from table_2 minus select * from table_3);end;".
"begin cursor cursor_1 is select * from table_1 union select * from table_2;end;".
"begin cursor cursor_1 IS SELECT * FROM table_1;end;".

%% -----------------------------------------------------------------------------
%% TESTS: CURSOR
%% =============================================================================
