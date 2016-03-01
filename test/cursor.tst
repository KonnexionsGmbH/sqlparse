%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"CLOSE name_cursor_1".
"DECLARE name_cursor_1 CURSOR FOR SELECT * FROM name_table_1".
"FETCH name_cursor_1 INTO name_column_1".
"OPEN name_cursor_1".
"DELETE FROM table_name WHERE CURRENT OF name_cursor".
"UPDATE name_table SET name_column_1 = :value_1 WHERE CURRENT OF name_cursor".
"UPDATE name_table SET name_column_1 = :value_1, name_column_2 = :value_2 WHERE CURRENT OF name_cursor".
