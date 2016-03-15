%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"UPDATE name_table SET name_column_1 = :value_1".
"UPDATE name_table SET name_column_1 = :value_1, name_column_2 = :value_2".

"UPDATE name_table SET name_column_1 = :value_1, name_column_2 = :value_2 WHERE employee_id = :id".

"UPDATE employees set salary = :sal where employee_id = :id".
"UPDATE employees set salary = :sal where employee_id = :id RETURNING c,d INTO :c, :d".
"UPDATE employees set salary = :sal where employee_id = :id RETURNING lob_column INTO :out_locator".

"UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a is NULL".
"UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a || b = 'c' || 'd'".
