%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: UPDATE
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"Update :param_1 alias_1 Set column_1 = value_1".
"Update :param_1 Set column_1 = value_1".
"update :param_1 set column_1=value_1 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1 where column_1 = column_2 and column_3 = column_4 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1 where column_1 = column_2 and column_3 = column_4 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1 where column_1 = column_2 and column_3 = column_4".
"update :param_1 set column_1=value_1 where column_1".
"update :param_1 set column_1=value_1 where current of cursor_1 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1 where current of cursor_1 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1 where current of cursor_1".
"update :param_1 set column_1=value_1".
"update :param_1 set column_1=value_1,column_2=value_2 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2 where column_1 = column_2 and column_3 = column_4 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2 where column_1 = column_2 and column_3 = column_4 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2 where column_1 = column_2 and column_3 = column_4".
"update :param_1 set column_1=value_1,column_2=value_2 where current of cursor_1 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2 where current of cursor_1 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2 where current of cursor_1".
"update :param_1 set column_1=value_1,column_2=value_2".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 where column_1 = column_2 and column_3 = column_4 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 where column_1 = column_2 and column_3 = column_4 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 where column_1 = column_2 and column_3 = column_4".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 where current of cursor_1 return column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 where current of cursor_1 returning column_1,column_2 into column_3,column_4".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3 where current of cursor_1".
"update :param_1 set column_1=value_1,column_2=value_2,column_3=value_3".
"Update :param_1\"@link_1\" alias_1 Set column_1 = value_1".
"Update :param_1\"@link_1\" Set column_1 = value_1".
"Update \"^&()\" alias_1 Set column_1 = value_1".
"Update \"^&()\" Set column_1 = value_1".
"UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a is NULL".
"UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a || b = 'c' || 'd'".
"UPDATE employees set salary = :sal where employee_id = :id RETURNING c,d INTO :c, :d".
"UPDATE employees set salary = :sal where employee_id = :id RETURNING lob_column INTO :out_locator".
"UPDATE employees set salary = :sal where employee_id = :id".
"UPDATE name_table SET name_column_1 = :value_1 WHERE CURRENT OF name_cursor".
"UPDATE name_table SET name_column_1 = :value_1".
"UPDATE name_table SET name_column_1 = :value_1, name_column_2 = :value_2 WHERE CURRENT OF name_cursor".
"UPDATE name_table SET name_column_1 = :value_1, name_column_2 = :value_2 WHERE employee_id = :id".
"UPDATE name_table SET name_column_1 = :value_1, name_column_2 = :value_2".
"Update schema_1.table_1 alias_1 Set column_1 = value_1".
"Update schema_1.table_1 Set column_1 = value_1".
"Update schema_1.table_1\"@link_1\" alias_1 Set column_1 = value_1".
"Update schema_1.table_1\"@link_1\" Set column_1 = value_1".
"Update table_1 alias_1 Set column_1 = value_1".
"Update table_1 Set column_1 = value_1".
"update table_1 set column_1=value_1 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1 where column_1 = column_2 and column_3 = column_4 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1 where column_1 = column_2 and column_3 = column_4 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1 where column_1 = column_2 and column_3 = column_4".
"update table_1 set column_1=value_1 where current of cursor_1 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1 where current of cursor_1 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1 where current of cursor_1".
"update table_1 set column_1=value_1".
"update table_1 set column_1=value_1,column_2=value_2 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2 where column_1 = column_2 and column_3 = column_4 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2 where column_1 = column_2 and column_3 = column_4 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2 where column_1 = column_2 and column_3 = column_4".
"update table_1 set column_1=value_1,column_2=value_2 where current of cursor_1 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2 where current of cursor_1 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2 where current of cursor_1".
"update table_1 set column_1=value_1,column_2=value_2".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 where column_1 = column_2 and column_3 = column_4 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 where column_1 = column_2 and column_3 = column_4 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 where column_1 = column_2 and column_3 = column_4".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 where current of cursor_1 return column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 where current of cursor_1 returning column_1,column_2 into column_3,column_4".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3 where current of cursor_1".
"update table_1 set column_1=value_1,column_2=value_2,column_3=value_3".
"Update table_1\"@link_1\" alias_1 Set column_1 = value_1".
"Update table_1\"@link_1\" Set column_1 = value_1".

%% -----------------------------------------------------------------------------
%% TESTS: UPDATE
%% =============================================================================
