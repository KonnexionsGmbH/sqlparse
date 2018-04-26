%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: DELETE
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"delete from 'exotic name' alias_1".
"delete from 'exotic name'".
"delete from :param_1 alias_1".
"delete from :param_1 where current of cursor_1 return * into *".
"delete from :param_1".
"Delete From :param_1\"@link_1\" alias_1".
"delete from :param_1\"@link_1\"".
"Delete From \"^&()\" alias_1".
"Delete From \"^&()\"".
"Delete From schema_1.table_1 alias_1".
"Delete From schema_1.table_1".
"Delete From schema_1.table_1\"@link_1\" alias_1".
"delete from schema_1.table_1\"@link_1\"".
"Delete From table_1 alias_1".
"delete from table_1 return * into *".
"delete from table_1 return column_1,column_2 into column_3".
"delete from table_1 returning * into *".
"delete from table_1 returning column_1,column_2 into column_3".
"delete from table_1 where column_1 = column_2 and column_3 = column_4 return * into *".
"delete from table_1 where column_1 = column_2 and column_3 = column_4 return column_1,column_2 into column_3".
"delete from table_1 where column_1 = column_2 and column_3 = column_4 returning * into *".
"delete from table_1 where column_1 = column_2 and column_3 = column_4 returning column_1,column_2 into column_3".
"delete from table_1 where column_1 = column_2 and column_3 = column_4".
"delete from table_1 where current of cursor_1 return * into *".
"delete from table_1 where current of cursor_1 return column_1,column_2 into column_3".
"delete from table_1 where current of cursor_1 returning * into *".
"delete from table_1 where current of cursor_1 returning column_1,column_2 into column_3".
"delete from table_1 where current of cursor_1".
"Delete From table_1".
"Delete From table_1\"@link_1\" alias_1".
"Delete From table_1\"@link_1\"".
"DELETE FROM table_name WHERE CURRENT OF name_cursor".
"DELETE FROM table_name WHERE some_column=some_value RETURN c,d INTO :c, :d".
"DELETE FROM table_name WHERE some_column=some_value RETURN lob_column INTO :out_locator".
"DELETE FROM table_name WHERE some_column=some_value RETURNING c,d INTO :c, :d".
"DELETE FROM table_name WHERE some_column=some_value RETURNING lob_column INTO :out_locator".
"DELETE FROM table_name WHERE some_column=some_value".
"DELETE FROM table_name".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table collection expression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"delete from table (column_1|:b|) (+)".
"delete from table (table_1.column_1|:b|) (+)".
"delete from table (schema_1.table_1.column_1|:b|) (+)".
"delete from table (column_1) (+)".
"delete from table (table_1.column_1) (+)".
"delete from table (schema_1.table_1.column_1) (+)".
"delete from table (column_1(+)) (+)".
"delete from table (table_1.column_1(+)) (+)".
"delete from table (schema_1.table_1.column_1(+)) (+)".
"delete from table (table_1.*) (+)".
"delete from table (schema_1.table_1.*) (+)".

"delete from table (schema_1.package_1.function_1(arg_1, arg_2)) (+)".
"delete from table (package_1.function_1(arg_1, arg_2)) (+)".
"delete from table (function_1(arg_1, arg_2)) (+)".
"delete from table (max) (+)".
"delete from table (max(arg_1, arg_2)) (+)".
"delete from table (max(*)) (+)".
"delete from table (max(distinct column_1)) (+)".
"delete from table (max(all column_1)) (+)".
"delete from table (schema_1.package_1.function_1(arg_1, arg_2)|:b[f(p:q)]|) (+)".
"delete from table (package_1.function_1(arg_1, arg_2)|:b[f(p:q)]|) (+)".
"delete from table (function_1(arg_1, arg_2)|:b[f(p:q)]|) (+)".
"delete from table (max|:b[f(p:q)]|) (+)".
"delete from table (max(arg_1, arg_2)|:b[f(p:q)]|) (+)".
"delete from table (max(*)|:b[f(p:q)]|) (+)".
"delete from table (max(distinct column_1)|:b[f(p:q)]|) (+)".
"delete from table (max(all column_1)|:b[f(p:q)]|) (+)".

"delete from table (select * from table_1) (+)".
"delete from table (select column_1, column_2 from table_1) (+)".

%% -----------------------------------------------------------------------------
%% TESTS: DELETE
%% =============================================================================
