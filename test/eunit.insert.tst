%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: INSERT
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"insert into \"exotic name\" alias_1".
"insert into \"exotic name\"".
"insert into :param_1 alias_1".
"insert into :param_1".
"insert into :param_1\"@link_1\"".
"insert into :param_1\"@link_1\"alias_1".
"insert into \"% &()\"".
"insert into \"% &()\"alias_1".
"insert into abc values (1, 'a', 'b', 'c' || 'd')".
"insert into def (col1,col2) values ('C', 5+1)".
"insert into def (col1,col2) values ('C', \"undefined\")".
"INSERT INTO default_values_into_table".
"insert into numbers (float_value,integer) values ('C', \"undefined\")".
"INSERT INTO Persons VALUES (4,'Nilsen', 'Johan', 'Bakken 2', 'Stavanger')".
"insert into schema_1.table_1 alias_1".
"insert into schema_1.table_1".
"insert into schema_1.table_1\"@link_1\"".
"insert into schema_1.table_1\"@link_1\"alias_1".
"INSERT INTO some_table VALUES (:a, :b) RETURNING c,d INTO :c, :d".
"INSERT INTO some_table VALUES (:in_locator) RETURNING lob_column INTO :out_locator".
"insert into table_1 (field_a, field_3) values ('third','31.12.2012 23:59:59')".
"insert into table_1 (field_a, field_b) values ('first','Stefan''s choice.')".
"insert into table_1 (field_a, field_c) values ('second','Double quote \" in string')".
"insert into table_1 (field_a, field_c) values ('second','Single quote '' in string')".
"insert into table_1 (field_a, field_d) values ('third',erl(\"{a,b,c}\"))".
"insert into table_1 alias_1".
"insert into table_1 return * into *".
"insert into table_1 return column_1,column_2 into column_3".
"insert into table_1 returning * into *".
"insert into table_1 returning column_1,column_2 into column_3".
"insert into table_1 select * from table_2 returning column_1,column_2 into column_3".
"insert into table_1 select * from table_2".
"insert into table_1 values(column_1,\"text_2\",column_3,\"text_4\")return * into *".
"insert into table_1 values(column_1,\"text_2\",column_3,\"text_4\")return column_1,column_2 into column_3".
"insert into table_1 values(column_1,\"text_2\",column_3,\"text_4\")returning * into *".
"insert into table_1".
"insert into table_1(column_1,\"text_2\",column_3,\"text_4\")select * from table_2".
"insert into table_1(column_1,\"text_2\",column_3,\"text_4\")select * from table_2 returning column_1,column_2 into column_3".
"insert into table_1(column_1,\"text_2\",column_3,\"text_4\")values(column_1,\"text_2\",column_3,\"text_4\")return * into *".
"insert into table_1(column_1,\"text_2\",column_3,\"text_4\")values(column_1,\"text_2\",column_3,\"text_4\")return column_1,column_2 into column_3".
"insert into table_1(column_1,\"text_2\",column_3,\"text_4\")values(column_1,\"text_2\",column_3,\"text_4\")returning * into *".
"insert into table_1\"@link_1\"".
"insert into table_1\"@link_1\"alias_1".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table collection expression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"insert into table (column_1|:b|)".
"insert into table (table_1.column_1|:b|)".
"insert into table (schema_1.table_1.column_1|:b|)".
"insert into table (column_1)".
"insert into table (table_1.column_1)".
"insert into table (schema_1.table_1.column_1)".
"insert into table (column_1(+))".
"insert into table (table_1.column_1(+))".
"insert into table (schema_1.table_1.column_1(+))".
"insert into table (table_1.*)".
"insert into table (schema_1.table_1.*)".

"insert into table (schema_1.package_1.function_1(arg_1, arg_2))".
"insert into table (package_1.function_1(arg_1, arg_2))".
"insert into table (function_1(arg_1, arg_2))".
"insert into table (max)".
"insert into table (max(arg_1, arg_2))".
"insert into table (max(*))".
"insert into table (max(distinct column_1))".
"insert into table (max(all column_1))".
"insert into table (schema_1.package_1.function_1(arg_1, arg_2)|:b[f(p:q)]|)".
"insert into table (package_1.function_1(arg_1, arg_2)|:b[f(p:q)]|)".
"insert into table (function_1(arg_1, arg_2)|:b[f(p:q)]|)".
"insert into table (max|:b[f(p:q)]|)".
"insert into table (max(arg_1, arg_2)|:b[f(p:q)]|)".
"insert into table (max(*)|:b[f(p:q)]|)".
"insert into table (max(distinct column_1)|:b[f(p:q)]|)".
"insert into table (max(all column_1)|:b[f(p:q)]|)".

"insert into table (select * from table_1)".
"insert into table (select column_1, column_2 from table_1)".

"insert into TESTTABLE (PERSONID, LASTNAME) (select PERSONID, LASTNAME from TESTTABLE);".

%% -----------------------------------------------------------------------------
%% TESTS: INSERT
%% =============================================================================
