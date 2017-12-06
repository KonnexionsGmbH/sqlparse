%% -*- erlang -*-
%% -*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS
%%

"INSERT INTO default_values_into_table".
"INSERT INTO some_table VALUES (:a, :b) RETURNING c,d INTO :c, :d".
"INSERT INTO some_table VALUES (:in_locator) RETURNING lob_column INTO :out_locator".
"insert into number (float,integer) values ('C', \"undefined\")".
"insert into def (col1,col2) values ('C', \"undefined\")".
"insert into def (col1,col2) values ('C', 5+1)".
"insert into table_1 (field_a, field_b) values ('first','Stefan''s choice.')".
"insert into table_1 (field_a, field_c) values ('second','Double quote \" in string')".
"insert into table_1 (field_a, field_c) values ('second','Single quote '' in string')".
"insert into table_1 (field_a, field_d) values ('third',erl(\"{a,b,c}\"))".
"insert into table_1 (field_a, field_3) values ('third','31.12.2012 23:59:59')".
"insert into abc values (1, 'a', 'b', 'c' || 'd')".
"INSERT INTO Persons VALUES (4,'Nilsen', 'Johan', 'Bakken 2', 'Stavanger')".
