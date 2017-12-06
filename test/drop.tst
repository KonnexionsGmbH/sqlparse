%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS DROP INDEX
%%

"DROP INDEX FROM name_table_1".
"DROP INDEX name_index_1 FROM name_table_1".
"DROP INDEX name_schema_1.name_index_1 FROM name_table_1".

%% 
%% TESTS DROP ROLE
%%

"DROP ROLE name_role_1".

%% 
%% TESTS DROP TABLE
%%

"DROP TABLE name_table_1".
"DROP TABLE name_table_1, name_table_2".
"DROP TABLE name_table_1 CASCADE".
"DROP TABLE name_table_1, name_table_2 CASCADE".
"DROP TABLE name_table_1 RESTRICT".
"DROP TABLE name_table_1, name_table_2 RESTRICT".
"DROP TABLE IF EXISTS name_table_1".
"DROP TABLE IF EXISTS name_table_1, name_table_2".
"DROP TABLE IF EXISTS name_table_1 CASCADE".
"DROP TABLE IF EXISTS name_table_1, name_table_2 CASCADE".
"DROP TABLE IF EXISTS name_table_1 RESTRICT".
"DROP TABLE IF EXISTS name_table_1, name_table_2 RESTRICT".

"DROP imem_dal_skvh TABLE skvhtest".

%% 
%% TESTS DROP USER
%%

"DROP USER name_user_1".
"DROP USER name_user_1 CASCADE".

% table reference -------------------------------------------------------------

"Drop Table table_1".
"Drop Table schema_1.table_1".
"Drop Table :param_1".
"Drop Table \"^&()\"".
