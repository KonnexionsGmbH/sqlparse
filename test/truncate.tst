%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS
%%

"truncate table tbl".
"truncate table name_schema.name_table".
"truncate table tbl preserve materialized view log".
"truncate table tbl purge materialized view log".
"truncate table tbl drop storage".
"truncate table tbl reuse storage".
"truncate table tbl preserve materialized view log drop storage".
"truncate table tbl preserve materialized view log reuse storage".
"truncate table tbl purge materialized view log drop storage".
"truncate table tbl purge materialized view log reuse storage".

% table reference -------------------------------------------------------------

"Truncate Table table_1 Drop Storage".
"Truncate Table schema_1.table_1 Drop Storage".
"Truncate Table :param_1 Drop Storage".
"Truncate Table \"^&()\" Drop Storage".
