%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

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
