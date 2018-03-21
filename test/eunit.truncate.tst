%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: TRUNCATE
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"truncate table :param_1 drop storage".
"Truncate Table :param_1 Drop Storage".
"truncate table :param_1 preserve materialized view log drop storage".
"truncate table :param_1 preserve materialized view log reuse storage".
"truncate table :param_1 preserve materialized view log".
"truncate table :param_1 purge materialized view log drop storage".
"truncate table :param_1 purge materialized view log reuse storage".
"truncate table :param_1 purge materialized view log".
"truncate table :param_1 reuse storage".
"truncate table :param_1".
"Truncate Table \"^&()\" Drop Storage".
"truncate table name_schema.name_table".
"Truncate Table schema_1.table_1 Drop Storage".
"truncate table table_1 drop storage".
"Truncate Table table_1 Drop Storage".
"truncate table table_1 preserve materialized view log drop storage".
"truncate table table_1 preserve materialized view log reuse storage".
"truncate table table_1 preserve materialized view log".
"truncate table table_1 purge materialized view log drop storage".
"truncate table table_1 purge materialized view log reuse storage".
"truncate table table_1 purge materialized view log".
"truncate table table_1 reuse storage".
"truncate table table_1".
"truncate table tbl drop storage".
"truncate table tbl preserve materialized view log drop storage".
"truncate table tbl preserve materialized view log reuse storage".
"truncate table tbl preserve materialized view log".
"truncate table tbl purge materialized view log drop storage".
"truncate table tbl purge materialized view log reuse storage".
"truncate table tbl purge materialized view log".
"truncate table tbl reuse storage".

%% -----------------------------------------------------------------------------
%% TESTS: TRUNCATE
%% =============================================================================
