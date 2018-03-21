%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: DROP INDEX & DROP ROLE & DROP TABLE & DROP USER
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop index from :param_1".
"DROP INDEX FROM name_table_1".
"drop index from s.b".
"drop index from table_1".
"drop index index_1 from :param_1".
"drop index index_1 from table_1".
"drop index index_1".
"DROP INDEX name_index_1 FROM name_table_1".
"DROP INDEX name_schema_1.name_index_1 FROM name_table_1".
"drop index s.a from s.b".
"drop index s.a".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"DROP ROLE name_role_1".
"drop role role_1".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop imem_dal_skvh table :param_1 cascade".
"drop imem_dal_skvh table :param_1 restrict".
"drop imem_dal_skvh table :param_1".
"drop imem_dal_skvh table if exists :param_1 cascade".
"drop imem_dal_skvh table if exists :param_1 restrict".
"drop imem_dal_skvh table if exists :param_1".
"drop imem_dal_skvh table if exists table_1 cascade".
"drop imem_dal_skvh table if exists table_1 restrict".
"drop imem_dal_skvh table if exists table_1".
"drop imem_dal_skvh table if exists table_1,:param_2 cascade".
"drop imem_dal_skvh table if exists table_1,:param_2 restrict".
"drop imem_dal_skvh table if exists table_1,:param_2".
"drop imem_dal_skvh table if exists table_1,table_2 cascade".
"drop imem_dal_skvh table if exists table_1,table_2 restrict".
"drop imem_dal_skvh table if exists table_1,table_2".
"DROP imem_dal_skvh TABLE skvhtest".
"drop imem_dal_skvh table table_1 cascade".
"drop imem_dal_skvh table table_1 restrict".
"drop imem_dal_skvh table table_1".
"drop imem_dal_skvh table table_1,:param_2 cascade".
"drop imem_dal_skvh table table_1,:param_2 restrict".
"drop imem_dal_skvh table table_1,:param_2".
"drop imem_dal_skvh table table_1,table_2 cascade".
"drop imem_dal_skvh table table_1,table_2 restrict".
"drop imem_dal_skvh table table_1,table_2".
"drop table :param_1 cascade".
"drop table :param_1 restrict".
"Drop Table :param_1".
"drop table :param_1".
"Drop Table \"^&()\"".
"drop table if exists :param_1 cascade".
"drop table if exists :param_1 restrict".
"drop table if exists :param_1".
"drop table if exists :param_1,table_2 cascade".
"drop table if exists :param_1,table_2 restrict".
"drop table if exists :param_1,table_2".
"DROP TABLE IF EXISTS name_table_1 CASCADE".
"DROP TABLE IF EXISTS name_table_1 RESTRICT".
"DROP TABLE IF EXISTS name_table_1".
"DROP TABLE IF EXISTS name_table_1,name_table_2 CASCADE".
"DROP TABLE IF EXISTS name_table_1,name_table_2 RESTRICT".
"DROP TABLE IF EXISTS name_table_1,name_table_2".
"drop table if exists table_1 cascade".
"drop table if exists table_1 restrict".
"drop table if exists table_1".
"drop table if exists table_1,:param_2 cascade".
"drop table if exists table_1,:param_2 restrict".
"drop table if exists table_1,:param_2".
"drop table if exists table_1,table_2 cascade".
"drop table if exists table_1,table_2 restrict".
"drop table if exists table_1,table_2".
"DROP TABLE name_table_1 CASCADE".
"DROP TABLE name_table_1 RESTRICT".
"DROP TABLE name_table_1".
"DROP TABLE name_table_1,name_table_2 CASCADE".
"DROP TABLE name_table_1,name_table_2 RESTRICT".
"DROP TABLE name_table_1,name_table_2".
"Drop Table schema_1.table_1".
"drop table table_1 cascade".
"drop table table_1 restrict".
"Drop Table table_1".
"drop table table_1,:param_2 cascade".
"drop table table_1,:param_2 restrict".
"drop table table_1,:param_2".
"drop table table_1,table_2 cascade".
"drop table table_1,table_2 restrict".
"drop table table_1,table_2".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"DROP USER name_user_1 CASCADE".
"DROP USER name_user_1".
"drop user user_1 cascade".
"drop user user_1".

%% -----------------------------------------------------------------------------
%% TESTS: DROP INDEX & DROP ROLE & DROP TABLE & DROP USER
%% =============================================================================
