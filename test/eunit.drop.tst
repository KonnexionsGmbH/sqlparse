%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: DROP INDEX & DROP ROLE & DROP TABLE & DROP USER
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_context_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop context context_name".
"drop context context_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop database".
"drop database;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop database link \"@Konnexions.ch\"".
"drop database link \"@Konnexions.ch\";".
"drop public database link \"@Konnexions.ch\"".
"drop public database link \"@Konnexions.ch\";".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_directory_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop directory directory_name".
"drop directory directory_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_function_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop function function_name".
"drop function function_name;".
"drop function schema_name.function_name".
"drop function schema_name.function_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop index from :param_1".
"DROP INDEX FROM name_table_1".
"drop index from s.b".
"drop index from schema_table.table_name deferred invalidation".
"drop index from schema_table.table_name deferred invalidation;".
"drop index from schema_table.table_name force deferred invalidation".
"drop index from schema_table.table_name force deferred invalidation;".
"drop index from schema_table.table_name force immediate invalidation".
"drop index from schema_table.table_name force immediate invalidation;".
"drop index from schema_table.table_name force".
"drop index from schema_table.table_name force;".
"drop index from schema_table.table_name immediate invalidation".
"drop index from schema_table.table_name immediate invalidation;".
"drop index from schema_table.table_name online deferred invalidation".
"drop index from schema_table.table_name online deferred invalidation;".
"drop index from schema_table.table_name online force deferred invalidation".
"drop index from schema_table.table_name online force deferred invalidation;".
"drop index from schema_table.table_name online force immediate invalidation".
"drop index from schema_table.table_name online force immediate invalidation;".
"drop index from schema_table.table_name online force".
"drop index from schema_table.table_name online force;".
"drop index from schema_table.table_name online immediate invalidation".
"drop index from schema_table.table_name online immediate invalidation;".
"drop index from schema_table.table_name online".
"drop index from schema_table.table_name online;".
"drop index from schema_table.table_name".
"drop index from schema_table.table_name;".
"drop index from table_1".
"drop index index_1 from :param_1".
"drop index index_1 from table_1".
"drop index index_1".
"DROP INDEX name_index_1 FROM name_table_1".
"DROP INDEX name_schema_1.name_index_1 FROM name_table_1".
"drop index s.a from s.b".
"drop index s.a".
"drop index schema_index.index_name deferred invalidation".
"drop index schema_index.index_name deferred invalidation;".
"drop index schema_index.index_name force deferred invalidation".
"drop index schema_index.index_name force deferred invalidation;".
"drop index schema_index.index_name force immediate invalidation".
"drop index schema_index.index_name force immediate invalidation;".
"drop index schema_index.index_name force".
"drop index schema_index.index_name force;".
"drop index schema_index.index_name from schema_table.table_name deferred invalidation".
"drop index schema_index.index_name from schema_table.table_name deferred invalidation;".
"drop index schema_index.index_name from schema_table.table_name force deferred invalidation".
"drop index schema_index.index_name from schema_table.table_name force deferred invalidation;".
"drop index schema_index.index_name from schema_table.table_name force immediate invalidation".
"drop index schema_index.index_name from schema_table.table_name force immediate invalidation;".
"drop index schema_index.index_name from schema_table.table_name force".
"drop index schema_index.index_name from schema_table.table_name force;".
"drop index schema_index.index_name from schema_table.table_name immediate invalidation".
"drop index schema_index.index_name from schema_table.table_name immediate invalidation;".
"drop index schema_index.index_name from schema_table.table_name online deferred invalidation".
"drop index schema_index.index_name from schema_table.table_name online deferred invalidation;".
"drop index schema_index.index_name from schema_table.table_name online force deferred invalidation".
"drop index schema_index.index_name from schema_table.table_name online force deferred invalidation;".
"drop index schema_index.index_name from schema_table.table_name online force immediate invalidation".
"drop index schema_index.index_name from schema_table.table_name online force immediate invalidation;".
"drop index schema_index.index_name from schema_table.table_name online force".
"drop index schema_index.index_name from schema_table.table_name online force;".
"drop index schema_index.index_name from schema_table.table_name online immediate invalidation".
"drop index schema_index.index_name from schema_table.table_name online immediate invalidation;".
"drop index schema_index.index_name from schema_table.table_name online".
"drop index schema_index.index_name from schema_table.table_name online;".
"drop index schema_index.index_name from schema_table.table_name".
"drop index schema_index.index_name from schema_table.table_name;".
"drop index schema_index.index_name immediate invalidation".
"drop index schema_index.index_name immediate invalidation;".
"drop index schema_index.index_name online deferred invalidation".
"drop index schema_index.index_name online deferred invalidation;".
"drop index schema_index.index_name online force deferred invalidation".
"drop index schema_index.index_name online force deferred invalidation;".
"drop index schema_index.index_name online force immediate invalidation".
"drop index schema_index.index_name online force immediate invalidation;".
"drop index schema_index.index_name online force".
"drop index schema_index.index_name online force;".
"drop index schema_index.index_name online immediate invalidation".
"drop index schema_index.index_name online immediate invalidation;".
"drop index schema_index.index_name online".
"drop index schema_index.index_name online;".
"drop index schema_index.index_name".
"drop index schema_index.index_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_materialized_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop materialized view materialized_view_name preserve table".
"drop materialized view materialized_view_name preserve table;".
"drop materialized view materialized_view_name".
"drop materialized view materialized_view_name;".
"drop materialized view schema_name.materialized_view_name preserve table".
"drop materialized view schema_name.materialized_view_name preserve table;".
"drop materialized view schema_name.materialized_view_name".
"drop materialized view schema_name.materialized_view_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop package body package_name".
"drop package body package_name;".
"drop package body schema_name.package_name".
"drop package body schema_name.package_name;".
"drop package package_name".
"drop package package_name;".
"drop package schema_name.package_name".
"drop package schema_name.package_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_procedure_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop procedure procedure_name".
"drop procedure procedure_name;".
"drop procedure schema_name.procedure_name".
"drop procedure schema_name.procedure_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_profile_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop profile profile_name cascade".
"drop profile profile_name cascade;".
"drop profile profile_name".
"drop profile profile_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"DROP ROLE name_role_1".
"drop role role_1".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_sequence_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop sequence sequence_name".
"drop sequence sequence_name;".
"drop sequence schema_name.sequence_name".
"drop sequence schema_name.sequence_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop public synonym schema_name.synonym_name force".
"drop public synonym schema_name.synonym_name force;".
"drop public synonym schema_name.synonym_name".
"drop public synonym schema_name.synonym_name;".
"drop public synonym synonym_name force".
"drop public synonym synonym_name force;".
"drop public synonym synonym_name".
"drop public synonym synonym_name;".
"drop synonym schema_name.synonym_name force".
"drop synonym schema_name.synonym_name force;".
"drop synonym schema_name.synonym_name".
"drop synonym schema_name.synonym_name;".
"drop synonym synonym_name force".
"drop synonym synonym_name force;".
"drop synonym synonym_name".
"drop synonym synonym_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop anything_else table test".
"drop bag table test".
"drop cluster anything_else table test".
"drop cluster anything_random table if exists test_1, test_2".
"drop cluster anything_random table test".
"drop cluster bag table test".
"drop cluster ordered_set table test".
"drop cluster set table test".
"drop cluster table test".
"drop diese table wwe_table_2".
"drop imem_dal_skvh table :param_1 cascade constraints".
"drop imem_dal_skvh table :param_1".
"drop imem_dal_skvh table if exists :param_1 cascade constraints".
"drop imem_dal_skvh table if exists :param_1".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1 cascade constraints purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1 cascade constraints;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1 purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1;".
"drop imem_dal_skvh table if exists table_1 cascade constraints".
"drop imem_dal_skvh table if exists table_1".
"drop imem_dal_skvh table if exists table_1,:param_2 cascade constraints".
"drop imem_dal_skvh table if exists table_1,:param_2".
"drop imem_dal_skvh table if exists table_1,table_2 cascade constraints".
"drop imem_dal_skvh table if exists table_1,table_2".
"drop imem_dal_skvh table schema_name_1.table_name_1 cascade constraints purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1 cascade constraints;".
"drop imem_dal_skvh table schema_name_1.table_name_1 purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2;".
"drop imem_dal_skvh table schema_name_1.table_name_1;".
"drop imem_dal_skvh table skvhtest".
"drop imem_dal_skvh table table_1 cascade constraints".
"drop imem_dal_skvh table table_1".
"drop imem_dal_skvh table table_1,:param_2 cascade constraints".
"drop imem_dal_skvh table table_1,:param_2".
"drop imem_dal_skvh table table_1,table_2 cascade constraints".
"drop imem_dal_skvh table table_1,table_2".
"drop imem_dal_skvh table test".
"drop local anything_else table test".
"drop local bag table test".
"drop local bag table test_1, test_2".
"drop local ordered_set table test".
"drop local set table if exists table_1, table_2".
"drop local set table schema_1.table_1".
"drop local set table table_1".
"drop local set table test".
"drop local table fun_test".
"drop local table table_1".
"drop local table test".
"drop ordered_set table test".
"drop schema anything_else table test".
"drop schema bag table test".
"drop schema ordered_set table test".
"drop schema set table test".
"drop schema table test".
"drop schema table test".
"drop set table if exists table_1, table_2, table_3".
"drop set table table_1".
"drop set table table_1, table_2".
"drop set table test".
"drop table :param_1 cascade constraints".
"drop table :param_1".
"drop table \"^&()\"".
"drop table if exists :param_1 cascade constraints".
"drop table if exists :param_1".
"drop table if exists :param_1,table_2 cascade constraints".
"drop table if exists :param_1,table_2".
"drop table if exists name_table_1 cascade constraints".
"drop table if exists name_table_1".
"drop table if exists name_table_1,name_table_2 cascade constraints".
"drop table if exists name_table_1,name_table_2".
"drop table if exists schema_name_1.table_name_1 cascade constraints purge;".
"drop table if exists schema_name_1.table_name_1 cascade constraints;".
"drop table if exists schema_name_1.table_name_1 purge;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2;".
"drop table if exists schema_name_1.table_name_1;".
"drop table if exists table_1 cascade constraints".
"drop table if exists table_1".
"drop table if exists table_1,:param_2 cascade constraints".
"drop table if exists table_1,:param_2".
"drop table if exists table_1,table_2 cascade constraints".
"drop table if exists table_1,table_2".
"drop table name_table_1 cascade constraints".
"drop table name_table_1".
"drop table name_table_1,name_table_2 cascade constraints".
"drop table name_table_1,name_table_2".
"drop table schema_1.table_1".
"drop table schema_name_1.table_name_1 cascade constraints purge;".
"drop table schema_name_1.table_name_1 cascade constraints;".
"drop table schema_name_1.table_name_1 purge;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2;".
"drop table schema_name_1.table_name_1;".
"drop table table_1 cascade constraints".
"drop table table_1".
"drop table table_1,:param_2 cascade constraints".
"drop table table_1,:param_2".
"drop table table_1,table_2 cascade constraints".
"drop table table_1,table_2".
"drop table wwe_table_1".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_tablespace_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop tablespace tablespace_name drop quota including contents and datafiles cascade constraints;".
"drop tablespace tablespace_name drop quota including contents cascade constraints;".
"drop tablespace tablespace_name drop quota including contents keep datafiles cascade constraints;".
"drop tablespace tablespace_name drop quota including contents;".
"drop tablespace tablespace_name drop quota;".
"drop tablespace tablespace_name keep quota including contents and datafiles cascade constraints;".
"drop tablespace tablespace_name keep quota including contents cascade constraints;".
"drop tablespace tablespace_name keep quota including contents keep datafiles cascade constraints;".
"drop tablespace tablespace_name keep quota including contents;".
"drop tablespace tablespace_name keep quota;".
"drop tablespace tablespace_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_trigger_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop trigger trigger_name".
"drop trigger trigger_name;".
"drop trigger schema_name.trigger_name".
"drop trigger schema_name.trigger_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop type schema_name.type_name force".
"drop type schema_name.type_name force;".
"drop type schema_name.type_name validate".
"drop type schema_name.type_name validate;".
"drop type schema_name.type_name".
"drop type schema_name.type_name;".
"drop type type_name force".
"drop type type_name force;".
"drop type type_name validate".
"drop type type_name validate;".
"drop type type_name".
"drop type type_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop type body type_name".
"drop type body type_name;".
"drop type body schema_name.type_name".
"drop type body schema_name.type_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"DROP USER name_user_1 CASCADE".
"DROP USER name_user_1".
"drop user user_name cascade".
"drop user user_name cascade;".
"drop user user_name".
"drop user user_name;".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"drop view schema_name.view_name cascade constraints".
"drop view schema_name.view_name cascade constraints;".
"drop view schema_name.view_name".
"drop view schema_name.view_name;".
"drop view view_name cascade constraints".
"drop view view_name cascade constraints;".
"drop view view_name".
"drop view view_name;".

%% -----------------------------------------------------------------------------
%% TESTS: DROP INDEX & DROP ROLE & DROP TABLE & DROP USER
%% =============================================================================
