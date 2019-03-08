%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

% ==============================================================================

"drop imem_dal_skvh table schema_name_1.table_name_1 cascade constraints purge;".
"drop table schema_name_1.table_name_1 cascade constraints purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1 cascade constraints purge;".
"drop table if exists schema_name_1.table_name_1 cascade constraints purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1 cascade constraints;".
"drop table schema_name_1.table_name_1 cascade constraints;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1 cascade constraints;".
"drop table if exists schema_name_1.table_name_1 cascade constraints;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 cascade constraints;".
"drop imem_dal_skvh table schema_name_1.table_name_1 purge;".
"drop table schema_name_1.table_name_1 purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1 purge;".
"drop table if exists schema_name_1.table_name_1 purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2 purge;".
"drop imem_dal_skvh table schema_name_1.table_name_1;".
"drop table schema_name_1.table_name_1;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1;".
"drop table if exists schema_name_1.table_name_1;".
"drop imem_dal_skvh table schema_name_1.table_name_1, schema_name_2.table_name_2;".
"drop table schema_name_1.table_name_1, schema_name_2.table_name_2;".
"drop imem_dal_skvh table if exists schema_name_1.table_name_1, schema_name_2.table_name_2;".
"drop table if exists schema_name_1.table_name_1, schema_name_2.table_name_2;".
