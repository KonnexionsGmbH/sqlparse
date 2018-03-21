%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: SCHEMA
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"create schema authorization schema_1 create table table_1(column_1 char)".
"create schema authorization schema_1 create table table_1(column_1 char) grant admin to public create view table_1 as select * from dual".
"create schema authorization schema_1 create view table_1 as select * from dual".
"create schema authorization schema_1 grant admin to public".
"create schema authorization schema_1".

%% -----------------------------------------------------------------------------
%% TESTS: SCHEMA
%% =============================================================================
