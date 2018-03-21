%% -*- erlang -*-
%% -*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: Multiple Statements
%% -----------------------------------------------------------------------------

"CREATE TABLE hr_regions( region_id integer NOT NULL PRIMARY KEY, region_name varchar2(25));CREATE TABLE hr_jobs(job_id varchar2(10) NOT NULL PRIMARY KEY, job_title varchar2(35) NOT NULL, min_salary number(6,0), max_salary number(6,0));".
"insert into hr_regions (region_id,region_name) values (1,'Europe');insert into hr_regions (region_id,region_name) values (2,'Americas');insert into hr_regions (region_id,region_name) values (3,'Asia');insert into hr_regions (region_id,region_name) values (4,'Middle East and Africa');".
"insert into hr_regions (region_id,region_name) values (1,'Europe');name_label_1;insert into hr_regions (region_id,region_name) values (2,'Americas');insert into hr_regions (region_id,region_name) values (3,'Asia');name_label_2;insert into hr_regions (region_id,egion_name) values (4,'Middle East and Africa');name_label_3".

%% -----------------------------------------------------------------------------
%% TESTS: Multiple Statements
%% =============================================================================
