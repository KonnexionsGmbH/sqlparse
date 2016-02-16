%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS CLOSE
%%

% ? "CLOSE name_cursor_1".

%%
%% TESTS DECLARE
%%

% ? "DECLARE name_cursor_1 CURSOR FOR SELECT * FROM name_table_1".


%%
%% TESTS DELETE
%%

% ? "DELETE FROM table_name WHERE CURRENT OF name_cursor".

%%
%% TESTS FETCH
%%

% ? "FETCH name_cursor_1 INTO name_column_1".

%%
%% TESTS OPEN
%%

% ? "OPEN name_cursor_1".

%%
%% TESTS UPDATE
%%

% ? "UPDATE name_table SET name_column_1 = :value_1 WHERE CURRENT OF name_cursor".
% ? "UPDATE name_table SET name_column_1 = :value_1, name_column_2 = :value_2 WHERE CURRENT OF name_cursor".


