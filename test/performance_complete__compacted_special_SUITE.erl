%%%-------------------------------------------------------------------
%%% File        : performance_complete__compacted_special_SUITE.erl
%%% Description : Test Suite for rule: special.
%%%
%%% Created     : 07.11.2017
%%%-------------------------------------------------------------------
-module(performance_complete__compacted_special_SUITE).

-export([
    all/0,
    end_per_suite/1,
    init_per_suite/1,
    suite/0,
    test_compacted/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%--------------------------------------------------------------------

suite() ->
    [
        {timetrap, {minutes, 15}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        test_compacted
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_compacted(_Config) ->
    {ok, _} = sqlparse:parsetree_with_tokens("Select - 10 * -5 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select - 10 / -5 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select - 10 + -5 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select - 10 - -5 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 5 + 7 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 5 - 7 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 5 * 7 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 5 / 7 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 + 3 * 3 + 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 - 3 * 3 - 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 + 3 / 3 + 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 - 3 / 3 - 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 * 3 + 3 * 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 * 3 - 3 * 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 / 3 + 3 / 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select 2 / 3 - 3 / 4 From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 + 3) * (3 - 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 + 3) / (3 - 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 * 3) - (3 / 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 / 3) + (3 * 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (Select * From Dual) * (3 - 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (Select * From Dual) / (3 - 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (Select * From Dual) - (3 / 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (Select * From Dual) + (3 * 4) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 + 3) * (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 + 3) / (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 - 3) * (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 - 3) / (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 * 3) + (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 / 3) + (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 * 3) - (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (2 / 3) - (Select * From Dual) From dual"),
    {ok, _} = sqlparse:parsetree_with_tokens("Create Index a On b (|:d{}|)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Create Index a On b (c | |:d{}|)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select |:a:b| From x"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select column_name|:a:b| From x"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select table_name.column_name|:a:b| From x"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select column_name From x"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select table_name.column_name From x"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select schema_name.table_name.column_name From x"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All Null)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All 5)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All 'text')"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All |:_a1:f()|)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All name|:_a1:f()|)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All name1.name2|:_a1:f()|)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All name)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All name1.name2)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Call Upper (All name1.name2.name3)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Create Table table_name (column_name name (1))"),
    {ok, _} = sqlparse:parsetree_with_tokens("Create Table table_name (column_name name (-1))"),
    {ok, _} = sqlparse:parsetree_with_tokens("Create Table table_name (column_name name (1, 2))"),
    {ok, _} = sqlparse:parsetree_with_tokens("Create Table table_name (column_name name (-1, -2))"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name Return column_name_a Into column_name_b"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name Values (1)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name Select source_column From source_table"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name (column_name) Values (1)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name ('column_string') Values (1)"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name (column_name) Select source_column From source_table"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name ('column_string') Select source_column From source_table"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select * From table_name Order By column_name"),
    {ok, _} = sqlparse:parsetree_with_tokens("Insert Into table_name (column_name) Values (1) Return result_column Into show_column"),
    {ok, _} = sqlparse:parsetree_with_tokens("Select (Select * From dual) From dual"),
    ok.
