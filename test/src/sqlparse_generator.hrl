%% -----------------------------------------------------------------------------
%%
%% sqlparse_generator.hrl: SQL - test data generator.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-ifndef(SQLPARSE_GENERATOR_HRL).

-define(SQLPARSE_GENERATOR_HRL, true).

-include("sqlparse.hrl").

-define(ALIVE_COUNTER, 500).
-define(ALL_CLAUSE_PERFORMANCE, [plsql_block, referenceExamples, special, sql_list]).
-define(ALL_CLAUSE_RELIABILITY, [plsql_block, referenceExamples, special, sql_list]).
-define(
  ALL_CLAUSE_RELIABILITY_PLSQL,
  [
    assignment_statement,
    close_statement,
    commit_statement,
    cursor_def,
    delete_statement,
    fetch_statement,
    function_ref,
    insert_statement,
    open_statement,
    procedure_call,
    rollback_statement,
    select_statement,
    update_statement,
    whenever
  ]
).
-define(
  ALL_CLAUSE_RELIABILITY_SQL,
  [
    alter_user_def,
    create_index_def,
    create_role_def,
    create_table_def,
    create_user_def,
    delete_statement,
    drop_cluster_def,
    drop_context_def,
    drop_database_def,
    drop_database_link_def,
    drop_directory_def,
    drop_function_def,
    drop_index_def,
    drop_materialized_view_def,
    drop_package_def,
    drop_procedure_def,
    drop_profile_def,
    drop_role_def,
    drop_sequence_def,
    drop_synonym_def,
    drop_table_def,
    drop_tablespace_def,
    drop_trigger_def,
    drop_type_def,
    drop_type_body_def,
    drop_user_def,
    drop_view_def,
    grant_def,
    insert_statement,
    revoke_def,
    schema,
    select_statement,
    truncate_cluster,
    truncate_table,
    update_statement,
    view_def
  ]
).
-define(
  ALL_CLAUSE_RELIABILITY_PLSQL_DETAILED,
  [
    %%    assignment_statement,
    %%    close_statement,
    %%    commit_statement,
    %%    cursor_def,
    %%    delete_statement,
    %%    fetch_statement,
    %%    function_ref,
    %%    insert_statement,
    %%    open_statement,
    %%    procedure_call,
    %%    rollback_statement,
    %%    select_statement,
    %%    update_statement,
    %%    whenever,
    %% .............................................................................
    special
  ]
).
-define(
  ALL_CLAUSE_RELIABILITY_SQL_DETAILED,
  [
    %%    all_or_any_predicate,
    %%    alter_user_def,
    %%    approxnum,
    %%    assignment,
    %%    assignment_commalist,
    %%    atom,
    %%    base_table_element,
    %%    between_predicate,
    %%    case_when_exp,
    %%    case_when_then,
    %%    case_when_then_list,
    %%    column,
    %%    column_commalist,
    %%    column_def,
    %%    column_def_opt,
    %%    column_ref,
    %%    column_ref_commalist,
    %%    comparison,
    %%    comparison_predicate,
    %%    create_index_def,
    %%    create_role_def,
    %%    create_table_def,
    %%    create_user_def,
    %%    cursor,
    %%    data_type,
    %%    db_user_proxy,
    %%    dblink,
    %%    delete_statement,
    %%    drop_cluster_def,
    %%    drop_context_def,
    %%    drop_database_def,
    %%    drop_database_link_def,
    %%    drop_directory_def,
    %%    drop_function_def,
    %%    drop_index_def,
    %%    drop_materialized_view_def,
    %%    drop_package_def,
    %%    drop_procedure_def,
    %%    drop_profile_def,
    %%    drop_role_def,
    %%    drop_sequence_def,
    %%    drop_synonym_def,
    %%    drop_table_def,
    %%    drop_tablespace_def,
    %%    drop_trigger_def,
    %%    drop_type_def,
    %%    drop_type_body_def,
    %%    drop_user_def,
    %%    drop_view_def,
    %%    existence_test,
    %%    extra,
    %%    from_column,
    %%    fun_arg,
    %%    fun_args,
    %%    function_ref_json,
    %%    funs,
    %%    grant_def,
    %%    grantee_identified_by,
    %%    grantee_revokee,
    %%    grantee_revokee_commalist,
    %%    hierarchical_query_clause,
    %%    hint,
    %%    identified,
    %%    in_predicate,
    %%    index_name,
    %%    inner_cross_join,
    %%    insert_statement,
    %%    intnum,
    %%    join,
    %%    join_clause,
    %%    join_on_or_using_clause,
    %%    join_ref,
    %%    json,
    %%    like_predicate,
    %%    literal,
    %%    manipulative_statement,
    %%    name,
    %%    object_privilege,
    %%    object_privilege_list,
    %%    object_with_grant_option,
    %%    object_with_revoke_option,
    %%    on_obj_clause,
    %%    order_by_clause,
    %%    ordering_spec,
    %%    outer_join,
    %%    outer_join_type,
    %%    parameter,
    %%    parameter_ref,
    %%    plsql_block,
    %%    plsql_body,
    %%    predicate,
    %%    proxy_clause,
    %%    proxy_with,
    %%    query_exp,
    %%    query_partition_clause,
    %%    query_spec,
    %%    query_term,
    %%    query_term_json,
    %%    quota,
    %%    referenceExamples,
    %%    returning,
    %%    revoke_def,
    %%    role_list,
    %%    scalar_exp,
    %%    scalar_exp_commalist,
    %%    scalar_opt_as_exp,
    %%    scalar_sub_exp,
    %%    scalar_sub_exp_json,
    %%    schema,
    %%    search_condition,
    %%    schema_element,
    %%    select_field,
    %%    select_field_commalist,
    %%    select_statement,
    %%    selection,
    %%    sgn_num,
    %%    spec_item,
    %%    sql,
    %%    sql_list,
    %%    string,
    %%    subquery,
    %%    system_privilege,
    %%    system_privilege_list,
    %%    system_with_grant_option,
    %%    table,
    %%    table_alias,
    %%    table_coll_expr,
    %%    table_constraint_def,
    %%    table_dblink,
    %%    table_exp,
    %%    table_ref,
    %%    target,
    %%    target_commalist,
    %%    tbl_scope,
    %%    tbl_type,
    %%    test_for_null,
    %%    truncate_cluster,
    %%    truncate_table,
    %%    update_statement,
    %%    user_opt,
    %%    user_role,
    %%    view_def,
    %%    when_action,
    %%    where_clause,
    %% .............................................................................
    special
  ]
).
-define(CODE_TEMPLATES, code_templates).
-define(
  CREATE_CODE_END,
  [_CodeFirst | _] = Code,
  {_, _MemorySize} = erlang:process_info(self(), memory),
  ?D(
    "~n time (ms)          ===  ~12.. B rule: ~s ~n",
    [erlang:monotonic_time(1000) - _Start, atom_to_list(Rule)]
  ),
  ?D("~n memory (bytes)     ===  ~12.. B rule: ~s ~n", [_MemorySize, atom_to_list(Rule)]),
  ?D("~n code size (bytes) <===  ~12.. B rule: ~s ~n", [length(_CodeFirst), atom_to_list(Rule)]),
  ok
).
-define(
  CREATE_CODE_START,
  [garbage_collect(Pid) || Pid <- processes()],
  _Start = erlang:monotonic_time(1000)
).
-define(F_RANDOM, fun (X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end).
-define(GENERATE_COMPACTED, list_to_atom(string:to_lower(os:getenv("GENERATE_COMPACTED", "true")))).

% TRUE: compacted / FALSE: detailed.
-define(GENERATE_CT, list_to_atom(string:to_lower(os:getenv("GENERATE_CT", "true")))).
-define(GENERATE_EUNIT, list_to_atom(string:to_lower(os:getenv("GENERATE_EUNIT", "true")))).
-define(
  GENERATE_PERFORMANCE,
  list_to_atom(string:to_lower(os:getenv("GENERATE_PERFORMANCE", "true")))
).
-define(
  GENERATE_RELIABILITY,
  list_to_atom(string:to_lower(os:getenv("GENERATE_RELIABILITY", "true")))
).
-define(LOGGING, list_to_atom(string:to_lower(os:getenv("LOGGING", "false")))).
-define(MAX_BASIC, list_to_integer(os:getenv("MAX_BASIC", "250"))).
-define(MAX_SQL, ?MAX_BASIC * 16).
-define(MAX_STATEMENT_COMPLEX, ?MAX_BASIC * 8).
-define(MAX_STATEMENT_SIMPLE, ?MAX_BASIC * 2).
-define(PATH_CT, "test/generated/ct/").
-define(PATH_EUNIT, "test/generated/eunit/").
-define(
  TESTS_FROM_DATABASE_SQL_LANGUAGE_REFERENCE_V12C_2,
  [
    % ==========================================================================
    % Tuple structure: {book, chapter, section, subsection, subsubsection, code}
    % ==========================================================================
    % SQL Language Reference: 14 SQL Statements: CREATE LIBRARY to CREATE SCHEMA
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "14 SQL Statements: CREATE LIBRARY to CREATE SCHEMA",
      "CREATE SCHEMA",
      "",
      "",
      "",
      "\n        CREATE SCHEMA AUTHORIZATION oe\n           CREATE TABLE new_product\n              (color VARCHAR2(10), quantity NUMBER)\n           CREATE VIEW new_product_view\n              AS SELECT color, quantity FROM new_product WHERE color = 'RED'\n           GRANT select ON new_product_view TO hr;\n    "
    },
    % ==========================================================================
    % SQL Language Reference: 18 SQL Statements: DROP TABLE to LOCK TABLE
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "18 SQL Statements: DROP TABLE to LOCK TABLE",
    %%        "GRANT",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        GRANT CREATE SESSION
    %%           TO hr;
    %%    "},
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "18 SQL Statements: DROP TABLE to LOCK TABLE",
    %%        "GRANT",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        GRANT CREATE SESSION
    %%          TO hr, newuser IDENTIFIED BY \"password1\", \"password2\";
    %%    "},
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "18 SQL Statements: DROP TABLE to LOCK TABLE",
    %%        "GRANT",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        GRANT
    %%             CREATE ANY MATERIALIZED VIEW
    %%           , ALTER ANY MATERIALIZED VIEW
    %%           , DROP ANY MATERIALIZED VIEW
    %%           , QUERY REWRITE
    %%           , GLOBAL QUERY REWRITE
    %%           TO dw_manager
    %%           WITH ADMIN OPTION;
    %%    "},
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "18 SQL Statements: DROP TABLE to LOCK TABLE",
      "GRANT",
      "",
      "",
      "",
      "\n        GRANT dw_manager\n           TO sh\n           WITH ADMIN OPTION;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "18 SQL Statements: DROP TABLE to LOCK TABLE",
      "GRANT",
      "",
      "",
      "",
      "\n        GRANT dw_manager\n           TO sh\n           WITH DELEGATE OPTION;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "18 SQL Statements: DROP TABLE to LOCK TABLE",
      "GRANT",
      "",
      "",
      "",
      "\n        GRANT SELECT ON sh.sales TO warehouse_user;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "18 SQL Statements: DROP TABLE to LOCK TABLE",
      "GRANT",
      "",
      "",
      "",
      "\n        GRANT warehouse_user TO dw_manager;\n    "
    },
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "18 SQL Statements: DROP TABLE to LOCK TABLE",
    %%        "GRANT",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        GRANT INHERIT PRIVILEGES ON USER sh TO hr;
    %%      )
    %%    "},
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "18 SQL Statements: DROP TABLE to LOCK TABLE",
    %%        "GRANT",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        GRANT READ ON DIRECTORY bfile_dir TO hr
    %%           WITH GRANT OPTION;
    %%    "},
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "18 SQL Statements: DROP TABLE to LOCK TABLE",
      "GRANT",
      "",
      "",
      "",
      "\n        GRANT ALL ON bonuses TO hr\n           WITH GRANT OPTION;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "18 SQL Statements: DROP TABLE to LOCK TABLE",
      "GRANT",
      "",
      "",
      "",
      "\n        GRANT SELECT, UPDATE\n           ON emp_view TO PUBLIC;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "18 SQL Statements: DROP TABLE to LOCK TABLE",
      "GRANT",
      "",
      "",
      "",
      "\n        GRANT SELECT\n           ON oe.customers_seq TO hr;\n    "
    },
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "18 SQL Statements: DROP TABLE to LOCK TABLE",
    %%        "GRANT",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        GRANT REFERENCES (employee_id),
    %%              UPDATE (employee_id, salary, commission_pct)
    %%           ON hr.employees
    %%           TO oe;
    %%    "},
    % ==========================================================================
    % SQL Language Reference: 19 SQL Statements: MERGE to UPDATE
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE DROP ANY TABLE\n            FROM hr, oe;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE dw_manager\n            FROM sh;\n    "
    },
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "19 SQL Statements: MERGE to UPDATE",
    %%        "REVOKE",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        REVOKE CREATE TABLESPACE
    %%           FROM dw_manager;
    %%    "},
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE dw_user\n          FROM dw_manager;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE DELETE\n           ON orders FROM hr;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE ALL\n           ON orders FROM hr;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE UPDATE\n            ON emp_details_view FROM public;\n    "
    },
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "19 SQL Statements: MERGE to UPDATE",
    %%        "REVOKE",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        REVOKE INHERIT PRIVILEGES ON USER sh FROM hr;
    %%    "},
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE SELECT\n            ON hr.departments_seq FROM oe;\n    "
    },
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE REFERENCES\n            ON hr.employees\n            FROM oe\n            CASCADE CONSTRAINTS;\n    "
    },
    %%    % --------------------------------------------------------------------------
    %%    {
    %%        "SQL Language Reference",
    %%        "19 SQL Statements: MERGE to UPDATE",
    %%        "REVOKE",
    %%        "",
    %%        "",
    %%        "",
    %%        "
    %%        REVOKE READ ON DIRECTORY bfile_dir FROM hr;
    %%    "},
    % --------------------------------------------------------------------------
    {
      "SQL Language Reference",
      "19 SQL Statements: MERGE to UPDATE",
      "REVOKE",
      "",
      "",
      "",
      "\n        REVOKE UPDATE ON hr.employees FROM oe;\n    "
    }
  ]
).
-define(TIMETRAP_MINUTES, 15).
-endif.
