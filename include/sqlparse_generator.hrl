%% -----------------------------------------------------------------------------
%%
%% sqlparse_generator.hrl: SQL - test data generator.
%%
%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
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

-define(ALL_CLAUSE_PERFORMANCE, [
    special,
    sql_list
]).
-define(ALL_CLAUSE_RELIABILITY, [
    special,
    sql_list
]).
-define(ALL_CLAUSE_RELIABILITY_SQL, [
    alter_user_def,
    close_statement,
    commit_statement,
    create_index_def,
    create_role_def,
    create_table_def,
    create_user_def,
    cursor_def,
    delete_statement,
    drop_index_def,
    drop_role_def,
    drop_table_def,
    drop_user_def,
    fetch_statement,
    grant_def,
    insert_statement,
    open_statement,
    procedure_call,
    revoke_def,
    rollback_statement,
    schema,
    select_statement,
    truncate_table,
    update_statement,
    view_def,
    whenever
]).
-define(ALL_CLAUSE_RELIABILITY_SQL_DETAILED, [
%%%% Level 01 ..........................
%%    approxnum,
%%    atom,
%%    commit_statement,
%%    comparison,
%%    funs,
%%    hint,
%%    intnum,
%%    json,
%%    name,
%%    outer_join_type,
%%    parameter,
%%    rollback_statement,
%%    string,
%%    tbl_scope,
%%    tbl_type,
%%    with_grant_option,
%%    with_revoke_option,
%%%% Level 02 ..........................
%%    column_commalist,
%%    column_ref,
%%    create_role_def,
%%    cursor,
%%    drop_role_def,
%%    drop_user_def,
%%    extra,
%%    grantee,
%%    identified,
%%    index_name,
%%    quota,
%%    role_list,
%%    sgn_num,
%%    system_privilege,
%%    table_name,
%%    when_action,
%%%% Level 03 ..........................
%%    close_statement,
%%    data_type,
%%    open_statement,
%%    parameter_ref,
%%    system_privilege_list,
%%    table,
%%    truncate_table,
%%    user_opt,
%%    user_role,
%%    whenever,
%%%% Level 04 ..........................
%%    create_index_def,
%%    create_user_def,
%%    drop_index_def,
%%    drop_table_def,
%%    on_obj_clause,
%%    proxy_with,
%%%% Level 05 ..........................
%%    db_user_proxy,
%%    grant_def,
%%    revoke_def,
%%    target_commalist,
%%%% Level 06 ..........................
%%    fetch_statement,
%%    proxy_clause,
%%%% Level 07 ..........................
%%    alter_user_def,
%%%% Level 11 ..........................
%%    fun_args,
%%    scalar_exp,
%%    schema,
%%    sql_list,
%%%% Level 12 ..........................
%%    between_predicate,
%%    function_ref,
%%    like_predicate,
%%    ordering_spec,
%%    scalar_opt_as_exp,
%%    test_for_null,
%%%% Level 13 ..........................
%%    assignment,
%%    case_when_then,
%%    column_ref_commalist,
%%    comparison_predicate,
%%    function_ref_list,
%%    order_by_clause,
%%    scalar_exp_commalist,
%%    table_constraint_def,
%%%% Level 14 ..........................
%%    assignment_commalist,
%%    case_when_then_list,
%%    create_table_def,
%%    procedure_call,
%%    query_partition_clause,
%%    search_condition,
%%%% Level 15 ..........................
%%    case_when_exp,
%%    column_def_opt,
%%    hierarchical_query_clause,
%%    returning,
%%    where_clause,
%%%% Level 16 ..........................
%%    column_def,
%%    delete_statement,
%%    select_field,
%%    update_statement,
%%%% Level 17 ..........................
%%    select_field_commalist,
%%%% Level 18 ..........................
%%    join_on_or_using_clause,
%%    selection,
%%    table_exp,
%%%% Level 19 ..........................
%%    inner_cross_join,
%%    outer_join,
%%    query_spec,
%%%% Level 20 ..........................
%%    insert_statement,
%%    join,
%%    join_clause,
%%    query_term,
%%    view_def,
%%%% Level 21 ..........................
%%    query_exp,
%%%% Level 22 ..........................
%%    all_or_any_predicate,
%%    cursor_def,
%%    existence_test,
%%    fun_arg,
%%    in_predicate,
%%    join_ref,
%%    scalar_sub_exp,
%%    table_ref,
%%%% Level 23 ..........................
%%    from_column,
%% Level 99 ..........................
    special
]).

-define(CODE_TEMPLATES, code_templates).
-define(CREATE_CODE_END,
    [_CodeFirst | _] = Code,
    {_, _MemorySize} = erlang:process_info(self(), memory),
    ?debugFmt("~ntime (ms)          ===  ~12.. B rule: ~s ~n", [erlang:monotonic_time(1000) - _Start, atom_to_list(Rule)]),
    ?debugFmt("~nmemory (bytes)     ===  ~12.. B rule: ~s ~n", [_MemorySize, atom_to_list(Rule)]),
    ?debugFmt("~ncode size (bytes) <===  ~12.. B rule: ~s ~n", [length(_CodeFirst), atom_to_list(Rule)]),
    ok
).
-define(CREATE_CODE_START,
    [garbage_collect(Pid) || Pid <- processes()],
    _Start = erlang:monotonic_time(1000)
).

-define(F_RANDOM, fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end).

-define(GENERATE_COMPACTED, list_to_atom(string:to_lower(os:getenv("GENERATE_COMPACTED", "true")))).          % TRUE: compacted / FALSE: detailed.
-define(GENERATE_CT, list_to_atom(string:to_lower(os:getenv("GENERATE_CT", "true")))).
-define(GENERATE_EUNIT, list_to_atom(string:to_lower(os:getenv("GENERATE_EUNIT", "true")))).
-define(GENERATE_PERFORMANCE, list_to_atom(string:to_lower(os:getenv("GENERATE_PERFORMANCE", "true")))).
-define(GENERATE_RELIABILITY, list_to_atom(string:to_lower(os:getenv("GENERATE_RELIABILITY", "true")))).

-define(MAX_BASIC, list_to_integer(string:to_lower(os:getenv("MAX_BASIC", "250")))).
-define(MAX_SQL, ?MAX_BASIC * 16).
-define(MAX_STATEMENT_COMPLEX, ?MAX_BASIC * 8).
-define(MAX_STATEMENT_SIMPLE, ?MAX_BASIC * 2).

-define(PATH_CT, "test").
-define(PATH_EUNIT, "test").

-define(TIMETRAP_MINUTES, 15).
