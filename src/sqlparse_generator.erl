%% -----------------------------------------------------------------------------
%%
%% sqlparse_generator.erl: SQL - test data generator.
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

-module(sqlparse_generator).

-export([generate/0]).

-define(ALL_CLAUSE_CT_PERFORMANCE, [
    sql_list
]).
-define(ALL_CLAUSE_CT_RELIABILITY, [
    sql_list
]).
-define(ALL_CLAUSE_CT_RELIABILITY_SQL, [
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
-define(ALL_CLAUSE_CT_RELIABILITY_SQL_DETAILED, [
    special
]).

-define(ALL_CLAUSE_EUNIT_RELIABILITY, [
%%    from_clause,
%%    group_by_clause,
%%    having_clause,
%%    hierarchical_query_clause,
%%    order_by_clause,
%%    query_spec,
%%    table_exp,
%%    where_clause
]).
-define(ALL_CLAUSE_EUNIT_RELIABILITY_SQL, [
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

-define(MAX_BASIC, 250).
-define(MAX_SQL, ?MAX_BASIC * 16).
-define(MAX_STATEMENT_COMPLEX, ?MAX_BASIC * 8).
-define(MAX_STATEMENT_SIMPLE, ?MAX_BASIC * 2).

-define(PATH_CT, "test").
-define(PATH_EUNIT, "test").

-define(TIMETRAP_MINUTES, 30).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Test Data.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    file:delete(?CODE_TEMPLATES),

    dets:open_file(?CODE_TEMPLATES, [
        {auto_save, 0}
    ]),

    create_code(),

    % performance common tests with compacted test cases
    ok = file_create_ct_all("performance", "complete_", "compacted", ?ALL_CLAUSE_CT_PERFORMANCE),

    % reliability common tests with compacted test cases
    ok = file_create_ct_all("reliability", "complete_", "compacted", ?ALL_CLAUSE_CT_RELIABILITY),

    % reliability common tests with semicolon completion and compacted test cases
    ok = file_create_ct_all("reliability", "semicolon", "compacted", ?ALL_CLAUSE_CT_RELIABILITY_SQL),

    % reliability common tests with semicolon completion and detailed test cases
    ok = file_create_ct_all("reliability", "semicolon", "detailed_", ?ALL_CLAUSE_CT_RELIABILITY_SQL_DETAILED),

    % reliability eunit tests with compacted test cases
    ok = file_create_eunit_all("reliability", "complete_", ?ALL_CLAUSE_EUNIT_RELIABILITY),

    % reliability common tests with semicolon completion and "compacted" test cases
    ok = file_create_eunit_all("reliability", "semicolon", ?ALL_CLAUSE_EUNIT_RELIABILITY_SQL),

    dets:close(?CODE_TEMPLATES).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code base.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code() ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(approxnum),
    create_code(commit_statement),
    create_code(comparison),
    create_code(funs),
    create_code(intnum),
    create_code(hint),
    create_code(json),
    create_code(name),
    create_code(outer_join_type),
    create_code(parameter),
    create_code(rollback_statement),
    create_code(string),
    create_code(tbl_scope),
    create_code(tbl_type),
    create_code(with_grant_option),
    create_code(with_revoke_option),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(column_commalist),
    create_code(column_ref),
    create_code(create_role_def),
    create_code(cursor),
    create_code(drop_role_def),
    create_code(drop_user_def),
    create_code(extra),
    create_code(grantee),
    create_code(identified),
    create_code(index_name),
    create_code(literal),
    create_code(opt_sgn_num),
    create_code(parameter_ref),
    create_code(quota),
    create_code(role_list),
    create_code(system_privilege),
    create_code(table),
    create_code(table_name),
    create_code(when_action),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(atom),
    create_code(close_statement),
    create_code(create_index_def),
    create_code(data_type),
    create_code(drop_index_def),
    create_code(drop_table_def),
    create_code(on_obj_clause),
    create_code(open_statement),
    create_code(proxy_with),
    create_code(system_privilege_list),
    create_code(target_commalist),
    create_code(truncate_table),
    create_code(user_opt),
    create_code(user_role),
    create_code(whenever),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 4
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(create_user_def),
    create_code(db_user_proxy),
    create_code(grant_def),
    create_code(into),
    create_code(revoke_def),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(fetch_statement),
    create_code(proxy_clause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 6
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(alter_user_def),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(table_ref_1),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 8
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code_layer(),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 9
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(join_ref),
    create_code(table_ref),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 10
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code_layer(),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 11
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(create_table_def),
    create_code(delete_statement),
    create_code(insert_statement),
    create_code(update_statement),
    create_code(view_def),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 12
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(manipulative_statement),
    create_code(schema_element),
    create_code(sql_1),
    create_code(sql_list),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 13
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(procedure_call),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(sql),
    create_code(sql_list),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 15
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(special),

    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code layered.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_layer() ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(fun_arg),
    create_code(scalar_sub_exp),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(from_column_commalist),
    create_code(fun_args),
    create_code(scalar_exp),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(between_predicate),
    create_code(from_clause),
    create_code(function_ref),
    create_code(like_predicate),
    create_code(ordering_spec),
    create_code(scalar_opt_as_exp),
    create_code(test_for_null),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 4
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(assignment),
    create_code(column_ref_commalist),
    create_code(function_ref_list),
    create_code(order_by_clause),
    create_code(scalar_exp_commalist),
    create_code(search_condition),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(assignment_commalist),
    create_code(case_when_then),
    create_code(column_def_opt),
    create_code(group_by_clause),
    create_code(having_clause),
    create_code(hierarchical_query_clause),
    create_code(query_partition_clause),
    create_code(table_constraint_def),
    create_code(where_clause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 6
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(case_when_then_list),
    create_code(column_def),
    create_code(table_exp),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(case_when_exp),
    create_code(query_spec),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 8
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(query_term),
    create_code(schema_element_1),
    create_code(select_field_commalist),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 9
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(join_on_or_using_clause),
    create_code(query_exp),
    create_code(schema),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 10
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(all_or_any_predicate),
    create_code(comparison_predicate),
    create_code(cursor_def),
    create_code(existence_test),
    create_code(in_predicate),
    create_code(inner_cross_join),
    create_code(outer_join),
    create_code(predicate),
    create_code(returning),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 11
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(join),
    create_code(join_list),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 12
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(join_clause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 13
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(from_column),

    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Adding parentheses to a query_spec.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bracket_query_spec(Expression) ->
    case string:substr(Expression, 1, 7) == "Select " of
        true -> lists:append(["(", Expression, ")"]);
        _ -> Expression
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% all_or_any_predicate ::= scalar_exp COMPARISON ( 'ANY' | 'ALL' | 'SOME' ) subquery
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(all_or_any_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{comparison, Comparison}] = dets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),
    [{subquery, Subquery}] = dets:lookup(?CODE_TEMPLATES, subquery),
    Subquery_Length = length(Subquery),

    Code =
        [
            lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                " ",
                lists:nth(rand:uniform(Comparison_Length), Comparison),
                case rand:uniform(3) rem 3 of
                    1 -> " Any";
                    2 -> " All";
                    _ -> " Some"
                end,
                " ",
                bracket_query_spec(lists:nth(rand:uniform(Subquery_Length), Subquery))
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% alter_user_def ::= ( 'ALTER' 'USER' NAME ( ',' NAME )* proxy_clause )
%%                  | ( 'ALTER' 'USER' NAME spec_item ( spec_item )* )
%%                  | ( 'ALTER' 'USER' NAME NAME NAME )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(alter_user_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{proxy_clause, Proxy_Clause}] = dets:lookup(?CODE_TEMPLATES, proxy_clause),
    Proxy_Clause_Length = length(Proxy_Clause),
    [{spec_item, Spec_Item}] = dets:lookup(?CODE_TEMPLATES, spec_item),
    Spec_Item_Length = length(Spec_Item),

    Code =
        [
            lists:append([
                "Alter User",
                " ",
                lists:nth(rand:uniform(Name_Length), Name),
                case rand:uniform(3) rem 3 of
                    1 -> lists:append([
                        case rand:uniform(4) rem 4 of
                            1 -> lists:append([
                                ",",
                                lists:nth(rand:uniform(Name_Length), Name),
                                ",",
                                lists:nth(rand:uniform(Name_Length), Name),
                                ",",
                                lists:nth(rand:uniform(Name_Length), Name)
                            ]);
                            2 -> lists:append([
                                ",",
                                lists:nth(rand:uniform(Name_Length), Name),
                                ",",
                                lists:nth(rand:uniform(Name_Length), Name)
                            ]);
                            3 -> lists:append([
                                ",",
                                lists:nth(rand:uniform(Name_Length), Name)
                            ]);
                            _ -> []
                        end,
                        " ",
                        lists:nth(rand:uniform(Proxy_Clause_Length), Proxy_Clause)
                    ]);
                    2 -> case rand:uniform(3) rem 3 of
                             1 -> lists:append([
                                 " ",
                                 lists:nth(rand:uniform(Spec_Item_Length), Spec_Item),
                                 " ",
                                 lists:nth(rand:uniform(Spec_Item_Length), Spec_Item),
                                 " ",
                                 lists:nth(rand:uniform(Spec_Item_Length), Spec_Item)
                             ]);
                             2 -> lists:append([
                                 " ",
                                 lists:nth(rand:uniform(Spec_Item_Length), Spec_Item),
                                 " ",
                                 lists:nth(rand:uniform(Spec_Item_Length), Spec_Item)
                             ]);
                             _ -> lists:append([
                                 " ",
                                 lists:nth(rand:uniform(Spec_Item_Length), Spec_Item)
                             ])
                         end;
                    _ -> case rand:uniform(3) rem 3 of
                             1 -> " Account Lock";
                             2 -> " Account Unlock";
                             _ -> " Password Expire"
                         end
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (([\.][0-9]+)|([0-9]+[\.]?[0-9]*))[eE]?[+-]?[0-9]*[fFdD]?)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(approxnum = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "0.5",
            "0.5d",
            "0.5e1",
            "0.5e2d",
            "05d",
            "1.0",
            "1.1D",
            "1D",
            "1e3",
            "1e4D",
            "2.5",
            "2.5e-03",
            "2.5f",
            "2.5F",
            "25e5",
            "25e-03",
            "25e-03d",
            "25e6f",
            "25f",
            "6.34",
            "6.34e7",
            "6.34e8F",
            "6.34F",
            "63.4f",
            "634F"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% assignment ::= column COMPARISON scalar_opt_as_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(assignment = Rule) ->
    ?CREATE_CODE_START,
    [{column, Column}] = dets:lookup(?CODE_TEMPLATES, column),
    Column_Length = length(Column),
    [{comparison, Comparison}] = dets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(Column_Length), Column),
                " ",
                lists:nth(rand:uniform(Comparison_Length), Comparison),
                " ",
                lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% assignment_commalist ::= assignment ( ',' assignment )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(assignment_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{assignment, Assignment}] = dets:lookup(?CODE_TEMPLATES, assignment),
    Assignment_Length = length(Assignment),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Assignment_Length), Assignment),
                    ",",
                    lists:nth(rand:uniform(Assignment_Length), Assignment),
                    ",",
                    lists:nth(rand:uniform(Assignment_Length), Assignment),
                    ",",
                    lists:nth(rand:uniform(Assignment_Length), Assignment)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Assignment_Length), Assignment),
                    ",",
                    lists:nth(rand:uniform(Assignment_Length), Assignment),
                    ",",
                    lists:nth(rand:uniform(Assignment_Length), Assignment)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Assignment_Length), Assignment),
                    ",",
                    lists:nth(rand:uniform(Assignment_Length), Assignment)
                ]);
                _ -> lists:nth(rand:uniform(Assignment_Length), Assignment)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% atom ::= parameter_ref
%%        | literal
%%        | 'USER'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atom = Rule) ->
    ?CREATE_CODE_START,
    [{literal, Literal}] = dets:lookup(?CODE_TEMPLATES, literal),
    Literal_Length = length(Literal),
    [{parameter_ref, Parameter_Ref}] = dets:lookup(?CODE_TEMPLATES, parameter_ref),
    Parameter_Ref_Length = length(Parameter_Ref),

    Code =
        [
            "User"
        ] ++
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:nth(rand:uniform(Literal_Length), Literal);
                _ ->
                    lists:nth(rand:uniform(Parameter_Ref_Length), Parameter_Ref)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% between_predicate ::= scalar_exp ( 'NOT' )? 'BETWEEN' scalar_exp 'AND' scalar_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(between_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),

    Code =
        [
            lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                case rand:uniform(2) rem 2 of
                    1 -> " Not";
                    _ -> []
                end,
                " Between ",
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                " And ",
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp))
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case_when_exp ::= ( '(' case_when_exp ')' )
%%                 | ( 'CASE' ( scalar_opt_as_exp )? case_when_then_list ( 'ELSE' scalar_opt_as_exp )? 'END' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(case_when_exp = Rule) ->
    ?CREATE_CODE_START,
    [{case_when_then_list, Case_When_Then_List}] = dets:lookup(?CODE_TEMPLATES, case_when_then_list),
    Case_When_Then_List_Length = length(Case_When_Then_List),
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),

    Code_1 =
        [
            lists:append([
                "Case",
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp);
                    _ -> []
                end,
                " ",
                lists:nth(rand:uniform(Case_When_Then_List_Length), Case_When_Then_List),
                case rand:uniform(2) rem 2 of
                    1 ->
                        " Else " ++ lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp);
                    _ -> []
                end,
                " End"
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    Code =
        [
            lists:append(["(", C, ")"]) || C <- Code_1
        ],
    store_code(Rule, Code ++ Code_1, ?MAX_BASIC, false),
    store_code(select_field, Code, ?MAX_BASIC, false),
    store_code(select_field_commalist, Code, ?MAX_BASIC, false),
    store_code(selection, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case_when_then ::= 'WHEN' search_condition 'THEN' scalar_opt_as_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(case_when_then = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),

    Code =
        [
            lists:append([
                "When ",
                lists:nth(rand:uniform(Search_Condition_Length), Search_Condition),
                " Then ",
                lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case_when_then_list ::= case_when_then ( case_when_then )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(case_when_then_list = Rule) ->
    ?CREATE_CODE_START,
    [{case_when_then, Case_When_Then}] = dets:lookup(?CODE_TEMPLATES, case_when_then),
    Case_When_Then_Length = length(Case_When_Then),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then),
                    " ",
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then),
                    " ",
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then),
                    " ",
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then),
                    " ",
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then),
                    " ",
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then),
                    " ",
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then)
                ]);
                _ ->
                    lists:nth(rand:uniform(Case_When_Then_Length), Case_When_Then)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% close_statement ::= 'CLOSE' cursor
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(close_statement = Rule) ->
    ?CREATE_CODE_START,
    [{cursor, Cursor}] = dets:lookup(?CODE_TEMPLATES, cursor),

    Code =
        [
                "Close " ++ C || C <- Cursor
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% column_commalist ::= column ( ',' column )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(column_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{column, Column}] = dets:lookup(?CODE_TEMPLATES, column),
    Column_Length = length(Column),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Column_Length), Column),
                    ",",
                    lists:nth(rand:uniform(Column_Length), Column),
                    ",",
                    lists:nth(rand:uniform(Column_Length), Column),
                    ",",
                    lists:nth(rand:uniform(Column_Length), Column)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Column_Length), Column),
                    ",",
                    lists:nth(rand:uniform(Column_Length), Column),
                    ",",
                    lists:nth(rand:uniform(Column_Length), Column)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Column_Length), Column),
                    ",",
                    lists:nth(rand:uniform(Column_Length), Column)
                ]);
                _ -> lists:nth(rand:uniform(Column_Length), Column)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% column_def ::= column data_type ( column_def_opt )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(column_def = Rule) ->
    ?CREATE_CODE_START,
    [{column, Column}] = dets:lookup(?CODE_TEMPLATES, column),
    Column_Length = length(Column),
    [{column_def_opt, Column_Def_Opt}] = dets:lookup(?CODE_TEMPLATES, column_def_opt),
    Column_Def_Opt_Length = length(Column_Def_Opt),
    [{data_type, Data_Type}] = dets:lookup(?CODE_TEMPLATES, data_type),
    Data_Type_Length = length(Data_Type),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(Column_Length), Column),
                " ",
                lists:nth(rand:uniform(Data_Type_Length), Data_Type),
                case rand:uniform(4) rem 4 of
                    1 -> lists:append([
                        lists:nth(rand:uniform(Column_Def_Opt_Length), Column_Def_Opt),
                        " ",
                        lists:nth(rand:uniform(Column_Def_Opt_Length), Column_Def_Opt),
                        " ",
                        lists:nth(rand:uniform(Column_Def_Opt_Length), Column_Def_Opt)
                    ]);
                    2 -> lists:append([
                        lists:nth(rand:uniform(Column_Def_Opt_Length), Column_Def_Opt),
                        " ",
                        lists:nth(rand:uniform(Column_Def_Opt_Length), Column_Def_Opt)
                    ]);
                    3 ->
                        lists:nth(rand:uniform(Column_Def_Opt_Length), Column_Def_Opt);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, 0, false),
    store_code(base_table_element, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% column_def_opt ::= ( 'NOT' 'NULL' ( 'UNIQUE' | 'PRIMARY' 'KEY' )? )
%%                  | ( 'DEFAULT' ( function_ref | literal | NAME | 'NULL' | 'USER' ) )
%%                  | ( 'CHECK' '(' search_condition ')' )
%%                  | ( 'REFERENCES' table ( '(' column_commalist ')' )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(column_def_opt = Rule) ->
    ?CREATE_CODE_START,
    [{column_commalist, Column_Commalist}] = dets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{function_ref, Function_Ref}] = dets:lookup(?CODE_TEMPLATES, function_ref),
    Function_Ref_Length = length(Function_Ref),
    [{literal, Literal}] = dets:lookup(?CODE_TEMPLATES, literal),
    Literal_Length = length(Literal),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            "Default Null",
            "Default User",
            "Not Null",
            "Not Null Unique",
            "Not Null Primary Key",
            "Not Null Unique"
        ] ++ [
        case rand:uniform(3) rem 3 of
            1 -> lists:append([
                "Default",
                case rand:uniform(3) rem 3 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Function_Ref_Length), Function_Ref);
                    2 ->
                        " " ++ lists:nth(rand:uniform(Literal_Length), Literal);
                    _ -> " " ++ lists:nth(rand:uniform(Name_Length), Name)
                end
            ]);
            2 -> lists:append([
                "Check (",
                lists:nth(rand:uniform(Search_Condition_Length), Search_Condition),
                ")"
            ]);
            _ -> lists:append([
                "References",
                " ",
                lists:nth(rand:uniform(Table_Length), Table),
                case rand:uniform(2) rem 2 of
                    1 -> lists:append([
                        " (",
                        lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                        ")"
                    ]);
                    _ -> []
                end
            ])
        end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% column_ref ::= ( ( ( NAME '.' )? NAME '.' )? ( JSON | NAME ) )
%%              | ( ( ( NAME '.' )? NAME '.' )? NAME '(' '+' ')' )
%%              | ( ( NAME '.' )? NAME '.' '*' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(column_ref = Rule) ->
    ?CREATE_CODE_START,
    [{json, Json}] = dets:lookup(?CODE_TEMPLATES, json),
    Json_Length = length(Json),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(9) rem 9 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    lists:nth(rand:uniform(Json_Length), Json)
                ]);
                4 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    lists:nth(rand:uniform(Json_Length), Json)
                ]);
                5 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "(+)"
                ]);
                6 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "(+)"
                ]);
                7 -> lists:nth(rand:uniform(Name_Length), Name) ++ "(+)";
                8 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".*"
                ]);
                _ -> lists:nth(rand:uniform(Name_Length), Name) ++ ".*"
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ccolumn_ref_commalist ::= ( column_ref | function_ref ) ( ',' ( column_ref | function_ref ) )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(column_ref_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{column_ref, Column_Ref}] = dets:lookup(?CODE_TEMPLATES, column_ref),
    Column_Ref_Length = length(Column_Ref),
    [{function_ref, Function_Ref}] = dets:lookup(?CODE_TEMPLATES, function_ref),
    Function_Ref_Length = length(Function_Ref),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end,
                    ",",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end,
                    ",",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end,
                    ",",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end
                ]);
                2 -> lists:append([
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end,
                    ",",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end,
                    ",",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end
                ]);
                3 -> lists:append([
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end,
                    ",",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                        _ ->
                            lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                    end
                ]);
                _ -> case rand:uniform(2) rem 2 of
                         1 ->
                             lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                         _ ->
                             lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
                     end
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% commit_statement ::= 'COMMIT' ( 'WORK' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(commit_statement = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Commit",
            "Commit Work"
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (:=|=|<>|<|>|<=|>=)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(comparison = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            " := ",
            " = ",
            " <> ",
            " < ",
            " > ",
            " <= ",
            " >= "
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% comparison_predicate ::= scalar_opt_as_exp
%%                        | ( ( 'PRIOR' )? scalar_exp COMPARISON scalar_exp )
%%                        | ( scalar_exp COMPARISON subquery )     RULE OBSOLETE
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(comparison_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{comparison, Comparison}] = dets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),
    [{subquery, Subquery}] = dets:lookup(?CODE_TEMPLATES, subquery),
    Subquery_Length = length(Subquery),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    case rand:uniform(2) rem 2 of
                        1 -> "Prior ";
                        _ -> []
                    end,
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                    " ",
                    lists:nth(rand:uniform(Comparison_Length), Comparison),
                    " ",
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp))
                ]);
                _ -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                    " ",
                    lists:nth(rand:uniform(Comparison_Length), Comparison),
                    " ",
                    bracket_query_spec(lists:nth(rand:uniform(Subquery_Length), Subquery))
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create_index_def ::= 'CREATE' ( 'BITMAP' | 'KEYLIST' | 'HASHMAP' | 'UNIQUE' )? 'INDEX' ( index_name )? 'ON' table ( '(' ( NAME | JSON ) ( '|' NAME | JSON )* ')' )?
%%                      ( 'NORM_WITH' STRING )?  ( 'FILTER_WITH' STRING )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create_index_def = Rule) ->
    ?CREATE_CODE_START,
    [{index_name, Index_Name}] = dets:lookup(?CODE_TEMPLATES, index_name),
    Index_Name_Length = length(Index_Name),
    [{json, Json}] = dets:lookup(?CODE_TEMPLATES, json),
    Json_Length = length(Json),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{string, String}] = dets:lookup(?CODE_TEMPLATES, string),
    String_Length = length(String),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            lists:append([
                "Create",
                case rand:uniform(5) rem 5 of
                    1 -> " Bitmap";
                    2 -> " Keylist";
                    3 -> " Hashmap";
                    4 -> " Unique";
                    _ -> []
                end,
                " Index",
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Index_Name_Length), Index_Name);
                    _ -> []
                end,
                " On",
                " ",
                lists:nth(rand:uniform(Table_Length), Table),
                case rand:uniform(4) rem 4 of
                    1 -> lists:append([
                        " (",
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " " ++ lists:nth(rand:uniform(Json_Length), Json);
                            _ ->
                                " " ++ lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        "|",
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " " ++ lists:nth(rand:uniform(Json_Length), Json);
                            _ ->
                                " " ++ lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        "|",
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " " ++ lists:nth(rand:uniform(Json_Length), Json);
                            _ ->
                                " " ++ lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ") "
                    ]);
                    2 -> lists:append([
                        " (",
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " " ++ lists:nth(rand:uniform(Json_Length), Json);
                            _ ->
                                " " ++ lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        "|",
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " " ++ lists:nth(rand:uniform(Json_Length), Json);
                            _ ->
                                " " ++ lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ") "
                    ]);
                    3 -> lists:append([
                        " (",
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " " ++ lists:nth(rand:uniform(Json_Length), Json);
                            _ ->
                                " " ++ lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ") "
                    ]);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " Norm_with " ++ lists:nth(rand:uniform(String_Length), String);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " Filter_with " ++ lists:nth(rand:uniform(String_Length), String);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create_role_def ::= 'CREATE' 'ROLE' NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create_role_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),

    Code =
        [
                "Create Role " ++ N || N <- Name
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create_table_def ::= 'CREATE' ( tbl_scope )? ( tbl_type )? 'TABLE' table '(' ( base_table_element ( ',' base_table_element )* )? ')'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create_table_def = Rule) ->
    ?CREATE_CODE_START,
    [{base_table_element, Base_Table_Element}] = dets:lookup(?CODE_TEMPLATES, base_table_element),
    Base_Table_Element_Length = length(Base_Table_Element),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),
    [{tbl_scope, Tbl_Scope}] = dets:lookup(?CODE_TEMPLATES, tbl_scope),
    Tbl_Scope_Length = length(Tbl_Scope),
    [{tbl_type, Tbl_Type}] = dets:lookup(?CODE_TEMPLATES, tbl_type),
    Tbl_Type_Length = length(Tbl_Type),

    Code =
        [
            lists:append([
                "Create",
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Tbl_Scope_Length), Tbl_Scope);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Tbl_Type_Length), Tbl_Type);
                    _ -> []
                end,
                    " Table" ++
                    " ",
                lists:nth(rand:uniform(Table_Length), Table),
                " (",
                case rand:uniform(4) rem 4 of
                    1 -> lists:append([
                        lists:nth(rand:uniform(Base_Table_Element_Length), Base_Table_Element),
                        ",",
                        lists:nth(rand:uniform(Base_Table_Element_Length), Base_Table_Element),
                        ",",
                        lists:nth(rand:uniform(Base_Table_Element_Length), Base_Table_Element)
                    ]);
                    2 -> lists:append([
                        lists:nth(rand:uniform(Base_Table_Element_Length), Base_Table_Element),
                        ",",
                        lists:nth(rand:uniform(Base_Table_Element_Length), Base_Table_Element)
                    ]);
                    3 ->
                        lists:nth(rand:uniform(Base_Table_Element_Length), Base_Table_Element);
                    _ -> []
                end,
                ")"
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create_user_def ::= 'CREATE' 'USER' NAME identified ( user_opt )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create_user_def = Rule) ->
    ?CREATE_CODE_START,
    [{identified, Identified}] = dets:lookup(?CODE_TEMPLATES, identified),
    Identified_Length = length(Identified),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{user_opt, User_Opt}] = dets:lookup(?CODE_TEMPLATES, user_opt),
    User_Opt_Length = length(User_Opt),

    Code =
        [
            lists:append([
                "Create User ",
                lists:nth(rand:uniform(Name_Length), Name),
                " ",
                lists:nth(rand:uniform(Identified_Length), Identified),
                case rand:uniform(4) rem 4 of
                    1 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(User_Opt_Length), User_Opt),
                        " ",
                        lists:nth(rand:uniform(User_Opt_Length), User_Opt),
                        " ",
                        lists:nth(rand:uniform(User_Opt_Length), User_Opt)
                    ]);
                    2 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(User_Opt_Length), User_Opt),
                        " ",
                        lists:nth(rand:uniform(User_Opt_Length), User_Opt)
                    ]);
                    3 ->
                        " " ++ lists:nth(rand:uniform(User_Opt_Length), User_Opt);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cursor ::= NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cursor = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),

    Code_1 =
        [
            re:replace(N, "ident", "cursor", [{return, list}]) || N <- Name
        ],

    Code =
        [
            re:replace(N, "IDENT", "CURSOR", [{return, list}]) || N <- Code_1
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cursor_def ::= 'DECLARE' cursor 'CURSOR' 'FOR' query_exp ( order_by_clause )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cursor_def = Rule) ->
    ?CREATE_CODE_START,
    [{cursor, Cursor}] = dets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{order_by_clause, Order_By_Clause}] = dets:lookup(?CODE_TEMPLATES, order_by_clause),
    Order_By_Clause_Length = length(Order_By_Clause),
    [{query_exp, Query_Exp}] = dets:lookup(?CODE_TEMPLATES, query_exp),
    Query_Exp_Length = length(Query_Exp),

    Code =
        [
            lists:append([
                "Declare ",
                lists:nth(rand:uniform(Cursor_Length), Cursor),
                " Cursor For ",
                bracket_query_spec(lists:nth(rand:uniform(Query_Exp_Length), Query_Exp)),
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Order_By_Clause_Length), Order_By_Clause);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data_type ::= STRING
%%             | ( NAME ( '(' opt_sgn_num ')' )? )
%%             | ( NAME '(' opt_sgn_num ',' opt_sgn_num ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(data_type = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{opt_sgn_num, Opt_Sgn_Num}] = dets:lookup(?CODE_TEMPLATES, opt_sgn_num),
    Opt_Sgn_Num_Length = length(Opt_Sgn_Num),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    "(",
                    lists:nth(rand:uniform(Opt_Sgn_Num_Length), Opt_Sgn_Num),
                    ")"
                ]);
                _ -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    "(",
                    lists:nth(rand:uniform(Opt_Sgn_Num_Length), Opt_Sgn_Num),
                    ",",
                    lists:nth(rand:uniform(Opt_Sgn_Num_Length), Opt_Sgn_Num),
                    ")"
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% db_user_proxy ::= proxy_with
%%                 | ( ( proxy_with )? 'AUTHENTICATION' 'REQUIRED' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(db_user_proxy = Rule) ->
    ?CREATE_CODE_START,
    [{proxy_with, Proxy_With}] = dets:lookup(?CODE_TEMPLATES, proxy_with),
    Proxy_With_Length = length(Proxy_With),

    Code =
        [
            "Authentication Required"
        ] ++ [
        case rand:uniform(2) rem 2 of
            1 -> lists:nth(rand:uniform(Proxy_With_Length), Proxy_With);
            _ -> lists:nth(rand:uniform(Proxy_With_Length), Proxy_With) ++
            " Authentication Required"
        end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% delete_statement_positioned ::= 'DELETE' 'FROM' table 'WHERE' 'CURRENT' 'OF' cursor ( returning )?
%% delete_statement_searched ::= 'DELETE' 'FROM' table ( where_clause )? ( returning )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(delete_statement = Rule) ->
    ?CREATE_CODE_START,
    [{cursor, Cursor}] = dets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{returning, Returning}] = dets:lookup(?CODE_TEMPLATES, returning),
    Returning_Length = length(Returning),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),
    [{where_clause, Where_Clause}] = dets:lookup(?CODE_TEMPLATES, where_clause),
    Where_Clause_Length = length(Where_Clause),

    Code =
        [
            lists:append([
                "Delete From ",
                lists:nth(rand:uniform(Table_Length), Table),
                case rand:uniform(2) rem 2 of
                    1 ->
                        " Where Current Of " ++ lists:nth(rand:uniform(Cursor_Length), Cursor);
                    _ ->
                        " " ++ lists:nth(rand:uniform(Where_Clause_Length), Where_Clause)
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Returning_Length), Returning);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 4)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_index_def ::= 'DROP' 'INDEX' ( index_name )? 'FROM' table
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_index_def = Rule) ->
    ?CREATE_CODE_START,
    [{index_name, Index_Name}] = dets:lookup(?CODE_TEMPLATES, index_name),
    Index_Name_Length = length(Index_Name),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            lists:append([
                "Drop Index",
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Index_Name_Length), Index_Name);
                    _ -> []
                end,
                " From ",
                lists:nth(rand:uniform(Table_Length), Table)
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_role_def ::= 'DROP' 'ROLE' NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_role_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),

    Code =
        [
                "Drop Role " ++ N || N <- Name
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_table_def ::= 'DROP' ( NAME )? 'TABLE' ( 'IF' 'EXISTS' )? ( table ( ',' table )* ) ( 'RESTRICT' | 'CASCADE' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_table_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            lists:append([
                "Drop",
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Name_Length), Name);
                    _ -> []
                end,
                " Table",
                case rand:uniform(2) rem 2 of
                    1 -> " If Exists";
                    _ -> []
                end,
                case rand:uniform(3) rem 3 of
                    1 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(Table_Length), Table),
                        ",",
                        lists:nth(rand:uniform(Table_Length), Table),
                        ",",
                        lists:nth(rand:uniform(Table_Length), Table)
                    ]);
                    2 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(Table_Length), Table),
                        ",",
                        lists:nth(rand:uniform(Table_Length), Table)
                    ]);
                    _ -> " " ++ lists:nth(rand:uniform(Table_Length), Table)
                end,
                case rand:uniform(3) rem 3 of
                    1 -> " Restrict";
                    2 -> " Cascade";
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_user_def ::= 'DROP' 'USER' NAME ( 'CASCADE' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_user_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append([
                "Drop User ",
                lists:nth(rand:uniform(Name_Length), Name),
                case rand:uniform(2) rem 2 of
                    1 -> " Cascade";
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% existence_test ::= 'EXISTS' subquery
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(existence_test = Rule) ->
    ?CREATE_CODE_START,
    [{subquery, Subquery}] = dets:lookup(?CODE_TEMPLATES, subquery),
    Subquery_Length = length(Subquery),

    Code =
        [
                "Exists " ++ bracket_query_spec(lists:nth(rand:uniform(Subquery_Length), Subquery))
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extra ::= NAME  ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(extra = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),

    Code_1 =
        [
            re:replace(N, "ident", "_extra_", [{return, list}]) || N <- Name
        ],

    Code =
        [
                re:replace(N, "IDENT", "_EXTRA_", [{return, list}]) ++ ";" || N <- Code_1
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fetch_statement ::= 'FETCH' cursor 'INTO' target_commalist
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(fetch_statement = Rule) ->
    ?CREATE_CODE_START,
    [{cursor, Cursor}] = dets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{target_commalist, Target_Commalist}] = dets:lookup(?CODE_TEMPLATES, target_commalist),
    Target_Commalist_Length = length(Target_Commalist),

    Code =
        [
            lists:append([
                "Fetch ",
                lists:nth(rand:uniform(Cursor_Length), Cursor),
                " Into ",
                lists:nth(rand:uniform(Target_Commalist_Length), Target_Commalist)
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% from_clause ::= 'FROM' from_column ( from_column )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(from_clause = Rule) ->
    ?CREATE_CODE_START,
    [{from_column_commalist, From_Column_Commalist}] = dets:lookup(?CODE_TEMPLATES, from_column_commalist),
    From_Column_Commalist_Length = length(From_Column_Commalist),

    Code =
        [
                "From " ++
                lists:nth(rand:uniform(From_Column_Commalist_Length), From_Column_Commalist)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% from_column ::= table_ref
%%               | ( '(' join_clause ')' )
%%               | join_clause
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(from_column = Rule) ->
    ?CREATE_CODE_START,
    [{join_clause, Join_Clause}] = dets:lookup(?CODE_TEMPLATES, join_clause),
    Join_Clause_Length = length(Join_Clause),
    [{table_ref, Table_Ref}] = dets:lookup(?CODE_TEMPLATES, table_ref),
    Table_Ref_Length = length(Table_Ref),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 ->
                    lists:nth(rand:uniform(Table_Ref_Length), Table_Ref);
                2 -> lists:append([
                    "(",
                    lists:nth(rand:uniform(Join_Clause_Length), Join_Clause),
                    ")"
                ]);
                _ ->
                    lists:nth(rand:uniform(Join_Clause_Length), Join_Clause)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% from_column_commalist ::= from_column ( ',' from_column )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(from_column_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{from_column, From_Column}] = dets:lookup(?CODE_TEMPLATES, from_column),
    From_Column_Length = length(From_Column),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(From_Column_Length), From_Column),
                    ",",
                    lists:nth(rand:uniform(From_Column_Length), From_Column),
                    ",",
                    lists:nth(rand:uniform(From_Column_Length), From_Column),
                    ",",
                    lists:nth(rand:uniform(From_Column_Length), From_Column)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(From_Column_Length), From_Column),
                    ",",
                    lists:nth(rand:uniform(From_Column_Length), From_Column),
                    ",",
                    lists:nth(rand:uniform(From_Column_Length), From_Column)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(From_Column_Length), From_Column),
                    ",",
                    lists:nth(rand:uniform(From_Column_Length), From_Column)
                ]);
                _ ->
                    lists:nth(rand:uniform(From_Column_Length), From_Column)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fun_arg ::= ( '(' fun_arg ')' )
%%           | function_ref
%%           | column_ref
%%           | ( fun_arg ( '+' | '-' | '*' | '/' | 'div' | '||' ) fun_arg )
%%           | ( ( '+' | '-' ) fun_arg )
%%           | ( ( '+' | '-' ) literal )                           RULE OBSOLETE
%%           | 'NULL'
%%           | atom
%%           | subquery
%%           | ( fun_arg 'AS' NAME )
%%           | ( fun_arg COMPARISON fun_arg )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(fun_arg = Rule) ->
    ?CREATE_CODE_START,
    [{comparison, Comparison}] = dets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{fun_arg, Fun_Arg}] = dets:lookup(?CODE_TEMPLATES, fun_arg),
    Fun_Arg_Length = length(Fun_Arg),
    [{literal, Literal}] = dets:lookup(?CODE_TEMPLATES, literal),
    Literal_Length = length(Literal),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        Fun_Arg ++
        [
            "Null"
        ] ++ [
        case rand:uniform(5) rem 5 of
            1 -> lists:append([
                "(",
                lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg),
                ")"
            ]);
            2 -> lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                case rand:uniform(6) rem 6 of
                    1 -> " + ";
                    2 -> " - ";
                    3 -> " * ";
                    4 -> " / ";
                    5 -> " div ";
                    _ -> " || "
                end,
                bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg))
            ]);
            3 -> case rand:uniform(2) rem 2 of
                     1 -> "+";
                     _ -> "-"
                 end ++
            case rand:uniform(2) rem 2 of
                1 ->
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg));
                _ -> lists:nth(rand:uniform(Literal_Length), Literal)
            end;
            4 -> lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                " As ",
                lists:nth(rand:uniform(Name_Length), Name)
            ]);
            _ -> lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                lists:nth(rand:uniform(Comparison_Length), Comparison),
                bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg))
            ])
        end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fun_args ::= fun_arg ( ',' fun_arg )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(fun_args = Rule) ->
    ?CREATE_CODE_START,
    [{fun_arg, Fun_Arg}] = dets:lookup(?CODE_TEMPLATES, fun_arg),
    Fun_Arg_Length = length(Fun_Arg),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                    ",",
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                    ",",
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                    ",",
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg))
                ]);
                2 -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                    ",",
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                    ",",
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg))
                ]);
                3 -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg)),
                    ",",
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg))
                ]);
                _ ->
                    bracket_query_spec(lists:nth(rand:uniform(Fun_Arg_Length), Fun_Arg))
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function_ref ::= ( ( ( NAME '.' )?  NAME '.' )? NAME '(' fun_args ')' )
%%                | ( 'FUNS' ( '(' ( fun_args | '*' | ( 'DISTINCT' column_ref ) | ( 'ALL' scalar_exp ) ) ')' )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(function_ref = Rule) ->
    ?CREATE_CODE_START,
    [{column_ref, Column_Ref}] = dets:lookup(?CODE_TEMPLATES, column_ref),
    Column_Ref_Length = length(Column_Ref),
    [{funs, Funs}] = dets:lookup(?CODE_TEMPLATES, funs),
    Funs_Length = length(Funs),
    [{fun_args, Fun_Args}] = dets:lookup(?CODE_TEMPLATES, fun_args),
    Fun_Args_Length = length(Fun_Args),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> case rand:uniform(3) rem 3 of
                         1 -> lists:append([
                             lists:nth(rand:uniform(Name_Length), Name),
                             ".",
                             lists:nth(rand:uniform(Name_Length), Name),
                             ".",
                             lists:nth(rand:uniform(Name_Length), Name),
                             "(",
                             lists:nth(rand:uniform(Fun_Args_Length), Fun_Args),
                             ")"
                         ]);
                         2 -> lists:append([
                             lists:nth(rand:uniform(Name_Length), Name),
                             ".",
                             lists:nth(rand:uniform(Name_Length), Name),
                             "(",
                             lists:nth(rand:uniform(Fun_Args_Length), Fun_Args),
                             ")"
                         ]);
                         _ -> lists:append([
                             lists:nth(rand:uniform(Name_Length), Name),
                             "(",
                             lists:nth(rand:uniform(Fun_Args_Length), Fun_Args),
                             ")"
                         ])
                     end;
                _ -> lists:nth(rand:uniform(Funs_Length), Funs) ++
                case rand:uniform(20) rem 20 of
                    1 -> [];
                    _ -> lists:append([
                        "(",
                        case rand:uniform(4) rem 4 of
                            1 ->
                                lists:nth(rand:uniform(Fun_Args_Length), Fun_Args);
                            2 -> "*";
                            3 ->
                                "Distinct " ++ lists:nth(rand:uniform(Column_Ref_Length), Column_Ref);
                            _ ->
                                "All " ++ bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp))
                        end,
                        ")"
                    ])
                end
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function_ref_list ::= ( function_ref ';' )
%%                     | ( function_ref ';' function_ref_list )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(function_ref_list = Rule) ->
    ?CREATE_CODE_START,
    [{function_ref, Function_Ref}] = dets:lookup(?CODE_TEMPLATES, function_ref),
    Function_Ref_Length = length(Function_Ref),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";",
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";",
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";",
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";"
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";",
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";",
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";"
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";",
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref),
                    ";"
                ]);
                _ ->
                    lists:nth(rand:uniform(Function_Ref_Length), Function_Ref) ++ ";"
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% from leex definition
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(funs = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "ABS",
            "ACOS",
            "ASIN",
            "ATAN",
            "ATAN2",
            "AVG",
            "CORR",
            "COS",
            "COSH",
            "COT",
            "COUNT",
            "COVAR_POP",
            "COVAR_SAMP",
            "LOWER",
            "LTRIM",
            "MAX",
            "MEDIAN",
            "MIN",
            "NVL",
            "REGR_AVGX",
            "REGR_AVGY",
            "REGR_COUNT",
            "REGR_INTERCEPT",
            "REGR_R2",
            "REGR_SLOPE",
            "REGR_SXX",
            "REGR_SXY",
            "REGR_SYY",
            "SIN",
            "SINH",
            "STDDEV",
            "STDDEV_POP",
            "STDDEV_SAMP",
            "SUM",
            "TAN",
            "TANH",
            "TO_CHAR",
            "TO_DATE",
            "TRUNC",
            "UPPER",
            "VAR_POP",
            "VAR_SAMP",
            "VARIANCE"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% grant_def ::= 'GRANT' system_privilege_list ( on_obj_clause )? 'TO' grantee ( ',' grantee )* ( with_grant_option )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(grant_def = Rule) ->
    ?CREATE_CODE_START,
    [{grantee, Grantee}] = dets:lookup(?CODE_TEMPLATES, grantee),
    Grantee_Length = length(Grantee),
    [{on_obj_clause, On_Obj_Clause}] = dets:lookup(?CODE_TEMPLATES, on_obj_clause),
    On_Obj_Clause_Length = length(On_Obj_Clause),
    [{system_privilege_list, System_Privilege_List}] = dets:lookup(?CODE_TEMPLATES, system_privilege_list),
    System_Privilege_List_Length = length(System_Privilege_List),
    [{with_grant_option, With_Grant_Option}] = dets:lookup(?CODE_TEMPLATES, with_grant_option),
    With_Grant_Option_Length = length(With_Grant_Option),

    Code =
        [
            lists:append([
                "Grant",
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(System_Privilege_List_Length), System_Privilege_List);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(On_Obj_Clause_Length), On_Obj_Clause);
                    _ -> []
                end,
                " To ",
                case rand:uniform(3) rem 3 of
                    1 -> lists:append([
                        lists:nth(rand:uniform(Grantee_Length), Grantee),
                        ",",
                        lists:nth(rand:uniform(Grantee_Length), Grantee),
                        ",",
                        lists:nth(rand:uniform(Grantee_Length), Grantee)
                    ]);
                    2 -> lists:append([
                        lists:nth(rand:uniform(Grantee_Length), Grantee),
                        ",",
                        lists:nth(rand:uniform(Grantee_Length), Grantee)
                    ]);
                    _ -> " " ++ lists:nth(rand:uniform(Grantee_Length), Grantee)
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(With_Grant_Option_Length), With_Grant_Option);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% grantee ::= 'PUBLIC'
%%           | ( NAME ( 'IDENTIFIED' 'BY' NAME )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(grantee = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "Public"
        ] ++ [
        case rand:uniform(2) rem 2 of
            1 -> lists:append([
                lists:nth(rand:uniform(Name_Length), Name),
                " Identified By ",
                lists:nth(rand:uniform(Name_Length), Name)
            ]);
            _ -> lists:nth(rand:uniform(Name_Length), Name)
        end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% group_by_clause ::= 'GROUP' 'BY' column_ref_commalist
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(group_by_clause = Rule) ->
    ?CREATE_CODE_START,
    [{column_ref_commalist, Column_Ref_Commalist}] = dets:lookup(?CODE_TEMPLATES, column_ref_commalist),
    Column_Ref_Commalist_Length = length(Column_Ref_Commalist),

    Code =
        [
                " Group By " ++ lists:nth(rand:uniform(Column_Ref_Commalist_Length), Column_Ref_Commalist)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% having_clause ::= 'HAVING' search_condition
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(having_clause = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),

    Code =
        [
                " Having " ++ lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hierarchical_query_clause ::= ( 'START' 'WITH' search_condition 'CONNECT' 'BY' ( 'NOCYCLE' )? search_condition )
%%                             | ( 'CONNECT' 'BY' ( 'NOCYCLE' )? search_condition 'START' 'WITH' search_condition )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(hierarchical_query_clause = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    "Start With ",
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition),
                    " Connect By",
                    case rand:uniform(2) rem 2 of
                        1 -> " Nocycle";
                        _ -> []
                    end,
                    " ",
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
                ]);
                _ -> lists:append([
                    "Connect By",
                    case rand:uniform(2) rem 2 of
                        1 -> " Nocycle";
                        _ -> []
                    end,
                    " ",
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition),
                    " Start With",
                    " ",
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ((\/\*)[^\*\/]*(\*\/))
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(hint = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "/*+ ALL_ROWS */",
            "/*+ CONTAINERS(DEFAULT_PDB_HINT='NO_PARALLEL') */",
            "/*+ DRIVING_SITE(departments) */",
            "/*+ DYNAMIC_SAMPLING(e 1) */",
            "/*+ DYNAMIC_SAMPLING(employees 1) */",
            "/*+ FIRST_ROWS(10) */",
            "/*+ FULL (hr_emp) CACHE(hr_emp) */",
            "/*+ FULL(e) */",
            "/*+ FULL(hr_emp) NOCACHE(hr_emp) */",
            "/*+ GROUPING */",
            "/*+ INDEX (employees emp_department_ix)*/",
            "/*+ INDEX_COMBINE(e emp_manager_ix emp_department_ix) */",
            "/*+ INDEX_DESC(e emp_name_ix) */",
            "/*+ INDEX_FFS(e emp_name_ix) */",
            "/*+ INDEX_JOIN(e emp_manager_ix emp_department_ix) */",
            "/*+ INDEX_SS(e emp_name_ix) */",
            "/*+ INDEX_SS_DESC(e emp_name_ix) */",
            "/*+ LEADING(e j) */",
            "/*+ MERGE(v) */",
            "/*+ NO_EXPAND */",
            "/*+ NO_INDEX(employees emp_empid) */",
            "/*+ NO_INDEX_FFS(items item_order_ix) */",
            "/*+ NO_MERGE(seattle_dept) */",
            "/*+ NO_MERGE(v) NO_PUSH_PRED(v) */",
            "/*+ NO_PARALLEL(hr_emp) */"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% identified ::= IDENTIFIED ( ( 'BY' NAME ) | ( EXTERNALLY ( 'AS' NAME ) ) | ( 'GLOBALLY' ( 'AS' NAME )? ) )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(identified = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "Identified Externally",
            "Identified Globally"
        ] ++ [
        lists:append([
            "Identified",
            case rand:uniform(3) rem 3 of
                1 -> " By ";
                2 -> " Externally As ";
                _ -> " Globally As "
            end,
            lists:nth(rand:uniform(Name_Length), Name)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(spec_item, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% in_predicate ::= ( scalar_exp ( 'NOT' )? 'IN' '(' subquery ')' )
%%                | ( scalar_exp ( 'NOT' )? 'IN' '(' scalar_exp_commalist ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(in_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),
    [{scalar_exp_commalist, Scalar_Exp_Commalist}] = dets:lookup(?CODE_TEMPLATES, scalar_exp_commalist),
    Scalar_Exp_Commalist_Length = length(Scalar_Exp_Commalist),
    [{subquery, Subquery}] = dets:lookup(?CODE_TEMPLATES, subquery),
    Subquery_Length = length(Subquery),

    Code =
        [
            lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                case rand:uniform(2) rem 2 of
                    1 -> " Not";
                    _ -> []
                end,
                " In (",
                case rand:uniform(2) rem 2 of
                    1 -> lists:nth(rand:uniform(Subquery_Length), Subquery);
                    _ ->
                        lists:nth(rand:uniform(Scalar_Exp_Commalist_Length), Scalar_Exp_Commalist)
                end,
                ")"
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% index_name ::= ( NAME '.' )? NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(index_name = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                _ -> lists:nth(rand:uniform(Name_Length), Name)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% inner_cross_join ::= ( 'INNER' )? 'JOIN' join_ref join_on_or_using_clause
%%                    | ( 'CROSS' | ( 'NATURAL' ( 'INNER' )? ) ) 'JOIN' join_ref
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(inner_cross_join = Rule) ->
    ?CREATE_CODE_START,
    [{join_on_or_using_clause, Join_On_Or_Using_Clause}] = dets:lookup(?CODE_TEMPLATES, join_on_or_using_clause),
    Join_On_Or_Using_Clause_Length = length(Join_On_Or_Using_Clause),
    [{join_ref, Join_Ref}] = dets:lookup(?CODE_TEMPLATES, join_ref),
    Join_Ref_Length = length(Join_Ref),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    case rand:uniform(2) rem 2 of
                        1 -> "Join ";
                        _ -> "Inner Join "
                    end,
                    lists:nth(rand:uniform(Join_Ref_Length), Join_Ref),
                    " ",
                    lists:nth(rand:uniform(Join_On_Or_Using_Clause_Length), Join_On_Or_Using_Clause)
                ]);
                _ -> case rand:uniform(3) rem 3 of
                         1 -> "Cross Join ";
                         2 -> "Natural Join ";
                         _ -> "Natural Inner Join "
                     end ++ lists:nth(rand:uniform(Join_Ref_Length), Join_Ref)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% insert_statement ::= 'INSERT' 'INTO' table ( ( '(' column_commalist ')' )? ( ( 'VALUES' '(' scalar_opt_as_exp ( ',' scalar_opt_as_exp )* ')' ) | query_spec ) ( returning )? )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(insert_statement = Rule) ->
    ?CREATE_CODE_START,
    [{column_commalist, Column_Commalist}] = dets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{query_spec, Query_Spec}] = dets:lookup(?CODE_TEMPLATES, query_spec),
    Query_Spec_Length = length(Query_Spec),
    [{returning, Returning}] = dets:lookup(?CODE_TEMPLATES, returning),
    Returning_Length = length(Returning),
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            lists:append([
                "Insert Into ",
                lists:nth(rand:uniform(Table_Length), Table),
                case rand:uniform(50) rem 50 of
                    1 -> [];
                    _ -> lists:append([
                        " (",
                        lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                        ")",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:append([
                                " Values (",
                                case rand:uniform(4) rem 4 of
                                    1 -> lists:append([
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                                        ",",
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                                        ",",
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                                        ",",
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
                                    ]);
                                    2 -> lists:append([
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                                        ",",
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                                        ",",
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
                                    ]);
                                    3 -> lists:append([
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                                        ",",
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
                                    ]);
                                    _ ->
                                        lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
                                end,
                                ") "
                            ]);
                            _ ->
                                " " ++ lists:nth(rand:uniform(Query_Spec_Length), Query_Spec)
                        end,
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " " ++ lists:nth(rand:uniform(Returning_Length), Returning);
                            _ -> []
                        end
                    ])
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ([0-9]+)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(intnum = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "0",
            "013456789012345",
            "1",
            "11121",
            "123456",
            "134567890",
            "22345678",
            "23",
            "2345678",
            "2345678901234567",
            "3123456",
            "34567890",
            "411121",
            "456",
            "456789012",
            "5678901234",
            "57891",
            "6456",
            "67890123456",
            "723",
            "789012345678",
            "7891",
            "8901234567890",
            "90",
            "90123456789012"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% into ::= 'INTO' target_commalist ( 'IN' NAME )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(into = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{target_commalist, Target_Commalist}] = dets:lookup(?CODE_TEMPLATES, target_commalist),
    Target_Commalist_Length = length(Target_Commalist),

    Code =
        [
            lists:append([
                " Into ",
                lists:nth(rand:uniform(Target_Commalist_Length), Target_Commalist),
                case rand:uniform(2) rem 2 of
                    1 ->
                        " In " ++ lists:nth(rand:uniform(Name_Length), Name);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join ::= inner_cross_join
%%        | outer_join
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(join = Rule) ->
    ?CREATE_CODE_START,
    [{inner_cross_join, Inner_Cross_Join}] = dets:lookup(?CODE_TEMPLATES, inner_cross_join),
    Inner_Cross_Join_Length = length(Inner_Cross_Join),
    [{outer_join, Outer_Join}] = dets:lookup(?CODE_TEMPLATES, outer_join),
    Outer_Join_Length = length(Outer_Join),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 ->
                    lists:nth(rand:uniform(Inner_Cross_Join_Length), Inner_Cross_Join);
                _ -> lists:nth(rand:uniform(Outer_Join_Length), Outer_Join)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join_clause ::= table_ref join ( join )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(join_clause = Rule) ->
    ?CREATE_CODE_START,
    [{join_list, Join_List}] = dets:lookup(?CODE_TEMPLATES, join_list),
    Join_List_Length = length(Join_List),
    [{table_ref, Table_Ref}] = dets:lookup(?CODE_TEMPLATES, table_ref),
    Table_Ref_Length = length(Table_Ref),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(Table_Ref_Length), Table_Ref),
                " ",
                lists:nth(rand:uniform(Join_List_Length), Join_List)
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join_list ::=  join ( join )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(join_list = Rule) ->
    ?CREATE_CODE_START,
    [{join, Join}] = dets:lookup(?CODE_TEMPLATES, join),
    Join_Length = length(Join),

    Code =
        Join ++
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Join_Length), Join),
                    " ",
                    lists:nth(rand:uniform(Join_Length), Join),
                    " ",
                    lists:nth(rand:uniform(Join_Length), Join)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Join_Length), Join),
                    " ",
                    lists:nth(rand:uniform(Join_Length), Join)
                ]);
                _ ->
                    lists:nth(rand:uniform(Join_Length), Join)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join_on_or_using_clause ::= ( 'ON' search_condition )
%%                           | ( 'USING' '(' select_field_commalist ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(join_on_or_using_clause = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),
    [{select_field_commalist, Select_Field_Commalist}] = dets:lookup(?CODE_TEMPLATES, select_field_commalist),
    Select_Field_Commalist_Length = length(Select_Field_Commalist),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 ->
                    "On " ++ lists:nth(rand:uniform(Search_Condition_Length), Search_Condition);
                _ -> lists:append([
                    "Using (",
                    lists:nth(rand:uniform(Select_Field_Commalist_Length), Select_Field_Commalist),
                    ")"
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join_ref ::= table
%%            | ( '(' query_exp ')' ( NAME )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(join_ref = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{query_exp, Query_Exp}] = dets:lookup(?CODE_TEMPLATES, query_exp),
    Query_Exp_Length = length(Query_Exp),

    Code =
        [
            lists:append([
                "(",
                lists:nth(rand:uniform(Query_Exp_Length), Query_Exp),
                ")",
                case rand:uniform(2) rem 2 of
                    1 -> " " ++ lists:nth(rand:uniform(Name_Length), Name);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\|[\.:{\[#]([^\|]*)+\|)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(json = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "|#_.g:b.f[f(p.r:s)]|",
            "|:.g:b.f[f(p.r:q)]|",
            "|::a|",
            "|::bc..12|",
            "|::b|",
            "|:[]|",
            "|:_a1:f()|",
            "|:_a::b::c|",
            "|:_a::b|",
            "|:_a:bc:df|",
            "|:_a:b|",
            "|:a0_ {}|",
            "|:b.f[f(p.r:q)]|",
            "|:b.f[f(p.r:r)]|",
            "|:b.f[f(p.r:s)]|",
            "|:b.f[f(p.r:t)]|",
            "|:b.fn[tf(p.r:q)]|",
            "|:b[f(p:q)]|",
            "|:b[f(p:r)]|",
            "|:b|",
            "|:f()|",
            "|:f(i)|",
            "|:{b}|",
            "|:{}|",
            "|[]|",
            "|[_:b[f(p:q)]]|",
            "|{_:b2}|",
            "|{}|"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(column_ref, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% like_predicate ::= scalar_exp ( 'NOT' )? 'LIKE' scalar_exp ( 'ESCAPE' atom )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(like_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{atom, Atom}] = dets:lookup(?CODE_TEMPLATES, atom),
    Atom_Length = length(Atom),
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),

    Code =
        [
            lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                case rand:uniform(2) rem 2 of
                    1 -> " Not";
                    _ -> []
                end,
                " Like ",
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                case rand:uniform(2) rem 2 of
                    1 ->
                        " Escape " ++ lists:nth(rand:uniform(Atom_Length), Atom);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% literal ::= STRING
%%           | INTNUM
%%           | APPROXNUM
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(literal = Rule) ->
    ?CREATE_CODE_START,
    [{approxnum, Approxnum}] = dets:lookup(?CODE_TEMPLATES, approxnum),
    [{intnum, Intnum}] = dets:lookup(?CODE_TEMPLATES, intnum),
    [{string, String}] = dets:lookup(?CODE_TEMPLATES, string),

    Code = Approxnum ++ Intnum ++ String,
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% manipulative_statement ::= close_statement
%%                          | commit_statement
%%                          | delete_statement_positioned
%%                          | delete_statement_searched
%%                          | fetch_statement
%%                          | insert_statement
%%                          | open_statement
%%                          | rollback_statement
%%                          | select_statement
%%                          | update_statement_positioned
%%                          | update_statement_searched
%%                          | create_table_def
%%                          | create_role_def
%%                          | create_index_def
%%                          | create_user_def
%%                          | drop_role_def
%%                          | drop_table_def
%%                          | drop_index_def
%%                          | alter_user_def
%%                          | drop_user_def
%%                          | view_def
%%                          | truncate_table
%%                          | grant_def
%%                          | revoke_def
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(manipulative_statement = Rule) ->
    ?CREATE_CODE_START,
    [{alter_user_def, Alter_User_Def}] = dets:lookup(?CODE_TEMPLATES, alter_user_def),
    Alter_User_Def_Length = length(Alter_User_Def),
    [{close_statement, Close_Statement}] = dets:lookup(?CODE_TEMPLATES, close_statement),
    Close_Statement_Length = length(Close_Statement),
    [{commit_statement, Commit_Statement}] = dets:lookup(?CODE_TEMPLATES, commit_statement),
    Commit_Statement_Length = length(Commit_Statement),
    [{create_index_def, Create_Index_Def}] = dets:lookup(?CODE_TEMPLATES, create_index_def),
    Create_Index_Def_Length = length(Create_Index_Def),
    [{create_role_def, Create_Role_Def}] = dets:lookup(?CODE_TEMPLATES, create_role_def),
    Create_Role_Def_Length = length(Create_Role_Def),
    [{create_table_def, Create_Table_Def}] = dets:lookup(?CODE_TEMPLATES, create_table_def),
    Create_Table_Def_Length = length(Create_Table_Def),
    [{create_user_def, Create_User_Def}] = dets:lookup(?CODE_TEMPLATES, create_user_def),
    Create_User_Def_Length = length(Create_User_Def),
    [{delete_statement, Delete_Statement}] = dets:lookup(?CODE_TEMPLATES, delete_statement),
    Delete_Statement_Length = length(Delete_Statement),
    [{drop_index_def, Drop_Index_Def}] = dets:lookup(?CODE_TEMPLATES, drop_index_def),
    Drop_Index_Def_Length = length(Drop_Index_Def),
    [{drop_role_def, Drop_Role_Def}] = dets:lookup(?CODE_TEMPLATES, drop_role_def),
    Drop_Role_Def_Length = length(Drop_Role_Def),
    [{drop_table_def, Drop_Table_Def}] = dets:lookup(?CODE_TEMPLATES, drop_table_def),
    Drop_Table_Def_Length = length(Drop_Table_Def),
    [{drop_user_def, Drop_User_Def}] = dets:lookup(?CODE_TEMPLATES, drop_user_def),
    Drop_User_Def_Length = length(Drop_User_Def),
    [{fetch_statement, Fetch_Statement}] = dets:lookup(?CODE_TEMPLATES, fetch_statement),
    Fetch_Statement_Length = length(Fetch_Statement),
    [{grant_def, Grant_Def}] = dets:lookup(?CODE_TEMPLATES, grant_def),
    Grant_Def_Length = length(Grant_Def),
    [{insert_statement, Insert_Statement}] = dets:lookup(?CODE_TEMPLATES, insert_statement),
    Insert_Statement_Length = length(Insert_Statement),
    [{open_statement, Open_Statement}] = dets:lookup(?CODE_TEMPLATES, open_statement),
    Open_Statement_Length = length(Open_Statement),
    [{revoke_def, Revoke_Def}] = dets:lookup(?CODE_TEMPLATES, revoke_def),
    Revoke_Def_Length = length(Revoke_Def),
    [{rollback_statement, Rollback_Statement}] = dets:lookup(?CODE_TEMPLATES, rollback_statement),
    Rollback_Statement_Length = length(Rollback_Statement),
    [{select_statement, Select_Statement}] = dets:lookup(?CODE_TEMPLATES, select_statement),
    Select_Statement_Length = length(Select_Statement),
    [{truncate_table, Truncate_Table}] = dets:lookup(?CODE_TEMPLATES, truncate_table),
    Truncate_Table_Length = length(Truncate_Table),
    [{update_statement, Update_Statement}] = dets:lookup(?CODE_TEMPLATES, update_statement),
    Update_Statement_Length = length(Update_Statement),
    [{view_def, View_Def}] = dets:lookup(?CODE_TEMPLATES, view_def),
    View_Def_Length = length(View_Def),

    Code =
        [
            case rand:uniform(22) rem 22 of
                1 ->
                    lists:nth(rand:uniform(Alter_User_Def_Length), Alter_User_Def);
                2 ->
                    lists:nth(rand:uniform(Close_Statement_Length), Close_Statement);
                3 ->
                    lists:nth(rand:uniform(Commit_Statement_Length), Commit_Statement);
                4 ->
                    lists:nth(rand:uniform(Create_Index_Def_Length), Create_Index_Def);
                5 ->
                    lists:nth(rand:uniform(Create_Role_Def_Length), Create_Role_Def);
                6 ->
                    lists:nth(rand:uniform(Create_Table_Def_Length), Create_Table_Def);
                7 ->
                    lists:nth(rand:uniform(Create_User_Def_Length), Create_User_Def);
                8 ->
                    lists:nth(rand:uniform(Delete_Statement_Length), Delete_Statement);
                9 ->
                    lists:nth(rand:uniform(Drop_Index_Def_Length), Drop_Index_Def);
                10 ->
                    lists:nth(rand:uniform(Drop_Role_Def_Length), Drop_Role_Def);
                11 ->
                    lists:nth(rand:uniform(Drop_Table_Def_Length), Drop_Table_Def);
                12 ->
                    lists:nth(rand:uniform(Drop_User_Def_Length), Drop_User_Def);
                13 ->
                    lists:nth(rand:uniform(Fetch_Statement_Length), Fetch_Statement);
                14 -> lists:nth(rand:uniform(Grant_Def_Length), Grant_Def);
                15 ->
                    lists:nth(rand:uniform(Insert_Statement_Length), Insert_Statement);
                16 ->
                    lists:nth(rand:uniform(Open_Statement_Length), Open_Statement);
                17 -> lists:nth(rand:uniform(Revoke_Def_Length), Revoke_Def);
                18 ->
                    lists:nth(rand:uniform(Rollback_Statement_Length), Rollback_Statement);
                19 ->
                    lists:nth(rand:uniform(Select_Statement_Length), Select_Statement);
                20 ->
                    lists:nth(rand:uniform(Truncate_Table_Length), Truncate_Table);
                21 ->
                    lists:nth(rand:uniform(Update_Statement_Length), Update_Statement);
                _ -> lists:nth(rand:uniform(View_Def_Length), View_Def)
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\"((\$|[^\"]*)*(\"\")*)*\")
%% [A-Za-z][A-Za-z0-9_@:#\$]*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(name = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "\\\"*** ident info ***\\\"",
            "\\\"ident name\\\"",
            "\\\"ident(s)\\\"",
            "\\\"on/off ident\\\"",
            "\\\"X+ident\\\"",
            "credit_limit_ident",
            "I@DENT_000_@",
            "I@DENT_100_@",
            "I@DENT_1_",
            "I@DENT_200_@",
            "I@DENT_2__",
            "I@DENT_3_#",
            "I@DENT_4_$",
            "I@DENT_5",
            "I@DENT_6",
            "I@DENT_7",
            "I@DENT_8",
            "I@DENT_9",
            "L@astName_ident",
            "m@oney$$$tree_ident",
            "o@racle$number_ident",
            "p@hone#_ident",
            "S@N##_ident",
            "t@2_ident",
            "t@ry_again__ident",
            "X@_ident",
            "X@YZ_ident"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(column, Code, ?MAX_BASIC, false),
    store_code(column_ref, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    store_code(target, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% on_obj_clause ::= ( 'ON' table )
%%                 | ( 'ON' 'DIRECTORY' NAME )
%%                 | ( 'ON' 'JAVA' ( 'SOURCE' | 'RESOURCE' ) table )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(on_obj_clause = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
                "On" ++
                case rand:uniform(3) rem 3 of
                    1 -> " " ++ lists:nth(rand:uniform(Table_Length), Table);
                    2 ->
                        " Directory " ++ lists:nth(rand:uniform(Name_Length), Name);
                    _ -> lists:append([
                        " Java",
                        case rand:uniform(2) rem 2 of
                            1 -> " Source";
                            _ -> " Resource"
                        end,
                        " ",
                        lists:nth(rand:uniform(Table_Length), Table)
                    ])
                end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% open_statement ::= 'OPEN' cursor
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(open_statement = Rule) ->
    ?CREATE_CODE_START,
    [{cursor, Cursor}] = dets:lookup(?CODE_TEMPLATES, cursor),

    Code =
        [
                "Open " ++ C || C <- Cursor
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% opt_sgn_num ::= ( '-' )? INTNUM
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(opt_sgn_num = Rule) ->
    ?CREATE_CODE_START,
    [{intnum, Intnum}] = dets:lookup(?CODE_TEMPLATES, intnum),

    Code = Intnum ++
        [
                "-" ++ I || I <- Intnum
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% order_by_clause ::= 'ORDER' 'BY' ordering_spec ( ',' ordering_spec )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(order_by_clause = Rule) ->
    ?CREATE_CODE_START,
    [{ordering_spec, Ordering_Spec}] = dets:lookup(?CODE_TEMPLATES, ordering_spec),
    Ordering_Spec_Length = length(Ordering_Spec),

    Code =
        [
                "Order By " ++
                case rand:uniform(4) rem 4 of
                    1 -> lists:append([
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec),
                        ",",
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec),
                        ",",
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec),
                        ",",
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec)
                    ]);
                    2 -> lists:append([
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec),
                        ",",
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec),
                        ",",
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec)
                    ]);
                    3 -> lists:append([
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec),
                        ",",
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec)
                    ]);
                    _ ->
                        lists:nth(rand:uniform(Ordering_Spec_Length), Ordering_Spec)
                end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ordering_spec ::= scalar_exp ( 'ASC' | 'DESC' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(ordering_spec = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),

    Code =
        [
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)) ++
                case rand:uniform(3) rem 3 of
                    1 -> " Asc";
                    2 -> " Desc";
                    _ -> []
                end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% outer_join ::= outer_join ::= ( NATURAL | query_partition_clause | (query_partition_clause NATURAL) )? outer_join_type JOIN join_ref ( query_partition_clause )? ( join_on_or_using_clause )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(outer_join = Rule) ->
    ?CREATE_CODE_START,
    [{join_on_or_using_clause, Join_On_Or_Using_Clause}] = dets:lookup(?CODE_TEMPLATES, join_on_or_using_clause),
    Join_On_Or_Using_Clause_Length = length(Join_On_Or_Using_Clause),
    [{join_ref, Join_Ref}] = dets:lookup(?CODE_TEMPLATES, join_ref),
    Join_Ref_Length = length(Join_Ref),
    [{outer_join_type, Outer_Join_Type}] = dets:lookup(?CODE_TEMPLATES, outer_join_type),
    Outer_Join_Type_Length = length(Outer_Join_Type),
    [{query_partition_clause, Query_Partition_Clause}] = dets:lookup(?CODE_TEMPLATES, query_partition_clause),
    Query_Partition_Clause_Length = length(Query_Partition_Clause),

    Code =
        [
            lists:append([
                case rand:uniform(4) rem 4 of
                    1 -> "Natural ";
                    2 ->
                        lists:nth(rand:uniform(Query_Partition_Clause_Length), Query_Partition_Clause) ++ " ";
                    3 ->
                        lists:nth(rand:uniform(Query_Partition_Clause_Length), Query_Partition_Clause) ++ " Natural ";
                    _ -> []
                end,
                lists:nth(rand:uniform(Outer_Join_Type_Length), Outer_Join_Type),
                " Join ",
                lists:nth(rand:uniform(Join_Ref_Length), Join_Ref),
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Query_Partition_Clause_Length), Query_Partition_Clause);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Join_On_Or_Using_Clause_Length), Join_On_Or_Using_Clause);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% outer_join_type ::= ( 'FULL' ( 'OUTER' )? )
%%                   | ( 'LEFT' ( 'OUTER' )? )
%%                   | ( 'RIGHT' ( 'OUTER' )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(outer_join_type = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Full",
            "Full Outer",
            "Left",
            "Left Outer",
            "Right",
            "Right Outer"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameter = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            ":.PAR_1",
            ":0par_0",
            ":_par_3",
            ":par_1",
            ":par_1_",
            ":par_2 ",
            ":PAR_2",
            ":par_2_",
            ":PAR_3",
            ":par_3__",
            ":PAR_4",
            ":par_4__",
            ":par_4_",
            ":PAR_5",
            ":par_5_1",
            ":par_5_2_",
            ":PAR_62",
            ":PAR_63",
            ":par_63__",
            ":PAR_64",
            ":par_64__",
            ":par_64_",
            ":PAR_65",
            ":par_65_1",
            ":par_65_2_"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameter_ref ::= parameter ( ( 'INDICATOR' )? parameter )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameter_ref = Rule) ->
    ?CREATE_CODE_START,
    [{parameter, Parameter}] = dets:lookup(?CODE_TEMPLATES, parameter),
    Parameter_Length = length(Parameter),

    Code =
        [
                lists:nth(rand:uniform(Parameter_Length), Parameter) ++
                case rand:uniform(3) rem 3 of
                    1 ->
                        " Indicator " ++ lists:nth(rand:uniform(Parameter_Length), Parameter);
                    2 ->
                        " " ++ lists:nth(rand:uniform(Parameter_Length), Parameter);
                    _ -> []
                end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(target, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate ::= comparison_predicate
%%             | between_predicate
%%             | like_predicate
%%             | test_for_null
%%             | in_predicate
%%             | all_or_any_predicate
%%             | existence_test
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(predicate = Rule) ->
    ?CREATE_CODE_START,
    [{all_or_any_predicate, All_Or_Any_Predicate}] = dets:lookup(?CODE_TEMPLATES, all_or_any_predicate),
    All_Or_Any_Predicate_Length = length(All_Or_Any_Predicate),
    [{between_predicate, Between_Predicate}] = dets:lookup(?CODE_TEMPLATES, between_predicate),
    Between_Predicate_Length = length(Between_Predicate),
    [{comparison_predicate, Comparison_Predicate}] = dets:lookup(?CODE_TEMPLATES, comparison_predicate),
    Comparison_Predicate_Length = length(Comparison_Predicate),
    [{existence_test, Existence_Test}] = dets:lookup(?CODE_TEMPLATES, existence_test),
    Existence_Test_Length = length(Existence_Test),
    [{in_predicate, In_Predicate}] = dets:lookup(?CODE_TEMPLATES, in_predicate),
    In_Predicate_Length = length(In_Predicate),
    [{like_predicate, Like_Predicate}] = dets:lookup(?CODE_TEMPLATES, like_predicate),
    Like_Predicate_Length = length(Like_Predicate),
    [{test_for_null, Test_For_Null}] = dets:lookup(?CODE_TEMPLATES, test_for_null),
    Test_For_Null_Length = length(Test_For_Null),

    Code =
        [
            case rand:uniform(7) rem 7 of
                1 ->
                    lists:nth(rand:uniform(All_Or_Any_Predicate_Length), All_Or_Any_Predicate);
                2 ->
                    lists:nth(rand:uniform(Between_Predicate_Length), Between_Predicate);
                3 ->
                    lists:nth(rand:uniform(Comparison_Predicate_Length), Comparison_Predicate);
                4 ->
                    lists:nth(rand:uniform(Existence_Test_Length), Existence_Test);
                5 -> lists:nth(rand:uniform(In_Predicate_Length), In_Predicate);
                6 ->
                    lists:nth(rand:uniform(Like_Predicate_Length), Like_Predicate);
                _ ->
                    lists:nth(rand:uniform(Test_For_Null_Length), Test_For_Null)
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% procedure_call ::= ( 'DECLARE' 'BEGIN' function_ref_list 'END' )
%%                  | ( 'DECLARE' 'BEGIN' sql_list          'END' )
%%                  | (           'BEGIN' function_ref_list 'END' )
%%                  | (           'BEGIN' sql_list          'END' )
%%                  | ( 'CALL' function_ref )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(procedure_call = Rule) ->
    ?CREATE_CODE_START,
    [{function_ref, Function_Ref}] = dets:lookup(?CODE_TEMPLATES, function_ref),
    Function_Ref_Length = length(Function_Ref),
    [{function_ref_list, Function_Ref_List}] = dets:lookup(?CODE_TEMPLATES, function_ref_list),
    Function_Ref_List_Length = length(Function_Ref_List),
    [{sql_list, Sql_List}] = dets:lookup(?CODE_TEMPLATES, sql_list),
    Sql_List_Length = length(Sql_List),

    Code =
        [
            case rand:uniform(5) rem 5 of
                1 -> lists:append([
                    "Declare Begin ",
                    lists:nth(rand:uniform(Function_Ref_List_Length), Function_Ref_List),
                    " End"
                ]);
                2 -> lists:append([
                    "Declare Begin ",
                    lists:nth(rand:uniform(Sql_List_Length), Sql_List),
                    " End"
                ]);
                3 -> lists:append([
                    "Begin ",
                    lists:nth(rand:uniform(Function_Ref_List_Length), Function_Ref_List),
                    " End"
                ]);
                4 -> lists:append([
                    "Begin ",
                    lists:nth(rand:uniform(Sql_List_Length), Sql_List),
                    " End"
                ]);
                _ ->
                    "Call " ++ lists:nth(rand:uniform(Function_Ref_Length), Function_Ref)
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% proxy_clause ::= ( 'GRANT' | 'REVOKE' ) 'CONNECT' 'THROUGH' ( ( 'ENTERPRISE' 'USERS' ) | db_user_proxy )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(proxy_clause = Rule) ->
    ?CREATE_CODE_START,
    [{db_user_proxy, Db_User_Proxy}] = dets:lookup(?CODE_TEMPLATES, db_user_proxy),
    Db_User_Proxy_Length = length(Db_User_Proxy),

    Code =
        [
            lists:append([
                case rand:uniform(2) rem 2 of
                    1 -> "Grant";
                    _ -> "Revoke"
                end,
                " Connect Through",
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Db_User_Proxy_Length), Db_User_Proxy);
                    _ -> " Enterprise Users"
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% proxy_with ::= ( 'WITH' 'NO' 'ROLES' )
%%              | ( 'WITH' 'ROLE' role_list )
%%              | ( 'WITH' 'ROLE' 'ALL' 'EXCEPT' role_list )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(proxy_with = Rule) ->
    ?CREATE_CODE_START,
    [{role_list, Role_List}] = dets:lookup(?CODE_TEMPLATES, role_list),
    Role_List_Length = length(Role_List),

    Code =
        [
            "With No Roles"
        ] ++ [
        lists:append([
            case rand:uniform(2) rem 2 of
                1 -> "With Role";
                _ -> "With Role all except"
            end,
            " ",
            lists:nth(rand:uniform(Role_List_Length), Role_List)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_partition_clause ::= 'PARTITION' 'BY' ( ( '(' scalar_exp_commalist ')' ) | scalar_exp_commalist )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_partition_clause = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp_commalist, Scalar_Exp_Commalist}] = dets:lookup(?CODE_TEMPLATES, scalar_exp_commalist),
    Scalar_Exp_Commalist_Length = length(Scalar_Exp_Commalist),

    Code =
        [
                "Partition By" ++
                case rand:uniform(2) rem 2 of
                    1 -> lists:append([
                        " (",
                        lists:nth(rand:uniform(Scalar_Exp_Commalist_Length), Scalar_Exp_Commalist),
                        ")"
                    ]);
                    _ ->
                        " " ++ lists:nth(rand:uniform(Scalar_Exp_Commalist_Length), Scalar_Exp_Commalist)
                end

            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_exp ::= query_term
%%             | ( query_exp ( ( 'UNION' ( 'ALL' )? ) | 'INTERSECT' | 'MINUS' ) query_term )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_exp = Rule) ->
    ?CREATE_CODE_START,
    [{query_exp, Query_Exp}] = dets:lookup(?CODE_TEMPLATES, query_exp),
    Query_Exp_Length = length(Query_Exp),
    [{query_term, Query_Term}] = dets:lookup(?CODE_TEMPLATES, query_term),
    Query_Term_Length = length(Query_Term),

    Code =
        Query_Exp ++
        [
            lists:append([
                "(",
                bracket_query_spec(lists:nth(rand:uniform(Query_Exp_Length), Query_Exp)),
                case rand:uniform(3) rem 3 of
                    1 -> " Union" ++
                    case rand:uniform(2) rem 2 of
                        1 -> " All";
                        _ -> []
                    end;
                    2 -> " Intersect";
                    _ -> " Minus"
                end,
                " ",
                bracket_query_spec(lists:nth(rand:uniform(Query_Term_Length), Query_Term)),
                ")"
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    store_code(select_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(subquery, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_spec ::= 'SELECT' ( HINT )? ( 'ALL' | 'DISTINCT' )? selection ( 'INTO' target_commalist ( 'IN' NAME )? )? table_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_spec = Rule) ->
    ?CREATE_CODE_START,
    [{hint, Hint}] = dets:lookup(?CODE_TEMPLATES, hint),
    Hint_Length = length(Hint),
    [{into, Into}] = dets:lookup(?CODE_TEMPLATES, into),
    Into_Length = length(Into),
    [{selection, Selection}] = dets:lookup(?CODE_TEMPLATES, selection),
    Selection_Length = length(Selection),
    [{table_exp, Table_Exp}] = dets:lookup(?CODE_TEMPLATES, table_exp),
    Table_Exp_Length = length(Table_Exp),

    Code =
        [
            lists:append([
                "Select",
                case rand:uniform(2) rem 2 of
                    1 -> " " ++ lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end,
                case rand:uniform(3) rem 3 of
                    1 -> " All";
                    2 -> " Distinct";
                    _ -> []
                end,
                " ",
                lists:nth(rand:uniform(Selection_Length), Selection),
                case rand:uniform(2) rem 2 of
                    1 -> lists:nth(rand:uniform(Into_Length), Into);
                    _ -> []
                end,
                " ",
                lists:nth(rand:uniform(Table_Exp_Length), Table_Exp)
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(query_exp, Code, ?MAX_STATEMENT_SIMPLE, false),
    store_code(query_term, Code, ?MAX_STATEMENT_SIMPLE, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    store_code(select_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(subquery, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_term ::= query_spec
%%              | ( '(' query_exp ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_term = Rule) ->
    ?CREATE_CODE_START,
    [{query_exp, Query_Exp}] = dets:lookup(?CODE_TEMPLATES, query_exp),
    Query_Exp_Length = length(Query_Exp),

    Code =
        [
            lists:append([
                "(",
                lists:nth(rand:uniform(Query_Exp_Length), Query_Exp),
                ")"
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(query_exp, Code, ?MAX_STATEMENT_SIMPLE, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    store_code(select_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(subquery, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% quota ::= ( 'QUOTA' 'UNLIMITED' 'ON' NAME )
%%         | ( 'QUOTA' INTNUM ( NAME )? 'ON' NAME )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(quota = Rule) ->
    ?CREATE_CODE_START,
    [{intnum, Intnum}] = dets:lookup(?CODE_TEMPLATES, intnum),
    Intnum_Length = length(Intnum),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 ->
                    "Quota Unlimited On " ++ lists:nth(rand:uniform(Name_Length), Name);
                2 -> lists:append([
                    "Quota ",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    " ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    " On ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                _ -> lists:append([
                    "Quota ",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    " On ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% returning ::= ( 'RETURNING' | 'RETURN' ) selection 'INTO' selection
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(returning = Rule) ->
    ?CREATE_CODE_START,
    [{selection, Selection}] = dets:lookup(?CODE_TEMPLATES, selection),
    Selection_Length = length(Selection),

    Code =
        [
            lists:append([
                case rand:uniform(2) rem 2 of
                    1 -> "Returning";
                    _ -> "Return"
                end,
                " ",
                lists:nth(rand:uniform(Selection_Length), Selection),
                " Into ",
                lists:nth(rand:uniform(Selection_Length), Selection)
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% revoke_def ::= 'REVOKE' system_privilege_list ( on_obj_clause )? 'FROM' grantee ( ',' grantee )* ( with_revoke_option )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(revoke_def = Rule) ->
    ?CREATE_CODE_START,
    [{grantee, Grantee}] = dets:lookup(?CODE_TEMPLATES, grantee),
    Grantee_Length = length(Grantee),
    [{on_obj_clause, On_Obj_Clause}] = dets:lookup(?CODE_TEMPLATES, on_obj_clause),
    On_Obj_Clause_Length = length(On_Obj_Clause),
    [{system_privilege_list, System_Privilege_List}] = dets:lookup(?CODE_TEMPLATES, system_privilege_list),
    System_Privilege_List_Length = length(System_Privilege_List),
    [{with_revoke_option, With_Revoke_Option}] = dets:lookup(?CODE_TEMPLATES, with_revoke_option),
    With_Revoke_Option_Length = length(With_Revoke_Option),

    Code =
        [
            lists:append([
                "Revoke",
                case rand:uniform(3) rem 3 of
                    1 -> [];
                    _ ->
                        " " ++ lists:nth(rand:uniform(System_Privilege_List_Length), System_Privilege_List)
                end,
                case rand:uniform(3) rem 3 of
                    1 -> [];
                    _ ->
                        " " ++ lists:nth(rand:uniform(On_Obj_Clause_Length), On_Obj_Clause)
                end,
                " From ",
                case rand:uniform(3) rem 3 of
                    1 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(Grantee_Length), Grantee),
                        ",",
                        lists:nth(rand:uniform(Grantee_Length), Grantee),
                        ",",
                        lists:nth(rand:uniform(Grantee_Length), Grantee)
                    ]);
                    2 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(Grantee_Length), Grantee),
                        ",",
                        lists:nth(rand:uniform(Grantee_Length), Grantee)
                    ]);
                    _ -> " " ++ lists:nth(rand:uniform(Grantee_Length), Grantee)
                end,
                case rand:uniform(3) rem 3 of
                    1 -> [];
                    _ ->
                        " " ++ lists:nth(rand:uniform(With_Revoke_Option_Length), With_Revoke_Option)
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% role_list ::= NAME ( ',' NAME )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(role_list = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ",",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ",",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ",",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ",",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ",",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ",",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                _ -> lists:nth(rand:uniform(Name_Length), Name)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% rollback_statement ::= 'ROLLBACK' ( 'WORK' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(rollback_statement = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Rollback",
            "Rollback Work"
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scalar_exp ::= scalar_sub_exp ( '||' scalar_exp )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(scalar_exp = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),
    [{scalar_sub_exp, Scalar_Sub_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_sub_exp),
    Scalar_Sub_Exp_Length = length(Scalar_Sub_Exp),

    Code =
        Scalar_Exp ++
        [
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Sub_Exp_Length), Scalar_Sub_Exp)) ++
                case rand:uniform(2) rem 2 of
                    1 ->
                        " || " ++ bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp));
                    _ ->
                        []
                end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scalar_exp_commalist ::= scalar_opt_as_exp ( ',' scalar_opt_as_exp )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(scalar_exp_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                    ",",
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                    ",",
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                    ",",
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                    ",",
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                    ",",
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp),
                    ",",
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
                ]);
                _ ->
                    lists:nth(rand:uniform(Scalar_Opt_As_Exp_Length), Scalar_Opt_As_Exp)
            end

            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scalar_opt_as_exp ::= ( scalar_exp ( COMPARISON scalar_exp )? )
%%                     | ( scalar_exp ( AS )? NAME )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(scalar_opt_as_exp = Rule) ->
    ?CREATE_CODE_START,
    [{comparison, Comparison}] = dets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                    lists:nth(rand:uniform(Comparison_Length), Comparison),
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp))
                ]);
                _ -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                    case rand:uniform(2) rem 2 of
                        1 -> " As";
                        _ -> []
                    end,
                    " ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(comparison_predicate, Code, ?MAX_BASIC, false),
    store_code(select_field, Code, ?MAX_BASIC, false),
    store_code(select_field_commalist, Code, ?MAX_BASIC, false),
    store_code(selection, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scalar_sub_exp ::= ( scalar_sub_exp ( '+' | '-' | '*' | '/' | 'div' ) scalar_sub_exp )
%%                  | ( ( '+' | '-' ) scalar_sub_exp )
%%                  | ( ( '+' | '-' ) literal )                    RULE OBSOLETE
%%                  | 'NULL'
%%                  | atom
%%                  | subquery
%%                  | column_ref
%%                  | function_ref
%%                  | ( '(' scalar_sub_exp ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(scalar_sub_exp = Rule) ->
    ?CREATE_CODE_START,
    [{literal, Literal}] = dets:lookup(?CODE_TEMPLATES, literal),
    Literal_Length = length(Literal),
    [{scalar_sub_exp, Scalar_Sub_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_sub_exp),
    Scalar_Sub_Exp_Length = length(Scalar_Sub_Exp),

    Code =
        Scalar_Sub_Exp ++
        [
            "Null"
        ] ++ [
        case rand:uniform(3) rem 3 of
            1 -> lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Sub_Exp_Length), Scalar_Sub_Exp)),
                case rand:uniform(5) rem 5 of
                    1 -> " + ";
                    2 -> " - ";
                    3 -> " * ";
                    4 -> " / ";
                    _ -> " div "
                end,
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Sub_Exp_Length), Scalar_Sub_Exp))
            ]);
            2 -> case rand:uniform(2) rem 2 of
                     1 -> "+";
                     _ -> "-"
                 end ++
            case rand:uniform(2) rem 2 of
                1 ->
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Sub_Exp_Length), Scalar_Sub_Exp));
                _ -> lists:nth(rand:uniform(Literal_Length), Literal)
            end;
            _ -> lists:append([
                "(",
                lists:nth(rand:uniform(Scalar_Sub_Exp_Length), Scalar_Sub_Exp),
                ")"
            ])
        end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% schema ::= 'CREATE' 'SCHEMA' 'AUTHORIZATION' NAME ( schema_element ( schema_element )* )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(schema = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{schema_element, Schema_Element}] = dets:lookup(?CODE_TEMPLATES, schema_element),
    Schema_Element_Length = length(Schema_Element),

    Code =
        [
            lists:append([
                "Create Schema Authorization ",
                lists:nth(rand:uniform(Name_Length), Name),
                case rand:uniform(5) rem 5 of
                    1 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element),
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element),
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element),
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element)
                    ]);
                    2 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element),
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element),
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element)
                    ]);
                    3 -> lists:append([
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element),
                        " ",
                        lists:nth(rand:uniform(Schema_Element_Length), Schema_Element)
                    ]);
                    4 ->
                        " " ++ lists:nth(rand:uniform(Schema_Element_Length), Schema_Element);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% schema_element ::= create_role_def
%%                  | create_table_def
%%                  | create_index_def
%%                  | create_user_def
%%                  | view_def
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(schema_element = Rule) ->
    ?CREATE_CODE_START,
    [{create_index_def, Create_Index_Def}] = dets:lookup(?CODE_TEMPLATES, create_index_def),
    Create_Index_Def_Length = length(Create_Index_Def),
    [{create_role_def, Create_Role_Def}] = dets:lookup(?CODE_TEMPLATES, create_role_def),
    Create_Role_Def_Length = length(Create_Role_Def),
    [{create_table_def, Create_Table_Def}] = dets:lookup(?CODE_TEMPLATES, create_table_def),
    Create_Table_Def_Length = length(Create_Table_Def),
    [{create_user_def, Create_User_Def}] = dets:lookup(?CODE_TEMPLATES, create_user_def),
    Create_User_Def_Length = length(Create_User_Def),
    [{view_def, View_Def}] = dets:lookup(?CODE_TEMPLATES, view_def),
    View_Def_Length = length(View_Def),

    Code =
        [
            case rand:uniform(5) rem 5 of
                1 ->
                    lists:nth(rand:uniform(Create_Index_Def_Length), Create_Index_Def);
                2 ->
                    lists:nth(rand:uniform(Create_Role_Def_Length), Create_Role_Def);
                3 ->
                    lists:nth(rand:uniform(Create_Table_Def_Length), Create_Table_Def);
                4 ->
                    lists:nth(rand:uniform(Create_User_Def_Length), Create_User_Def);
                _ -> lists:nth(rand:uniform(View_Def_Length), View_Def)
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% schema_element ::= create_role_def
%%                  | ...
%%                  | create_index_def
%%                  | create_user_def
%%                  | ...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(schema_element_1 = _Rule) ->
    ?CREATE_CODE_START,
    [{create_index_def, Create_Index_Def}] = dets:lookup(?CODE_TEMPLATES, create_index_def),
    Create_Index_Def_Length = length(Create_Index_Def),
    [{create_role_def, Create_Role_Def}] = dets:lookup(?CODE_TEMPLATES, create_role_def),
    Create_Role_Def_Length = length(Create_Role_Def),
    [{create_user_def, Create_User_Def}] = dets:lookup(?CODE_TEMPLATES, create_user_def),
    Create_User_Def_Length = length(Create_User_Def),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 ->
                    lists:nth(rand:uniform(Create_Index_Def_Length), Create_Index_Def);
                2 ->
                    lists:nth(rand:uniform(Create_Role_Def_Length), Create_Role_Def);
                _ ->
                    lists:nth(rand:uniform(Create_User_Def_Length), Create_User_Def)
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(schema_element, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% search_condition ::= ( search_condition ( 'AND' | 'OR' ) search_condition )
%%                    | ( 'NOT' search_condition )
%%                    | ( '(' search_condition ')' )
%%                    | predicate
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(search_condition = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),

    Code_1 =
        Search_Condition ++
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition),
                    " And ",
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition),
                    " Or ",
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
                ]);
                _ ->
                    "Not " ++ lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    Code =
        [
            lists:append(["(", C, ")"]) || C <- Code_1 ++ Search_Condition
        ],
    store_code(Rule, Code ++ Code_1, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% select_field ::= ( case_when_exp ( ( 'AS' )? NAME )? )
%%                | scalar_opt_as_exp
%%                | '*'
%%
%% select_field_commalist ::= select_field ( ',' select_field )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(select_field_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{case_when_exp, Case_When_Exp}] = dets:lookup(?CODE_TEMPLATES, case_when_exp),
    Case_When_Exp_Length = length(Case_When_Exp),
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code_1 =
        [
            "*"
        ] ++ [
            lists:nth(rand:uniform(Case_When_Exp_Length), Case_When_Exp) ++
            case rand:uniform(3) rem 3 of
                1 -> " As " ++ lists:nth(rand:uniform(Name_Length), Name);
                2 -> " " ++ lists:nth(rand:uniform(Name_Length), Name);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code_1, ?MAX_BASIC, false),
    store_code(selec_field, Code_1, ?MAX_BASIC, false),
    store_code(select_field_commalist, Code_1, ?MAX_BASIC, false),
    store_code(selection, Code_1, ?MAX_BASIC, false),

    [{select_field, Select_Field}] = dets:lookup(?CODE_TEMPLATES, select_field),
    Select_Field_Length = length(Select_Field),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field),
                    ",",
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field),
                    ",",
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field),
                    ",",
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field),
                    ",",
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field),
                    ",",
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field),
                    ",",
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field)
                ]);
                _ ->
                    lists:nth(rand:uniform(Select_Field_Length), Select_Field)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(selection, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special variations.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(special = Rule) ->
    ?CREATE_CODE_START,
    Code = [
        %% ---------------------------------------------------------------------
        %% Boolean and arithmetic binary operators handled with precedence
        %% ---------------------------------------------------------------------
        "Select 5 + 7 From dual",
        "Select 5 - 7 From dual",
        "Select 5 * 7 From dual",
        "Select 5 / 7 From dual",
        "Select 2 + 3 * 3 + 4 From dual",
        "Select 2 - 3 * 3 - 4 From dual",
        "Select 2 + 3 / 3 + 4 From dual",
        "Select 2 - 3 / 3 - 4 From dual",
        "Select 2 * 3 + 3 * 4 From dual",
        "Select 2 * 3 - 3 * 4 From dual",
        "Select 2 / 3 + 3 / 4 From dual",
        "Select 2 / 3 - 3 / 4 From dual",
        "Select (2 + 3) * (3 - 4) From dual",
        "Select (2 + 3) / (3 - 4) From dual",
        "Select (2 * 3) - (3 / 4) From dual",
        "Select (2 / 3) + (3 * 4) From dual",
        "Select (Select * From Dual) * (3 - 4) From dual",
        "Select (Select * From Dual) / (3 - 4) From dual",
        "Select (Select * From Dual) - (3 / 4) From dual",
        "Select (Select * From Dual) + (3 * 4) From dual",
        "Select (2 + 3) * (Select * From Dual) From dual",
        "Select (2 + 3) / (Select * From Dual) From dual",
        "Select (2 - 3) * (Select * From Dual) From dual",
        "Select (2 - 3) / (Select * From Dual) From dual",
        "Select (2 * 3) + (Select * From Dual) From dual",
        "Select (2 / 3) + (Select * From Dual) From dual",
        "Select (2 * 3) - (Select * From Dual) From dual",
        "Select (2 / 3) - (Select * From Dual) From dual",
        %% ---------------------------------------------------------------------
        %% changed: JSON
        %% ---------------------------------------------------------------------
        %% create_index_spec_items -> JSON
        %% create_index_spec_items -> JSON '|' create_index_spec_items
        %% ---------------------------------------------------------------------
        "Create Index a On b (|:d{}|)",
        "Create Index a On b (c | |:d{}|)",
        %% ---------------------------------------------------------------------
        %% column_ref -> JSON
        %% column_ref -> NAME     JSON
        %% column_ref -> NAME '.' NAME     JSON
        %% ---------------------------------------------------------------------
        "Select |:a:b| From x",
        "Select column_name|:a:b| From x",
        "Select table_name.column_name|:a:b| From x",
        "Select column_name From x",
        "Select table_name.column_name From x",
        "Select schema_name.table_name.column_name From x",
        %% ---------------------------------------------------------------------
        %% Problem: ALL
        %% ---------------------------------------------------------------------
        %% function_ref -> FUNS     '(' ALL scalar_exp ')'
        %% ---------------------------------------------------------------------
        "Call Upper (All Null)",
        "Call Upper (All 5)",
        "Call Upper (All 'text')",
        "Call Upper (All |:_a1:f()|)",
        "Call Upper (All name|:_a1:f()|)",
        "Call Upper (All name1.name2|:_a1:f()|)",
        "Call Upper (All name)",
        "Call Upper (All name1.name2)",
        "Call Upper (All name1.name2.name3)",
        %% ---------------------------------------------------------------------
        %% Problem: data_type with parenteheses
        %% ---------------------------------------------------------------------
        %% data_type -> NAME '(' opt_sgn_num ')'
        %% data_type -> NAME '(' opt_sgn_num ',' opt_sgn_num ')'
        %% ---------------------------------------------------------------------
        "Create Table table_name (column_name name (1))",
        "Create Table table_name (column_name name (-1))",
        "Create Table table_name (column_name name (1, 2))",
        "Create Table table_name (column_name name (-1, -2))",
        %% ---------------------------------------------------------------------
        %% Problem: INSERT
        %% ---------------------------------------------------------------------
        %% insert_statement -> INSERT INTO table opt_column_commalist values_or_query_spec returning
        %% ---------------------------------------------------------------------
        "Insert Into table_name Return column_name_a Into column_name_b",
        "Insert Into table_name Values (1)",
        "Insert Into table_name Select source_column From source_table",
        "Insert Into table_name (column_name) Values (1)",
        "Insert Into table_name ('column_string') Values (1)",
        "Insert Into table_name (column_name) Select source_column From source_table",
        "Insert Into table_name ('column_string') Select source_column From source_table",
        %% ---------------------------------------------------------------------
        %% Problem: ORDER BY
        %% ---------------------------------------------------------------------
        %% opt_order_by_clause -> ORDER BY ordering_spec_commalist
        %% ---------------------------------------------------------------------
        "Select * From table_name Order By column_name",
        %% ---------------------------------------------------------------------
        %% Problem: Optional Returning phrase
        %% ---------------------------------------------------------------------
        %% returning -> RETURNING selection INTO selection
        %% returning -> RETURN    selection INTO selection
        %% ---------------------------------------------------------------------
        "Insert Into table_name (column_name) Values (1) Return result_column Into show_column",
        %% ---------------------------------------------------------------------
        %% Problem: subquery as select_field
        %% ---------------------------------------------------------------------
        %% select_field -> scalar_opt_as_exp
        %% ---------------------------------------------------------------------
        "Select (Select * From dual) From dual"
    ],
    dets:insert(?CODE_TEMPLATES, {Rule, Code}),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sql ::= procedure_call
%%       | schema
%%       | cursor_def
%%       | manipulative_statement
%%       | 'WHENEVER' 'NOT' 'FOUND' when_action
%%       | 'WHENEVER' 'SQLERROR' when_action
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sql = Rule) ->
    ?CREATE_CODE_START,
    [{cursor_def, Cursor_Def}] = dets:lookup(?CODE_TEMPLATES, cursor_def),
    Cursor_Def_Length = length(Cursor_Def),
    [{manipulative_statement, Manipulative_Statement}] = dets:lookup(?CODE_TEMPLATES, manipulative_statement),
    Manipulative_Statement_Length = length(Manipulative_Statement),
    [{procedure_call, Procedure_Call}] = dets:lookup(?CODE_TEMPLATES, procedure_call),
    Procedure_Call_Length = length(Procedure_Call),
    [{schema, Schema}] = dets:lookup(?CODE_TEMPLATES, schema),
    Schema_Length = length(Schema),
    [{whenever, Whenever}] = dets:lookup(?CODE_TEMPLATES, whenever),
    Whenever_Length = length(Whenever),

    Code =
        [
            case rand:uniform(5) rem 5 of
                1 -> lists:nth(rand:uniform(Cursor_Def_Length), Cursor_Def);
                2 ->
                    lists:nth(rand:uniform(Manipulative_Statement_Length), Manipulative_Statement);
                3 ->
                    lists:nth(rand:uniform(Procedure_Call_Length), Procedure_Call);
                4 -> lists:nth(rand:uniform(Schema_Length), Schema);
                _ -> lists:nth(rand:uniform(Whenever_Length), Whenever)
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sql ::= ...
%%       | schema
%%       | cursor_def
%%       | manipulative_statement
%%       | 'WHENEVER' 'NOT' 'FOUND' when_action
%%       | 'WHENEVER' 'SQLERROR' when_action
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sql_1 = _Rule) ->
    ?CREATE_CODE_START,
    [{cursor_def, Cursor_Def}] = dets:lookup(?CODE_TEMPLATES, cursor_def),
    Cursor_Def_Length = length(Cursor_Def),
    [{manipulative_statement, Manipulative_Statement}] = dets:lookup(?CODE_TEMPLATES, manipulative_statement),
    Manipulative_Statement_Length = length(Manipulative_Statement),
    [{schema, Schema}] = dets:lookup(?CODE_TEMPLATES, schema),
    Schema_Length = length(Schema),
    [{whenever, Whenever}] = dets:lookup(?CODE_TEMPLATES, whenever),
    Whenever_Length = length(Whenever),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:nth(rand:uniform(Cursor_Def_Length), Cursor_Def);
                2 ->
                    lists:nth(rand:uniform(Manipulative_Statement_Length), Manipulative_Statement);
                3 -> lists:nth(rand:uniform(Schema_Length), Schema);
                _ -> lists:nth(rand:uniform(Whenever_Length), Whenever)
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sql_list ::= sql ';' ( extra )? ( sql ';' ( extra )? )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sql_list = Rule) ->
    ?CREATE_CODE_START,
    [{extra, Extra}] = dets:lookup(?CODE_TEMPLATES, extra),
    Extra_Length = length(Extra),
    [{sql, Sql}] = dets:lookup(?CODE_TEMPLATES, sql),
    Sql_Length = length(Sql),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end
                ]);
                _ -> lists:append([
                    lists:nth(rand:uniform(Sql_Length), Sql),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(Extra_Length), Extra);
                        _ -> []
                    end
                ])
            end
            || _ <- lists:seq(1, ?MAX_SQL * 2)
        ],
    store_code(Rule, Code, ?MAX_SQL, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\'([^\']*(\'\')*)*\')
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(string = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "' string_0 '",
            "' string_3'",
            "'STRING''_1'",
            "'string''_2$'",
            "'string_1'",
            "'string_1_'",
            "'string_2 '",
            "'STRING_2'",
            "'string_3$_'",
            "'STRING_3'",
            "'string_4$_'",
            "'STRING_4'",
            "'string_4_'",
            "'STRING_5'",
            "'string_5_1'",
            "'string_5_2_'",
            "'STRING_62'",
            "'string_63$_'",
            "'STRING_63'",
            "'string_64$_'",
            "'STRING_64'",
            "'string_64_'",
            "'STRING_65'",
            "'string_65_1'",
            "'string_65_2_'"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(column, Code, ?MAX_BASIC, false),
    store_code(data_type, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(table, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% system_privilege ::= 'SELECT'
%%                    | 'UPDATE'
%%                    | 'DELETE'
%%                    | 'INSERT'
%%                    | 'DROP'
%%                    | ( NAME ( NAME ( NAME ( NAME )? )? )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(system_privilege = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "Select",
            "Update",
            "Delete",
            "Insert",
            "Drop"
        ] ++ [
        case rand:uniform(4) rem 4 of
            1 -> lists:append([
                lists:nth(rand:uniform(Name_Length), Name),
                " ",
                lists:nth(rand:uniform(Name_Length), Name),
                " ",
                lists:nth(rand:uniform(Name_Length), Name),
                " ",
                lists:nth(rand:uniform(Name_Length), Name)
            ]);
            2 -> lists:append([
                lists:nth(rand:uniform(Name_Length), Name),
                " ",
                lists:nth(rand:uniform(Name_Length), Name),
                " ",
                lists:nth(rand:uniform(Name_Length), Name)
            ]);
            3 -> lists:append([
                lists:nth(rand:uniform(Name_Length), Name),
                " ",
                lists:nth(rand:uniform(Name_Length), Name)
            ]);
            _ -> lists:nth(rand:uniform(Name_Length), Name)
        end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% system_privilege_list ::= ( system_privilege ( ',' system_privilege )* )
%%                         | ( 'ALL' ( 'PRIVILEGES' )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(system_privilege_list = Rule) ->
    ?CREATE_CODE_START,
    [{system_privilege, System_Privilege}] = dets:lookup(?CODE_TEMPLATES, system_privilege),
    System_Privilege_Length = length(System_Privilege),

    Code =
        [
            "All",
            "All Privileges"
        ] ++ [
        case rand:uniform(4) rem 4 of
            1 -> lists:append([
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege),
                ",",
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege),
                ",",
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege),
                ",",
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege)
            ]);
            2 -> lists:append([
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege),
                ",",
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege),
                ",",
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege)
            ]);
            3 -> lists:append([
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege),
                ",",
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege)
            ]);
            _ ->
                lists:nth(rand:uniform(System_Privilege_Length), System_Privilege)
        end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table ::= ( ( NAME '.' )? NAME ( NAME )? )
%%         | STRING
%%         | ( parameter ( NAME )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(table = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{parameter, Parameter}] = dets:lookup(?CODE_TEMPLATES, parameter),
    Parameter_Length = length(Parameter),
    [{string, String}] = dets:lookup(?CODE_TEMPLATES, string),
    String_Length = length(String),

    Code =
        [
            case rand:uniform(7) rem 7 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    " ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    " ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                4 -> lists:nth(rand:uniform(Name_Length), Name);
                5 -> lists:nth(rand:uniform(String_Length), String);
                6 -> lists:append([
                    lists:nth(rand:uniform(Parameter_Length), Parameter),
                    " ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                _ -> lists:nth(rand:uniform(Parameter_Length), Parameter)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(join_ref, Code, ?MAX_BASIC, false),
    store_code(table_ref, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table_constraint_def ::= ( 'UNIQUE'        '(' column_commalist ')' )
%%                        | ( 'PRIMARY' 'KEY' '(' column_commalist ')' )
%%                        | ( 'FOREIGN' 'KEY' '(' column_commalist ')' 'REFERENCES' table ( '(' column_commalist ')' )? )
%%                        | ( 'CHECK' '(' search_condition ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(table_constraint_def = Rule) ->
    ?CREATE_CODE_START,
    [{column_commalist, Column_Commalist}] = dets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    "Unique (",
                    lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                    ")"
                ]);
                2 -> lists:append([
                    "Primary Key (",
                    lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                    ")"
                ]);
                3 -> lists:append([
                    "Foreign Key (",
                    lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                    ")",
                    " References ",
                    lists:nth(rand:uniform(Table_Length), Table),
                    case rand:uniform(2) rem 2 of
                        1 -> lists:append([
                            "(",
                            lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                            ")"
                        ]);
                        _ -> []
                    end
                ]);
                _ -> lists:append([
                    "Check (",
                    lists:nth(rand:uniform(Search_Condition_Length), Search_Condition),
                    ")"
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, 0, false),
    store_code(base_table_element, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table_exp ::= 'FROM' from_column ( from_column )* ( where_clause )? ( hierarchical_query_clause )? ( 'GROUP' 'BY' column_ref_commalist )? ( 'HAVING' search_condition )? ( order_by_clause )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(table_exp = Rule) ->
    ?CREATE_CODE_START,
    [{from_clause, From_Clause}] = dets:lookup(?CODE_TEMPLATES, from_clause),
    From_Clause_Length = length(From_Clause),
    [{group_by_clause, Group_By_Clause}] = dets:lookup(?CODE_TEMPLATES, group_by_clause),
    Group_By_Clause_Length = length(Group_By_Clause),
    [{having_clause, Having_Clause}] = dets:lookup(?CODE_TEMPLATES, having_clause),
    Having_Clause_Length = length(Having_Clause),
    [{hierarchical_query_clause, Hierarchical_Query_Clause}] = dets:lookup(?CODE_TEMPLATES, hierarchical_query_clause),
    Hierarchical_Query_Clause_Length = length(Hierarchical_Query_Clause),
    [{order_by_clause, Order_By_Clause}] = dets:lookup(?CODE_TEMPLATES, order_by_clause),
    Order_By_Clause_Length = length(Order_By_Clause),
    [{where_clause, Where_Clause}] = dets:lookup(?CODE_TEMPLATES, where_clause),
    Where_Clause_Length = length(Where_Clause),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(From_Clause_Length), From_Clause),
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Where_Clause_Length), Where_Clause);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Hierarchical_Query_Clause_Length), Hierarchical_Query_Clause);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Group_By_Clause_Length), Group_By_Clause);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Having_Clause_Length), Having_Clause);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Order_By_Clause_Length), Order_By_Clause);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table_name ::= ( ( NAME '.' )? NAME '.' )? NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(table_name = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                _ -> lists:nth(rand:uniform(Name_Length), Name)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table_ref ::= table
%%             | ( '(' query_exp ')' ( NAME )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(table_ref = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{query_exp, Query_Exp}] = dets:lookup(?CODE_TEMPLATES, query_exp),
    Query_Exp_Length = length(Query_Exp),

    Code =
        [
            lists:append([
                "(",
                lists:nth(rand:uniform(Query_Exp_Length), Query_Exp),
                ")",
                case rand:uniform(2) rem 2 of
                    1 -> " " ++ lists:nth(rand:uniform(Name_Length), Name);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(from_column, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table_ref ::= ( table )
%%             | ( '(' query_exp ')' ( NAME )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(table_ref_1 = _Rule) ->
    ?CREATE_CODE_START,
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            lists:nth(rand:uniform(Table_Length), Table)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(table_ref, Code, ?MAX_BASIC, false),
    store_code(from_column, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% target_commalist ::= target ( ',' target )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(target_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{target, Target}] = dets:lookup(?CODE_TEMPLATES, target),
    Target_Length = length(Target),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Target_Length), Target),
                    ",",
                    lists:nth(rand:uniform(Target_Length), Target),
                    ",",
                    lists:nth(rand:uniform(Target_Length), Target),
                    ",",
                    lists:nth(rand:uniform(Target_Length), Target)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Target_Length), Target),
                    ",",
                    lists:nth(rand:uniform(Target_Length), Target),
                    ",",
                    lists:nth(rand:uniform(Target_Length), Target)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Target_Length), Target),
                    ",",
                    lists:nth(rand:uniform(Target_Length), Target)
                ]);
                _ -> lists:nth(rand:uniform(Target_Length), Target)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tbl_scope ::= 'LOCAL'
%%             | 'CLUSTER'
%%             | 'SCHEMA'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(tbl_scope = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Cluster",
            "Local",
            "Schema"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tbl_type ::= 'SET'
%%            | 'ORDERED_SET'
%%            | 'BAG'
%%            | NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(tbl_type = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Bag",
            "Name",
            "Ordered_set",
            "Set"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test_for_null ::= scalar_exp 'IS' ( 'NOT' )? 'NULL'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(test_for_null = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp, Scalar_Exp}] = dets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),

    Code =
        [
            lists:append([
                bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                " Is",
                case rand:uniform(2) rem 2 of
                    1 -> " Not";
                    _ -> []
                end,
                " Null"
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% truncate_table ::= 'TRUNCATE' 'TABLE' table_name ( ( 'PRESERVE' | 'PURGE' ) 'MATERIALIZED' 'VIEW' 'LOG' )? ( ( 'DROP' | 'REUSE' ) 'STORAGE' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(truncate_table = Rule) ->
    ?CREATE_CODE_START,
    [{table_name, Table_Name}] = dets:lookup(?CODE_TEMPLATES, table_name),
    Table_Name_Length = length(Table_Name),

    Code =
        [
            lists:append([
                "Truncate Table ",
                lists:nth(rand:uniform(Table_Name_Length), Table_Name),
                case rand:uniform(3) rem 3 of
                    1 -> " Preserve Materialized View Log";
                    2 -> " Purge Materialized View Log";
                    _ -> []
                end,
                case rand:uniform(3) rem 3 of
                    1 -> " Drop Storage";
                    2 -> " Reuse Storage";
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% update_statement_positioned ::= 'UPDATE' table 'SET' assignment_commalist 'WHERE' 'CURRENT' 'OF' cursor ( returning )?
%% update_statement_searched ::= 'UPDATE' table 'SET' assignment_commalist ( where_clause )? ( returning )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(update_statement = Rule) ->
    ?CREATE_CODE_START,
    [{assignment_commalist, Assignment_Commalist}] = dets:lookup(?CODE_TEMPLATES, assignment_commalist),
    Assignment_Commalist_Length = length(Assignment_Commalist),
    [{cursor, Cursor}] = dets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{returning, Returning}] = dets:lookup(?CODE_TEMPLATES, returning),
    Returning_Length = length(Returning),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),
    [{where_clause, Where_Clause}] = dets:lookup(?CODE_TEMPLATES, where_clause),
    Where_Clause_Length = length(Where_Clause),

    Code =
        [
            lists:append([
                "Update ",
                lists:nth(rand:uniform(Table_Length), Table),
                " Set ",
                lists:nth(rand:uniform(Assignment_Commalist_Length), Assignment_Commalist),
                case rand:uniform(3) rem 3 of
                    1 ->
                        " Where Current Of " ++ lists:nth(rand:uniform(Cursor_Length), Cursor);
                    2 ->
                        " " ++ lists:nth(rand:uniform(Where_Clause_Length), Where_Clause);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " " ++ lists:nth(rand:uniform(Returning_Length), Returning);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 4)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user_opt ::= ( ( 'DEFAULT' | 'TEMPORARY' ) 'TABLESPACE' NAME )
%%            | ( quota  ( quota )* )
%%            | ( 'PROFILE' NAME )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(user_opt = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{quota, Quota}] = dets:lookup(?CODE_TEMPLATES, quota),
    Quota_Length = length(Quota),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    case rand:uniform(2) rem 2 of
                        1 -> "Default";
                        _ -> "Temporary"
                    end,
                    " Tablespace ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]);
                2 -> case rand:uniform(3) rem 3 of
                         1 -> lists:append([
                             lists:nth(rand:uniform(Quota_Length), Quota),
                             " ",
                             lists:nth(rand:uniform(Quota_Length), Quota),
                             " ",
                             lists:nth(rand:uniform(Quota_Length), Quota)
                         ]);
                         2 -> lists:append([
                             lists:nth(rand:uniform(Quota_Length), Quota),
                             " ",
                             lists:nth(rand:uniform(Quota_Length), Quota)
                         ]);
                         _ -> lists:nth(rand:uniform(Quota_Length), Quota)
                     end;
                _ -> "Profile " ++ lists:nth(rand:uniform(Name_Length), Name)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(spec_item, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user_role ::= 'DEFAULT' 'ROLE' ( ( 'ALL' ( 'EXCEPT' role_list )? ) | NONE | role_list )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(user_role = Rule) ->
    ?CREATE_CODE_START,
    [{role_list, Role_List}] = dets:lookup(?CODE_TEMPLATES, role_list),
    Role_List_Length = length(Role_List),

    Code =
        [
            "Default Role All",
            "Default Role None"
        ] ++ [
            "Default Role" ++
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    " All Except",
                    " ",
                    lists:nth(rand:uniform(Role_List_Length), Role_List)
                ]);
                _ -> " " ++ lists:nth(rand:uniform(Role_List_Length), Role_List)
            end
        || _ <- lists:seq(1, ?MAX_BASIC * 2)
    ],
    store_code(Rule, Code, 0, false),
    store_code(spec_item, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% view_def ::= ( 'CREATE' 'VIEW' table ( '(' column_commalist ')' )? )
%%            | ( 'AS' query_spec ( 'WITH' 'CHECK' 'OPTION' )? )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(view_def = Rule) ->
    ?CREATE_CODE_START,
    [{column_commalist, Column_Commalist}] = dets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{query_spec, Query_Spec}] = dets:lookup(?CODE_TEMPLATES, query_spec),
    Query_Spec_Length = length(Query_Spec),
    [{table, Table}] = dets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    "Create View ",
                    lists:nth(rand:uniform(Table_Length), Table),
                    case rand:uniform(5) rem 5 of
                        1 -> [];
                        _ -> lists:append([
                            " (",
                            lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                            ")"
                        ])
                    end
                ]);
                _ -> lists:append([
                    "As ",
                    lists:nth(rand:uniform(Query_Spec_Length), Query_Spec),
                    case rand:uniform(2) rem 2 of
                        1 -> " With Check Option";
                        _ -> []
                    end
                ])
            end
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% when_action ::= ( 'GOTO' NAME )
%%               | 'CONTINUE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(when_action = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = dets:lookup(?CODE_TEMPLATES, name),

    Code =
        [
            "Continue"
        ] ++ [
            "Goto " ++ N || N <- Name
    ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sql ::= ...
%%       | WHENEVER NOT FOUND when_action
%%       | WHENEVER SQLERROR when_action
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(whenever = Rule) ->
    ?CREATE_CODE_START,
    [{when_action, When_Action}] = dets:lookup(?CODE_TEMPLATES, when_action),
    When_Action_Length = length(When_Action),

    Code =
        [
            lists:append([
                "Whenever",
                case rand:uniform(2) rem 2 of
                    1 -> " Not Found";
                    _ -> " Sqlerror"
                end,
                " ",
                lists:nth(rand:uniform(When_Action_Length), When_Action)
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% where_clause ::= 'WHERE' search_condition
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(where_clause = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = dets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),

    Code =
        [
                "Where " ++ lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% with_grant_option ::= 'WITH' ( 'GRANT' | NAME | 'HIERARCHY' ) 'OPTION'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(with_grant_option = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "With Grant Option",
            "With Name Option",
            "With Hierarchy Option"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% with_revoke_option ::= ( 'CASCADE' 'CONSTRAINTS' )
%%                      | 'FORCE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(with_revoke_option = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Cascade Constraints",
            "Force"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating Common Test data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_Type, _CompleteSQL, _CompactedDetailed, []) ->
    ok;
file_create_ct_all(Type, CompleteSemicolon, CompactedDetailed, [Rule | Rules]) ->
    file_create_ct(Type, CompleteSemicolon, CompactedDetailed, Rule),
    file_create_ct_all(Type, CompleteSemicolon, CompactedDetailed, Rules).

file_create_ct(Type, CompleteSemicolon, CompactedDetailed, Rule) ->
    [{Rule, Code}] = dets:lookup(?CODE_TEMPLATES, Rule),

    CodeLength = length(Code),
    RuleString = atom_to_list(Rule),

    FileName = lists:append([Type, "_", CompleteSemicolon, "_", CompactedDetailed, "_", RuleString, "_SUITE"]),
    {ok, File, _} = file:path_open([?PATH_CT], FileName ++ ".erl", [write]),

    erlang:display(io:format("final common tests ===> ~12.. B file_name: ~s ", [CodeLength, FileName ++ ".erl"])),

    {{Current_Year, Current_Month, Current_Day}, _} = calendar:local_time(),

    io:format(File, "~s~n", ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", [lists:append(["%%% File        : ", FileName, ".erl"])]),
    io:format(File, "~s~n", [lists:append(["%%% Description : Test Suite for rule: ", RuleString, "."])]),
    io:format(File, "~s~n", ["%%%"]),
    io:format(File, "~s~n", ["%%% Created     : " ++ lists:flatten(io_lib:format("~2..0w.~2..0w.~4..0w", [Current_Day, Current_Month, Current_Year]))]),
    io:format(File, "~s~n", ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", [lists:append(["-module(", FileName, ")."])]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-export(["]),
    io:format(File, "~s~n", ["    all/0,"]),
    io:format(File, "~s~n", ["    end_per_suite/1,"]),
    io:format(File, "~s~n", ["    init_per_suite/1,"]),

    case CodeLength of
        0 -> io:format(File, "~s~n", ["    suite/0"]);
        _ -> io:format(File, "~s~n", ["    suite/0,"]),
            case CompactedDetailed of
                "compacted" ->
                    io:format(File, "~s~n", [lists:append(["    test_compacted/1"])]);
                _ -> file_write_ct_export(1, File, CodeLength)
            end
    end,

    io:format(File, "~s~n", ["])."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-include_lib(\"common_test/include/ct.hrl\")."]),
    io:format(File, "~s~n", ["-include_lib(\"eunit/include/eunit.hrl\")."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - SUITE"]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["suite() ->"]),
    io:format(File, "~s~n", ["    ["]),
    io:format(File, "~s~n", [lists:append(["        {timetrap, {minutes, ", integer_to_list(?TIMETRAP_MINUTES), "}}"])]),
    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["init_per_suite(Config) ->"]),
    io:format(File, "~s~n", ["    Config."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["end_per_suite(_Config) ->"]),
    io:format(File, "~s~n", ["    ok."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - ALL"]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["all() ->"]),
    io:format(File, "~s~n", ["    ["]),

    case CodeLength of
        0 -> ok;
        _ -> case CompactedDetailed of
                 "compacted" ->
                     io:format(File, "~s~n", [lists:append(["        test_compacted"])]);
                 _ -> file_write_ct_all(1, File, CodeLength)
             end
    end,

    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% TEST CASES"]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),

    case CodeLength of
        0 -> ok;
        _ -> case CompactedDetailed of
                 "compacted" ->
                     io:format(File, "~s~n", [lists:append(["test_compacted(_Config) ->"])]);
                 _ -> ok
             end,
            file_write_ct(1, Type, CompleteSemicolon, CompactedDetailed, File, Code)
    end.

file_write_ct(_Current, _Type, _CompleteSQL, CompactedDetailed, File, []) ->
    case CompactedDetailed of
        "compacted" -> io:format(File, "~s~n", ["    ok."]);
        _ -> ok
    end,
    file:close(File);
file_write_ct(Current, Type, CompleteSemicolon, CompactedDetailed, File, [H | T]) ->
    case CompactedDetailed of
        "compacted" -> io:format(File, "~s~n", [lists:append([
            "    ",
            case Type of
                "performance" -> "{ok, _} = sqlparse:parsetree_with_tokens";
                _ -> "sqlparse_test:common_test_source"
            end,
            "(\"",
            case CompleteSemicolon of
                "semicolon" -> H ++ ";";
                _ -> H
            end,
            "\"),"
        ])]);
        _ ->
            io:format(File, "~s~n", [lists:append(["test_", integer_to_list(Current), "(_Config) ->"])]),
            io:format(File, "~s~n", [lists:append([
                "    ",
                case Type of
                    "performance" -> "{ok, _} = sqlparse:parsetree_with_tokens";
                    _ -> "sqlparse_test:common_test_source"
                end,
                "(\"",
                case CompleteSemicolon of
                    "semicolon" -> H ++ ";";
                    _ -> H
                end,
                "\")."
            ])]),
            io:format(File, "~s~n", [""])
    end,
    file_write_ct(Current + 1, Type, CompleteSemicolon, CompactedDetailed, File, T).

file_write_ct_all(Current, File, Target)
    when Current == Target ->
    io:format(File, "~s~n", [lists:append(["        test_", integer_to_list(Current)])]);
file_write_ct_all(Current, File, Target) ->
    io:format(File, "~s~n", [lists:append(["        test_", integer_to_list(Current), ","])]),
    file_write_ct_all(Current + 1, File, Target).

file_write_ct_export(Current, File, Target)
    when Current == Target ->
    io:format(File, "~s~n", [lists:append(["    test_", integer_to_list(Current), "/1"])]);
file_write_ct_export(Current, File, Target) ->
    io:format(File, "~s~n", [lists:append(["    test_", integer_to_list(Current), "/1,"])]),
    file_write_ct_export(Current + 1, File, Target).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating EUnit data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_eunit_all(_Type, _CompleteSQL, []) ->
    ok;
file_create_eunit_all(Type, CompleteSemicolon, [Rule | Rules]) ->
    file_create_eunit(Type, CompleteSemicolon, Rule),
    file_create_eunit_all(Type, CompleteSemicolon, Rules).

file_create_eunit(Type, CompleteSemicolon, Rule) ->
    [{Rule, Code}] = dets:lookup(?CODE_TEMPLATES, Rule),

    RuleStrimg = atom_to_list(Rule),

    FileName = lists:append([Type, "_", CompleteSemicolon, " ", RuleStrimg, ".tst"]),
    {ok, File, _} = file:path_open([?PATH_EUNIT], FileName, [write]),

    erlang:display(io:format("final eunit  tests ===> ~12.. B file_name: ~s ", [length(Code), FileName])),

    io:format(File, "~s~n", ["%%-*- mode: erlang -*-"]),
    io:format(File, "~s~n", ["%%-*- coding: utf-8 -*-"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["% Manual testing."]),
    io:format(File, "~s~n", ["[{tests, []}]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", ["%% Tests for rule: " ++ RuleStrimg]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", [""]),

    file_write_eunit(CompleteSemicolon, File, Code).

file_write_eunit(_CompleteSQL, File, []) ->
    file:close(File);
file_write_eunit(CompleteSemicolon, File, [H | T]) ->
    io:format(File, "~s~n", ["\"" ++
        case CompleteSemicolon of
            "semicolon" ->
                H ++ ";";
            _ ->
                H
        end ++
        "\"."]),
    file_write_eunit(CompleteSemicolon, File, T).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Store generated code in helper table.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_code(Rule, Code, Max, Strict) ->
%   erlang:display(io:format("store Code         ===> ~12.. B rule: ~s ", [length(Code), atom_to_list(Rule)])),

    case Max == 0 of
        true ->
%           erlang:display(io:format("store CodeNew      ===> ~12.. B rule: ~s ", [0, atom_to_list(Rule)])),
            ?debugFmt("~ncode lines         ===> ~12.. B rule: ~s ~n", [0, atom_to_list(Rule)]);
        _ ->
            CodeUnique = ordsets:to_list(ordsets:from_list(Code)),
            CodeUnique_Length = length(CodeUnique),
            CodeUniqueSorted = lists:sort(?F_RANDOM, CodeUnique),
            CodeUniqueLimited = case CodeUnique_Length > Max of
                                    true ->
                                        lists:sublist(CodeUniqueSorted, 1, Max);
                                    _ -> CodeUnique
                                end,
            CodeTotal = case dets:lookup(?CODE_TEMPLATES, Rule) of
                            [{Rule, CodeOld}] ->
                                lists:sort(?F_RANDOM, ordsets:to_list(ordsets:from_list(lists:append([CodeOld, CodeUniqueLimited]))));
                            _ -> CodeUniqueLimited
                        end,
            CodeTotal_Length = length(CodeTotal),
            CodeNew = case Strict andalso CodeTotal_Length > Max of
                          true ->
                              [lists:nth(rand:uniform(CodeTotal_Length), CodeTotal) || _ <- lists:seq(1, Max)];
                          _ -> CodeTotal
                      end,
            dets:insert(?CODE_TEMPLATES, {Rule, CodeNew}),
%           erlang:display(io:format("store CodeNew      ===> ~12.. B rule: ~s ", [length(CodeNew), atom_to_list(Rule)])),
            ?debugFmt("~ncode lines         ===> ~12.. B rule: ~s ~n", [length(CodeNew), atom_to_list(Rule)])
    end.
