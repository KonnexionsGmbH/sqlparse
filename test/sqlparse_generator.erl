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

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-include("sqlparse_generator.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Test Data.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    ets:new(?CODE_TEMPLATES, [
        named_table,
        private,
        ordered_set
    ]),

    create_code(),

    %% Common tests ............................................................

    case ?GENERATE_CT of
        true ->
            case ?GENERATE_PERFORMANCE of
                true ->
                    ok = file_create_ct_all("performance", "complete_", "compacted", ?ALL_CLAUSE_PERFORMANCE);
                _ -> ok
            end,
            case ?GENERATE_RELIABILITY of
                true ->
                    case ?GENERATE_COMPACTED of
                        true ->
                            ok = file_create_ct_all("reliability", "complete_", "compacted", ?ALL_CLAUSE_RELIABILITY),
                            ok = file_create_ct_all("reliability", "semicolon", "compacted", ?ALL_CLAUSE_RELIABILITY_SQL);
                        _ ->
                            ok = file_create_ct_all("reliability", "complete_", "detailed_", ?ALL_CLAUSE_RELIABILITY),
                            ok = file_create_ct_all("reliability", "semicolon", "detailed_", ?ALL_CLAUSE_RELIABILITY_SQL)
                    end;
                _ -> ok
            end;
        _ -> ok
    end,

    %% EUnit tests .............................................................

    case ?GENERATE_EUNIT of
        true ->
            case ?GENERATE_RELIABILITY of
                true ->
                    ok = file_create_eunit_all("reliability", "complete_", ?ALL_CLAUSE_RELIABILITY),
                    ok = file_create_eunit_all("reliability", "semicolon", ?ALL_CLAUSE_RELIABILITY_SQL),
                    ok = file_create_eunit_all("reliability", "complete_", ?ALL_CLAUSE_RELIABILITY_SQL_DETAILED);
                _ -> ok
            end;
        _ -> ok
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code base.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code() ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 01
%% -----------------------------------------------------------------------------
%%
%% APPROXNUM ::= ( ( '.' [0-9]+ ) | ( [0-9]+ '.'? [0-9]* ) ) ( [eE] [+-]? [0-9]+ )? [fFdD]?
%%
%% ==> atom                                == atom = ... literal ...
%% ==> fun_arg                             == fun_arg = ... atom ...
%% ==> literal                             == literal = ... APPROXNUM ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... atom ...
%%
%% == atom ::= parameter_ref
%% ==        | literal
%%        | 'USER'
%%
%% ==> fun_arg                             == fun_arg = ... atom ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... atom ...
%%
%% commit_statement ::= 'COMMIT' ( 'WORK' )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... commit_statement ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% COMPARISON ::= '!=' | '^=' | '<>' | '<' | '>' | '<=' | '>='
%%
%% {"^(?i)(AVG)$",             'FUNS'},
%% {"^(?i)(CORR)$",            'FUNS'},
%% {"^(?i)(COUNT)$",           'FUNS'},
%% ...
%%
%% grantee_revokee ::= 'PUBLIC'
%%                   | NAME
%%
%% HINT ::= '/*' [^\*\/]* '*/'
%%
%% INTNUM ::= ([0-9]+)
%%
%% ==> atom                                == atom = ... literal ...
%% ==> fun_arg                             == fun_arg = ... atom ...
%% ==> literal                             == literal = ... INTNUM ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... atom ...
%%
%% JSON ::= ([|] [:{\[#] [^|]+ [|])
%%
%% ==> column_ref                          == column_ref = ... JSON ...
%% ==> fun_arg                             == fun_arg = ... column_ref ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... column_ref ...
%%
%% NAME ::= [A-Za-z][A-Za-z0-9_@\$~]*
%%
%% ==> column                              == column = ... NAME ...
%% ==> column_ref                          == column_ref = ... NAME ...
%% ==> from_column                         == from_column = ... table_ref ...
%% ==> grantee_revokee                     == grantee_revokee = NAME ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... column_ref ...
%% ==> system_privilege                    == system_privilege = ... NAME
%% ==> table                               == table = ... NAME ...
%% ==> table_ref                           == table_ref = ... table ...
%% ==> target                              == target = ... NAME ...
%% ==> tbl_type                            == tbl_type = ... NAME ...
%%
%% object_privilege ::= 'ALL'
%%                    | 'ALTER'
%%                    | 'DELETE'
%%                    | 'EXECUTE'
%%                    | 'INDEX'
%%                    | 'INSERT'
%%                    | 'REFERENCES'
%%                    | 'SELECT'
%%                    | 'UPDATE'
%%
%% object_with_grant_option ::= 'WITH' ( 'GRANT' | 'HIERARCHY' ) 'OPTION'
%%
%% object_with_grant_option ::= ( 'CASCADE' 'CONSTRAINTS' ) | 'FORCE'

%% outer_join_type ::= ( 'FULL' ( 'OUTER' )? )
%%                   | ( 'LEFT' ( 'OUTER' )? )
%%                   | ( 'RIGHT' ( 'OUTER' )? )
%%
%% PARAMETER ::= ':' [A-Za-z0-9_\.]+
%%
%% ==> from_column                         == from_column = ... table_ref ...
%% ==> parameter                           == parameter = PARAMETER
%% ==> table                               == table = parameter
%% ==> table_ref                           == table_ref = ... table ...
%%
%% rollback_statement ::= 'ROLLBACK' ( 'WORK' )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... rollback_statement ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% STRING ::= ( 'fun' [A-Za-z0-9,_]* [.]* '->' [.]* 'end.' )
%%          | ( 'fun\s' ['A-Za-z0-9_]+ ':' ['A-Za-z0-9_]+ '/' [0-9]+ '.' )
%%          | ( "'" [^\']* "''**" )
%%
%% ==> atom                                == atom = ... literal ...
%% ==> column                              == column = ... STRING ...
%% ==> data_type                           == data_type = ... STRING ...
%% ==> from_column                         == from_column = ... table_ref ...
%% ==> fun_arg                             == fun_arg = ... atom ...
%% ==> literal                             == literal = ... STRING ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... atom ...
%% ==> table                               == table = ... STRING ...
%% ==> table_ref                           == table_ref = ... table ...
%%
%% system_privilege ::=  'ADMIN'
%%                    |  'ALL' 'PRIVILEGES'
%%                    | ( ( 'ALTER' | 'CREATE' | 'DROP' ) 'ANY' ( 'INDEX' | ( 'MATERIALIZED' 'VIEW' ) | 'TABLE' | 'VIEW' ) )
%%                    | ( 'CREATE' ( ( 'MATERIALIZED' 'VIEW' ) | 'TABLE' | 'VIEW' ) )
%%                    | ( ( 'DELETE' | 'INSERT' | 'SELECT' | 'UPDATE' ) 'ANY' 'TABLE' )
%%                    | NAME
%%
%% system_with_grant_option ::= 'WITH' ( 'ADMIN' | 'DELEGATE' ) 'OPTION'
%%
%% tbl_scope ::= 'LOCAL'
%%             | 'CLUSTER'
%%             | 'SCHEMA'
%%
%% tbl_type ::= 'SET'
%%            | 'ORDERED_SET'
%%            | 'BAG'
%% ==            | NAME
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 01   <===================~n", [])),

    create_code(approxnum),
    create_code(atom),
    create_code(commit_statement),
    create_code(comparison),
    create_code(funs),
    create_code(grantee_revokee),
    create_code(hint),
    create_code(intnum),
    create_code(json),
    create_code(name),
    create_code(object_privilege),
    create_code(object_with_grant_option),
    create_code(object_with_revoke_option),
    create_code(outer_join_type),
    create_code(parameter),
    create_code(referenceExamples),
    create_code(rollback_statement),
    create_code(string),
    create_code(system_privilege),
    create_code(system_with_grant_option),
    create_code(tbl_scope),
    create_code(tbl_type),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 02
%% -----------------------------------------------------------------------------
%%
%% column_commalist ::= column ( ',' column )*
%%
%% column_ref ::= ( ( ( ( NAME '.' )? NAME '.' )? NAME )? JSON )
%%              | ( ( ( NAME '.' )? NAME '.' )? NAME )
%%              | ( ( ( NAME '.' )? NAME '.' )? NAME '(' '+' ')' )
%%              | ( ( NAME '.' )? NAME '.' '*' )
%%
%% ==> fun_arg                             == fun_arg = ... column_ref ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... column_ref ...
%%
%% create_role_def ::= 'CREATE' 'ROLE' NAME
%%
%% ==> manipulative_statement              == manipulative_statement = ... create_role_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% cursor ::= NAME
%%
%% drop_role_def ::= 'DROP' 'ROLE' NAME
%%
%% ==> manipulative_statement              == manipulative_statement = ... drop_role_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% drop_user_def ::= 'DROP' 'USER' NAME ( 'CASCADE' )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... drop_user_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% extra ::= NAME  ';'
%%
%% grantee_identified_by ::= NAME 'IDENTIFIED' 'BY' STRING
%%
%% grantee_revokee_commalist ::= grantee_revokee ( ',' grantee_revokee )*
%%
%% identified ::= IDENTIFIED ( ( 'BY' NAME ) | ( EXTERNALLY ( 'AS' NAME ) ) | ( 'GLOBALLY' ( 'AS' NAME )? ) )
%%
%% ==> spec_item                           == spec_item = ... identified ...
%%
%% index_name ::= ( NAME '.' )? NAME
%%
%% object_privilege_list ::= object_privilege ( ',' object_privilege )*
%%
%% quota ::= ( 'QUOTA' 'UNLIMITED' 'ON' NAME )
%%         | ( 'QUOTA' INTNUM ( NAME )? 'ON' NAME )
%%
%% role_list ::= NAME ( ',' NAME )*
%%
%% ==> proxy_with                          == proxy_with = ... 'WITH' 'ROLE' role_list ...
%% ==> proxy_with                          == proxy_with = ... 'WITH' 'ROLE' 'ALL' 'EXCEPT' role_list ...
%%
%% sgn_num ::= ( '-' )? INTNUM
%%
%% system_privilege_list ::= system_privilege ( ',' system_privilege )*
%%
%% table_name ::= ( ( NAME '.' )? NAME '.' )? NAME
%%
%% when_action ::= ( 'GOTO' NAME )
%%               | 'CONTINUE'
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 02   <===================~n", [])),

    create_code(column_commalist),
    create_code(column_ref),
    create_code(create_role_def),
    create_code(cursor),
    create_code(drop_role_def),
    create_code(drop_user_def),
    create_code(extra),
    create_code(grantee_identified_by),
    create_code(grantee_revokee_commalist),
    create_code(identified),
    create_code(index_name),
    create_code(object_privilege_list),
    create_code(quota),
    create_code(role_list),
    create_code(sgn_num),
    create_code(system_privilege_list),
    create_code(table_name),
    create_code(when_action),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 03
%% -----------------------------------------------------------------------------
%%
%% close_statement ::= 'CLOSE' cursor
%%
%% ==> manipulative_statement              == manipulative_statement = ... close_statement ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% data_type ::= STRING
%%              | ( NAME ( '(' sgn_num ')' )? )
%%              | ( NAME '(' sgn_num ',' sgn_num ')' )
%%
%% open_statement ::= 'OPEN' cursor
%%
%% ==> manipulative_statement              == manipulative_statement = ... open_statement ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% parameter_ref ::= parameter ( ( 'INDICATOR' )? parameter )?
%%
%% ==> atom                                == atom = ... parameter_ref ...
%% ==> fun_arg                             == fun_arg = ... atom ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... atom ...
%% ==> target                              == target = ... parameter_ref ...
%%
%% table ::= ( ( NAME '.' )? NAME ( NAME )? )
%%         | STRING
%%         | ( parameter ( NAME )? )
%%
%% ==> from_column                         == from_column = ... table_ref ...
%% ==> join_ref                            == join_ref = ... table ...
%% ==> table_ref                           == table_ref = ... table ...
%%
%% truncate_table ::= 'TRUNCATE' 'TABLE' table_name ( ( 'PRESERVE' | 'PURGE' ) 'MATERIALIZED' 'VIEW' 'LOG' )? ( ( 'DROP' | 'REUSE' ) 'STORAGE' )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... truncate_table ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% user_opt ::= ( ( 'DEFAULT' | 'TEMPORARY' ) 'TABLESPACE' NAME )
%%            | ( quota  ( quota )* )
%%            | ( 'PROFILE' NAME )
%%
%% ==> spec_item                           == spec_item = ... user_opt ...
%%
%% user_role ::= 'DEFAULT' 'ROLE' ( ( 'ALL' ( 'EXCEPT' role_list )? ) | NONE | role_list )
%%
%% ==> spec_item                           == spec_item = ... user_role ...
%%
%% == sql ::= procedure_call
%% ==       | schema
%% ==       | cursor_def
%% ==       | manipulative_statement
%%       | 'WHENEVER' 'NOT' 'FOUND' when_action
%%       | 'WHENEVER' 'SQLERROR' when_action
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 03   <===================~n", [])),

    create_code(close_statement),
    create_code(data_type),
    create_code(open_statement),
    create_code(parameter_ref),
    create_code(table),
    create_code(truncate_table),
    create_code(user_opt),
    create_code(user_role),
    create_code(whenever),                                                %% sql

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 04
%% -----------------------------------------------------------------------------
%%
%% create_index_def ::= 'CREATE' ( 'BITMAP' | 'KEYLIST' | 'HASHMAP' | 'UNIQUE' )? 'INDEX' ( index_name )?
%%                      'ON' table ( '(' ( NAME  JSON? ) ( ',' NAME JSON? )* ')' )?
%%                      ( 'NORM_WITH' STRING )?  ( 'FILTER_WITH' STRING )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... create_index_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% create_user_def ::= 'CREATE' 'USER' NAME identified ( user_opt )*
%%
%% ==> manipulative_statement              == manipulative_statement = ... create_user_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% drop_index_def ::= 'DROP' 'INDEX' ( index_name )? 'FROM' table
%%
%% ==> manipulative_statement              == manipulative_statement = ... drop_index_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% drop_table_def ::= 'DROP' ( NAME )? 'TABLE' ( 'IF' 'EXISTS' )? ( table ( ',' table )* ) ( 'RESTRICT' | 'CASCADE' )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... drop_table_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% on_obj_clause ::= ( 'ON' table )
%%                 | ( 'ON' 'DIRECTORY' NAME )
%%
%% proxy_with ::= ( 'WITH' 'NO' 'ROLES' )
%%              | ( 'WITH' 'ROLE' role_list )
%%              | ( 'WITH' 'ROLE' 'ALL' 'EXCEPT' role_list )
%%
%% ==> db_user_proxy                       == db_user_proxy = ... proxy_with ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 04   <===================~n", [])),

    create_code(create_index_def),
    create_code(create_user_def),
    create_code(drop_index_def),
    create_code(drop_table_def),
    create_code(on_obj_clause),
    create_code(proxy_with),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 05
%% -----------------------------------------------------------------------------
%%
%% db_user_proxy ::= proxy_with
%%                 | ( ( proxy_with )? 'AUTHENTICATION' 'REQUIRED' )
%%
%% grant_def ::= 'GRANT' (
%%                         ( ( ( 'ALL' 'PRIVILEGES' ) | ( object_privilege (',' object_privilege )* ) ) on_obj_clause 'TO' ( grantee_identified_by | ( grantee_revokee ( ',' grantee_revokee )* ) ) ( 'WITH' ( 'GRANT' | 'HIERARCHY' ) 'OPTION' )? )
%%                       | ( ( ( 'ALL' 'PRIVILEGES' ) | ( system_privilege (',' system_privilege )* ) )               'TO' ( grantee_identified_by | ( grantee_revokee ( ',' grantee_revokee )* ) ) ( 'WITH' ( 'ADMIN' | 'DELEGATE'  ) 'OPTION' )? )
%%                       )
%%
%% ==> manipulative_statement              == manipulative_statement = ... grant_def ...
%% ==> schema_element                      == schema_element = ... grant_def
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% revoke_def ::= 'REVOKE' (
%%                           ( ( ( 'ALL' 'PRIVILEGES' ) | ( object_privilege (',' object_privilege )* ) ) on_obj_clause 'FROM' grantee_revokee ( ',' grantee_revokee )* ( ( 'CASCADE' 'CONSTRAINTS' ) | 'FORCE' )? )
%%                         | ( ( ( 'ALL' 'PRIVILEGES' ) | ( system_privilege (',' system_privilege )* ) )               'FROM' grantee_revokee ( ',' grantee_revokee )* )
%%                         )
%%
%% ==> manipulative_statement              == manipulative_statement = ... revoke_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% target_commalist ::= target ( ',' target )*
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 05   <===================~n", [])),

    create_code(db_user_proxy),
    create_code(grant_def),
    create_code(revoke_def),
    create_code(target_commalist),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 06
%% -----------------------------------------------------------------------------
%%
%% fetch_statement ::= 'FETCH' cursor 'INTO' target_commalist
%%
%% ==> manipulative_statement              == manipulative_statement = ... fetch_statement ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% proxy_clause ::= ( 'GRANT' | 'REVOKE' ) 'CONNECT' 'THROUGH' ( ( 'ENTERPRISE' 'USERS' ) | db_user_proxy )
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 06   <===================~n", [])),

    create_code(fetch_statement),
    create_code(proxy_clause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 07
%% -----------------------------------------------------------------------------
%%
%% alter_user_def ::= ( 'ALTER' 'USER' NAME ( ',' NAME )* proxy_clause )
%%                  | ( 'ALTER' 'USER' NAME spec_item ( spec_item )* )
%%                  | ( 'ALTER' 'USER' NAME NAME NAME )
%%
%% ==> manipulative_statement              == manipulative_statement = ... alter_user_def ...
%% ==> sql                                 == sql = ... manipulative_statement ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 07   <===================~n", [])),

    create_code(alter_user_def),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 08 = 11-22/1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 08   <===================~n", [])),

    create_code_layer("1"),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 09 = 11-22/2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 09   <===================~n", [])),

    create_code_layer("2"),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 99
%% -----------------------------------------------------------------------------
%%
%% base_table_element ::= column_def
%%                      | table_constraint_def
%%
%% column ::= NAME
%%          | STRING
%%
%% from_column ::= table_ref
%%                  | ( '(' join_clause ')' )
%%                  | join_clause
%%
%% join ::= inner_cross_join
%%        | outer_join
%%
%% literal ::= STRING
%%           | INTNUM
%%           | APPROXNUM
%%
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
%%
%% on_obj_clause ::= ( 'ON' table )
%%                 | ( 'ON' 'DIRECTORY' NAME )
%%
%% parameter ::= PARAMETER
%%
%% predicate ::= comparison_predicate
%%             | between_predicate
%%             | like_predicate
%%             | test_for_null
%%             | in_predicate
%%             | all_or_any_predicate
%%             | existence_test
%%
%% schema_element ::= create_table_def
%%                  | view_def
%%                  | grant_def
%%
%% select_statement ::= query_exp
%%
%% selection ::= select_field_commalist
%%
%% spec_item ::= identified
%%             | user_opt
%%             | user_role
%%
%% subquery ::= query_exp
%%
%% target ::= NAME
%%          | parameter_ref
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n======================================================> create_code: Level 99   <===================~n", [])),

    create_code(special),

    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code layered.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_layer(_Version) ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 11
%% -----------------------------------------------------------------------------
%%
%% fun_args ::= fun_arg ( ',' fun_arg )*
%%
%% scalar_exp ::= scalar_sub_exp ( '||' scalar_exp )?
%%
%% schema ::= 'CREATE' 'SCHEMA' 'AUTHORIZATION' NAME ( schema_element ( schema_element )* )?
%%
%% ==> sql                                 == sql = ... schema ...
%%
%% sql_list ::= sql ';' ( extra )? ( sql ';' ( extra )? )*
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 11/~s <===================~n", [_Version])),

    create_code(fun_args),
    create_code(scalar_exp),
    create_code(schema),
    create_code(sql_list),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 12
%% -----------------------------------------------------------------------------
%%
%% between_predicate ::= scalar_exp ( 'NOT' )? 'BETWEEN' scalar_exp 'AND' scalar_exp
%%
%% ==> predicate                           == predicate = ... between_predicate ...
%% ==> search_condition                    == search_condition = ... predicate ...
%%
%% function_ref ::= ( ( ( NAME '.' )?  NAME '.' )? NAME '(' fun_args ')' )
%%                | ( 'FUNS' ( '(' ( fun_args | '*' | ( 'DISTINCT' column_ref ) | ( 'ALL' scalar_exp ) ) ')' )? )
%%
%% ==> fun_arg                             == fun_arg = ... function_ref ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... function_ref ...
%%
%% like_predicate ::= scalar_exp ( 'NOT' )? 'LIKE' scalar_exp ( 'ESCAPE' atom )?
%%
%% ==> predicate                           == predicate = ... like_predicate ...
%% ==> search_condition                    == search_condition = ... predicate ...
%%
%% ordering_spec ::= scalar_exp ( 'ASC' | 'DESC' )?
%%
%% scalar_opt_as_exp ::= ( scalar_exp ( ( '=' | COMPARISON ) scalar_exp )? )
%%                     | ( scalar_exp ( AS )? NAME )
%%
%% ==> select_field                        == select_field = ... comparison_predicate ...
%% ==> selection                           == selection = select_field_commalist
%%
%% test_for_null ::= scalar_exp 'IS' ( 'NOT' )? 'NULL'
%%
%% ==> predicate                           == predicate = ... test_for_null ...
%% ==> search_condition                    == search_condition = ... predicate ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 12/~s <===================~n", [_Version])),

    create_code(between_predicate),
    create_code(function_ref),
    create_code(like_predicate),
    create_code(ordering_spec),
    create_code(scalar_opt_as_exp),
    create_code(test_for_null),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 13
%% -----------------------------------------------------------------------------
%%
%% assignment ::= column '=' scalar_opt_as_exp
%%
%% case_when_then ::= 'WHEN' search_condition 'THEN' scalar_opt_as_exp
%%
%% column_ref_commalist ::= ( column_ref | function_ref ) ( ',' ( column_ref | function_ref ) )*
%%
%% comparison_predicate ::= scalar_opt_as_exp
%%                        | (         scalar_exp ( '=' | COMPARISON ) 'PRIOR' scalar_exp )
%%                        | ( 'PRIOR' scalar_exp ( '=' | COMPARISON )         scalar_exp )
%%
%% ==> predicate                           == predicate = ... comparison_predicate ...
%% ==> search_condition                    == search_condition = ... predicate ...
%%
%% function_ref_list ::= ( function_ref ';' )
%%                     | ( function_ref ';' function_ref_list )
%%
%% order_by_clause ::= 'ORDER' 'BY' ordering_spec ( ',' ordering_spec )*
%%
%% scalar_exp_commalist ::= scalar_opt_as_exp ( ',' scalar_opt_as_exp )*
%%
%% table_constraint_def ::= ( 'UNIQUE'        '(' column_commalist ')' )
%%                        | ( 'PRIMARY' 'KEY' '(' column_commalist ')' )
%%                        | ( 'FOREIGN' 'KEY' '(' column_commalist ')' 'REFERENCES' table ( '(' column_commalist ')' )? )
%%                        | ( 'CHECK' '(' search_condition ')' )
%%
%% ==> base_table_element                  == base_table_element = ... table_constraint_def ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 13/~s <===================~n", [_Version])),

    create_code(assignment),
    create_code(case_when_then),
    create_code(column_ref_commalist),
    create_code(comparison_predicate),
    create_code(function_ref_list),
    create_code(order_by_clause),
    create_code(scalar_exp_commalist),
    create_code(table_constraint_def),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%% -----------------------------------------------------------------------------
%%
%% assignment_commalist ::= assignment ( ',' assignment )*
%%
%% case_when_then_list ::= case_when_then ( case_when_then )*
%%
%% create_table_def ::= 'CREATE' ( tbl_scope )? ( tbl_type )? 'TABLE' table '(' ( base_table_element ( ',' base_table_element )* )? ')'
%%
%% ==> manipulative_statement              == manipulative_statement = ... create_table_def ...
%% ==> schema_element                      == schema_element = ... create_table_def ...
%%
%% procedure_call ::= ( 'DECLARE' 'BEGIN' function_ref_list 'END' )
%%                  | ( 'DECLARE' 'BEGIN' sql_list          'END' )
%%                  | (           'BEGIN' function_ref_list 'END' )
%%                  | (           'BEGIN' sql_list          'END' )
%%                  | ( 'CALL' function_ref )
%%
%% ==> sql                                 == sql = ... procedure_call ...
%%
%% query_partition_clause ::= 'PARTITION' 'BY' ( ( '(' scalar_exp_commalist ')' ) | scalar_exp_commalist )
%%
%% search_condition ::= ( search_condition ( 'AND' | 'OR' ) search_condition )
%%                    | ( 'NOT' search_condition )
%%                    | ( '(' search_condition ')' )
%%                    | predicate
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 14/~s <===================~n", [_Version])),

    create_code(assignment_commalist),
    create_code(case_when_then_list),
    create_code(create_table_def),
    create_code(procedure_call),
    create_code(query_partition_clause),
    create_code(search_condition),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 15
%% -----------------------------------------------------------------------------
%%
%% case_when_exp ::= ( '(' case_when_exp ')' )
%%                 | ( 'CASE' ( scalar_opt_as_exp )? case_when_then_list ( 'ELSE' scalar_opt_as_exp )? 'END' )
%%
%% column_def_opt ::= ( 'NOT' 'NULL' ( 'UNIQUE' | 'PRIMARY' 'KEY' )? )
%%                  | ( 'DEFAULT' ( function_ref | literal | NAME | 'NULL' | 'USER' ) )
%%                  | ( 'CHECK' '(' search_condition ')' )
%%                  | ( 'REFERENCES' table ( '(' column_commalist ')' )? )
%%
%% hierarchical_query_clause ::= ( 'START' 'WITH' search_condition 'CONNECT' 'BY' ( 'NOCYCLE' )? search_condition )
%%                             | ( 'CONNECT' 'BY' ( 'NOCYCLE' )? search_condition 'START' 'WITH' search_condition )
%%
%% returning ::= ( 'RETURNING' | 'RETURN' ) selection 'INTO' selection
%%
%% where_clause ::= 'WHERE' search_condition
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 15/~s <===================~n", [_Version])),

    create_code(case_when_exp),
    create_code(column_def_opt),
    create_code(hierarchical_query_clause),
    create_code(returning),
    create_code(where_clause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 16
%% -----------------------------------------------------------------------------
%%
%% column_def ::= column data_type ( column_def_opt )*
%%
%% ==> base_table_element                  == base_table_element = ... column_def ...
%%
%% delete_statement_positioned ::= 'DELETE' 'FROM' table 'WHERE' 'CURRENT' 'OF' cursor ( returning )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... delete_statement ...
%%
%% delete_statement_searched ::= 'DELETE' 'FROM' table ( where_clause )? ( returning )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... delete_statement ...
%%
%% select_field ::= ( case_when_exp ( ( 'AS' )? NAME )? )
%%                | scalar_opt_as_exp
%%                | '*'
%%
%% ==> select_field_commalist              == select_field_commalist = select_field ...
%% ==> selection                           == selection = select_field_commalist
%%
%% update_statement_positioned ::= 'UPDATE' table 'SET' assignment_commalist 'WHERE' 'CURRENT' 'OF' cursor ( returning )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... update_statement ...
%%
%% update_statement_searched ::= 'UPDATE' table 'SET' assignment_commalist ( where_clause )? ( returning )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... update_statement ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 16/~s <===================~n", [_Version])),

    create_code(column_def),
    create_code(delete_statement),
    create_code(select_field),
    create_code(update_statement),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 17
%% -----------------------------------------------------------------------------
%%
%% select_field_commalist ::= select_field ( ',' select_field )*
%%
%% ==> select                              == select = ... select_field_commalist ...
%% ==> selection                           == selection = select_field_commalist
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 17/~s <===================~n", [_Version])),

    create_code(select_field_commalist),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 18
%% -----------------------------------------------------------------------------
%%
%% join_on_or_using_clause ::= ( 'ON' search_condition )
%%                           | ( 'USING' '(' select_field_commalist ')' )
%%
%% table_exp ::= 'FROM' from_column ( from_column )* ( where_clause )? ( hierarchical_query_clause )? ( 'GROUP' 'BY' column_ref_commalist )? ( 'HAVING' search_condition )? ( order_by_clause )?
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 18/~s <===================~n", [_Version])),

    create_code(join_on_or_using_clause),
    create_code(table_exp),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 19
%% -----------------------------------------------------------------------------
%%
%% inner_cross_join ::= ( 'INNER' )? 'JOIN' join_ref join_on_or_using_clause
%%                    | ( 'CROSS' | ( 'NATURAL' ( 'INNER' )? ) ) 'JOIN' join_ref
%%
%% ==> join                                == join = ... inner_cross_join ...
%%
%% outer_join ::= ( NATURAL | query_partition_clause | (query_partition_clause NATURAL) )? outer_join_type JOIN join_ref ( query_partition_clause )? ( join_on_or_using_clause )?
%%
%% ==> join                                == join = ... outer_join ...
%%
%% query_spec ::= 'SELECT' ( HINT )? ( 'ALL' | 'DISTINCT' )? selection ( 'INTO' target_commalist ( 'IN' NAME )? )? table_exp
%%
%% ==> fun_arg                             == fun_arg = ... subquery ...
%% ==> query_exp                           == query_exp = ... query_term ...
%% ==> query_term                          == query_term = ... query_spec ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... subquery ...
%% ==> select_statement                    == select_statement = ... query_term ...
%% ==> subquery                            == subquery = ... query_term ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 19/~s <===================~n", [_Version])),

    create_code(inner_cross_join),
    create_code(outer_join),
    create_code(query_spec),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 20
%% -----------------------------------------------------------------------------
%%
%% insert_statement ::= 'INSERT' 'INTO' table ( ( '(' column_commalist ')' )? ( ( 'VALUES' '(' scalar_opt_as_exp ( ',' scalar_opt_as_exp )* ')' ) | query_spec ) ( returning )? )?
%%
%% ==> manipulative_statement              == manipulative_statement = ... insert_statement ...
%%
%% join_clause ::= table_ref join ( join )*
%%
%% ==> from_column                         == from_column = ... join_clause ...
%% ==> from_column                         == from_column = ... '(' join_clause ')' ...
%%
%% query_term ::= query_spec
%%              | ( '(' query_exp ')' )
%%
%% ==> fun_arg                             == fun_arg = ... subquery ...
%% ==> query_exp                           == query_exp = ... query_term ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... subquery ...
%% ==> select_statement                    == select_statement = ... query_term ...
%% ==> subquery                            == subquery = ... query_term ...
%%
%% view_def ::= ( 'CREATE' 'VIEW' table ( '(' column_commalist ')' )? )
%%            | ( 'AS' query_spec ( 'WITH' 'CHECK' 'OPTION' )? )
%%
%% ==> manipulative_statement              == manipulative_statement = ... view_def ...
%% ==> schema_element                      == schema_element = ... view_def ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 20/~s <===================~n", [_Version])),

    create_code(insert_statement),
    create_code(join_clause),
%% wwe: currently not supported
%%    create_code(query_term),
    create_code(view_def),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 21
%% -----------------------------------------------------------------------------
%%
%% query_exp ::= query_term
%%             | ( query_exp ( ( 'UNION' ( 'ALL' )? ) | 'INTERSECT' | 'MINUS' ) query_term )
%%
%% ==> fun_arg                             == fun_arg = ... subquery ...
%% ==> scalar_exp                          == scalar_exp = ... scalar_sub_exp ...
%% ==> scalar_sub_exp                      == scalar_sub_exp = ... subquery ...
%% ==> select_statement                    == select_statement = ... query_exp ...
%% ==> subquery                            == subquery = ... query_exp ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 21/~s <===================~n", [_Version])),

    create_code(query_exp),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 22
%% -----------------------------------------------------------------------------
%%
%% all_or_any_predicate ::= scalar_exp ( '=' | COMPARISON ) ( 'ANY' | 'ALL' | 'SOME' ) subquery
%%
%% ==> predicate                           == predicate = ... all_or_any_predicate ...
%% ==> search_condition                    == search_condition = ... predicate ...
%%
%% cursor_def ::= 'DECLARE' cursor 'CURSOR' 'FOR' query_exp ( order_by_clause )?
%%
%% ==> sql                                 == sql = ... cursor_def ...
%%
%% existence_test ::= 'EXISTS' subquery
%%
%% ==> predicate                           == predicate = ... existence_test ...
%% ==> search_condition                    == search_condition = ... predicate ...
%%
%% fun_arg ::= ( '(' fun_arg ')' )
%%           | function_ref
%%           | column_ref
%%           | ( fun_arg ( '+' | '-' | '*' | '/' | 'div' | '||' ) fun_arg )
%%           | ( ( '+' | '-' ) fun_arg )
%%           | 'NULL'
%%           | atom
%%           | subquery
%%           | ( fun_arg 'AS' NAME )
%%           | ( fun_arg ( '=' | COMPARISON ) fun_arg )
%%
%% in_predicate ::= ( scalar_exp ( 'NOT' )? 'IN' '(' subquery ')' )
%%                | ( scalar_exp ( 'NOT' )? 'IN' '(' scalar_exp_commalist ')' )
%%                | ( scalar_exp ( 'NOT' )? 'IN' scalar_exp )
%%
%% ==> predicate                           == predicate = ... in_predicate ...
%% ==> search_condition                    == search_condition = ... predicate ...
%%
%% join_ref ::= table
%%            | ( query_term ( NAME )? )
%%
%% scalar_sub_exp ::= ( scalar_sub_exp ( '+' | '-' | '*' | '/' | 'div' ) scalar_sub_exp )
%%                  | ( ( '+' | '-' ) scalar_sub_exp )
%%                  | 'NULL'
%%                  | atom
%%                  | subquery
%%                  | column_ref
%%                  | function_ref
%%                  | ( '(' scalar_sub_exp ')' )
%%
%% table_ref ::= table
%%             | ( query_term ( NAME )? )
%%
%% ==> from_column                         == from_column = ... table_ref ...
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format("~n~n================================================> create_code_layer: Level 22/~s <===================~n", [_Version])),

    create_code(all_or_any_predicate),
    create_code(cursor_def),
    create_code(existence_test),
    create_code(fun_arg),
    create_code(in_predicate),
    create_code(join_ref),
    create_code(scalar_sub_exp),
    create_code(table_ref),

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
    [{comparison, Comparison}] = ets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),
    [{subquery, Subquery}] = ets:lookup(?CODE_TEMPLATES, subquery),
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
    store_code(predicate, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(search_condition, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% alter_user_def ::= ( 'ALTER' 'USER' NAME ( ',' NAME )* proxy_clause )
%%                  | ( 'ALTER' 'USER' NAME spec_item ( spec_item )* )
%%                  | ( 'ALTER' 'USER' NAME NAME NAME )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(alter_user_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{proxy_clause, Proxy_Clause}] = ets:lookup(?CODE_TEMPLATES, proxy_clause),
    Proxy_Clause_Length = length(Proxy_Clause),
    [{spec_item, Spec_Item}] = ets:lookup(?CODE_TEMPLATES, spec_item),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
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
    store_code(atom, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(literal, Code, ?MAX_BASIC, false),
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% assignment ::= column '=' scalar_opt_as_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(assignment = Rule) ->
    ?CREATE_CODE_START,
    [{column, Column}] = ets:lookup(?CODE_TEMPLATES, column),
    Column_Length = length(Column),
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(Column_Length), Column),
                " = ",
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
    [{assignment, Assignment}] = ets:lookup(?CODE_TEMPLATES, assignment),
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

    Code =
        [
            "User"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% between_predicate ::= scalar_exp ( 'NOT' )? 'BETWEEN' scalar_exp 'AND' scalar_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(between_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
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
    store_code(predicate, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(search_condition, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case_when_exp ::= ( '(' case_when_exp ')' )
%%                 | ( 'CASE' ( scalar_opt_as_exp )? case_when_then_list ( 'ELSE' scalar_opt_as_exp )? 'END' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(case_when_exp = Rule) ->
    ?CREATE_CODE_START,
    [{case_when_then_list, Case_When_Then_List}] = ets:lookup(?CODE_TEMPLATES, case_when_then_list),
    Case_When_Then_List_Length = length(Case_When_Then_List),
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
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
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case_when_then ::= 'WHEN' search_condition 'THEN' scalar_opt_as_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(case_when_then = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
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
    [{case_when_then, Case_When_Then}] = ets:lookup(?CODE_TEMPLATES, case_when_then),
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
    [{cursor, Cursor}] = ets:lookup(?CODE_TEMPLATES, cursor),

    Code =
        [
                "Close " ++ C || C <- Cursor
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% column_commalist ::= column ( ',' column )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(column_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{column, Column}] = ets:lookup(?CODE_TEMPLATES, column),
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
    [{column, Column}] = ets:lookup(?CODE_TEMPLATES, column),
    Column_Length = length(Column),
    [{column_def_opt, Column_Def_Opt}] = ets:lookup(?CODE_TEMPLATES, column_def_opt),
    Column_Def_Opt_Length = length(Column_Def_Opt),
    [{data_type, Data_Type}] = ets:lookup(?CODE_TEMPLATES, data_type),
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
    [{column_commalist, Column_Commalist}] = ets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{function_ref, Function_Ref}] = ets:lookup(?CODE_TEMPLATES, function_ref),
    Function_Ref_Length = length(Function_Ref),
    [{literal, Literal}] = ets:lookup(?CODE_TEMPLATES, literal),
    Literal_Length = length(Literal),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            "Default Null",
            "Default User",
            "Not Null",
            "Not Null Unique",
            "Not Null Primary Key",
            "Not Null Unique"
        ] ++
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    "Default ",
                    case rand:uniform(3) rem 3 of
                        1 -> lists:nth(rand:uniform(Function_Ref_Length), Function_Ref);
                        2 -> lists:nth(rand:uniform(Literal_Length), Literal);
                        _ -> lists:nth(rand:uniform(Name_Length), Name)
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
    [{json, Json}] = ets:lookup(?CODE_TEMPLATES, json),
    Json_Length = length(Json),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(10) rem 10 of
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
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    lists:nth(rand:uniform(Json_Length), Json)
                ]);
                4 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    lists:nth(rand:uniform(Json_Length), Json)
                ]);
                5 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    lists:nth(rand:uniform(Json_Length), Json)
                ]);
                6 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "(+)"
                ]);
                7 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "(+)"
                ]);
                8 -> lists:nth(rand:uniform(Name_Length), Name) ++ "(+)";
                9 -> lists:append([
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
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ccolumn_ref_commalist ::= ( column_ref | function_ref ) ( ',' ( column_ref | function_ref ) )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(column_ref_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{column_ref, Column_Ref}] = ets:lookup(?CODE_TEMPLATES, column_ref),
    Column_Ref_Length = length(Column_Ref),
    [{function_ref, Function_Ref}] = ets:lookup(?CODE_TEMPLATES, function_ref),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (=|!=|^=|<>|<|>|<=|>=)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(comparison = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            " = ",
            " != ",
            " ^= ",
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
%%                        | (         scalar_exp COMPARISON 'PRIOR' scalar_exp )
%%                        | ( 'PRIOR' scalar_exp COMPARISON         scalar_exp )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(comparison_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{comparison, Comparison}] = ets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                    " ",
                    lists:nth(rand:uniform(Comparison_Length), Comparison),
                    " ",
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp))
                ]);
                2 -> lists:append([
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                    " ",
                    lists:nth(rand:uniform(Comparison_Length), Comparison),
                    " Prior ",
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp))
                ]);
                _ -> lists:append([
                    "Prior ",
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp)),
                    " ",
                    lists:nth(rand:uniform(Comparison_Length), Comparison),
                    " ",
                    bracket_query_spec(lists:nth(rand:uniform(Scalar_Exp_Length), Scalar_Exp))
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(predicate, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(search_condition, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create_index_def ::= 'CREATE' ( 'BITMAP' | 'KEYLIST' | 'HASHMAP' | 'UNIQUE' )? 'INDEX' ( index_name )? 'ON' table ( '(' ( NAME JSON? ) ( ',' NAME JSON? )* ')' )?
%%                      ( 'NORM_WITH' STRING )?  ( 'FILTER_WITH' STRING )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create_index_def = Rule) ->
    ?CREATE_CODE_START,
    [{index_name, Index_Name}] = ets:lookup(?CODE_TEMPLATES, index_name),
    Index_Name_Length = length(Index_Name),
    [{json, Json}] = ets:lookup(?CODE_TEMPLATES, json),
    Json_Length = length(Json),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{string, String}] = ets:lookup(?CODE_TEMPLATES, string),
    String_Length = length(String),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
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
                        "( ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:append([
                                lists:nth(rand:uniform(Name_Length), Name),
                                " ",
                                lists:nth(rand:uniform(Json_Length), Json)
                            ]);
                            _ -> lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ", ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:append([
                                lists:nth(rand:uniform(Name_Length), Name),
                                " ",
                                lists:nth(rand:uniform(Json_Length), Json)
                            ]);
                            _ -> lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ", ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:append([
                                lists:nth(rand:uniform(Name_Length), Name),
                                " ",
                                lists:nth(rand:uniform(Json_Length), Json)
                            ]);
                            _ -> lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ") "
                    ]);
                    2 -> lists:append([
                        "( ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:append([
                                lists:nth(rand:uniform(Name_Length), Name),
                                " ",
                                lists:nth(rand:uniform(Json_Length), Json)
                            ]);
                            _ -> lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ", ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:append([
                                lists:nth(rand:uniform(Name_Length), Name),
                                " ",
                                lists:nth(rand:uniform(Json_Length), Json)
                            ]);
                            _ -> lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ") "
                    ]);
                    3 -> lists:append([
                        "( ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:append([
                                lists:nth(rand:uniform(Name_Length), Name),
                                " ",
                                lists:nth(rand:uniform(Json_Length), Json)
                            ]);
                            _ -> lists:nth(rand:uniform(Name_Length), Name)
                        end,
                        ") "
                    ]);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 -> " Norm_with " ++ lists:nth(rand:uniform(String_Length), String);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 -> " Filter_with " ++ lists:nth(rand:uniform(String_Length), String);
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create_role_def ::= 'CREATE' 'ROLE' NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create_role_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),

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
    [{base_table_element, Base_Table_Element}] = ets:lookup(?CODE_TEMPLATES, base_table_element),
    Base_Table_Element_Length = length(Base_Table_Element),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),
    [{tbl_scope, Tbl_Scope}] = ets:lookup(?CODE_TEMPLATES, tbl_scope),
    Tbl_Scope_Length = length(Tbl_Scope),
    [{tbl_type, Tbl_Type}] = ets:lookup(?CODE_TEMPLATES, tbl_type),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(schema_element, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create_user_def ::= 'CREATE' 'USER' NAME identified ( user_opt )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create_user_def = Rule) ->
    ?CREATE_CODE_START,
    [{identified, Identified}] = ets:lookup(?CODE_TEMPLATES, identified),
    Identified_Length = length(Identified),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{user_opt, User_Opt}] = ets:lookup(?CODE_TEMPLATES, user_opt),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cursor ::= NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cursor = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),

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
    [{cursor, Cursor}] = ets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{order_by_clause, Order_By_Clause}] = ets:lookup(?CODE_TEMPLATES, order_by_clause),
    Order_By_Clause_Length = length(Order_By_Clause),
    [{query_exp, Query_Exp}] = ets:lookup(?CODE_TEMPLATES, query_exp),
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
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data_type ::= STRING
%%             | ( NAME ( '(' opt_sgn_num ')' )? )
%%             | ( NAME '(' sgn_num ',' sgn_num ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(data_type = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{sgn_num, Sgn_Num}] = ets:lookup(?CODE_TEMPLATES, sgn_num),
    Sgn_Num_Length = length(Sgn_Num),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    " (",
                    lists:nth(rand:uniform(Sgn_Num_Length), Sgn_Num),
                    ")"
                ]);
                _ -> lists:append([
                    lists:nth(rand:uniform(Name_Length), Name),
                    " (",
                    lists:nth(rand:uniform(Sgn_Num_Length), Sgn_Num),
                    ",",
                    lists:nth(rand:uniform(Sgn_Num_Length), Sgn_Num),
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
    [{proxy_with, Proxy_With}] = ets:lookup(?CODE_TEMPLATES, proxy_with),
    Proxy_With_Length = length(Proxy_With),

    Code =
        [
            "Authentication Required"
        ] ++
        [
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
    [{cursor, Cursor}] = ets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{returning, Returning}] = ets:lookup(?CODE_TEMPLATES, returning),
    Returning_Length = length(Returning),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),
    [{where_clause, Where_Clause}] = ets:lookup(?CODE_TEMPLATES, where_clause),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_index_def ::= 'DROP' 'INDEX' ( index_name )? 'FROM' table
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_index_def = Rule) ->
    ?CREATE_CODE_START,
    [{index_name, Index_Name}] = ets:lookup(?CODE_TEMPLATES, index_name),
    Index_Name_Length = length(Index_Name),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_role_def ::= 'DROP' 'ROLE' NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_role_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),

    Code =
        [
                "Drop Role " ++ N || N <- Name
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_table_def ::= 'DROP' ( NAME )? 'TABLE' ( 'IF' 'EXISTS' )? ( table ( ',' table )* ) ( 'RESTRICT' | 'CASCADE' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_table_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop_user_def ::= 'DROP' 'USER' NAME ( 'CASCADE' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(drop_user_def = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% existence_test ::= 'EXISTS' subquery
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(existence_test = Rule) ->
    ?CREATE_CODE_START,
    [{subquery, Subquery}] = ets:lookup(?CODE_TEMPLATES, subquery),
    Subquery_Length = length(Subquery),

    Code =
        [
                "Exists " ++ bracket_query_spec(lists:nth(rand:uniform(Subquery_Length), Subquery))
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(predicate, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(search_condition, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extra ::= NAME  ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(extra = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),

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
    [{cursor, Cursor}] = ets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{target_commalist, Target_Commalist}] = ets:lookup(?CODE_TEMPLATES, target_commalist),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
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
    [{comparison, Comparison}] = ets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{fun_arg, Fun_Arg}] = ets:lookup(?CODE_TEMPLATES, fun_arg),
    Fun_Arg_Length = length(Fun_Arg),
    [{literal, Literal}] = ets:lookup(?CODE_TEMPLATES, literal),
    Literal_Length = length(Literal),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        lists:append([
            Fun_Arg,
            [
                "Null"
            ],
            [
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
            ]
        ]),
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fun_args ::= fun_arg ( ',' fun_arg )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(fun_args = Rule) ->
    ?CREATE_CODE_START,
    [{fun_arg, Fun_Arg}] = ets:lookup(?CODE_TEMPLATES, fun_arg),
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
    [{column_ref, Column_Ref}] = ets:lookup(?CODE_TEMPLATES, column_ref),
    Column_Ref_Length = length(Column_Ref),
    [{funs, Funs}] = ets:lookup(?CODE_TEMPLATES, funs),
    Funs_Length = length(Funs),
    [{fun_args, Fun_Args}] = ets:lookup(?CODE_TEMPLATES, fun_args),
    Fun_Args_Length = length(Fun_Args),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
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
                             " (",
                             lists:nth(rand:uniform(Fun_Args_Length), Fun_Args),
                             ")"
                         ]);
                         2 -> lists:append([
                             lists:nth(rand:uniform(Name_Length), Name),
                             ".",
                             lists:nth(rand:uniform(Name_Length), Name),
                             " (",
                             lists:nth(rand:uniform(Fun_Args_Length), Fun_Args),
                             ")"
                         ]);
                         _ -> lists:append([
                             lists:nth(rand:uniform(Name_Length), Name),
                             " (",
                             lists:nth(rand:uniform(Fun_Args_Length), Fun_Args),
                             ")"
                         ])
                     end;
                _ -> lists:nth(rand:uniform(Funs_Length), Funs) ++
                case rand:uniform(20) rem 20 of
                    1 -> [];
                    _ -> lists:append([
                        " (",
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
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function_ref_list ::= ( function_ref ';' )
%%                     | ( function_ref ';' function_ref_list )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(function_ref_list = Rule) ->
    ?CREATE_CODE_START,
    [{function_ref, Function_Ref}] = ets:lookup(?CODE_TEMPLATES, function_ref),
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
%% grant_def ::= 'GRANT' (
%%                         ( ( ( 'ALL' 'PRIVILEGES' ) | ( object_privilege (',' object_privilege )* ) ) on_obj_clause 'TO' ( grantee_identified_by | ( grantee_revokee ( ',' grantee_revokee )* ) ) ( 'WITH' ( 'GRANT' | 'HIERARCHY' ) 'OPTION' )? )
%%                       | ( ( ( 'ALL' 'PRIVILEGES' ) | ( system_privilege (',' system_privilege )* ) )               'TO' ( grantee_identified_by | ( grantee_revokee ( ',' grantee_revokee )* ) ) ( 'WITH' ( 'ADMIN' | 'DELEGATE'  ) 'OPTION' )? )
%%                       )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(grant_def = Rule) ->
    ?CREATE_CODE_START,
    [{grantee_identified_by, Grantee_Identified_By}] = ets:lookup(?CODE_TEMPLATES, grantee_identified_by),
    Grantee_Identified_By_Length = length(Grantee_Identified_By),
    [{grantee_revokee_commalist, Grantee_Revokee_Commalist}] = ets:lookup(?CODE_TEMPLATES, grantee_revokee_commalist),
    Grantee_Revokee_Commalist_Length = length(Grantee_Revokee_Commalist),
    [{object_privilege_list, Object_Privilege_List}] = ets:lookup(?CODE_TEMPLATES, object_privilege_list),
    Object_Privilege_List_Length = length(Object_Privilege_List),
    [{object_with_grant_option, Object_With_Grant_Option}] = ets:lookup(?CODE_TEMPLATES, object_with_grant_option),
    Object_With_Grant_Option_Length = length(Object_With_Grant_Option),
    [{on_obj_clause, On_Obj_Clause}] = ets:lookup(?CODE_TEMPLATES, on_obj_clause),
    On_Obj_Clause_Length = length(On_Obj_Clause),
    [{system_privilege_list, System_Privilege_List}] = ets:lookup(?CODE_TEMPLATES, system_privilege_list),
    System_Privilege_List_Length = length(System_Privilege_List),
    [{system_with_grant_option, System_With_Grant_Option}] = ets:lookup(?CODE_TEMPLATES, system_with_grant_option),
    System_With_Grant_Option_Length = length(System_With_Grant_Option),

    Code =
        [
                "Grant" ++
                case rand:uniform(2) rem 2 of
                    1 ->
                        lists:append([
                            " ",
                            case rand:uniform(4) rem 4 of
                                1 -> "All Privileges ";
                                _ ->
                                    lists:nth(rand:uniform(Object_Privilege_List_Length), Object_Privilege_List)
                            end,
                            " ",
                            lists:nth(rand:uniform(On_Obj_Clause_Length), On_Obj_Clause),
                            " To ",
                            case rand:uniform(4) rem 4 of
                                1 ->
                                    lists:nth(rand:uniform(Grantee_Identified_By_Length), Grantee_Identified_By);
                                _ ->
                                    lists:nth(rand:uniform(Grantee_Revokee_Commalist_Length), Grantee_Revokee_Commalist)
                            end,
                            case rand:uniform(2) rem 2 of
                                1 ->
                                    " " ++ lists:nth(rand:uniform(Object_With_Grant_Option_Length), Object_With_Grant_Option);
                                _ -> []
                            end
                        ]);
                    _ ->
                        lists:append([
                            " ",
                            case rand:uniform(4) rem 4 of
                                1 -> "All Privileges ";
                                _ ->
                                    lists:nth(rand:uniform(System_Privilege_List_Length), System_Privilege_List)
                            end,
                            " To ",
                            case rand:uniform(4) rem 4 of
                                1 ->
                                    lists:nth(rand:uniform(Grantee_Identified_By_Length), Grantee_Identified_By);
                                _ ->
                                    lists:nth(rand:uniform(Grantee_Revokee_Commalist_Length), Grantee_Revokee_Commalist)
                            end,
                            case rand:uniform(2) rem 2 of
                                1 ->
                                    " " ++ lists:nth(rand:uniform(System_With_Grant_Option_Length), System_With_Grant_Option);
                                _ -> []
                            end
                        ])
                end
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(schema_element, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% grantee_identified_by ::= NAME 'IDENTIFIED' 'BY' STRING
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(grantee_identified_by = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{string, String}] = ets:lookup(?CODE_TEMPLATES, string),
    String_Length = length(String),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(Name_Length), Name),
                " Identified By ",
                lists:nth(rand:uniform(String_Length), String)
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% grantee_revokee ::= NAME
%%                  | 'PUBLIC'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(grantee_revokee = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Public"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% grantee_revokee_commalist ::= grantee_revokee ( ',' grantee_revokee )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(grantee_revokee_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{grantee_revokee, Grantee_Revokee}] = ets:lookup(?CODE_TEMPLATES, grantee_revokee),
    Grantee_Revokee_Length = length(Grantee_Revokee),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee),
                    ",",
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee),
                    ",",
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee),
                    ",",
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee),
                    ",",
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee),
                    ",",
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee),
                    ",",
                    lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee)
                ]);
                _ -> lists:nth(rand:uniform(Grantee_Revokee_Length), Grantee_Revokee)
            end
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
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
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
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "Identified Externally",
            "Identified Globally"
        ] ++
        [
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
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),
    [{scalar_exp_commalist, Scalar_Exp_Commalist}] = ets:lookup(?CODE_TEMPLATES, scalar_exp_commalist),
    Scalar_Exp_Commalist_Length = length(Scalar_Exp_Commalist),
    [{subquery, Subquery}] = ets:lookup(?CODE_TEMPLATES, subquery),
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
    store_code(predicate, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(search_condition, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% index_name ::= ( NAME '.' )? NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(index_name = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
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
    [{join_on_or_using_clause, Join_On_Or_Using_Clause}] = ets:lookup(?CODE_TEMPLATES, join_on_or_using_clause),
    Join_On_Or_Using_Clause_Length = length(Join_On_Or_Using_Clause),
    [{join_ref, Join_Ref}] = ets:lookup(?CODE_TEMPLATES, join_ref),
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
    store_code(join, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% insert_statement ::= 'INSERT' 'INTO' table ( ( '(' column_commalist ')' )? ( ( 'VALUES' '(' scalar_opt_as_exp ( ',' scalar_opt_as_exp )* ')' ) | query_spec ) ( returning )? )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(insert_statement = Rule) ->
    ?CREATE_CODE_START,
    [{column_commalist, Column_Commalist}] = ets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{query_spec, Query_Spec}] = ets:lookup(?CODE_TEMPLATES, query_spec),
    Query_Spec_Length = length(Query_Spec),
    [{returning, Returning}] = ets:lookup(?CODE_TEMPLATES, returning),
    Returning_Length = length(Returning),
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
    Scalar_Opt_As_Exp_Length = length(Scalar_Opt_As_Exp),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, true),
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
    store_code(atom, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(literal, Code, ?MAX_BASIC, false),
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join_clause ::= table_ref join ( join )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(join_clause = Rule) ->
    ?CREATE_CODE_START,
    [{join, Join}] = ets:lookup(?CODE_TEMPLATES, join),
    Join_Length = length(Join),
    [{table_ref, Table_Ref}] = ets:lookup(?CODE_TEMPLATES, table_ref),
    Table_Ref_Length = length(Table_Ref),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(Table_Ref_Length), Table_Ref),
                " ",
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

            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(from_column, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(from_column, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join_on_or_using_clause ::= ( 'ON' search_condition )
%%                           | ( 'USING' '(' select_field_commalist ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(join_on_or_using_clause = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),
    [{select_field_commalist, Select_Field_Commalist}] = ets:lookup(?CODE_TEMPLATES, select_field_commalist),
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
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{query_exp, Query_Exp}] = ets:lookup(?CODE_TEMPLATES, query_exp),
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
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% like_predicate ::= scalar_exp ( 'NOT' )? 'LIKE' scalar_exp ( 'ESCAPE' atom )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(like_predicate = Rule) ->
    ?CREATE_CODE_START,
    [{atom, Atom}] = ets:lookup(?CODE_TEMPLATES, atom),
    Atom_Length = length(Atom),
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
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
    store_code(predicate, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(search_condition, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(search_condition, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\"((\$|[^\"]*)*(\"\")*)*\")
%% [A-Za-z][A-Za-z0-9_@\$]*
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
            "credit_limit_ident_1",
            "credit_limit_ident_2",
            "credit_limit_ident_3",
            "credit_limit_ident_4",
            "credit_limit_ident_5",
            "I1IDENT_000_1",
            "I1IDENT_100_1",
            "I1IDENT_1_",
            "I1IDENT_200_1",
            "I1IDENT_2__",
            "I1IDENT_3_5",
            "I1IDENT_4_6",
            "I1IDENT_5",
            "I1IDENT_6",
            "I1IDENT_7",
            "I1IDENT_8",
            "I1IDENT_9",
            "I@IDENT_000_@",
            "I@IDENT_100_@",
            "I@IDENT_1_",
            "I@IDENT_200_@",
            "I@IDENT_2__",
            "I@IDENT_3_$",
            "I@IDENT_4_$",
            "I@IDENT_5",
            "I@IDENT_6",
            "I@IDENT_7",
            "I@IDENT_8",
            "I@IDENT_9",
            "L1astName_ident",
            "L@astName_ident",
            "m1oney~66tree_ident",
            "m@oney~~$tree_ident",
            "o1racle6number_ident",
            "o@racle$number_ident",
            "p1hone5_ident",
            "p@hone$_ident",
            "S1N~5_ident",
            "S@N~$_ident",
            "t12_ident",
            "t1ry_again__ident",
            "t@2_ident",
            "t@ry_again__ident",
            "X1_ident",
            "X1YZ_ident",
            "X@_ident",
            "X@YZ_ident"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(column, [re:replace(re:replace(C, "ident", "ident_column", [{return, list}]), "IDENT", "IDENT_COLUMN", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(column_ref, [re:replace(re:replace(C, "ident", "ident_column_ref", [{return, list}]), "IDENT", "IDENT_COLUMN_REF", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(from_column, [re:replace(re:replace(C, "ident", "ident_from_column", [{return, list}]), "IDENT", "IDENT_FROM_COLUMN", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(grantee_revokee, [re:replace(re:replace(C, "ident", "ident_grantee_revokee", [{return, list}]), "IDENT", "IDENT_GRANTEE_REVOKEE", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(scalar_exp, [re:replace(re:replace(C, "ident", "ident_scalar_exp", [{return, list}]), "IDENT", "IDENT_SCALAR_EXP", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(scalar_sub_exp, [re:replace(re:replace(C, "ident", "ident_scalar_sub_exp", [{return, list}]), "IDENT", "IDENT_SCALAR_SUB_EXP", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(system_privilege, [re:replace(re:replace(C, "ident", "ident_sys_priv", [{return, list}]), "IDENT", "IDENT_SYS_PRIV", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(table, [re:replace(re:replace(C, "ident", "ident_table", [{return, list}]), "IDENT", "IDENT_TABLE", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(table_ref, [re:replace(re:replace(C, "ident", "ident_table_ref", [{return, list}]), "IDENT", "IDENT_TABLE_REF", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(target, [re:replace(re:replace(C, "ident", "ident_target", [{return, list}]), "IDENT", "IDENT_TARGET", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(tbl_type, [re:replace(re:replace(C, "ident", "ident_tbl_type", [{return, list}]), "IDENT", "IDENT_TBL_TYPE", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% object_privilege ::= 'ALL'
%%                    | 'ALTER'
%%                    | 'DELETE'
%%                    | 'EXECUTE'
%%                    | 'INDEX'
%%                    | 'INSERT'
%%                    | 'REFERENCES'
%%                    | 'SELECT'
%%                    | 'UPDATE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(object_privilege = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "All",
            "Alter",
            "Delete",
            "Execute",
            "Index",
            "Insert",
            "References",
            "Select",
            "Update"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% object_privilege_list ::= object_privilege ( ',' object_privilege )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(object_privilege_list = Rule) ->
    ?CREATE_CODE_START,
    [{object_privilege, Object_Privilege}] = ets:lookup(?CODE_TEMPLATES, object_privilege),
    Object_Privilege_Length = length(Object_Privilege),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append([
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege),
                    ",",
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege),
                    ",",
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege),
                    ",",
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege)
                ]);
                2 -> lists:append([
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege),
                    ",",
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege),
                    ",",
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege)
                ]);
                3 -> lists:append([
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege),
                    ",",
                    lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege)
                ]);
                _ -> lists:nth(rand:uniform(Object_Privilege_Length), Object_Privilege)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% object_with_grant_option ::= ( 'CASCADE' 'CONSTRAINTS' ) | 'FORCE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(object_with_grant_option = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "With Grant Option",
            "With Hierarchy Option"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% object_with_revoke_option ::= 'WITH' ( 'GRANT' | 'HIERARCHY' ) 'OPTION'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(object_with_revoke_option = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Cascade Constraints",
            "Force"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% on_obj_clause ::= ( 'ON' table )
%%                 | ( 'ON' 'DIRECTORY' NAME )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(on_obj_clause = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
                "On" ++
                case rand:uniform(3) rem 3 of
                    1 -> " " ++ lists:nth(rand:uniform(Table_Length), Table);
                    _ -> " Directory " ++ lists:nth(rand:uniform(Name_Length), Name)
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
    [{cursor, Cursor}] = ets:lookup(?CODE_TEMPLATES, cursor),

    Code =
        [
                "Open " ++ C || C <- Cursor
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% order_by_clause ::= 'ORDER' 'BY' ordering_spec ( ',' ordering_spec )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(order_by_clause = Rule) ->
    ?CREATE_CODE_START,
    [{ordering_spec, Ordering_Spec}] = ets:lookup(?CODE_TEMPLATES, ordering_spec),
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
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
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
    [{join_on_or_using_clause, Join_On_Or_Using_Clause}] = ets:lookup(?CODE_TEMPLATES, join_on_or_using_clause),
    Join_On_Or_Using_Clause_Length = length(Join_On_Or_Using_Clause),
    [{join_ref, Join_Ref}] = ets:lookup(?CODE_TEMPLATES, join_ref),
    Join_Ref_Length = length(Join_Ref),
    [{outer_join_type, Outer_Join_Type}] = ets:lookup(?CODE_TEMPLATES, outer_join_type),
    Outer_Join_Type_Length = length(Outer_Join_Type),
    [{query_partition_clause, Query_Partition_Clause}] = ets:lookup(?CODE_TEMPLATES, query_partition_clause),
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
    store_code(join, Code, ?MAX_BASIC, false),
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
    store_code(from_column, [re:replace(re:replace(C, "par", "par_from_column", [{return, list}]), "PAR", "PAR_FROM_COLUMN", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(table, [re:replace(re:replace(C, "par", "par_table", [{return, list}]), "PAR", "PAR_TABLE", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(table_ref, [re:replace(re:replace(C, "par", "par_table_ref", [{return, list}]), "PAR", "PAR_TABLE_REF", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameter_ref ::= parameter ( ( 'INDICATOR' )? parameter )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameter_ref = Rule) ->
    ?CREATE_CODE_START,
    [{parameter, Parameter}] = ets:lookup(?CODE_TEMPLATES, parameter),
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
    store_code(atom, Code, ?MAX_BASIC, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    store_code(target, Code, ?MAX_BASIC, false),
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
    [{function_ref, Function_Ref}] = ets:lookup(?CODE_TEMPLATES, function_ref),
    Function_Ref_Length = length(Function_Ref),
    [{function_ref_list, Function_Ref_List}] = ets:lookup(?CODE_TEMPLATES, function_ref_list),
    Function_Ref_List_Length = length(Function_Ref_List),
    [{sql_list, Sql_List}] = ets:lookup(?CODE_TEMPLATES, sql_list),
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
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% proxy_clause ::= ( 'GRANT' | 'REVOKE' ) 'CONNECT' 'THROUGH' ( ( 'ENTERPRISE' 'USERS' ) | db_user_proxy )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(proxy_clause = Rule) ->
    ?CREATE_CODE_START,
    [{db_user_proxy, Db_User_Proxy}] = ets:lookup(?CODE_TEMPLATES, db_user_proxy),
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
    [{role_list, Role_List}] = ets:lookup(?CODE_TEMPLATES, role_list),
    Role_List_Length = length(Role_List),

    Code =
        [
            "With No Roles"
        ] ++
        [
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
    store_code(db_user_proxy, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_partition_clause ::= 'PARTITION' 'BY' ( ( '(' scalar_exp_commalist ')' ) | scalar_exp_commalist )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_partition_clause = Rule) ->
    ?CREATE_CODE_START,
    [{scalar_exp_commalist, Scalar_Exp_Commalist}] = ets:lookup(?CODE_TEMPLATES, scalar_exp_commalist),
    Scalar_Exp_Commalist_Length = length(Scalar_Exp_Commalist),

    Code =
        [
                "Partition By " ++
                lists:nth(rand:uniform(Scalar_Exp_Commalist_Length), Scalar_Exp_Commalist)
%% wwe: currently not supported
%%                "Partition By" ++
%%                case rand:uniform(2) rem 2 of
%%                    1 -> lists:append([
%%                        "(",
%%                        lists:nth(rand:uniform(Scalar_Exp_Commalist_Length), Scalar_Exp_Commalist),
%%                        ")"
%%                    ]);
%%                    _ ->
%%                        " " ++ lists:nth(rand:uniform(Scalar_Exp_Commalist_Length), Scalar_Exp_Commalist)
%%                end
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
    [{query_exp, Query_Exp}] = ets:lookup(?CODE_TEMPLATES, query_exp),
    Query_Exp_Length = length(Query_Exp),
    [{query_term, Query_Term}] = ets:lookup(?CODE_TEMPLATES, query_term),
    Query_Term_Length = length(Query_Term),

    Code =
        Query_Exp ++
        [
            lists:append([
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
                bracket_query_spec(lists:nth(rand:uniform(Query_Term_Length), Query_Term))
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, false),
    store_code(fun_arg, Code, ?MAX_BASIC, false),
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    store_code(scalar_sub_exp, Code, ?MAX_BASIC, false),
    store_code(select_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(subquery, Code, ?MAX_STATEMENT_COMPLEX, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_spec ::= 'SELECT' ( HINT )? ( 'ALL' | 'DISTINCT' )? selection ( 'INTO' target_commalist ( 'IN' NAME )? )? table_exp
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_spec = Rule) ->
    ?CREATE_CODE_START,
    [{hint, Hint}] = ets:lookup(?CODE_TEMPLATES, hint),
    Hint_Length = length(Hint),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{selection, Selection}] = ets:lookup(?CODE_TEMPLATES, selection),
    Selection_Length = length(Selection),
    [{table_exp, Table_Exp}] = ets:lookup(?CODE_TEMPLATES, table_exp),
    Table_Exp_Length = length(Table_Exp),
    [{target_commalist, Target_Commalist}] = ets:lookup(?CODE_TEMPLATES, target_commalist),
    Target_Commalist_Length = length(Target_Commalist),

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
                    1 -> lists:append([" Into ",
                        lists:nth(rand:uniform(Target_Commalist_Length), Target_Commalist),
                        case rand:uniform(2) rem 2 of
                            1 ->
                                " In " ++ lists:nth(rand:uniform(Name_Length), Name);
                            _ -> []
                        end
                    ]);
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
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
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
    [{query_exp, Query_Exp}] = ets:lookup(?CODE_TEMPLATES, query_exp),
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
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
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
    [{intnum, Intnum}] = ets:lookup(?CODE_TEMPLATES, intnum),
    Intnum_Length = length(Intnum),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
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
%% Reference examples from Oracle documentation.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(referenceExamples = Rule) ->
    ?CREATE_CODE_START,

    LineSep = io_lib:nl(),

    Code = [
        lists:append([
            LineSep,
            "        -- =====================================================================", LineSep,
            "        -- from book:          ", Book, LineSep,
            "        --      chapter:       ", Chapter, LineSep,
            "        --      section:       ", Section, LineSep,
            case SubSection of
                [] -> [];
                _ ->
                    lists:append(["        --      subsection:    ", SubSection, LineSep])
            end,
            case SubSubSection of
                [] -> [];
                _ ->
                    lists:append(["        --      subsubsection: ", SubSubSection, LineSep])
            end,
            case Example of
                [] -> [];
                _ ->
                    lists:append(["        --                     ", Example, LineSep])
            end,
            "        -- ---------------------------------------------------------------------", LineSep,
            string:replace(CodeExample, "\"", "\\\"", all)
        ]) || {Book, Chapter, Section, SubSection, SubSubSection, Example, CodeExample} <- ?TESTS_FROM_DATABASE_SQL_LANGUAGE_REFERENCE_V12C_2
    ],
    ets:insert(?CODE_TEMPLATES, {Rule, Code}),
    ok;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% returning ::= ( 'RETURNING' | 'RETURN' ) selection 'INTO' selection
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(returning = Rule) ->
    ?CREATE_CODE_START,
    [{selection, Selection}] = ets:lookup(?CODE_TEMPLATES, selection),
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
%% revoke_def ::= 'REVOKE' (
%%                           ( ( ( 'ALL' 'PRIVILEGES' ) | ( object_privilege (',' object_privilege )* ) ) on_obj_clause 'FROM' grantee_revokee ( ',' grantee_revokee )* ( ( 'CASCADE' 'CONSTRAINTS' ) | 'FORCE' )? )
%%                         | ( ( ( 'ALL' 'PRIVILEGES' ) | ( system_privilege (',' system_privilege )* ) )               'FROM' grantee_revokee ( ',' grantee_revokee )* )
%%                         )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(revoke_def = Rule) ->
    ?CREATE_CODE_START,
    [{grantee_revokee_commalist, Grantee_Revokee_Commalist}] = ets:lookup(?CODE_TEMPLATES, grantee_revokee_commalist),
    Grantee_Revokee_Commalist_Length = length(Grantee_Revokee_Commalist),
    [{object_privilege_list, Object_Privilege_List}] = ets:lookup(?CODE_TEMPLATES, object_privilege_list),
    Object_Privilege_List_Length = length(Object_Privilege_List),
    [{object_with_revoke_option, Object_With_Revoke_Option}] = ets:lookup(?CODE_TEMPLATES, object_with_revoke_option),
    Object_With_Revoke_Option_Length = length(Object_With_Revoke_Option),
    [{on_obj_clause, On_Obj_Clause}] = ets:lookup(?CODE_TEMPLATES, on_obj_clause),
    On_Obj_Clause_Length = length(On_Obj_Clause),
    [{system_privilege_list, System_Privilege_List}] = ets:lookup(?CODE_TEMPLATES, system_privilege_list),
    System_Privilege_List_Length = length(System_Privilege_List),

    Code =
        [
                "Revoke" ++
                case rand:uniform(2) rem 2 of
                    1 ->
                        lists:append([
                            " ",
                            case rand:uniform(4) rem 4 of
                                1 -> "All Privileges ";
                                _ ->
                                    lists:nth(rand:uniform(Object_Privilege_List_Length), Object_Privilege_List)
                            end,
                            " ",
                            lists:nth(rand:uniform(On_Obj_Clause_Length), On_Obj_Clause),
                            " From ",
                            lists:nth(rand:uniform(Grantee_Revokee_Commalist_Length), Grantee_Revokee_Commalist),
                            case rand:uniform(2) rem 2 of
                                1 ->
                                    " " ++ lists:nth(rand:uniform(Object_With_Revoke_Option_Length), Object_With_Revoke_Option);
                                _ -> []
                            end
                        ]);
                    _ ->
                        lists:append([
                            " ",
                            case rand:uniform(4) rem 4 of
                                1 -> "All Privileges ";
                                _ ->
                                    lists:nth(rand:uniform(System_Privilege_List_Length), System_Privilege_List)
                            end,
                            " From ",
                            lists:nth(rand:uniform(Grantee_Revokee_Commalist_Length), Grantee_Revokee_Commalist)
                        ])
                end
            || _ <- lists:seq(1, ?MAX_STATEMENT_SIMPLE * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% role_list ::= NAME ( ',' NAME )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(role_list = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
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
    store_code(proxy_with, ["With Role " ++ C || C <- Code], ?MAX_BASIC, false),
    store_code(proxy_with, ["With Role All Except " ++ C || C <- Code], ?MAX_BASIC, false),
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
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
    Scalar_Exp_Length = length(Scalar_Exp),
    [{scalar_sub_exp, Scalar_Sub_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_sub_exp),
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
    [{scalar_opt_as_exp, Scalar_Opt_As_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_opt_as_exp),
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
    [{comparison, Comparison}] = ets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
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
    store_code(select_field, Code, ?MAX_BASIC, false),
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
    [{literal, Literal}] = ets:lookup(?CODE_TEMPLATES, literal),
    Literal_Length = length(Literal),
    [{scalar_sub_exp, Scalar_Sub_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_sub_exp),
    Scalar_Sub_Exp_Length = length(Scalar_Sub_Exp),

    Code =
        lists:append([
            Scalar_Sub_Exp,
            [
                "Null"
            ],
            [
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
            ]
        ]),
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(scalar_exp, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% schema ::= 'CREATE' 'SCHEMA' 'AUTHORIZATION' NAME ( schema_element ( schema_element )* )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(schema = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{schema_element, Schema_Element}] = ets:lookup(?CODE_TEMPLATES, schema_element),
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
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% search_condition ::= ( search_condition ( 'AND' | 'OR' ) search_condition )
%%                    | ( 'NOT' search_condition )
%%                    | ( '(' search_condition ')' )
%%                    | predicate
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(search_condition = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
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
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(select_field = Rule) ->
    ?CREATE_CODE_START,
    [{case_when_exp, Case_When_Exp}] = ets:lookup(?CODE_TEMPLATES, case_when_exp),
    Case_When_Exp_Length = length(Case_When_Exp),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "*"
        ] ++
        [
                lists:nth(rand:uniform(Case_When_Exp_Length), Case_When_Exp) ++
                case rand:uniform(3) rem 3 of
                    1 -> " As " ++ lists:nth(rand:uniform(Name_Length), Name);
                    2 -> " " ++ lists:nth(rand:uniform(Name_Length), Name);
                    _ -> []
                end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(select_field_commalist, Code, ?MAX_BASIC, false),
    store_code(selection, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% select_field_commalist ::= select_field ( ',' select_field )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(select_field_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{select_field, Select_Field}] = ets:lookup(?CODE_TEMPLATES, select_field),
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
    store_code(select, Code, ?MAX_BASIC, false),
    store_code(selection, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sgn_num ::= ( '-' )? INTNUM
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sgn_num = Rule) ->
    ?CREATE_CODE_START,
    [{intnum, Intnum}] = ets:lookup(?CODE_TEMPLATES, intnum),

    Code = Intnum ++
        [
                "-" ++ I || I <- Intnum
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
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
        "Select - 10                     * -5                       From dual",
        "Select - 10                     / -5                       From dual",
        "Select - 10                     + -5                       From dual",
        "Select - 10                     * col_2                    From dual",
        "Select - 10                     / col_2                    From dual",
        "Select - 10                     + col_2                    From dual",
        "Select - 10                     * 10 + 5                   From dual",
        "Select - 10                     / 10 + 5                   From dual",
        "Select - 10                     + 10 + 5                   From dual",
        "Select - 10                     * (Select col_2 From dual) From dual",
        "Select - 10                     / (Select col_2 From dual) From dual",
        "Select - 10                     + (Select col_2 From dual) From dual",
        "Select col_1                    * -5                       From dual",
        "Select col_1                    / -5                       From dual",
        "Select col_1                    + -5                       From dual",
        "Select 5 + 10                   * -5                       From dual",
        "Select 5 + 10                   / -5                       From dual",
        "Select 5 + 10                   + -5                       From dual",
        "Select (Select col_1 From dual) * -5                       From dual",
        "Select (Select col_1 From dual) / -5                       From dual",
        "Select (Select col_1 From dual) + -5                       From dual",
        "Select col_1                    * col_2                    From dual",
        "Select col_1                    / col_2                    From dual",
        "Select col_1                    + col_2                    From dual",
        "Select 5 + 10                   * 10 + 5                   From dual",
        "Select 5 + 10                   / 10 + 5                   From dual",
        "Select 5 + 10                   + 10 + 5                   From dual",
        "Select (Select col_1 From dual) * (Select col_2 From dual) From dual",
        "Select (Select col_1 From dual) / (Select col_2 From dual) From dual",
        "Select (Select col_1 From dual) + (Select col_2 From dual) From dual",
        %% ---------------------------------------------------------------------
        %% changed: JSON
        %% ---------------------------------------------------------------------
        %% create_index_spec_items -> NAME JSON?
        %% create_index_spec_items -> NAME JSON? ',' create_index_spec_items
        %% ---------------------------------------------------------------------
        "Create Index a On b (c|:d{}|)",
        "Create Index a On b (c, d|:d{}|)",
        %% ---------------------------------------------------------------------
        %% column_ref -> NAME     JSON
        %% column_ref -> NAME '.' NAME     JSON
        %% ---------------------------------------------------------------------
        "Select col|:a:b| From x",
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
        "Call Upper (All name|:_a1:f()|)",
        "Call Upper (All name|:_a1:f()|)",
        "Call Upper (All name1.name2|:_a1:f()|)",
        "Call Upper (All name)",
        "Call Upper (All name1.name2)",
        "Call Upper (All name1.name2.name3)",
        %% ---------------------------------------------------------------------
        %% Problem: data_type with parenteheses
        %% ---------------------------------------------------------------------
        %% data_type -> NAME '(' sgn_num ')'
        %% data_type -> NAME '(' sgn_num ',' sgn_num ')'
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
    ets:insert(?CODE_TEMPLATES, {Rule, Code}),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sql_list ::= sql ';' ( extra )? ( sql ';' ( extra )? )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sql_list = Rule) ->
    ?CREATE_CODE_START,
    [{extra, Extra}] = ets:lookup(?CODE_TEMPLATES, extra),
    Extra_Length = length(Extra),
    [{sql, Sql}] = ets:lookup(?CODE_TEMPLATES, sql),
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
    store_code(atom, [re:replace(re:replace(C, "string", "string_atom", [{return, list}]), "STRING", "STRING_ATOM", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(column, [re:replace(re:replace(C, "string", "string_column", [{return, list}]), "STRING", "STRING_COLUMN", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(data_type, [re:replace(re:replace(C, "string", "string_data_type", [{return, list}]), "STRING", "STRING_DATA_TYPE", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(from_column, [re:replace(re:replace(C, "string", "string_from_column", [{return, list}]), "STRING", "STRING_FROM_COLUMN", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(fun_arg, [re:replace(re:replace(C, "string", "string_fun_arg", [{return, list}]), "STRING", "STRING_FUN_ARG", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(literal, [re:replace(re:replace(C, "string", "string_literal", [{return, list}]), "STRING", "STRING_LITERAL", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(scalar_exp, [re:replace(re:replace(C, "string", "string_scalar_exp", [{return, list}]), "STRING", "STRING_SCALAR_EXP", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(scalar_sub_exp, [re:replace(re:replace(C, "string", "string_scalar_sub_exp", [{return, list}]), "STRING", "STRING_SCALAR_SUB_EXP", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(table, [re:replace(re:replace(C, "string", "string_table", [{return, list}]), "STRING", "STRING_TABLE", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    store_code(table_ref, [re:replace(re:replace(C, "string", "string_table_ref", [{return, list}]), "STRING", "STRING_TABLE_REF", [{return, list}]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% system_privilege ::=  'ADMIN'
%%                    |  'ALL' 'PRIVILEGES'
%%                    | ( ( 'ALTER' | 'CREATE' | 'DROP' ) 'ANY' ( 'INDEX' | ( 'MATERIALIZED' 'VIEW' ) | 'TABLE' | 'VIEW' ) )
%%                    | ( 'CREATE' ( ( 'MATERIALIZED' 'VIEW' ) | 'TABLE' | 'VIEW' ) )
%%                    | ( ( 'DELETE' | 'INSERT' | 'SELECT' | 'UPDATE' ) 'ANY' 'TABLE' )
%%                    | NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(system_privilege = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Admin",
            "Alter Any Index",
            "Alter Any Materialized View",
            "Alter Any Table",
            "Alter Any View",
            "Create Any Index",
            "Create Any Materialized View",
            "Create Any Table",
            "Create Any View",
            "Create Materialized View",
            "Create Table",
            "Create View",
            "Delete Any Table",
            "Drop Any Index",
            "Drop Any Materialized View",
            "Drop Any Table",
            "Drop Any View",
            "Insert Any Table",
            "Select Any Table",
            "Update Any Table"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% system_privilege_list ::= ( system_privilege ( ',' system_privilege )* )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(system_privilege_list = Rule) ->
    ?CREATE_CODE_START,
    [{system_privilege, System_Privilege}] = ets:lookup(?CODE_TEMPLATES, system_privilege),
    System_Privilege_Length = length(System_Privilege),

    Code =
        [
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
%% system_with_grant_option ::= 'WITH' ( 'ADMIN' | 'DELEGATE' ) 'OPTION'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(system_with_grant_option = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "With Admin Option",
            "With Delegate Option"
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
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{parameter, Parameter}] = ets:lookup(?CODE_TEMPLATES, parameter),
    Parameter_Length = length(Parameter),

    Code =
        [
            case rand:uniform(4) rem 4 of
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
                _ -> lists:append([
                    lists:nth(rand:uniform(Parameter_Length), Parameter),
                    " ",
                    lists:nth(rand:uniform(Name_Length), Name)
                ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(from_column, Code, ?MAX_BASIC, false),
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
    [{column_commalist, Column_Commalist}] = ets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
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
                            " (",
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
    [{column_ref_commalist, Column_Ref_Commalist}] = ets:lookup(?CODE_TEMPLATES, column_ref_commalist),
    Column_Ref_Commalist_Length = length(Column_Ref_Commalist),
    [{from_column, From_Column}] = ets:lookup(?CODE_TEMPLATES, from_column),
    From_Column_Length = length(From_Column),
    [{hierarchical_query_clause, Hierarchical_Query_Clause}] = ets:lookup(?CODE_TEMPLATES, hierarchical_query_clause),
    Hierarchical_Query_Clause_Length = length(Hierarchical_Query_Clause),
    [{order_by_clause, Order_By_Clause}] = ets:lookup(?CODE_TEMPLATES, order_by_clause),
    Order_By_Clause_Length = length(Order_By_Clause),
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),
    [{where_clause, Where_Clause}] = ets:lookup(?CODE_TEMPLATES, where_clause),
    Where_Clause_Length = length(Where_Clause),

    Code =
        [
            lists:append([
                "From ",
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
                end,
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
                        " Group By " ++ lists:nth(rand:uniform(Column_Ref_Commalist_Length), Column_Ref_Commalist);
                    _ -> []
                end,
                case rand:uniform(2) rem 2 of
                    1 ->
                        " Having " ++ lists:nth(rand:uniform(Search_Condition_Length), Search_Condition);
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
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
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
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{query_exp, Query_Exp}] = ets:lookup(?CODE_TEMPLATES, query_exp),
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
%% target_commalist ::= target ( ',' target )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(target_commalist = Rule) ->
    ?CREATE_CODE_START,
    [{target, Target}] = ets:lookup(?CODE_TEMPLATES, target),
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
    [{scalar_exp, Scalar_Exp}] = ets:lookup(?CODE_TEMPLATES, scalar_exp),
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
    store_code(predicate, Code, ?MAX_BASIC, false),
    store_code(search_condition, Code, ?MAX_BASIC, false),
%% wwe: currently not supported
%%    store_code(search_condition, [lists:append(["(", C, ")"]) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% truncate_table ::= 'TRUNCATE' 'TABLE' table_name ( ( 'PRESERVE' | 'PURGE' ) 'MATERIALIZED' 'VIEW' 'LOG' )? ( ( 'DROP' | 'REUSE' ) 'STORAGE' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(truncate_table = Rule) ->
    ?CREATE_CODE_START,
    [{table_name, Table_Name}] = ets:lookup(?CODE_TEMPLATES, table_name),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_SIMPLE, true),
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% update_statement_positioned ::= 'UPDATE' table 'SET' assignment_commalist 'WHERE' 'CURRENT' 'OF' cursor ( returning )?
%% update_statement_searched ::= 'UPDATE' table 'SET' assignment_commalist ( where_clause )? ( returning )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(update_statement = Rule) ->
    ?CREATE_CODE_START,
    [{assignment_commalist, Assignment_Commalist}] = ets:lookup(?CODE_TEMPLATES, assignment_commalist),
    Assignment_Commalist_Length = length(Assignment_Commalist),
    [{cursor, Cursor}] = ets:lookup(?CODE_TEMPLATES, cursor),
    Cursor_Length = length(Cursor),
    [{returning, Returning}] = ets:lookup(?CODE_TEMPLATES, returning),
    Returning_Length = length(Returning),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),
    [{where_clause, Where_Clause}] = ets:lookup(?CODE_TEMPLATES, where_clause),
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
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user_opt ::= ( ( 'DEFAULT' | 'TEMPORARY' ) 'TABLESPACE' NAME )
%%            | ( quota  ( quota )* )
%%            | ( 'PROFILE' NAME )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(user_opt = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{quota, Quota}] = ets:lookup(?CODE_TEMPLATES, quota),
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
    [{role_list, Role_List}] = ets:lookup(?CODE_TEMPLATES, role_list),
    Role_List_Length = length(Role_List),

    Code =
        [
            "Default Role All",
            "Default Role None"
        ] ++
        [
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
%% view_def ::= 'CREATE' 'VIEW' table ( '(' column_commalist ')' )? 'AS' query_spec ( 'WITH' 'CHECK' 'OPTION' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(view_def = Rule) ->
    ?CREATE_CODE_START,
    [{column_commalist, Column_Commalist}] = ets:lookup(?CODE_TEMPLATES, column_commalist),
    Column_Commalist_Length = length(Column_Commalist),
    [{query_spec, Query_Spec}] = ets:lookup(?CODE_TEMPLATES, query_spec),
    Query_Spec_Length = length(Query_Spec),
    [{table, Table}] = ets:lookup(?CODE_TEMPLATES, table),
    Table_Length = length(Table),

    Code =
        [
            lists:append([
                "Create View ",
                lists:nth(rand:uniform(Table_Length), Table),
                case rand:uniform(5) rem 5 of
                    1 -> [];
                    _ -> lists:append([
                        " (",
                        lists:nth(rand:uniform(Column_Commalist_Length), Column_Commalist),
                        ")"
                    ])
                end,
                " As ",
                lists:nth(rand:uniform(Query_Spec_Length), Query_Spec),
                case rand:uniform(2) rem 2 of
                    1 -> " With Check Option";
                    _ -> []
                end
            ])
            || _ <- lists:seq(1, ?MAX_STATEMENT_COMPLEX * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(manipulative_statement, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(schema_element, Code, ?MAX_STATEMENT_COMPLEX, true),
    store_code(sql, Code, ?MAX_STATEMENT_COMPLEX, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% when_action ::= ( 'GOTO' NAME )
%%               | 'CONTINUE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(when_action = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),

    Code =
        [
            "Continue"
        ] ++
        [
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
    [{when_action, When_Action}] = ets:lookup(?CODE_TEMPLATES, when_action),
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
    store_code(sql, Code, ?MAX_STATEMENT_SIMPLE, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% where_clause ::= 'WHERE' search_condition
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(where_clause = Rule) ->
    ?CREATE_CODE_START,
    [{search_condition, Search_Condition}] = ets:lookup(?CODE_TEMPLATES, search_condition),
    Search_Condition_Length = length(Search_Condition),

    Code =
        [
                "Where " ++ lists:nth(rand:uniform(Search_Condition_Length), Search_Condition)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating Common Test data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_Type, _CompleteSemicolon, _CompactedDetailed, []) ->
    ok;
file_create_ct_all(Type, CompleteSemicolon, CompactedDetailed, [Rule | Rules]) ->
    file_create_ct(Type, CompleteSemicolon, CompactedDetailed, Rule),
    file_create_ct_all(Type, CompleteSemicolon, CompactedDetailed, Rules).

file_create_ct(Type, CompleteSemicolon, CompactedDetailed, Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),

    CodeLength = length(Code),
    RuleString = atom_to_list(Rule),

    filelib:ensure_dir(?PATH_CT),

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

file_write_ct(_Current, _Type, _CompleteSemicolon, CompactedDetailed, File, []) ->
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

file_create_eunit_all(_Type, _CompleteSemicolon, []) ->
    ok;
file_create_eunit_all(Type, CompleteSemicolon, [Rule | Rules]) ->
    file_create_eunit(Type, CompleteSemicolon, Rule),
    file_create_eunit_all(Type, CompleteSemicolon, Rules).

file_create_eunit(Type, CompleteSemicolon, Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),

    RuleStrimg = atom_to_list(Rule),

    filelib:ensure_dir(?PATH_EUNIT),

    FileName = lists:append([Type, "_", CompleteSemicolon, "_", RuleStrimg, ".tst"]),
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

file_write_eunit(_CompleteSemicolon, File, []) ->
    file:close(File);
file_write_eunit(CompleteSemicolon, File, [H | T]) ->
    io:format(File, "~s~n", [lists:append([
        "\"",
        case CompleteSemicolon of
            "semicolon" ->
                H ++ ";";
            _ ->
                H
        end,
        "\"."
    ])]),
    file_write_eunit(CompleteSemicolon, File, T).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Store generated code in helper table.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_code(Rule, Code, Max, Strict) ->
    case ?LOGGING of
        true ->
            {total_heap_size, MSize} = erlang:process_info(whereis(code_server), total_heap_size),
            erlang:display(io:format("store total_heap   ===> ~12.. B rule: ~s ", [MSize, atom_to_list(Rule)])),
            erlang:display(io:format("store Code         ===> ~12.. B rule: ~s ", [length(Code), atom_to_list(Rule)]));
        _ -> ok
    end,

    case Max == 0 of
        true ->
            case ?LOGGING of
                true ->
                    erlang:display(io:format("store CodeNew      ===> ~12.. B rule: ~s ", [0, atom_to_list(Rule)]));
                _ -> ok
            end;
        _ ->
            CodeUnique = ordsets:to_list(ordsets:from_list(Code)),
            CodeUnique_Length = length(CodeUnique),
            CodeUniqueSorted = lists:sort(?F_RANDOM, CodeUnique),
            CodeUniqueLimited = case CodeUnique_Length > Max of
                                    true ->
                                        lists:sublist(CodeUniqueSorted, 1, Max);
                                    _ -> CodeUnique
                                end,
            CodeTotal = case ets:lookup(?CODE_TEMPLATES, Rule) of
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
            ets:insert(?CODE_TEMPLATES, {Rule, CodeNew}),
            case ?LOGGING of
                true ->
                    erlang:display(io:format("store CodeNew      ===> ~12.. B rule: ~s ", [length(CodeNew), atom_to_list(Rule)]));
                _ -> ok
            end
    end,

    case ?LOGGING of
        true ->
            erlang:display(io:format("store table size   ===> ~12.. B rule: ~s ", [ets:info(?CODE_TEMPLATES, memory), atom_to_list(Rule)]));
        _ -> ok
    end.
