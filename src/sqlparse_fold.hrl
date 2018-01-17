%% -----------------------------------------------------------------------------
%%
%% sqlparse_fold.hrl: SQL - utilities.
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

-ifndef(SQLPARSE_FOLD_HRL).
-define(SQLPARSE_FOLD_HRL, true).

%% Allowed values: init_cap, keep_unchanged, lower,upper -----------------------
-define(CASE_IDENTIFIER, list_to_atom(
    string:to_lower(os:getenv("CASE_IDENTIFIER", "keep_unchanged")))).
%% Allowed values: init_cap, lower,upper ---------------------------------------
-define(CASE_KEYWORD, list_to_atom(
    string:to_lower(os:getenv("CASE_KEYWORD", "upper")))).

-define(CHAR_NEWLINE, case os:type() of
                          {unix, _} -> "\n";
                          _ -> "\r\n"
                      end).
-define(CHAR_NEWLINE_1, case os:type() of
                            {unix, _} -> "\n";
                            _ -> "\r"
                        end).
-define(CHAR_TAB, "\t").

%% Allowed values: 1 ... n -----------------------------------------------------
-define(CR_LIMIT_ALTER_ROLES, list_to_integer(
    os:getenv("CR_LIMIT_ALTER_ROLES", "3"))).
-define(CR_LIMIT_ALTER_USERS, list_to_integer(
    os:getenv("CR_LIMIT_ALTER_USERS", "3"))).
-define(CR_LIMIT_CREATE_INDEX, list_to_integer(
    os:getenv("CR_LIMIT_CREATE_INDEX", "3"))).
-define(CR_LIMIT_DROP_TABLE, list_to_integer(
    os:getenv("CR_LIMIT_DROP_TABLE", "3"))).
-define(CR_LIMIT_FUNC_ARGS, list_to_integer(
    os:getenv("CR_LIMIT_FUNC_ARGS", "3"))).
-define(CR_LIMIT_GRANT_GRANTEE, list_to_integer(
    os:getenv("CR_LIMIT_GRANT_GRANTEE", "3"))).
-define(CR_LIMIT_GRANT_PRIVILEGE, list_to_integer(
    os:getenv("CR_LIMIT_GRANT_PRIVILEGE", "3"))).
-define(CR_LIMIT_GROUP_BY, list_to_integer(
    os:getenv("CR_LIMIT_GROUP_BY", "3"))).
-define(CR_LIMIT_INSERT, list_to_integer(os:getenv("CR_LIMIT_INSERT", "3"))).
-define(CR_LIMIT_INTO, list_to_integer(os:getenv("CR_LIMIT_INTO", "3"))).
-define(CR_LIMIT_ORDER_BY, list_to_integer(
    os:getenv("CR_LIMIT_ORDER_BY", "3"))).
-define(CR_LIMIT_PARTITION, list_to_integer(
    os:getenv("CR_LIMIT_PARTITION", "3"))).
-define(CR_LIMIT_RETURNING, list_to_integer(
    os:getenv("CR_LIMIT_RETURNING", "3"))).
-define(CR_LIMIT_REVOKE_PRIVILEGE, list_to_integer(
    os:getenv("CR_LIMIT_REVOKE_PRIVILEGE", "3"))).
-define(CR_LIMIT_REVOKE_REVOKEE, list_to_integer(
    os:getenv("CR_LIMIT_REVOKE_REVOKEE", "3"))).
-define(CR_LIMIT_SELECT, list_to_integer(os:getenv("CR_LIMIT_SELECT", "3"))).
-define(CR_LIMIT_USING, list_to_integer(os:getenv("CR_LIMIT_USING", "3"))).
-define(CR_LIMIT_VIEW, list_to_integer(os:getenv("CR_LIMIT_VIEW", "3"))).

-define(DATA_TYPES, [
    "bfile",
    "binary_double",
    "binary_float",
    "blob",
    "clob",
    "char",
    "date",
    "integer",
    "long",
    "nchar",
    "nclob",
    "number",
    "nvarchar2",
    "raw",
    "rowid",
    "timestamp",
    "urowid",
    "varchar2"
]).

%% Allowed values: 1 - 8 -------------------------------------------------------
-define(INDENT_SPACES, list_to_integer(os:getenv("INDENT_SPACES", "4"))).
%% Allowed values: spaces, tab -------------------------------------------------
-define(INDENT_WITH, list_to_atom(
    string:to_lower(os:getenv("INDENT_WITH", "spaces")))).

-define(OBJECT_PRIVILEGES, [
    "all",
    "all privileges",
    "alter",
    "delete",
    "execute",
    "index",
    "insert",
    "references",
    "select",
    "update"
]).

-define(SYSTEM_PRIVILEGES, [
    "admin",
    "alter any index",
    "alter any materialized view",
    "alter any table",
    "alter any view",
    "create any index",
    "create any materialized view",
    "create any table",
    "create any view",
    "create materialized view",
    "create table",
    "create view",
    "delete any table",
    "drop any index",
    "drop any materialized view",
    "drop any table",
    "drop any view",
    "insert any table",
    "select any table",
    "update any table"
]).

-define(TABLE_OPTIONS, [
    "bag",
    "cluster",
    "local",
    "ordered_set",
    "schema",
    "set"
]).

%% Allowed values: false, true -------------------------------------------------
-define(WS_OPERATORS, list_to_atom(
    string:to_lower(os:getenv("WS_OPERATORS", "true")))).

-record(state, {
    function_level = 0,
    indentation_level = 1,
    select_clause = none,
    statement
}).

-endif.
