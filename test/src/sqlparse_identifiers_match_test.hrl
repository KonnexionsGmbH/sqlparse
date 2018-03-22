%% -----------------------------------------------------------------------------
%%
%% sqlparse_identifiers_match_test.hrl: SQL - test driver.
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

-ifndef(SQLPARSE_IDENTIFIERS_MATCH_HRL).
-define(SQLPARSE_IDENTIFIERS_MATCH_HRL, true).

-include_lib("eunit/include/eunit.hrl").
-include("sqlparse.hrl").

%%------------------------------------------------------------------------------
%% Special fields.
%%------------------------------------------------------------------------------

-define(IN_FIELDS, [
    <<"rownum">>,
    <<"systimestamp">>,
    <<"user">>,
    <<"username">>,
    <<"sysdate">>,
    <<"schema">>,
    <<"node">>
]).

%%------------------------------------------------------------------------------
%% TEST 01.
%%------------------------------------------------------------------------------

-define(TEST_01, "
select name, d.lastLoginTime
from ddAccountDyn d, ddAccount a
where d.lastLoginTime >= sysdate - 1.1574074074074073e-5
and d.id = a.id").

-define(TEST_01_RESULT, [
    <<"sysdate">>
]).

%%------------------------------------------------------------------------------
%% TEST 02.
%%------------------------------------------------------------------------------

-define(TEST_02, "
select * from ddTable where rownum <= 10").

-define(TEST_02_RESULT, [
    <<"rownum">>
]).

%%------------------------------------------------------------------------------
%% TEST 03.
%%------------------------------------------------------------------------------

-define(TEST_03, "
select * from ddTable where 10 > rownum").

-define(TEST_03_RESULT, [
    <<"rownum">>
]).

%%------------------------------------------------------------------------------
%% TEST 04.
%%------------------------------------------------------------------------------

-define(TEST_04, "
select * from ddTable where rownum <= user and rownum > username").

-define(TEST_04_RESULT, [
    <<"rownum">>,
    <<"user">>,
    <<"username">>
]).

%%------------------------------------------------------------------------------
%% TEST 05.
%%------------------------------------------------------------------------------

-define(TEST_05, "
select * from ddTable where user < rownum and username > rownum").

-define(TEST_05_RESULT, [
    <<"rownum">>,
    <<"user">>,
    <<"username">>
]).

%%------------------------------------------------------------------------------
%% TEST 06.
%%------------------------------------------------------------------------------

-define(TEST_06, "
select * from ddTable where sysdate > rownum").

-define(TEST_06_RESULT, [
    <<"rownum">>,
    <<"sysdate">>
]).

%%------------------------------------------------------------------------------
%% TEST 07.
%%------------------------------------------------------------------------------

-define(TEST_07, "
select * from ddTable where rownum and rownum > username").

-define(TEST_07_RESULT, [
    <<"rownum">>,
    <<"username">>
]).

%%------------------------------------------------------------------------------
%% TEST 08.
%%------------------------------------------------------------------------------

-define(TEST_08, "
select * from ddTable where rownum <= user and username").

-define(TEST_08_RESULT, [
    <<"rownum">>,
    <<"user">>,
    <<"username">>
]).

%%------------------------------------------------------------------------------
%% TEST 09.
%%------------------------------------------------------------------------------

-define(TEST_09, "
select * from ddTable where rownumx and rownum > username").

-define(TEST_09_RESULT, [
    <<"rownum">>,
    <<"username">>
]).

%%------------------------------------------------------------------------------
%% TEST 10.
%%------------------------------------------------------------------------------

-define(TEST_10, "
select * from ddTable where rownum <= user and usernamex").

-define(TEST_10_RESULT, [
    <<"rownum">>,
    <<"user">>
]).

%%------------------------------------------------------------------------------
%% TEST 11.
%%------------------------------------------------------------------------------

-define(TEST_11, "
select rownum + 1  as RowNumPlus
from def
where col2 <= 5;").

-define(TEST_11_RESULT, [
    <<"rownum">>
]).

%%------------------------------------------------------------------------------
%% TEST 12.
%%------------------------------------------------------------------------------

-define(TEST_12, "
select rownum  as RowNumPlus
from def
where col2 <= 5;").

-define(TEST_12_RESULT, [
    <<"rownum">>
]).

%%------------------------------------------------------------------------------
%% TEST 13.
%%------------------------------------------------------------------------------

-define(TEST_13, "
select rownum
from def
where col2 <= 5;").

-define(TEST_13_RESULT, [
    <<"rownum">>
]).

%%------------------------------------------------------------------------------
%% TEST 14.
%%------------------------------------------------------------------------------

-define(TEST_14, "
select username,user,systimestamp,sysdate,rownum,node
from def
where col2 <= 5;").

-define(TEST_14_RESULT, [
    <<"node">>,
    <<"rownum">>,
    <<"sysdate">>,
    <<"systimestamp">>,
    <<"user">>,
    <<"username">>
]).

%%------------------------------------------------------------------------------
%% TEST 15.
%%------------------------------------------------------------------------------

-define(TEST_15, "
select rownumx
from def
where col2 <= 5;").

-define(TEST_15_RESULT, [
]).

%%------------------------------------------------------------------------------
%% TEST 16.
%%------------------------------------------------------------------------------

-define(TEST_16, "
create table table_1 (column_1 date default sysdate)").

-define(TEST_16_RESULT, [
    <<"sysdate">>
]).

%%------------------------------------------------------------------------------
%% TEST 17.
%%------------------------------------------------------------------------------

-define(TEST_17, "
select node
from dual
where rownum <= sysdate
group by systimestamp
having user is null
order by username").

-define(TEST_17_RESULT, [
    <<"node">>,
    <<"rownum">>,
    <<"sysdate">>,
    <<"systimestamp">>,
    <<"user">>,
    <<"username">>
]).

-endif.
