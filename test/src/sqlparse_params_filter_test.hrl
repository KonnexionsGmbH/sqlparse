%% -----------------------------------------------------------------------------
%%
%% sqlparse_params_filter_test.hrl: SQL - test driver.
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
%% TEST 01.
%%------------------------------------------------------------------------------

-define(TEST_01, "
select :param_1 name, :param_2
from ddAccountDyn d, ddAccount a
where d.lastLoginTime >= sysdate - 1.1574074074074073e-5
and d.id = a.id
having :param_4 = :param_6
order by :param_7,:param_8 asc").

-define(TEST_01_RESULT, [
    <<":param_1">>,
    <<":param_2">>,
    <<":param_4">>,
    <<":param_6">>,
    <<":param_7">>,
    <<":param_8">>
]).

%%------------------------------------------------------------------------------
%% TEST 02.
%%------------------------------------------------------------------------------

-define(TEST_02, "
select :param_5 name, :param_4
from :param_1 d, :param_2
where d.lastLoginTime >= :param_3
and :param_6 = :param_7
and :param_8 = sysdate").

-define(TEST_02_RESULT, [
    <<":param_1">>,
    <<":param_2">>,
    <<":param_3">>,
    <<":param_4">>,
    <<":param_5">>,
    <<":param_6">>,
    <<":param_7">>,
    <<":param_8">>
]).

%%------------------------------------------------------------------------------
%% TEST 03.
%%------------------------------------------------------------------------------

-define(TEST_03, "
select param_1 name, param_2
from ddAccountDyn d, ddAccount a
where d.lastLoginTime >= sysdate - 1.1574074074074073e-5
and d.id = a.id").

-define(TEST_03_RESULT, [
]).

-endif.
