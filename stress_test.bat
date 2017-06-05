@ECHO OFF
rem ----------------------------------------------------------------------------
rem
rem stress_test.bat: SQL - stress testing.
rem
rem Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
rem
rem This file is provided to you under the Apache License,
rem Version 2.0 (the "License"); you may not use this file
rem except in compliance with the License.  You may obtain
rem a copy of the License at
rem
rem   http://www.apache.org/licenses/LICENSE-2.0
rem
rem Unless required by applicable law or agreed to in writing,
rem software distributed under the License is distributed on an
rem "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
rem KIND, either express or implied.  See the License for the
rem specific language governing permissions and limitations
rem under the License.
rem
rem ----------------------------------------------------------------------------

call rebar3 compile

Setlocal EnableDelayedExpansion

ECHO ============================================================================
ECHO !TIME! Start run

SET no_runs=%1
IF "%1" == "" (
   SET no_runs=1
)

RD ct\logs /Q /S
MD ct\logs

FOR /L %%G IN (1,1,%no_runs%) DO (
   ECHO ----------------------------------------------------------------------------
   ECHO !TIME! %%G. Step: gen_tests.bat
   CALL gen_tests.bat
   del test\reliability_*_SUITE.erl
   ECHO !TIME! %%G. Step: rebar3 ct
   rmdir /q /s _build\test
   CALL rebar3.cmd ct
)

ECHO ----------------------------------------------------------------------------
ECHO !TIME! End   run
ECHO ============================================================================
