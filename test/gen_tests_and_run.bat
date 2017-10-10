@ECHO off
rem ----------------------------------------------------------------------------
rem
rem gen_tests_and_run.bat: SQL - generate and run test data.
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

> gen_tests_and_run.log (

    CALL gen_tests.bat

    SET SOURCEFILES_OLD=SOURCEFILES
    SET SOURCEFILES=
    CALL rebar3 eunit
    SET SOURCEFILES=SOURCEFILES_OLD
    ECHO !DATE!_!TIME!
    CALL rebar3 ct
    ECHO !DATE!_!TIME!
    CALL rebar3 cover
    ECHO !DATE!_!TIME!
    CALL rebar3 dialyzer
    ECHO !DATE!_!TIME!

)
