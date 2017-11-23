@ECHO off
REM ----------------------------------------------------------------------------
REM
REM gen_tests_and_run.bat: SQL - generate and run test data.
REM
REM Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
REM
REM This file is provided to you under the Apache License,
REM Version 2.0 (the "License"); you may not use this file
REM except in compliance with the License.  You may obtain
REM a copy of the License at
REM
REM   http://www.apache.org/licenses/LICENSE-2.0
REM
REM Unless required by applicable law or agreed to in writing,
REM software distributed under the License is distributed on an
REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
REM KIND, either express or implied.  See the License for the
REM specific language governing permissions and limitations
REM under the License.
REM
REM ----------------------------------------------------------------------------

> gen_tests_and_run.log (

    SETLOCAL enableDelayedExpansion
    ECHO %time% Start Test Data Generation and Run

    REM Setting sqlparse options ...............................................
    REM true: compacted / false: detailed.
    SET GENERATE_COMPACTED=true
    SET GENERATE_CT=true
    SET GENERATE_EUNIT=false
    SET GENERATE_PERFORMANCE=true
    SET GENERATE_RELIABILITY=true
    SET HEAP_SIZE=+hms 100663296
    SET LOGGING=false
    SET MAX_BASIC=250
    CALL test\gen_tests

    ECHO %time% Start EUnit Tests
    SET SOURCEFILES_OLD=SOURCEFILES
    SET SOURCEFILES=
    CALL rebar3 eunit

    SET SOURCEFILES=SOURCEFILES_OLD
    ECHO %time% Start Common Tests
    CALL rebar3 ct

    ECHO %time% Start Coverage Analysis
    CALL rebar3 cover

    ECHO %time% Start Dialyzer
    CALL rebar3 dialyzer

    ECHO %time% End   Test Data Generation and Run

)
