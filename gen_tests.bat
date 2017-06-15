@echo off
rem ----------------------------------------------------------------------------
rem
rem gen_tests.bat: SQL - generating test data.
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

> gen_tests.log (

    setlocal enableDelayedExpansion
    echo !DATE!_!TIME!
    call rebar3 compile
    erl -noshell -pa _build\default\lib\sqlparse\ebin -s sqlparse_generator generate -s init stop
    echo !DATE!_!TIME!

)
