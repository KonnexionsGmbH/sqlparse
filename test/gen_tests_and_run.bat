@ECHO off
REM ----------------------------------------------------------------------------
REM
REM gen_tests_and_run.bat: SQL - generate and run test data.
REM
REM Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
REM
REM ----------------------------------------------------------------------------

SETLOCAL enableDelayedExpansion

> gen_tests_and_run.log (

    ECHO =======================================================================
    ECHO Start %0
    ECHO -----------------------------------------------------------------------
    ECHO Start Test Data Generation and Run
    ECHO -----------------------------------------------------------------------
    ECHO:| TIME
    ECHO -----------------------------------------------------------------------

    REM Setting sqlparse options ...............................................
    REM true: compacted / false: detailed.
    SET GENERATE_COMPACTED=true
    SET GENERATE_CT=true
    SET GENERATE_EUNIT=false
    SET GENERATE_PERFORMANCE=true
    SET GENERATE_RELIABILITY=true
    SET HEAP_SIZE=+hms 33554432
    SET LOGGING=false
    SET MAX_BASIC=250
    CALL test\gen_tests

    ECHO !time! Start EUnit Tests
    SET SOURCEFILES_OLD=SOURCEFILES
    SET SOURCEFILES=
    CALL rebar3 eunit

    SET SOURCEFILES=SOURCEFILES_OLD
    ECHO !time! Start Common Tests
    CALL rebar3 ct

    ECHO !time! Start Coverage Analysis
    CALL rebar3 cover

    ECHO !time! Start Dialyzer
    CALL rebar3 dialyzer

    ECHO !time! geas (Guess Erlang Application Scattering)
    CALL rebar3 as test geas

    ECHO -----------------------------------------------------------------------
    ECHO:| TIME
    ECHO -----------------------------------------------------------------------
    ECHO End   %0
    ECHO =======================================================================

)
