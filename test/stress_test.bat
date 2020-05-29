@ECHO OFF
REM ----------------------------------------------------------------------------
REM
REM stress_test.bat: SQL - stress testing.
REM
REM Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
REM
REM ----------------------------------------------------------------------------

Setlocal EnableDelayedExpansion

SET NO_RUNS=%1
IF "%1" == "" (
   SET NO_RUNS=1
)

> stress_test.log (

    ECHO =======================================================================
    ECHO Start %0
    ECHO -----------------------------------------------------------------------
    ECHO Start stress testing - in total %NO_RUNS%
    ECHO -----------------------------------------------------------------------
    ECHO:| TIME
    ECHO -----------------------------------------------------------------------

    IF EXIST _build\test\logs (
        RD /Q /S _build\test\logs
    )
    MD _build\test\logs
    IF EXIST tmp\backup (
        RD /Q /S tmp\backup
    )
    MD tmp\backup

    REM Setting sqlparse options ...............................................
    REM true: compacted / false: detailed.
    SET GENERATE_COMPACTED=true
    SET GENERATE_CT=true
    SET GENERATE_EUNIT=false
    SET GENERATE_PERFORMANCE=true
    SET GENERATE_RELIABILITY=false
    SET HEAP_SIZE=+hms 33554432
    SET LOGGING=false
    SET MAX_BASIC=250

    FOR /L %%G IN (1,1,%NO_RUNS%) DO (
       ECHO -----------------------------------------------------------------------
       ECHO !time! %%G. Step: gen_tests.bat
       CALL test\gen_tests.bat

       MD tmp\backup\%%G
       IF EXIST test\generated\ct (
          COPY test\generated\ct\*_SUITE.erl tmp\backup\%%G
       )
       IF EXIST test\generated\eunit (
          COPY test\generated\eunit\*_SUITE.erl tmp\backup\%%G
       )

       ECHO !time! %%G. Step: rebar3 ct
       CALL rebar3 ct
    )

    ECHO -----------------------------------------------------------------------
    ECHO:| TIME
    ECHO -----------------------------------------------------------------------
    ECHO End   %0
    ECHO =======================================================================

)
