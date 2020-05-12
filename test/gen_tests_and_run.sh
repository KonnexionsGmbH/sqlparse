#!/bin/bash

exec > >(tee -i gen_tests_and_run.log)
sleep .1

# ----------------------------------------------------------------------------
#
# gen_tests_and_run.sh: SQL - generate and run test data.
#
# Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
#
# ----------------------------------------------------------------------------

echo "========================================================================="
echo "Start $0"
echo "-------------------------------------------------------------------------"
echo "Start Test Data Generation and Run"
echo "-------------------------------------------------------------------------"
date +"DATE TIME : %d.%m.%Y %H:%M:%S"
echo "-------------------------------------------------------------------------"

# Setting sqlparse options ...............................................
# "true": compacted / "false": detailed.
export GENERATE_COMPACTED="true"
export GENERATE_CT="true"
export GENERATE_EUNIT="false"
export GENERATE_PERFORMANCE="true"
export GENERATE_RELIABILITY="true"
export HEAP_SIZE="+hms 33554432"
export LOGGING="false"
export MAX_BASIC=250
test/gen_tests.sh

echo "Start EUnit Tests"
SOURCEFILES_OLD=SOURCEFILES
SOURCEFILES=
rebar3 eunit
SOURCEFILES=SOURCEFILES_OLD

echo "Start Common Tests"
rebar3 ct

echo "Start Coverage Analysis"
rebar3 cover

echo "Start Dialyzer"
rebar3 dialyzer

echo "Start geas (Guess Erlang Application Scattering)"
rebar3 as test geas

echo "-------------------------------------------------------------------------"
date +"DATE TIME : %d.%m.%Y %H:%M:%S"
echo "-------------------------------------------------------------------------"
echo "End   $0"
echo "========================================================================="

exit 0
