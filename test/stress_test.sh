#!/bin/bash

exec > >(tee -i stress_test.log)
sleep .1

# ------------------------------------------------------------------------------
#
# stress_test.sh: SQL - stress testing.
#
# Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
#
# ------------------------------------------------------------------------------

NO_RUNS=$1
if [ "$#" -ne 1 ]; then
    NO_RUNS=1
fi

echo "========================================================================="
echo "Start $0"
echo "-------------------------------------------------------------------------"
echo "Start stress testing - in total $NO_RUNS"
echo "-------------------------------------------------------------------------"
date +"DATE TIME : %d.%m.%Y %H:%M:%S"
echo "-------------------------------------------------------------------------"

if [ -d "_build/test/logs" ]; then
    rm -rf _build/test/logs
fi
mkdir  _build/test/logs
if [ -d "tmp/backup" ]; then
    rm -rf tmp/backup
fi
mkdir  tmp/backup

# Setting sqlparse options .....................................................
# true: compacted / false: detailed.
export GENERATE_COMPACTED="true"
export GENERATE_CT="true"
export GENERATE_EUNIT="false"
export GENERATE_PERFORMANCE="true"
export GENERATE_RELIABILITY="false"
export HEAP_SIZE="+hms 33554432"
export LOGGING="false"
export MAX_BASIC=250

for i in $(seq 1 $NO_RUNS)
do
   echo "-----------------------------------------------------------------------"
   echo "$(timestamp) $i. Step: gen_tests.bat"
   test/gen_tests.sh

   mkdir tmp/backup/$i
   cp test/generated/*/*_SUITE.erl tmp/backup/$i

   echo "$(timestamp) $i. Step: rebar3 ct"
   rebar3 ct
done

echo "-------------------------------------------------------------------------"
date +"DATE TIME : %d.%m.%Y %H:%M:%S"
echo "-------------------------------------------------------------------------"
echo "End   $0"
echo "========================================================================="

exit 0
