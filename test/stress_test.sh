#!/bin/bash

exec > >(tee -i stress_test.log)
sleep .1

# ----------------------------------------------------------------------------
#
# stress_test.sh: SQL - stress testing.
#
# Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
# ----------------------------------------------------------------------------

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

# Setting sqlparse options ...............................................
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
