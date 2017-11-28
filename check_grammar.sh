#!/bin/bash

exec > >(tee -i check_grammar.log)
sleep .1

# ----------------------------------------------------------------------------
#
# check_grammar.sh: SQL - checking grammar definition with BNFC.
#
# Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
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

timestamp() {
  date +"%T"
}

echo "============================================================================"
echo "$(timestamp) Start run"
echo "----------------------------------------------------------------------------"

if [ -d "tmp" ]; then
   rm tmp/*
fi

bnfc -o tmp --haskell priv/bnf_converter/sqlparse.cf

happy -i tmp/ParSqlparse.y

echo "----------------------------------------------------------------------------"
echo "$(timestamp) End   run"
echo "============================================================================"

exit 0
