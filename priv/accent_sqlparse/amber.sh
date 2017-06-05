# ----------------------------------------------------------------------------
#
# amber.sh: SQL - checking grammar ambiguity with ACCENT and AMBER.
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

#!/bin/bash

rm -f amber.log
exec >  >(tee -ia amber.log)
exec 2> >(tee -ia amber.log >&2)

# ------------------------------------------------------------------------------
# accent
# ------------------------------------------------------------------------------

ACCENT=../accent/accent
ENTIRE=../entire/entire.c
LEX=flex
CC=cc

$ACCENT sqlparse.acc

$LEX sqlparse.lex

$CC -o sqlparse yygrammar.c lex.yy.c auxil.c $ENTIRE

# ------------------------------------------------------------------------------
# amber
# ------------------------------------------------------------------------------

ACCENTHOME=..
ACCENT=$ACCENTHOME/accent/accent
AMBER=$ACCENTHOME/amber/amber.c

set -e
set -x

$ACCENT sqlparse.acc

cc -o amber -O3 yygrammar.c $AMBER

./amber ellipsis examples 50000000 silent
