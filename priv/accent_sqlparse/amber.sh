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
