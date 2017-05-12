%{
#include "yygrammar.h"
%}
%%
(\|\|)                                  { return OPERATOR_CONCAT; }
(div)                                   { return OPERATOR_INTDIV; }
(fun\([A-Za-z0-9,_]*\).*\->.*end\.)     { return STRING; }
(fun\s['A-Za-z0-9_]+:['A-Za-z0-9_]+\/[0-9]+\.) {
                                          return STRING; }
(\'([^\']*(\'\')*)*\')                  { return STRING; }
(\"((\$|[^\"]*)*(\"\")*)*\")            { return NAME; }
((\/\*)[^\*\/]*(\*\/))                  { return HINT; }
([A-Za-z0-9_\.]+([:#\[\{]+|([\s\t\n\r]*[#\[\{]+))[A-Za-z0-9_\.\:\(\)\[\]\{\}\#\,\|\-\+\*\/\\%\s\t\n\r]*) {
                                          return JSON; }
(:=|=|<>|<|>|<=|>=)                     { return COMPARISON; }
[A-Za-z][A-Za-z0-9_@:#\$]*              { return NAME; }
(\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)       { return PARAMETER; }
([0-9]+)                                { return INTNUM; }
((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))[eE]?[+-]?[0-9]*[fFdD]?)  {
                                          return APPROXNUM; }
((\-\-).*[\n])                          { /* COMMENT */ }
.                                       { yyerror("illegal token"); }

