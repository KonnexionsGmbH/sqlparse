/*
%% -----------------------------------------------------------------------------
%%
%% sqlparse.lex: SQL - lexer definition for ACCENT.
%%
%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------
*/

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

/* -------------------------------------------------------------------------- */
