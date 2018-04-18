%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: LEXX
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% W = \s\r\n
% A = [A-Za-z0-9,_{W}]
% OS = (.*|[{W}]*)
%
% (fun[{W}]*\({A}*\){OS}*\->{OS}*end\.)               : match_fun(TokenLine, TokenChars).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select fun()->end. from dual".
"select fun( )a->end. from dual".
"select fun()->bend. from dual".
"select fun( )a->bend. from dual".

"select fun(myArg_1)->end. from dual".
"select fun(myArg_1)a->end. from dual".
"select fun(myArg_1)->bend. from dual".
"select fun(myArg_1)a->b end. from dual".

"select fun(myArg_1, myArg_2)a->b end. from dual".

"select fun () -> end. from dual".
"select fun ( ) a -> end. from dual".
"select fun () -> b end. from dual".
"select fun ( ) a -> b end. from dual".

"select fun ( myArg_1 ) -> end. from dual".
"select fun ( myArg_1 ) a -> end. from dual".
"select fun ( myArg_1 ) -> b end. from dual".
"select fun ( myArg_1 ) a -> b end. from dual".

"
select fun
(myArg_1)-> end.
from dual
".

"
select fun(myArg_1)
-> end.
from dual
".

"
select fun(myArg_1)->
end.
from dual
".

"
select
fun
(
myArg_1
)
a
->
b
end.
from dual
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% W = \s\r\n
%
% (fun[{W}]+['A-Za-z0-9_]+:['A-Za-z0-9_]+\/[0-9]+\.)  : {token, {'STRING', TokenLine, TokenChars}}.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"select fun aB_1:cD_2/5. from dual".
"select fun 'aB_1:'cD_2/5. from dual".

"
select
 fun
 aB_1:cD_2/5.
from dual
".

%% -----------------------------------------------------------------------------
%% TESTS: LEXX
%% =============================================================================
