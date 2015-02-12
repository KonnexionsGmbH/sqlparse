%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"select a:b from x".
"select a::b from x".
"select a[] from x".
"select a {} from x".
"select a:f() from x".
"select a:b[f(p:q)] from x".
"select a.g:b.f[f(p.r:q)] from x".
"select a.g:b.f\n[\tf(p.r:q)] from x".
