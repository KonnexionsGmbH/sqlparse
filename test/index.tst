%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"create bitmap index s.a on s.d (f)".
"create index a on b (a:d)".
"create index a on b (a:d|e:f)".
"create index a on b (f) norm_with fun() -> norm end.".
"create index a on b (a|d{}) norm_with fun() -> norm end. filter_with fun mod:modfun/5.".
