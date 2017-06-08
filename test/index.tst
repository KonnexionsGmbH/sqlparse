%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%%
%% TESTS
%%

"create index on tab".

"CREATE BITMAP INDEX ON tab".

"create unique index s.a on s.d (f)".
"create bitmap index s.a on s.d (f)".
"create keylist index s.a on s.d (f)".
"create hashmap index s.a on s.d (f)".
"create index a on b (a:d)".
"create index a on b (a:d|e:f)".
"create index a on b (f) norm_with fun() -> norm end.".
"create index a on b (a | |.d{}|) norm_with fun() -> norm end. filter_with fun mod:modfun/5.".
"create index name_sort on skvhACCOUNT (cvalue:NAME) norm_with fun imem_index:vnf_lcase_ascii/1. filter_with fun imem_index:iff_binterm_list_1/1.".


"drop index s.a from s.b".
"drop index from s.b".
