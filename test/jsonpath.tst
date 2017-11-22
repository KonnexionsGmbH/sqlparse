%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%%
%% TESTS
%%

"select a|:| from x".

"select a|:b| from x".
"select a|::b| from x".
"select a|[]| from x".
"select a|{}| from x".
"select a|:f()| from x".
"select a|:b[f(p:q)]| from x".
"select a.g|:b.f[f(p.r:q)]| from x".
"select a.g|:b.f\n[\tf(p.r:q)]| from x".

"select col1|:x| from dual".
"select col1|:x:y| from dual".
"select col1|:x:y| from dual".
"select col1.col2|:x:y| from dual".
"select col1.col2.col3|:x:y| from dual".

"select column_1|:a_obj:x|,column_2|:b_obj:y| from x where column_3|:a_obj:x| = 1 and column_4|:b_obj:y| = 0 and column_5|:rw_obj1:a| > 0".

"SELECT column~1|:x:y|, column~2|[]| FROM table_~_1, table_0000_2 WHERE column_3 IS NULL".
