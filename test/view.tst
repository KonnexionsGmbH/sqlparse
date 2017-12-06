%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%%
%% TESTS
%%

"Create View table_1                      As Select * From dual                  ".
"Create View table_1                      As Select * From dual With Check Option".
"Create View table_1 (column_1, column_2) As Select * From dual                  ".
"Create View table_1 (column_1, column_2) As Select * From dual With Check Option".
