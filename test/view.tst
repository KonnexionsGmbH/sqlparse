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

% table reference -------------------------------------------------------------

"Create View table_1 As Select * From dual".
"Create View schema_1.table_1 As Select * From dual".
"Create View :param_1 As Select * From dual".
"Create View \"^&()\" As Select * From dual".
