%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: VIEW
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"create view :param_1 as select * from dual with check option".
"create view :param_1 as select * from dual".
"Create View :param_1 As Select * From dual".
"create view :param_1(column_1)as select * from dual with check option".
"create view :param_1(column_1)as select * from dual".
"create view :param_1(column_1,column_2,column_3)as select * from dual with check option".
"create view :param_1(column_1,column_2,column_3)as select * from dual".
"Create View \"^&()\" As Select * From dual".
"Create View schema_1.table_1 As Select * From dual".
"Create View table_1                      As Select * From dual                  ".
"Create View table_1                      As Select * From dual With Check Option".
"Create View table_1 (column_1, column_2) As Select * From dual                  ".
"Create View table_1 (column_1, column_2) As Select * From dual With Check Option".
"create view table_1 as select * from dual with check option".
"Create View table_1 As Select * From dual".
"create view table_1(column_1)as select * from dual with check option".
"create view table_1(column_1)as select * from dual".
"create view table_1(column_1,column_2,column_3)as select * from dual with check option".
"create view table_1(column_1,column_2,column_3)as select * from dual".

%% -----------------------------------------------------------------------------
%% TESTS: VIEW
%% =============================================================================
