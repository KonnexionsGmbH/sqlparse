%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: INTERSECT & MINUS & UNION & UNION ALL
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"(select column_1 from table_1)|:a| union (select column_2 from table_2)|:b|".
"select column_1 from table_1 intersect select column_2 from table_2 intersect select column_3 from table_3 intersect select column_4 from table_4".
"select column_1 from table_1 intersect select column_2 from table_2 intersect select column_3 from table_3".
"select column_1 from table_1 intersect select column_2 from table_2".
"select column_1 from table_1 minus select column_2 from table_2 minus select column_3 from table_3 minus select column_4 from table_4".
"select column_1 from table_1 minus select column_2 from table_2 minus select column_3 from table_3".
"select column_1 from table_1 minus select column_2 from table_2".
"select column_1 from table_1 union all select column_2 from table_2 union all select column_3 from table_3 union all select column_4 from table_4".
"select column_1 from table_1 union all select column_2 from table_2 union all select column_3 from table_3".
"select column_1 from table_1 union all select column_2 from table_2".
"select column_1 from table_1 union select column_2 from table_2 union select column_3 from table_3 union select column_4 from table_4".
"select column_1 from table_1 union select column_2 from table_2 union select column_3 from table_3".
"select column_1 from table_1 union select column_2 from table_2".

%% -----------------------------------------------------------------------------
%% TESTS: INTERSECT & MINUS & UNION & UNION ALL
%% =============================================================================
