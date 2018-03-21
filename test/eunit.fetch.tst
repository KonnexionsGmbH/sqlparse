%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: FETCH
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fetch_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"fetch cursor_1 into :param_1".
"fetch cursor_1 into :param_1,:param_2".
"fetch cursor_1 into :param_1,:param_2,:param_3".
"fetch cursor_1 into :param_1_a :param_1_b".
"fetch cursor_1 into :param_1_a :param_1_b,:param_2_a :param_2_b".
"fetch cursor_1 into :param_1_a :param_1_b,:param_2_a :param_2_b,:param_3_a :param_3_b".
"fetch cursor_1 into :param_1_a indicator :param_1_b".
"fetch cursor_1 into :param_1_a indicator :param_1_b,:param_2_a indicator :param_2_b".
"fetch cursor_1 into :param_1_a indicator :param_1_b,:param_2_a indicator :param_2_b,:param_3_a indicator :param_3_b".
"fetch cursor_1 into column_1".
"fetch cursor_1 into column_1,column_2".
"fetch cursor_1 into column_1,column_2,column_3".
"FETCH name_cursor_1 INTO name_column_1".

%% -----------------------------------------------------------------------------
%% TESTS: FETCH
%% =============================================================================
