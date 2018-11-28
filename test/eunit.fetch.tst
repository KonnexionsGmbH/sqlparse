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

"begin fetch cursor_1 into :param_1;end;".
"begin fetch cursor_1 into :param_1,:param_2;end;".
"begin fetch cursor_1 into :param_1,:param_2,:param_3;end;".
"begin fetch cursor_1 into :param_1_a :param_1_b;end;".
"begin fetch cursor_1 into :param_1_a :param_1_b,:param_2_a :param_2_b;end;".
"begin fetch cursor_1 into :param_1_a :param_1_b,:param_2_a :param_2_b,:param_3_a :param_3_b;end;".
"begin fetch cursor_1 into :param_1_a indicator :param_1_b;end;".
"begin fetch cursor_1 into :param_1_a indicator :param_1_b,:param_2_a indicator :param_2_b;end;".
"begin fetch cursor_1 into :param_1_a indicator :param_1_b,:param_2_a indicator :param_2_b,:param_3_a indicator :param_3_b;end;".
"begin fetch cursor_1 into column_1;end;".
"begin fetch cursor_1 into column_1,column_2;end;".
"begin fetch cursor_1 into column_1,column_2,column_3;end;".
"begin fetch name_cursor_1 INTO name_column_1;end;".

%% -----------------------------------------------------------------------------
%% TESTS: FETCH
%% =============================================================================
