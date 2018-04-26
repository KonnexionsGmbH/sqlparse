%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

% ==============================================================================

"
SELECT *
FROM TABLE(DBSS.IO.FILE_READ_LOG (
:SQLT_STR_DIRECTORY,
:SQLT_STR_FILE_NAME
))(+)
order by 1
".
