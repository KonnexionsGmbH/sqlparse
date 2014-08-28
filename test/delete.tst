%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"DELETE FROM table_name WHERE some_column=some_value".
"DELETE FROM table_name WHERE some_column=some_value RETURNING c,d INTO :c, :d".
"DELETE FROM table_name WHERE some_column=some_value RETURNING lob_column INTO :out_locator".
"DROP USER test_user_123".
"DROP USER test_user_123 CASCADE".
"DROP TABLE table_name".
"DROP TABLE IF EXISTS table_name RESTRICT".
