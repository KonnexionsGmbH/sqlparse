%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS
%%

"DELETE FROM table_name".

"DELETE FROM table_name WHERE some_column=some_value".

"DELETE FROM table_name WHERE some_column=some_value RETURN c,d INTO :c, :d".
"DELETE FROM table_name WHERE some_column=some_value RETURN lob_column INTO :out_locator".

"DELETE FROM table_name WHERE some_column=some_value RETURNING c,d INTO :c, :d".
"DELETE FROM table_name WHERE some_column=some_value RETURNING lob_column INTO :out_locator".

% table reference -------------------------------------------------------------

"Delete From table_1".
"Delete From table_1\"@link_1\"".
"Delete From schema_1.table_1\"@link_1\"".
"Delete From schema_1.table_1".
"Delete From :param_1\"@link_1\"".
"Delete From :param_1".
"Delete From \"^&()\"".

"Delete From table_1\"@link_1\" alias_1".
"Delete From table_1 alias_1".
"Delete From schema_1.table_1\"@link_1\" alias_1".
"Delete From schema_1.table_1 alias_1".
"Delete From :param_1\"@link_1\" alias_1".
"Delete From :param_1 alias_1".
"Delete From \"^&()\" alias_1".
