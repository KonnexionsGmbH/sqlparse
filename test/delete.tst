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
