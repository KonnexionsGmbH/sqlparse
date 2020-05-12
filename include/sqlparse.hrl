%% -----------------------------------------------------------------------------
%%
%% sqlparse.hrl: SQL - unparsing utilities.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(SQLPARSE_HRL).
-define(SQLPARSE_HRL, true).

-ifdef(NODEBUG).
    -define(D(Format), undefined).
    -define(D(Format, Args), undefined).
-else.
    -define(D(Format), ?D(Format, [])).
    -define(D(Format, Args),
        io:format(user, "~p:~p:~p ===> "Format,
                  [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).
-endif.

-define(E(Format), ?E(Format, [])).
-define(E(Format, Args), io:format(user, "~p:~p:~p ===> "Format, [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

-endif.
