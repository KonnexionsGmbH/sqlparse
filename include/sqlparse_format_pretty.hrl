%% -----------------------------------------------------------------------------
%%
%% sqlparse_format_pretty.hrl: SQL - creating a pretty formatted version
%%                                   of the SQL statement.
%%
%% Copyright (c) 2012-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(SQLPARSE_FORMAT_PRETTY_HRL).
-define(SQLPARSE_FORMAT_PRETTY_HRL, true).

-include("sql_lex.hrl").
-include("sqlparse_fold.hrl").

-record(lopts, {
%% Allowed values: init_cap, keep_unchanged, lower,upper -----------------------
    case_identifier = keep_unchanged,
%% Allowed values: init_cap, lower,upper ---------------------------------------
    case_keyword = upper,
%% Allowed values: 1 - 8 -------------------------------------------------------
    indent_space = 4,
%% Allowed values: space, tab --------------------------------------------------
    indent_with = space,
%% Allowed values: any integer greater than zero -------------------------------
    line_break_after = 80,
%% Allowed values: false, true -------------------------------------------------
    ws_operator = true
}).

-endif.
