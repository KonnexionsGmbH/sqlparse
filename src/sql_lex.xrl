Definitions.

Rules.

% erlang funcs
(fun\(\).*end\.)                                    : {token, {'STRING', TokenLine, TokenChars}}.

% strings
(\'([^\']*(\'\')*)*\')                              : {token, {'STRING', TokenLine, TokenChars}}.
(\"((\$|[^\"]*)*(\"\")*)*\")                        : {token, {'NAME', TokenLine, TokenChars}}.

% hint
((\/\*)[^\*\/]*(\*\/))                              : {token, {'HINT', TokenLine, TokenChars}}.

% punctuation
(=|<>|<|>|<=|>=)                                    : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
([\|\-\+\*\/\(\)\,\.\;]|(\|\|)|(div))               : {token, {list_to_atom(TokenChars), TokenLine}}.

% names
%[A-Za-z][A-Za-z0-9_@:#]*                           : {token, {'NAME', TokenLen, TokenChars}}.
[A-Za-z][A-Za-z0-9_@:#\$]*                          : match_any(TokenChars, TokenLen, TokenLine, ?TokenPatters).

% parameters
(\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)                   : {token, {'PARAMETER', TokenLine, TokenChars}}.

% numbers
%(([\+\-]?)([0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)*))    : {token, {'APPROXNUM', TokenLine, list_to_float(TokenChars)}}.
%([\+\-]?[0-9]+)                                    : {token, {'INTNUM', TokenLine, list_to_integer(TokenChars)}}.
(([0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)*))              : {token, {'APPROXNUM', TokenLine, TokenChars}}.
([0-9]+)                                            : {token, {'INTNUM', TokenLine, TokenChars}}.

% skips
([\s\t\r\n]+)                                       : skip_token.    %% white space

% comments
%((\-\-).*[\n])                                     : {token, {'COMMENT', TokenLine, TokenChars}}.
((\-\-).*[\n])                                      : skip_token.


Erlang code.

-export([reserved_keywords/0]).

-define(TokenPatters, [

    % sql joins
    {"^(?i)(UNION)$",                    'UNION'},
    {"^(?i)(INTERSECT)$",                'INTERSECT'},
    {"^(?i)(MINUS)$",                    'MINUS'},
    {"^(?i)(INNER)$",                    'INNER'},
    {"^(?i)(OUTER)$",                    'OUTER'},
    {"^(?i)(LEFT)$",                     'LEFT'},
    {"^(?i)(RIGHT)$",                    'RIGHT'},
    {"^(?i)(PARTITION)$",                'PARTITION'},
    {"^(?i)(FULL)$",                     'FULL'},
    {"^(?i)(CROSS)$",                    'CROSS'},
    {"^(?i)(NATURAL)$",                  'NATURAL'},
    {"^(?i)(JOIN)$",                     'JOIN'},
    {"^(?i)(USING)$",                    'USING'},

    % Data Types (SQL)
%    {"^(?i)((N)?CHAR(ACTER)?)$",         'CHARACTER'},
%    {"^(?i)((N)?CLOB)$",                 'CLOB'},
%    {"^(?i)(RAW)$",                      'RAW'},
%    {"^(?i)(ROWID)$",                    'ROWID'},
%    {"^(?i)((N)?VARCHAR2)$",             'VARCHARACTER'},
%    {"^(?i)(BLOB)$",                     'BLOB'},
%    {"^(?i)(INT(EGER)?)$",               'INTEGER'},
%    {"^(?i)(FLOAT)$",                    'FLOAT'},
%    {"^(?i)(DECIMAL)$",                  'DECIMAL'},
%    {"^(?i)(DATE(TIME)?)$",              'DATETIME'},
%    {"^(?i)(TIMESTAMP)$",                'TIMESTAMP'},
%    {"^(?i)(TIME)$",                     'TIME'},
%    {"^(?i)(YEAR)$",                     'YEAR'},
%    {"^(?i)(NUMERIC)$",                  'NUMERIC'},
%
%    {"^(?i)(TERM)$",                     'ETERM'},
%    {"^(?i)(BOOL(EAN)?)$",               'EBOOL'},
%    {"^(?i)(TUPLE)$",                    'ETUPLE'},
%    {"^(?i)(BINARY)$",                   'EBINARY'},
%    {"^(?i)(ATOM)$",                     'EATOM'},
%    {"^(?i)(IPADDR)$",                   'EIPADDR'},
%    {"^(?i)(LIST)$",                     'ELIST'},
%    {"^(?i)(BINSTR)$",                   'EBINSTR'},
%    {"^(?i)(PID)$",                      'EPID'}, 
%    {"^(?i)(REF)$",                      'EREF'},
%    {"^(?i)(FUN)$",                      'EFUN'},
%    {"^(?i)(STRING)$",                   'ESTRING'},
%    {"^(?i)(USERID)$",                   'EUSERID'},
%    {"^(?i)(DECIMAL)$",                  'DECIMAL'},
%    {"^(?i)(NUMBER)$",                   'DECIMAL'},
%    {"^(?i)(FLOAT)$",                    'FLOAT'},

    {"^(?i)(LOCAL)$",                    'LOCAL'},
    {"^(?i)(SET)$",                      'SET'},
    {"^(?i)(ALL)$",                      'ALL'},
    {"^(?i)(AND)$",                      'AND'},
    {"^(?i)(ANY)$",                      'ANY'},
    {"^(?i)(AS)$",                       'AS'},
    {"^(?i)(ASC)$",                      'ASC'},
    {"^(?i)(AUTHORIZATION)$",            'AUTHORIZATION'},
    {"^(?i)(BETWEEN)$",                  'BETWEEN'},
    {"^(?i)(BY)$",                       'BY'},
    {"^(?i)(NOCYCLE)$",                  'NOCYCLE'},
    {"^(?i)(START)$",                    'START'},
    {"^(?i)(PRIOR)$",                    'PRIOR'},
    {"^(?i)(CHECK)$",                    'CHECK'},
    {"^(?i)(CLOSE)$",                    'CLOSE'},
    {"^(?i)(COMMIT)$",                   'COMMIT'},
    {"^(?i)(CONTINUE)$",                 'CONTINUE'},
    {"^(?i)(CREATE)$",                   'CREATE'},
    {"^(?i)(REPLACE)$",                  'REPLACE'},
    {"^(?i)(CURRENT)$",                  'CURRENT'},
    {"^(?i)(CURSOR)$",                   'CURSOR'},
    {"^(?i)(DECLARE)$",                  'DECLARE'},
    {"^(?i)(DEFAULT)$",                  'DEFAULT'},
    {"^(?i)(DELETE)$",                   'DELETE'},
    {"^(?i)(DESC)$",                     'DESC'},
    {"^(?i)(DISTINCT)$",                 'DISTINCT'},
    {"^(?i)(DOUBLE)$",                   'DOUBLE'},
    {"^(?i)(ESCAPE)$",                   'ESCAPE'},
    {"^(?i)(IF)$",                       'IF'},
    {"^(?i)(EXISTS)$",                   'EXISTS'},
    {"^(?i)(FETCH)$",                    'FETCH'},
    {"^(?i)(FOR)$",                      'FOR'},
    {"^(?i)(FOREIGN)$",                  'FOREIGN'},
    {"^(?i)(FOUND)$",                    'FOUND'},
    {"^(?i)(FROM)$",                     'FROM'},
    {"^(?i)((GO[\s\t]*TO))$",            'GOTO'},
    {"^(?i)(GRANT)$",                    'GRANT'},
    {"^(?i)(CONSTRAINS)$",               'CONSTRAINS'},
    {"^(?i)(FORCE)$",                    'FORCE'},
    {"^(?i)(GROUP)$",                    'GROUP'},
    {"^(?i)(HAVING)$",                   'HAVING'},
    {"^(?i)(IN)$",                       'IN'},
    {"^(?i)(INDICATOR)$",                'INDICATOR'},
    {"^(?i)(INSERT)$",                   'INSERT'},
    {"^(?i)(RETURNING)$",                'RETURNING'},
    {"^(?i)(RETURN)$",                   'RETURN'},
    {"^(?i)(INTO)$",                     'INTO'},
    {"^(?i)(IS)$",                       'IS'},
    {"^(?i)(KEY)$",                      'KEY'},
    {"^(?i)(LANGUAGE)$",                 'LANGUAGE'},
    {"^(?i)(LIKE)$",                     'LIKE'},
    {"^(?i)(NOT)$",                      'NOT'},
    {"^(?i)(NULL)$",                     'NULLX'},
    {"^(?i)(OF)$",                       'OF'},
    {"^(?i)(ON)$",                       'ON'},
    {"^(?i)(OPEN)$",                     'OPEN'},
    {"^(?i)(OPTION)$",                   'OPTION'},
    {"^(?i)(OR)$",                       'OR'},
    {"^(?i)(ORDER)$",                    'ORDER'},
    {"^(?i)(PRECISION)$",                'PRECISION'},
    {"^(?i)(PRIMARY)$",                  'PRIMARY'},
    {"^(?i)(PRIVILEGES)$",               'PRIVILEGES'},
    {"^(?i)(PROCEDURE)$",                'PROCEDURE'},
    {"^(?i)(PUBLIC)$",                   'PUBLIC'},
    {"^(?i)(REAL)$",                     'REAL'},
    {"^(?i)(REFERENCES)$",               'REFERENCES'},
    {"^(?i)(ROLLBACK)$",                 'ROLLBACK'},
    {"^(?i)(SCHEMA)$",                   'SCHEMA'},
    {"^(?i)(SELECT)$",                   'SELECT'},
    {"^(?i)(DROP)$",                     'DROP'},
    {"^(?i)(RESTRICT)$",                 'RESTRICT'},
    {"^(?i)(CASCADE)$",                  'CASCADE'},
    {"^(?i)(SOME)$",                     'SOME'},
    {"^(?i)(SQLCODE)$",                  'SQLCODE'},
    {"^(?i)(TABLE)$",                    'TABLE'},
    {"^(?i)(TO)$",                       'TO'},
    {"^(?i)(UNIQUE)$",                   'UNIQUE'},
    {"^(?i)(UPDATE)$",                   'UPDATE'},
    {"^(?i)(USER)$",                     'USER'},
    {"^(?i)(VALUES)$",                   'VALUES'},
    {"^(?i)(VIEW)$",                     'VIEW'},
    {"^(?i)(WHENEVER)$",                 'WHENEVER'},
    {"^(?i)(WHERE)$",                    'WHERE'},
    {"^(?i)(WITH)$",                     'WITH'},
    {"^(?i)(WORK)$",                     'WORK'},
    {"^(?i)(IDENTIFIED)$",               'IDENTIFIED'},
    {"^(?i)(EXTERNALLY)$",               'EXTERNALLY'},
    {"^(?i)(GLOBALLY)$",                 'GLOBALLY'},
    {"^(?i)(TABLESPACE)$",               'TABLESPACE'},
    {"^(?i)(TEMPORARY)$",                'TEMPORARY'},
    {"^(?i)(PROFILE)$",                  'PROFILE'},
    %{"^(?i)(EXPIRE)$",                   'EXPIRE'},
    %{"^(?i)(PASSWORD)$",                 'PASSWORD'},
    {"^(?i)(QUOTA)$",                    'QUOTA'},
    {"^(?i)(UNLIMITED)$",                'UNLIMITED'},
    {"^(?i)(ALTER)$",                    'ALTER'},
    {"^(?i)(ENTERPRISE)$",               'ENTERPRISE'},
    {"^(?i)(REVOKE)$",                   'REVOKE'},
    {"^(?i)(THROUGH)$",                  'THROUGH'},
    {"^(?i)(USERS)$",                    'USERS'},
    {"^(?i)(ROLE)$",                     'ROLE'},
    {"^(?i)(EXCEPT)$",                   'EXCEPT'},
    {"^(?i)(NONE)$",                     'NONE'},
    {"^(?i)(CONNECT)$",                  'CONNECT'},
    {"^(?i)(CASE)$",                     'CASE'},
    {"^(?i)(WHEN)$",                     'WHEN'},
    {"^(?i)(THEN)$",                     'THEN'},
    {"^(?i)(ELSE)$",                     'ELSE'},
    {"^(?i)(END)$",                      'END'},

    % create options
    {"^(?i)(CLUSTER)$",                  'CLUSTER'},
    {"^(?i)(BAG)$",                      'BAG'},
    {"^(?i)(ORDERED_SET)$",              'ORDERED_SET'},

    % AMMSCs
    {"^(?i)(AVG)$",                      'AMMSC'},
    {"^(?i)(MIN)$",                      'AMMSC'},
    {"^(?i)(MAX)$",                      'AMMSC'},
    {"^(?i)(SUM)$",                      'AMMSC'},
    {"^(?i)(COUNT)$",                    'AMMSC'},

    % FUNs
    {"^(?i)(TO_CHAR)$",                  'FUNS'},
    {"^(?i)(NVL)$",                      'FUNS'},
    {"^(?i)(LTRIM)$",                    'FUNS'},
    {"^(?i)(TO_DATE)$",                  'FUNS'},
    {"^(?i)(UPPER)$",                    'FUNS'},
    {"^(?i)(LOWER)$",                    'FUNS'},
    {"^(?i)(TRUNC)$",                    'FUNS'},
    {"^(?i)(ABS)$",                      'FUNS'},
    {"^(?i)(ACOS)$",                     'FUNS'},
    {"^(?i)(ASIN)$",                     'FUNS'},
    {"^(?i)(ATAN)$",                     'FUNS'},
    {"^(?i)(COS)$",                      'FUNS'},
    {"^(?i)(COSH)$",                     'FUNS'},
    {"^(?i)(COT)$",                      'FUNS'},
    {"^(?i)(SIN)$",                      'FUNS'},
    {"^(?i)(SINH)$",                     'FUNS'},
    {"^(?i)(TAN)$",                      'FUNS'},
    {"^(?i)(TANH)$",                     'FUNS'},
    {"^(?i)(ATAN2)$",                    'FUNS'},

    % Logical funs
    {"^(?i)(BOOL_AND)$",                 'UFUN'},
    {"^(?i)(BOOL_OR)$",                  'UFUN'},
    {"^(?i)(SELECTIVITY)$",              'UFUN'},
    {"^(?i)(STDDEV_POP)$",               'UFUN'},

    % Truncate
    {"^(?i)(TRUNCATE)$",                 'TRUNCATE'},
    {"^(?i)(PRESERVE)$",                 'PRESERVE'},
    {"^(?i)(PURGE)$",                    'PURGE'},
    {"^(?i)(MATERIALIZED)$",             'MATERIALIZED'},
    {"^(?i)(LOG)$",                      'LOG'},
    {"^(?i)(REUSE)$",                    'REUSE'},
    {"^(?i)(STORAGE)$",                  'STORAGE'}
]).

reserved_keywords() -> [T || {_, T} <- ?TokenPatters].

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P,T}|TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match,[_]} ->
            if (T =:= 'AMMSC') orelse
               (T =:= 'FUNS') orelse
               (T =:= 'UFUN') -> {token, {T, TokenLine, list_to_atom(TokenChars)}};
            true              -> {token, {T, TokenLine}}
        end;
        nomatch     -> match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.
