%% -----------------------------------------------------------------------------
%%
%% sql_lex.xrl: SQL - lexer definition.
%%
%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -*- erlang -*-
Definitions.

Rules.

% erlang funcs
(fun\([A-Za-z0-9,_]*\).*\->.*end\.)                 : match_fun(TokenLine, TokenChars).
(fun\s['A-Za-z0-9_]+:['A-Za-z0-9_]+\/[0-9]+\.)      : {token, {'STRING', TokenLine, TokenChars}}.

% strings
(\'([^\']*(\'\')*)*\')                              : {token, {'STRING', TokenLine, TokenChars}}.
(\"((\$|[^\"]*)*(\"\")*)*\")                        : {token, {'NAME', TokenLine, TokenChars}}.

% hint
((\/\*)[^\*\/]*(\*\/))                              : {token, {'HINT', TokenLine, TokenChars}}.

% punctuation
(!=|\^=|<>|<|>|<=|>=)                               : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
([=\|\-\+\*\/\(\)\,\.\;]|(\|\|)|(div))              : {token, {list_to_atom(TokenChars), TokenLine}}.

% JSON
(\|[:{\[#]([^\|]*)+\|)                              : parse_json(TokenLine, TokenChars).

% names
[A-Za-z][A-Za-z0-9_@\$~]*                           : match_any(TokenChars, TokenLen, TokenLine, ?TokenPatters).

% parameters
(\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)                   : {token, {'PARAMETER', TokenLine, TokenChars}}.

% numbers
([0-9]+)                                            : {token, {'INTNUM', TokenLine, TokenChars}}.
((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))([eE][+-]?[0-9]+)?[fFdD]?)
                                                    : {token, {'APPROXNUM', TokenLine, TokenChars}}.

% skips
([\s\t\r\n]+)                                       : skip_token.    %% white space

% comments
%((\-\-).*[\n])                                     : {token, {'COMMENT', TokenLine, TokenChars}}.
((\-\-).*[\n])                                      : skip_token.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% sqllexer.erl: SQL - lexer.
%%
%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-export([reserved_keywords/0]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-define(TokenPatters, [

    % sql joins
    {"^(?i)(CROSS)$",           'CROSS'},
    {"^(?i)(FULL)$",            'FULL'},
    {"^(?i)(INNER)$",           'INNER'},
    {"^(?i)(INTERSECT)$",       'INTERSECT'},
    {"^(?i)(JOIN)$",            'JOIN'},
    {"^(?i)(LEFT)$",            'LEFT'},
    {"^(?i)(MINUS)$",           'MINUS'},
    {"^(?i)(NATURAL)$",         'NATURAL'},
    {"^(?i)(OUTER)$",           'OUTER'},
    {"^(?i)(PARTITION)$",       'PARTITION'},
    {"^(?i)(RIGHT)$",           'RIGHT'},
    {"^(?i)(UNION)$",           'UNION'},
    {"^(?i)(USING)$",           'USING'},

    {"^(?i)((GO[\s\t]*TO))$",   'GOTO'},
    {"^(?i)(ALL)$",             'ALL'},
    {"^(?i)(ALTER)$",           'ALTER'},
    {"^(?i)(AND)$",             'AND'},
    {"^(?i)(ANY)$",             'ANY'},
    {"^(?i)(AS)$",              'AS'},
    {"^(?i)(ASC)$",             'ASC'},
    {"^(?i)(AUTHENTICATION)$",  'AUTHENTICATION'},
    {"^(?i)(AUTHORIZATION)$",   'AUTHORIZATION'},
    {"^(?i)(BEGIN)$",           'BEGIN'},
    {"^(?i)(BETWEEN)$",         'BETWEEN'},
    {"^(?i)(BY)$",              'BY'},
    {"^(?i)(CALL)$",            'CALL'},
    {"^(?i)(CASCADE)$",         'CASCADE'},
    {"^(?i)(CASE)$",            'CASE'},
    {"^(?i)(CHECK)$",           'CHECK'},
    {"^(?i)(CLOSE)$",           'CLOSE'},
    {"^(?i)(COMMIT)$",          'COMMIT'},
    {"^(?i)(CONNECT)$",         'CONNECT'},
    {"^(?i)(CONSTRAINTS)$",     'CONSTRAINTS'},
    {"^(?i)(CONTINUE)$",        'CONTINUE'},
    {"^(?i)(CREATE)$",          'CREATE'},
    {"^(?i)(CURRENT)$",         'CURRENT'},
    {"^(?i)(CURSOR)$",          'CURSOR'},
    {"^(?i)(DECLARE)$",         'DECLARE'},
    {"^(?i)(DEFAULT)$",         'DEFAULT'},
    {"^(?i)(DELETE)$",          'DELETE'},
    {"^(?i)(DESC)$",            'DESC'},
    {"^(?i)(DISTINCT)$",        'DISTINCT'},
    {"^(?i)(DOUBLE)$",          'DOUBLE'},
    {"^(?i)(DROP)$",            'DROP'},
    {"^(?i)(ELSE)$",            'ELSE'},
    {"^(?i)(ELSIF)$",           'ELSIF'},
    {"^(?i)(END)$",             'END'},
    {"^(?i)(ENTERPRISE)$",      'ENTERPRISE'},
    {"^(?i)(ESCAPE)$",          'ESCAPE'},
    {"^(?i)(EXCEPT)$",          'EXCEPT'},
    {"^(?i)(EXISTS)$",          'EXISTS'},
%   {"^(?i)(EXPIRE)$",          'EXPIRE'},
    {"^(?i)(EXTERNALLY)$",      'EXTERNALLY'},
    {"^(?i)(FETCH)$",           'FETCH'},
    {"^(?i)(FOR)$",             'FOR'},
    {"^(?i)(FORCE)$",           'FORCE'},
    {"^(?i)(FOREIGN)$",         'FOREIGN'},
    {"^(?i)(FOUND)$",           'FOUND'},
    {"^(?i)(FROM)$",            'FROM'},
    {"^(?i)(GLOBALLY)$",        'GLOBALLY'},
    {"^(?i)(GRANT)$",           'GRANT'},
    {"^(?i)(GROUP)$",           'GROUP'},
    {"^(?i)(HAVING)$",          'HAVING'},
    {"^(?i)(IDENTIFIED)$",      'IDENTIFIED'},
    {"^(?i)(IF)$",              'IF'},
    {"^(?i)(IN)$",              'IN'},
    {"^(?i)(INDICATOR)$",       'INDICATOR'},
    {"^(?i)(INSERT)$",          'INSERT'},
    {"^(?i)(INTO)$",            'INTO'},
    {"^(?i)(IS)$",              'IS'},
    {"^(?i)(JAVA)$",            'JAVA'},
    {"^(?i)(KEY)$",             'KEY'},
    {"^(?i)(LANGUAGE)$",        'LANGUAGE'},
    {"^(?i)(LIKE)$",            'LIKE'},
    {"^(?i)(LOCAL)$",           'LOCAL'},
    {"^(?i)(NO)$",              'NO'},
    {"^(?i)(NOCYCLE)$",         'NOCYCLE'},
    {"^(?i)(NONE)$",            'NONE'},
    {"^(?i)(NOT)$",             'NOT'},
    {"^(?i)(NULL)$",            'NULLX'},
    {"^(?i)(OF)$",              'OF'},
    {"^(?i)(ON)$",              'ON'},
    {"^(?i)(OPEN)$",            'OPEN'},
    {"^(?i)(OPTION)$",          'OPTION'},
    {"^(?i)(OR)$",              'OR'},
    {"^(?i)(ORDER)$",           'ORDER'},
%   {"^(?i)(PASSWORD)$",        'PASSWORD'},
    {"^(?i)(PRECISION)$",       'PRECISION'},
    {"^(?i)(PRIMARY)$",         'PRIMARY'},
    {"^(?i)(PRIOR)$",           'PRIOR'},
    {"^(?i)(PRIVILEGES)$",      'PRIVILEGES'},
    {"^(?i)(PROFILE)$",         'PROFILE'},
    {"^(?i)(PUBLIC)$",          'PUBLIC'},
    {"^(?i)(QUOTA)$",           'QUOTA'},
    {"^(?i)(REAL)$",            'REAL'},
    {"^(?i)(REFERENCES)$",      'REFERENCES'},
    {"^(?i)(REQUIRED)$",        'REQUIRED'},
    {"^(?i)(RESOURCE)$",        'RESOURCE'},
    {"^(?i)(RESTRICT)$",        'RESTRICT'},
    {"^(?i)(RETURN)$",          'RETURN'},
    {"^(?i)(RETURNING)$",       'RETURNING'},
    {"^(?i)(REVOKE)$",          'REVOKE'},
    {"^(?i)(ROLE)$",            'ROLE'},
    {"^(?i)(ROLES)$",           'ROLES'},
    {"^(?i)(ROLLBACK)$",        'ROLLBACK'},
    {"^(?i)(SCHEMA)$",          'SCHEMA'},
    {"^(?i)(SELECT)$",          'SELECT'},
    {"^(?i)(SET)$",             'SET'},
    {"^(?i)(SOME)$",            'SOME'},
    {"^(?i)(SOURCE)$",          'SOURCE'},
    {"^(?i)(SQLERROR)$",        'SQLERROR'},
    {"^(?i)(START)$",           'START'},
    {"^(?i)(TABLE)$",           'TABLE'},
    {"^(?i)(TABLESPACE)$",      'TABLESPACE'},
    {"^(?i)(TEMPORARY)$",       'TEMPORARY'},
    {"^(?i)(THEN)$",            'THEN'},
    {"^(?i)(THROUGH)$",         'THROUGH'},
    {"^(?i)(TO)$",              'TO'},
    {"^(?i)(UNIQUE)$",          'UNIQUE'},
    {"^(?i)(UNLIMITED)$",       'UNLIMITED'},
    {"^(?i)(UPDATE)$",          'UPDATE'},
    {"^(?i)(USER)$",            'USER'},
    {"^(?i)(USERS)$",           'USERS'},
    {"^(?i)(VALUES)$",          'VALUES'},
    {"^(?i)(VIEW)$",            'VIEW'},
    {"^(?i)(WHEN)$",            'WHEN'},
    {"^(?i)(WHENEVER)$",        'WHENEVER'},
    {"^(?i)(WHERE)$",           'WHERE'},
    {"^(?i)(WITH)$",            'WITH'},
    {"^(?i)(WORK)$",            'WORK'},

    % create options
    {"^(?i)(BAG)$",             'BAG'},
    {"^(?i)(CLUSTER)$",         'CLUSTER'},
    {"^(?i)(ORDERED_SET)$",     'ORDERED_SET'},

    % AMMSCs
    {"^(?i)(AVG)$",             'FUNS'},
    {"^(?i)(CORR)$",            'FUNS'},
    {"^(?i)(COUNT)$",           'FUNS'},
    {"^(?i)(COVAR_POP)$",       'FUNS'},
    {"^(?i)(COVAR_SAMP)$",      'FUNS'},
    {"^(?i)(MAX)$",             'FUNS'},
    {"^(?i)(MEDIAN)$",          'FUNS'},
    {"^(?i)(MIN)$",             'FUNS'},
    {"^(?i)(REGR_AVGX)$",       'FUNS'},
    {"^(?i)(REGR_AVGY)$",       'FUNS'},
    {"^(?i)(REGR_COUNT)$",      'FUNS'},
    {"^(?i)(REGR_INTERCEPT)$",  'FUNS'},
    {"^(?i)(REGR_R2)$",         'FUNS'},
    {"^(?i)(REGR_SLOPE)$",      'FUNS'},
    {"^(?i)(REGR_SXX)$",        'FUNS'},
    {"^(?i)(REGR_SXY)$",        'FUNS'},
    {"^(?i)(REGR_SYY)$",        'FUNS'},
    {"^(?i)(STDDEV)$",          'FUNS'},
    {"^(?i)(STDDEV_POP)$",      'FUNS'},
    {"^(?i)(STDDEV_SAMP)$",     'FUNS'},
    {"^(?i)(SUM)$",             'FUNS'},
    {"^(?i)(VAR_POP)$",         'FUNS'},
    {"^(?i)(VAR_SAMP)$",        'FUNS'},
    {"^(?i)(VARIANCE)$",        'FUNS'},

    % FUNs
    {"^(?i)(ABS)$",             'FUNS'},
    {"^(?i)(ACOS)$",            'FUNS'},
    {"^(?i)(ASIN)$",            'FUNS'},
    {"^(?i)(ATAN)$",            'FUNS'},
    {"^(?i)(ATAN2)$",           'FUNS'},
    {"^(?i)(COS)$",             'FUNS'},
    {"^(?i)(COSH)$",            'FUNS'},
    {"^(?i)(COT)$",             'FUNS'},
    {"^(?i)(LOWER)$",           'FUNS'},
    {"^(?i)(LTRIM)$",           'FUNS'},
    {"^(?i)(NVL)$",             'FUNS'},
    {"^(?i)(SIN)$",             'FUNS'},
    {"^(?i)(SINH)$",            'FUNS'},
    {"^(?i)(TAN)$",             'FUNS'},
    {"^(?i)(TANH)$",            'FUNS'},
    {"^(?i)(TO_CHAR)$",         'FUNS'},
    {"^(?i)(TO_DATE)$",         'FUNS'},
    {"^(?i)(TRUNC)$",           'FUNS'},
    {"^(?i)(UPPER)$",           'FUNS'},

    % Logical funs
    {"^(?i)(BOOL_AND)$",        'UFUN'},
    {"^(?i)(BOOL_OR)$",         'UFUN'},
    {"^(?i)(SELECTIVITY)$",     'UFUN'},

    % Truncate
    {"^(?i)(LOG)$",             'LOG'},
    {"^(?i)(MATERIALIZED)$",    'MATERIALIZED'},
    {"^(?i)(PRESERVE)$",        'PRESERVE'},
    {"^(?i)(PURGE)$",           'PURGE'},
    {"^(?i)(REUSE)$",           'REUSE'},
    {"^(?i)(STORAGE)$",         'STORAGE'},
    {"^(?i)(TRUNCATE)$",        'TRUNCATE'},

    % Index
    {"^(?i)(BITMAP)$",          'BITMAP'},
    {"^(?i)(FILTER_WITH)$",     'FILTER_WITH'},
    {"^(?i)(HASHMAP)$",         'HASHMAP'},
    {"^(?i)(INDEX)$",           'INDEX'},
    {"^(?i)(KEYLIST)$",         'KEYLIST'},
    {"^(?i)(NORM_WITH)$",       'NORM_WITH'}
]).

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(Dbg(F, A), io:format(user, "[~p] " ++ F ++ "~n", [?LINE | A])).
-else.
-define(Dbg(F, A), ok).
-endif.

reserved_keywords() -> [T || {_, T} <- ?TokenPatters].

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P, T} | TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match, [_]} ->
            if (T =:= 'FUNS') orelse
                (T =:= 'UFUN') ->
                {token, {T, TokenLine, list_to_atom(TokenChars)}};
                true -> {token, {T, TokenLine}}
            end;
        nomatch -> match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.

match_fun(TokenLine, TokenChars) ->
    {match, [MatchedFunStr]} = re:run(TokenChars, "^fun.*end\\.", [ungreedy, dotall, {capture, all, list}]),
    {token, {'STRING', TokenLine, MatchedFunStr}, string:sub_string(TokenChars, length(MatchedFunStr) + 1)}.

parse_json(TokenLine, TokenCharsIn) ->
    TokenChars = string:substr(TokenCharsIn, 2, length(TokenCharsIn) - 2),
    ?Dbg("TokenChars=~p", [TokenChars]),
    parse_json(TokenLine, TokenChars, "", "", 0).

parse_json(TokenLine, [], Json, PushBack, 0) ->
    {token, {'JSON', TokenLine, lists:reverse(Json)}, PushBack};
parse_json(_TokenLine, [], Json, _PushBack, Open) ->
    {error, lists:flatten(
        io_lib:format("malformed JSON path '~s',"
        " ~p bracket(s) not closed"
            , [lists:reverse(Json), Open]))};
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when T =:= $]; T =:= $}; T =:= $) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    if Open > 0 ->
        parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open - 1);
        true ->
            parse_json(TokenLine, [], Json, [T | TokenChars], Open)
    end;
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when T =:= $[; T =:= ${; T =:= $( ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open + 1);
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when Open > 0 ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open);
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when (Open =:= 0) andalso (
    (T =:= $:) orelse (T =:= $#)
        orelse (T =:= $_) orelse (T =:= $.)
        orelse ((T >= $A) andalso (T =< $Z))
        orelse ((T >= $a) andalso (T =< $z))
        orelse ((T >= $0) andalso (T =< $9))
) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open);
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when (Open =:= 0) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    {NewTokenChars, NewJson, NewPushBack} =
        case T of
            T when [T] =:= " ";T =:= $\n;T =:= $\t;T =:= $\r -> % white space characters
                case re:run(TokenChars, "^([[:space:]]*)(.*)", [{capture, [1, 2], list}, dotall]) of
                    {match, [WhiteSpace, [F | _] = Rest]} when F =:= $[;F =:= ${;F =:= $( ->
                        {Rest, lists:reverse(WhiteSpace) ++ [T | Json], PushBack};
                    _ ->
                        {[], Json, [T | TokenChars]}
                end;
            _ -> {[], Json, [T | TokenChars]}
        end,
    ?Dbg("NewTokenChars=~p NewJson=~p NewPushBack=~p", [NewTokenChars, NewJson, NewPushBack]),
    parse_json(TokenLine, NewTokenChars, NewJson, NewPushBack, Open).
