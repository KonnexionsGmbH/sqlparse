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

% JSON
([A-Za-z0-9_\.]+([:#\[\{]+|([\s\t\n\r]*[#\[\{]+))[A-Za-z0-9_\.\:\(\)\[\]\{\}\#\,\|\-\+\*\/\\%\s\t\n\r]*)
                                                    : parse_json(TokenLine, TokenChars).

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
    {"^(?i)(UNION)$",           'UNION'},
    {"^(?i)(INTERSECT)$",       'INTERSECT'},
    {"^(?i)(MINUS)$",           'MINUS'},
    {"^(?i)(INNER)$",           'INNER'},
    {"^(?i)(OUTER)$",           'OUTER'},
    {"^(?i)(LEFT)$",            'LEFT'},
    {"^(?i)(RIGHT)$",           'RIGHT'},
    {"^(?i)(PARTITION)$",       'PARTITION'},
    {"^(?i)(FULL)$",            'FULL'},
    {"^(?i)(CROSS)$",           'CROSS'},
    {"^(?i)(NATURAL)$",         'NATURAL'},
    {"^(?i)(JOIN)$",            'JOIN'},
    {"^(?i)(USING)$",           'USING'},

    {"^(?i)(LOCAL)$",           'LOCAL'},
    {"^(?i)(SET)$",             'SET'},
    {"^(?i)(ALL)$",             'ALL'},
    {"^(?i)(AND)$",             'AND'},
    {"^(?i)(ANY)$",             'ANY'},
    {"^(?i)(AS)$",              'AS'},
    {"^(?i)(ASC)$",             'ASC'},
    {"^(?i)(AUTHORIZATION)$",   'AUTHORIZATION'},
    {"^(?i)(AUTHENTICATION)$",  'AUTHENTICATION'},
    {"^(?i)(REQUIRED)$",        'REQUIRED'},
    {"^(?i)(BETWEEN)$",         'BETWEEN'},
    {"^(?i)(BY)$",              'BY'},
    {"^(?i)(NOCYCLE)$",         'NOCYCLE'},
    {"^(?i)(START)$",           'START'},
    {"^(?i)(PRIOR)$",           'PRIOR'},
    {"^(?i)(CHECK)$",           'CHECK'},
    {"^(?i)(CLOSE)$",           'CLOSE'},
    {"^(?i)(COMMIT)$",          'COMMIT'},
    {"^(?i)(CONTINUE)$",        'CONTINUE'},
    {"^(?i)(CREATE)$",          'CREATE'},
    {"^(?i)(CURRENT)$",         'CURRENT'},
    {"^(?i)(CURSOR)$",          'CURSOR'},
    {"^(?i)(DECLARE)$",         'DECLARE'},
    {"^(?i)(BEGIN)$",           'BEGIN'},
    {"^(?i)(CALL)$",            'CALL'},
    {"^(?i)(DEFAULT)$",         'DEFAULT'},
    {"^(?i)(DELETE)$",          'DELETE'},
    {"^(?i)(DESC)$",            'DESC'},
    {"^(?i)(DISTINCT)$",        'DISTINCT'},
    {"^(?i)(DOUBLE)$",          'DOUBLE'},
    {"^(?i)(ESCAPE)$",          'ESCAPE'},
    {"^(?i)(EXISTS)$",          'EXISTS'},
    {"^(?i)(FETCH)$",           'FETCH'},
    {"^(?i)(FOR)$",             'FOR'},
    {"^(?i)(FOREIGN)$",         'FOREIGN'},
    {"^(?i)(FOUND)$",           'FOUND'},
    {"^(?i)(FROM)$",            'FROM'},
    {"^(?i)((GO[\s\t]*TO))$",   'GOTO'},
    {"^(?i)(GRANT)$",           'GRANT'},
    {"^(?i)(CONSTRAINS)$",      'CONSTRAINS'},
    {"^(?i)(FORCE)$",           'FORCE'},
    {"^(?i)(GROUP)$",           'GROUP'},
    {"^(?i)(HAVING)$",          'HAVING'},
    {"^(?i)(IN)$",              'IN'},
    {"^(?i)(INDICATOR)$",       'INDICATOR'},
    {"^(?i)(INSERT)$",          'INSERT'},
    {"^(?i)(RETURNING)$",       'RETURNING'},
    {"^(?i)(RETURN)$",          'RETURN'},
    {"^(?i)(INTO)$",            'INTO'},
    {"^(?i)(IS)$",              'IS'},
    {"^(?i)(KEY)$",             'KEY'},
    {"^(?i)(LANGUAGE)$",        'LANGUAGE'},
    {"^(?i)(LIKE)$",            'LIKE'},
    {"^(?i)(NOT)$",             'NOT'},
    {"^(?i)(NULL)$",            'NULLX'},
    {"^(?i)(OF)$",              'OF'},
    {"^(?i)(ON)$",              'ON'},
    {"^(?i)(OPEN)$",            'OPEN'},
    {"^(?i)(OPTION)$",          'OPTION'},
    {"^(?i)(OR)$",              'OR'},
    {"^(?i)(ORDER)$",           'ORDER'},
    {"^(?i)(PRECISION)$",       'PRECISION'},
    {"^(?i)(PRIMARY)$",         'PRIMARY'},
    {"^(?i)(PRIVILEGES)$",      'PRIVILEGES'},
    {"^(?i)(PUBLIC)$",          'PUBLIC'},
    {"^(?i)(REAL)$",            'REAL'},
    {"^(?i)(REFERENCES)$",      'REFERENCES'},
    {"^(?i)(ROLLBACK)$",        'ROLLBACK'},
    {"^(?i)(SCHEMA)$",          'SCHEMA'},
    {"^(?i)(SELECT)$",          'SELECT'},
    {"^(?i)(DROP)$",            'DROP'},
    {"^(?i)(RESTRICT)$",        'RESTRICT'},
    {"^(?i)(CASCADE)$",         'CASCADE'},
    {"^(?i)(SOME)$",            'SOME'},
    {"^(?i)(SQLCODE)$",         'SQLCODE'},
    {"^(?i)(TABLE)$",           'TABLE'},
    {"^(?i)(TO)$",              'TO'},
    {"^(?i)(UNIQUE)$",          'UNIQUE'},
    {"^(?i)(UPDATE)$",          'UPDATE'},
    {"^(?i)(USER)$",            'USER'},
    {"^(?i)(VALUES)$",          'VALUES'},
    {"^(?i)(VIEW)$",            'VIEW'},
    {"^(?i)(WHENEVER)$",        'WHENEVER'},
    {"^(?i)(WHERE)$",           'WHERE'},
    {"^(?i)(WITH)$",            'WITH'},
    {"^(?i)(WORK)$",            'WORK'},
    {"^(?i)(IDENTIFIED)$",      'IDENTIFIED'},
    {"^(?i)(EXTERNALLY)$",      'EXTERNALLY'},
    {"^(?i)(GLOBALLY)$",        'GLOBALLY'},
    {"^(?i)(TABLESPACE)$",      'TABLESPACE'},
    {"^(?i)(TEMPORARY)$",       'TEMPORARY'},
    {"^(?i)(PROFILE)$",         'PROFILE'},
    %{"^(?i)(EXPIRE)$",          'EXPIRE'},
    %{"^(?i)(PASSWORD)$",        'PASSWORD'},
    {"^(?i)(QUOTA)$",           'QUOTA'},
    {"^(?i)(UNLIMITED)$",       'UNLIMITED'},
    {"^(?i)(ALTER)$",           'ALTER'},
    {"^(?i)(ENTERPRISE)$",      'ENTERPRISE'},
    {"^(?i)(REVOKE)$",          'REVOKE'},
    {"^(?i)(THROUGH)$",         'THROUGH'},
    {"^(?i)(USERS)$",           'USERS'},
    {"^(?i)(ROLE)$",            'ROLE'},
    {"^(?i)(ROLES)$",           'ROLES'},
    {"^(?i)(EXCEPT)$",          'EXCEPT'},
    {"^(?i)(NO)$",              'NO'},
    {"^(?i)(NONE)$",            'NONE'},
    {"^(?i)(CONNECT)$",         'CONNECT'},
    {"^(?i)(CASE)$",            'CASE'},
    {"^(?i)(WHEN)$",            'WHEN'},
    {"^(?i)(IF)$",              'IF'},
    {"^(?i)(THEN)$",            'THEN'},
    {"^(?i)(ELSIF)$",           'ELSIF'},
    {"^(?i)(ELSE)$",            'ELSE'},
    {"^(?i)(END)$",             'END'},

    % create options
    {"^(?i)(CLUSTER)$",         'CLUSTER'},
    {"^(?i)(BAG)$",             'BAG'},
    {"^(?i)(ORDERED_SET)$",     'ORDERED_SET'},

    % AMMSCs
    {"^(?i)(AVG)$",             'AMMSC'},
    {"^(?i)(MIN)$",             'AMMSC'},
    {"^(?i)(MAX)$",             'AMMSC'},
    {"^(?i)(SUM)$",             'AMMSC'},
    {"^(?i)(COUNT)$",           'AMMSC'},

    % FUNs
    {"^(?i)(TO_CHAR)$",         'FUNS'},
    {"^(?i)(NVL)$",             'FUNS'},
    {"^(?i)(LTRIM)$",           'FUNS'},
    {"^(?i)(TO_DATE)$",         'FUNS'},
    {"^(?i)(UPPER)$",           'FUNS'},
    {"^(?i)(LOWER)$",           'FUNS'},
    {"^(?i)(TRUNC)$",           'FUNS'},
    {"^(?i)(ABS)$",             'FUNS'},
    {"^(?i)(ACOS)$",            'FUNS'},
    {"^(?i)(ASIN)$",            'FUNS'},
    {"^(?i)(ATAN)$",            'FUNS'},
    {"^(?i)(COS)$",             'FUNS'},
    {"^(?i)(COSH)$",            'FUNS'},
    {"^(?i)(COT)$",             'FUNS'},
    {"^(?i)(SIN)$",             'FUNS'},
    {"^(?i)(SINH)$",            'FUNS'},
    {"^(?i)(TAN)$",             'FUNS'},
    {"^(?i)(TANH)$",            'FUNS'},
    {"^(?i)(ATAN2)$",           'FUNS'},

    % Logical funs
    {"^(?i)(BOOL_AND)$",        'UFUN'},
    {"^(?i)(BOOL_OR)$",         'UFUN'},
    {"^(?i)(SELECTIVITY)$",     'UFUN'},
    {"^(?i)(STDDEV_POP)$",      'UFUN'},

    % Truncate
    {"^(?i)(TRUNCATE)$",        'TRUNCATE'},
    {"^(?i)(PRESERVE)$",        'PRESERVE'},
    {"^(?i)(PURGE)$",           'PURGE'},
    {"^(?i)(MATERIALIZED)$",    'MATERIALIZED'},
    {"^(?i)(LOG)$",             'LOG'},
    {"^(?i)(REUSE)$",           'REUSE'},
    {"^(?i)(STORAGE)$",         'STORAGE'},

    % Index
    {"^(?i)(BITMAP)$",          'BITMAP'},
    {"^(?i)(KEYLIST)$",         'KEYLIST'},
    {"^(?i)(HASHMAP)$",         'HASHMAP'},
    {"^(?i)(INDEX)$",           'INDEX'},
    {"^(?i)(NORM_WITH)$",       'NORM_WITH'},
    {"^(?i)(FILTER_WITH)$",     'FILTER_WITH'}
]).

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(Dbg(F,A), io:format(user, "[~p] "++F++"~n", [?LINE|A])).
-else.
-define(Dbg(F,A), ok).
-endif.

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

match_fun(TokenLine, TokenChars) ->
    {match,[MatchedFunStr]} = re:run(TokenChars, "^fun.*end\\.", [ungreedy,dotall,{capture, all, list}]),
    {token, {'STRING', TokenLine, MatchedFunStr}, string:sub_string(TokenChars, length(MatchedFunStr)+1)}.

parse_json(TokenLine, TokenChars) ->
    ?Dbg("TokenChars=~p", [TokenChars]),
    parse_json(TokenLine, TokenChars, "", "", 0).

parse_json(TokenLine, [], Json, PushBack, 0) ->
    {token, {'JSON', TokenLine, lists:reverse(Json)}, PushBack};
parse_json(_TokenLine, [], Json, _PushBack, Open) ->
    {error, lists:flatten(
                io_lib:format("malformed JSON path '~s',"
                              " ~p bracket(s) not closed"
                             , [lists:reverse(Json), Open]))};
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when T =:= $]; T =:= $}; T =:= $) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    if Open > 0 ->
            parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open-1);
        true ->
            parse_json(TokenLine, [], Json, [T|TokenChars], Open)
    end;
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when T =:= $[; T =:= ${; T =:= $( ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open+1);
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when Open > 0 ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open);
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when (Open =:= 0) andalso (
        (T =:= $:) orelse (T =:= $#)
        orelse (T =:= $_) orelse (T =:= $.)
        orelse ((T >= $A) andalso (T =< $Z))
        orelse ((T >= $a) andalso (T =< $z))
        orelse ((T >= $0) andalso (T =< $9))
      ) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open);
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when (Open =:= 0) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    {NewTokenChars, NewJson, NewPushBack} =
    case T of
        T when [T]=:=" ";T=:=$\n;T=:=$\t;T=:=$\r -> % white space characters
        case re:run(TokenChars, "^([[:space:]]*)(.*)", [{capture, [1,2], list}, dotall]) of
            {match,[WhiteSpace,[F|_] = Rest]} when F=:=$[;F=:=${;F=:=$( ->
                {Rest, lists:reverse(WhiteSpace)++[T|Json], PushBack};
            _ ->
                {[], Json, [T|TokenChars]}
        end;
        _ -> {[], Json, [T|TokenChars]}
    end,
    ?Dbg("NewTokenChars=~p NewJson=~p NewPushBack=~p", [NewTokenChars, NewJson, NewPushBack]),
    parse_json(TokenLine, NewTokenChars, NewJson, NewPushBack, Open).
