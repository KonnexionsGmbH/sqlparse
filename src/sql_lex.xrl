Definitions.

Rules.

(ALL|all)						:				{token, {'ALL', TokenLine}}.
(AND|and)						:				{token, {'AND', TokenLine}}.

(AVG|avg|MIN|min|MAX|max|SUM|sum|COUNT|count)
                                :				{token, {'AMMSC', TokenLine, list_to_atom(TokenChars)}}.
(To_Char|TO_CHAR|to_char|NVL|nvl|DECODE|decode|ltrim|LTRIM|to_date|TO_DATE|upper|UPPER|lower|LOWER|trunc|TRUNC|sydate|SYSDATE)
                                :				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(BOOL_AND|bool_and|BOOL_OR|bool_or|SELECTIVITY|selectivity|STDDEV_POP|stddev_pop)
                                :				{token, {'UFUN', TokenLine, list_to_atom(TokenChars)}}.
(ABS|abs|ACOS|acos|ASIN|asin|ATAN|atan|COS|cos|COSH|cosh|COT|cot|SIN|sin|SINH|sinh|TAN|tan|TANH|tanh)
			                    :				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(ATAN2|atan2)                   :				{token, {'BFUN', TokenLine, list_to_atom(TokenChars)}}.

(ANY|any)						:				{token, {'ANY', TokenLine}}.
(AS|as)							:				{token, {'AS', TokenLine}}.
(ASC|asc)						:				{token, {'ASC', TokenLine}}.
(AUTHORIZATION|authorization)	:				{token, {'AUTHORIZATION', TokenLine}}.
(BETWEEN|between)				:				{token, {'BETWEEN', TokenLine}}.
(BY|by)							:				{token, {'BY', TokenLine}}.
((CHAR(ACTER)?)|(char(acter)?))	:				{token, {'CHARACTER', TokenLine}}.
(CHECK|check)					:				{token, {'CHECK', TokenLine}}.
(CLOSE|close)					:				{token, {'CLOSE', TokenLine}}.
(COMMIT|commit)					:				{token, {'COMMIT', TokenLine}}.
(CONTINUE|continue)				:				{token, {'CONTINUE', TokenLine}}.
(CREATE|create)					:				{token, {'CREATE', TokenLine}}.
(CURRENT|current)				:				{token, {'CURRENT', TokenLine}}.
(CURSOR|cursor)					:				{token, {'CURSOR', TokenLine}}.
(DECIMAL|decimal)				:				{token, {'DECIMAL', TokenLine}}.
(DECLARE|declare)				:				{token, {'DECLARE', TokenLine}}.
(DEFAULT|default)				:				{token, {'DEFAULT', TokenLine}}.
(DELETE|delete)					:				{token, {'DELETE', TokenLine}}.
(DESC|desc)						:				{token, {'DESC', TokenLine}}.
(DISTINCT|distinct)				:				{token, {'DISTINCT', TokenLine}}.
(DOUBLE|double)					:				{token, {'DOUBLE', TokenLine}}.
(ESCAPE|escape)					:				{token, {'ESCAPE', TokenLine}}.
(EXISTS|exists)					:				{token, {'EXISTS', TokenLine}}.
(FETCH|fetch)					:				{token, {'FETCH', TokenLine}}.
(FLOAT|float)					:				{token, {'FLOAT', TokenLine}}.
(FOR|for)						:				{token, {'FOR', TokenLine}}.
(FOREIGN|foreign)				:				{token, {'FOREIGN', TokenLine}}.
(FOUND|found)					:				{token, {'FOUND', TokenLine}}.
(FROM|from)						:				{token, {'FROM', TokenLine}}.
((GO[\s\t]*TO)|(go[\s\t]*to))	:				{token, {'GOTO', TokenLine}}.
(GRANT|grant)					:				{token, {'GRANT', TokenLine}}.
(GROUP|group)					:				{token, {'GROUP', TokenLine}}.
(HAVING|having)					:				{token, {'HAVING', TokenLine}}.
(IN|in)							:				{token, {'IN', TokenLine}}.
(INDICATOR|indicator)			:				{token, {'INDICATOR', TokenLine}}.
(INSERT|insert)					:				{token, {'INSERT', TokenLine}}.
(INT(EGER)?|int(eger)?)			:				{token, {'INTEGER', TokenLine}}.
(INTO|into)						:				{token, {'INTO', TokenLine}}.
(IS|is)							:				{token, {'IS', TokenLine}}.
(KEY|key)						:				{token, {'KEY', TokenLine}}.
(LANGUAGE|language)				:				{token, {'LANGUAGE', TokenLine}}.
(LIKE|like)						:				{token, {'LIKE', TokenLine}}.
(NOT|not)						:				{token, {'NOT', TokenLine}}.
(NULL|null)						:				{token, {'NULLX', TokenLine}}.
(NUMERIC|numeric)				:				{token, {'NUMERIC', TokenLine}}.
(OF|of)							:				{token, {'OF', TokenLine}}.
(ON|on)							:				{token, {'ON', TokenLine}}.
(OPEN|open)						:				{token, {'OPEN', TokenLine}}.
(OPTION|option)					:				{token, {'OPTION', TokenLine}}.
(OR|or)							:				{token, {'OR', TokenLine}}.
(ORDER|order)					:				{token, {'ORDER', TokenLine}}.
(PRECISION|precision)			:				{token, {'PRECISION', TokenLine}}.
(PRIMARY|primary)				:				{token, {'PRIMARY', TokenLine}}.
(PRIVILEGES|privileges)			:				{token, {'PRIVILEGES', TokenLine}}.
(PROCEDURE|procedure)			:				{token, {'PROCEDURE', TokenLine}}.
(PUBLIC|public)					:				{token, {'PUBLIC', TokenLine}}.
(REAL|real)						:				{token, {'REAL', TokenLine}}.
(REFERENCES|references)			:				{token, {'REFERENCES', TokenLine}}.
(ROLLBACK|rollback)				:				{token, {'ROLLBACK', TokenLine}}.
(SCHEMA|schema)					:				{token, {'SCHEMA', TokenLine}}.
(SELECT|select)					:				{token, {'SELECT', TokenLine}}.
(SET|set)						:				{token, {'SET', TokenLine}}.
(SMALLINT|smallint)				:				{token, {'SMALLINT', TokenLine}}.
(SOME|some)						:				{token, {'SOME', TokenLine}}.
(SQLCODE|sqlcode)				:				{token, {'SQLCODE', TokenLine}}.
(TABLE|table)					:				{token, {'TABLE', TokenLine}}.
(TO|to)							:				{token, {'TO', TokenLine}}.
(UNION|union)					:				{token, {'UNION', TokenLine}}.
(UNIQUE|unique)					:				{token, {'UNIQUE', TokenLine}}.
(UPDATE|update)					:				{token, {'UPDATE', TokenLine}}.
(USER|user)						:				{token, {'USER', TokenLine}}.
(VALUES|values)					:				{token, {'VALUES', TokenLine}}.
(VIEW|view)						:				{token, {'VIEW', TokenLine}}.
(WHENEVER|whenever)				:				{token, {'WHENEVER', TokenLine}}.
(WHERE|where)					:				{token, {'WHERE', TokenLine}}.
(WITH|with)						:				{token, {'WITH', TokenLine}}.
(WORK|work)						:				{token, {'WORK', TokenLine}}.


% hint
((\/\*).*(\*\/))        : {token, {'HINT', TokenLine, TokenChars}}.

% punctuation
(=|<>|<|>|<=|>=)                                                                         : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
([\|\-\+\*\/\(\)\,\.\;]|(\|\|))                                                          : {token, {list_to_atom(TokenChars), TokenLine}}.

% names
[A-Za-z][A-Za-z0-9_]*                                                                    : {token, {'NAME', TokenLen, TokenChars}}.

% parameters
(\:[A-Za-z][A-Za-z0-9_]*)                                                                : {token, {'PARAMETER', TokenLine, TokenChars}}.

% numbers
%(([\+\-]?)([0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)*))                                          : {token, {'APPROXNUM', TokenLine, list_to_float(TokenChars)}}.
%([\+\-]?[0-9]+)                                                                          : {token, {'INTNUM', TokenLine, list_to_integer(TokenChars)}}.
(([\+\-]?)([0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)*))                                          : {token, {'APPROXNUM', TokenLine, TokenChars}}.
([\+\-]?[0-9]+)                                                                          : {token, {'INTNUM', TokenLine, TokenChars}}.

	%% strings

(\'[^\'\n\r]*\')        : {token, {'STRING', TokenLine, TokenChars}}.
%(\'[^\'\n\r]*)$	    : {error, "Unterminated string"}.

%% - <SQL>\n		{ save_str(" ");lineno++; }
%% - \n		{ lineno++; ECHO; }

([\s\t\r\n]+)  :   skip_token.	%% white space

((\-\-).*[\n])	:	{token, {'COMMENT', TokenLine, TokenChars}}.

%% - .		ECHO;	/* random non-SQL text */
%%

Erlang code.

