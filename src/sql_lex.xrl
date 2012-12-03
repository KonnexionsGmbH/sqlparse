Definitions.

Rules.

% Data Types (SQL)
(CHAR(ACTER)?|char(acter)?)		:				{token, {'CHARACTER', TokenLine}}.
(VARCHAR(2)?|varchar(2)?)		:				{token, {'VARCHARACTER', TokenLine}}.
(TINYTEXT|tinytext)				:				{token, {'TINYTEXT', TokenLine}}.
(TEXT|text)						:				{token, {'TEXT', TokenLine}}.
(BLOB|blob)						:				{token, {'BLOB', TokenLine}}.
(MEDIUMTEXT|mediumtext)			:				{token, {'MEDIUMTEXT', TokenLine}}.
(MEDIUMBLOB|mediumblob)			:				{token, {'MEDIUMBLOB', TokenLine}}.
(LONGTEXT|longtext)				:				{token, {'LONGTEXT', TokenLine}}.
(LONGBLOB|longblob)				:				{token, {'LONGBLOB', TokenLine}}.
(ENUM|enum)						:				{token, {'ENUM', TokenLine}}.
(SET|set)						:				{token, {'SET', TokenLine}}.
(TINYINT|tinyint)				:				{token, {'TINYINT', TokenLine}}.
(SMALLINT|smallint)				:				{token, {'SMALLINT', TokenLine}}.
(MEDIUMINT|mediumint)			:				{token, {'MEDIUMINT', TokenLine}}.
(INT(EGER)?|int(eger)?)			:				{token, {'INTEGER', TokenLine}}.
(BIGINT|bigint)					:				{token, {'BIGINT', TokenLine}}.
(FLOAT|float)					:				{token, {'FLOAT', TokenLine}}.
(DOUBLE|double)					:				{token, {'DOUBLE', TokenLine}}.
(DECIMAL|decimal)				:				{token, {'DECIMAL', TokenLine}}.
(DATE|date)						:				{token, {'DATE', TokenLine}}.
(DATETIME|datetime)				:				{token, {'DATETIME', TokenLine}}.
(TIMESTAMP|timestamp)			:				{token, {'TIMESTAMP', TokenLine}}.
(TIME|time)						:				{token, {'TIME', TokenLine}}.
(YEAR|year)						:				{token, {'YEAR', TokenLine}}.
(NUMERIC|numeric)				:				{token, {'NUMERIC', TokenLine}}.
(LOCAL|local)				    :				{token, {'LOCAL', TokenLine}}.

% Data Types (Erlang)
(eTuple|etuple)					:				{token, {'ETUPLE', TokenLine}}.
(eBinary|ebinary)				:				{token, {'EBINARY', TokenLine}}.
(eAtom|eatom)					:				{token, {'EATOM', TokenLine}}.
(eIpaddr|eipaddr)				:				{token, {'EIPADDR', TokenLine}}.
(eList|elist)					:				{token, {'ELIST', TokenLine}}.
(eBinstr|ebinstr)				:				{token, {'EBINSTR', TokenLine}}.
(ePid|epid)						:				{token, {'EPID', TokenLine}}. 
(eRef|eref)						:				{token, {'EREF', TokenLine}}.
(eFun|efun)						:				{token, {'EFUN', TokenLine}}.
(eDatetime|edatetime)			:				{token, {'EDATETIME', TokenLine}}.
(eTimestamp|etimestamp)			:				{token, {'ETIMESTAMP', TokenLine}}.
(eInteger|einteger)				:				{token, {'EINTEGER', TokenLine}}.


% AMMSCs
(AVG|avg)						:				{token, {'AMMSC', TokenLine, list_to_atom(TokenChars)}}.
(MIN|min)						:				{token, {'AMMSC', TokenLine, list_to_atom(TokenChars)}}.
(MAX|max)						:				{token, {'AMMSC', TokenLine, list_to_atom(TokenChars)}}.
(SUM|sum)						:				{token, {'AMMSC', TokenLine, list_to_atom(TokenChars)}}.
(COUNT|count)					:				{token, {'AMMSC', TokenLine, list_to_atom(TokenChars)}}.

% FUNs
(To_Char|TO_CHAR|to_char)		:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(NVL|nvl)(DECODE|decode)		:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(ltrim|LTRIM)					:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(to_date|TO_DATE)				:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(upper|UPPER)					:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(lower|LOWER)					:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(trunc|TRUNC)					:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.
(sydate|SYSDATE)				:				{token, {'FUNS', TokenLine, list_to_atom(TokenChars)}}.

% Logical funs
(BOOL_AND|bool_and)				:				{token, {'UFUN', TokenLine, list_to_atom(TokenChars)}}.
(BOOL_OR|bool_or)				:				{token, {'UFUN', TokenLine, list_to_atom(TokenChars)}}.
(SELECTIVITY|selectivity)		:				{token, {'UFUN', TokenLine, list_to_atom(TokenChars)}}.
(STDDEV_POP|stddev_pop)			:				{token, {'UFUN', TokenLine, list_to_atom(TokenChars)}}.

% Trig funs
(ABS|abs)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(ACOS|acos)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(ASIN|asin)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(ATAN|atan)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(COS|cos)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(COSH|cosh)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(COT|cot)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(SIN|sin)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(SINH|sinh)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(TAN|tan)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(TANH|tanh)						:				{token, {'TRIGFUN', TokenLine, list_to_atom(TokenChars)}}.
(ATAN2|atan2)                   :				{token, {'BFUN', TokenLine, list_to_atom(TokenChars)}}.

(ALL|all)						:				{token, {'ALL', TokenLine}}.
(AND|and)						:				{token, {'AND', TokenLine}}.
(ANY|any)						:				{token, {'ANY', TokenLine}}.
(AS|as)							:				{token, {'AS', TokenLine}}.
(ASC|asc)						:				{token, {'ASC', TokenLine}}.
(AUTHORIZATION|authorization)	:				{token, {'AUTHORIZATION', TokenLine}}.
(BETWEEN|between)				:				{token, {'BETWEEN', TokenLine}}.
(BY|by)							:				{token, {'BY', TokenLine}}.
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
(IF|if)					        :				{token, {'IF', TokenLine}}.
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
(INTO|into)						:				{token, {'INTO', TokenLine}}.
(IS|is)							:				{token, {'IS', TokenLine}}.
(KEY|key)						:				{token, {'KEY', TokenLine}}.
(LANGUAGE|language)				:				{token, {'LANGUAGE', TokenLine}}.
(LIKE|like)						:				{token, {'LIKE', TokenLine}}.
(NOT|not)						:				{token, {'NOT', TokenLine}}.
(NULL|null)						:				{token, {'NULLX', TokenLine}}.
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
(DROP|drop)					    :				{token, {'DROP', TokenLine}}.
(RESTRICT|restrict)				:				{token, {'RESTRICT', TokenLine}}.
(CASCADE|cascade)				:				{token, {'CASCADE', TokenLine}}.
(SOME|some)						:				{token, {'SOME', TokenLine}}.
(SQLCODE|sqlcode)				:				{token, {'SQLCODE', TokenLine}}.
(TABLE|table)					:				{token, {'TABLE', TokenLine}}.
(TO|to)							:				{token, {'TO', TokenLine}}.
(UNIQUE|unique)					:				{token, {'UNIQUE', TokenLine}}.
(UPDATE|update)					:				{token, {'UPDATE', TokenLine}}.
(USER|user)						:				{token, {'USER', TokenLine}}.
(VALUES|values)					:				{token, {'VALUES', TokenLine}}.
(VIEW|view)						:				{token, {'VIEW', TokenLine}}.
(WHENEVER|whenever)				:				{token, {'WHENEVER', TokenLine}}.
(WHERE|where)					:				{token, {'WHERE', TokenLine}}.
(WITH|with)						:				{token, {'WITH', TokenLine}}.
(WORK|work)						:				{token, {'WORK', TokenLine}}.
(IDENTIFIED|identified)         :				{token, {'IDENTIFIED', TokenLine}}.
(EXTERNALLY|externally)         :				{token, {'EXTERNALLY', TokenLine}}.
(GLOBALLY|globally)             :				{token, {'GLOBALLY', TokenLine}}.
(TABLESPACE|tablespace)         :				{token, {'TABLESPACE', TokenLine}}.
(TEMPORARY|temporary)           :				{token, {'TEMPORARY', TokenLine}}.
(PROFILE|profile)               :				{token, {'PROFILE', TokenLine}}.
(EXPIRE|expite)                 :				{token, {'EXPIRE', TokenLine}}.
(PASSWORD|password)             :				{token, {'PASSWORD', TokenLine}}.
(ACCOUNT|account)               :				{token, {'ACCOUNT', TokenLine}}.
(LOCK|lock)                     :				{token, {'LOCK', TokenLine}}.
(UNLOCK|unlock)                 :				{token, {'UNLOCK', TokenLine}}.
(QUOTA|quota)                   :				{token, {'QUOTA', TokenLine}}.
(UNLIMITED|unlimited)           :				{token, {'UNLIMITED', TokenLine}}.
(ALTER|alter)                   :				{token, {'ALTER', TokenLine}}.
(ENTERPRISE|enterprise)         :				{token, {'ENTERPRISE', TokenLine}}.
(REVOKE|revoke)                 :				{token, {'REVOKE', TokenLine}}.
(THROUGH|through)               :				{token, {'THROUGH', TokenLine}}.
(USERS|users)                   :				{token, {'USERS', TokenLine}}.
(ROLE|role)                     :				{token, {'ROLE', TokenLine}}.
(EXCEPT|except)                 :				{token, {'EXCEPT', TokenLine}}.
(NONE|none)                     :				{token, {'NONE', TokenLine}}.
(CONNECT|connect)               :				{token, {'CONNECT', TokenLine}}.

% sql joins
(UNION|union)					:				{token, {'UNION', TokenLine}}.
(INTERSECT|intersect)			:				{token, {'INTERSECT', TokenLine}}.
(MINUS|minus)					:				{token, {'MINUS', TokenLine}}.

% create options
(CLUSTER|cluster)               :				{token, {'CLUSTER', TokenLine}}.
(BAG|bag)                       :				{token, {'BAG', TokenLine}}.
(ORDERED_SET|ordered_set)       :				{token, {'ORDERED_SET', TokenLine}}.

% erlang funcs
(fun\(\).*end\.)                :               {token, {'STRING', TokenLine, TokenChars}}.

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
(([0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)*))                                                   : {token, {'APPROXNUM', TokenLine, TokenChars}}.
([0-9]+)                                                                                 : {token, {'INTNUM', TokenLine, TokenChars}}.

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

