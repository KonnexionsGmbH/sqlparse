Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email bikram.chatterjee@k2informatics.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 sql_list
 schema
 opt_schema_element_list
 schema_element_list
 schema_element
 base_table_def
 drop_table_def
 base_table_element_commalist
 base_table_element
 column_def
 column_def_opt_list
 column_def_opt
 table_constraint_def
 column_commalist
 view_def
 opt_with_check_option
 opt_column_commalist
 privilege_def
 opt_with_grant_option
 privileges
 operation_commalist
 operation
 grantee_commalist
 grantee
 cursor_def
 opt_order_by_clause
 ordering_spec_commalist
 ordering_spec
 opt_asc_desc
 manipulative_statement
 close_statement
 commit_statement
 delete_statement_positioned
 delete_statement_searched
 fetch_statement
 insert_statement
 values_or_query_spec
 insert_atom_commalist
 insert_atom
 open_statement
 rollback_statement
 select_statement
 opt_all_distinct
 update_statement_positioned
 assignment_commalist
 assignment
 update_statement_searched
 target_commalist
 target
 opt_where_clause
 query_exp
 query_term
 query_spec
 selection
 table_exp
 from_clause
 table_ref_commalist
 table_ref
 where_clause
 opt_group_by_clause
 column_ref_commalist
 opt_having_clause
 search_condition
 predicate
 comparison_predicate
 between_predicate
 like_predicate
 opt_escape
 test_for_null
 in_predicate
 all_or_any_predicate
 any_all_some
 existence_test
 subquery
 scalar_exp
 scalar_sub_exp
 scalar_exp_commalist
 atom
 parameter_ref
 function_ref
 %concat_list
 fun_args
 literal
 table
 column_ref
 data_type
 column
 cursor
 parameter
 range_variable
 user
 sql
 when_action
 opt_hint
 table_list
 opt_exists
 opt_restrict_cascade
.

    %% symbolic tokens
    %% literal keyword tokens

%'LANGUAGE' 'PROCEDURE' 'SQLCODE'
Terminals
 NAME
 STRING
 INTNUM
 APPROXNUM
 COMPARISON
 ALL
 FUNS
 AMMSC
 ANY
 AS
 ASC
 AUTHORIZATION
 BETWEEN
 BY
 CHARACTER
 VARCHARACTER
 CHECK
 CLOSE
 COMMIT
 CONTINUE
 CREATE
 CURRENT
 CURSOR
 DECIMAL
 DECLARE
 DEFAULT
 DELETE
 DESC
 DISTINCT
 DOUBLE
 ESCAPE
 EXISTS
 DROP
 IF
 RESTRICT
 CASCADE
 FETCH
 FLOAT
 FOR
 FOREIGN
 FOUND
 FROM
 GOTO
 GRANT
 GROUP
 HAVING
 IN
 INDICATOR
 INSERT
 INTEGER
 INTO
 IS
 KEY
 LIKE
 NULLX
 NUMERIC
 OF
 ON
 OPEN
 OPTION
 ORDER
 PARAMETER
 PRECISION
 PRIMARY
 PRIVILEGES
 PUBLIC
 REAL
 REFERENCES
 ROLLBACK
 SCHEMA
 SELECT
 SET
 SMALLINT
 SOME
 SQLERROR
 TABLE
 TO
 UNION
 UNIQUE
 UPDATE
 USER
 VALUES
 VIEW
 WHENEVER
 WHERE
 WITH
 WORK
 %COMMENT
 HINT
 'AND'
 'NOT'
 'OR'
 '+'
 '-'
 '*'
 '/'
 ';'
 '('
 ')'
 ','
 '='
 '.'
.

Rootsymbol sql_list.


    %% operators

Left        110 'OR'.
Left        120 'AND'.
Left        130 'NOT'.
Nonassoc    200 COMPARISON. %% = <> < > <= >=
Left        300 '+' '-'.
Left        400 '*' '/'.
%Unary       500 '-'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sql_list -> sql ';'                                                                             : ['$1'].
sql_list -> sql_list sql ';'                                                                    : '$1' ++ ['$2'].


    %% schema definition language
sql -> schema                                                                                   : '$1'.
   
schema -> CREATE SCHEMA AUTHORIZATION user opt_schema_element_list                              : {'create_schema_auth', '$4', '$5'}.

opt_schema_element_list -> '$empty'                                                             : [].
opt_schema_element_list -> schema_element_list                                                  : '$1'.

schema_element_list -> schema_element                                                           : ['$1'].
schema_element_list -> schema_element_list schema_element                                       : '$1' ++ ['$2'].

schema_element -> base_table_def                                                                : '$1'.
schema_element -> view_def                                                                      : '$1'.
schema_element -> privilege_def                                                                 : '$1'.

base_table_def -> CREATE TABLE table '(' base_table_element_commalist ')'                       : {'create_table', '$3', '$5'}.
drop_table_def -> DROP TABLE opt_exists table_list opt_restrict_cascade                         : list_to_tuple(['drop_table', {'tables', '$4'}] ++ '$3' ++ '$5').

table_list -> table                                                                             : ['$1'].
table_list -> table_list ',' table                                                              : '$1' ++ ['$3'].

opt_exists -> '$empty'                                                                          : [{'exists', 'false'}].
opt_exists -> IF EXISTS                                                                         : [{'exists', 'true'}].

opt_restrict_cascade -> '$empty'                                                                : [{'opt', 'restrict'}].
opt_restrict_cascade -> RESTRICT                                                                : [{'opt', 'restrict'}].
opt_restrict_cascade -> CASCADE                                                                 : [{'opt', 'cascade'}].

base_table_element_commalist -> base_table_element                                              : ['$1'].
base_table_element_commalist -> base_table_element_commalist ',' base_table_element             : '$1' ++ ['$3'].

base_table_element -> column_def                                                                : '$1'.
base_table_element -> table_constraint_def                                                      : '$1'.

column_def -> column data_type column_def_opt_list                                              : {'$1', '$2'}.

column_def_opt_list -> '$empty'                                                                 : [].
column_def_opt_list -> column_def_opt_list column_def_opt                                       : '$1' ++ ['$2'].

column_def_opt -> NOT NULLX                                                                     : 'not_nullx'.
column_def_opt -> NOT NULLX UNIQUE                                                              : 'not_nullx_unique'.
column_def_opt -> NOT NULLX PRIMARY KEY                                                         : 'not_nullx_primary_key'.
column_def_opt -> DEFAULT literal                                                               : {'default', '$2'}.
column_def_opt -> DEFAULT NULLX                                                                 : {'default', 'nullx'}.
column_def_opt -> DEFAULT USER                                                                  : {'default', 'user'}.
column_def_opt -> CHECK '(' search_condition ')'                                                : {'check', '$3'}.
column_def_opt -> REFERENCES table                                                              : {'ref', '$2'}.
column_def_opt -> REFERENCES table '(' column_commalist ')'                                     : {'ref', {'$2', '$4'}}.

table_constraint_def -> UNIQUE '(' column_commalist ')'                                         : {'unique', '$3'}.
table_constraint_def -> PRIMARY KEY '(' column_commalist ')'                                    : {'primary_key', '$4'}.
table_constraint_def -> FOREIGN KEY '(' column_commalist ')' REFERENCES table                   : {'foreign_key', '$4', {'ref', '$7'}}.
table_constraint_def ->
            FOREIGN KEY '(' column_commalist ')' REFERENCES table '(' column_commalist ')'      : {'foreign_key', '$4', {'ref', {'$7', '$9'}}}.
table_constraint_def -> CHECK '(' search_condition ')'                                          : {'check', '$3'}.

column_commalist -> column                                                                      : ['$1'].
column_commalist -> column_commalist ',' column                                                 : '$1' ++ ['$3'].

view_def -> CREATE VIEW table opt_column_commalist                                              : {'create_view', '$3', '$4'}.
view_def -> AS query_spec opt_with_check_option                                                 : {'as', '$2', '$3'}.
   
opt_with_check_option -> '$empty'                                                               : [].
opt_with_check_option -> WITH CHECK OPTION                                                      : 'with_check_opt'.

opt_column_commalist -> '$empty'                                                                : [].
opt_column_commalist -> '(' column_commalist ')'                                                : '$2'.

privilege_def -> GRANT privileges ON table TO grantee_commalist opt_with_grant_option           : {'grant', '$2', {'on', '$4'}, {'to', '$6'}, '$7'}.

opt_with_grant_option -> '$empty'                                                               : [].
opt_with_grant_option -> WITH GRANT OPTION                                                      : 'with_grant_opt'.

privileges -> ALL PRIVILEGES                                                                    : 'all_privileges'.
privileges -> ALL                                                                               : 'all'.
privileges -> operation_commalist                                                               : '$1'.

operation_commalist -> operation                                                                : ['$1'].
operation_commalist -> operation_commalist ',' operation                                        : '$1' ++ ['$3'].

operation -> SELECT                                                                             : 'select'.
operation -> INSERT                                                                             : 'insert'.
operation -> DELETE                                                                             : 'delete'.
operation -> UPDATE opt_column_commalist                                                        : {'update', '$2'}.
operation -> REFERENCES opt_column_commalist                                                    : {'referances', '$2'}.


grantee_commalist -> grantee                                                                    : ['$1'].
grantee_commalist -> grantee_commalist ',' grantee                                              : '$1' ++ ['$3'].

grantee -> PUBLIC                                                                               : 'public'.
grantee -> user                                                                                 : '$1'.

    %% cursor definition

sql -> cursor_def                                                                               : '$1'.


cursor_def -> DECLARE cursor CURSOR FOR query_exp opt_order_by_clause                           : {'declare', '$2', {'cur_for', '$5'}, '$6'}.

opt_order_by_clause -> '$empty'                                                                 : {'order by', []}.
opt_order_by_clause -> ORDER BY ordering_spec_commalist                                         : {'order by', '$3'}.

ordering_spec_commalist -> ordering_spec                                                        : ['$1'].
ordering_spec_commalist -> ordering_spec_commalist ',' ordering_spec                            : '$1' ++ ['$3'].

ordering_spec -> INTNUM opt_asc_desc                                                            : {'intnum', '$2'}.
ordering_spec -> column_ref opt_asc_desc                                                        : {'$1', '$2'}.
ordering_spec -> function_ref opt_asc_desc                                                      : {'$1', '$2'}.

opt_asc_desc -> '$empty'                                                                        : <<>>.
opt_asc_desc -> ASC                                                                             : <<"asc">>.
opt_asc_desc -> DESC                                                                            : <<"desc">>.

    %% manipulative statements

sql -> manipulative_statement                                                                   : '$1'.

manipulative_statement -> close_statement                                                       : '$1'.
manipulative_statement -> commit_statement                                                      : '$1'.
manipulative_statement -> delete_statement_positioned                                           : '$1'.
manipulative_statement -> delete_statement_searched                                             : '$1'.
manipulative_statement -> fetch_statement                                                       : '$1'.
manipulative_statement -> insert_statement                                                      : '$1'.
manipulative_statement -> open_statement                                                        : '$1'.
manipulative_statement -> rollback_statement                                                    : '$1'.
manipulative_statement -> select_statement                                                      : '$1'.
manipulative_statement -> update_statement_positioned                                           : '$1'.
manipulative_statement -> update_statement_searched                                             : '$1'.
manipulative_statement -> base_table_def                                                        : '$1'.
manipulative_statement -> drop_table_def                                                        : '$1'.
manipulative_statement -> view_def                                                              : '$1'.

close_statement -> CLOSE cursor                                                                 : {'close', '$2'}.

commit_statement -> COMMIT WORK                                                                 : 'commit_work'.

delete_statement_positioned -> DELETE FROM table WHERE CURRENT OF cursor                        : {'delete', '$3',{'where_current_of', '$7'}}.

delete_statement_searched -> DELETE FROM table opt_where_clause                                 : {'delete', '$3', '$4'}.

fetch_statement -> FETCH cursor INTO target_commalist                                           : {'fetch', '$2', {'into', '$4'}}.

insert_statement -> INSERT INTO table opt_column_commalist values_or_query_spec                 : {'insert', '$3', {cols, '$4'}, '$5'}.

values_or_query_spec -> VALUES '(' insert_atom_commalist ')'                                    : {'values', '$3'}.
values_or_query_spec -> query_spec                                                              : '$1'.

insert_atom_commalist -> insert_atom                                                            : ['$1'].
insert_atom_commalist -> insert_atom_commalist ',' insert_atom                                  : '$1' ++ ['$3'].

insert_atom -> atom                                                                             : '$1'.
insert_atom -> NULLX                                                                            : 'nullx'.

open_statement -> OPEN cursor                                                                   : {'open', '$2'}.

rollback_statement -> ROLLBACK WORK                                                             : 'rollback_work'.

select_statement -> SELECT opt_hint opt_all_distinct selection INTO target_commalist table_exp
                 : case '$2' of
                     {hint, ""} -> list_to_tuple([select, ['$3', '$4', '$6'] ++ '$7']);
                     _          -> list_to_tuple([select, ['$2', '$3', '$4', '$6'] ++ '$7'])
                   end.
select_statement -> query_spec                                                                  : '$1'.

opt_hint -> '$empty'                                                                            : {hints, <<>>}.
opt_hint -> HINT                                                                                : {hints, unwrap_bin('$1')}.

opt_all_distinct -> '$empty'                                                                    : {opt, <<>>}.
opt_all_distinct -> ALL                                                                         : {opt, <<"all">>}.
opt_all_distinct -> DISTINCT                                                                    : {opt, <<"distinct">>}.

update_statement_positioned -> UPDATE table SET assignment_commalist WHERE CURRENT OF cursor    : {'update', '$2', {'set', '$4'}, {'where_cur_of', '$8'}}.

assignment_commalist -> assignment                                                              : ['$1'].
assignment_commalist -> assignment_commalist ',' assignment                                     : '$1' ++ ['$3'].

assignment -> column '=' scalar_exp                                                             : {'=', '$1', '$3'}.

update_statement_searched -> UPDATE table SET assignment_commalist opt_where_clause             : {'update', '$2', {'set', '$4'}, '$5'}.

target_commalist -> target                                                                      : ['$1'].
target_commalist -> target_commalist ',' target                                                 : '$1' ++ ['$3'].

target -> parameter_ref                                                                         : '$1'.

opt_where_clause -> '$empty'                                                                    : {'where', []}.
opt_where_clause -> where_clause                                                                : '$1'.

    %% query expressions

query_exp -> query_term                                                                         : '$1'.
query_exp -> query_exp UNION query_term                                                         : {union, '$1', '$3'}.
query_exp -> query_exp UNION ALL query_term                                                     : {union_all, '$1', '$3'}.

query_term -> query_spec                                                                        : '$1'.
query_term -> '(' query_exp ')'                                                                 : '$2'.

query_spec -> SELECT opt_hint opt_all_distinct selection table_exp
           : case '$2' of
               {hint, ""} -> list_to_tuple([select, ['$3', {fields, '$4'}, {into, []}] ++ '$5']);
               _          -> list_to_tuple([select, ['$2', '$3', {fields, '$4'}, {into, []}] ++ '$5'])
             end.

selection -> scalar_exp_commalist                                                               : '$1'.
selection -> '*'                                                                                : [<<"*">>].

table_exp ->
     from_clause opt_where_clause opt_group_by_clause opt_having_clause opt_order_by_clause     : ['$1', '$2', '$3', '$4', '$5'].

from_clause -> FROM table_ref_commalist                                                         : {from, '$2'}.

table_ref_commalist -> table_ref                                                                : ['$1'].
table_ref_commalist -> table_ref_commalist ',' table_ref                                        : '$1' ++ ['$3'].

table_ref -> table                                                                              : '$1'.
table_ref -> '(' query_spec ')'                                                                 : '$2'.
table_ref -> table range_variable                                                               : {'$1', '$2'}.

where_clause -> WHERE search_condition                                                          : {'where', '$2'}.

opt_group_by_clause  -> '$empty'                                                                : {'group by', []}.
opt_group_by_clause  -> GROUP BY column_ref_commalist                                           : {'group by', '$2'}.

column_ref_commalist -> column_ref                                                              : ['$1'].
column_ref_commalist -> column_ref_commalist ',' column_ref                                     : '$1' ++ ['$3'].

opt_having_clause -> '$empty'                                                                   : {'having', {}}.
opt_having_clause -> HAVING search_condition                                                    : {'having', '$2'}.

    %% search conditions

search_condition -> search_condition OR search_condition                                        : {'or', '$1', '$3'}.
search_condition -> search_condition AND search_condition                                       : {'and', '$1', '$3'}.
search_condition -> NOT search_condition                                                        : {'not', '$2'}.
search_condition -> '(' search_condition ')'                                                    : '$2'.
search_condition -> predicate                                                                   : '$1'.

predicate -> comparison_predicate                                                               : '$1'.
predicate -> between_predicate                                                                  : '$1'.
predicate -> like_predicate                                                                     : '$1'.
predicate -> test_for_null                                                                      : '$1'.
predicate -> in_predicate                                                                       : '$1'.
predicate -> all_or_any_predicate                                                               : '$1'.
predicate -> existence_test                                                                     : '$1'.

comparison_predicate -> scalar_exp COMPARISON scalar_exp                                        : {unwrap('$2'), '$1', '$3'}.
comparison_predicate -> scalar_exp COMPARISON subquery                                          : {unwrap('$2'), '$1', '$3'}.

between_predicate -> scalar_exp NOT BETWEEN scalar_exp AND scalar_exp                           : {'not', {'between', '$1', '$4', '$6'}}.
between_predicate -> scalar_exp BETWEEN scalar_exp AND scalar_exp                               : {'between', '$1', '$3', '$5'}.

like_predicate -> scalar_exp NOT LIKE atom opt_escape                                           : {'not', {'like', '$1', '$4', '$5'}}.
like_predicate -> scalar_exp LIKE atom opt_escape                                               : {'like', '$1', '$3', '$4'}.

opt_escape -> '$empty'                                                                          : <<>>.
opt_escape -> ESCAPE atom                                                                       : '$2'.

test_for_null -> column_ref IS NOT NULLX                                                        : {'not', {'is', '$1', <<"null">>}}.
test_for_null -> column_ref IS NULLX                                                            : {'is', '$1', <<"null">>}.

in_predicate -> scalar_exp NOT IN '(' subquery ')'                                              : {'not', {'in', '$1', '$5'}}.
in_predicate -> scalar_exp IN '(' subquery ')'                                                  : {'in', '$1', '$4'}.
in_predicate -> scalar_exp NOT IN '(' scalar_exp_commalist ')'                                  : {'not', {'in', '$1', {'list', '$5'}}}.
in_predicate -> scalar_exp IN '(' scalar_exp_commalist ')'                                      : {'in', '$1', {'list', '$4'}}.

all_or_any_predicate -> scalar_exp COMPARISON any_all_some subquery                             : {unwrap('$2'), '$1', {'$3', '$4'}}.
           
any_all_some -> ANY                                                                             : 'ANY'.
any_all_some -> ALL                                                                             : 'ALL'.
any_all_some -> SOME                                                                            : 'SOME'.

existence_test -> EXISTS subquery                                                               : {exists, '$2'}.

subquery -> query_spec                                                                          : '$1'.

    %% scalar expressions

scalar_exp -> scalar_sub_exp                                                                    : '$1'.
scalar_exp -> scalar_sub_exp NAME                                                               : {as, '$1', unwrap_bin('$2')}. 
scalar_exp -> scalar_sub_exp AS NAME                                                            : {as, '$1', unwrap_bin('$3')}. 

scalar_sub_exp -> scalar_sub_exp '+' scalar_sub_exp                                             : {'+','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '-' scalar_sub_exp                                             : {'-','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '*' scalar_sub_exp                                             : {'*','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '/' scalar_sub_exp                                             : {'/','$1','$3'}.
scalar_sub_exp -> '+' scalar_sub_exp                                                            : {'+','$2'}. %prec UMINU
scalar_sub_exp -> '-' scalar_sub_exp                                                            : {'-','$2'}. %prec UMINU
scalar_sub_exp -> '+' literal                                                                   : '$2'.
scalar_sub_exp -> '-' literal                                                                   : list_to_binary(["-",'$2']).
scalar_sub_exp -> NULLX                                                                         : <<"NULL">>.
scalar_sub_exp -> atom                                                                          : '$1'.
scalar_sub_exp -> subquery                                                                      : '$1'.
scalar_sub_exp -> column_ref                                                                    : '$1'.
scalar_sub_exp -> function_ref                                                                  : '$1'.
scalar_sub_exp -> '(' scalar_sub_exp ')'                                                        : '$2'.

scalar_exp_commalist -> scalar_exp                                                              : ['$1'].
scalar_exp_commalist -> scalar_exp_commalist ',' scalar_exp                                     : '$1' ++ ['$3'].

atom -> parameter_ref                                                                           : '$1'.
atom -> literal                                                                                 : '$1'.
atom -> USER                                                                                    : <<"user">>.

parameter_ref -> parameter                                                                      : '$1'.
parameter_ref -> parameter parameter                                                            : {'$1', '$2'}.
parameter_ref -> parameter INDICATOR parameter                                                  : {'indicator', '$1', '$3'}.

function_ref -> NAME  '(' fun_args ')'                                                          : {'fun', list_to_atom(unwrap('$1')), '$3'}.
function_ref -> FUNS  '(' fun_args ')'                                                          : {'fun', unwrap('$1'), '$3'}.

function_ref -> AMMSC '(' '*' ')'                                                               : {'fun', unwrap('$1'), [<<"*">>]}.
function_ref -> AMMSC '(' DISTINCT column_ref ')'                                               : {'fun', unwrap('$1'), {'distinct', '$4'}}.
function_ref -> AMMSC '(' ALL scalar_exp ')'                                                    : {'fun', unwrap('$1'), {'all', '$4'}}.
function_ref -> AMMSC '(' scalar_exp ')'                                                        : {'fun', unwrap('$1'), {'$4'}}.

fun_args -> NAME                                                                                : [unwrap_bin('$1')].
fun_args -> STRING                                                                              : [unwrap_bin('$1')].
fun_args -> INTNUM                                                                              : [unwrap_bin('$1')].
fun_args -> APPROXNUM                                                                           : [unwrap_bin('$1')].
fun_args -> function_ref                                                                        : ['$1'].
fun_args -> fun_args ',' fun_args                                                               : '$1' ++ '$3'.

literal -> STRING                                                                               : unwrap_bin('$1').
literal -> INTNUM                                                                               : unwrap_bin('$1').
literal -> APPROXNUM                                                                            : unwrap_bin('$1').

    %% miscellaneous

table -> NAME                                                                                   : unwrap_bin('$1').
table -> NAME '.' NAME                                                                          : list_to_binary(unwrap('$1') ++ "." ++ unwrap('$3')).

column_ref -> NAME                                                                              : unwrap_bin('$1').
column_ref -> NAME '.' NAME                                                                     : list_to_binary(unwrap('$1') ++ "." ++ unwrap('$3')).
column_ref -> NAME '.' NAME '.' NAME                                                            : list_to_binary(unwrap('$1') ++ "." ++ unwrap('$3') ++ "." ++ unwrap('$5')).

        %% data types

data_type -> CHARACTER                                                                          : 'char'.
data_type -> CHARACTER '(' INTNUM ')'                                                           : {'char', unwrap('$3')}.
data_type -> VARCHARACTER '(' INTNUM ')'                                                        : {'varchar', unwrap('$3')}.
data_type -> NUMERIC                                                                            : 'num'.
data_type -> NUMERIC '(' INTNUM ')'                                                             : {'num', unwrap('$3')}.
data_type -> NUMERIC '(' INTNUM ',' INTNUM ')'                                                  : {'num', unwrap('$3'), unwrap('$5')}.
data_type -> DECIMAL                                                                            : 'dec'.
data_type -> DECIMAL '(' INTNUM ')'                                                             : {'dec', unwrap('$3')}.
data_type -> DECIMAL '(' INTNUM ',' INTNUM ')'                                                  : {'dec', unwrap('$3'), unwrap('$5')}.
data_type -> INTEGER                                                                            : 'int'.
data_type -> SMALLINT                                                                           : 'smallint'.
data_type -> FLOAT                                                                              : 'float'.
data_type -> FLOAT '(' INTNUM ')'                                                               : {'float', unwrap('$3')}.
data_type -> REAL                                                                               : 'real'.
data_type -> DOUBLE PRECISION                                                                   : 'double'.

    %% the various things you can name

column -> NAME                                                                                  : unwrap_bin('$1').

cursor -> NAME                                                                                  : {'cur', unwrap('$1')}.

parameter -> PARAMETER                                                                          : {'param', unwrap('$1')}.

range_variable -> NAME                                                                          : unwrap('$1').

user -> NAME                                                                                    : {'user', unwrap('$1')}.

    %% embedded condition things

sql -> WHENEVER NOT FOUND when_action                                                           : {'when_not_found', '$1'}.
sql -> WHENEVER SQLERROR when_action                                                            : {'when_sql_err', '$1'}.

when_action -> GOTO NAME                                                                        : {'goto', unwrap('$2')}.
when_action -> CONTINUE                                                                         : 'continue'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

-export([collapse/1, clean_cr/1, trim_nl/1, str_diff/2]).

-include_lib("eunit/include/eunit.hrl").
-include("sql_tests.hrl").

unwrap({_,_,X}) -> X.
unwrap_bin({_,_,X}) -> list_to_binary(X).

-define(REG_COL, [
    {"([\n\r\t ]+)",                              " "}    % \r,\n or spaces               -> single space
  , {"(^[ ]+)|([ ]+$)",                           ""}     % leading or trailing spaces    -> removed
  , {"([ ]*)([\(\),])([ ]*)",                     "\\2"}  % spaces before or after ( or ) -> removed
  , {"([ ]*)([\\/\\*\\+\\-\\=\\<\\>]+)([ ]*)",    "\\2"}  % spaces around math operators  -> removed
% , {"([\)])([ ]*)",                              "\\1 "} % no space after )              -> added one space
]).

-define(REG_CR, [
    {"(\r)",                 		""}     % all carriage returns		-> removed
]).

-define(REG_NL, [
    {"(^[\r\n]+)",             		""}     % leading newline    		-> removed
  , {"([\r\n]+$)",             		""}     % trailing newline    		-> removed
]).

str_diff([], [])                                            -> same;
str_diff(String1, []) when length(String1) > 0              -> {String1, ""};
str_diff([], String2) when length(String2) > 0              -> {"", String2};
str_diff([S0|_] = String1, [S1|_] = String2) when S0 =/= S1 -> {String1, String2};
str_diff([_|String1], [_|String2])                          -> str_diff(String1, String2).

collapse(Sql) ->
    lists:foldl(
        fun({Re,R}, S) -> re:replace(S, Re, R, [{return, list}, global]) end,
        Sql,
        ?REG_COL).

clean_cr(Sql) ->
    lists:foldl(
        fun({Re,R}, S) -> re:replace(S, Re, R, [{return, list}, global]) end,
        Sql,
        ?REG_CR).

trim_nl(Sql) ->
    lists:foldl(
        fun({Re,R}, S) -> re:replace(S, Re, R, [{return, list}, global]) end,
        Sql,
        ?REG_NL).

%remove_eva(S) ->
%	re:replace(S, "([ \t]eva[ \t])", "\t\t", [global, {return, list}]).



parse_test() ->
    io:format(user, "===============================~n", []),
    io:format(user, "|    S Q L   P A R S I N G    |~n", []),
    io:format(user, "===============================~n", []),
    test_parse(?TEST_SQLS, 0).
test_parse([], _) -> ok;
test_parse([Sql|Sqls], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    io:format(user, "Sql collapsed:~n~p~n", [collapse(Sql)]),
    case (catch sql_lex:string(Sql ++ ";")) of
        {ok, Tokens, _} ->
            case (catch sql_parse:parse(Tokens)) of
                {ok, [ParseTree|_]} -> 
                	io:format(user, "-------------------------------~nParseTree:~n", []),
                	io:format(user, "~p~n", [ParseTree]),
                	io:format(user, "-------------------------------~n", []);
                {'EXIT', Error} ->
                    io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
                    ?assertEqual(ok, Error);
                Error ->
                    io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
                    ?assertEqual(ok, Error)
            end;
        {'EXIT', Error} ->
            io:format(user, "Failed ~p~n", [Error]),
            ?assertEqual(ok, Error)
    end,
    test_parse(Sqls, N+1).
