-module(sql_box).

-export([fold_tree/3, sqlstring/7]).


-include_lib("eunit/include/eunit.hrl").


-define (TEST_PT, 
{select,[{hints,<<>>},
         {opt,[]},
         {fields,[<<"*">>]},
         {into,[]},
         {from,[<<"abc">>]},
         {where,{'and',{'or',{'=',<<"a">>,<<"b">>},{'=',<<"c">>,<<"d">>}},
                       {'or',{'=',<<"e">>,<<"f">>},{'=',<<"g">>,<<"h">>}}}},
         {'group by',[]},
         {having,{}},
         {'order by',[]}]
}
).        

%% -------------------------------
%% Example Sql:
%% select 
%% 	*
%% from 
%% 	abc
%% 	, def
%% where
%% 	(
%% 		a
%% 		=
%% 		b
%% 	or
%% 		c
%% 		=
%% 		d
%% 	)
%% 	and
%% 		e
%% 		=
%% 		f
%% -------------------------------
%% Example ParseTree:
%% {select,[{hints,<<"/*+ index (table column) */">>},
%%          {opt,[]},
%%          {fields,[<<"*">>]},
%%          {into,[]},
%%          {from,[<<"abc">>,<<"def">>]},
%%          {where,{'and',{'or',{'=',<<"a">>,<<"b">>},{'=',<<"c">>,<<"d">>}},
%%                        {'=',<<"e">>,<<"f">>}}},
%%          {'group by',[]},
%%          {having,{}},
%%          {'order by',[]}]
%% }
%% -------------------------------


%% -- in-order parse tree nodes (all others are pre-order nodes) --

-define(inOrderNodes, ['=',is,'<','>','<>','<=','>=',in,between,and,or]).

%% -record('=',{left,right}).
%% -record('is',{left,right}).
%% -record('<',{left,right}).
%% -record('>',{left,right}).
%% -record('<>',{left,right}).
%% -record('<=',{left,right}).
%% -record('>=',{left,right}).
%% -record('in',{left,right}).
%% -record('between',{left,right}).
%% -record('and',{left,right}).
%% -record('or',{left,right}).

% sql_box record

-record(sql_box_rec, {
        name,
        children = []
    }).

fold_tree(ParseTree, Fun, Acc) -> fold_tree(0, 0, undefined, ParseTree, Fun, Acc).

fold_tree(_Indent, _Index, _Parent, {_,[]}, _Fun, Acc) -> 
	Acc;	%% pre-order element with empty content 
fold_tree(_Indent, _Index, _Parent, [], _Fun, Acc) -> 
	Acc;	%% pre-order content traversal complete 
fold_tree(_Indent, _Index, _Parent, {_,<<>>}, _Fun, Acc) -> 
	Acc;	%% pre-order empty hints 
fold_tree(Indent, Index, Parent, [B|Rest], Fun, Acc0) when is_binary(B) ->
	%% pre-order terminal content
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, B]),
	Acc1 = Fun(Indent, Index, Parent, [], visit, B, Acc0),			%% visit list node
	fold_tree(Indent, Index+1, Parent, Rest, Fun, Acc1);			%% visit rest of list
fold_tree(Indent, Index, Parent, B, Fun, Acc) when is_binary(B) ->
	%% in-order terminal content
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, B]),
	Fun(Indent, Index, Parent, [], visit, B, Acc);				%% visit in-order terminal
fold_tree(Indent, Index, Parent, [{Name, Children}|Rest], Fun, Acc0) when is_list(Children) ->
	%% pre-order traversal recurse
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, Children, visit, Name, Acc0),		%% visit node
	NewIndent = Indent+1,
	Acc2 = fold_tree(NewIndent, 0 , Name, Children, Fun, Acc1),		%% visit first child
	fold_tree(Indent, Index+1 , Parent, Rest, Fun, Acc2);			%% visit remaining children	
fold_tree(Indent, Index, Parent, [{Name, {}}|Rest], Fun, Acc0) ->
	%% pre-order to empty in-order transition
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, {}, visit, Name, Acc0),			%% visit pre-order node
	fold_tree(Indent, Index+1, Parent, Rest, Fun, Acc1);			%% resume pre-order processing
fold_tree(Indent, Index, Parent, [{Name, <<>>}|Rest], Fun, Acc0) ->
	%% pre-order empty hints
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, <<>>, visit, Name, Acc0),			%% visit pre-order node
	fold_tree(Indent, Index+1, Parent, Rest, Fun, Acc1);			%% resume pre-order processing
fold_tree(Indent, Index, Parent, [{Name, B}|Rest], Fun, Acc0) when is_binary(B) ->
	%% pre-order hints
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, B, visit, Name, Acc0),			%% visit pre-order node
	Acc2 = Fun(Indent+1, 1, Name, <<>>, visit, B, Acc1),			%% visit pre-order node
	fold_tree(Indent, Index+1, Parent, Rest, Fun, Acc2);			%% resume pre-order processing
fold_tree(Indent, Index, Parent, [{Name, {Child, Unary}}|Rest], Fun, Acc0) ->
	%% pre-order to unary in-order transition
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, {Child, Unary}, visit, Name, Acc0),	%% visit pre-order node
	NewIndent = Indent+1,
	Acc2 = fold_tree(NewIndent, 0, Name, {Child, Unary}, Fun, Acc1),	%% visit in-order unary child
	fold_tree(Indent, Index+1, Parent, Rest, Fun, Acc2);			%% resume pre-order processing
fold_tree(Indent, Index, Parent, [{Name, {Child, Left, Right}}|Rest], Fun, Acc0) ->
	%% pre-order to binary in-order transition
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, {Child, Left, Right}, visit, Name, Acc0),	%% visit pre-order node
	NewIndent = Indent+1,
	Acc2 = fold_tree(NewIndent, 0, Name, {Child, Left, Right}, Fun, Acc1),	%% visit in-order children
	fold_tree(Indent, Index+1, Parent, Rest, Fun, Acc2);			%% resume pre-order processing
fold_tree(Indent, _Index, Parent, {Name, Left, Right}, Fun, Acc0) ->
	%% in-order recurse
	case binding(Left) > binding(Name) of
		true	->
			Acc1 = Fun(Indent, 0, Name, undefined, suffix, Left, 
				fold_tree(Indent, 1, Name, Left, Fun, 
					Fun(Indent, 0, Name, undefined, prefix, Left, Acc0)
					)
				);
		false	->
			Acc1 = fold_tree(Indent, 1, Name, Left, Fun, Acc0)		%% visit left child
	end,
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, 0, Parent, Name]),
	Acc2 = Fun(Indent, 0, Parent, {Left, Right}, visit, Name, Acc1),		%% visit middle node
	case binding(Right) > binding(Name) of
		true	->
			Fun(Indent, 0, Name, undefined, suffix, Right,
				fold_tree(Indent, 2, Name, Right, Fun, 
				Fun(Indent, 0, Name, undefined, prefix, Right, Acc2)	%% visit right child
				)
			);
		false	->
			fold_tree(Indent, 2, Name, Right, Fun, Acc2)		%% visit right child
	end;
fold_tree(Indent, Index, Parent, {Name, List}, Fun, Acc0) when is_list(List) ->
	%% pre-order traversal recurse
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, Index, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, List, visit, Name, Acc0),			%% visit node
	fold_tree(Indent, 0 , Name, List, Fun, Acc1);				%% visit children	
fold_tree(Indent, _Index, Parent, {Name, Unary}, Fun, Acc0) when is_tuple(Unary) ->
	%% in-order unary recurse
   io:format(user, "~n~p,~p,~p,~p~n", [Indent, 0, Parent, Name]),
	Acc1 = Fun(Indent, 0, Parent, Unary, visit, Name, Acc0),		%% visit node
	fold_tree(Indent, 1, Name, Unary, Fun, Acc1);				%% visit unary child
fold_tree(_Indent, _Index, _Parent, T, _Fun, Acc)-> 
	%% catch remaining
   io:format(user, "~n----remaining term---------~n~p~n", [T]),
	Acc. 

binding('not') -> 3;
binding('and') -> 2;
binding('or') -> 1;
binding(_) -> 0.

sql_box(_Indent, _Index, _Parent, _Children, prefix, _, Acc)                     -> Acc ++ "(" ;
sql_box(_Indent, _Index, _Parent, _Children, suffix, _, Acc)                     -> Acc ++ ")" ;
sql_box(_Indent, _Index, _Parent, _Children, visit, hints, Acc)                  -> Acc;
sql_box(_Indent, _Index, _Parent, _Children, visit, fields, Acc)                 -> Acc;
sql_box(_Indent, _Index, _Parent, [], visit, A, Acc) when is_atom(A)             -> Acc;
sql_box(_Indent, _Index, _Parent, {}, visit, A, Acc) when is_atom(A)             -> Acc;
sql_box(_Indent, _Index, _Parent, <<>>, visit, A, Acc) when is_atom(A)           -> Acc;
sql_box(_Indent, _Index, _Parent, _Children, visit, A, Acc) when is_atom(A)      -> Acc#sql_box_rec{name=atom_to_list(A)};
sql_box(_Indent, _Index, _Parent, _Children, visit, A, Acc) when is_atom(A)      -> Acc ++ " " ++ atom_to_list(A);
sql_box(_Indent, 0, _Parent, _Children, visit, B, Acc) when is_binary(B)         -> Acc#sql_box_rec{children=Acc#sql_box_rec.children ++ [#sql_box_rec{name=binary_to_list(B)}]};
sql_box(_Indent, _Index, 'fields', _Children, visit, B, Acc) when is_binary(B)   -> Acc ++ " ," ++ binary_to_list(B);
sql_box(_Indent, _Index, 'from', _Children, visit, B, Acc) when is_binary(B)     -> Acc ++ " ," ++ binary_to_list(B);
sql_box(_Indent, _Index, 'group by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sql_box(_Indent, _Index, 'order by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sql_box(_Indent, _Index, _Parent, _Children, visit, B, Acc) when is_binary(B)    -> Acc ++ " " ++ binary_to_list(B);
sql_box(_Indent, _Index, _Parent, _Children, visit, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.


sqlstring(_Indent, _Index, _Parent, _Children, prefix, _, Acc) -> Acc ++ "(" ;
sqlstring(_Indent, _Index, _Parent, _Children, suffix, _, Acc) -> Acc ++ ")" ;
sqlstring(_Indent, _Index, _Parent, _Children, visit, hints, Acc) -> Acc;
sqlstring(_Indent, _Index, _Parent, _Children, visit, fields, Acc) -> Acc;
sqlstring(_Indent, _Index, _Parent, [], visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Indent, _Index, _Parent, {}, visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Indent, _Index, _Parent, <<>>, visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Indent, _Index, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqlstring(_Indent, _Index, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqlstring(_Indent, 0, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlstring(_Indent, _Index, 'fields', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Indent, _Index, 'from', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Indent, _Index, 'group by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Indent, _Index, 'order by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Indent, _Index, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlstring(_Indent, _Index, _Parent, _Children, visit, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.
	
	
sql_test() -> 
       	io:format(user, "--fold_tree()--sql_test()------~nParseTree:~n", []),
      	io:format(user, "~p~n", [?TEST_PT]),
	Sql = fold_tree(?TEST_PT, fun sqlstring/7, []),
       	io:format(user, "-------------------------------~nSql:~n", []),
      	io:format(user, "~p~n", [Sql]),
    %Sql2 = fold_tree(?TEST_PT, fun sql_box/7, #sql_box_rec{}),
    %   	io:format(user, "-------------------------------~nSql:~n", []),
    %  	io:format(user, "~p~n", [Sql2]),
	ok.

