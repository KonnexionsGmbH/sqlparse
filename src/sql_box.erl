-module(sql_box).

-export([fold_tree/3, sqls/7]).

-export([test_sqls/0, test_sqlp/0]).

-include_lib("eunit/include/eunit.hrl").
-include("sql_tests.hrl").


binding(A) when is_binary(A) -> 9;
binding('fun') -> 8;
binding('*') -> 7;
binding('/') -> 7;
binding('+') -> 6;
binding('-') -> 6;
binding('=') -> 5;
binding('<=') -> 5;
binding('>=') -> 5;
binding('<>') -> 5;
binding('<') -> 5;
binding('>') -> 5;
binding('like') -> 5;
binding('is') -> 5;
binding('between') -> 5;
binding('in') -> 4;
binding('not') -> 3;
binding('and') -> 2;
binding('or') -> 1;
binding({A,_}) -> binding(A);
binding({A,_,_}) -> binding(A);
binding({A,_,_,_}) -> binding(A);
binding(_) -> 0.

ct([]) -> [];
ct(L) when is_list(L) -> '[_]';
ct(<<>>) -> <<>>;
ct({}) -> {};
ct({_A}) -> '{_}';
ct({_A,_B}) -> '{_,_}';
ct({_A,_B,_C}) -> '{_,_,_}';
ct(A) when is_atom(A) -> A;
ct(B) when is_binary(B) -> B;
ct(X) -> X.

fold_tree(ParseTree, Fun, Acc) -> fold_tree(0, 0, undefined, ParseTree, undefined, Fun, Acc).

fold_tree(_Ind, _Idx, _Parent, [], _Children, _Fun, Acc) -> Acc;			%% pre-order list traversal complete 

fold_tree(Ind, Idx, Parent, B, Ch, Fun, Acc0) when is_binary(B) ->			%% binary terminal
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, Idx, B, Parent, ct(Ch)]),
	Fun(Ind, Idx, Parent, <<>>, visit, B, Acc0);						

fold_tree(Ind, Idx, Parent, A, Ch, Fun, Acc0) when is_atom(A) ->			%% atomic terminal
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, Idx, A, Parent, ct(Ch)]),
	Fun(Ind, Idx, Parent, Ch, visit, A, Acc0);						

fold_tree(Ind, Idx, Parent, {Node, List}, _, Fun, Acc0) when is_list(List) -> 	%% pre-order traversal recurse
	Acc1 = fold_tree(Ind, Idx, Parent, Node, List, Fun, Acc0),	
	fold_tree(Ind+1, 0 , Node, List, undefined, Fun, Acc1);
	
fold_tree(Ind, Idx, Parent, {Node, B}, _, Fun, Acc0) when is_binary(B) ->	%% pre-order recurse for 'hints', 'opt' and 'order by'
	Acc1 = fold_tree(Ind, Idx , Parent, Node, B, Fun, Acc0),		 		
	Fun(Ind, 0, Parent, <<>>, visit, B, Acc1);					
	
fold_tree(Ind, Idx, Parent, {Node, {}}, Ch, Fun, Acc0) ->					%% pre-order -> empty in-order for 'having'
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind+1, Idx, Node, Parent, ct(Ch)]),
	Fun(Ind+1, 0, Parent, {}, visit, Node, Acc0);			
	
fold_tree(Ind, 0, Parent, {Node, Unary}, _, Fun, Acc0) when is_tuple(Unary) ->	%% in-order unary recurse
	Acc1 = fold_tree(Ind+1, 0 , Parent, Node, Unary, Fun, Acc0),		 		
	fold_in(Ind+1, 0, Node, Unary, undefined, Fun, Acc1);			

fold_tree(Ind, Idx, Parent, {Node, T}, _, Fun, Acc0) when is_tuple(T) ->	%% pre-order to in-order transition
	Acc1 = fold_tree(Ind, Idx , Parent, Node, T, Fun, Acc0),		 		
	fold_in(Ind, 0, Node, T, undefined, Fun, Acc1);			

fold_tree(Ind, Idx, Parent, [Node|Rest], _, Fun, Acc0) ->					%% pre-order list traversal
	Acc1 = fold_tree(Ind, Idx, Parent, Node, undefined, Fun, Acc0),					
	fold_tree(Ind, Idx+1, Parent, Rest, undefined, Fun, Acc1);						

fold_tree(Ind, Idx, Parent, {'fun', Node, Parameters}, _, Fun, Acc0) -> 	%% function evaluation
	Acc1 = fold_tree(Ind, Idx , Parent, Node, Parameters, Fun, Acc0),		
	fold_fun(Ind+1, 0, 'fun', Parameters, undefined, Fun, Acc1);
	
fold_tree(Ind, _, Parent, {Node, Left, Middle, Right}, Ch, Fun, Acc0) ->	%% {_,_,_,_} in-order ternary recurse
	Acc1 = fold_in(Ind+1, 0, Node, Left, undefined, Fun, Acc0),
	Acc2 = Fun(Ind+1, 0, Parent, {Left, Middle, Right}, visit, Node, Acc1),		
	Acc3 = fold_in(Ind+1, 0, Node, Middle, undefined, Fun, Acc2),
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind+1, 0, Node, Parent, ct(Ch)]),
	Acc4 = Fun(Ind+1, 0, Parent, <<>>, 'between and', Node, Acc3),		
	fold_in(Ind+1, 0, Node, Right, undefined, Fun, Acc4);
	
fold_tree(Ind, Idx, Parent, {'as', Node, Alias}, Ch, Fun, Acc0) -> 			%% {_,_,_} alias for fields
	Acc1 = fold_tree(Ind, Idx, Parent, Node, Ch, Fun, Acc0),
	Acc2 = Fun(Ind, 0, Parent, Alias, visit, 'as', Acc1),
	Fun(Ind, 0, 'as', Alias, visit, Alias, Acc2);

fold_tree(Ind, _, Node, {Node, Left, Right}, _, Fun, Acc0) ->				%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind, 0, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_tree(Ind, 0, Node, Node, {Left, Right}, Fun, Acc1),
	fold_in(Ind, 0, Node, Right, undefined, Fun, Acc2);

fold_tree(Ind, _, Parent, {Node, Left, Right}, _, Fun, Acc0)->				%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind+1, 0, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_tree(Ind+1, 0, Parent, Node, {Left, Right}, Fun, Acc1),
	fold_in(Ind+1, 0, Node, Right, undefined, Fun, Acc2);
	

fold_tree(_Ind, _Idx, _Parent, T, _, _Fun, Acc)-> 							%% catch remaining
	io:format(user, "~n----remaining term---------~n~p~n", [T]),
	Acc. 


fold_fun(Ind, 0, Parent, Node, Children, Fun, Acc0)  ->
	io:format(user, "~n~p,~p (", [Ind, 0]),
	Acc1 = Fun(Ind, 0, Parent, undefined, close, Node, 
			fold_tree(Ind+1, 0, Parent, Node, Children, Fun, 
			Fun(Ind, 0, Parent, undefined, open, Node, Acc0)
			)
		),
	io:format(user, "~n~p,~p )", [Ind, 0]),
	Acc1.

fold_in(Ind, 0, Parent, Node, Children, Fun, Acc0)  ->
	case binding(Node) < binding(Parent) of
		true ->
			io:format(user, "~n~p,~p (", [Ind, 0]),
			Acc1 = Fun(Ind+1, 0, Parent, undefined, close, Node, 
					fold_tree(Ind+1, 0, Parent, Node, Children, Fun, 
					Fun(Ind+1, 0, Parent, undefined, open, Node, Acc0)
					)
				),
			io:format(user, "~n~p,~p )", [Ind, 0]),
			Acc1;
		false ->										
			fold_tree(Ind, 0, Parent, Node, Children, Fun, Acc0)
	end.

sqls(_Ind, _Idx, _Parent, _Children, open, _, Acc) -> Acc ++ " (" ;
sqls(_Ind, _Idx, _Parent, _Children, close, _, Acc) -> Acc ++ " )" ;
sqls(_Ind, _Idx, _Parent, _Children, 'between and', _, Acc) -> Acc ++ " and" ;
sqls(_Ind, _Idx, _Parent, _Children, visit, 'fun', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, visit, 'opt', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, visit, 'list', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, visit, 'hints', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, visit, 'fields', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, [], visit, A, Acc) when is_atom(A) -> Acc;
sqls(_Ind, _Idx, _Parent, {}, visit, A, Acc) when is_atom(A) -> Acc;
sqls(_Ind, _Idx, _Parent, <<>>, visit, A, Acc) when is_atom(A) -> Acc;
sqls(_Ind, 0, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqls(_Ind, _Idx, 'fun', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqls(_Ind, _Idx, 'fields', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqls(_Ind, _Idx, 'from', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqls(_Ind, _Idx, 'list', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqls(_Ind, _Idx, 'group by', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqls(_Ind, _Idx, 'order by', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqls(_Ind, _Idx, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqls(_Ind, 0, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqls(_Ind, _Idx, 'fun', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqls(_Ind, _Idx, 'fields', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqls(_Ind, _Idx, 'from', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqls(_Ind, _Idx, 'list', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqls(_Ind, _Idx, 'group by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqls(_Ind, _Idx, 'order by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqls(_Ind, _Idx, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqls(_Ind, _Idx, _Parent, _Children, visit, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.

indent(N) -> [$\r|[$\n|lists:duplicate(N, $\t)]].

sqlp(_Ind, _Idx, _Parent, _Children, visit, <<>>, Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, visit, 'fun', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, visit, 'opt', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, visit, 'list', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, visit, 'hints', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, visit, 'fields', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, [], visit, A, Acc) when is_atom(A) -> Acc;
sqlp(_Ind, _Idx, _Parent, {}, visit, A, Acc) when is_atom(A) -> Acc;
sqlp(_Ind, _Idx, _Parent, <<>>, visit, A, Acc) when is_atom(A) -> Acc;
sqlp(_Ind, _Idx, opt, <<>>, visit, B, Acc) when is_binary(B) -> Acc;

sqlp(Ind, _Idx, _Parent, _Children, open, _, Acc) -> Acc ++ indent(Ind) ++ "(" ;
sqlp(Ind, _Idx, _Parent, _Children, close, _, Acc) -> Acc ++ indent(Ind) ++ ")" ;
sqlp(Ind, _Idx, _Parent, _Children, 'between and', _, Acc) -> Acc ++ indent(Ind) ++ "and" ;
sqlp(_Ind, _Idx, _Parent, _Children, visit, 'as', Acc) -> Acc ++ " as";
sqlp(Ind, 0, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ atom_to_list(A);
sqlp(Ind, _Idx, 'fun', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ "," ++ atom_to_list(A);
sqlp(Ind, _Idx, 'fields', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ "," ++ atom_to_list(A);
sqlp(Ind, _Idx, 'from', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ "," ++ atom_to_list(A);
sqlp(Ind, _Idx, 'list', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ "," ++ atom_to_list(A);
sqlp(Ind, _Idx, 'group by', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ "," ++ atom_to_list(A);
sqlp(Ind, _Idx, 'order by', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ "," ++ atom_to_list(A);
sqlp(Ind, _Idx, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ atom_to_list(A);
sqlp(_Ind, _Idx, 'as', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlp(Ind, 0, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ binary_to_list(B);
sqlp(Ind, _Idx, 'fun', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ "," ++ binary_to_list(B);
sqlp(Ind, _Idx, 'fields', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ "," ++ binary_to_list(B);
sqlp(Ind, _Idx, 'from', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ "," ++ binary_to_list(B);
sqlp(Ind, _Idx, 'list', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ "," ++ binary_to_list(B);
sqlp(Ind, _Idx, 'group by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ "," ++ binary_to_list(B);
sqlp(Ind, _Idx, 'order by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ "," ++ binary_to_list(B);
sqlp(Ind, _Idx, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ " " ++ binary_to_list(B);

sqlp(_Ind, _Idx, _Parent, _Children, visit, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.


sql_box_test() ->
	test_sqls(),	
	test_sqlp(),
	ok.
	
test_sqls() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L  S T R I N G  T E S T  |~n", []),
    io:format(user, "=================================~n", []),
    sqls_loop(?TEST_SQLS, 0).

sqls_loop([], _) -> ok;
sqls_loop([Sql|Rest], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} -> 
        	io:format(user, "-------------------------------~nParseTree:~n", []),
        	io:format(user, "~p~n", [ParseTree]),
        	io:format(user, "-------------------------------~n", []),
			Sqlstr = fold_tree(ParseTree, fun sqls/7, []),
       		io:format(user, "~n-------------------------------~nSqlstr:~n", []),
      		io:format(user, "~p~n-------------------------------~n", [Sqlstr]),        	
      		io:format(user, "~p~n-------------------------------~n", [sql_parse:collapse(Sqlstr)]),
    		?assertEqual(sql_parse:collapse(Sql),sql_parse:collapse(Sqlstr));  		
        Error -> 
        	io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
        	?assertEqual(ok, Error)
    end,
    sqls_loop(Rest, N+1).

test_sqlp() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L  P R E T T Y  T E S T  |~n", []),
    io:format(user, "=================================~n", []),
    sqlp_loop(?TEST_SQLS, 0).

sqlp_loop([], _) -> ok;
sqlp_loop([Sql|Rest], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} -> 
        	io:format(user, "-------------------------------~nParseTree:~n", []),
        	io:format(user, "~p~n", [ParseTree]),
        	io:format(user, "-------------------------------~n", []),
			Sqlstr = fold_tree(ParseTree, fun sqlp/7, []),
       		io:format(user, "~n-------------------------------~nSqlstr:~n", []),
      		io:format(user, Sqlstr ++ "~n", []),        	
    		?assertEqual(sql_parse:trim_nl(sql_parse:clean_cr(Sql)), sql_parse:trim_nl(sql_parse:clean_cr(Sqlstr)));  		
        Error -> 
        	io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
        	?assertEqual(ok, Error)
    end,
    sqlp_loop(Rest, N+1).

