-module(sql_box).

-export([fold_tree/3, sqlstring/7]).


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


fold_tree(ParseTree, Fun, Acc) -> fold_tree(0, 0, undefined, ParseTree, Fun, Acc).

fold_tree(_Ind, _Idx, _Parent, [], _Fun, Acc) -> Acc;										%% pre-order content traversal complete 

fold_tree(Ind, Idx, Parent, B, Fun, Acc0) when is_binary(B) ->								%% binary terminal
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, B]),
	Fun(Ind, Idx, Parent, [], visit, B, Acc0);						

fold_tree(Ind, Idx, Parent, A, Fun, Acc0) when is_atom(A) ->								%% atomic terminal
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, A]),
	Fun(Ind, Idx, Parent, [], visit, A, Acc0);						

fold_tree(Ind, Idx, Parent, [B|Rest], Fun, Acc0) when is_binary(B) ->						%% pre-order terminal content
	Acc1 = fold_tree(Ind, Idx, Parent, B, Fun, Acc0),					
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc1);						

fold_tree(Ind, Idx, Parent, [{Name, Sort}|Rest], Fun, Acc0) when is_binary(Sort) ->			%% pre-order traversal recurse
	Acc2 = fold_tree(Ind, Idx , Parent, Name, Fun, Acc0),				
	Acc3 = Fun(Ind, 0, Parent, [], visit, Sort, Acc2),				
	fold_tree(Ind, Idx+1 , Parent, Rest, Fun, Acc3);					
	
fold_tree(Ind, Idx, Parent, [{Name, Children}|Rest], Fun, Acc0) when is_list(Children) ->	%% pre-order traversal recurse
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, Children, visit, Name, Acc0),				
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0 , Name, Children, Fun, Acc1),				
	fold_tree(Ind, Idx+1 , Parent, Rest, Fun, Acc2);					
	
fold_tree(Ind, Idx, Parent, [{Name, B}|Rest], Fun, Acc0) when is_binary(B) ->				%% pre-order hints or opt
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, B, visit, Name, Acc0),					
	Acc2 = Fun(Ind+1, 0, Name, <<>>, visit, B, Acc1),				
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);				
	
fold_tree(Ind, Idx, Parent, {Name, List}, Fun, Acc0) when is_list(List) ->					%% pre-order traversal recurse
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, List, visit, Name, Acc0),			
	fold_tree(Ind, 0 , Name, List, Fun, Acc1);
	
fold_tree(Ind, Idx, Parent, [{Name, {Child, Left, Middle, Right}}|Rest], Fun, Acc0) ->		%% pre-order to ternary in-order transition
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {Child, Left, Middle, Right}, visit, Name, Acc0),		
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0, Name, {Child, Left, Middle, Right}, Fun, Acc1),		
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);					

fold_tree(Ind, Idx, Parent, [{Name, {Child, Left, Right}}|Rest], Fun, Acc0) ->				%% pre-order to binary in-order transition
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {Child, Left, Right}, visit, Name, Acc0),			
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0, Name, {Child, Left, Right}, Fun, Acc1),			
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);					
	
fold_tree(Ind, Idx, Parent, [{'as', Name, Alias}|Rest], Fun, Acc0) -> 						%% alias for fields
	Acc1 = fold_tree(Ind, Idx, Parent, Name, Fun, Acc0),
	Acc2 = Fun(Ind, 0, Parent, Alias, visit, 'as', Acc1),		
	Acc3 = Fun(Ind, 0, Parent, Alias, visit, Alias, Acc2),		
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc3);			
	
fold_tree(Ind, Idx, Parent, [{'fun', Name, Parameters}|Rest], Fun, Acc0) ->					%% pre-order to function in-order transition
	NewInd = Ind+1,
	Acc1 = fold_tree(NewInd, Idx, Parent, {'fun', Name, Parameters}, Fun, Acc0),	
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc1);			

fold_tree(Ind, Idx, Parent, [{Name, Left, Right}|Rest], Fun, Acc0) ->						%% list head binary in-order
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, Idx, Name, {Name, Left, Right}, Fun, Acc0),			
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);					
	
fold_tree(Ind, Idx, Parent, [{Name, {}}|Rest], Fun, Acc0) ->								%% pre-order -> empty in-order
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {}, visit, Name, Acc0),			
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc1);			
	
fold_tree(Ind, Idx, Parent, [{Name, {Child, Unary}}|Rest], Fun, Acc0) ->					%% pre-order to unary in-order transition
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {Child, Unary}, visit, Name, Acc0),	
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0, Name, {Child, Unary}, Fun, Acc1),	
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);			

fold_tree(Ind, Idx, Parent, {'fun', Name, Parameters}, Fun, Acc0) -> 						%% function evaluation
	io:format(user, "~n~p,~p,~p,~p", [Ind, 0, Parent, Name]),
	Acc1 = Fun(Ind, Idx, Parent, Parameters, visit, Name, Acc0),		
	fold_in_brackets(Ind, 0, 'fun', Parameters, Fun, Acc1);
	
fold_tree(Ind, Idx, Parent, B, Fun, Acc) when is_binary(B) ->								%% in-order terminal
	io:format(user, "~n~p,~p,~p,~p", [Ind, Idx, Parent, B]),
	Fun(Ind, Idx, Parent, [], visit, B, Acc);							
	
fold_tree(Ind, 0, Parent, {Name, Left, Middle, Right}, Fun, Acc0) ->						%% in-order ternary recurse
	Acc1 = fold_in_brackets(Ind, 0, Name, Left, Fun, Acc0),
	io:format(user, "~n~p,~p,~p,~p", [Ind, 0, Parent, Name]),
	Acc2 = Fun(Ind, 0, Parent, {Left, Middle, Right}, visit, Name, Acc1),		
	Acc3 = fold_in_brackets(Ind, 0, Name, Middle, Fun, Acc2),
	io:format(user, "~n~p,~p,~p,~p, and", [Ind, 0, Parent, Name]),
	Acc4 = Fun(Ind, 0, Parent, {}, 'between and', Name, Acc3),		
	fold_in_brackets(Ind, 0, Name, Right, Fun, Acc4);
	
fold_tree(Ind, 0, Parent, {Name, Left, Right}, Fun, Acc0) ->								%% in-order binary recurse
	Acc1 = fold_in_brackets(Ind, 0, Name, Left, Fun, Acc0),
	io:format(user, "~n~p,~p,~p,~p", [Ind, 0, Parent, Name]),
	Acc2 = Fun(Ind, 0, Parent, {Left, Right}, visit, Name, Acc1),		
	fold_in_brackets(Ind, 0, Name, Right, Fun, Acc2);
	
fold_tree(Ind, 0, Parent, {Name, Unary}, Fun, Acc0) when is_tuple(Unary) ->					%% in-order unary recurse
	io:format(user, "~n~p,~p,~p,~p", [Ind, 0, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, Unary, visit, Name, Acc0),		
	fold_in_brackets(Ind, 0, Name, Unary, Fun, Acc1);

fold_tree(_Ind, _Idx, _Parent, T, _Fun, Acc)-> 												%% catch remaining
	io:format(user, "~n----remaining term---------~n~p~n", [T]),
	Acc. 


fold_in_brackets(Ind, 0, Name, Child, Fun, Acc0)  ->
	case binding(Child) < binding(Name) of
		true ->
			io:format(user, "~n~p,~p (", [Ind, 0]),
			Acc1 = Fun(Ind, 0, Name, undefined, close, Child, 
					fold_tree(Ind, 0, Name, Child, Fun, 
					Fun(Ind, 0, Name, undefined, open, Child, Acc0)
					)
				),
			io:format(user, "~n~p,~p )", [Ind, 0]),
			Acc1;
		false ->										
			fold_tree(Ind, 0, Name, Child, Fun, Acc0)
	end.

sqlstring(_Ind, _Idx, _Parent, _Children, open, _, Acc) -> Acc ++ " (" ;
sqlstring(_Ind, _Idx, _Parent, _Children, close, _, Acc) -> Acc ++ " )" ;
sqlstring(_Ind, _Idx, _Parent, _Children, 'between and', _, Acc) -> Acc ++ " and" ;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, 'fun', Acc) -> Acc;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, 'opt', Acc) -> Acc;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, 'list', Acc) -> Acc;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, 'hints', Acc) -> Acc;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, 'fields', Acc) -> Acc;
sqlstring(_Ind, _Idx, _Parent, [], visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Ind, _Idx, _Parent, {}, visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Ind, _Idx, _Parent, <<>>, visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Ind, 0, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqlstring(_Ind, _Idx, 'fun', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqlstring(_Ind, _Idx, 'fields', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqlstring(_Ind, _Idx, 'from', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqlstring(_Ind, _Idx, 'list', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqlstring(_Ind, _Idx, 'group by', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqlstring(_Ind, _Idx, 'order by', _Children, visit, A, Acc) when is_atom(A) -> Acc ++ "," ++ atom_to_list(A);
sqlstring(_Ind, _Idx, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqlstring(_Ind, 0, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'fun', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'fields', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'from', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'list', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'group by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'order by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ "," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlstring(_Ind, _Idx, _Parent, _Children, visit, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.
	
sqlstring_test() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L  S T R I N G  T E S T  |~n", []),
    io:format(user, "=================================~n", []),
    test_loop(?TEST_SQLS, 0).

test_loop([], _) -> ok;
test_loop([Sql|Sqls], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} -> 
        	io:format(user, "-------------------------------~nParseTree:~n", []),
        	io:format(user, "~p~n", [ParseTree]),
        	io:format(user, "-------------------------------~n", []),
			Sqlstring = fold_tree(ParseTree, fun sqlstring/7, []),
       		io:format(user, "~n-------------------------------~nSqlstring:~n", []),
      		io:format(user, "~p~n-------------------------------~n", [Sqlstring]),        	
      		io:format(user, "~p~n-------------------------------~n", [sql_parse:collapse(Sqlstring)]),
    		?assertEqual(sql_parse:collapse(Sql),sql_parse:collapse(Sqlstring));  		
        Error -> io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens])
    end,
    test_loop(Sqls, N+1).
