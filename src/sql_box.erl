-module(sql_box).

-export([fold_tree/3, sqls/6, sqlp/6, sqlb/6]).

-export([test_sqls/0, test_sqlp/0]).		%% , test_sqlb/0


-include_lib("eunit/include/eunit.hrl").

-include("sql_tests.hrl").


%% -include("sql_box.hrl").

-record(box, {
        name,
        children = []
    }).


binding(A) when is_binary(A) -> 190;
binding('fun') -> 180;
binding('*') -> 170;
binding('/') -> 170;
binding('+') -> 160;
binding('-') -> 160;
binding('=') -> 150;
binding('<=') -> 150;
binding('>=') -> 150;
binding('<>') -> 150;
binding('<') -> 150;
binding('>') -> 150;
binding('like') -> 150;
binding('is') -> 150;
binding('between') -> 150;
binding('lists') -> 145;
binding('in') -> 140;
binding('not') -> 130;
binding('and') -> 120;
binding('or') -> 100;
binding('as') -> 90;
binding('fields') -> 80;
binding('into') -> 80;
binding('hints') -> 80;
binding('opt') -> 80;
binding('from') -> 80;
binding('where') -> 80;
binding('order by') -> 80;
binding('group by') -> 80;
binding('having') -> 80;
binding('select') -> 70;
binding('union') -> 30;
binding('all') -> 30;
binding('minus') -> 30;
binding('intersect') -> 30;
binding({A,_}) -> binding(A);
binding({A,_,_}) -> binding(A);
binding({A,_,_,_}) -> binding(A);
binding(undefined) -> -1;
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

fold_tree(ParseTree, Fun, Acc) -> fold_node(0, 0, undefined, ParseTree, undefined, Fun, Acc).

fold_node(_Ind, _Idx, _Parent, [], _Children, _Fun, Acc) -> Acc;			%% pre-order list traversal complete 

fold_node(Ind, Idx, Parent, B, Ch, Fun, Acc0) when is_binary(B) ->			%% binary terminal
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, Idx, B, Parent, ct(Ch)]),
	Fun(Ind, Idx, Parent, <<>>, B, Acc0);						

fold_node(Ind, Idx, Parent, A, Ch, Fun, Acc0) when is_atom(A) ->			%% atomic terminal
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, Idx, A, Parent, ct(Ch)]),
	Fun(Ind, Idx, Parent, Ch, A, Acc0);						

fold_node(Ind, Idx, Parent=undefined, {Node='select', List}, _, Fun, Acc0) when is_list(List) -> 	%% pre-order traversal recurse
	Acc1 = fold_node(Ind, Idx, Parent, Node, List, Fun, Acc0),	
	Acc2 = fold_node(Ind+1, 0 , Node, List, undefined, Fun, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);
	
fold_node(Ind, Idx, Parent, {Node='select', List}, _, Fun, Acc0) when is_list(List) -> 	%% pre-order traversal recurse
	Acc1 = fold_node(Ind+1, Idx, Parent, Node, List, Fun, Acc0),	
	Acc2 = fold_node(Ind+2, 0 , Node, List, undefined, Fun, Acc1),
	Acc3 = fold_return(Ind, Idx , 'select', Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
	
fold_node(Ind, Idx, Parent, {Node, List}, _, Fun, Acc0) when is_list(List) -> 	%% pre-order traversal recurse
	Acc1 = fold_node(Ind, Idx, Parent, Node, List, Fun, Acc0),	
	Acc2 = fold_node(Ind+1, 0 , Node, List, undefined, Fun, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);
	
fold_node(Ind, Idx, Parent, {Node, B}, _, Fun, Acc0) when is_binary(B) ->	%% pre-order recurse for 'hints', 'opt' and 'order by'
	Acc1 = fold_node(Ind, Idx , Parent, Node, B, Fun, Acc0),		 		
	Acc2 = Fun(Ind, 0, Parent, <<>>, B, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);					
	
fold_node(Ind, Idx, Parent, {Node, {}}, Ch, Fun, Acc0) ->					%% pre-order -> empty in-order for 'having'
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, Idx, Node, Parent, ct(Ch)]),
	Acc1 = Fun(Ind+1, 0, Parent, {}, Node, Acc0),
	fold_return(Ind, Idx , Parent, Fun, Acc1);			
	
fold_node(Ind, Idx, Parent, {Node='not', Child}, _, Fun, Acc0) ->					%% in-order unary recurse
	Acc1 = fold_node(Ind+1, 0 , Parent, Node, Child, Fun, Acc0),		 		
	Acc2 = fold_in(Ind+1, 0, Node, Child, undefined, Fun, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);			

fold_node(Ind, Idx, Parent, {Node, T}, _, Fun, Acc0) when is_tuple(T) ->	%% pre-order to in-order transition
	Acc1 = fold_in(Ind, Idx , Parent, Node, T, Fun, Acc0),		 		
	fold_in(Ind, 0, Node, T, undefined, Fun, Acc1);			

fold_node(Ind, Idx=0, Parent, [Node|Rest], _, Fun, Acc0) ->					%% pre-order list traversal
	Acc1 = fold_in(Ind, Idx, Parent, Node, undefined, Fun, Acc0),					
	fold_node(Ind, Idx+1, Parent, Rest, undefined, Fun, Acc1);						

fold_node(Ind, Idx, Parent='fun', [Node|Rest], _, Fun, Acc) ->					%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='fields', [Node|Rest], _, Fun, Acc) ->					%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='from', [Node|Rest], _, Fun, Acc) ->					%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='list', [Node|Rest], _, Fun, Acc) ->					%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='group by', [Node|Rest], _, Fun, Acc) ->					%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='order by', [Node|Rest], _, Fun, Acc) ->					%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent, [Node|Rest], _, Fun, Acc0) ->					%% pre-order list traversal
	Acc1 = fold_in(Ind, Idx, Parent, Node, undefined, Fun, Acc0),					
	fold_node(Ind, Idx+1, Parent, Rest, undefined, Fun, Acc1);						

fold_node(Ind, Idx, Parent, {'fun', Node, Parameters}, _, Fun, Acc0) -> 	%% function evaluation
	Acc1 = fold_node(Ind+1, Idx , Parent, Node, Parameters, Fun, Acc0),		
	Acc2 = fold_fun(Ind+2, 0, 'fun', Parameters, undefined, Fun, Acc1),
	Acc3 = fold_return(Ind, Idx , 'fun', Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
	
fold_node(Ind, Idx, Parent, {Node, Left, Middle, Right}, Ch, Fun, Acc0) ->	%% {_,_,_,_} in-order ternary recurse
	Acc1 = fold_in(Ind+1, 0, Node, Left, undefined, Fun, Acc0),
	Acc2 = Fun(Ind+1, 0, Parent, {Left, Middle, Right}, Node, Acc1),		
	Acc3 = fold_in(Ind+1, 0, Node, Middle, undefined, Fun, Acc2),
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind+1, 0, Node, Parent, ct(Ch)]),
	Acc4 = Fun(Ind+1, 0, Parent, undefined, 'and', Acc3),		
	Acc5 = fold_in(Ind+1, 0, Node, Right, undefined, Fun, Acc4),
	fold_return(Ind, Idx , Parent, Fun, Acc5);
	
fold_node(Ind, Idx, Parent, {'as', Node, Alias}, Ch, Fun, Acc0) -> 			%% {_,_,_} alias for fields
	Acc1 = fold_node(Ind, Idx, Parent, Node, Ch, Fun, Acc0),
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, 0, 'as', 'as', ct(Ch)]),
	Acc2 = Fun(Ind, 0, 'as', Alias, 'as', Acc1),
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, 0, 'as', Alias, ct(Ch)]),
	Fun(Ind, 0, 'as', Alias, Alias, Acc2);

fold_node(Ind, Idx, Node, {Node, Left, Right}, _, Fun, Acc0) ->				%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind, 0, Node, Node, {Left, Right}, Fun, Acc1),
	fold_in(Ind, 0, Node, Right, undefined, Fun, Acc2);

fold_node(Ind, Idx, Parent, {Node, Left, Right}, _, Fun, Acc0)->				%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind+1, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind+1, 0, Parent, Node, {Left, Right}, Fun, Acc1),
	Acc3 = fold_in(Ind+1, 0, Node, Right, undefined, Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
	
fold_node(_Ind, _Idx, _Parent, T, _, _Fun, Acc)-> 							%% catch remaining
	io:format(user, "~n----remaining term---------~n~p~n", [T]),
	Acc. 

fold_comma(Ind, Idx, Parent, [Node|Rest], _, Fun, Acc0) ->					%% pre-order list traversal
	io:format(user, "~n~p,~p,(~p),~p,~p", [Ind, Idx, ',', Parent, undefined]),
	Acc1 = Fun(Ind, Idx, Parent, undefined, ',', Acc0),
	Acc2 = fold_in(Ind, Idx, Parent, Node, undefined, Fun, Acc1),					
	fold_node(Ind, Idx+1, Parent, Rest, undefined, Fun, Acc2).						

fold_fun(Ind, 0, Parent, Node, Children, Fun, Acc0)  ->
	io:format(user, "~n~p,~p (", [Ind, 0]),
	Acc1 = Fun(Ind, 0, Parent, undefined, ')', 
			fold_node(Ind+1, 0, Parent, Node, Children, Fun, 
			Fun(Ind, 0, Parent, undefined, '(', Acc0)
			)
		),
	io:format(user, "~n~p,~p )", [Ind, 0]),
	fold_return(Ind, 0 , Parent, Fun, Acc1).

fold_in(Ind, Idx, Parent='as', Node, Children, Fun, Acc0)  ->
	fold_node(Ind, Idx, Parent, Node, Children, Fun, Acc0);
fold_in(Ind, Idx, Parent, Node, Children, Fun, Acc0)  ->
	case binding(Node) < binding(Parent) of
		true ->
			io:format(user, "~n~p,~p (", [Ind, Idx]),
			Acc1 = Fun(Ind+1, 0, Parent, undefined, ')', 
					fold_node(Ind+1, 0, Parent, Node, Children, Fun, 
					Fun(Ind+1, Idx, Parent, undefined, '(', Acc0)
					)
				),
			io:format(user, "~n~p,~p )", [Ind, Idx]),
			fold_return(Ind, Idx , Parent, Fun, Acc1);
		false ->										
			fold_node(Ind, Idx, Parent, Node, Children, Fun, Acc0)
	end.

fold_return(Ind, Idx, Parent, Fun, Acc0)  ->
	io:format(user, "~n~p,~p %ret%", [Ind, Idx]),
	Fun(Ind, Idx, Parent, undefined, '%ret%',Acc0).

sqls(_Ind, _Idx, _Parent, _Children, 'opt', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, 'list', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, 'hints', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, 'fields', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, '%ret%', Acc) -> Acc;
sqls(_Ind, _Idx, _Parent, [], A, Acc) when is_atom(A) -> Acc;
sqls(_Ind, _Idx, _Parent, {}, A, Acc) when is_atom(A) -> Acc;
sqls(_Ind, _Idx, _Parent, <<>>, A, Acc) when is_atom(A) -> Acc;
sqls(_Ind, _Idx, _Parent, _Children, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqls(_Ind, _Idx, _Parent, _Children, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqls(_Ind, _Idx, _Parent, _Children, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.

indent(N) -> [$\r|[$\n|lists:duplicate(N, $\t)]].

sqlp(_Ind, _Idx, _Parent, _Children, <<>>, Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, 'opt', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, 'list', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, 'hints', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, 'fields', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, '%ret%', Acc) -> Acc;
sqlp(_Ind, _Idx, _Parent, [], A, Acc) when is_atom(A) -> Acc;
sqlp(_Ind, _Idx, _Parent, {}, A, Acc) when is_atom(A) -> Acc;
sqlp(_Ind, _Idx, _Parent, <<>>, A, Acc) when is_atom(A) -> Acc;
sqlp(_Ind, _Idx, opt, <<>>, B, Acc) when is_binary(B) -> Acc;
sqlp(_Ind, _Idx, _Parent, _Children, 'as', Acc) -> Acc ++ " as";
sqlp(_Ind, _Idx, 'as', _Children, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqlp(_Ind, _Idx, 'as', _Children, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlp(_Ind, _Idx, _Parent, _Children, <<"asc">>, Acc) -> Acc ++ " asc";
sqlp(_Ind, _Idx, _Parent, _Children, <<"desc">>, Acc) -> Acc ++ " desc";
sqlp(Ind, _Idx, _Parent, _Children, A, Acc) when is_atom(A) -> Acc ++ indent(Ind) ++ atom_to_list(A);
sqlp(Ind, _Idx, _Parent, _Children, B, Acc) when is_binary(B) -> Acc ++ indent(Ind) ++ binary_to_list(B);

sqlp(_Ind, _Idx, _Parent, _Children, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.

sqlb(_Ind, _Idx, _Parent, _Children, <<>>, Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Children, 'opt', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Children, 'list', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Children, 'hints', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Children, 'fields', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, [], A, Acc) when is_atom(A) -> Acc;
sqlb(_Ind, _Idx, _Parent, {}, A, Acc) when is_atom(A) -> Acc;
sqlb(_Ind, _Idx, _Parent, <<>>, A, Acc) when is_atom(A) -> Acc;
sqlb(_Ind, _Idx, opt, <<>>, B, Acc) when is_binary(B) -> Acc;

sqlb(Ind, Idx, Parent, Children, A, []) ->
	{Ind, #box{name=atom_to_list(A)}};

sqlb(Ind, Idx, Parent, Children, X='%ret%', Acc={Rind, Box}) when Ind < Rind ->
	io:format(user, "~n---sqlb(~p,~p,~p,~p,~p,~p)", [Ind, Idx, Parent, ct(Children), X, Acc]),
	Acc1 = sqlb_return(Ind, Idx, Parent, Children, X, Acc),
	io:format(user, " --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, Parent, Children, X, Acc) when Ind==length(Acc)-1 -> 
	io:format(user, "~n---sqlb(~p,~p,~p,~p,~p,~p)", [Ind, Idx, Parent, ct(Children), X, Acc]),
	Acc1 = sqlb_push(Ind, Idx, Parent, Children, X, Acc),
	io:format(user, " --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, Parent, Children, X, Acc) when Ind==length(Acc) -> 
	io:format(user, "~n---sqlb(~p,~p,~p,~p,~p,~p)", [Ind, Idx, Parent, ct(Children), X, Acc]),
	Acc1 = sqlb_indent(Ind, Idx, Parent, Children, X, Acc),
	io:format(user, " --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, Parent, Children, X, Acc) when Ind>length(Acc) -> 
	io:format(user, "~n---sqlb(~p,~p,~p,~p,~p,~p)", [Ind, Idx, Parent, ct(Children), X, Acc]),
	Acc1 = sqlb_indent(Ind, Idx, Parent, Children, undefined, Acc),
	io:format(user, " --> ~p", [Acc1]),
	sqlb(Ind, Idx, Parent, Children, X, Acc1);
sqlb(Ind, Idx, Parent, Children, X, Acc) -> 
	io:format(user, "~n---Fun ignores ~p~n", [X]),
	Acc.	

sqlb_push(_, _, _, _, 'as', [[#box{name=Name}|Tail]|Rest]) ->
	[[#box{name=Name ++ " as"}|Tail]|Rest];
sqlb_push(_, _, 'as', _, A, [[#box{name=Name}|Tail]|Rest]) when is_atom(A) -> 
	[[#box{name=Name ++ atom_to_list(A)}|Tail]|Rest];
sqlb_push(_, _, 'as', _, B, [[#box{name=Name}|Tail]|Rest]) when is_binary(B) -> 
	[[#box{name=Name ++ binary_to_list(B)}|Tail]|Rest];
sqlb_push(_, _, _, _, A, [List|Rest]) when is_atom(A) ->
	[[#box{name=atom_to_list(A)}|List]|Rest];
sqlb_push(_, _, _, _, B, [List|Rest]) when is_binary(B) -> 
	[[#box{name=binary_to_list(B)}|List]|Rest];
sqlb_push(_Ind, _Idx, _Parent, _Children, X, Acc) -> 
	io:format(user, "~n---Fun sqlb_merge ignores ~p~n", [X]),
	Acc.

sqlb_indent(_, _, _, _, A, []) when is_atom(A) ->
	[#box{name=atom_to_list(A)}];
sqlb_indent(_, _, _, _, B, [Acc]) when is_binary(B) -> 
	[[#box{name=binary_to_list(B)}]|Acc];
sqlb_indent(Ind, Idx, Parent, Children, undefined, [Acc]) ->
	sqlb(Ind, Idx, Parent, Children, undefined, [[#box{}]|Acc]);
sqlb_indent(_, _, _, _, X, Acc) -> 
	io:format(user, "~n---Fun sqlb_indent ignores ~p~n", [X]),
	Acc.
	

sqlb_return(Ind, Idx, Parent, Children, X, Acc=[List|[Parent=#box{children=Children}|Rest]]) 
	when (length(Acc) > Ind) ->
		sqlb_return(Ind, Idx, Parent, Children, X, [Parent#box{children=[lists:reverse(List)|Children]}|Rest]);
sqlb_return(_, _, _, _, _, Acc) -> Acc.



sql_box_test() ->
	%test_sqls(),	
	test_sqlp(),
	%test_sqlb(),
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
			Sqlstr = fold_tree(ParseTree, fun sqls/6, []),
       		io:format(user, "~n-------------------------------~nSqlstr:~n", []),
      		io:format(user, "~p~n-------------------------------~n", [Sqlstr]),
      		SqlCollapsed = sql_parse:collapse(Sqlstr),
      		io:format(user, "~p~n-------------------------------~n", [SqlCollapsed]),
    		?assertEqual(sql_parse:collapse(Sql), SqlCollapsed),  		
    		{ok, NewTokens, _} = sql_lex:string(SqlCollapsed ++ ";"),
    		case sql_parse:parse(NewTokens) of
        		{ok, [NewParseTree|_]} ->
        			?assertEqual(ParseTree, NewParseTree);
				NewError -> 
					io:format(user, "Failed ~p~nNewTokens~p~n", [NewError, NewTokens]),
					?assertEqual(ok, NewError)
        	end;
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
			Sqlstr = fold_tree(ParseTree, fun sqlp/6, []),
       		io:format(user, "~n-------------------------------~nSqlstr:~n", []),
      		io:format(user, Sqlstr ++ "~n", []),
      		SqlCleaned = sql_parse:trim_nl(sql_parse:clean_cr(Sqlstr)),
    		?assertEqual(sql_parse:trim_nl(sql_parse:clean_cr(Sql)), SqlCleaned),  		
    		{ok, NewTokens, _} = sql_lex:string(SqlCleaned ++ ";"),
    		case sql_parse:parse(NewTokens) of
        		{ok, [NewParseTree|_]} ->
        			?assertEqual(ParseTree, NewParseTree);
				NewError -> 
					io:format(user, "Failed ~p~nNewTokens~p~n", [NewError, NewTokens]),
					?assertEqual(ok, NewError)
        	end;
        Error -> 
        	io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
        	?assertEqual(ok, Error)
    end,
    sqlp_loop(Rest, N+1).

test_sqlb() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|     S Q L  B O X  T E S T     |~n", []),
    io:format(user, "=================================~n", []),
    sqlb_loop(?TEST_SQLS, 0).

sqlb_loop([], _) -> ok;
sqlb_loop([Sql|Rest], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} -> 
        	io:format(user, "-------------------------------~nParseTree:~n", []),
        	io:format(user, "~p~n", [ParseTree]),
        	io:format(user, "-------------------------------~n", []),
			Sqlbox = fold_tree(ParseTree, fun sqlb/6, []),
       		io:format(user, "~n-------------------------------~nSqlbox:~n", []),
      		io:format(user, Sqlbox ++ "~n", []),
      		?assertMatch([#box{}], Sqlbox);
        Error -> 
        	io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
        	?assertEqual(ok, Error)
    end,
    sqlb_loop(Rest, N+1).

