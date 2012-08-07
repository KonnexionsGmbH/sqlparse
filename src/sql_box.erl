-module(sql_box).

-export([fold_tree/3, sqlstring/7]).


-include_lib("eunit/include/eunit.hrl").
-include("sql_box.hrl").


-define (TEST_PT, 
{select,[{hints,<<>>},
         {opt,[]},
         {fields,[<<"a">>,<<"b">>,<<"c">>]},
         {into,[]},
         {from,[<<"abc">>,<<"def">>]},
         {where,{'or',{'and',{'and',{in,<<"a">>,{'list',[<<"a">>,<<"b">>,<<"c">>]}},
                                    {'=',<<"c">>,<<"d">>}},
                             {'=',<<"e">>,<<"f">>}},
                      {between,<<"g">>,<<"h">>,<<"i">>}}},
         {'group by',[]},
         {having,{}},
         {'order by',[]}]
}
).

-define (TEST_PT1, 
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


fold_tree(ParseTree, Fun, Acc) -> fold_tree(0, 0, undefined, ParseTree, Fun, Acc).

fold_tree(_Ind, _Idx, _Parent, {_,[]}, _Fun, Acc) -> Acc;									%% pre-order element with empty content 
fold_tree(_Ind, _Idx, _Parent, [], _Fun, Acc) -> Acc;										%% pre-order content traversal complete 
fold_tree(_Ind, _Idx, _Parent, {_,<<>>}, _Fun, Acc) -> Acc;									%% pre-order empty hints 

fold_tree(Ind, Idx, Parent, [B|Rest], Fun, Acc0) when is_binary(B) ->						%% pre-order terminal content
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, B]),
	Acc1 = Fun(Ind, Idx, Parent, [], visit, B, Acc0),					
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc1);						

fold_tree(Ind, Idx, Parent, B, Fun, Acc) when is_binary(B) ->								%% in-order terminal content
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, B]),
	Fun(Ind, Idx, Parent, [], visit, B, Acc);							
	
fold_tree(Ind, Idx, Parent, [{Name, Children}|Rest], Fun, Acc0) when is_list(Children) ->	%% pre-order traversal recurse
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, Children, visit, Name, Acc0),				
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0 , Name, Children, Fun, Acc1),				
	fold_tree(Ind, Idx+1 , Parent, Rest, Fun, Acc2);					
	
fold_tree(Ind, Idx, Parent, [{Name, B}|Rest], Fun, Acc0) when is_binary(B) ->	%% pre-order hints
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, B, visit, Name, Acc0),					
	Acc2 = Fun(Ind+1, 1, Name, <<>>, visit, B, Acc1),				
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);				
	
fold_tree(Ind, Idx, Parent, {Name, List}, Fun, Acc0) when is_list(List) ->					%% pre-order traversal recurse
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, List, visit, Name, Acc0),			
	fold_tree(Ind, 0 , Name, List, Fun, Acc1);
	
fold_tree(Ind, Idx, Parent, [{Name, {}}|Rest], Fun, Acc0) ->								%% pre-order -> empty in-order
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {}, visit, Name, Acc0),			
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc1);			
	
fold_tree(Ind, Idx, Parent, [{Name, {Child, Unary}}|Rest], Fun, Acc0) ->	%% pre-order to unary in-order transition
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {Child, Unary}, visit, Name, Acc0),	
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0, Name, {Child, Unary}, Fun, Acc1),	
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);			
	
fold_tree(Ind, Idx, Parent, [{Name, {Child, Left, Right}}|Rest], Fun, Acc0) ->		%% pre-order to binary in-order transition
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {Child, Left, Right}, visit, Name, Acc0),			
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0, Name, {Child, Left, Right}, Fun, Acc1),			
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);					
	
fold_tree(Ind, Idx, Parent, [{Name, {Child, Left, Middle, Right}}|Rest], Fun, Acc0) ->	%% pre-order to ternary in-order transition
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, Idx, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, {Child, Left, Middle, Right}, visit, Name, Acc0),		
	NewInd = Ind+1,
	Acc2 = fold_tree(NewInd, 0, Name, {Child, Left, Middle, Right}, Fun, Acc1),		
	fold_tree(Ind, Idx+1, Parent, Rest, Fun, Acc2);					
	
fold_tree(Ind, _Idx, Parent, {Name, Unary}, Fun, Acc0) when is_tuple(Unary) ->	%% in-order unary recurse
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, 0, Parent, Name]),
	Acc1 = Fun(Ind, 0, Parent, Unary, visit, Name, Acc0),		
	fold_tree(Ind, 1, Name, Unary, Fun, Acc1);				

fold_tree(Ind, Idx, Parent, {Name, Left, Right}, Fun, Acc0) ->				%% in-order binary recurse
	case binding(Left) < binding(Name) of
		true	->
   io:format(user, "~n~p,~p,~p,~p (~n", [Ind, Idx, Parent, Name]),
			Acc1 = Fun(Ind, 0, Name, undefined, suffix, Left, 
				fold_tree(Ind, 0, Name, Left, Fun, 
					Fun(Ind, 0, Name, undefined, prefix, Left, Acc0)
					)
				);
		false	->
			Acc1 = fold_tree(Ind, 0, Name, Left, Fun, Acc0)		
	end,
   io:format(user, "~n~p,~p,~p,~p~n", [Ind, 0, Parent, Name]),
	Acc2 = Fun(Ind, 0, Parent, {Left, Right}, visit, Name, Acc1),		
	case binding(Right) < binding(Name) of
		true	->
   io:format(user, "~n~p,~p,~p,~p )~n", [Ind, Idx, Parent, Name]),
			Fun(Ind, 0, Name, undefined, suffix, Right,
				fold_tree(Ind, 0, Name, Right, Fun, 
				Fun(Ind, 0, Name, undefined, prefix, Right, Acc2)	
				)
			);
		false	->
			fold_tree(Ind, 0, Name, Right, Fun, Acc2)		
	end;
	
fold_tree(_Ind, _Idx, _Parent, T, _Fun, Acc)-> 									%% catch remaining
   io:format(user, "~n----remaining term---------~n~p~n", [T]),
	Acc. 

binding({'=',_,_}) -> 5;
binding({'<=',_,_}) -> 5;
binding({'>=',_,_}) -> 5;
binding({'<>',_,_}) -> 5;
binding({'<',_,_}) -> 5;
binding({'>',_,_}) -> 5;
binding({'between',_,_}) -> 5;
binding({'in',_,_}) -> 4;
binding({'not',_}) -> 3;
binding({'and',_,_}) -> 2;
binding({'or',_,_}) -> 1;
binding(_) -> 0.


sqlstring(_Ind, _Idx, _Parent, _Children, prefix, _, Acc) -> Acc ++ " (" ;
sqlstring(_Ind, _Idx, _Parent, _Children, suffix, _, Acc) -> Acc ++ " )" ;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, hints, Acc) -> Acc;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, fields, Acc) -> Acc;
sqlstring(_Ind, _Idx, _Parent, [], visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Ind, _Idx, _Parent, {}, visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Ind, _Idx, _Parent, <<>>, visit, A, Acc) when is_atom(A) -> Acc;
sqlstring(_Ind, _Idx, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqlstring(_Ind, _Idx, _Parent, _Children, visit, A, Acc) when is_atom(A) -> Acc ++ " " ++ atom_to_list(A);
sqlstring(_Ind, 0, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'fields', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'from', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'in', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'group by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, 'order by', _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " ," ++ binary_to_list(B);
sqlstring(_Ind, _Idx, _Parent, _Children, visit, B, Acc) when is_binary(B) -> Acc ++ " " ++ binary_to_list(B);
sqlstring(_Ind, _Idx, _Parent, _Children, visit, X, Acc) -> 
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

