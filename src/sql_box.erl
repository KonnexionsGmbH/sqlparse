-module(sql_box).

-export([ box_tree/1
		, box_shift/1
		]).

-include_lib("eunit/include/eunit.hrl").

-include("sql_tests.hrl").


%% -define(logf, ok).
-ifdef(logf).
-define(LOG(F, A), io:format(user, "{~p,~p}:"++F, [?MODULE,?LINE] ++ A)).

	%% Child Type Label ------------------------------------------
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

-else.
-define(LOG(F, A), ok).
-endif.

-include("sql_box.hrl").

box_tree(ParseTree) ->
	case (catch fold_tree(ParseTree, fun sqlb/6, [])) of
		{'EXIT', Error} -> {error, Error};
		[#box{ind=0}=Box] -> Box;
		{error, Error} -> {error, Error}
	end.

%% Operator Binding Power -------------------------------

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
binding('union all') -> 30;
binding('all') -> 30;			%% needed here?
binding('minus') -> 30;
binding('intersect') -> 30;
binding({A,_}) -> binding(A);
binding({A,_,_}) -> binding(A);
binding({A,_,_,_}) -> binding(A);
binding(undefined) -> -1;
binding(_) -> 0.


%% SQL Parse Tree Traversal ------------------------------------

fold_tree(ParseTree, Fun, Acc) -> fold_node(0, 0, undefined, ParseTree, undefined, Fun, Acc).

fold_node(_Ind, _Idx, _Parent, [], _Children, _Fun, Acc) -> Acc;			%% pre-order list traversal complete 

fold_node(Ind, Idx, Parent, B, _Ch, Fun, Acc0) when is_binary(B) ->			%% binary terminal
	?LOG("~n~p,~p,(~p),~p,~p", [Ind, Idx, B, Parent, ct(_Ch)]),
	Fun(Ind, Idx, Parent, <<>>, B, Acc0);						

fold_node(Ind, Idx, Parent, A, Ch, Fun, Acc0) when is_atom(A) ->			%% atomic terminal
	?LOG("~n~p,~p,(~p),~p,~p", [Ind, Idx, A, Parent, ct(Ch)]),
	Fun(Ind, Idx, Parent, Ch, A, Acc0);						

fold_node(Ind, Idx, Parent=undefined, {Node='select', List}, _, Fun, Acc0) when is_list(List) -> 	%% pre-order traversal recurse
	Acc1 = fold_node(Ind, Idx, Parent, Node, List, Fun, Acc0),	
	Acc2 = fold_node(Ind+1, 0 , Node, List, undefined, Fun, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);
	
fold_node(Ind, Idx, Parent, {Node='select', List}, _, Fun, Acc0) when is_list(List) -> 	%% pre-order traversal recurse
	Acc1 = fold_node(Ind+1, Idx, Parent, Node, List, Fun, Acc0),	%% Idx ??
	Acc2 = fold_node(Ind+2, 0 , Node, List, undefined, Fun, Acc1),
	Acc3 = fold_return(Ind+1, 0 , Node, Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
	
fold_node(Ind, Idx, Parent, {Node, List}, _, Fun, Acc0) when is_list(List) -> 	%% pre-order traversal recurse
	Acc1 = fold_node(Ind, Idx, Parent, Node, List, Fun, Acc0),		%% Idx ??
	Acc2 = fold_node(Ind+1, 0 , Node, List, undefined, Fun, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);
	
fold_node(Ind, Idx, Parent, {Node, B}, _, Fun, Acc0) when is_binary(B) ->	%% pre-order recurse for 'hints', 'opt' and 'order by'
	Acc1 = fold_node(Ind, Idx , Parent, Node, B, Fun, Acc0),		 		
	Acc2 = Fun(Ind, 0, Parent, <<>>, B, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);					
	
fold_node(Ind, Idx, Parent, {Node, {}}, _Ch, Fun, Acc0) ->					%% pre-order -> empty in-order for 'having'
	?LOG("~n~p,~p,(~p),~p,~p", [Ind, Idx, Node, Parent, ct(_Ch)]),
	Acc1 = Fun(Ind+1, 0, Parent, {}, Node, Acc0),
	fold_return(Ind, Idx , Parent, Fun, Acc1);			
	
fold_node(Ind, Idx, Parent, {Node='not', Child}, _, Fun, Acc0) ->			%% in-order unary recurse
	Acc1 = fold_node(Ind+1, 0 , Parent, Node, Child, Fun, Acc0),		 		
	Acc2 = fold_in(Ind+1, 0, Node, Child, undefined, Fun, Acc1),
	fold_return(Ind, Idx , Parent, Fun, Acc2);			

fold_node(Ind, Idx, Parent, {Node, T}, _, Fun, Acc0) when is_tuple(T) ->	%% pre-order to in-order transition
	Acc1 = fold_in(Ind, Idx , Parent, Node, T, Fun, Acc0),		 		
	fold_in(Ind, 0, Node, T, undefined, Fun, Acc1);			

fold_node(Ind, Idx=0, Parent, [Node|Rest], _, Fun, Acc0) ->					%% pre-order list traversal
	Acc1 = fold_in(Ind, Idx, Parent, Node, undefined, Fun, Acc0),					
	fold_node(Ind, Idx+1, Parent, Rest, undefined, Fun, Acc1);						

fold_node(Ind, Idx, Parent='fun', [Node|Rest], _, Fun, Acc) ->				%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='fields', [Node|Rest], _, Fun, Acc) ->			%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='from', [Node|Rest], _, Fun, Acc) ->				%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='list', [Node|Rest], _, Fun, Acc) ->				%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='group by', [Node|Rest], _, Fun, Acc) ->			%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent='order by', [Node|Rest], _, Fun, Acc) ->			%% pre-order list traversal
	fold_comma(Ind, Idx, Parent, [Node|Rest], undefined, Fun, Acc);						

fold_node(Ind, Idx, Parent, [Node|Rest], _, Fun, Acc0) ->					%% pre-order list traversal
	Acc1 = fold_in(Ind, Idx, Parent, Node, undefined, Fun, Acc0),					
	fold_node(Ind, Idx+1, Parent, Rest, undefined, Fun, Acc1);						

fold_node(Ind, Idx, Parent, {'fun', Node, Parameters}, _, Fun, Acc0) -> 	%% function evaluation
	Acc1 = fold_node(Ind+1, Idx , Parent, Node, Parameters, Fun, Acc0),		
	Acc2 = fold_fun(Ind+2, 0, 'fun', Parameters, undefined, Fun, Acc1),
	Acc3 = fold_return(Ind+1, Idx , 'fun', Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
	
fold_node(Ind, Idx, Parent, {Node, Left, Middle, Right}, _Ch, Fun, Acc0) ->	%% {_,_,_,_} in-order ternary recurse
	Acc1 = fold_in(Ind+1, 0, Node, Left, undefined, Fun, Acc0),
	Acc2 = Fun(Ind+1, 0, Parent, {Left, Middle, Right}, Node, Acc1),		
	Acc3 = fold_in(Ind+1, 0, Node, Middle, undefined, Fun, Acc2),
	?LOG("~n~p,~p,(~p),~p,~p", [Ind+1, 0, Node, Parent, ct(_Ch)]),
	Acc4 = Fun(Ind+1, 0, Parent, undefined, 'and', Acc3),		
	Acc5 = fold_in(Ind+1, 0, Node, Right, undefined, Fun, Acc4),
	fold_return(Ind, Idx , Parent, Fun, Acc5);
	
fold_node(Ind, Idx, Parent, {'as', Node, Alias}, Ch, Fun, Acc0) -> 			%% {_,_,_} alias for fields
	Acc1 = fold_node(Ind, Idx, Parent, Node, Ch, Fun, Acc0),
	?LOG("~n~p,~p,(~p),~p,~p", [Ind, 0, 'as', 'as', ct(Ch)]),
	Acc2 = Fun(Ind, 0, 'as', Alias, 'as', Acc1),
	?LOG("~n~p,~p,(~p),~p,~p", [Ind, 0, 'as', Alias, ct(Ch)]),
	Fun(Ind, 0, 'as', Alias, Alias, Acc2);

fold_node(Ind, Idx, Node, {Node, Left, Right}, _, Fun, Acc0) ->				%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind, 0, Node, Node, {Left, Right}, Fun, Acc1),
	fold_in(Ind, 0, Node, Right, undefined, Fun, Acc2);

fold_node(Ind, Idx, Parent, {intersect=Node, Left, Right}, _, Fun, Acc0)->		%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind, 0, Parent, Node, {Left, Right}, Fun, Acc1),
	Acc3 = fold_in(Ind, 0, Node, Right, undefined, Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
fold_node(Ind, Idx, Parent, {minus=Node, Left, Right}, _, Fun, Acc0)->		%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind, 0, Parent, Node, {Left, Right}, Fun, Acc1),
	Acc3 = fold_in(Ind, 0, Node, Right, undefined, Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
fold_node(Ind, Idx, Parent, {union=Node, Left, Right}, _, Fun, Acc0)->		%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind, 0, Parent, Node, {Left, Right}, Fun, Acc1),
	Acc3 = fold_in(Ind, 0, Node, Right, undefined, Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
fold_node(Ind, Idx, Parent, {'union all'=Node, Left, Right}, _, Fun, Acc0)->		%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind, 0, Parent, Node, {Left, Right}, Fun, Acc1),
	Acc3 = fold_in(Ind, 0, Node, Right, undefined, Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
fold_node(Ind, Idx, Parent, {Node, Left, Right}, _, Fun, Acc0)->			%% {_,_,_} in-order binary recurse
	Acc1 = fold_in(Ind+1, Idx, Node, Left, undefined, Fun, Acc0),
	Acc2 = fold_node(Ind+1, 0, Parent, Node, {Left, Right}, Fun, Acc1),
	Acc3 = fold_in(Ind+1, 0, Node, Right, undefined, Fun, Acc2),
	fold_return(Ind, Idx , Parent, Fun, Acc3);
	
fold_node(_Ind, _Idx, _Parent, _T, _, _Fun, Acc)-> 							%% catch remaining
	?LOG("~n----remaining term---------~n~p~n", [_T]),
	Acc. 

fold_comma(Ind, Idx, Parent, [Node|Rest], _, Fun, Acc0) ->					%% pre-order list traversal
	?LOG("~n~p,~p,(~p),~p,~p", [Ind, Idx, ',', Parent, undefined]),
	Acc1 = Fun(Ind, Idx, Parent, undefined, ',', Acc0),
	Acc2 = fold_in(Ind, Idx, Parent, Node, undefined, Fun, Acc1),					
	fold_node(Ind, Idx+1, Parent, Rest, undefined, Fun, Acc2).						

fold_fun(Ind, 0, Parent, Node, Children, Fun, Acc0)  ->						%% brackets and index for functions
	?LOG("~n~p,~p (", [Ind, 0]),
	Acc1 = Fun(Ind, 0, Parent, undefined, ')',
			fold_return(Ind, 0 , Parent, Fun, 
				fold_node(Ind+1, 0, Parent, Node, Children, Fun, 
					Fun(Ind, 0, Parent, undefined, '(', Acc0)
				)
		  	)
		),
	?LOG("~n~p,~p )", [Ind, 0]),
	Acc1.

fold_in(Ind, Idx, Parent='as', Node, Children, Fun, Acc0)  ->				%% setting brackets
	fold_node(Ind, Idx, Parent, Node, Children, Fun, Acc0);
fold_in(Ind, Idx, Parent, Node, Children, Fun, Acc0)  ->
	case binding(Node) < binding(Parent) of
		true ->
			?LOG("~n~p,~p (", [Ind, Idx]),
			Acc1 = Fun(Ind+1, 0, Parent, undefined, ')', 
					fold_node(Ind+1, 0, Parent, Node, Children, Fun, 
					Fun(Ind+1, Idx, Parent, undefined, '(', Acc0)
					)
				),
			?LOG("~n~p,~p )", [Ind, Idx]),
			fold_return(Ind, Idx , Parent, Fun, Acc1);
		false ->										
			fold_node(Ind, Idx, Parent, Node, Children, Fun, Acc0)
	end.

fold_return(Ind, Idx, Parent, Fun, Acc0)  ->								%% un-indenting
	?LOG("~n~p,~p %ret%", [Ind, Idx]),
	Fun(Ind, Idx, Parent, undefined, '%ret%',Acc0).

%% Sql String Printing Fun ---------------------------------------

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
sqls(_Ind, _Idx, _Parent, _Children, _X, Acc) -> 
	?LOG("~n---Fun ignores ~p~n", [_X]),
	Acc.

%% SQL Pretty Printing Fun ---------------------------------------

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

sqlp(_Ind, _Idx, _Parent, _Children, _X, Acc) -> 
	?LOG("~n---Fun ignores ~p~n", [_X]),
	Acc.

%% SQL Boxing Fun ---------------------------------------

sqlb_collapse(0) -> false;
sqlb_collapse(1) -> false;
sqlb_collapse(_Ind) -> true.

sqlb(_Ind, _Idx, _Parent, _Ch, <<>>, Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Ch, 'opt', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Ch, 'list', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Ch, 'hints', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, _Ch, 'fields', Acc) -> Acc;
sqlb(_Ind, _Idx, _Parent, [], A, Acc) when is_atom(A) -> Acc;
sqlb(_Ind, _Idx, _Parent, {}, A, Acc) when is_atom(A) -> Acc;
sqlb(_Ind, _Idx, _Parent, <<>>, A, Acc) when is_atom(A) -> Acc;
sqlb(_Ind, _Idx, opt, <<>>, B, Acc) when is_binary(B) -> Acc;

sqlb(Ind, Idx, _Parent, _Ch, A, []) ->
    [#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=atom_to_binary(A, utf8)}];

sqlb(Ind, _Idx, _Parent, _Ch, _X='%ret%', Acc=[#box{ind=I}|_]) when I<Ind -> 
%	sqlb_retlog(Ind, _Idx, _Parent, _Ch, keep, Acc),
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, _Idx, _Parent, _X]),
	?LOG(" --> ~p", [Acc]),
	Acc;
sqlb(Ind, _Idx, _Parent, _Ch, _X='%ret%', Acc=[#box{ind=I}|_]) when I==Ind ->
%	sqlb_retlog(Ind, _Idx, _Parent, _Ch, keep, Acc),
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, _Idx, _Parent, _X]),
	?LOG(" --> ~p", [Acc]),
	Acc;
sqlb(Ind, Idx, Parent, Ch, X='%ret%', Acc=[#box{ind=I}|_]) when I>Ind+1 ->
%	sqlb_retlog(I-1, Idx, Parent, Ch, reduce, Acc),
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, Parent, X]),
	Acc1 = sqlb_reduce(I-1, Idx, Parent, Ch, X, Acc),
%	sqlb_retlog(I-1, Idx, Parent, Ch, reduced, Acc1),
	?LOG(" --> ~p", [Acc1]),
	sqlb(Ind, Idx, Parent, Ch, X, Acc1);
sqlb(Ind, Idx, Parent, Ch, X='%ret%', Acc=[#box{ind=I}|_]) when I==Ind+1 ->
%	sqlb_retlog(Ind, Idx, Parent, Ch, reduce, Acc),
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, Parent, X]),
	Acc1 = sqlb_reduce(Ind, Idx, Parent, Ch, X, Acc),
%	sqlb_retlog(Ind, Idx, Parent, Ch, reduced, Acc1),
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent, _Ch, _X='as', [#box{ind=I,name=Name}=Box|Rest]) when I==Ind+1 -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, _X]),
    Acc1 = [Box#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=list_to_binary([Name, " as"])}|Rest],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent, _Ch, _X = <<"asc">>, [#box{ind=I,name=Name}=Box|Rest]) when I==Ind+1 -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, _X]),
	Acc1 = [Box#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=list_to_binary([Name, " asc"])}|Rest],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent, _Ch, _X = <<"desc">>, [#box{ind=I,name=Name}=Box|Rest]) when I==Ind+1 -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, _X]),
	Acc1 = [Box#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=list_to_binary([Name, " desc"])}|Rest],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent='as', _Ch, X, [#box{ind=I,name=Name}=Box|Rest]) when I==Ind+1 -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, X]),
	Acc1 = [Box#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=list_to_binary([Name, " ", X])}|Rest],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent, _Ch, X, Acc=[#box{ind=I}|_]) when I==Ind+1, is_atom(X) -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, X]),
	Acc1 = [#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=atom_to_binary(X, utf8)}|Acc],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent, _Ch, X, Acc=[#box{ind=I}|_]) when I==Ind+1, is_binary(X) -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, X]),
	Acc1 = [#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=X}|Acc],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent, _Ch, X, Acc=[#box{ind=I}|_]) when I<Ind -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, X]),
	Acc1 = [#box{ind=I+1, idx=Idx, collapsed=sqlb_collapse(Ind+1), name=X}|Acc],
	?LOG(" --> ~p", [Acc1]),
	Acc1; 								 %% sqlb(Ind, Idx, Parent, Ch, X, Acc1);
sqlb(Ind, Idx, _Parent, _Ch, X, Acc=[#box{ind=I}|_]) when I<Ind+1, is_atom(X) -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, X]),
	Acc1 = [#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=atom_to_binary(X, utf8)}|Acc],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(Ind, Idx, _Parent, _Ch, X, Acc=[#box{ind=I}|_]) when I<Ind+1, is_binary(X) -> 
	?LOG("~n---sqlb(~p,~p,~p,~p)", [Ind, Idx, _Parent, X]),
	Acc1 = [#box{ind=Ind, idx=Idx, collapsed=sqlb_collapse(Ind), name=X}|Acc],
	?LOG(" --> ~p", [Acc1]),
	Acc1;
sqlb(_Ind, _Idx, _Parent, _Ch, _X, Acc) -> 
	?LOG("~n---Fun ignores ~p~n", [_X]),
	Acc.	

sqlb_reduce(Ind, Idx, Parent, Ch, X, Acc) -> 
	sqlb_reduce(Ind, Idx, Parent, Ch, X, Acc,[]).

sqlb_reduce(Ind, Idx, Parent, Ch, _X, [#box{ind=I}=Second,#box{name= <<"intersect">>}=Merge|First], _What) when I==Ind+1 ->
	sqlb_set_merge(Ind, Idx, Parent, Ch, Second, Merge, First);
sqlb_reduce(Ind, Idx, Parent, Ch, _X, [#box{ind=I}=Second,#box{name= <<"minus">>}=Merge|First], _What) when I==Ind+1 ->
	sqlb_set_merge(Ind, Idx, Parent, Ch, Second, Merge, First);
sqlb_reduce(Ind, Idx, Parent, Ch, _X, [#box{ind=I}=Second,#box{name= <<"union all">>}=Merge|First], _What) when I==Ind+1 ->
	sqlb_set_merge(Ind, Idx, Parent, Ch, Second, Merge, First);
sqlb_reduce(Ind, Idx, Parent, Ch, _X, [#box{ind=I}=Second,#box{name= <<"union">>}=Merge|First], _What) when I==Ind+1 ->
	sqlb_set_merge(Ind, Idx, Parent, Ch, Second, Merge, First);

sqlb_reduce(Ind, _Idx, intersect, _Ch, _X, [], [#box{ind=I}|_]=Buf) when I==Ind+1 ->
%	sqlb_retlog(Ind, _Idx, union, _Ch, keep, Buf),
	Buf;
sqlb_reduce(Ind, _Idx, minus, _Ch, _X, [], [#box{ind=I}|_]=Buf) when I==Ind+1 ->
%	sqlb_retlog(Ind, _Idx, union, _Ch, keep, Buf),
	Buf;
sqlb_reduce(Ind, _Idx, 'union all', _Ch, _X, [], [#box{ind=I}|_]=Buf) when I==Ind+1 ->
%	sqlb_retlog(Ind, _Idx, union, _Ch, keep, Buf),
	Buf;
sqlb_reduce(Ind, _Idx, union, _Ch, _X, [], [#box{ind=I}|_]=Buf) when I==Ind+1 ->
%	sqlb_retlog(Ind, _Idx, union, _Ch, keep, Buf),
	Buf;

sqlb_reduce(Ind, Idx, Parent, Ch, X, [#box{ind=I}=Box|Rest]=_Acc, Buf) when I==Ind+1 ->
%	sqlb_retlog(Ind, Idx, Parent, Ch, buffer, _Acc),
	sqlb_reduce(Ind, Idx, Parent, Ch, X, Rest, [Box|Buf]);
sqlb_reduce(Ind, _Idx, _Parent, _Ch, _X, [#box{ind=I, children=Children}=Box|Rest]=_Acc, Buf) when I==Ind ->
%	sqlb_retlog(Ind, _Idx, _Parent, _Ch, reduce, _Acc),
	[Box#box{children=Children++Buf}|Rest].

sqlb_set_merge(_Ind, _Idx, _Parent, _Ch, Second, Merge, [First]) ->
%	sqlb_retlog(_Ind, _Idx, _Parent, _Ch, set_reduce_first, [Second,Merge,First]),
	Result = [box_shift(First),Merge#box{children=[Second]}],
%	sqlb_retlog(_Ind, _Idx, _Parent, _Ch, set_reduced, Result),
	Result;

sqlb_set_merge(_Ind, _Idx, _Parent, _Ch, Second, Merge, First) ->
%	sqlb_retlog(_Ind, _Idx, _Parent, _Ch, set_reduce_more, [Second,Merge|First]),
	Result = lists:flatten([First] ++ [Merge#box{children=[Second]}]),
%	sqlb_retlog(_Ind, _Idx, _Parent, _Ch, set_reduced, Result),
	Result.



%sqlb_retlog(Ind, Idx, Parent, Ch, Action, Acc) ->
%	io:format(user, "sqlb_ret ~p ~p ~p ~p ~p ~p ~n", [Ind, Idx, Parent, Ch, Action, [{box,Id,Ix,N,ltype(C)}|| #box{ind=Id,idx=Ix,name=N,children=C} <- Acc]]).

% ltype([])->
% 	[];
% ltype([A])->
% 	[A#box.name];	
% ltype([H|_])->
% 	[H#box.name,'..'].

box_shift(Boxes) when is_list(Boxes)->
	box_shift(Boxes,[]);
box_shift(Box) ->
	[Shifted]=box_shift([Box]),
	Shifted.

box_shift([],Acc) -> lists:reverse(Acc);
box_shift([#box{ind=Ind,children=Children}=Box|Rest], Acc) ->
	box_shift(Rest, [Box#box{ind=Ind-1,children=box_shift(Children)}|Acc]). 


setup() -> ?TEST_SQLS.

teardown(_) -> ok.

sql_box_test_() ->
	{timeout, 1000000, 
		{
	        setup,
	        fun setup/0,
	        fun teardown/1,
	        {with, 
	        	[
	        	fun test_sql_all/1
	            % , fun test_sqls/1
	            % , fun test_sqlp/1
	            % , fun test_sqlb/1
	        	]
			}
		}
	}.

test_sql_all(X) ->
	try 
    	test_sqls(X),
    	test_sqlp(X),
    	test_sqlb(X)
    catch
        Class:Reason ->  io:format(user, "Exception ~p:~p~n~p~n", [Class, Reason, erlang:get_stacktrace()]),
        ?assert( true == "all tests completed")
    end,
    ok.
	
test_sqls(Sqls) ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L  S T R I N G  T E S T  |~n", []),
    io:format(user, "=================================~n", []),
    sqls_loop(Sqls, 0).

sqls_loop([], _) -> ok;
sqls_loop([Sql|Rest], N) ->
	case re:run(Sql, "select|SELECT", [global, {capture, [1], list}]) of
		nomatch ->	
			sqlp_loop(Rest, N+1);
		_ ->		
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
		      		SqlCollapsed = collapse(Sqlstr),
		      		io:format(user, "~p~n-------------------------------~n", [SqlCollapsed]),
		    		?assertEqual(collapse(string:to_lower(Sql)), string:to_lower(SqlCollapsed)),  		
		    		% ?assertEqual(collapse(Sql), SqlCollapsed),  		
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
		    sqls_loop(Rest, N+1)
	end.

test_sqlp(Sqls) ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L  P R E T T Y  T E S T  |~n", []),
    io:format(user, "=================================~n", []),
    sqlp_loop(Sqls, 0).

sqlp_loop([], _) -> ok;
sqlp_loop([Sql|Rest], N) ->
	case re:run(Sql, "select|SELECT", [global, {capture, [1], list}]) of
		nomatch ->	
			sqlp_loop(Rest, N+1);
		_ ->		
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
		      		SqlCleaned = trim_nl(clean_cr(Sqlstr)),
		    		?assertEqual(trim_nl(clean_cr(string:to_lower(Sql))), string:to_lower(SqlCleaned)),  		
		    		% ?assertEqual(trim_nl(clean_cr(Sql)), SqlCleaned),  		
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
		    sqlp_loop(Rest, N+1)
	end.

test_sqlb(Sqls) ->
    io:format(user, "=================================~n", []),
    io:format(user, "|     S Q L  B O X  T E S T     |~n", []),
    io:format(user, "=================================~n", []),
    sqlb_loop(Sqls, 0).

sqlb_loop([], _) -> ok;
sqlb_loop([Sql|Rest], N) ->
	case re:run(Sql, "select|SELECT", [global, {capture, [1], list}]) of
		nomatch ->	
			sqlp_loop(Rest, N+1);
		_ ->		
		    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
		    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
		    case sql_parse:parse(Tokens) of
		        {ok, [ParseTree|_]} -> 
		        	io:format(user, "-------------------------------~nParseTree:~n", []),
		        	io:format(user, "~p~n", [ParseTree]),
		        	io:format(user, "-------------------------------~n", []),
					Sqlbox = fold_tree(ParseTree, fun sqlb/6, []),
		       		io:format(user, "~n-------------------------------~nSqlbox:~n", []),
		       		io:format(user, "~p~n", [Sqlbox]),
		      		[?assertMatch(#box{ind=0},Box) || Box <- Sqlbox];
		        Error -> 
		        	io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
		        	?assertEqual(ok, Error)
		    end,
		    sqlb_loop(Rest, N+1)
	end.

-define(REG_COL, [
    {"(--.*[\n\r]+)",                             " "}    % comments                      -> removed
  , {"([\n\r\t ]+)",                              " "}    % \r,\n or spaces               -> single space
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
