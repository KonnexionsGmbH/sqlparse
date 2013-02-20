-module(sql_box).

-export([ flat/1 		%% includes parse and validate (removes comments for now)
		, pretty/1 		%% includes parse and validate (removes comments for now)
        , boxed/1 		%% includes parse and validate (removes comments for now)
        ]).

-export([ flat_from_pt/1
		, pretty_from_pt/1
		, boxed_from_pt/1
		, flat_from_box/1
		, pretty_from_box/1
		]).

-export([ sqlb_loop/5	%% needed for test interface (sql iterator)
		]).

-include("sql_parse.hrl").
-include("sql_tests.hrl").
-include("sql_box.hrl").

parse(Sql) ->
	case sql_lex:string(Sql ++ ";") of
		{ok, Tokens, _} -> 
			case sql_parse:parse(Tokens) of
				{ok, [ParseTree|_]} ->
					ParseTree;
				PError -> 
					?ParserException({"Parser Error",{PError,Tokens}})
			end;
		LError ->
			?LexerException({"Lexer Error",LError})
	end.

flat(Sql) ->
	ParseTree = parse(Sql),
	flat_from_pt(ParseTree).

flat_from_pt(ParseTree) ->
	try
		sql_parse:fold(ParseTree)
	catch
		_:Reason ->	?RenderingException({"Cannot render parse tree to flat SQL",{Reason,ParseTree}})
	end.

validate_box([]) -> ok;
validate_box(#box{children=[]}) -> ok;
validate_box(#box{children=CH}) ->
	validate_children(CH), 
	[validate_box(C) || C <-CH];
validate_box([Box|Boxes]) ->
	validate_box(Box),
	validate_box(Boxes).

validate_children([]) -> ok;
validate_children([#box{}]) -> ok;
validate_children(Children) ->
	try
		case length(lists:usort([C#box.ind || C <- Children])) of
			1 ->	ok;
			_ ->	?RenderingException({"Mixed identation error",Children})
		end
	catch
		% _:_ -> 	ChStr = lists:flatten("Illegal box structure " ++ io_lib:format("~p",[Children])),
		% 		?RenderingException({ChStr})
		_:_ -> 	?RenderingException({"Illegal box structure ",Children})
	end.

flat_from_box([]) -> "";
flat_from_box(#box{name= <<",">> ,children=[]}) -> ",";
flat_from_box(#box{name=Name,children=[]}) -> 
 	lists:flatten([" ",binary_to_list(Name)]);
flat_from_box(#box{name= <<>>,children=CH}) -> 
	lists:flatten([[flat_from_box(C) || C <-CH]]);
flat_from_box(#box{name=Name,children=CH}) -> 
	lists:flatten([" ",binary_to_list(Name),[flat_from_box(C) || C <-CH]]);
flat_from_box([Box|Boxes]) ->
	lists:flatten([flat_from_box(Box),flat_from_box(Boxes)]).

pretty(Sql) ->
	ParseTree = parse(Sql),
	pretty_from_pt(ParseTree).

pretty_from_pt(ParseTree) ->
	SqlBox = foldb(ParseTree),
	pretty_from_box(SqlBox).

pretty_from_box([]) -> "";
pretty_from_box(#box{ind=Ind,name=Name,children=[]}) -> 
 	lists:flatten(["\r\n",lists:duplicate(Ind,9),binary_to_list(Name)]);
pretty_from_box(#box{name= <<>>,children=CH}) -> 
	lists:flatten([[pretty_from_box(C) || C <-CH]]);
pretty_from_box(#box{ind=Ind,name=Name,children=CH}) -> 
	lists:flatten(["\r\n",lists:duplicate(Ind,9),binary_to_list(Name),[pretty_from_box(C) || C <-CH]]);
pretty_from_box([Box|Boxes]) ->
	lists:flatten([pretty_from_box(Box),pretty_from_box(Boxes)]).

boxed(Sql) ->
	ParseTree = parse(Sql),
	boxed_from_pt(ParseTree).
	
boxed_from_pt(ParseTree) ->
	foldb(ParseTree).

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
% binding('lists') -> 145;
binding('in') -> 140;
binding('list') -> 135;
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

neItems(List) ->	%% non-empty items of as list
	lists:filter(fun(X)-> X /= empty end,List).

bStr(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
bStr(B) when is_binary(B) -> B;
bStr(C) -> ?RenderingException({"Expected atom or binary, got",C}).

foldb(ParseTree) ->
	foldb(0, ParseTree).	

foldb(Ind, {select, List}) when is_list(List) ->
	Ch = neItems([foldb(Ind+1, Sli) || Sli <- List]),
	fb(Ind, neItems(Ch), select);
foldb(_, {hints,<<>>}) -> empty;
foldb(_, {opt,  <<>>}) -> empty;
foldb(_, {into,   _ }) -> empty;
foldb(_, {where,  []}) -> empty;
foldb(_, {having, {}}) -> empty;
foldb(Ind, T) when is_binary(T);is_atom(T) -> fb(Ind, [], T);
foldb(Ind, {'as', Item, Alias}) ->
	B = foldb(Ind, Item),
	case B#box.children of
		[] ->	EName=lists:flatten([binary_to_list(B#box.name)," as ", binary_to_list(Alias)]),
				B#box{name=list_to_binary(EName)};
		Ch ->	{CF,[CL]} = lists:split(length(Ch)-1,Ch),
				ECName=lists:flatten([binary_to_list(CL#box.name)," as ", binary_to_list(Alias)]),
				EChildren = [CF|[CL#box{name=list_to_binary(ECName)}]],
		 		B#box{children=EChildren}
	end;
foldb(Ind, {hints, Hint}) -> fb(Ind, [], Hint);
foldb(Ind, {opt, Opt}) -> fb(Ind, [], Opt);

foldb(Ind, {where, WC}) ->
	case foldb(Ind+1, WC) of
		Ch when is_list(Ch) -> 
			fb(Ind, Ch, where);
		#box{children=Children} ->
			fb(Ind, Children, where)
	end;

foldb(Ind, {having, HC}) -> 
	fb(Ind, foldb(Ind+1, HC), having);

foldb(Ind, {'fun', Fun, List}) ->
	Ch = foldb_commas(neItems([foldb(Ind+2, Li) || Li <- List])),
	B0 = fb(Ind+1, [], <<"(">>),
	B1 = fb(Ind+1, Ch, <<>>),
	B2 = fb(Ind+1, [], <<")">>),
	Child = fb(Ind, [B0,B1,B2], Fun),
	fb(Ind, [Child],<<>>);
foldb(Ind, {fields, List}) when is_list(List) ->	%% Sli from, 'group by', 'order by'
	Ch = [foldb(Ind+1, Li) || Li <- List],
	fb(Ind, foldb_commas(Ch), bStr(<<>>));
foldb(Ind, {list, List}) when is_list(List) ->	
	Ch = [foldb(Ind, Li) || Li <- List],
	fb(Ind,foldb_commas(Ch), <<>>);
foldb(Ind, {Item, Dir}) when is_binary(Dir) ->	
	B = foldb(Ind, Item),
	case B#box.children of
		[] ->	EName=lists:flatten([binary_to_list(B#box.name)," ", binary_to_list(Dir)]),
				B#box{name=list_to_binary(EName)};
		Ch ->	{CF,[CL]} = lists:split(length(Ch)-1,Ch),
				ECName=lists:flatten([binary_to_list(CL#box.name)," ", binary_to_list(Dir)]),
				EChildren = [CF|[CL#box{name=list_to_binary(ECName)}]],
		 		B#box{children=EChildren}
	end;
foldb(Ind, {Sli, List}) when is_list(List) ->		%% Sli from, 'group by', 'order by'
	case neItems([foldb(Ind+1, Li) || Li <- List]) of
		[] ->	empty; 
		Ch ->	fb(Ind, foldb_commas(Ch), Sli)
	end;

foldb(Ind, {Op, L, R}) when is_atom(Op), is_tuple(L), is_tuple(R) ->
	Bo = binding(Op),
	Bl = binding(L),
	Br = binding(R),
    Fl = if
    	Bo > Bl ->
			L0 = fb(Ind+1, [], <<"(">>),
			L1 = fb(Ind+1, [foldb(Ind+2,L)], <<>>),
			L2 = fb(Ind+1, [], <<")">>),
			fb(Ind, [L0,L1,L2], <<>>);
			%[L0,L1,L2];
        Bo == Bl ->
        	% fb(Ind, [foldb(Ind,L)], <<>>);
        	foldb(Ind,L);
        true ->
        	fb(Ind, foldb(Ind+1,L), <<>>)
        	% foldb(Ind+1,L)
    end,
    Fr = if
    	Bo > Br ->
			R0 = fb(Ind+1, [], <<"(">>),
			R1 = fb(Ind+1, [foldb(Ind+2,R)], <<>>),
			R2 = fb(Ind+1, [], <<")">>),
			%fb(Ind, [R0,R1,R2], <<>>);
			[R0,R1,R2];
        Bo == Br ->
        	% fb(Ind, [foldb(Ind,R)], <<>>);
        	foldb(Ind,R);
        true ->
        	fb(Ind, foldb(Ind+1,R), <<>>)
        	% foldb(Ind+1,R)
    end,
    lists:flatten([Fl,foldb(Ind,Op),Fr]);
    % fb(Ind, [Fl,foldb(Ind,Op),Fr], <<>>);
    % neItems([Fl,foldb(Ind,Op),Fr]);
foldb(Ind, {Op, L, R}) when is_atom(Op), is_binary(L), is_tuple(R) ->
	Bo = binding(Op),
	Br = binding(R),
    Fr = if
    	Bo > Br ->
			R0 = fb(Ind+1, [], <<"(">>),
			R1 = fb(Ind+1, [foldb(Ind+2,R)], <<>>),
			R2 = fb(Ind+1, [], <<")">>),
			fb(Ind, [R0,R1,R2], <<>>);
			% [R0,R1,R2];
        Bo == Br ->
        	% fb(Ind, [foldb(Ind,R)], <<>>);
        	foldb(Ind,R);
        true ->
        	fb(Ind, foldb(Ind+1,R), <<>>)
        	% foldb(Ind+1,R)
    end,
    % fb(Ind, [foldb(Ind,L),foldb(Ind,Op),Fr], <<>>);
    lists:flatten([foldb(Ind,L),foldb(Ind,Op),Fr]);
foldb(Ind, {Op, L, R}) when is_atom(Op), is_tuple(L), is_binary(R) ->
	Bo = binding(Op),
	Bl = binding(L),
    Fl = if
    	Bo > Bl ->
			L0 = fb(Ind+1, [], <<"(">>),
			L1 = fb(Ind+1, [foldb(Ind+2,L)], <<>>),
			L2 = fb(Ind+1, [], <<")">>),
			%fb(Ind, [L0,L1,L2], <<>>);
			[L0,L1,L2];
        Bo == Bl ->
        	% fb(Ind, [foldb(Ind,L)], <<>>);
        	foldb(Ind,L);
        true ->
        	fb(Ind, foldb(Ind+1,L), <<>>)
        	% foldb(Ind+1,L)
    end,
    % fb(Ind, [Fl,foldb(Ind,Op),foldb(Ind,R)], <<>>);
    lists:flatten([Fl,foldb(Ind,Op),foldb(Ind,R)]);
foldb(Ind, {Op, L, R}) when is_atom(Op), is_binary(L), is_binary(R) ->    
    % fb(Ind, [foldb(Ind,L),foldb(Ind,Op),foldb(Ind,R)], <<>>);
    [foldb(Ind,L),foldb(Ind,Op),foldb(Ind,R)];

foldb(_Ind, Term) ->	
	% fb(Ind, [], list_to_binary(io_lib:format("~p",[Term]))).
	Error = {"Unrecognized parse tree term in foldb",Term},
	?RenderingException(Error).

fb(Ind, Child,Name) when is_tuple(Child) ->
	fb(Ind,[Child],Name);
fb(0,Children,Name) ->
	% validate_children(Children),
	#box{ind=0, collapsed=false, children=Children, name=bStr(Name)};
fb(1,Children,Name) ->
	% validate_children(Children),
	#box{ind=1, collapsed=false, children=Children, name=bStr(Name)};
fb(Ind,Children,Name) ->
	% validate_children(Children),
	#box{ind=Ind, collapsed=true, children=Children, name=bStr(Name)}.

foldb_commas(Boxes) ->
	Comma = (hd(Boxes))#box{children=[],name=bStr(<<",">>)},
	[_|Result] = lists:flatten([[Comma,B] || B <- Boxes]),
	Result.


%% TESTS ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

setup() -> ok.

teardown(_) -> ok.

sql_box_test_() ->
	{timeout, 30000, 
		{
	        setup,
	        fun setup/0,
	        fun teardown/1,
	        {with, 
	        	[
	            fun test_sqlb/1
	        	]
			}
		}
	}.
	
test_sqlb(_) ->
    io:format(user, "=================================~n", []),
    io:format(user, "|     S Q L  B O X  T E S T     |~n", []),
    io:format(user, "=================================~n", []),
    sql_test:parse_groups(fun ?MODULE:sqlb_loop/5, false).

sqlb_loop(_, [], _, _, Private) -> Private;
sqlb_loop(PrintParseTree, [Sql|Rest], N, Limit, Private) ->
	case re:run(Sql, "(select\r|SELECT\r)", [global, {capture, [1], list}]) of
		nomatch ->	
			sqlb_loop(PrintParseTree, Rest, N+1, Limit, Private);
		_ ->		
		    io:format(user, "[~p]===============================~n", [N]),
		    io:format(user, Sql ++ "~n", []),
			ParseTree = parse(Sql),
   			print_parse_tree(ParseTree), 
			% SqlBox = fold_tree(ParseTree, fun sqlb/6, []),
			SqlBox = foldb(ParseTree),
			print_box(SqlBox),
			?assertMatch(#box{ind=0},SqlBox),
			?assert(is_list(validate_box(SqlBox))),
			FlatSql = (catch flat_from_box(SqlBox)),
			io:format(user, FlatSql ++ "~n", []),
			?assertEqual(ParseTree, parse(FlatSql)),
			PrettySql = (catch pretty_from_box(SqlBox)),
			io:format(user, PrettySql ++ "~n", []),
			?assertEqual(ParseTree, parse(PrettySql)),
			CleanSql = clean(Sql),
			CleanPrettySql = clean(PrettySql),
			case str_diff(CleanSql,CleanPrettySql) of
				same -> ok;
				Diff ->
					io:format(user, "Sql Difference: ~p~n", [Diff])
			end,
			?assertEqual(CleanSql,CleanPrettySql),
			NewPrivate = sql_test:update_counters(ParseTree, Private),
            if 
            	(Limit =:= 1) -> 
            		NewPrivate; 
            	true ->
                	sqlb_loop(PrintParseTree, Rest, N+1, Limit-1, NewPrivate)
            end
	end.

print_parse_tree(_ParseTree) -> 
	io:format(user, "~p~n", [_ParseTree]),
	ok.

print_box(_Box) -> 
	io:format(user, "~p~n", [_Box]),
	ok.

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

%% Child Type Label ------------------------------------------
% ct([]) -> [];
% ct(L) when is_list(L) -> '[_]';
% ct(<<>>) -> <<>>;
% ct({}) -> {};
% ct({_A}) -> '{_}';
% ct({_A,_B}) -> '{_,_}';
% ct({_A,_B,_C}) -> '{_,_,_}';
% ct(A) when is_atom(A) -> A;
% ct(B) when is_binary(B) -> B;
% ct(X) -> X.

% collapse(Sql) ->
%     lists:foldl(
%         fun({Re,R}, S) -> re:replace(S, Re, R, [{return, list}, global]) end,
%         Sql,
%         ?REG_COL).

clean(Sql) ->
	trim_nl(clean_cr(string:to_lower(Sql))).

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
