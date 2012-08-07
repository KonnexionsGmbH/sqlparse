-module(sql2json).

-export([to_json/1
        , to_json/2
        , json_tree/1]).

to_json(Sql) -> to_json(Sql, "Query").
to_json(Sql,File) ->
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} ->
           Json = parse_tree_json(tuple_to_list(ParseTree)),
           PathPrefix = case lists:last(filename:split(filename:absname(""))) of
               ".eunit" -> "../priv/www/";
               _ -> "./priv/www/"
           end,
           NewFile = PathPrefix++File++".sql",           
           file:write_file(NewFile, list_to_binary(
                    "var parsetree = new Object();\n"++
                    "parsetree.json = function() {\n"++
                        "\tJson =\n" ++
                        Json ++ ";\n" ++
                        "\treturn Json;\n" ++
                    "}\n" ++
                    "parsetree.sql = function() {\n" ++
                        "sql = " ++ io_lib:format("~p", [Sql]) ++ ";\n" ++
                        "return sql;\n" ++
                    "}"
               )),
           file:write_file(PathPrefix++"sqls.js", list_to_binary(
                   "var sqlFiles=new Array(\n" ++
                   string:join(["\t\"" ++ filename:absname(X) ++ "\"" || X <- filelib:wildcard(PathPrefix++"*.sql")], ",\n") ++
                   "\n);"
               ));
        Error ->
           io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens])
    end.


parse_tree_json([select|ParseTree]) ->
    [PT|_] = ParseTree,
    "{id:\""++ref()++"\", name:\"select\", data:{}, children:[\n\t"
    ++
    string:join(lists:foldl(fun(Elm, Acc) ->
                                case process_simple_list(Elm) of
                                    Val when length(Val) > 0 ->
                                        Acc ++ [Val];
                                    _ -> Acc
                                 end
                            end,
                [],
                PT), ",")
    ++
    "]}".

process_simple_list({hints,<<>>}) -> [];
process_simple_list({E,V}) ->
    case {process_value(V), E} of
        {undefined, _} -> "";
        {Val, fields} ->
            "{id:\""++ref()++"\", name:\"\", data:{}, children:[\n\t" ++ Val ++ "]}";
        {Val,_} ->
            "{id:\""++ref()++"\", name:\""++atom_to_list(E)++"\", data:{}, children:[\n\t" ++ Val ++ "]}"
    end.

process_value([]) -> undefined;
process_value({}) -> undefined;
process_value(D) -> process_value(D, "").
process_value(B, Buf) when is_binary(B) ->
    Buf ++ "{id:\""++ref()++"\", name:\"" ++ binary_to_list(B) ++ "\", data:{}, children:[]}";
process_value({'not', E}, Buf) ->
    Buf ++
    "{id:\""++ref()++"\", name:\"not\", data:{}, children:[\n\t"
    ++ process_value(E, Buf) ++ "]}";
process_value({'fun', N, Args}, Buf) when is_atom(N) ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(N)++"()\", data:{}, children:[\n\t"
    ++ string:join([process_value(A, Buf) || A <- Args], ",") ++ "]}";
process_value({as, L, R}, Buf) ->
    Buf ++
    "{id:\""++ref()++"\", name:\"as\", data:{}, children:[\n\t"
    ++ process_value(L, Buf) ++ "," ++ process_value(R, Buf) ++ "]}";
process_value({in, L, {list, R}}, Buf) when is_binary(L) ->
    process_value({in, L, R}, Buf);
process_value({in, L, R}, Buf) when is_binary(L), is_tuple(R) ->
    Buf ++
    "{id:\""++ref()++"\", name:\"in\", data:{}, children:[\n\t"
    ++ "{id:\""++ref()++"\", name:\""++binary_to_list(L)++"\", data:{}, children:[]}," ++ parse_tree_json(tuple_to_list(R)) ++ "]}";
process_value({in, L, R}, Buf) when is_binary(L) ->
    Buf ++
    "{id:\""++ref()++"\", name:\"in\", data:{}, children:[\n\t"
    ++ "{id:\""++ref()++"\", name:\""++binary_to_list(L)++"\", data:{}, children:[]}, {id:\""++ref()++"\", name:\"\", data:{}, children:["
    ++ process_value(R, "")
    ++ "]}]}";
process_value({where, {C,L,R}}, Buf) ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
    ++ process_value(L, Buf) ++ "," ++ process_value(R, Buf) ++ "]}";
process_value({between=C,L,L1,R1}, Buf) ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
        ++ "{id:\""++ref()++"\", name:\""++binary_to_list(L)++"\", data:{}, children:[]},"
        ++ "{id:\""++ref()++"\", name:\""++binary_to_list(L1)++"\", data:{}, children:[]},"
        ++ "{id:\""++ref()++"\", name:\""++binary_to_list(R1)++"\", data:{}, children:[]}"
    ++"]}";
process_value({C,L,R}, Buf) when is_atom(C), is_binary(L), is_binary(R)  ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
    ++ "{id:\""++ref()++"\", name:\""++binary_to_list(L)++"\", data:{}, children:[]} , {id:\""++ref()++"\", name:\""++binary_to_list(R)++"\", data:{}, children:[]}]}";
process_value({C,L,R}, Buf) when is_atom(C), is_tuple(L), is_tuple(R)  ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
    ++ process_value(L, Buf) ++ "," ++ process_value(R, Buf) ++ "]}";
process_value(DList, Buf) ->
    Buf ++ string:join([if is_tuple(D) -> process_value(tuple_to_list(D), Buf);
                           is_binary(D) -> "{id:\""++ref()++"\", name:\""++binary_to_list(D)++"\", data:{}, children:[]}";
                           true -> ""
                       end
                       || D <- DList], ",").

ref() -> re:replace(erlang:ref_to_list(erlang:make_ref()),"([#><.])","_",[global,{return, list}]).

json_tree({_,[]}) -> "";
json_tree(Sql) when is_tuple(Sql) -> json_tree(tuple_to_list(Sql));
json_tree([Op|Args]) when is_atom(Op) ->
    Children = string:join(json_tree(Args), ","),
    ["{\"name\":\""++atom_to_list(Op)++"\", \"children\":["++Children++"]}"];
json_tree([Op|Args]) when is_tuple(Op) -> json_tree(Op)++json_tree(Args);
json_tree([[Op|_]=F|Args]) when is_atom(Op) ->
    json_tree(F) ++ json_tree(Args);
json_tree(Ts) when is_list(Ts) ->
    R = lists:foldl(fun(E, AccIn) ->
                    case is_string(E) of
                        true -> ["{\"name\":\""++E++"\", \"children\":[]}"|AccIn];
                        _ ->
                            io:format(user, ".... ~p~n", [E]),
                            [json_tree(E)|AccIn]
                    end
                end,
                [],
                Ts),
    io:format(user, "-- ~p~n", [R]),
    R.

is_string(XY) ->            
    case is_list(XY) of           
        false -> false;           
        true ->
            lists:all(fun(XX) ->         
                        if XX < 0 -> false;  
                           XX > 255 -> false;
                           true -> true      
                        end                  
                      end, XY)
    end.

% sql2json:to_json("select a,b,c from def").

% sql2json:to_json("select a,b,c from abc, def where a in (select b from def) and c=12.5 and e=f or g between h and i").
 
%sql2json:to_json("
%select
%	*
%from
%	abc
%where
%	not	(
%				a=b 
%			and	c=d
%		)
%	or	e=f
%	or	g=h
%").
%
%sql2json:to_json("
%select 
%	a
%	,b
%	,c
%from 
%	abc
%	, def
%").

%   dbg:start().
%   dbg:tracer().
%   dbg:tp(sql2json,[]).
%   dbg:tpl(sql2json,[]).
%   dbg:p(all,c).             

%   {ok, Tokens, _} = sql_lex:string("select
%   	a
%   	,b
%   	,c
%   from
%   	abc
%   where
%           	
%           		a=b 
%           	and	
%           		not	c=d 
%   	or	e=f
%   	or	g=h;").
%    {ok, [P|_]} = sql_parse:parse(Tokens).           
% sql2json:json_tree(P).

-include_lib("eunit/include/eunit.hrl").
-include("sql_tests.hrl").

sql2json_test() ->
    io:format(user, "=========================================~n", []),
    io:format(user, "|  T O   J S O N   P A R S E   T R E E  |~n", []),
    io:format(user, "=========================================~n", []),
    test_parse(?TEST_SQLS, 0).
test_parse([], _) -> ok;
test_parse([Sql|Sqls], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    case (catch to_json(Sql, "Query" ++ integer_to_list(N))) of
        {'EXIT', Error} ->
            io:format(user, "Error ~p~n", [Error]),
            ?assertEqual(ok, Error);
        Result -> ok
    end,
    io:format(user, "-------------------------------~n", []),
    test_parse(Sqls, N+1).
