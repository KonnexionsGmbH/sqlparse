-module(sql2json).

-export([to_json/1]).

to_json(Sql) ->
    Sql0 = string:strip(Sql, both),
    {ok, Tokens, _} = sql_lex:string(Sql0 ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} ->
           Json = parse_tree_json(tuple_to_list(ParseTree)),
           io:format(user, "Json = ~n"++Json++"~n", []);
        Error ->
           io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens])
    end.

parse_tree_json([select|ParseTree]) ->
    "{id:\""++ref()++"\", name:\"select\", data:{}, children:[\n\t"
    ++
    string:join(lists:foldl(fun(Elm, Acc) ->
                                case process_simple_list(Elm) of
                                    Val when length(Val) > 0 ->
                                        Acc ++ [process_simple_list(Elm)];
                                    _ -> Acc
                                 end
                            end,
                [],
                ParseTree), ",")
    ++
    "]}".

process_simple_list({E,V}) ->
    case process_value(V) of
        undefined -> "";
        Val -> "{id:\""++ref()++"\", name:\""++atom_to_list(E)++"\", data:{}, children:[\n\t"
               ++ Val ++ "]}"
    end.

process_value([]) -> undefined;
process_value(D) -> process_value(D, "").
process_value({in, L, R}, Buf) when is_list(L), is_tuple(R) ->
    Buf ++
    "{id:\""++ref()++"\", name:\"in\", data:{}, children:[\n\t"
    ++ "{id:\""++ref()++"\", name:\""++L++"\", data:{}, children:[]}," ++ parse_tree_json(tuple_to_list(R)) ++ "]}";
process_value({where, {C,L,R}}, Buf) ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
    ++ process_value(L, Buf) ++ "," ++ process_value(R, Buf) ++ "]}";
process_value({between=C,L,{L1,R1}}, Buf) ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
        ++ "{id:\""++ref()++"\", name:\""++L++"\", data:{}, children:[]},"
        ++ "{id:\""++ref()++"\", name:\".\", data:{}, children:["++
            "\n\t{id:\""++ref()++"\", name:\""++L1++"\", data:{}, children:[]}, {id:\""++ref()++"\", name:\""++R1++"\", data:{}, children:[]}"
        ++ "]}"
    ++"]}";
process_value({C,L,R}, Buf) when is_atom(C), is_list(L), is_list(R) ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
    ++ "{id:\""++ref()++"\", name:\""++L++"\", data:{}, children:[]} , {id:\""++ref()++"\", name:\""++R++"\", data:{}, children:[]}]}";
process_value({C,L,R}, Buf) when is_atom(C), is_tuple(L), is_tuple(R)  ->
    Buf ++
    "{id:\""++ref()++"\", name:\""++atom_to_list(C)++"\", data:{}, children:[\n\t"
    ++ process_value(L, Buf) ++ "," ++ process_value(R, Buf) ++ "]}";
process_value(DList, Buf) ->
    Buf ++ string:join(["{id:\""++ref()++"\", name:\""++D++"\", data:{}, children:[]}" || D <- DList], ",").

ref() -> re:replace(erlang:ref_to_list(erlang:make_ref()),"([#><.])","_",[global,{return, list}]).

% sql2json:to_json("select a,b,c from def").
% sql2json:to_json("select a,b,c from abc, def where a in (select b from def) and c=d and e=f or g between h and i").

