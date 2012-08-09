-module(sql_walk).

-export([walk_tree/1, walk_tree/2, to_json/1]).

-include_lib("eunit/include/eunit.hrl").
-include("sql_box.hrl").
-include("sql_tests.hrl").

to_json(Sql0) ->
    Sql1 = string:strip(Sql0, both),
    Sql = case lists:nth(length(Sql1), Sql1) of
        $; -> Sql1;
        _ -> Sql1 ++ ";"
    end,
    case sql_lex:string(Sql) of
        {error, Error} -> {error, Error};
        {ok, Tokens, _} ->
            case sql_parse:parse(Tokens) of
                {error, Error} -> {error, Error};
                {ok, [ParseTree|_]} ->
                    case (catch walk_tree(ParseTree)) of
                        {'EXIT', Error} -> {error, Error};
                        Rec ->
                            case (catch rec2json(Rec, no_format)) of
                                {'EXIT', Error} -> {error, Error};
                                Json -> Json
                            end
                    end
            end
    end.

precedence(A) when is_atom(A) ->
    {_,P} = lists:keyfind(A, 1,
        [
            {'u+',9},
            {'u-',9},
            {'fun',8},
            {'*',7},
            {'/',7},
            {'+',6},
            {'-',6},
            {'=',5},
            {'<=',5},
            {'>=',5},
            {'<>',5},
            {'<',5},
            {'>',5},
            {'like',5},
            {'is',5},
            {'between',5},
            {'in',4},
            {'not',3},
            {'and',2},
            {'or',1}
        ]
    ),
    P.

walk_tree(SqlParseTree) -> walk_tree(SqlParseTree, #sql_box_rec{}).

% ignore empty fields
walk_tree({_, <<>>}, _Acc) -> [];
walk_tree({_, []}, _Acc) -> [];
walk_tree({_, {}}, _Acc) -> [];

% create record
walk_tree({Op, Args}, undefined) when is_list(Args), length(Args) > 0   -> walk_tree({Op, Args}, #sql_box_rec{});
walk_tree({Op, {Op1,L,R}}, undefined)                                   -> walk_tree({Op, {Op1,L,R}}, #sql_box_rec{});
walk_tree(L, undefined) when is_binary(L)                               -> walk_tree(L, #sql_box_rec{});
walk_tree({Op,Con,D0,D1,D2}, undefined)                                 -> walk_tree({Op,Con,D0,D1,D2}, #sql_box_rec{});
walk_tree({Op,L,R}, undefined)                                          -> walk_tree({Op,L,R}, #sql_box_rec{});
walk_tree({Op,D}, undefined)                                            -> walk_tree({Op,D}, #sql_box_rec{});


% Args as list used as a generic walk detection
walk_tree({Op, Args}, Acc) when is_list(Args), length(Args) > 0 ->    
    Childs0 = [walk_tree(E,undefined) || E <- Args],
    Childs = if (Op =:= 'fields')  or (Op =:= 'from') ->
                comma(Childs0);
                true -> Childs0
    end,
    Acc#sql_box_rec{name=atom_to_list(Op), children=lists:flatten(Childs)};

%%% begning of 1,2,3-tree walk
%%walk_tree({Op, {Op1,L,R}}, Acc) ->
%%    Acc#sql_box_rec{ name=atom_to_list(Op)
%%                   , children=lists:flatten([
%%                         walk_tree(L, undefined)                               % 1st child is left sub-tree
%%                         , #sql_box_rec{ name=atom_to_list(Op1)                % 2nd child is operator
%%                                       , children=[walk_tree(R, undefined)]}   % 1st child of operator is right sub-tree
%%                 ])};
% begning of 1,2,3-tree walk
walk_tree({Op, {_,_,_}=D}, Acc) ->
    Acc#sql_box_rec{name=atom_to_list(Op), children=lists:flatten([walk_tree(D, undefined)])};

% process single tree leaves
walk_tree(L, Acc) when is_binary(L) ->
   Acc#sql_box_rec{name=binary_to_list(L), children=[]};

% recursive binary tree walk
walk_tree({'fun',L,R}, Acc) -> walk_tree({L,R}, Acc); %(functions triggers generic walk)
walk_tree({Op0,L,{Op1,_,_}=R}, Acc) when is_atom(Op0), is_atom(Op1), is_binary(L)->
    P0 = precedence(Op0),
    P1 = precedence(Op1),
    io:format(user, "Prece - ~p ~p~n", [{Op0,Op1},{P0,P1}]),
    Childs = if
        (P0 > P1) ->
            [ walk_tree(L, Acc)
            , #sql_box_rec{ name=atom_to_list(Op0)}
            , #sql_box_rec{children=[#sql_box_rec{name="("}, walk_tree(R, Acc), #sql_box_rec{name=")"}]}
            ];
        (P0 =< P1) ->
            [ walk_tree(L, Acc)
            , #sql_box_rec{ name=atom_to_list(Op0)}
            , walk_tree(R, Acc)
            ]
    end,
    Acc#sql_box_rec{children=Childs};
walk_tree({Op0,{Op1,_,_}=L,R}, Acc) when is_atom(Op0), is_atom(Op1), is_binary(R)->
    P0 = precedence(Op0),
    P1 = precedence(Op1),
    io:format(user, "Prece - ~p ~p~n", [{Op0,Op1},{P0,P1}]),
    Childs = if
        (P0 > P1) ->
            [ #sql_box_rec{children=[#sql_box_rec{name="("}, walk_tree(L, Acc), #sql_box_rec{name=")"}]}
            , #sql_box_rec{ name=atom_to_list(Op0)}
            , walk_tree(R, Acc)
            ];
        (P0 =< P1) ->
            [ walk_tree(L, Acc)
            , #sql_box_rec{ name=atom_to_list(Op0)}
            , walk_tree(R, Acc)
            ]
    end,
    Acc#sql_box_rec{children=Childs};
walk_tree({Op,L,R}, Acc) when is_binary(R) ->
    io:format(user, "Right sub Bin ~p~n", [{Op,L,R}]),
    Acc#sql_box_rec{children=[
        walk_tree(L, Acc)                       % 1st child is left sub-tree
        , #sql_box_rec{name=atom_to_list(Op)}   % 2nd child is operator
        , walk_tree(R, Acc)                     % 3rd child is right sub-tree
    ]};
walk_tree({Op0,{Op1,_,_}=L,{Op2,_,_}=R}, Acc) when is_atom(Op0), is_atom(Op1), is_atom(Op2)->
    P0 = precedence(Op0),
    P1 = precedence(Op1),
    P2 = precedence(Op2),
    io:format(user, "Prece - ~p ~p~n", [{Op0,Op1,Op2},{P0,P1,P2}]),
    Childs = if
        (P0 > P1) and (P0 > P2) ->
            [ #sql_box_rec{children=[#sql_box_rec{name="("}, walk_tree(L, Acc), #sql_box_rec{name=")"}]}
            , #sql_box_rec{ name=atom_to_list(Op0), children=[#sql_box_rec{children=[#sql_box_rec{name="("}, walk_tree(R, Acc), #sql_box_rec{name=")"}]}]}
            ];
        (P0 > P1) and (P0 =< P2) ->
            [ #sql_box_rec{children=[#sql_box_rec{name="("}, walk_tree(L, Acc), #sql_box_rec{name=")"}]}
            , #sql_box_rec{ name=atom_to_list(Op0), children=[walk_tree(R, Acc)]}
            ];
        (P0 =< P1) and (P0 > P2) ->
            [ #sql_box_rec{children=[walk_tree(L, Acc)]}
            , #sql_box_rec{ name=atom_to_list(Op0), children=[#sql_box_rec{children=[#sql_box_rec{name="("}, walk_tree(R, Acc), #sql_box_rec{name=")"}]}]}
            ];
        true ->
            [ #sql_box_rec{children=[walk_tree(L, Acc)]}
            , #sql_box_rec{ name=atom_to_list(Op0), children=[walk_tree(R, Acc)]}
            ]
    end,
    Acc#sql_box_rec{children=Childs};
walk_tree({Op,L,R}, Acc) ->
    Acc#sql_box_rec{children=[
        walk_tree(L, Acc)                       % 1st child is left sub-tree
        , #sql_box_rec{ name=atom_to_list(Op)   % 2nd child is operator
            , children=[walk_tree(R, Acc)]}     % 1st child of operator is right sub-tree
    ]};

% recursive uninary tree walk
walk_tree({Op,D}, Acc) ->
    Acc#sql_box_rec{children=[
        #sql_box_rec{ name=atom_to_list(Op)         % 1st child is operator
                    , children=[walk_tree(D, Acc)]} % 1st child of operator is only sibling
    ]};

% recursive ternary tree walk
walk_tree({between,D0,D1,D2}, Acc) -> walk_tree({between,'and',D0,D1,D2}, Acc); %(e.g. - D0 between D1 and D2)
walk_tree({Op,Con,D0,D1,D2}, Acc) ->
    Acc#sql_box_rec{children=[
        walk_tree(D0, Acc)                           % 1st child is D0
        , #sql_box_rec{name=atom_to_list(Op)}        % 2nd child is operator
        , walk_tree(D1, Acc)                         % 3rd child is D1
        , #sql_box_rec{name=atom_to_list(Con)}       % 4th child is connector
        , walk_tree(D2, Acc)                         % 5th child is D2
    ]}.

comma(L)                                    -> comma(L,[]).
comma([], Acc)                              -> Acc;
comma([E|Rest], Acc) when length(Acc) == 0  -> comma(Rest, Acc ++ [E]);
comma([#sql_box_rec{name=N}=E|Rest], Acc)   -> comma(Rest, Acc ++ [E#sql_box_rec{name=", "++N}]).

rec_sav_json(Sql, Rec, File) ->
   PathPrefix = case lists:last(filename:split(filename:absname(""))) of
       ".eunit" -> "../priv/www/sql/";
       _ -> "./priv/www/sql/"
   end,
   NewFile = PathPrefix++File++".sql",           
   file:write_file(NewFile, list_to_binary(
            "var parsetree = new Object();\n"++
            "parsetree.json = function() {\n"++
                "\tJson =JSON.parse(\n" ++
                rec2json(Rec, format) ++ "'\n);\n" ++
                "\treturn Json;\n" ++
            "}\n" ++
            "parsetree.sql = function() {\n" ++
                "sql = " ++ io_lib:format("~p", [Sql]) ++ ";\n" ++
                "return sql;\n" ++
            "}"
       )),
   file:write_file(PathPrefix++"sqls.js", list_to_binary(
           "var sqlFiles=new Array(\n" ++
           string:join(["\t\"" ++ filename:basename(X) ++ "\"" || X <- filelib:wildcard(PathPrefix++"*.sql")], ",\n") ++
           "\n);"
       )).

rec2json(Rec, Format) -> rec2json(Rec, "", 0, Format).
rec2json(#sql_box_rec{name="fields"}=Rec, Json, T, Format) -> rec2json(Rec#sql_box_rec{name=""}, Json, T, Format);

rec2json(#sql_box_rec{name=N,children=[]}, _Json, _, no_format) -> "{\"name\":\""++N++"\", \"children\":[]}";
rec2json(#sql_box_rec{name=N,children=Childs}, Json, T, no_format) ->
    "{\"name\":\""++N++"\", \"children\":["++
     string:join([rec2json(C, Json, T+1, no_format) || C <- Childs], ",")
    ++"]}";

rec2json(#sql_box_rec{children=[]}=Rec, Json, T, format) -> lists:duplicate(T, $\t)++"'"++rec2json(Rec, Json, T, no_format);
rec2json(#sql_box_rec{name=N,children=Childs}, Json, T, format) ->
    lists:duplicate(T, $\t)++"'{\"name\":\""++N++"\", \"children\":['\n+"++
    string:join([rec2json(C, Json, T+1, format) || C <- Childs], ",'\n+")
    ++"'\n+"++lists:duplicate(T, $\t)++"']}".

walk_test() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L   P A R S E   W A L K  |~n", []),
    io:format(user, "=================================~n", []),
    test_loop(?TEST_SQLS, 0).
test_loop([], _) -> ok;
test_loop([Sql|Sqls], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    io:format(user, "-------------------------------~nlex ok:~n", []),
    case (catch sql_parse:parse(Tokens)) of
        {ok, [ParseTree|_]} -> 
        	io:format(user, "-------------------------------~nParseRec:~n", []),
            case (catch walk_tree(ParseTree)) of
                {'EXIT', Error} ->
                    io:format(user, "Error ~p~n~p~n", [Error, ParseTree]),
                    ?assertEqual(ok, Error);
                Result ->
                    io:format(user, "Rec~n~p~n", [Result]),
                    case (catch rec_sav_json(Sql, Result, "Query" ++ integer_to_list(N))) of
                        {'EXIT', Error} ->
                            io:format(user, "Error ~p~n~p~n", [Error, Result]),
                            ?assertEqual(ok, Error);
                        Json ->
                            io:format(user, "Json~n~p~n", [Json])
                    end
            end,
        	io:format(user, "-------------------------------~n", []);
        {'EXIT', Error} ->
            io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens]),
            ?assertEqual(ok, Error);
        Err ->
            io:format(user, "Failed ~p~nTokens~p~n", [Err, Tokens]),
            ?assertEqual(ok, Err)
    end,
    test_loop(Sqls, N+1).
