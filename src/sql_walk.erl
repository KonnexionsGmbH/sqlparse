-module(sql_walk).

-export([walk_tree/1]).

-include_lib("eunit/include/eunit.hrl").
-include("sql_box.hrl").
-include("sql_tests.hrl").

walk_tree(SqlParseTree) -> walk_tree(SqlParseTree, #sql_box_rec{}).

% ignore empty fields
walk_tree({_, <<>>}, _Acc) -> [];
walk_tree({_, []}, _Acc) -> [];
walk_tree({_, {}}, _Acc) -> [];

% Args as list used as a generic walk detection
walk_tree({Op, Args}, Acc) when is_list(Args), length(Args) > 0 ->
    Childs = lists:foldl(fun(E, A) ->
                A ++ [walk_tree(E,#sql_box_rec{})]
                end,
                [],
                Args),
    Acc#sql_box_rec{name=atom_to_list(Op), children=lists:flatten(Childs)};

% begning of 1,2,3-tree walk
walk_tree({Op, {Op1,L,R}}, Acc) ->
    Acc#sql_box_rec{ name=atom_to_list(Op)
                   , children=lists:flatten([
                         walk_tree(L, #sql_box_rec{})                               % 1st child is left sub-tree
                         , #sql_box_rec{ name=atom_to_list(Op1)                     % 2nd child is operator
                                       , children=[walk_tree(R, #sql_box_rec{})]}   % 1st child of operator is right sub-tree
                 ])};

% process non tree leaves
walk_tree(L, Acc) when is_binary(L) ->
   Acc#sql_box_rec{name=binary_to_list(L), children=[]};

% recursive ternary tree walk
walk_tree({between,D0,D1,D2}, Acc) -> walk_tree({between,'and',D0,D1,D2}, Acc); %(e.g. - D0 between D1 and D2)
walk_tree({Op,Con,D0,D1,D2}, Acc) ->
    Acc#sql_box_rec{children=[
        walk_tree(D0, Acc)                           % 1st child is D0
        , #sql_box_rec{name=atom_to_list(Op)}        % 2nd child is operator
        , walk_tree(D1, Acc)                         % 3rd child is D1
        , #sql_box_rec{name=atom_to_list(Con)}       % 4th child is connector
        , walk_tree(D2, Acc)                         % 5th child is D2
    ]};

% recursive binary tree walk
walk_tree({'fun',L,R}, Acc) -> walk_tree({L,R}, Acc); %(functions triggers generic walk)
walk_tree({Op,L,R}, Acc) ->
    Acc#sql_box_rec{children=[
        walk_tree(L, Acc)                               % 1st child is left sub-tree
        , #sql_box_rec{ name=atom_to_list(Op)           % 2nd child is operator
                      , children=[walk_tree(R, Acc)]}   % 1st child of operator is right sub-tree
    ]};

% recursive uninary tree walk
walk_tree({Op,D}, Acc) ->
    Acc#sql_box_rec{children=[
        #sql_box_rec{ name=atom_to_list(Op)         % 1st child is operator
                    , children=[walk_tree(D, Acc)]} % 1st child of operator is only sibling
    ]}.

rec_sav_json(Sql, Rec, File) ->
   PathPrefix = case lists:last(filename:split(filename:absname(""))) of
       ".eunit" -> "../priv/www/sql/";
       _ -> "./priv/www/sql/"
   end,
   NewFile = PathPrefix++File++".sql",           
   file:write_file(NewFile, list_to_binary(
            "var parsetree = new Object();\n"++
            "parsetree.json = function() {\n"++
                "\tJson =\n" ++
                rec2json(Rec) ++ ";\n" ++
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
       )).

rec2json(Rec) -> rec2json(Rec, "").
rec2json(#sql_box_rec{name=N,children=[]}, _Json) ->
    "{name:"++N++"\", top:0, height:0, children:[]}";
rec2json(#sql_box_rec{name=N,children=Childs}, Json) ->
    "{name:\""++N++"\", top:0, height:0, children:["++
     string:join([rec2json(C, Json) || C <- Childs], ",")
    ++"]}".

walk_test() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L   P A R S E   W A L K  |~n", []),
    io:format(user, "=================================~n", []),
    test_loop(?TEST_SQLS, 0).
test_loop([], _) -> ok;
test_loop([Sql|Sqls], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    case sql_parse:parse(Tokens) of
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
        Error -> io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens])
    end,
    test_loop(Sqls, N+1).
