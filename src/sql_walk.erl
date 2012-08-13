-module(sql_walk).

-export([walk_tree/1, walk_tree/3, to_json/1]).

-export([cb/0]).

-include_lib("eunit/include/eunit.hrl").
-include("sql_box.hrl").
-include("sql_tests.hrl").

%-define(logf, ok).

-ifdef(logf).
-define(LOG(F, A), io:format(user, "{~p,~p}:"++F, [?MODULE,?LINE,A])).
-else.
-define(LOG(F, A), true).
-endif.

% Webmachine safe interface
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
             {'u+',9}
            ,{'u-',9}
            ,{'fun',8}
            ,{'*',7}
            ,{'/',7}
            ,{'+',6}
            ,{'-',6}
            ,{'as',5}
            ,{'=',5}
            ,{'<=',5}
            ,{'>=',5}
            ,{'<>',5}
            ,{'<',5}
            ,{'>',5}
            ,{'like',5}
            ,{'is',5}
            ,{'between',5}
            ,{'in',4}
            ,{'not',3}
            ,{'and',2}
            ,{'or',1}
        ]
    ),
    P.

walk_tree(SqlParseTree) -> walk_tree(SqlParseTree, #box{}, 0).

% ignore empty fields
walk_tree({_, <<>>}, _Acc, _) -> [];
walk_tree({_, []},   _Acc, _) -> [];
walk_tree({_, {}},   _Acc, _) -> [];

% create record
walk_tree({Op, Args}, undefined, Dep) when is_list(Args), length(Args) > 0   -> walk_tree({Op, Args}, #box{}, Dep);
walk_tree({Op, {Op1,L,R}}, undefined, Dep)                                   -> walk_tree({Op, {Op1,L,R}}, #box{}, Dep);
walk_tree(L, undefined, Dep) when is_binary(L)                               -> walk_tree(L, #box{}, Dep);
walk_tree({Op,Con,D0,D1,D2}, undefined, Dep)                                 -> walk_tree({Op,Con,D0,D1,D2}, #box{}, Dep);
walk_tree({Op,L,R}, undefined, Dep)                                          -> walk_tree({Op,L,R}, #box{}, Dep);
walk_tree({Op,D}, undefined, Dep)                                            -> walk_tree({Op,D}, #box{}, Dep);


% Args as list used as a generic walk detection
walk_tree({Op0, Args}, Acc, Dep) when is_list(Args), length(Args) > 0 ->    
    Childs0 = [walk_tree(E,undefined, Dep+1) || E <- Args],
    {Op, Childs} = case Op0 of
        'fields'    -> {atom_to_list(Op0), lists:flatten(comma(Childs0))};
        'from'      -> {atom_to_list(Op0), lists:flatten(comma(Childs0))};
        'order by'  ->
            Childs1 = [
                case O of
                    <<>> -> walk_tree(E,undefined, Dep+1);
                    _ -> walk_tree(E,undefined, Dep+1)
                end
            || {E, O} <- Args
            ],
            {atom_to_list(Op0), lists:flatten(comma(Childs1))};
        'list'      -> {"",                lists:flatten([#box{name="("}, comma(Childs0), #box{name=")"}])};
        {'fun',Op1} -> {atom_to_list(Op1), lists:flatten([#box{name="("}, comma(Childs0), #box{name=")"}])};
        _           -> {atom_to_list(Op0), lists:flatten(Childs0)}
    end,
    case Op of
        "select" when Dep > 0 -> Acc#box{children=[ #box{name="("}
                                             , #box{name=Op, children=Childs}
                                             , #box{name=")"}
                                             ]};
        _        -> Acc#box{name=Op, children=Childs}
    end;

%%% begning of 1,2,3-tree walk
%%walk_tree({Op, {Op1,L,R}, Dep}, Acc) ->
%%    Acc#box{ name=atom_to_list(Op)
%%                   , children=lists:flatten([
%%                         walk_tree(L, undefined, Dep+1)                               % 1st child is left sub-tree
%%                         , #box{ name=atom_to_list(Op1)                % 2nd child is operator
%%                                       , children=[walk_tree(R, undefined, Dep+1)]}   % 1st child of operator is right sub-tree
%%                 ])};
% begning of 1,2,3-tree walk
walk_tree({Op, {_,_,_}=D}, Acc, Dep) when (Op =:= 'where') or (Op =:= 'having') ->
    Acc#box{name=atom_to_list(Op), children=lists:flatten([walk_tree(D, undefined, Dep+1)])};

% process single tree leaves
walk_tree(L, Acc, _) when is_binary(L) ->
   Acc#box{name=binary_to_list(L), children=[]};

% recursive binary tree walk
walk_tree({'fun',L,R}, Acc, Dep) -> walk_tree({{'fun',L},R}, Acc, Dep+1); %(functions triggers generic walk)
walk_tree({Op0,L,{Op1,_,_}=R}, Acc, Dep) when is_atom(Op0), is_atom(Op1), is_binary(L)->
    ?LOG("Prece - ~p", [{Op0,Op1}]),
    P0 = precedence(Op0),
    P1 = precedence(Op1),
    ?LOG(" ~p~n", [{P0,P1}]),
    Childs = if
        (P0 > P1) ->
            [ walk_tree(L, Acc, Dep+1)
            , #box{ name=atom_to_list(Op0)}
            , #box{children=[#box{name="("}, walk_tree(R, Acc, Dep+1), #box{name=")"}]}
            ];
        (P0 =< P1) ->
            [ walk_tree(L, Acc, Dep+1)
            , #box{ name=atom_to_list(Op0)}
            , walk_tree(R, Acc, Dep+1)
            ]
    end,
    Acc#box{children=Childs};
walk_tree({Op0,{Op1,_,_}=L,R}, Acc, Dep) when is_atom(Op0), is_atom(Op1), is_binary(R)->
    ?LOG("Prece - ~p", [{Op0,Op1}]),
    P0 = precedence(Op0),
    P1 = precedence(Op1),
    ?LOG(" ~p~n", [{P0,P1}]),
    Childs = if
        (P0 > P1) ->
            [ #box{children=[#box{name="("}, walk_tree(L, Acc, Dep+1), #box{name=")"}]}
            , #box{ name=atom_to_list(Op0)}
            , walk_tree(R, Acc, Dep+1)
            ];
        (P0 =< P1) ->
            [ walk_tree(L, Acc, Dep+1)
            , #box{ name=atom_to_list(Op0)}
            , walk_tree(R, Acc, Dep+1)
            ]
    end,
    Acc#box{children=Childs};
walk_tree({Op,L,R}, Acc, Dep) when is_binary(R) ->
    ?LOG("Right sub Bin ~p~n", [{Op,L,R}]),
    Acc#box{children=[
        walk_tree(L, Acc, Dep+1)                       % 1st child is left sub-tree
        , #box{name=atom_to_list(Op)}   % 2nd child is operator
        , walk_tree(R, Acc, Dep+1)                     % 3rd child is right sub-tree
    ]};
walk_tree({Op0,{Op1,_,_}=L,{Op2,_,_}=R}, Acc, Dep) when is_atom(Op0), is_atom(Op1), is_atom(Op2)->
    ?LOG("Prece - ~p", [{Op0,Op1,Op2}]),
    P0 = precedence(Op0),
    P1 = precedence(Op1),
    P2 = precedence(Op2),
    ?LOG(" ~p~n", [{P0,P1,P2}]),
    Childs = if
        (P0 > P1) and (P0 > P2) ->
            [ #box{children=[#box{name="("}, walk_tree(L, Acc, Dep+1), #box{name=")"}]}
            , #box{ name=atom_to_list(Op0), children=[#box{children=[#box{name="("}, walk_tree(R, Acc, Dep+1), #box{name=")"}]}]}
            ];
        (P0 > P1) and (P0 =< P2) ->
            [ #box{children=[#box{name="("}, walk_tree(L, Acc, Dep+1), #box{name=")"}]}
            , #box{ name=atom_to_list(Op0), children=[walk_tree(R, Acc, Dep+1)]}
            ];
        (P0 =< P1) and (P0 > P2) ->
            [ #box{children=[walk_tree(L, Acc, Dep+1)]}
            , #box{ name=atom_to_list(Op0), children=[#box{children=[#box{name="("}, walk_tree(R, Acc, Dep+1), #box{name=")"}]}]}
            ];
        true ->
            [ #box{children=[walk_tree(L, Acc, Dep+1)]}
            , #box{ name=atom_to_list(Op0), children=[walk_tree(R, Acc, Dep+1)]}
            ]
    end,
    Acc#box{children=Childs};
walk_tree({Op,L,R}, Acc, Dep) ->
    Acc#box{children=[
        walk_tree(L, Acc, Dep+1)                       % 1st child is left sub-tree
        , #box{ name=atom_to_list(Op)   % 2nd child is operator
            , children=[walk_tree(R, Acc, Dep+1)]}     % 1st child of operator is right sub-tree
    ]};

% recursive unary tree walk
% - precedence check
walk_tree({Op0,{Op1,_,_}=D}, Acc, Dep) when is_atom(Op0), is_atom(Op1) ->
    ?LOG("Prece - ~p", [{Op0,Op1}]),
    P0 = precedence(Op0),
    P1 = precedence(Op1),
    ?LOG(" ~p~n", [{P0,P1}]),
    Childs = if
        (P0 > P1) ->
            [#box{name=atom_to_list(Op0)}
            , #box{children=[#box{name="("}, walk_tree(D, Acc, Dep+1), #box{name=")"}]}
            ];
        (P0 =< P1) ->
            [#box{name=atom_to_list(Op0)}
            , walk_tree(D, Acc, Dep+1)
            ]
    end,
    Acc#box{children=Childs};
% - handling {atom,binary} pattern atom is ignored in specific cases
walk_tree({'opt',D}, Acc, _) when is_binary(D) -> Acc#box{name=binary_to_list(D)};
% - generic unary processing
walk_tree({Op,D}, Acc, Dep) ->
    Acc#box{children=[
        #box{ name=atom_to_list(Op)         % 1st child is operator
                    , children=[walk_tree(D, Acc, Dep)]} % 1st child of operator is only sibling
    ]};

% recursive ternary tree walk
walk_tree({between,D0,D1,D2}, Acc, Dep) -> walk_tree({between,'and',D0,D1,D2}, Acc, Dep+1); %(e.g. - D0 between D1 and D2)
walk_tree({Op,Con,D0,D1,D2}, Acc, Dep) ->
    Acc#box{children=[
        walk_tree(D0, Acc, Dep+1)                           % 1st child is D0
        , #box{name=atom_to_list(Op)}        % 2nd child is operator
        , walk_tree(D1, Acc, Dep+1)                         % 3rd child is D1
        , #box{name=atom_to_list(Con)}       % 4th child is connector
        , walk_tree(D2, Acc, Dep+1)                         % 5th child is D2
    ]}.

comma(L)                                    ->
    case (catch comma(L,[])) of
        {'EXIT', _Error} ->
            io:format(user, "comma : error, param ~p~n", [L]),
            throw(comma_error);
        C -> C
    end.
comma([], Acc)                                              -> Acc;
comma([E|Rest], Acc) when length(Acc) == 0                  -> comma(Rest, Acc ++ [E]);
comma([E|Rest], Acc) when is_list(E), length(E) == 0        -> comma(Rest, Acc ++ [E]);
comma([#box{name=N}=E|Rest], Acc)                   -> comma(Rest, Acc ++ [E#box{name=", "++N}]).

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

-define(SAMPLE, 
    {box,0,"select",[
            {box,1,"where",[
                    {box,2,"or",[
                            {box,3,"0", []}
                            ,{box,3,"=",[]}
                            ,{box,3,"",[
                                    {box,4,"nvl",[
                                            {box,5,")",[]}
                                            ,{box,5,"(",[
                                                    {box,6,"0",[]}
                                                    ,{box,6,",",[]}
                                                    ,{box,6,"a", []}]}]}]}]}
                    ,{box,2,"",[
                            {box,3,"b",[]}
                            ,{box,3,"=",[]}
                            ,{box,3,"a",[]}]}]}
            ,{box,2,"abc",[]}
            ,{box,2,"abc",[]}
            ,{box,2,"def",[]}
            ,{box,1,"",[
                    {box,2,"a",[]}
                    ,{box,2,",",[]}
                    ,{box,2,"b as bb",[]}
                    ,{box,2,",",[]}
                    ,{box,2,"c",[]}]}
            ,{box,1,"/*+003*/",[]}]}
).

cb() ->
    io:format("Prev ~p~n", [?SAMPLE]),
    Box = correct_box(?SAMPLE),
    io:format("After ~p~n", [Box]).

correct_box(#box{children=[]}=Rec) -> Rec;
correct_box(#box{children=Childs}=Rec) -> Rec#box{children=lists:reverse([correct_box(C) || C <- Childs])}.

rec2json(Rec, Format) -> rec2json(Rec, "", 0, Format).
rec2json(#box{name="fields"}=Rec, Json, T, Format) -> rec2json(Rec#box{name=""}, Json, T, Format);
rec2json(#box{name="hints"}=Rec, Json, T, Format)  -> rec2json(Rec#box{name=""}, Json, T, Format);

rec2json(#box{name=N,children=[]}, _Json, _, no_format) -> "{\"name\":\""++N++"\", \"children\":[]}";
rec2json(#box{name=N,children=Childs}, Json, T, no_format) ->
    "{\"name\":\""++N++"\", \"children\":["++
     string:join([rec2json(C, Json, T+1, no_format) || C <- Childs], ",")
    ++"]}";

rec2json(#box{children=[]}=Rec, Json, T, format) -> lists:duplicate(T, $\t)++"'"++rec2json(Rec, Json, T, no_format);
rec2json(#box{name=N,children=Childs}, Json, T, format) ->
    lists:duplicate(T, $\t)++"'{\"name\":\""++N++"\", \"children\":['\n+"++
    string:join([rec2json(C, Json, T+1, format) || C <- Childs], ",'\n+")
    ++"'\n+"++lists:duplicate(T, $\t)++"']}".

rec2sql(Rec, Format) -> rec2sql(Rec, "", 0, Format).
rec2sql(#box{name="fields"}=Rec, Str, T, Format) -> rec2sql(Rec#box{name=""}, Str, T+1, Format);
rec2sql(#box{name="hints"}=Rec, Str, T, Format)  -> rec2sql(Rec#box{name=""}, Str, T-2, Format);

rec2sql(#box{name=N,children=[]}, _Str, _, no_format) -> N;
rec2sql(#box{name=N,children=Childs}, Str, T, no_format) ->
     N ++ " " ++ string:join([rec2sql(C, Str, T+1, no_format) || C <- Childs], " ");

rec2sql(#box{name=N,children=[]}, _Str, _, format) -> N;
rec2sql(#box{name=N,children=Childs}, Str, T, format) ->
    if length(N) > 0 -> N ++ "\n"; true -> "" end
    ++
    lists:duplicate(T, $\t)
    ++
    string:join([rec2sql(C, Str, T+1, format) || C <- Childs], "\n")
    ++
    "\t".

walk_test() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L   P A R S E   W A L K  |~n", []),
    io:format(user, "=================================~n", []),
    test_loop(?TEST_SQLS, 0).
test_loop([], _) -> ok;
test_loop([Sql|Sqls], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    io:format(user, "-------------------------------~nlex: ok~n", []),
    case (catch sql_parse:parse(Tokens)) of
        {ok, [ParseTree|_]} -> 
        	io:format(user, "-------------------------------~nparse: ok~n", []),
            case (catch walk_tree(ParseTree)) of
                {'EXIT', Error} ->
                    io:format(user, "Error ~p~n~p~n", [Error, ParseTree]),
                    ?assertEqual(ok, nok);
                Result ->
        	        io:format(user, "-------------------------------~nbox: ok~n", []),
                    case (catch rec_sav_json(Sql, Result, "Query" ++ integer_to_list(N))) of
                        {'EXIT', Error0} ->
                            io:format(user, "Error ~p~n~p~n", [Error0, Result]),
                            ?assertEqual(ok, nok);
                        Json ->
        	                io:format(user, "-------------------------------~n", []),
                            io:format(user, "Json: ~p~n", [Json])
                    end,
                    case (catch rec2sql(Result, no_format)) of
                        {'EXIT', Error1} ->
                            io:format(user, "Error ~p~n~p~n", [Error1, Result]),
                            ?assertEqual(ok, nok);
                        StrPlain ->
        	                io:format(user, "-------------------------------~n", []),
                            io:format(user, "Sql Plain~n~p~n", [StrPlain]),
                            Orig = sql_parse:collapse(Sql),
                            Genr = sql_parse:collapse(StrPlain),
                            case Genr of
                                Orig -> ok;
                                _ ->
                                    io:format(user, "Error ~p~n", [ParseTree]),
        	                        io:format(user, "-------------------------------~n", []),
                                    {S1, S2} = sql_parse:str_diff(Orig,Genr),
                                    io:format(user, "Diff ~p~n     ~p~n", [S1, S2]),
                                    io:format(user, "Src ~p~n", [Orig]),
                                    io:format(user, "Dst ~p~n", [Genr]),
        	                        io:format(user, "-------------------------------~n", []),
                                    ?assertEqual(Orig, Genr)
                            end
                    end
                    %case (catch rec2sql(Result, format)) of
                    %    {'EXIT', Error2} ->
                    %        io:format(user, "Error ~p~n~p~n", [Error2, Result]),
                    %        ?assertEqual(ok, nok);
                    %    Str ->
        	        %        io:format(user, "-------------------------------~n", []),
                    %        io:format(user, "Sql Pretty~n"++Str++"~n", []),
                    %        Src = sql_parse:trim_nl(sql_parse:clean_cr(Sql)),
                    %        Dst = sql_parse:trim_nl(sql_parse:clean_cr(Str)),
                    %        case Dst of
                    %            Src -> ok;
                    %            _ ->
                    %                io:format(user, "Error ~p~n", [ParseTree]),
        	        %                io:format(user, "-------------------------------~n", []),
                    %                {S1, S2} = sql_parse:str_diff(Src,Dst),
                    %                io:format(user, "Diff ~p~n     ~p~n", [S1, S2]),
                    %                io:format(user, "Src ~p~n", [Src]),
                    %                io:format(user, "Dst ~p~n", [Dst]),
        	        %                io:format(user, "-------------------------------~n", []),
                    %                ?assertEqual(Src, Dst)
                    %        end
                    %end
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
