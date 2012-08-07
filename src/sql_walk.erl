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

% process non biary tree leaves
walk_tree(L, Acc) when is_binary(L) -> Acc#sql_box_rec{name=binary_to_list(L), children=[]};

% Args as list used as a non-binary walk detection
walk_tree({Op, Args}, Acc) when is_list(Args), length(Args) > 0 ->
    Childs = lists:foldl(fun(E, A) ->
                A ++ [walk_tree(E,#sql_box_rec{})]
                end,
                [],
                Args),
    Acc#sql_box_rec{name=atom_to_list(Op), children=lists:flatten(Childs)};

% begning of binary walk
walk_tree({Op, {Op1,L,R}}, Acc) ->
    Acc#sql_box_rec{ name=atom_to_list(Op)
                 , children=lists:flatten([
                         walk_tree(L, [])                            % 1st child is left sub-tree
                         , #sql_box_rec{ name=atom_to_list(Op1)      % 2nd child is operator
                                       , children=walk_tree(R, [])}  % 1st child of operator is right sub-tree
                 ])};

% both leaf
walk_tree({Op,L,R}, Acc) when is_binary(L), is_binary(R) ->
    Acc ++
    [leaf_to_rec(L)                              % 1st child is left sub-tree
    , #sql_box_rec{ name=atom_to_list(Op)        % 2nd child is operator
                  , children=[leaf_to_rec(R)]}   % 1st child of operator is right sub-tree
    ];


% left leaf
walk_tree({Op,L,R}, Acc) when is_binary(L) ->
    Acc ++
    [leaf_to_rec(L)                              % 1st child is left sub-tree
    , #sql_box_rec{ name=atom_to_list(Op)        % 2nd child is operator
                  , children=walk_tree(R, Acc)}  % 1st child of operator is right sub-tree
    ];

% right leaf
walk_tree({Op,L,R}, Acc) when is_binary(R) ->
    Acc ++
    [walk_tree(L, Acc)                           % 1st child is left sub-tree
    , #sql_box_rec{ name=atom_to_list(Op)        % 2nd child is operator
                  , children=[leaf_to_rec(R)]}   % 1st child of operator is right sub-tree
    ];

% recursive binary tree walk
walk_tree({Op,L,R}, Acc) ->
    Acc ++
    [walk_tree(L, Acc)                           % 1st child is left sub-tree
    , #sql_box_rec{ name=atom_to_list(Op)        % 2nd child is operator
                  , children=walk_tree(R, Acc)}  % 1st child of operator is right sub-tree
    ].

leaf_to_rec(L) when is_binary(L) -> #sql_box_rec{name=binary_to_list(L), children=[]}.

walk_test() ->
    io:format(user, "=================================~n", []),
    io:format(user, "|  S Q L   P A R S E   W A L K  |~n", []),
    io:format(user, "=================================~n", []),
    test_loop(?TEST_SQLS0, 0).
test_loop([], _) -> ok;
test_loop([Sql|Sqls], N) ->
    io:format(user, "[~p]===============================~nSql: "++Sql++"~n", [N]),
    {ok, Tokens, _} = sql_lex:string(Sql ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} -> 
        	io:format(user, "-------------------------------~nParseRec:~n", []),
        	io:format(user, "~p~n", [walk_tree(ParseTree)]),
        	io:format(user, "-------------------------------~n", []);
        Error -> io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens])
    end,
    test_loop(Sqls, N+1).
