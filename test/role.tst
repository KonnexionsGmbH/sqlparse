%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

% ? "CREATE SCHEMA AUTHORIZATION name_user CREATE ROLE name_role".

"CREATE ROLE role1".
"DROP ROLE role1".
