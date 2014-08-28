%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"REVOKE manage_system FROM admin".
"REVOKE a,b,c FROM user1,user2".
"REVOKE SELECT ON ddTable FROM user_1".
"REVOKE ALL ON schema1.ddTable FROM user1,user2".
"REVOKE EXECUTE ON module1 FROM user1".
"revoke update, delete on ddTable from test_user_1".
