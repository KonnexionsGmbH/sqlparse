%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"ALTER USER test_user_123 IDENTIFIED BY new_password".
"ALTER USER test_user_123 ACCOUNT LOCK".
"ALTER USER test_user_123 ACCOUNT UNLOCK".
"ALTER USER test_user_123 PASSWORD EXPIRE".
