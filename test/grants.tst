%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"GRANT manage_system TO user_1".
"GRANT a,b,c TO user2".
"GRANT SELECT ON ddTable TO user_1".
"GRANT SELECT ON schema1.ddTable TO user_1".
"GRANT ALL ON ddTable TO user1,user2".
"GRANT EXECUTE ON module1 TO user1".
"GRANT all privileges ON schema1.ddTable TO role2".
"grant update, delete on ddTable to test_user_1".
"grant insert on ddTable to test_user_1 WITH GRANT OPTION".
"GRANT manage_system TO test_user_1 with admin option".
