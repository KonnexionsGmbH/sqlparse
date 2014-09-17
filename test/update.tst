%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"UPDATE employees set salary = :sal where employee_id = :id".
"UPDATE employees set salary = :sal where employee_id = :id RETURNING c,d INTO :c, :d".
"UPDATE employees set salary = :sal where employee_id = :id RETURNING lob_column INTO :out_locator".
"ALTER USER test_user_123 IDENTIFIED BY new_password".

% Unsupported tests
"ALTER USER test_user_123 ACCOUNT LOCK".
"ALTER USER test_user_123 ACCOUNT UNLOCK".
"ALTER USER test_user_123 PASSWORD EXPIRE".
"UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a is NULL".
"UPDATE abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a || b = 'c' || 'd'".
