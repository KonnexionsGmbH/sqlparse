%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% 
%% TESTS
%%

"declare begin schm.proc(:p_first,:p_second,:p_result); end".
"declare begin proc(:p_first,:p_second,:p_result); end".
"call proc(:p_first,:p_second,:p_result)".
"call schm.proc(:p_first,:p_second,:p_result)".
"begin schm.proc(:p_first,:p_second,:p_result); end".
"begin proc(:p_first,:p_second,:p_result); end".
"declare begin schm.proc(:p_first,:p_second,:p_result); dbms_output.put_line('Goodbye cruel World!'); end".
"begin dbms_output.put_line('Hello World!'); dbms_output.put_line('Goodbye cruel World!'); end;".
"begin update a set d = g where e=f; insert into a values(1,2); end;".
