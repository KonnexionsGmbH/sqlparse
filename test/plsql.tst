%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{verbose, 0}, {tests, []}].

%% 
%% TESTS
%%

"declare begin schm.proc(:p_first,:p_second,:p_result); end".
"declare begin proc(:p_first,:p_second,:p_result); end".
"call proc(:p_first,:p_second,:p_result)".
"call schm.proc(:p_first,:p_second,:p_result)".
"begin schm.proc(:p_first,:p_second,:p_result); end".
"begin proc(:p_first,:p_second,:p_result); end".
