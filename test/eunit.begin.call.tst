%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: BEGIN & CALL
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure_call
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"begin create schema authorization schema_1;create table table_1(column_1 char);grant admin to public;create view table_1 as select * from dual;end".
"begin create schema authorization schema_1;create view table_1 as select * from dual;end".
"begin create schema authorization schema_1;end".
"begin dbms_output.put_line('Hello World!'); dbms_output.put_line('Goodbye cruel World!'); end;".
"begin package_1.procedure_1(parameter_1);end".
"begin package_1.procedure_1(parameter_1);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|;end".
"begin package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin package_1.procedure_1(parameter_1,parameter_2);end".
"begin package_1.procedure_1(parameter_1,parameter_2);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end".
"begin package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin package_1.procedure_1(parameter_1,parameter_2,parameter_3);end".
"begin package_1.procedure_1(parameter_1,parameter_2,parameter_3);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;sum;end".
"begin package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end".
"begin package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin proc(:p_first,:p_second,:p_result); end".
"begin proc(:p_first,:p_second,:p_result); end".
"begin procedure_1(parameter_1);end".
"begin procedure_1(parameter_1);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin procedure_1(parameter_1)|:b.f[f(p.r:q)]|;end".
"begin procedure_1(parameter_1)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin procedure_1(parameter_1,parameter_2);end".
"begin procedure_1(parameter_1,parameter_2);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end".
"begin procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin procedure_1(parameter_1,parameter_2,parameter_3);end".
"begin procedure_1(parameter_1,parameter_2,parameter_3);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end".
"begin procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin schema_1.package_1.procedure_1(parameter_1);end".
"begin schema_1.package_1.procedure_1(parameter_1);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin schema_1.package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|;end".
"begin schema_1.package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2);end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3);end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end".
"begin schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin schm.proc(:p_first,:p_second,:p_result); dbms_output.put_line('Goodbye cruel World!'); end".
"begin schm.proc(:p_first,:p_second,:p_result); end".
"begin schm.proc(:p_first,:p_second,:p_result); end".
"begin sum(*);end".
"begin sum(*);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(*)|:b.f[f(p.r:q)]|;end".
"begin sum(*)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin sum(all parameter_1);end".
"begin sum(all parameter_1);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(all parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(all parameter_1)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin sum(distinct parameter_1);end".
"begin sum(distinct parameter_1);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(distinct parameter_1)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin sum(parameter_1);end".
"begin sum(parameter_1);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(parameter_1)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin sum(parameter_1,parameter_2);end".
"begin sum(parameter_1,parameter_2);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end".
"begin sum(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin sum(parameter_1,parameter_2,parameter_3);end".
"begin sum(parameter_1,parameter_2,parameter_3);sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end".
"begin sum(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"begin sum;end".
"begin sum;sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end".
"begin sum|:b.f[f(p.r:q)]|;end".
"begin sum|:b.f[f(p.r:q)]|;max(parameter_1,parameter_2,parameter_3);end".
"call package_1.procedure_1(parameter_1)".
"call package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|".
"call package_1.procedure_1(parameter_1,parameter_2)".
"call package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|".
"call package_1.procedure_1(parameter_1,parameter_2,parameter_3)".
"call package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|".
"call proc(:p_first,:p_second,:p_result)".
"call procedure_1(parameter_1)".
"call procedure_1(parameter_1)|:b.f[f(p.r:q)]|".
"call procedure_1(parameter_1,parameter_2)".
"call procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|".
"call procedure_1(parameter_1,parameter_2,parameter_3)".
"call procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|".
"call schema_1.package_1.procedure_1(parameter_1)".
"call schema_1.package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|".
"call schema_1.package_1.procedure_1(parameter_1,parameter_2)".
"call schema_1.package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|".
"call schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3)".
"call schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|".
"call schm.proc(:p_first,:p_second,:p_result)".
"call sum".
"call sum(*)".
"call sum(*)|:b.f[f(p.r:q)]|".
"call sum(all parameter_1)".
"call sum(all parameter_1)|:b.f[f(p.r:q)]|".
"call sum(distinct parameter_1)".
"call sum(distinct parameter_1)|:b.f[f(p.r:q)]|".
"call sum(parameter_1)".
"call sum(parameter_1)|:b.f[f(p.r:q)]|".
"call sum(parameter_1,parameter_2)".
"call sum(parameter_1,parameter_2)|:b.f[f(p.r:q)]|".
"call sum(parameter_1,parameter_2,parameter_3)".
"call sum(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|".
"call sum|:b.f[f(p.r:q)]|".

"begin update a set d = g where e=f; insert into a values(1,2); end;".

%% -----------------------------------------------------------------------------
%% TESTS: BEGIN & CALL
%% =============================================================================
