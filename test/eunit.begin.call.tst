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

"BEGIN KILL_SESSION(1502,41617); END;".
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

"begin call package_1.procedure_1(parameter_1);end;".
"begin call package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|;end;".
"begin call package_1.procedure_1(parameter_1,parameter_2);end;".
"begin call package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end;".
"begin call package_1.procedure_1(parameter_1,parameter_2,parameter_3);end;".
"begin call package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end;".
"begin call proc(:p_first,:p_second,:p_result);end;".
"begin call procedure_1(parameter_1);end;".
"begin call procedure_1(parameter_1)|:b.f[f(p.r:q)]|;end;".
"begin call procedure_1(parameter_1,parameter_2);end;".
"begin call procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end;".
"begin call procedure_1(parameter_1,parameter_2,parameter_3);end;".
"begin call procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end;".
"begin call schema_1.package_1.procedure_1(parameter_1);end;".
"begin call schema_1.package_1.procedure_1(parameter_1)|:b.f[f(p.r:q)]|;end;".
"begin call schema_1.package_1.procedure_1(parameter_1,parameter_2);end;".
"begin call schema_1.package_1.procedure_1(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end;".
"begin call schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3);end;".
"begin call schema_1.package_1.procedure_1(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end;".
"begin call schm.proc(:p_first,:p_second,:p_result);end;".
"begin call sum;end;".
"begin call sum(*);end;".
"begin call sum(*)|:b.f[f(p.r:q)]|;end;".
"begin call sum(all parameter_1);end;".
"begin call sum(all parameter_1)|:b.f[f(p.r:q)]|;end;".
"begin call sum(distinct parameter_1);end;".
"begin call sum(distinct parameter_1)|:b.f[f(p.r:q)]|;end;".
"begin call sum(parameter_1);end;".
"begin call sum(parameter_1)|:b.f[f(p.r:q)]|;end;".
"begin call sum(parameter_1,parameter_2);end;".
"begin call sum(parameter_1,parameter_2)|:b.f[f(p.r:q)]|;end;".
"begin call sum(parameter_1,parameter_2,parameter_3);end;".
"begin call sum(parameter_1,parameter_2,parameter_3)|:b.f[f(p.r:q)]|;end;".
"begin call sum|:b.f[f(p.r:q)]|;end;".

"begin update a set d = g where e=f; insert into a values(1,2); end;".

"begin abs();end".
"begin abs();max();end".
"begin abs();max;end".
"begin abs();pack_1.proc_2();end".
"begin abs();proc_2();end".
"begin abs();schema_0.pack_1.proc_2();end".
"begin abs;end".
"begin abs;max();end".
"begin abs;max;end".
"begin pack_1.proc_1();abs();end".
"begin pack_1.proc_1();end".
"begin pack_1.proc_1();pack_1.proc_2();end".
"begin pack_1.proc_1();proc_2();end".
"begin proc_1();abs();end".
"begin proc_1();end".
"begin proc_1();pack_1.proc_2();end".
"begin proc_1();proc_2();end".
"begin proc_1();schema_0.pack_1.proc_2();end".
"begin schema_0.pack_1.proc_1();abs();end".
"begin schema_0.pack_1.proc_1();end".
"begin schema_0.pack_1.proc_1();proc_2();end".
"begin schema_0.pack_1.proc_1();schema_0.pack_1.proc_2();end".

"begin call abs;end;".
"begin call abs();end;".
"begin call pack_1.proc_1();end;".
"begin call proc_1();end;".
"begin call schema_0.pack_1.proc_1();end;".

"begin sum(par_1=>'test',par_2=>3.14); end;".
"begin sum(par_1=>'test',par_2=>5); end;".
"begin sum(par_1=>:val_1,par_2=>:val_2); end;".
"begin sum(par_1=>val_1); end;".
"begin sum(par_1=>val_1,par_2=>val_2); end;".

"begin call sum(par_1=>'test',par_2=>3.14);end;".
"begin call sum(par_1=>'test',par_2=>5);end;".
"begin call sum(par_1=>:val_1,par_2=>:val_2);end;".
"begin call sum(par_1=>val_1);end;".
"begin call sum(par_1=>val_1,par_2=>val_2);end;".

%% -----------------------------------------------------------------------------
%% TESTS: BEGIN & CALL
%% =============================================================================
