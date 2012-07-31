-module(sql_parse_tests).
-compile(export_all).

%
% TEST CASES %
%


-include_lib("eunit/include/eunit.hrl").

-define (TEST_SQLS, [
"
select 
	a
	,b
	,c
	from 
		abc
		, def
	where
		eva	
			eva	
				a
				in(
				select
					b
				from
					def
					,ghi
				where
					eva
						h
						=
						0
				)
			and	
				upper
				(
					c
				)
				=
				'D' 
			and	
				e
				in
				(
					1
					,2
					,3
				)
		or	
			g
			=
			h
"
,
"
select 
	a
	,b
	,c
from 
	abc
	, def
where
	eva	
		eva	
			a
			in	(
			select
				b
			from
				def
			)
		and	
			c
			=
			d 
		and	
			e
			=
			f
	or	
		g 
		between 
		h 
		and 
		i
"
,
"
select 
	a
	,b
	,c
from 
	abc
	, def
where
	eva	
		eva	a
				in	(
					a
					, b
					, c
					)
		and	c=d 
		and	e=f
	or	g between h and i
"
]).

%% +select a, b, c from abc, def where a in (select b from def) and c=d and e=f or g=h
%% 
%% -select 
%% 	a
%% 	, b
%% 	, c
%% +from abc, def
%% +where a in (select b from def) and c=d and e=f or g=h
%% 
%% -select 
%% 	a
%% 	, b
%% 	, c
%% -from 
%% 	abc
%% 	, def
%% -where
%% 	+	a in (select b from def) and c=d  and e=f
%% 	+or	g=h
%% 
%% -select 
%% 	a
%% 	, b
%% 	, c
%% -from 
%% 	abc
%% 	, def
%% -where
%% 		+	a in (select b from def)
%% 		+and 	c=d 
%% 		+and 	e=f
%% 	-or	
%% 		g
%% 		=
%% 		h
%% 
%% -select 
%% 	a
%% 	, b
%% 	, c
%% +from abc, def
%% -where
%% 		-	a
%% 			+	in (select b from def)
%% 		-and	
%% 			c
%% 			=
%% 			d 
%% 		-and	
%% 			e
%% 			=
%% 			f
%% 	-or	
%% 		g
%% 		=
%% 		h
%% 	
%% +select a, b, c
%% +from  abc, def
%% -where
%% 		 -	a
%% 			-	in	(
%% 					select
%% 						b
%% 					from
%% 						def
%% 					)
%% 		-and	
%% 			c
%% 			=
%% 			d 
%% 		+and	e=f
%% 	-or	
%% 		g
%% 		=
%% 		h
%%
%% +select a, b, c
%% +from  abc, def
%% -where
%% 	+	a in (select b from def) and c=d  and e=f
%% 	+or	g between h and i
%%
%%
%% +select a, b, c
%% +from  abc, def
%% -where
%% 	+	a in (select b from def) and c=d  and e=f
%% 	-or	
%% 		g
%% 		between	
%%		h 
%%		and 
%%		i
%%
%% +select a, b, c
%% +from  abc, def
%% -where
%% 	+	a in (select b from def) and c=d  and e=f
%% 	-or	
%% 		g
%% 		between
%% 		-	h	
%%			and
%%			i
	

-define (TEST_SQLS1, [
"
select 
	*
from 
	abc 
where 
	eva	a=b
"
,
"
select 
	*
from 
	abc
where 
	eva	a=b 
	and	c=d
"
,
"
select 
	*
from 
	abc
where	
	eva	a=b 
	and	c=d 
	and	e=f
	and	g=h
"
,
"
select 
	*
from 
	abc
where
	not	a=b 
	and	c=d 
	and	e=f
	and	g=h
"  
,
"
select 
	*
from
	abc
where
	eva	a=b 
	and	
		not	c=d 
	and	e=f
	and	g=h
"  
, 
"
select 
	*
from 
	abc
where
	eva	a=b
	and	c=d 
	and	e=f
	and	
		not	g=h
"
,
"
select 
	*
from 
	abc
where
	eva	
		eva	a=b 
		and	c=d 
		and	e=f
	or	g=h
"
,
"
select
	*
from
	abc
where
	eva	eva	a=b 
		and	c=d 
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	
		not	a=b 
		and	c=d 
	or	e=f
	or	g=h
"
,
"
select
	*
from
	abc
where
        eva	
        	eva	a=b 
        	and	
        		not	c=d 
	or	e=f
	or	g=h
"
,
"
select
	*
from
	abc
where
	eva	
		not	a=b 
		and	
			not	c=d 
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	
		eva	a=b 
		and	c=d 
	or
		not	e=f
	or
		not	g=h
"  
,
"
select
	*
from
	abc
where
	eva	a=b 
	or	c=d 
	or
		not	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	not	(
			eva	a=b 
			and	c=d
		)
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	
		not a=b 
		and c=d
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva
		eva	(
				eva	a=b 
				or	c=d
        	  	)
		and	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	(	
			eva	a=b 
			or	c=d
		) 
	and	e=f
	and	g=h
"
,
"
select
	*
from
	abc
where
	eva	a=b 
	or 
		eva	c=d 
		and
			not	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	a=b 
	or
		eva	c=d 
		and	e=f 
		and	g=h
"
,
"
select
	*
from
	abc
where
	eva	a between b and c  
	and	d between e and f 
	and	g=h
"
,
"
select
	*
from
	abc
where
	eva	a between b and c 
	or 
		eva	d between e and f 
          	and	g=h
"
,
"
select
	*
from
	abc
where
	not	a between b and c 
	and	d between e and f 
	and	g=h
"
,
"
select
	*
from	abc
where
	eva
		eva	a between b and c
		and	d between e and f 
	or g=h
"
,
"
select
	*
from
	abc
where
	eva	(
			eva	a=b 
			or	c=d
		) 
	and 	(
			eva	e=f 
			or	g=h
		)
"
,
"
select
	*
from
	abc
where
	eva	a=b 
	or 
		eva	c=d 
		and 	(
				eva	e=f 
				or	g=h
			)
"
,
"
select
	/*+ index(t1 t1_abc) */
	*
from
	abc
where
	eva	a=b
"
,
"
select
	/*+ index(BDETAIL6 IDX_BD_UMSGGRPID) */
	NULL ROW_ID_S
	, BDETAIL6.ROWID ROW_ID_M
	, BD_UMSGGRPID MSGID
	, to_char(BD_DATESUBMIT,'DD.MM.YYYY HH24:MI:SS') SUBMITTIME
	, to_char(BD_DATEEXPIRE,'DD.MM.YYYY HH24:MI:SS') EXPIRETIME
	, to_char(BD_DATEDELIVERY,'DD.MM.YYYY HH24:MI:SS') RECTIME
	, BD_MSISDN_A SENDER
	, BD_MSISDN_B RECEIVER
	, BD_MSGSIZE MSGLEN
	, nvl(MMSCCRT_LANG01,BD_CDRRECTYPE) TYPE
	, nvl(MMSCCRT_VALUE1,BD_CDRRECTYPE) TYPE_TT1
	, nvl(MMSCCRT_VALUE2,BD_CDRRECTYPE) TYPE_TT2
	, decode(BD_MSGTYPE||BD_EVENTDISP,01,'Y',012,'Y','N') ISDELIV
	, nvl(MMSCET_LANG02,BD_EVENTDISP) EVENTDISP_STATCODE
	, nvl(MMSCMT_LANG02,BD_MSGTYPE) MSGTYPE_ERRCODE
	, nvl(MMSCET_VALUE2,BD_EVENTDISP) EVENTDISP_TT
	, nvl(MMSCMT_VALUE2,BD_MSGTYPE) MSGTYPE_TT
	, 'MMS' ROWTYPE
	, to_char(BD_DATETIME,'DD.MM.YYYY HH24:MI:SS') DATETIME
from
	BDETAIL6
	, MMSC_CDRRECTYPE
	, MMSC_EVENTDISPTYPE
	, MMSC_MSGTYPE
where	
	eva	BD_CDRRECTYPE=MMSCCRT_ID(+) 
	and	ltrim(to_char(BD_EVENTDISP))=MMSCET_ID(+)
	and	ltrim(to_char(BD_MSGTYPE))=MMSCMT_ID(+)
	and	BD_UMSGGRPID='mj78yk7r307fga5a01'
	and	BD_MSISDN_B='41796187332'
	and	BD_DATETIME>=to_date('19.06.12 11:15:09','DD.MM.YY HH24:MI:SS')-14
	and	BD_DATETIME<=to_date('19.06.12 11:15:09','DD.MM.YY HH24:MI:SS')+14
order by
	BD_DATETIME
	, nvl(BD_DATEDELIVERY,BD_DATETIME)
	, BD_MSGTYPE
"
,
"
select
	/*+ INDEX(ACCOUNT IDXU_AC_SHORT)*/
	AC_ID
	, AC_NAME
	, AC_ETID
	, AC_SHORT
	, AC_DEPTID
	, AC_LANGID
	, AC_LOGRET
	, nvl(AC_MAXLOG, SYS_MAXLOG) MAXLOG
	, AC_LASTLOGINTIME
	, AC_IPMASK
	, AC_REMOTEADDR
	, (sysdate-nvl(AC_LASTLOGINTIME,sysdate))*24*60-nvl(SYS_DELAY,3) TIME_DIFF
from 
	ACCOUNT
	, SYSPARAMETERS
where
	eva	AC_ESID='A'
	and	AC_SHORT='ADMIN'
"
]).

remove_eva(S) ->
	re:replace(S, "([ \t]eva[ \t])", "\t\t", [global, {return, list}]).

parse_test() -> test_parse(?TEST_SQLS).		
test_parse([]) -> ok;
test_parse([S|Sqls]) ->
    Sql = remove_eva(S) ++ ";",
    io:format(user, "===============================~nSql: "++Sql++"~n...............................~nParseTree:~n", []),
    {ok, Tokens, _} = sql_lex:string(Sql),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} -> io:format(user, "~p~n", [ParseTree]);
        Error -> io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens])
    end,
    test_parse(Sqls).

% TODO: render_test() must parse and render and return the original string ?TEST_SQLS
