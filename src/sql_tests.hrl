-define (TEST_SQLS0, [
"
select  /*+ 010 */
	to_date(a)
from 
	abc 
where 
		a=b
"
,
"
select  /*+ 011 */ 
	*
from 
	abc
where 
		a=b 
	and	c=d
"
]).


-define (TEST_SQLS, [
"
select  /*+ 000 */ 
	*
from 
	abc
where 
		a=b 
	and	c=d
"
,
"
select /*+ 001 */
	field1 as a 
from
	abc
where
	not	( 
			to_date(c) = d
			or
			e=f
		)
"	
,
"select /*+ 002 */ * from abc where a = 10 and b = 10.5 and c = -10 and e = -10.5 and -10.6 = g"
,
"
select /*+ 003 */
	a
	,b as bb
	,c
from
	abc
where
        	
        		a=b 
        	and	
        		not	c=d 
	or	e=f
	or	g=h
"
,
"
select 
	/*+ 004 */ 
	a
	,b
	,c as cc
from 
	abc
	, def
where
		a
		=
		b
	or
		c
		=
		d
"
,
"
select  /*+ 005 */
	a as aa
	,b
	,c
from 
	abc
	, def
where
		
			
			a
			in(
				a
				, b
				, c
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
select  /*+ 006 */
	*
from 
	abc
	, def
where
	(
		a
		=
		b
	or
		c
		=
		d
	)
	and
		e
		=
		f
"
,
"
select  /*+ 007 */
	distinct
	a
	,b
	,c
from 
	abc
	, def
where
		
			
			a
			in(
				select
					b
				from
					def
					,ghi
				where
					
						h
						=
						0
			)
		and	
			c
			=
			d 
		and	
			e
			in(
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
select  /*+ 008 */
	*
from 
	abc
where
		
			
			a
			in(
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
			g 
			between 
			h 
			and 
			i
	or	
		e
		=
		f
"
,
"
select  /*+ 009 */
	NVL(a) as a
from 
	abc
	, def
where
		
			
			c
			=
			d 
		and(	
				a
				in(
					select
						b
					from
						def
				)
			or	
				e
				=
				f
		)
	or	
		g 
		between 
		h 
		and 
		i
"
,
"
select  /*+ 010 */
	to_date(a) + to_date(b)
from 
	abc 
where 
		a=b
"
,
"
select  /*+ 011 */ 
	*
from 
	abc
where 
		a=b 
	and	c=d
"
,
"
select  /*+ 012 */ 
	*
from 
	abc
where	
		a=b 
	and	c=d 
	and	e=f
	and	g=h
"
,
"
select  /*+ 013 */ 
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
select  /*+ 014 */ 
	*
from
	abc
where
		a=b 
	and	
		not	c=d 
	and	e=f
	and	g=h
"  
, 
"
select  /*+ 015 */ 
	*
from 
	abc
where
		a=b
	and	c=d 
	and	e=f
	and	
		not	g=h
"
,
"
select  /*+ 016 */ 
	*
from 
	abc
where
		
			a=b 
		and	c=d 
		and	e=f
	or	g=h
"
,
"
select  /*+ 017 */
	*
from
	abc
where
			a=b 
		and	c=d 
	or	e=f
	or	g=h
"  
,
"
select  /*+ 018 */
	*
from
	abc
where
		
		not	a=b 
		and	c=d 
	or	e=f
	or	g=h
"
,
"
select  /*+ 019 */
	*
from
	abc
where
        	
        		a=b 
        	and	
        		not	c=d 
	or	e=f
	or	g=h
"
,
"
select  /*+ 020 */
	*
from
	abc
where
		
		not	a=b 
		and	
			not	c=d 
	or	e=f
	or	g=h
"  
,
"
select  /*+ 021 */
	*
from
	abc
where
		
			a=b 
		and	c=d 
	or
		not	e=f
	or
		not	g=h
"  
,
"
select  /*+ 022 */
	*
from
	abc
where
		a=b 
	or	c=d 
	or
		not	e=f
	or	g=h
"  
,
"
select  /*+ 023 */
	*
from
	abc
where
	not	(
				a=b 
			and	c=d
		)
	or	e=f
	or	g=h
"  
,
"
select  /*+ 024 */
	*
from
	abc
where
		
		not a=b 
		and c=d
	or	e=f
	or	g=h
"  
,
"
select  /*+ 025 */
	*
from
	abc
where
	
			(
					a=b 
				or	c=d
        	  	)
		and	e=f
	or	g=h
"  
,
"
select  /*+ 026 */
	*
from
	abc
where
		(	
				a=b 
			or	c=d
		) 
	and	e=f
	and	g=h
"
,
"
select  /*+ 027 */
	*
from
	abc
where
		a=b 
	or 
			c=d 
		and
			not	e=f
	or	g=h
"  
,
"
select  /*+ 028 */
	*
from
	abc
where
		a=b 
	or
			c=d 
		and	e=f 
		and	g=h
"
,
"
select  /*+ 029 */
	*
from
	abc
where
		a between b and c  
	and	d between e and f 
	and	g=h
"
,
"
select  /*+ 030 */
	*
from
	abc
where
		a between b and c 
	or 
			d between e and f 
          	and	g=h
"
,
"
select  /*+ 031 */
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
select  /*+ 032 */
	*
from	abc
where
	
			a between b and c
		and	d between e and f 
	or g=h
"
,
"
select  /*+ 033 */
	*
from
	abc
where
		(
				a=b 
			or	c=d
		) 
	and 	(
				e=f 
			or	g=h
		)
"
,
"
select  /*+ 034 */
	*
from
	abc
where
		a=b 
	or 
			c=d 
		and 	(
					e=f 
				or	g=h
			)
"
,
"
select  /*+ 035 */
	/*+ index(t1 t1_abc) */
	*
from
	abc
where
		a=b
"
,
%	, decode(BD_MSGTYPE||BD_EVENTDISP,01,'Y',012,'Y','N') ISDELIV
"
select  /*+ 036 */
	NULL as ROW_ID_S
	, BDETAIL6.ROWID as ROW_ID_M
	, BD_UMSGGRPID as MSGID
	, to_char(BD_DATESUBMIT,'DD.MM.YYYY HH24:MI:SS') as SUBMITTIME
	, to_char(BD_DATEEXPIRE,'DD.MM.YYYY HH24:MI:SS') as EXPIRETIME
	, to_char(BD_DATEDELIVERY,'DD.MM.YYYY HH24:MI:SS') as RECTIME
	, BD_MSISDN_A as SENDER
	, BD_MSISDN_B as RECEIVER
	, BD_MSGSIZE as MSGLEN
	, nvl(MMSCCRT_LANG01,BD_CDRRECTYPE) as TYPE
	, nvl(MMSCCRT_VALUE1,BD_CDRRECTYPE) as TYPE_TT1
	, nvl(MMSCCRT_VALUE2,BD_CDRRECTYPE) as TYPE_TT2
	, decode(BD_MSGTYPE,01,'Y',012,'Y','N') as ISDELIV
	, nvl(MMSCET_LANG02,BD_EVENTDISP) as EVENTDISP_STATCODE
	, nvl(MMSCMT_LANG02,BD_MSGTYPE) as MSGTYPE_ERRCODE
	, nvl(MMSCET_VALUE2,BD_EVENTDISP) as EVENTDISP_TT
	, nvl(MMSCMT_VALUE2,BD_MSGTYPE) as MSGTYPE_TT
	, 'MMS' as ROWTYPE
	, to_char(BD_DATETIME,'DD.MM.YYYY HH24:MI:SS') as DATETIME
from
	BDETAIL6
	, MMSC_CDRRECTYPE
	, MMSC_EVENTDISPTYPE
	, MMSC_MSGTYPE
where	
		BD_CDRRECTYPE=MMSCCRT_ID
	and	ltrim(to_char(BD_EVENTDISP))=MMSCET_ID
	and	ltrim(to_char(BD_MSGTYPE))=MMSCMT_ID
	and	BD_UMSGGRPID='mj78yk7r307fga5a01'
	and	BD_MSISDN_B='41796187332'
	and	BD_DATETIME>=to_date('19.06.12 11:15:09','DD.MM.YY HH24:MI:SS') - 14
	and	BD_DATETIME<=to_date('19.06.12 11:15:09','DD.MM.YY HH24:MI:SS') + 14
order by
	BD_DATETIME
	, nvl(BD_DATEDELIVERY,BD_DATETIME)
	, BD_MSGTYPE
"
%% ,
%% "
%% select  /*+ 037 */
%% 	AC_ID
%% 	, AC_NAME
%% 	, AC_ETID
%% 	, AC_SHORT
%% 	, AC_DEPTID
%% 	, AC_LANGID
%% 	, AC_LOGRET
%% 	, nvl(AC_MAXLOG, SYS_MAXLOG) as MAXLOG
%% 	, AC_LASTLOGINTIME
%% 	, AC_IPMASK
%% 	, AC_REMOTEADDR
%% 	, (sysdate-nvl(AC_LASTLOGINTIME,sysdate))*24*60-nvl(SYS_DELAY,3)
%% from 
%% 	ACCOUNT
%% 	, SYSPARAMETERS
%% where
%% 		AC_ESID='A'
%% 	and	AC_SHORT='ADMIN'
%% "
]).
