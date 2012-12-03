-define (TEST_SQLS,[
"
CREATE TABLE test
(
fld CHAR
, fld VARCHAR
, fld VARCHAR2(13) DEFAULT '123'
, fld TINYTEXT
, fld TEXT DEFAULT 'abc'
, fld BLOB(2000)
, fld MEDIUMTEXT
, fld MEDIUMBLOB
, fld LONGTEXT(1000) DEFAULT ''
, fld LONGBLOB(1000000000)
, fld TINYINT
, fld SMALLINT DEFAULT 0
, fld MEDIUMINT(10,-4)
, fld BIGINT DEFAULT 99999999999999999
, fld FLOAT(-3) DEFAULT 123456
, fld DOUBLE(3)
, fld DECIMAL(10,3) DEFAULT 1.1234
, fld DATE DEFAULT SYSDATE
, fld DATETIME
, fld TIMESTAMP
, fld TIME
, fld YEAR
, fld INT
, fld ENUM()
, fld SET()
)
"
,
"
CREATE LOCAL TABLE test
(
fld eTuple(0) default fun() -> {} end.
, fld eBinary(1000)
, fld eAtom
, fld eIpaddr(4) default fun() -> {0,0,0,0} end. 
, fld eList(0) default fun() -> [] end.
, fld eBinstr(1000) default fun() -> <<\"no_value\">> end.
, fld ePid
, fld eRef
, fld eFun(1)
, fld eDatetime default fun() -> calendar:local_time() end.
, fld eTimestamp(3) default fun() -> erlang:now() end.
, fld eInteger(10,-3)
)
"
%,


%% -- "
%% -- CREATE TABLE test
%% -- (
%% -- fld eTuple
%% -- , fld eBinary
%% -- , fld eAtom
%% -- , fld eIpaddr
%% -- , fld eList
%% -- , fld eBinstr
%% -- )
%% -- "
%% -- ,
%% -- 
%% -- "
%% -- select
%% -- 	/*+011*/
%% -- 			a
%% -- 			+
%% -- 			1
%% -- 		,
%% -- 			t1.b
%% -- 			+
%% -- 			schma.t1.c
%% -- 		,
%% -- 			upper(a.a)
%% -- 			-
%% -- 			schma.a.b as fh
%% -- 		,
%% --  			b.*
%% --  		,
%% -- 			schma.b.c
%% -- 	from
%% --  		t1, t2 a, schma.t3, schma.t4 b 
%% -- 	where
%% -- 			upper(scm.tbl.a)
%% -- 			=
%% -- 			upper(b)
%% -- 		and
%% -- 			c
%% -- 			=
%% -- 			d
%% -- "	
%% fields to be returned as  
%%		<<"*">> | <<"table.*">> | <<"schema.table.*">> | <<"name">> | <<"schema.name">> | {as,{expression},<<"alias">>}

%% tables to be returned as 
%%		<<"name">> | <<"schema.name">> | {<<"name">>, "alias"} | {<<"schema.name">>, "alias"}


% "CREATE USER test_user_1 IDENTIFIED BY a_password",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY",
% "CREATE USER test_user_2 IDENTIFIED EXTERNALLY AS test_usr_2_extern",
% "CREATE USER test_user_4 IDENTIFIED GLOBALLY",
% "CREATE USER test_user_4 IDENTIFIED GLOBALLY AS test_usr_2_extern",
% "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1",
% "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 DEFAULT TABLESPACE table_2",
% "CREATE USER test_user_1 IDENTIFIED EXTERNALLY AS test_usr_2_extern TEMPORARY TABLESPACE table_1",
% "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 TEMPORARY TABLESPACE table_2",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY PROFILE user_profile",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY PASSWORD EXPIRE",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT LOCK",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT UNLOCK",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA UNLIMITED ON table_1",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_2",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_3 QUOTA UNLIMITED ON table_1",
% "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10 ON table_3 QUOTA 10M ON table_4 QUOTA UNLIMITED ON table_1",

% "ALTER USER test_user_123 IDENTIFIED BY new_password",
% "ALTER USER test_user_123 ACCOUNT LOCK",
% "ALTER USER test_user_123 ACCOUNT UNLOCK",
% "ALTER USER test_user_123 PASSWORD EXPIRE",

% "DROP USER test_user_123",
% "DROP USER test_user_123 CASCADE",

% "INSERT INTO Persons VALUES (4,'Nilsen', 'Johan', 'Bakken 2', 'Stavanger')",

% - "
% - CREATE TABLE Persons
% - (
% - P_Id int,
% - LastName varchar,
% - LastName varchar(255),
% - FirstName varchar(255),
% - Address varchar(255),
% - City varchar(255)
% - )
% - "

% ,
% "DROP TABLE table_name"
% ,
%% - "
%% - select
%% - 	/*+038*/
%% - 		AC_ID
%% - 		,
%% - 		AC_NAME
%% - 		,
%% - 		AC_ETID
%% - 		,
%% - 		AC_SHORT
%% - 		,
%% - 		AC_DEPTID
%% - 		,
%% - 		AC_LANGID
%% - 		,
%% - 		AC_LOGRET
%% - 		,
%% - 			nvl
%% - 				(
%% - 					AC_MAXLOG
%% - 					,
%% - 					SYS_MAXLOG
%% - 				) as MAXLOG
%% - 		,
%% - 		AC_LASTLOGINTIME
%% - 		,
%% - 		AC_IPMASK
%% - 		,
%% - 		AC_REMOTEADDR
%% - 		,
%% - 					(
%% - 						sysdate
%% - 						-
%% - 							nvl
%% - 								(
%% - 									AC_LASTLOGINTIME
%% - 									,
%% - 									sysdate
%% - 								)
%% - 					)
%% - 				*
%% - 				24
%% - 				*
%% - 				60
%% - 			-
%% - 				nvl
%% - 					(
%% - 						SYS_DELAY
%% - 						,
%% - 						3
%% - 					)
%% - 	from
%% - 		ACCOUNT
%% - 		,
%% - 		SYSPARAMETERS
%% - 	where
%% - 			AC_ESID
%% - 			=
%% - 			'A'
%% - 		and
%% - 			AC_SHORT
%% - 			=
%% - 			'ADMIN'
%% - 		and
%% - 				1
%% - 				+
%% - 				-4
%% - 			=
%% - 			-3
%% - 		and
%% - 			2
%% - 			=
%% - 				5
%% - 				-
%% - 				3
%% - 		and
%% - 			2.3
%% - 			=
%% - 				5.9
%% - 				-
%% - 				3.6
%% - 		and
%% - 				a
%% - 				-
%% - 				10.5
%% - 			=
%% - 			-
%% - 			c
%% - 		and
%% - 			-10.5
%% - 			=
%% - 				a
%% - 				-
%% - 				12.9
%% - "
]).


-define (TEST_SQLS0,[
"
select
	/*+000*/
		*
	from
		abc
"
,
"select
	/*+001*/
		a
	from
		abc
	where
			a
			=
			10
		and
			b
			=
			10.5
		and
			c
			=
			-10
		and
			e
			=
			-10.5
		and
			-10.6
			=
			g
"
,
"
select
	/*+002*/
		a
		,
		b
	from
		abc
	where
				a
				=
				b
			and
				not
					c
					=
					d
		or
			e
			=
			f
		or
			g
			=
			h
"
,
"
select
	/*+003*/
		a
		,
		b as bb
		,
		c
	from
		abc
		,
		def
	where
			a
			=
			b
		or
				nvl
					(
						a
						,
						0
					)
			=
			0
"
,
"
select
	/*+004*/
		-
		c
	from
		abc
		,
		def
	where
				a
				in
					(
						a
						,
						b
						,
						c
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
	/*+005*/
			a
			+
			b
	from
		abc
	where
		not
			(
						to_date
							(
								c
							)
					=
					d
				or
					e
					=
					f
			)
"
,
"
select
	/*+006*/
		-
		c as cc
	from
		abc
		,
		def
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
select
	/*+007*/
		*
	from
		abc
	where
				a
				=
				b
			and
				c
				=
				d
		or
			e
			=
			f
		or
			g
			=
			h
" 
,
"
select
	/*+008*/
	distinct
		a
		,
		-
			nvl
				(
					b
				)
		,
		c
	from
		abc
		,
		def
	where
				a
				in
					(
						select
								b
							from
								def
								,
								ghi
							where
								h
								=
								0
					)
			and
					(
						select
								b
							from
								def
					)
				=
				10
			and
				e
				in
					(
						1
						,
						2
						,
						3
					)
		or
			g
			=
			h
"
,
"
select
	/*+010*/
				to_date
					(
						a
					)
			+
				to_date
					(
						b
					)
	from
		abc
	where
		a
		=
		b
"
,
"
select
	/*+011*/
			a
			+
			1
		,
			b
			*
			c
		,
			f
			-
			h as fh
	from
		abc
	where
			a
			=
			b
		and
			c
			=
			d
"
,
"
select
	/*+012*/
		*
	from
		abc
	where
			a
			=
			b
		and
			c
			=
			d
		and
			e
			=
			f
		and
			g
			=
			h
"
,
"
select
	/*+013*/
		*
	from
		abc
	where
			not
				a
				=
				b
		and
			c
			=
			d
		and
			e
			=
			f
		and
			g
			=
			h
"
,
"
select
	/*+014*/
		*
	from
		abc
	where
			a
			=
			b
		and
			not
				c
				=
				d
		and
			e
			=
			f
		and
			g
			=
			h
" 
,
"
select
	/*+015*/
		*
	from
		abc
	where
			a
			=
			b
		and
			c
			=
			d
		and
			e
			=
			f
		and
			not
				g
				=
				h
"
,
"
select
	/*+009*/
			NVL
				(
					a
				) as a
	from
		abc
		,
		def
	where
				c
				=
				d
			and
				(
						a
						in
							(
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
select
	/*+016*/
		*
	from
		abc
	where
				a
				=
				b
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
			=
			h
"
,
"
select
	/*+017*/
		a
		,
			(
				select
						c
					from
						def
			)
	from
			(
				select
						d
					from
						ghi
			)
	where
			(
				select
						b
					from
						def
			)
		=
		b
"
,
"
select
	/*+018*/
		*
	from
		abc
	where
				not
					a
					=
					b
			and
				c
				=
				d
		or
			e
			=
			f
		or
			g
			=
			h
"
,
"
select
	/*+019*/
		*
	from
		abc
	where
				a
				=
				b
			and
				not
					c
					=
					d
		or
			e
			=
			f
		or
			g
			=
			h
"
,
"
select
	/*+020*/
		*
	from
		abc
	where
				not
					a
					=
					b
			and
				not
					c
					=
					d
		or
			e
			=
			f
		or
			g
			=
			h
" 
,
"
select
	/*+021*/
		*
	from
		abc
	where
				a
				=
				b
			and
				c
				=
				d
		or
			not
				e
				=
				f
		or
			not
				g
				=
				h
" 
,
"
select
	/*+022*/
		*
	from
		abc
	where
			a
			=
			b
		or
			c
			=
			d
		or
			not
				e
				=
				f
		or
			g
			=
			h
" 
,
"
select
	/*+023*/
		*
	from
		abc
	where
			not
				(
						a
						=
						b
					and
						c
						=
						d
				)
		or
			e
			=
			f
		or
			g
			=
			h
" 
,
"
select
	/*+024*/
		*
	from
		abc
	where
				not
					a
					=
					b
			and
				c
				=
				d
		or
			e
			=
			f
		or
			g
			=
			h
" 
,
"
select
	/*+025*/
		*
	from
		abc
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
		or
			g
			=
			h
" 
,
"
select
	/*+026*/
		*
	from
		abc
	where
			(
					a
					<
					b
				or
					c
					<=
					d
			)
		and
			e
			<>
			f
		and
			g
			>
			h
"
,
"
select
	/*+027*/
		*
	from
		abc
	where
			a
			=
			b
		or
				c
				=
				d
			and
				not
					e
					=
					f
		or
			g
			=
			h
" 
,
"
select
	/*+028*/
		*
	from
		abc
	where
			a
			=
			b
		or
				c
				=
				d
			and
				e
				=
				f
			and
				g
				=
				h
"
,
"
select
	/*+029*/
		*
	from
		abc
	where
			a
			between
			b
			and
			c
		and
			d
			between
				(
					select
							e
						from
							fhi
				)
			and
			f
		and
			g
			=
			h
"
,
"
select
	/*+030*/
		*
	from
		abc
	where
				(
					select
							a
						from
							klm
				)
			between
			b
			and
			c
		or
				d
				between
				e
				and
				f
			and
				g
				=
				h
"
,
"
select
	/*+031*/
		*
	from
		abc
	where
			not
				a
				between
				b
				and
				c
		and
			d
			between
			e
			and
			f
		and
			g
			=
			h
"
,
"
select
	/*+032*/
		*
	from
		abc
	where
				a
				between
				b
				and
				c
			and
				d
				between
				e
				and
				f
		or
			g
			=
			h
"
,
"
select
	/*+033*/
	distinct
		a
		,
		b
	from
		abc
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
			(
					e
					=
					f
				or
					g
					=
					h
			)
	order by
		c
		,
		d desc
		,
		e
"
,
"
select
	/*+034*/
		*
	from
		abc
	where
			a
			=
			b
		or
				c
				=
				d
			and
				(
						e
						=
						f
					or
						g
						=
						h
				)
"
,
"
select
	/*+035*/
		*
	from
		abc
	where
		a
		=
		b
"
,
%	,decode(BD_MSGTYPE||BD_EVENTDISP,01,'Y',012,'Y','N') ISDELIV
"
select
	/*+036*/
		NULL as ROW_ID_S
		,
		BDETAIL6.ROWID as ROW_ID_M
		,
		BD_UMSGGRPID as MSGID
		,
			to_char
				(
					BD_DATESUBMIT
					,
					'DD.MM.YYYY HH24:MI:SS'
				) as SUBMITTIME
		,
			to_char
				(
					BD_DATEEXPIRE
					,
					'DD.MM.YYYY HH24:MI:SS'
				) as EXPIRETIME
		,
			to_char
				(
					BD_DATEDELIVERY
					,
					'DD.MM.YYYY HH24:MI:SS'
				) as RECTIME
		,
		BD_MSISDN_A as SENDER
		,
		BD_MSISDN_B as RECEIVER
		,
		BD_MSGSIZE as MSGLEN
		,
			nvl
				(
					MMSCCRT_LANG01
					,
					BD_CDRRECTYPE
				) as TYPE
		,
			nvl
				(
					MMSCCRT_VALUE1
					,
					BD_CDRRECTYPE
				) as TYPE_TT1
		,
			nvl
				(
					MMSCCRT_VALUE2
					,
					BD_CDRRECTYPE
				) as TYPE_TT2
		,
			decode
				(
					BD_MSGTYPE
					,
					01
					,
					'Y'
					,
					012
					,
					'Y'
					,
					'N'
				) as ISDELIV
		,
			nvl
				(
					MMSCET_LANG02
					,
					BD_EVENTDISP
				) as EVENTDISP_STATCODE
		,
			nvl
				(
					MMSCMT_LANG02
					,
					BD_MSGTYPE
				) as MSGTYPE_ERRCODE
		,
			nvl
				(
					MMSCET_VALUE2
					,
					BD_EVENTDISP
				) as EVENTDISP_TT
		,
			nvl
				(
					MMSCMT_VALUE2
					,
					BD_MSGTYPE
				) as MSGTYPE_TT
		,
		'MMS' as ROWTYPE
		,
			to_char
				(
					BD_DATETIME
					,
					'DD.MM.YYYY HH24:MI:SS'
				) as DATETIME
	from
		BDETAIL6
		,
		MMSC_CDRRECTYPE
		,
		MMSC_EVENTDISPTYPE
		,
		MMSC_MSGTYPE
	where
			BD_CDRRECTYPE
			=
			MMSCCRT_ID
		and
				ltrim
					(
							to_char
								(
									BD_EVENTDISP
								)
					)
			=
			MMSCET_ID
		and
				ltrim
					(
							to_char
								(
									BD_MSGTYPE
								)
					)
			=
			MMSCMT_ID
		and
			BD_UMSGGRPID
			=
			'mj78yk7r307fga5a01'
		and
			BD_MSISDN_B
			=
			'41796187332'
		and
			BD_DATETIME
			>=
					to_date
						(
							'19.06.12 11:15:09'
							,
							'DD.MM.YY HH24:MI:SS'
						)
				-
				14
		and
			BD_DATETIME
			<=
					to_date
						(
							'19.06.12 11:15:09'
							,
							'DD.MM.YY HH24:MI:SS'
						)
				+
				14
	order by
		BD_DATETIME
		,
			nvl
				(
					BD_DATEDELIVERY
					,
					BD_DATETIME
				)
		,
		BD_MSGTYPE
"
,
"
select
	/*+037*/
		AC_ID
		,
			15
			*
			3
		,
			sysdate
			-
				nvl
					(
						AC_LASTLOGINTIME
						,
						sysdate
					)
	from
		ACCOUNT
		,
		SYSPARAMETERS
	where
			AC_ESID
			=
			'A'
		and
			AC_SHORT
			=
			'ADMIN'
		and
				1
				+
				-4
			=
			-3
		and
			2
			=
				5
				-
				3
"
,
"
select
	/*+038*/
		AC_ID
		,
		AC_NAME
		,
		AC_ETID
		,
		AC_SHORT
		,
		AC_DEPTID
		,
		AC_LANGID
		,
		AC_LOGRET
		,
			nvl
				(
					AC_MAXLOG
					,
					SYS_MAXLOG
				) as MAXLOG
		,
		AC_LASTLOGINTIME
		,
		AC_IPMASK
		,
		AC_REMOTEADDR
		,
					(
						sysdate
						-
							nvl
								(
									AC_LASTLOGINTIME
									,
									sysdate
								)
					)
				*
				24
				*
				60
			-
				nvl
					(
						SYS_DELAY
						,
						3
					)
	from
		ACCOUNT
		,
		SYSPARAMETERS
	where
			AC_ESID
			=
			'A'
		and
			AC_SHORT
			=
			'ADMIN'
		and
				1
				+
				-4
			=
			-3
		and
			2
			=
				5
				-
				3
		and
			2.3
			=
				5.9
				-
				3.6
		and
				a
				-
				10.5
			=
			-
			c
		and
			-10.5
			=
				a
				-
				12.9
"
]).
