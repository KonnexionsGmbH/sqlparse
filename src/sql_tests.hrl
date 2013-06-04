-define (TEST_SELECT,[
<<"select * from d">>,
<<"select rowid, * from d">>,
<<"select *, rowid from d">>,
<<"select a, rowid, * from d">>,
<<"select a, *, rowid from d">>,
<<"select *, a, rowid from d">>,
<<"select a || 'test', rowid, * from d">>,
<<"select rowid, *, col1 * col from d">>,
<<"select a,b from d where a = \"üטיצהא\"">>,
<<"select
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
">>
 ,
 <<"
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
">>
 ,
<<"select
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
">>
 ,
<<"SELECT
 		a
 		,
 			(
 				SELECT
 						c
 					FROM
 						d
 			)
 	FROM
 		b
">>,
 "select
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
 "select
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
 ",
 "select
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
 			h038
 ",
 "select
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
 				) as DATETIM
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
 "select
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
 ",
 "select
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
"SELECT f, MAX(d) FROM def where a=b GROUP BY f HAVING SUM(d) < 2 order by f",
"select 'aa' || 'b\nb' || 'cc' || field from def",
"select \"aa\" || \"b\nb\" || \"c\r\nc\" || field from def",
"select 'a' || 'b' || 'c' || field from def where field2 = 'a' || fld3 || 's' and 's' || fld2 = 'd' || fld4",
"select
 	distInct
 		id
 	from
 			(
 				SELECT
 						id
 						,
 						2 as ordered
 					FROM
 						a
 			UNION
 				SELECT
 						id
 						,
 						1 as ordered
 					FROM
 						b
 			)
 	order by
 		ordered
 ",
 "SELECT
 		/*+001*/
 		DISTINCT
 			a
 		FROM
 			b
 UNION
 	SELECT
 		/*+002*/
 			c
 		FROM
 			d
 INTERSECT
 	SELECT
 			e
 		FROM
 			f
 MINUS
 	SELECT
 			g
 		FROM
 			h
 UNION ALL
 	SELECT
 			i
 		FROM
 			j
",	
"select
 	distInct
 		id
 	from
 			(
 				SELECT
 						id
 						,
 						2 as ordered
 					FROM
 						a
 			UNION
 				SELECT
 						id
 						,
 						1 as ordered
 					FROM
 						b
 			)
 	order by
 		ordered
 ",
"SELECT
	/*+011*/
		a
		,
		b
	FROM
		c
		,
		d
	WHERE
		e
		=
		f
",
"select
		name
		,
		lastLoginTime
	from
		ddAccount
	where
		lastLoginTime
		>
			sysdate
			-
			1.1574074074074073e-4
",
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
			f
			=
			-11.5
",
"select
			3
			+
			a
	from
		def
	where
			a
			in
				(
					5
					,
						7
						+
						1
					,
					3
				)
		and
			b
			in
				(
					\"bikram\"
					,
					\"stefan\"
				)
	order by
		a asc
",
"select
	/*+003*/
		*
	from
		abc
		,
		def as d
	where
			a
			=
				b
				+
				1
		or
				nvl
					(
						a
						,
							b
							+
							1
						,
						c
					)
			=
			0
		or
			c
			in
				(
					d
					,
					e
					,
							upper
								(
									k
								)
						+
							upper
								(
									j
								)
				)
",
"select
	/*+003*/
		a
		,
		b as bb
		,
			c
			+
			1 as cc
		,
				(
					d
					-
					1
				)
			/
			4
		,
		5
		,
			upper
				(
					b
				)
	from
		abc
		,
		def as d
",
"select * from def where (((a = a1 or b = b1)) and (((w=(3)))))",
"select * from def where a = abs(abs(1-2+(3)))",
"select * from def where t1.col1 in (5,7) and abs((sin(t2.col1) - trunc((t2.col1/t1.col1)))) = 1 and t2.col1 > ( t1.col1 div 2 )",
"select * from def where t1.col1 in (5,7) and abs(sin(t2.col1) - trunc(t2.col1/t1.col1)) = 1 and t2.col1 > t1.col1 div 2",
"select
		d.col1
		,
		m.col1
		,
		m.\"'$_'\"
	from
		def as d
		,
		member_test as m
",
"select
		d.col1
		,
		m.col1
		,
		\"'$_'\".m
	from
		def as d
		,
		member_test as m
",
"select
		d.col1
		,
		m.col1
	from
		def as d
		,
		member_test as m
	where
		is_member
			(
				d.col1
				,
				m.\"'$_'\"
			)
",
"select
		*
	from
		member_test
	where
		is_member
			(
				3
				,
				col2
			)
",
"sElect
		*
	frOm
		tab@1234@_no#de:@nohost
",
"select
	/*+011*/
			a
			+
			1
		,
			t1.b
			+
			schma.t1.c
		,
				upper
					(
						a.a
					)
			-
			schma.a.b as fh
		,
		b.*
		,
		schma.b.c
	from
		t1
		,
		t3 as a
		,
		schma.t1
		,
		schma.t2 as b
	where
				upper
					(
						scm.tbl.a
					)
			=
				upper
					(
						b
					)
		and
			c
			=
			d
",
"select
		12
		,
		\"12\"
		,
		'12'
		,
		'field_a'
		,
			erl
				(
					\"['a',b]\"
				)
		,
			erl
				(
					\"{field_a,b}\"
				)
	from
		'table_1'
",
"select
	/*+000*/
		*
	from
		abc
",
"select
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
						+
						1
		or
			e
			=
			f
		or
			g
			in
				(
					h
					,
						i
						+
						1
					,
					k
				)
"
,
"select
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

"select
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
"select
	/*+006*/
		- c as cc
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
"select
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
"select
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
"select
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
"select
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
"select
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
"select
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
"select
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
"select
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
		e "
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
				)",
"select
	/*+035*/
		*
	from
		abc
	where
		a
		=
		b",
%	,decode(BD_MSGTYPE||BD_EVENTDISP,01,'Y',012,'Y','N') ISDELIV
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
		ACC
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
		ACC
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
				- c
		and
			-10.5
			=
				a
				-
				12.9
",
"select
		t1.col1
		,
		t2.col1
	from
		def as t1
		,
		def as t2
	where
				abs
					(
							t2.col1
							-
							t1.col1
						,
						t3.col4
					)
			=
			1
		and
				abs
					(
							t2.col1
							-
							t1.col1
					)
			=
			2
"
]).


-define (TEST_UPDATE,[
                                        "ALTER USER test_user_123 IDENTIFIED BY new_password",
                                        "ALTER USER test_user_123 ACCOUNT LOCK",
                                        "ALTER USER test_user_123 ACCOUNT UNLOCK",
                                        "ALTER USER test_user_123 PASSWORD EXPIRE",
                                        "update abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a is NULL",
                                        "update abc set a='a', b='b\nb', c='c' || \"c\r\nc\" where a || b = 'c' || 'd'"
]).

-define (TEST_DELETE,[
                                        "DROP USER test_user_123",
                                        "DROP USER test_user_123 CASCADE",
                                        "DROP TABLE table_name",
                                        "DROP TABLE IF EXISTS table_name RESTRICT"
]).

-define (TEST_CREATE, [
                                        "create table key_test (col1 '{atom,integer}', col2 '{string,binstr}')",
                                        "CREATE TABLE Persons
                                        (
                                        P_Id int,
                                        LastName varchar2,
                                        LastName varchar2(255),
                                        FirstName varchar2(255),
                                        Address varchar2(255),
                                        City varchar2(255)
                                        )",
                                        "create table table_1 (
                                                       owner userid,
                                                       private term,
                                                       field_t decimal,
                                                       field_t1 bool,
                                                       field_t1 boolean,
                                                       field_t1 number,
                                                       field_t1 number(1),
                                                       field_t1 number(1,2),
                                                       field_a atom default 'undefined',
                                                       field_b list,
                                                       'field_c' string default 'NULL',
                                                       'field_d' tuple default erl(\"{1,2}\"),
                                                       field_e date default fun()-> calendar:localtime() end.
                                                       )",
                                        "CREATE LOCAL BAG TABLE test (fld CHAR)",
                                        "CREATE CLUSTER SET TABLE test (fld CHAR)",
                                        "CREATE SCHEMA ORDERED_SET TABLE test (fld CHAR)",
                                        "CREATE ORDERED_SET TABLE test (fld CHAR)",
                                        "CREATE SCHEMA TABLE test (fld CHAR)",
                                        "CREATE TABLE test (fld CHAR)",
                                        "CREATE LOCAL BAG TABLE test
                                        (
                                        fld CHAR
                                        , fld VARCHAR2(13) DEFAULT '123'
                                        , fld BLOB(2000)
                                        , fld INT DEFAULT 99999999999999999
                                        , fld FLOAT(-3) DEFAULT 123456
                                        , fld DECIMAL(10,3) DEFAULT 1.1234
                                        , fld DATE DEFAULT SYSDATE
                                        , fld DATETIME
                                        , fld TIMESTAMP
                                        , fld INT
                                        )",
                                        "CREATE LOCAL TABLE test
                                        (
                                        fld TUPLE(0) default fun() -> {} end.
                                        , fld BINARY(1000)
                                        , fld atom
                                        , fld ipaddr(4) default fun() -> {0,0,0,0} end. 
                                        , fld LIST(0) default fun() -> [] end.
                                        , fld BINSTR(1000) default fun() -> <<\"no_value\">> end.
                                        , fld PID
                                        , fld ref
                                        , fld FUN(1)
                                        , fld datetime default fun() -> calendar:local_time() end.
                                        , fld timestamp(3) default fun() -> erlang:now() end.
                                        , fld INTEGER(10,-3)
                                        )",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY",
                                        "CREATE USER test_user_2 IDENTIFIED EXTERNALLY AS test_usr_2_extern",
                                        "CREATE USER test_user_4 IDENTIFIED GLOBALLY",
                                        "CREATE USER test_user_4 IDENTIFIED GLOBALLY AS test_usr_2_extern",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 DEFAULT TABLESPACE table_2",
                                        "CREATE USER test_user_1 IDENTIFIED EXTERNALLY AS test_usr_2_extern TEMPORARY TABLESPACE table_1",
                                        "CREATE USER test_user_1 IDENTIFIED BY a_password DEFAULT TABLESPACE table_1 TEMPORARY TABLESPACE table_2",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY PROFILE user_profile",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY PASSWORD EXPIRE",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT LOCK",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY ACCOUNT UNLOCK",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA UNLIMITED ON table_1",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_2",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10M ON table_3 QUOTA UNLIMITED ON table_1",
                                        "CREATE USER test_user_3 IDENTIFIED EXTERNALLY QUOTA 10 ON table_3 QUOTA 10M ON table_4 QUOTA UNLIMITED ON table_1"
]).

-define (TEST_INSERT,[
                                        "insert into number (float,integer) values ('C', \"undefined\")",
                                        "insert into def (col1,col2) values ('C', \"undefined\")",
                                        "insert into def (col1,col2) values ('C', 5+1)",
                                        "insert into table_1 (field_a, field_b) values ('first','Stefan''s choice.')",
                                        "insert into table_1 (field_a, field_c) values ('second','Double quote \" in string')",
                                        "insert into table_1 (field_a, field_c) values ('second','Single quote '' in string')",
                                        "insert into table_1 (field_a, field_d) values ('third',erl(\"{a,b,c}\"))",
                                        "insert into table_1 (field_a, field_3) values ('third','31.12.2012 23:59:59')",
                                        "insert into abc values (1, 'a', 'b', 'c' || 'd')",
                                        "INSERT INTO Persons VALUES (4,'Nilsen', 'Johan', 'Bakken 2', 'Stavanger')"
]).

-define (TEST_TRUNCT,[
                                        "truncate table tbl",
                                        "truncate table scm.tbl",
                                        "truncate table scm.tbl preserve materialized view log",
                                        "truncate table scm.tbl purge materialized view log",
                                        "truncate table scm.tbl drop storage",
                                        "truncate table scm.tbl reuse storage",
                                        "truncate table scm.tbl purge materialized view log drop storage",
                                        "truncate table tbl purge materialized view log drop storage"
]).

-define (TEST_GRANTS,[
										"GRANT manage_system TO user_1",
										"GRANT a,b,c TO user2",
										"GRANT SELECT ON ddTable TO user_1",
										"GRANT SELECT ON schema1.ddTable TO user_1",
										"GRANT ALL ON ddTable TO user1,user2",
										"GRANT EXECUTE ON module1 TO user1",
										"GRANT all privileges ON schema1.ddTable TO role2",
										"grant update, delete on ddTable to test_user_1",
										"grant insert on ddTable to test_user_1 WITH GRANT OPTION",
                                        "GRANT manage_system TO test_user_1 with admin option"
]).

-define (TEST_REVOKE,[
										"REVOKE manage_system FROM admin",
										"REVOKE a,b,c FROM user1,user2",
										"REVOKE SELECT ON ddTable FROM user_1",
										"REVOKE ALL ON schema1.ddTable FROM user1,user2",
										"REVOKE EXECUTE ON module1 FROM user1",
										"revoke update, delete on ddTable from test_user_1"
]).

-define (TEST_SQLS, [
      {"SELECT", ?TEST_SELECT, -1} % 1
    , {"INSERT", ?TEST_INSERT, -1} % 2
    , {"CREATE", ?TEST_CREATE, -1} % 3 
    , {"UPDATE", ?TEST_UPDATE, -1} % 4 
    , {"DELETE", ?TEST_DELETE, -1} % 5 
    , {"TRUNCT", ?TEST_TRUNCT, -1} % 6 
    , {"GRANTS", ?TEST_GRANTS, -1} % 7 
    , {"REVOKE", ?TEST_REVOKE, -1} % 8 
]).
