all: liblibmysql.a liblibpq.a libsqlite.a

liblibmysql.a: libmysql.def
	dlltool --input-def libmysql.def --dllname libmysql.dll --output-lib liblibmysql.a -k
	
liblibpq.a: libpqdll.def
	dlltool --input-def libpqdll.def --dllname libpq.dll --output-lib liblibpq.a -k

libsqlite.a: sqlite.def
	dlltool --input-def sqlite.def --dllname sqlite.dll --output-lib libsqlite.a -k
