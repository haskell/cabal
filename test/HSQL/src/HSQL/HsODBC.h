#ifndef HsODBC
#define HsODBC

#if defined(_WIN32_)
#include <windows.h>
#endif

#include <sqlext.h>
#include <sqlucode.h>

#define FIELD_NAME_LENGTH 255

typedef struct
	{
		HSTMT hSTMT;
		SQLUSMALLINT fieldsCount;
		SQLCHAR fieldName[FIELD_NAME_LENGTH];
		SQLSMALLINT NameLength;
        SQLSMALLINT DataType;
		SQLULEN ColumnSize;
        SQLSMALLINT DecimalDigits;
		SQLSMALLINT Nullable;
	} FIELD;

#if defined(_WIN32_)
void my_sqlFreeEnv(HENV hEnv);
#endif

#endif
