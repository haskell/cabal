#include "HsODBC.h"

#if defined(_WIN32_)
// Under Windows SQLFreeEnv function has stdcall calling convention
// while in Haskell functions represented with FunPtr must be always
// with ccall convention. For that reason we need to redirect calling
// to this function.

void my_sqlFreeEnv(HENV hEnv)
{
	SQLFreeEnv(hEnv);
}
#endif
