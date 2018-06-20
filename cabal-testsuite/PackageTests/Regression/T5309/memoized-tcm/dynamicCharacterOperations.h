#ifndef DYNAMIC_CHARACTER_OPERATIONS
#define DYNAMIC_CHARACTER_OPERATIONS

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdint.h>

typedef uint64_t packedChar;
typedef void *costMatrix_p;

typedef struct dynChar_t {
    size_t      alphSize;
    size_t      numElems;     // how many dc elements are stored
    size_t      dynCharLen;   // how many uint64_ts are necessary to store the elements
    packedChar *dynChar;
} dynChar_t;

typedef struct dcElement_t {
    size_t      alphSize;
    packedChar *element;
} dcElement_t;

size_t dynCharSize(size_t alphSize, size_t numElems);

size_t dcElemSize(size_t alphSize);

void freeDynChar( dynChar_t *p );

void freeDCElem( const dcElement_t *p );

packedChar *allocatePackedChar( size_t alphSize, size_t numElems );

packedChar *makePackedCharCopy( packedChar *inChar, size_t alphSize, size_t numElems );

#endif /* DYNAMIC_CHARACTER_OPERATIONS */
