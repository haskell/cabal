#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "dynamicCharacterOperations.h"

#define __STDC_FORMAT_MACROS

size_t dynCharSize(size_t alphSize, size_t numElems) { return 1; }

size_t dcElemSize(size_t alphSize) { return 1; }

packedChar *allocatePackedChar( size_t alphSize, size_t numElems ) {
    packedChar *outChar = (packedChar*) calloc( dynCharSize(alphSize, numElems), sizeof(packedChar) );
    if (outChar == NULL) {
        printf("Out of memory.\n");
        fflush(stdout);
        exit(1);
    }
    return outChar;
}

packedChar *makePackedCharCopy( packedChar *inChar, size_t alphSize, size_t numElems) {
    packedChar *outChar = allocatePackedChar(alphSize, numElems);
    size_t length = dynCharSize(alphSize, numElems);
    for (size_t i = 0; i < length; i++) {
        outChar[i] = inChar[i];
    }
    return outChar;
}

void freeDynChar( dynChar_t *p ) { free( p->dynChar ); }

void freeDCElem( const dcElement_t *p ) { free( p->element ); }
