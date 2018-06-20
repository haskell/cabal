#include <stdint.h>
#include <stdio.h>

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"

costMatrix_p matrixInit(size_t alphSize, int *tcm) {
   return (costMatrix_p) construct_CostMatrix_C(alphSize, tcm);
}

void matrixDestroy(costMatrix_p untyped_ptr) {
    destruct_CostMatrix_C(untyped_ptr);
}

int getCostAndMedian(dcElement_t *elem1, dcElement_t *elem2, dcElement_t *retElem, costMatrix_p tcm) {
    size_t alphSize = elem1->alphSize;
    dcElement_t *elem1copy = (dcElement_t *) malloc(sizeof(dcElement_t));
    elem1copy->alphSize    = alphSize;
    dcElement_t *elem2copy = (dcElement_t *) malloc(sizeof(dcElement_t));
    elem2copy->alphSize    = alphSize;

    elem1copy->element = makePackedCharCopy( elem1->element, alphSize, 1 );
    elem2copy->element = makePackedCharCopy( elem2->element, alphSize, 1 );

    int cost = call_getSetCost_C(tcm, elem1copy, elem2copy, retElem);

    return cost;
}
