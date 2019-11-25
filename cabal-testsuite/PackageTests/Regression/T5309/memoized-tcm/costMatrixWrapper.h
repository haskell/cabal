#ifndef _COST_MATRIX_WRAPPER_H
#define _COST_MATRIX_WRAPPER_H

#include <stdint.h>

#include "dynamicCharacterOperations.h"

/** Initialize a matrix (fill in all values for non-ambiguous character transition costs) using a TCM sent in from an outside source. */
costMatrix_p matrixInit(size_t alphSize, int *tcm);

/** C wrapper for cpp destructor */
void matrixDestroy(costMatrix_p untyped_ptr);

/** Like getCost, but also returns a pointer to a median value. */
int getCostAndMedian(dcElement_t *elem1, dcElement_t *elem2, dcElement_t *retElem, costMatrix_p tcm);

/** Following three fns are C references to cpp functions found in costMatrix.cpp */
costMatrix_p construct_CostMatrix_C(size_t alphSize, int *tcm);

void destruct_CostMatrix_C(costMatrix_p mytype);

int call_getSetCost_C(costMatrix_p untyped_self, dcElement_t *left, dcElement_t *right, dcElement_t *retMedian);

#endif // _COST_MATRIX_WRAPPER_H
