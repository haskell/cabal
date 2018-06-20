#include <inttypes.h>

/*
#include <climits>
#include <cstdlib>
#include <unordered_map>
*/

#include "costMatrix.h"
#include "dynamicCharacterOperations.h"
#include <cstring> //for memcpy;

#define __STDC_FORMAT_MACROS

// TODO: I'll need this for the Haskell side of things: https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-StablePtr.html

costMatrix_p construct_CostMatrix_C(size_t alphSize, int* tcm) {
    return new CostMatrix(alphSize, tcm);
}

void destruct_CostMatrix_C(costMatrix_p untyped_self) {
    delete static_cast<CostMatrix*> (untyped_self);
}

int call_getSetCost_C(costMatrix_p untyped_self, dcElement_t* left, dcElement_t* right, dcElement_t* retMedian) {

    CostMatrix* thisMtx = static_cast<CostMatrix*> (untyped_self);
    return thisMtx->getSetCostMedian(left, right, retMedian);
}


void freeCostMedian_t (costMedian_t* toFree) {
    free(toFree->second);
}

CostMatrix::CostMatrix(size_t alphSize, int* inTcm) {
    alphabetSize = alphSize;
    size_t space = alphabetSize * alphabetSize * sizeof(int);
    tcm = (int*) malloc(space);
    memcpy(tcm, inTcm, space);
    initializeMatrix();
}

CostMatrix::~CostMatrix() {
    for ( auto& thing: myMatrix ) {
        freeCostMedian_t(&thing.second);
    }
    myMatrix.clear();
    hasher.clear();

}

int CostMatrix::getCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian) {
    keys_t toLookup;
    toLookup.first  = *left;
    toLookup.second = *right;
    mapIterator found;
    int foundCost;

    found = myMatrix.find(toLookup);

    if ( found == myMatrix.end() ) {
        return -1;
    } else {
        foundCost          = found->second.first;
        retMedian->element = found->second.second;
    }

    return foundCost;
}

int CostMatrix::getSetCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian) {
    keys_t* toLookup = (keys_t*) malloc( sizeof(keys_t) );
    toLookup->first  = *left;
    toLookup->second = *right;
    mapIterator found;
    int foundCost;

    found = myMatrix.find(*toLookup);

    if ( found == myMatrix.end() ) {
      foundCost = 0;
    } else {
        foundCost = found->second.first;
    }
    return foundCost;
}

void CostMatrix::initializeMatrix () { ; }

void CostMatrix::setValue(keys_t* key, costMedian_t* median) {
    myMatrix.insert(std::make_pair(*key, *median));
}
