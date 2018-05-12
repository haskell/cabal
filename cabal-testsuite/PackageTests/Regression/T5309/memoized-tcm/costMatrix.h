/** costMatrix object to provide for a memoizable cost lookup table. Table is indexed by two
 *  dcElement values, and returns an int, for the cost. In addition, an additional dcElement
 *  is passed in by reference, and the median value of the two input elements is placed there.
 *  The getCost function is designed to interface directly with C.
 *
 *  The key lookup is an ordered pair, so when looking up transition a -> b, a must go in as
 *  first in pair
 *
 *  WARNING: In the interest of speed this code does no "type checking" to make sure that the
 *  two passed deElements are of the same type, i.e. that they have the same alphabet length.
 *  Any such checks should be done exterior to this library.
 */

#ifndef _COSTMATRIX_H
#define _COSTMATRIX_H

#define DEBUG 0

#include <climits>
#include <cstdlib>
#include <unordered_map>

#ifdef __cplusplus
extern "C" {
#endif

#include "dynamicCharacterOperations.h"

/** Next three fns defined here to use on C side. */
costMatrix_p construct_CostMatrix_C (size_t alphSize, int* tcm);

void destruct_CostMatrix_C (costMatrix_p mytype);

int call_getSetCost_C (costMatrix_p untyped_self, dcElement_t* left, dcElement_t* right, dcElement_t* retMedian);

#ifdef __cplusplus
}
#endif

typedef std::pair<dcElement_t, dcElement_t>  keys_t;
typedef std::pair<int,         packedChar*>  costMedian_t;
typedef std::pair<keys_t,      costMedian_t> mapAccessPair_t;

typedef void* costMatrix_p;

/** Allocate room for a costMedian_t. Assumes alphabetSize is already initialized. */
costMedian_t* allocCostMedian_t (size_t alphabetSize);

/** dealloc costMedian_t. */
void freeCostMedian_t (costMedian_t* toFree);

/** Allocate room for a keys_t. */
keys_t* allocKeys_t (size_t alphSize);

/** dealloc keys_t. Calls various other free fns. */
void freeKeys_t (const keys_t* toFree);

/** Allocate space for Pair<keys_t, costMedian_t>, calling allocators for both types. */
mapAccessPair_t* allocateMapAccessPair (size_t alphSize);

/** Hashes two `dcElement`s, and returns an order-dependent hash value. In this case
 *  "order dependent" means that the order of the arrays within the `dcElement`s matter,
 *  and the order that the `dcElement`s are sent in also matters, as is necessary for a
 *  non-symmetric tcm.
 *
 *  First loops through each `dcElement` and combines all of the element values (recall that a
 *  `dcElement` has two fields, the second of which is the element, and is an array of `uint64_t`s)
 *  using two different seeds, then combines the two resulting values.
 */
struct KeyHash {
    /** Following hash_combine code modified from here (seems to be based on Boost):
     *  http://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
     */
    std::size_t hash_combine (const dcElement_t lhs, const dcElement_t rhs) const {
        std::size_t left_seed  = 3141592653; // PI used as arbitrarily random seed
        std::size_t right_seed = 2718281828; // E  used as arbitrarily random seed

        std::hash<uint64_t> hasher;
        size_t elemArrCount = dcElemSize(lhs.alphSize);
        for (size_t i = 0; i < elemArrCount; i++) {
            left_seed  ^= hasher(lhs.element[i]) + 0x9e3779b9 + (left_seed  << 6) + (left_seed  >> 2);
            right_seed ^= hasher(rhs.element[i]) + 0x9e3779b9 + (right_seed << 6) + (right_seed >> 2);
        }
        left_seed ^= hasher(right_seed) + 0x9e3779b9 + (left_seed << 6) + (left_seed >> 2);
        return left_seed;
    }

    std::size_t operator()(const keys_t& k) const
    {
        return hash_combine (k.first, k.second);
    }
};

struct KeyEqual {
    // Return true if every `uint64_t` in lhs->element and rhs->element is equal, else false.
    bool operator()(const keys_t& lhs, const keys_t& rhs) const
    {
      // Assert that all key components share the same alphSize value
      if (   lhs.first.alphSize  != rhs.first.alphSize
          || lhs.first.alphSize  != lhs.second.alphSize
          || lhs.second.alphSize != rhs.second.alphSize) {
          return false;
      }

      //Assert that the left key elements match the right key elements
      size_t elemArrWidth = dcElemSize(lhs.first.alphSize);
        for (size_t i = 0; i < elemArrWidth; i++) {
            if (lhs.first.element[i] != rhs.first.element[i]) {
                return false;
            }
            if (lhs.second.element[i] != rhs.second.element[i]) {
                return false;
            }
        }
        return true;
    }
};

typedef std::unordered_map<keys_t, costMedian_t, KeyHash, KeyEqual>::const_iterator mapIterator;


class CostMatrix
{
    public:
//        CostMatrix();

        CostMatrix(size_t alphSize, int* tcm);

        ~CostMatrix();

        /** Getter only for cost. Necessary for testing, to insure that particular
         *  key pair has, in fact, already been inserted into lookup table.
         */
        int getCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian);

        /** Acts as both a setter and getter, mutating myMap.
         *
         *  Receives two dcElements and computes the transformation cost as well as
         *  the median for the two. Puts the median and alphabet size into retMedian,
         *  which must therefore by necessity be allocated elsewhere.
         *
         *  This functin allocates _if necessary_. So freeing inputs after a call will not
         *  cause invalid reads from the cost matrix.
         */
        int getSetCostMedian(dcElement_t* left, dcElement_t* right, dcElement_t* retMedian);

    private:
        std::unordered_map <keys_t, costMedian_t, KeyHash, KeyEqual> myMatrix;

        std::unordered_map <keys_t, costMedian_t, KeyHash, KeyEqual> hasher;

        size_t alphabetSize;

        /** Stored unambiguous tcm, necessary to do first calls to findDistance() without having to rewrite findDistance()
         *  and computeCostMedian()
         */
        int *tcm;

        /** Takes in a `keys_t` and a `costMedian_t` and updates myMap to store the new values,
         *  with @key as a key, and @median as the value.
         */
        void setValue(keys_t* key, costMedian_t* median);

        /** Takes in a pair of keys_t (each of which is a single `dcElement`) and computes their lowest-cost median.
         *  Uses a Sankoff-like algorithm, where all bases are considered, and the lowest cost bases are included in the
         *  cost and median calculations. That means a base might appear in the median that is not present in either of
         *  the two elements being compared.
         */
        costMedian_t* computeCostMedian(keys_t key);

        /** Find distance between an ambiguous nucleotide and an unambiguous ambElem. Return that value and the median.
         *  @param ambElem is ambiguous input.
         *  @param nucleotide is unambiguous.
         *  @param median is used to return the calculated median value.
         *
         *  This fn is necessary because there isn't yet a cost matrix set up, so it's not possible to
         *  look up ambElems, therefore we must loop over possible values of the ambElem
         *  and find the lowest cost median.
         *
         *  Nota bene: Requires symmetric, if not metric, matrix. TODO: Is this true? If so fix it?
         */
        int findDistance (keys_t* searchKey, dcElement_t* ambElem);

        /** Takes in an initial TCM, which is actually just a row-major array, creates hash table of costs
         *  where cost is least cost between two elements, and medians, where median is union of characters.
         *
         *  Nota bene:
         *  Can only be called once this.alphabetSize has been set.
         */
        void initializeMatrix ();

        // DEPRECATED!!!
        /** Takes in a pair of keys_t (each of which is a single `dcElement`) and computes their lowest-cost median.
         *  Contrast with computeCostMedian(). In this algorithm only bases which are present in at least one of
         *  the two elements being compared are considered.
         */
        /* costMedian_t* computeCostMedianFitchy(keys_t keys); */

};

#endif // COSTMATRIX_H
