#include "assert.h"
#include "stdlib.h"

#define MAX(a, b) ((a)>(b)?(a):(b))

struct Vector {
  int* data;
  int dataLength; // Invariant: malloc size of the data must always be dataLength
  int length;
};

inline int getDataLength(length) {
  return MAX(8, length / 2 * 2 + 1);
}

struct Vector* newVector(length) {
  assert(length >= 0);
  struct Vector* p = malloc(sizeof(struct Vector));
  p->length = length;
  p->dataLength = getDataLength(length);
  p->data = calloc(p->dataLength, sizeof(int));
  return p;
}

int getValueAt(struct Vector* p, int index) {
  // TODO: add assert print statements for index of failure and vector length
  assert(index < p->length);
  return p->data[index];
}

void setValueAt(struct Vector* p, int index, int value) {
  p->data[index] = value;
  return;
}

void pushBack(struct Vector* p, int value) {
  if (getDataLength(p->length + 1) > p->dataLength) {
    p->dataLength = getDataLength(p->length + 1);
    int* oldData = p->data;
    p->data = calloc(p->dataLength, sizeof(int));
    memcpy(p->data, oldData, p->dataLength * sizeof(int));
    free(oldData);
  }
  p->data[p->length] = value;
  p->length = (p->length + 1);
  return;
}

void clear(struct Vector* p) {
  free(p->data);
  p->length = 0;
  p->dataLength = getDataLength(p->length);
  p->data = calloc(p->dataLength, sizeof(int));
  return;
}

void insert(struct Vector* p, int first, int last, int value) {
  assert(first < p->length && first >= 0);
  assert(last <= p->length && last >= 0 && last >= first);
  if (last - first + p->length < p->dataLength) {
    memcpy(p->data + last, p->data + first, (p->length - first) * sizeof(int));
  } else {
    int* oldData = p->data;
    p->data = malloc(getDataLength(p->length + last - first) * sizeof(int));
    memcpy(p->data, oldData, first * sizeof(int));
    memcpy(p->data + last, oldData + first, (p->length - first) * sizeof(int));
    free(oldData);
  }
  int i = 0;
  for (i = first; i < last; i += 1) {
    p->data[i] = value;
  }
  p->length = last - first + p->length;
  p->dataLength = getDataLength(p->dataLength);
  return;
}

void erase(struct Vector* p, int first, int last) {
  assert(first < p->length && first >= 0);
  assert(last <= p->length && last >= 0 && last >= first);
  memcpy(p->data + first, p->data + last, (p->length - last) * sizeof(int));
  p->length = last - first + p->length;
  if (getDataLength(p->length) / p->dataLength >= 4) {
    int oldDataLength = p->dataLength;
    p->dataLength = getDataLength(p->dataLength);
    int* oldData = p->data;
    p->data = calloc(getDataLength(p->length), sizeof(int));
    memcpy(p->data, oldData, getDataLength(p->length) * sizeof(int));
    free(oldData);
  }
  return;
}

int length(struct Vector* p) {
  return p->length;
}
