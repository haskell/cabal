#include "assert.h"
#include "stdlib.h"
#include "stdio.h"

#define HASH_KEY_MAX_SALT 999
#define HASH_KEY_MAX_INT 2147483647
#define HASH_KEY_FAST_ASSERT 0
#define HASH_KEY_MAX_HASHID 4
#define HASH_KEY_TABLE_SALT 0

#define MAX(a, b) ((a)>(b)?(a):(b))

struct Container {
  int exists;
  int key;
  int value;
};

struct Entry {
  struct Container* containers;
  int length;
  int hashId;
  int salt;
};

struct IntSet {
  struct Entry* table;
  int tableLength;
  int size;
};

int powerAndMod(int x, int power, int mod) {
  if (!HASH_KEY_FAST_ASSERT) {
    assert(power >= 0 && power < 1000); // Use only for positive, small powers
    assert(mod * mod < HASH_KEY_MAX_INT);
    assert(x * x < HASH_KEY_MAX_INT);
  }

  int result = x;
  while (power > 0) {
    result = result * x;
    result = result % mod;
    power -= 1;
  }
  return result;
}

int hash(int intKey, int hashId, int salt) {
  assert((intKey + HASH_KEY_MAX_SALT) * (intKey + HASH_KEY_MAX_SALT) <
      HASH_KEY_MAX_INT);
  int primeP = 0;
  int modP = 0;
  assert(hashId >= 0 && hashId < 5);
  if (hashId == 0) {
    primeP = 306133;
    modP = 26407;
  } else if (hashId == 1) {
    primeP = 307189;
    modP = 26687;
  } else if (hashId == 2) {
    primeP = 307823;
    modP = 27259;
  } else if (hashId == 3) {
    primeP = 308887;
    modP = 26717;
  } else if (hashId == 4) {
    primeP = 310693;
    modP = 27197;
  } else {
    assert(hashId < HASH_KEY_MAX_HASHID); // This should throw an exception
  }

  assert(intKey < HASH_KEY_MAX_INT - HASH_KEY_MAX_SALT);
  int totalExps = 0;
  int LOG_5_PRIME_P = 9;
  int* primePBase5 = malloc(LOG_5_PRIME_P * sizeof(int));
  {
    int i = 0;
    int currentPrimeP = primeP;
    for (i = 0; i < LOG_5_PRIME_P; i += 1) {
      primePBase5[i] = currentPrimeP % 5;
      currentPrimeP = currentPrimeP / 5;
    }

    int finalHash = 0;
    int currentPowerOf5 = intKey + salt;
    for (i = 0; i < LOG_5_PRIME_P; i += 1) {
      finalHash += currentPowerOf5 * primePBase5[i];
      currentPowerOf5 = powerAndMod(currentPowerOf5, 5, modP);
    }
    finalHash = finalHash % modP;
    free(primePBase5);
    return finalHash;
  }
}

int getEntryLength(int length) {
  if (length == 1) {
    return 1;
  } else {
    return length * length * 2;
  }
}

int getTableLength(int length) {
  return MAX(8, (length * HASH_KEY_MAX_HASHID) / 2);
}

struct IntSet* makeNewTable(int newTableLength) {
  struct IntSet* result = malloc(sizeof(struct IntSet));
  result->table = malloc(newTableLength * sizeof(struct Entry));
  result->tableLength = newTableLength;
  result->size = 0;

  int i = 0;
  for (i = 0; i < newTableLength; i += 1) {
    result->table[i].containers = NULL;
    result->table[i].length = 0;
    result->table[i].hashId = 0;
    result->table[i].salt = rand() % HASH_KEY_MAX_SALT;
  }
  return result;
}

void destroyTable(struct IntSet* p) {
  int i = 0;
  for (i = 0; i < p->tableLength; i += 1) {
    free(p->table[i].containers);
  }
  free(p->table);
  free(p);
}

void add(struct IntSet* p, int v);

struct IntSet* intSetResize(struct IntSet* p) {
  int i = 0;
  int newTableLength = getTableLength(p->size);
  struct IntSet* newTable = makeNewTable(newTableLength);
  for (i = 0; i < p->tableLength; i += 1) {
    int j = 0;
    for (j = 0; j < p->table[i].length; j += 1) {
      if (!p->table[i].containers[j].exists) {
        continue;
      } else {
        add(newTable, p->table[i].containers[j].value);
      }
    }
    
  }
  destroyTable(p);
  return newTable;
}

void printTable(struct IntSet* p) {
  printf("---TABLE---\n");
  printf("size: %d\n", p->size);
  printf("tableLength: %d\n", p->tableLength);
  int i = 0;
  for (i = 0; i < p->tableLength; i += 1) {
    int trueLength = 0;
    int j = 0;
    for (j = 0; j < getEntryLength(p->table[i].length); j += 1) {
      if (p->table[i].containers[j].exists) {
        trueLength += p->table[i].containers[j].key;
        printf("key: %d\n", p->table[i].containers[j].key);
      }
    }
    printf("entries%d: %d,%d\n", i, p->table[i].length, trueLength);
  }
  printf("-----------\n");
}

struct IntSet* newIntSet() {
  struct IntSet* p = malloc(sizeof(struct IntSet));
  p->table = malloc(1);
  p->tableLength = 0;
  p->size = 0;
  p = intSetResize(p);
  return p;
}

int in_(struct IntSet* p, int v) {
  int entryId = hash(v, HASH_KEY_MAX_HASHID, HASH_KEY_TABLE_SALT) % p->tableLength;
  if (p->table[entryId].length == 0) { return 0; }
  int containerId = (hash(v, p->table[entryId].hashId, p->table[entryId].salt) %
      getEntryLength(p->table[entryId].length));
  if (p->table[entryId].containers[containerId].key == v) {
    return 1;
  } else {
    return 0;
  }
}

void add(struct IntSet* p, int v) {
  if (in_(p, v)) {
    return;
  }
  if (getTableLength(p->size + 1) > p->tableLength) {
    p = intSetResize(p);   
  }

  int entryId = hash(v, HASH_KEY_MAX_HASHID, HASH_KEY_TABLE_SALT) % p->tableLength;
  int newEntryLength = getEntryLength(p->table[entryId].length + 1);
  struct Entry* entry = &(p->table[entryId]);
  struct Container* newContainers = NULL;
  while (1) {
    newContainers = malloc(newEntryLength * sizeof(struct Container));
    // Move new value into new container list
    int newContainerId = hash(v, entry->hashId, entry->salt) % newEntryLength;
    newContainers[newContainerId].exists = 1;
    newContainers[newContainerId].key = v;
    newContainers[newContainerId].value = v;
    // Attempt to move old values into container list
    int i = 0;
    int noCollisions = 1;
    for (i = 0; i < getEntryLength(entry->length); i += 1) {
      // put old existing containers into newContainers
      if (!entry->containers[i].exists) {
        // empty container, no need to copy
        continue;
      } else if (entry->containers[i].key == v) {
        // duplicate key, no need to copy (insert new instead)
        continue;
      }
      int containerId = (hash(entry->containers[i].key, entry->hashId, entry->salt) %
          newEntryLength);
      if (newContainers[containerId].exists) {
        // Collision detected; remalloc, free and try again with new hash
        free(newContainers);
        newContainers = NULL;
        entry->hashId += 1;
        if (entry->hashId == HASH_KEY_MAX_HASHID) {
          entry->hashId = 0;
          entry->salt += 1;
        }
        noCollisions = 0;
        break;
      }
      newContainers[containerId].exists = 1;
      newContainers[containerId].key = entry->containers[i].key;
      newContainers[containerId].value = entry->containers[i].value;
    }
    if (noCollisions) { break; }
  }
  free(entry->containers);
  entry->containers = newContainers;
  entry->length += 1;
  p->size += 1;
  
  return;
}

void intSetRemove(struct IntSet* p, int v) {
  if (!in_(p, v)) {
    return;
  }
  if (getTableLength(p->size - 1) < p->tableLength / 4) {
    p = intSetResize(p);
  }
  int entryId = hash(v, HASH_KEY_MAX_HASHID, HASH_KEY_TABLE_SALT) % p->tableLength;
  int newEntryLength = getEntryLength(p->table[entryId].length - 1);
  struct Entry* entry = &(p->table[entryId]);
  struct Container* newContainers = NULL;
  while (1) {
    newContainers = malloc(newEntryLength * sizeof(struct Container));
    // Move new value into new container list
    int oldContainerId = hash(v, entry->hashId, entry->salt) % entry->length;
    entry->containers[oldContainerId].exists = 0;
    entry->containers[oldContainerId].key = 0;
    entry->containers[oldContainerId].value = 0;
    // Attempt to move old values into container list
    int i = 0;
    int noCollisions = 1;
    for (i = 0; i < getEntryLength(entry->length); i += 1) {
      // put old existing containers into newContainers
      if (!entry->containers[i].exists) {
        // empty container, no need to copy
        continue;
      } else if (entry->containers[i].key == v) {
        // duplicate key, no need to copy (insert new instead)
        continue;
      }
      int containerId = (hash(entry->containers[i].key, entry->hashId, entry->salt) %
          newEntryLength);
      if (newContainers[containerId].exists) {
        // Collision detected; remalloc, free and try again with new hash
        free(newContainers);
        newContainers = NULL;
        entry->hashId += 1;
        if (entry->hashId == HASH_KEY_MAX_HASHID) {
          entry->hashId = 0;
          entry->salt += 1;
        }
        noCollisions = 0;
        break;
      }
      newContainers[containerId].exists = 1;
      newContainers[containerId].key = entry->containers[i].key;
      newContainers[containerId].value = entry->containers[i].value;
    }
    if (noCollisions) { break; }
  }
  free(entry->containers);
  entry->containers = newContainers;
  entry->length -= 1;
  p->size -= 1;

  return;
}

int size(struct IntSet* p) {
  return p->size;
}
