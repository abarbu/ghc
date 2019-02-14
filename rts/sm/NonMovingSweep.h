/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator: Sweep phase
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "NonMoving.h"
#include "Hash.h"

GNUC_ATTR_HOT void nonmovingSweep(void);

// Remove unmarked entries in oldest generation mut_lists
void nonmovingSweepMutLists(void);

// Remove unmarked entries in oldest generation large objects list
void nonmovingSweepLargeObjects(void);

// Remove unmarked entries in oldest generation compact objects list
void nonmovingSweepCompactObjects(void);

// Remove dead entries in the stable name table
void nonmovingSweepStableNameTable(void);

// Collect the set of segments to be collected during a major GC into
// nonmovingHeap.sweep_list.
void nonmovingPrepareSweep(void);

#if defined(DEBUG)
// The non-moving equivalent of the moving collector's gcCAFs.
void nonmovingGcCafs(struct MarkQueue_ *queue);
#endif
