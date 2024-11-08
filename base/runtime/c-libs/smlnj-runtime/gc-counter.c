/* gc-counter.c
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Some counters for measuring allocation rates, etc.  Note that these
 * are not part of the distribution, but are just defined in this branch
 * for research purposes.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "vproc-state.h"
#include "gc.h"
#include "heap.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

static Unsigned32_t numGCs[MAX_NGENS+1];

/* _ml_RunT_gc_counter_reset : bool -> unit
 *
 * reset the counters.  If the flag is true, the we force a GC of all
 * generations before resetting.
 */
ml_val_t _ml_RunT_gc_counter_reset (ml_state_t *msp, ml_val_t arg)
{
    heap_t	*heap = msp->ml_heap;

    /* check if a full GC is requested */
    if (arg == ML_true) {
        // collect all generations
        InvokeGCWithRoots (msp, heap->numGens, &arg, NIL(ml_val_t *));
    } else {
        // minor collection
        InvokeGCWithRoots (msp, 0, &arg, NIL(ml_val_t *));
    }

    ResetGCStats (heap);

    return ML_unit;
}

/* _ml_RunT_gc_counter_read : unit
 *      -> (Word.word * Word.word * Word.word * Word.word * Word.word list)
 *
 * read the counters.  The results are:
 *
 *  - scaling factor for counts (Word.word)
 *  - scaled allocation count (Word.word)
 *  - scaled first-generation allocation count (Word.word)
 *  - scaled count of promotions to first generation (Word.word)
 *  - # of collections in a list `[n0, n1, n2, ...]`, where ni is the number of
 *    times generation i has been collected since the "reset" call. (Word.word list)
 */
ml_val_t _ml_RunT_gc_counter_read (ml_state_t *msp, ml_val_t arg)
{
    gc_stats_t  stats;

    GetGCStats (msp, &stats);

    ml_val_t scale = INT_CtoML(stats.bytesPerCnt);
    ml_val_t nAlloc = INT_CtoML(stats.allocCnt);
    ml_val_t nFirstAlloc = INT_CtoML(stats.allocFirstCnt);
    ml_val_t nPromote = INT_CtoML(stats.promoteCnt[0]);

    /* allocate number of GCs list */
    ml_val_t lp = LIST_nil;
    if (stats.numGCs[0] > 0) {
        /* allocate the list of number of collections */
        int n = 0;
        for (int i = 1;  i <= msp->ml_heap->numGens;  ++i) {
            if (stats.numGCs[i] > 0) { n = i; } else break;
        }
        for (int i = n;  i >= 0;  --i) {
            LIST_cons(msp, lp, INT_CtoML(stats.numGCs[i]), lp);
        }
    }

    /* allocate result tuple */
    ml_val_t res = ML_Alloc5(msp, scale, nAlloc, nFirstAlloc, nPromote, lp);

    return res;

}
