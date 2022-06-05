/* ml-roots.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The root register indices for various machines.
 *
 * NROOTS gives the size of the variable-size portion (roots[]) of the
 * ML state vector.  Note that the name "roots" is slightly misleading;
 * while every entry in the vector must be saved over calls to C, not
 * every entry is a valid root on every entry to C.  The valididity of
 * most entries is indicated using the register map convention (via
 * ArgRegMap); these entries are valid (and live) iff the corresponding
 * bit in the register mask is set (see cps/generic.sml).  N_ARG_REGS
 * gives the number of such entries. The pc, exncont, varptr, and baseptr
 * (if defined) are always valid roots, and the icounter (if defined) never is.
 *
 * NOTE: N_ARG_REGS is use to implement the old FFI mechanism that Lorenz
 * Huelsbergen implemented (see c-libs/smlnj-ccalls).  I suspect that this
 * mechanism no longer works and it has been superseded by NLFFI.  We should
 * probably remove it at some time (JHR; 2019-11-10).
 */

#ifndef _ML_ROOTS_
#define _ML_ROOTS_

#if defined (ARCH_AMD64)

#   define NROOTS	    15
#   define N_ARG_REGS	    19
#   define N_PSEUDO_REGS    2
#   define EXN_INDX	    0		/* 40(rsp) */
#   define ARG_INDX	    1		/* rbp */
#   define CONT_INDX	    2		/* rsi */
#   define CLOSURE_INDX	    3		/* rd */
#   define VAR_INDX	    4		/* 56(rsp) */
#   define LINK_INDX	    5		/* r8 */
#   define BASE_INDX	    6		/* 32(rsp) */
#   define PC_INDX	    7		/* rip */
#   define MISC0_INDX	    8		/* rbx */
#   define MISC1_INDX	    9		/* rcx */
#   define MISC2_INDX	    10		/* rdx */
#   define MISC3_INDX	    11		/* r10 */
#   define MISC4_INDX	    12		/* r11 */
#   define MISC5_INDX	    13		/* r12 */
#   define MISC6_INDX	    14		/* r13 */

#elif defined(ARCH_PPC)

#   define NROOTS	    24
#   define N_ARG_REGS       19
#   define N_PSEUDO_REGS    2

#   define LINK_INDX        0
#   define CLOSURE_INDX	    1
#   define ARG_INDX	    2
#   define CONT_INDX	    3
#   define EXN_INDX	    4
#   define VAR_INDX         5
#   define BASE_INDX	    6
#   define PC_INDX	    8

#   define MISC0_INDX	    9	/* 24 */
#   define MISC1_INDX	    10	/* 25 */
#   define MISC2_INDX	    11	/* 26 */
#   define MISC3_INDX	    12	/* 27 */
#   define MISC4_INDX	    13	/* 3 */
#   define MISC5_INDX	    14	/* 4 */
#   define MISC6_INDX	    15	/* 5 */
#   define MISC7_INDX	    16	/* 6 */
#   define MISC8_INDX	    17	/* 7 */
#   define MISC9_INDX	    18	/* 8 */
#   define MISC10_INDX	    19	/* 9 */
#   define MISC11_INDX	    20	/* 10 */
#   define MISC12_INDX	    21	/* 11 */
#   define MISC13_INDX	    22	/* 12 */
#   define MISC14_INDX	    23	/* 13 */

#elif defined(ARCH_SPARC)

#   define NROOTS	    23		/* pc, %i0-i5, %g7, %g1-%g3, %l0-%l7, %o0-%o1 %o3-%o4 */
#   define N_ARG_REGS       19          /* exclude baseptr */
#   define N_PSEUDO_REGS    2
#   define PC_INDX	    6
#   define EXN_INDX	    7		/* %g7 */
#   define ARG_INDX	    0		/* %i0 */
#   define CONT_INDX	    1		/* %i1 */
#   define CLOSURE_INDX	    2		/* %i2 */
#   define BASE_INDX	    3		/* %i3 */
#   define VAR_INDX         5		/* %i5 */
#   define LINK_INDX        4		/* %g1 */
#   define MISC0_INDX	    8		/* %g2 */
#   define MISC1_INDX	    9		/* %g3 */
#   define MISC2_INDX	    10		/* %o0 */
#   define MISC3_INDX	    11		/* %o1 */
#   define MISC4_INDX	    12		/* %l0 */
#   define MISC5_INDX	    13		/* %l1 */
#   define MISC6_INDX	    14		/* %l2 */
#   define MISC7_INDX	    15		/* %l3 */
#   define MISC8_INDX	    16		/* %l4 */
#   define MISC9_INDX	    17		/* %l5 */
#   define MISC10_INDX	    18		/* %l6 */
#   define MISC11_INDX	    19		/* %l7 */
#   define MISC12_INDX	    20		/* %i4 */
#   define MISC13_INDX	    21		/* %o3 */
#   define MISC14_INDX	    22		/* %o4 */

#elif defined (ARCH_X86)

#   define NROOTS	    26
#   define N_ARG_REGS	    23
#   define N_PSEUDO_REGS    2
#   define EXN_INDX	    0		/* 8(esp) */
#   define ARG_INDX	    1		/* ebp	   */
#   define CONT_INDX	    2		/* esi	   */
#   define CLOSURE_INDX	    3		/* 16(esp) */
#   define VAR_INDX	    4		/* 28(esp) */
#   define LINK_INDX	    5		/* 20(esp) */
#   define PC_INDX	    6		/* eip	   */
#   define MISC0_INDX	    7		/* ebx	   */
#   define MISC1_INDX	    8		/* ecx	   */
#   define MISC2_INDX	    9		/* edx	   */
    /* MISCn, where n > 2, is a virtual register */
#   define MISC3_INDX	    10		/* 40(esp) */
#   define MISC4_INDX	    11		/* 44(esp) */
#   define MISC5_INDX	    12		/* 48(esp) */
#   define MISC6_INDX	    13		/* 52(esp) */
#   define MISC7_INDX	    14		/* 56(esp) */
#   define MISC8_INDX	    15		/* 60(esp) */
#   define MISC9_INDX	    16		/* 64(esp) */
#   define MISC10_INDX	    17		/* 68(esp) */
#   define MISC11_INDX	    18		/* 72(esp) */
#   define MISC12_INDX	    19		/* 76(esp) */
#   define MISC13_INDX	    20		/* 80(esp) */
#   define MISC14_INDX	    21		/* 84(esp) */
#   define MISC15_INDX	    22		/* 88(esp) */
#   define MISC16_INDX	    23		/* 92(esp) */
#   define MISC17_INDX	    24		/* 96(esp) */
#   define MISC18_INDX	    25		/* 100(esp) */

#else

#  error unknown architecture

#endif

#endif /* !_ML_ROOTS_ */

