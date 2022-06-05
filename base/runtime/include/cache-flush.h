/* cache-flush.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * System dependent includes and macros for flushing the cache.
 */

#ifndef _CACHE_FLUSH_
#define _CACHE_FLUSH_

#if defined(ARCH_X86)
/* 386 & 486 have unified caches and the pentium has hardware consistency */
#  define FlushICache(addr, size)

#elif (defined(ARCH_PPC) && defined(OPSYS_AIX))
#  include <sys/cache.h>
#  define FlushICache(addr, size)	_sync_cache_range((addr), (size))

#elif (defined(ARCH_SPARC) || defined(OPSYS_MKLINUX))
extern FlushICache (void *addr, int nbytes);

#elif (defined(ARCH_PPC) && (defined(OPSYS_LINUX) || defined(OPSYS_DARWIN) ))
extern FlushICache (void *addr, int nbytes);

#else
#  define FlushICache(addr, size)
#endif

#endif /* !_CACHE_FLUSH_ */

