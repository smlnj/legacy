/* 
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * generates, on stdout, a "cc-info" file for the SML/NJ C interface
 * using the data sizes of the compiling C compiler.
 */

#include <stdio.h>

#define PUTS(s) printf("%s",s)
#define PUTVAL(n,i) printf("\t\tval %s = %d\n", n, i)
#define COMMENT(s) printf("(* %s *)\n", s)
#define NEWLINE() putchar('\n')

main(int argc,char *argv[])
{
  if (argc != 2) {
    fprintf(stderr,"usage: %s name-for-structure\n", argv[0]);
    exit(1);
  }
  NEWLINE();
  PUTS("structure "); PUTS(argv[1]); PUTS(" : CC_INFO =\n");
  PUTS("\tstruct\n");
  PUTS("\t\t");COMMENT("all sizes in bytes");
  NEWLINE();
  PUTVAL("intSzB", sizeof(int));
  PUTVAL("shortSzB", sizeof(short));
  PUTVAL("longSzB", sizeof(long));
  NEWLINE();
  PUTVAL("charSzB", sizeof(char));
  NEWLINE();
  PUTVAL("floatSzB", sizeof(float));
  PUTVAL("doubleSzB", sizeof(double));
  NEWLINE();
  PUTVAL("ptrSzB", sizeof(int *));
  NEWLINE();
  PUTVAL("unionAlign", sizeof(int *));
  PUTVAL("structAlign", sizeof(int *));
  NEWLINE();
  PUTS("\tend "); PUTS("(* structure "); PUTS(argv[1]); PUTS(" *)\n");
  exit(0);
}
