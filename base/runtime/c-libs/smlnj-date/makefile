#
# the makefile for the Date library
#

SHELL =		/bin/sh

INC_DIR =	../../include
CLIB_DIR =	../

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I../../objs

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libsmlnj-date.a

VERSION =	v-dummy

OBJS =		smlnj-date-lib.o \
		gmtime.o \
		localoffset.o \
		localtime.o \
		mktime.o \
		strftime.o \
		unix-date.o

$(OBJS) : $(VERSION)
$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:	$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(CLIB_DIR)/ml-c.h cfun-proto-list.h cfun-list.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -c $<

clean :
	rm -f v-* *.o $(LIBRARY)

