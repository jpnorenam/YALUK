# File: makefile
# Purpose: compile and link atp
#
# Change this line (and directories if necessary)
# Change paramet.inc, if necessary
#
SYSTEM = LINUX
INCFLAGS = -Inodaline
CC = gcc
FOR = gfortran
#
# OBJECTS is a customized list of modules that are being worked on
# and are present in the current directory

OBJECTS  = yaluk_exec.o \
		       yaluk_init.o \
					 em_field_funct.o \
					 basic_functions.o

# ------------------------

CFLAGS = -O2
FFLAGS = -Ofast -ffree-form -ffree-line-length-512 -std=f95 -w

LIBRARY=yaluk.a

.f.o:
	$(FOR) -c $(FFLAGS) $<
.c.o:
	$(CC) -c $(CFLAGS) $(INCFLAGS) $<
$(LIBRARY) : $(OBJECTS)
	$(FOR) -static -fpic -o $(LIBRARY) $(OBJECTS)
#
# make available the following objects:
#
# /usr/lib/i386-linux-gnu/crt1.o
# /usr/lib/i386-linux-gnu/crti.o
#
# This is the dependancy relationship for powerflow which
# specifies the link and causes the use of the utility compile
# procedures below.
# WARNING:
# The <tab> as the first character signifies the action to
# take for a given dependancy.
# The back slash causes a continuation of the current line
# even if the line is commented out.
# The following are the compile procedures for any source code specified
# in the dependancy.  These override the system defaults.
# the first procedure is for any fortran code (appendix .f creating .o)
# the second is for an C code (appendix .c creating .o)

#INCFLAGS = -Inodaline

# DO NOT DELETE THIS LINE -- make depend depends on it.GS) $<
#
#
# end of makefile
