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

OBJECTS = dimdef.o \
	      newmods.o \
	      comtac.o \
	      cfun.o \
	      crandom.o \
	      cmodel.o \
	      fgnmod_yaluk.o \
	      usrfun.o \
	      analyt.o \
	      usernl.o \
	      devt69.o \
	      userline.o \
	      nlelem.o \
	      user10.o \
	      user96.o \
	      hopcod.o
# ------------------------

CFLAGS = -O2
FFLAGS=-Ofast


IMAGE=tpbig
LIBRARY = tpbig.a dislin.a ../source/libyaluk.a

.f.o:
	$(FOR) -c $(FFLAGS) $<
.c.o:
	$(CC) -c $(CFLAGS) $(INCFLAGS) $<
$(IMAGE) : $(OBJECTS)
	$(FOR) -o tpbig $(OBJECTS) $(LIBRARY)
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
