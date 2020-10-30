# File: makefile
# Purpose: compile and link atp
#
# Change this line (and directories if necessary)
# Change paramet.inc, if necessary
#
SYSTEM = LINUX
FOR = gfortran
AR = ar
#
# OBJECTS is a customized list of modules that are being worked on
# and are present in the current directory

OBJECTS = yaluk_exec2.o \
	   		 	yaluk_init2.o \
		   		em_field_funct.o \
		   		basic_functions.o

# ------------------------

FFLAGS = -Ofast -ffree-form -ffree-line-length-512 -fopenmp -std=gnu -w -march=native

LIBRARY = libyaluk.a

.f.o:
	$(FOR) -c $(FFLAGS) $<
$(LIBRARY) : $(OBJECTS)
	$(AR) rc $(LIBRARY) $(OBJECTS)
