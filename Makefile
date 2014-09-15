#!/bin/bash

# Compiler
FC=mpif90
FFLAGS=-g

# Directories
PWD    = $(shell pwd)
OBJDIR = $(PWD)/obj
SRCDIR = $(PWD)/src

SACLIBDIR=$(SACHOME)/lib

ASDF_LIBDIR=/home/lei/bin/asdf_util/lib
ASDF_INCDIR=/home/lei/bin/asdf_util/include

ADIOS_FLIB=$(shell adios_config -lf)
ADIOS_INC=$(shell adios_config -cf)

# Libraries
LIBS = -lsacio -lsac -lm -lasdf

# Files and folders
TARGET = sac_to_asdf
_OBJ = seismo_var.o main_subs.o main.o

# Make all
all: $(MKOBJDIR) $(TARGET)

OBJ = $(patsubst %,$(OBJDIR)/%,$(_OBJ))

$(MKOBJDIR):
	mkdir -p $(OBJDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) -c -o $@ $< $(FFLAGS) -module $(OBJDIR) -I$(ASDF_INCDIR)

$(TARGET) : $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) -L$(SACLIBDIR) -L$(ASDF_LIBDIR) $(LIBS) $(ADIOS_FLIB)

.PHONY: clean

clean:
	rm -f $(OBJDIR)/*.f90 $(OBJDIR)/*.o $(OBJDIR)/*.mod core.* file_list response_file $(TARGET)
