SHELL=/bin/bash
RM=rm -f

CC  ?= gcc
CXX ?= g++

#
# Configure gcc
#
ifeq ($(findstring g++,$(CXX)),g++)
    GCC_VERSION = $(shell $(CXX) -v 2>&1 | grep -oP "gcc version ..." | sed "s/gcc version //")

    WARN = -Wall -Wextra \
           -Wcast-align -Wcast-qual -Wconversion -Wignored-qualifiers -Wmissing-field-initializers \
           -Wno-pmf-conversions -Wnoexcept -Wold-style-cast -Woverloaded-virtual -Wpointer-arith \
           -Wsign-compare -Wstrict-null-sentinel -Wtype-limits -Wunused-but-set-parameter \
           -Wuseless-cast -Wwrite-strings -Wzero-as-null-pointer-constant
    
    ifeq ($(shell [[ "$(GCC_VERSION)" > "4.9" ]] && echo 1),1)
        WARN += -Wsuggest-override -Wsuggest-final-types -Wsuggest-final-methods
    endif
endif

#
# Configure clang
#
ifeq ($(findstring clang,$(CXX)),clang)
    CLANG_VERSION=$(shell $(CXX) -v 2>&1 | grep -oP "clang version ..." | sed "s/clang version //")

    WARN = -Weverything -Wno-c++98-compat -Wno-c++98-c++11-compat-pedantic -Wno-padded

    ifdef SANITIZE
        SANITIZE += -fsanitize-blacklist=./CI/blacklist.txt -fno-omit-frame-pointer
    endif
endif

CPPFLAGS = -g -std=c++1y -isystem "$(INCLUDE)" $(COVERAGE) $(SANITIZE) $(WARN)
LDFLAGS  = -g
LDLIBS   =

SRCS=$(wildcard *.cpp)
OBJS=$(subst .cpp,.o,$(SRCS))

all: bktga

bktga: $(OBJS)
	$(CXX) $(LDFLAGS) $(COVERAGE) $(SANITIZE) -o bktga_test $(OBJS) $(LDLIBS) 

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CPPFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS)

dist-clean: clean
	$(RM) *~ .depend

include .depend
