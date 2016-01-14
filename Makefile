ifndef CC
    CC=gcc
endif

ifndef CXX
    CXX=g++
endif

RM=rm -f

ifeq ($(findstring g++,$(CXX)),g++)
    WARN=-Wall -Wnoexcept -Wold-style-cast -Wstrict-null-sentinel -Woverloaded-virtual -Wno-pmf-conversions -Wignored-qualifiers -Wunused-but-set-parameter -Wpointer-arith -Wtype-limits -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wzero-as-null-pointer-constant -Wuseless-cast -Wsign-compare -Wmissing-field-initializers
    SANITIZE=-fsanitize=address,undefined
endif
ifeq ($(findstring clang,$(CXX)),clang)
    WARN=-Weverything -Wno-c++98-compat -Wno-c++98-c++11-compat-pedantic
    SANITIZE=
endif

CPPFLAGS=-g -Wall -std=c++1y -isystem "$(INCLUDE)" $(SANITIZE) $(WARN)
LDFLAGS=-g
LDLIBS=

SRCS=$(wildcard *.cpp)
OBJS=$(subst .cpp,.o,$(SRCS))

all: bkassert

bkassert: $(OBJS)
	$(CXX) $(LDFLAGS) $(SANITIZE) -o bkassert_test $(OBJS) $(LDLIBS) 

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CPPFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS)

dist-clean: clean
	$(RM) *~ .depend

include .depend
