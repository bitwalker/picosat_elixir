CWD := $(shell pwd)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)

C_SRC_DIR = $(CWD)/c_src
C_SRC_OUTPUT = $(CWD)/priv/picosat_nif.so

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CFLAGS ?= -std=c99 -finline-functions -Wall
	LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CFLAGS ?= -std=c99 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CFLAGS ?= -std=c99 -finline-functions -Wall
endif

MIX_ENV ?= dev
# Optimize if MIX_ENV=prod
ifeq ($(MIX_ENV), prod)
	CFLAGS += -O3 -DNDEBUG
# Otherwise, if gcc, then build with full debug symbols
else ifneq (,$(findstring gcc, $(CC)))
	CFLAGS += -g3 -ggdb
# If not gcc, do the same, but more conservatively
else
	CFLAGS += -g
endif
CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR)
LDFLAGS += -shared

all: $(C_SRC_OUTPUT)

clean:
	rm -f $(C_SRC_OUTPUT)
	rm -f $(C_SRC_DIR)/*.o

analyze:
	clang --analyze $(CFLAGS) $(C_SRC_DIR)/*.{c,h}

$(C_SRC_OUTPUT): $(C_SRC_DIR)/picosat.o $(C_SRC_DIR)/picosat_nif.o
	@mkdir -p $(CWD)/priv
	$(CC) $(LDFLAGS) $(LDLIBS) $(C_SRC_DIR)/picosat_nif.o $(C_SRC_DIR)/picosat.o -o $(C_SRC_OUTPUT)

$(C_SRC_DIR)/picosat.o: $(C_SRC_DIR)/picosat.c $(C_SRC_DIR)/picosat.h
	$(CC) $(CFLAGS) -o $(C_SRC_DIR)/picosat.o -c $<

.PHONY: all clean
