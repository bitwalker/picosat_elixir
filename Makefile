CWD := $(shell pwd)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)])." -s erlang halt)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)])." -s erlang halt)

C_SRC_DIR = $(CWD)/c_src
C_SRC_OUTPUT = $(CWD)/priv/picosat_nif.so

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall
	LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
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
