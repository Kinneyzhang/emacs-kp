# Makefile for EKP C dynamic module
#
# Copyright (C) 2024-2026 Kinney Zhang
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Builds ekp.so (or ekp.dylib on macOS) for Emacs dynamic loading.
# Uses pthread for multi-threading, optimizes for native CPU.

# Detect OS
ifeq ($(OS),Windows_NT)
    UNAME := Windows
else
    UNAME := $(shell uname 2>/dev/null || echo Unknown)
endif

# Compiler settings
# Default to gcc on Windows if cc is not found
ifeq ($(UNAME), Windows)
    CC = gcc
    # Point EMACS_ROOT at your Emacs installation (the directory that
    # contains include/emacs-module.h), e.g.:
    #   make EMACS_ROOT=C:/path/to/emacs-30.2
    EMACS_ROOT ?=
    ifeq ($(EMACS_ROOT),)
        $(warning EMACS_ROOT is not set; emacs-module.h may not be found. \
Set it to your Emacs installation directory.)
    endif
    EMACS ?= emacs.exe
else
    CC ?= cc
    EMACS ?= emacs
endif

PROFILE ?= portable
COMMON_CFLAGS := -std=c11 -Wall -Wextra -Wpedantic -fPIC
COMMON_CFLAGS += -D_POSIX_C_SOURCE=200809L

ifeq ($(PROFILE),portable)
    PROFILE_CFLAGS := -O3
else ifeq ($(PROFILE),native)
    PROFILE_CFLAGS := -O3 -march=native -flto
else ifeq ($(PROFILE),debug)
    PROFILE_CFLAGS := -O0 -g3
else ifeq ($(PROFILE),sanitize)
    PROFILE_CFLAGS := -O1 -g3 -fno-omit-frame-pointer
    PROFILE_CFLAGS += -fsanitize=address,undefined
else
    $(error Unknown PROFILE '$(PROFILE)'; use portable, native, debug, or sanitize)
endif

CFLAGS := $(COMMON_CFLAGS) $(PROFILE_CFLAGS)

# Platform-specific settings
ifeq ($(UNAME), Darwin)
    # macOS
    MODULE_EXT := dylib
    LDFLAGS := -shared -lpthread
    # Find Emacs headers (check Emacs.app, homebrew, /usr/local)
    EMACS_INCLUDE := $(shell find /Applications/Emacs.app/Contents/Resources/include /opt/homebrew /usr/local -name emacs-module.h -exec dirname {} \; 2>/dev/null | head -1)
    ifdef EMACS_INCLUDE
        CFLAGS += -I$(EMACS_INCLUDE)
    endif
else ifeq ($(UNAME), Linux)
    # Linux
    MODULE_EXT := so
    LDFLAGS := -shared -lpthread -lm
else ifeq ($(UNAME), Windows)
    # Windows (MinGW)
    MODULE_EXT := dll
    LDFLAGS := -shared -lpthread
    CFLAGS += -I$(EMACS_ROOT)/include
else
    # Fallback / Unknown
    MODULE_EXT := so
    LDFLAGS := -shared -lpthread
endif

# Source files
SRCS := ekp.c ekp_kp.c ekp_thread_pool.c
OBJS := $(SRCS:.c=.o)

# Output
MODULE := ekp.$(MODULE_EXT)

# Targets
.PHONY: all clean install test

all: $(MODULE)

$(MODULE): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
	@rm -f $(OBJS)
	@echo "Built $@"

%.o: %.c ekp_module.h
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJS) $(MODULE)

# Install to Emacs load path (customize DESTDIR as needed)
DESTDIR ?= $(HOME)/.emacs.d/modules
install: $(MODULE)
	@mkdir -p $(DESTDIR)
	cp $(MODULE) $(DESTDIR)/
	@echo "Installed to $(DESTDIR)/$(MODULE)"

# Test with Emacs
test: $(MODULE)
	$(EMACS) -Q --batch \
		-L . \
		--eval '(module-load (expand-file-name "./$(MODULE)"))' \
		--eval '(message "ekp-c version: %s" (ekp-c-version))' \
		--eval '(ekp-c-init)' \
		--eval '(message "ekp-c initialized with %d threads" (ekp-c-thread-count))' \
		--eval '(ekp-c-cleanup)' \
		--eval '(message "Test passed!")'

# Show compiler info
info:
	@echo "CC: $(CC)"
	@echo "CFLAGS: $(CFLAGS)"
	@echo "LDFLAGS: $(LDFLAGS)"
	@echo "MODULE: $(MODULE)"
	@echo "UNAME: $(UNAME)"
	@echo "EMACS: $(EMACS)"
	@echo "PROFILE: $(PROFILE)"
ifdef EMACS_INCLUDE
	@echo "EMACS_INCLUDE: $(EMACS_INCLUDE)"
endif
