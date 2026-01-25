# Makefile for EKP C dynamic module
#
# Builds ekp.so (or ekp.dylib on macOS) for Emacs dynamic loading.
# Uses pthread for multi-threading, optimizes for native CPU.

# Detect OS
UNAME := $(shell uname)

# Compiler settings
CC := cc
CFLAGS := -std=c11 -Wall -Wextra -Wpedantic -O3 -fPIC
CFLAGS += -march=native -flto
CFLAGS += -D_POSIX_C_SOURCE=200809L

# Debug build
ifdef DEBUG
CFLAGS := -std=c11 -Wall -Wextra -Wpedantic -O0 -g -fPIC
CFLAGS += -fsanitize=address,undefined
endif

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
else
    # Windows (MinGW)
    MODULE_EXT := dll
    LDFLAGS := -shared -lpthread
endif

# Source files
SRCS := ekp.c ekp_kp.c ekp_hyphen.c ekp_paragraph.c ekp_thread_pool.c
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
	emacs -Q --batch \
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
ifdef EMACS_INCLUDE
	@echo "EMACS_INCLUDE: $(EMACS_INCLUDE)"
endif
