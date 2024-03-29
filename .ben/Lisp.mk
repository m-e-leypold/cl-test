# BEN - Build Everything Now
# Copyright (C) 2023  M E Leypold
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

BEN := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

include $(BEN)/prolog.mk

ASD-FILE := $(wildcard *.asd)

ifeq ($(words $(strip $(ASD-FILE))),1)
else
$(error No or more than one .asd file: $(ASD-FILE)) 
endif

PRODUCT-NAME = $(lastword $(subst ., ,$(ASD-FILE:%.asd=%)))
SYSTEM-NAME  = $(ASD-FILE:%.asd=%)

include $(BEN)/common/project.mk

ALL-LISP-IMPLEMENTATIONS ?= sbcl ecl clasp ccl cmucl clisp abcl mkcl

SUPPORTED-LISP-IMPLEMENTATIONS ?= \
	$(filter-out $(UNSUPPORTED-LISP-IMPLEMENTATIONS), \
                     $(ALL-LISP-IMPLEMENTATIONS))

LISP-IMPLEMENTATION ?= $(firstword $(SUPPORTED-LISP-IMPLEMENTATIONS))
LISP-IMPLEMENTATION := $(strip $(LISP-IMPLEMENTATION))

$(info )
$(info SYSTEM-NAME             = $(SYSTEM-NAME))
$(info ASD-FILE                = $(ASD-FILE))
$(info LISP-IMPLEMENTATION     = $(LISP-IMPLEMENTATION))
$(info ALL-LISP-IMPLEMENTATION = $(SUPPORTED-LISP-IMPLEMENTATIONS))


# * Testing --------------------------------------------------------------------

LISP-TEST-RUNNER ?= $(wildcard test.lisp)

$(info LISP-TEST-RUNNER        = $(LISP-TEST-RUNNER))

ifneq ($(strip $(LISP-TEST-RUNNER)),)

check-with-%::
	: -- Run tests written in lisp with $* --
	$(SET-SH)
	mkdir -p .build/log
	$(LISP-$*-RUN-TEST) 2>&1 | tee .build/log/$@.log
	:

full-check:: $(SUPPORTED-LISP-IMPLEMENTATIONS:%=check-with-%)
quick-check:: check-with-$(LISP-IMPLEMENTATION)
check:: check-with-$(LISP-IMPLEMENTATION)

endif


# * Check for warnings (currently SBCL only) -----------------------------------

LISP-IMPLEMENTATIONS-SUPPORTED-FOR-WARNINGS = sbcl

ifeq ($(strip $(filter \
        $(LISP-IMPLEMENTATION),$(LISP-IMPLEMENTATIONS-SUPPORTED-FOR-WARNINGS))),)
check-warnings::
else
check-warnings:: check-warnings-$(LISP-IMPLEMENTATION)
endif

check-warnings-%::
	: -- Checking for warnings with $* --
	$(SET-SH)
	$(BEN)/clear-user-cache $*
	$(LISP-$*-LOAD-SYSTEM) "$(SYSTEM-NAME)/prerequisites"
	mkdir -p .build
	$(LISP-$*-LOAD-SYSTEM) "$(SYSTEM-NAME)/load" 2> .build/lisp-warnings
	grep -A1 '; caught ' .build/lisp-warnings || true
	test 0 -eq "$$(grep '; caught ' .build/lisp-warnings | wc -l)"
	:

check:: check-warnings

# * Implementations  -----------------------------------------------------------

# TODO: Technically the LISP-*-RUN-TEST should exit dependend on test status.
#       But also when loading TEST-RUNNER we do not want to exit even on failure,
#       at least not alway, when we're in slime.
#
#       This needs some general research to understand how we want the
#       interface of TEST-RUNNER to work as general as possible
#       (possibility would be command line parameters or environment
#       variable) if conditions alone do not suffice.

# TODO: The interface from makefile to specific lisp implementations
#       needs to be overhauled. It is much clearer now how that has to
#       work:
#
#       Always call a driver script (eg sbcl-driver) to perform
#       specific operations.
#
#       $ sbcl-driver load-system ...
#       $ sbcl-driver clear-user-cache ...
#       $ sbcl-driver parse-warnings

# ** SBCL  ---------------------------------------------------------------------

LISP-sbcl-RUN-TEST = \
	sbcl --noinform --disable-debugger --load $(LISP-TEST-RUNNER) --quit

LISP-sbcl-LOAD-SYSTEM = \
        _load_system() { \
	   sbcl --noinform --disable-debugger \
                --eval "(asdf:operate 'asdf:load-op "'"'"$$1"'"'")" --quit ; };\
        _load_system


# ** ECL  ----------------------------------------------------------------------

LISP-ecl-RUN-TEST = \
	ecl -q --shell $(LISP-TEST-RUNNER)

# ** CMUCL  --------------------------------------------------------------------

LISP-cmucl-RUN-TEST = \
	cmucl  -quiet -batch -load $(LISP-TEST-RUNNER) -eval "(quit)"

# ** CLISP  --------------------------------------------------------------------

CLISP-INIT-FILE = $(HOME)/.clisprc.lisp

LISP-clisp-RUN-TEST = \
	clisp --silent -i $(CLISP-INIT-FILE) $(LISP-TEST-RUNNER)

# ** CCL  ----------------------------------------------------------------------

LISP-ccl-RUN-TEST = ccl -b -Q --load $(LISP-TEST-RUNNER) --eval '(quit)'

# ** ABCL  ---------------------------------------------------------------------

LISP-abcl-RUN-TEST = \
	abcl --noinform --batch \
             --load $(BEN)/abcl-batch.lisp test.lisp $(LISP-TEST-RUNNER)

# ** CLASP  --------------------------------------------------------------------

LISP-clasp-RUN-TEST = clasp --non-interactive --load $(LISP-TEST-RUNNER)

# ** MKCL ----------------------------------------------------------------------

LISP-mkcl-RUN-TEST = mkcl -q -shell $(LISP-TEST-RUNNER)

# * More modules from 'common' -------------------------------------------------

include $(BEN)/common/git.mk
include $(BEN)/epilog.mk
