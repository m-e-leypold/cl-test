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

BEN := $(dir $(lastword $(MAKEFILE_LIST)))

include $(BEN)/prolog.mk

ASD-FILE := $(wildcard *.asd)

ifeq ($(words $(strip $(ASD-FILE))),1)
else
$(error No or more than one .asd file: $(ASD-FILE)) 
endif

PRODUCT-NAME = $(lastword $(subst ., ,$(ASD-FILE:%.asd=%)))
SYSTEM-NAME  = $(ASD-FILE:%.asd=%)

include $(BEN)/common/project.mk

ALL-LISP-IMPLEMENTATIONS ?= sbcl ecl ccl cmucl clisp

LISP-IMPLEMENTATION ?= $(firstword $(ALL-LISP-IMPLEMENTATIONS))
LISP-IMPLEMENTATION := $(strip $(LISP-IMPLEMENTATION))

$(info )
$(info SYSTEM-NAME             = $(SYSTEM-NAME))
$(info ASD-FILE                = $(ASD-FILE))
$(info LISP-IMPLEMENTATION     = $(LISP-IMPLEMENTATION))
$(info ALL-LISP-IMPLEMENTATION = $(ALL-LISP-IMPLEMENTATIONS))


# * Testing --------------------------------------------------------------------

LISP-TEST-RUNNER ?= $(wildcard test.lisp)

$(info LISP-TEST-RUNNER        = $(LISP-TEST-RUNNER))

ifneq ($(strip $(LISP-TEST-RUNNER)),)

check-with-%::
	: Run tests written in lisp.
	$(SET-SH)
	$(LISP-$*-RUN-TEST)
	:

full-check:: $(ALL-LISP-IMPLEMENTATIONS:%=check-with-%)
quick-check:: check-with-$(LISP-IMPLEMENTATION)
check:: check-with-$(LISP-IMPLEMENTATION)

endif

# * Implementations  -----------------------------------------------------------

# TODO: Technically the LISP-*-RUN-TEST should exist dependend on test status.
#       But also when loading TEST-RUNNER we do not want to exit even on failure,
#       at least not alway, when we're in slime.
#
#       This needs some general research to understand how we want the
#       interface of TEST-RUNNER to work as general as possible
#       (possibility would be command line parameters or environment
#       variable) if conditions alone do not suffice.

# ** SBCL  ---------------------------------------------------------------------

LISP-sbcl-RUN-TEST = \
	sbcl --noinform --disable-debugger --load $(LISP-TEST-RUNNER) --quit

# ** ECL  ----------------------------------------------------------------------

LISP-ecl-RUN-TEST = \
	ecl -q --shell $(LISP-TEST-RUNNER)

# ** CMUCL  --------------------------------------------------------------------

LISP-cmucl-RUN-TEST = \
	cmucl  -load $(LISP-TEST-RUNNER) -eval "(quit)"

# ** CLISP  --------------------------------------------------------------------

CLISP-INIT-FILE = $(HOME)/.clisprc.lisp

LISP-clisp-RUN-TEST = \
	clisp --silent -i $(CLISP-INIT-FILE) $(LISP-TEST-RUNNER)

# ** CCL -----------------------------------------------------------------------

LISP-ccl-RUN-TEST = ccl  --load $(LISP-TEST-RUNNER) --eval '(quit)'

# * More modules from 'common' -------------------------------------------------

include $(BEN)/common/git.mk
include $(BEN)/epilog.mk
