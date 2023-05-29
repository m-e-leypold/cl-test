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

LISP-IMPLEMENTATION ?= sbcl
LISP-IMPLEMENTATION := $(strip $(LISP-IMPLEMENTATION))

# * Testing --------------------------------------------------------------------

LISP-TEST-RUNNER ?= $(wildcard test.lisp)

ifneq ($(strip $(LISP-TEST-RUNNER)),)
check::
	: Run tests written in lisp.
	$(SET-SH)
	$(LISP-$(LISP-IMPLEMENTATION)-RUN-TEST)
	:
endif

# * Implementations  -----------------------------------------------------------

LISP-sbcl-RUN-TEST = \
	sbcl --noinform --disable-debugger --load $(LISP-TEST-RUNNER) --quit

# * More modules from 'common' -------------------------------------------------

include $(BEN)/common/project.mk
include $(BEN)/common/git.mk
include $(BEN)/epilog.mk
