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

GIT-SLUG        ?= $(PRODUCT-NAME)
GIT-PUBLIC-USER ?= $(USER)
GIT-REMOTES     ?= $(GIT-UPSTREAMS) origin

GIT-PUBLIC-USER := $(strip $(GIT-PUBLIC-USER))
GIT-REMOTES     := $(strip $(GIT-REMOTES))

$(info )
$(info GIT-SLUG        = $(GIT-SLUG))
$(info GIT-PUBLIC-USER = $(GIT-PUBLIC-USER))
$(info GIT-REMOTES     = $(GIT-REMOTES))

GIT-GITLAB.URL = git@gitlab.com:$(GIT-PUBLIC-USER)/$(GIT-SLUG).git
GIT-GITHUB.URL = git@github.com:$(GIT-PUBLIC-USER)/$(GIT-SLUG).git
GIT-origin.URL = $(shell git remote get-url origin)

.PHONY: git-setup

git-setup: $(patsubst %,git-setup.%,$(filter-out origin,$(GIT-REMOTES)))   # we do not need to setup origin!

git-setup.%:
	git remote rm $*
	git remote add $* $(GIT-$*.URL)
	git fetch $*


setup:: git-setup

