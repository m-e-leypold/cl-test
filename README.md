cl-test -- Another test framework for common lisp
=================================================

(WIP)

Documentation
-------------

Documentation of this system is available as extensively linked
documentation strings in the packages themselves. *Slime* here
functions as hypertext facility using `C-c C-d C-d`
(`slime-describe-symbol`).

In order to read the documentation you need

- Emacs.
- Load *slime* and start a lisp REPL (only SBCL works these days).
- Load "tests.lisp" or "load.lisp" with `C-c C-l`
  (`slime-load-file`). It is recommended to run the tests at the
  beginning so you know everything is working with your platform
  properly.
- Then continue with the package documentation string as described below.

In the following block, position the cursor on `*DOCUMENTATION*` and run `M-x
slime-describe-symbol`. This should open a `*slime-description*`
buffer with the package documentation string from where you can follow
to the referenced functions and variables.

```common-lisp
	(in-package :de.m-e-leypold.cl-test)
    *documentation*  ;; M-x slime-describe-symbol on this
```

In the above block the `in-package` stanzas exist only for the benefit
of *slime* as a hint in which package to look up `*documentation*`.


License
-------

    cl-test -- another test framework for common lisp.
    Copyright (C) 2022  M E Leypold
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    For altermative licensing options, see README.md

See [LICENSE.md](./LICENSE.md) for the full license text.


