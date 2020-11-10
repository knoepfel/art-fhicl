art-fhicl-mode (0.4)
==============

Commentary:
-----------

This is a major mode for editing files FHiCL files that are used with
the art framework.  It has rudimentary commenting features and
indentation support.  The syntax highlighting is done such that
strings reserved to/used by FHiCL or art are placed in either:

- constant-face,
- keyword-face,
- function-name-face, or
- type-face

depending on the priority of the string.

Installation:
-------------

Place the `art-fhicl-mode.el` file in a directory `<my_dir>`, and add
the following commands to your `.emacs` file:

```.lisp
(add-to-list 'load-path "<my_dir>")
(load "art-fhicl-mode")
```

Known Bugs:
-----------


(1) Strings that start in a comment take precedence.  For example, the following line:

```
# here is a comment with a "string
```

could cause a problem if not terminated in a subsequent comment line.

(2) For lines using multiple assignments and the modified binding operators (e.g.):

```
here @protect_error: are   two: assignments
```

The "two" name will not be highlighted correctly.
