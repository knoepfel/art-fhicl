==============
art-fhicl-mode
==============

Commentary:
-----------

 This is a major mode for editing files FHiCL files that are used
 with the art framework.  It has rudimentary commenting features and
 indentation support.  The syntax highlighting is done such that
 strings reserved to/used by FHiCL or art are placed in either:

   constant-face,
   keyword-face,
   function-name-face, or
   type-face

 depending on the priority of the string (according to the author's
 understanding of FHiCL and art).  Very little attempt has been made
 to optimize the syntax below, due to the author's very limited
 knowledge of elisp.

 This mode was developed with help from Andrei Gaponenko (Fermilab).

Installation:
-------------

 The following must be added to your .emacs file:

    (load "art-fhicl-mode.el" nil t t)
    (add-to-list 'auto-mode-alist '("\\.fcl$" . art-fhicl-mode))

Known Bugs:
-----------

 (1) The highlighting for "process_name", "source", "services",
     "physics", and "outputs" is keyword-face ONLY if these identifiers
     occur at the start of a line.  If the indentation facility is
     enabled, this happens automatically.

 (2) The character '#' or sequence of characters "//", when appearing in a string literal will be parsed as the beginning of a comment.
