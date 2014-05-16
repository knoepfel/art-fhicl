;;; art-fhicl-mode.el --- Major mode for editing FHiCL files used with
;;; the art framework

;; Copyright (C) 2014 Fermilab

;; Author: Kyle Knoepfel <knoepfel@fnal.gov>
;;
;; Keywords: art fhicl framework 
;; Version: 0.0.1

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is a major mode for editing files FHiCL files that are used
;; with the art framework.  It has rudimentary commenting features and
;; indentation support.  The syntax highlighting is done such that
;; strings reserved to/used by FHiCL or art are placed in either:
;;
;;   constant-face,
;;   keyword-face,
;;   function-name-face, or
;;   type-face
;;
;; depending on the priority of the string (according to the author's
;; understanding of FHiCL and art).  Very little attempt has been made
;; to optimize the syntax below, due to the author's very limited
;; knowledge of elisp.
;;
;; This mode was developed with help from Andrei Gaponenko (Fermilab).

;;; Installation:
;;
;; The following must be added to your .emacs file:
;;
;;    (load "art-fhicl-mode.el" nil t t)
;;    (add-to-list 'auto-mode-alist '("\\.fcl$" . art-fhicl-mode))

;;; Known Bugs:
;;
;; (1) This mode does not currently support syntax highlighting for
;;     bracketed constructs like:
;;
;;        list[0]: a.v
;;
;;     which is supported by FHiCL.
;;
;; (2) The highlighting for "process_name", "source", "services",
;;     "physics", and "outputs" is keyword-face ONLY if these identifiers
;;     occur at the start of a line.  If the indentation facility is
;;     enabled, this happens automatically.

(defvar art-fhicl-mode-hook nil)

;; define several classes of keywords
(setq art-fhicl-reserved '("RootInput"
                           "RootOutput"
                           "EmptyEvent"
                           "true"
                           "false"
                           "infinity" ) )
(setq art-fhicl-module-type '("module_type") )
(setq art-fhicl-keywords-prolog '( "BEGIN_PROLOG" 
                                   "END_PROLOG") )

;; create regex string for each class of keywords
(setq art-fhicl-reserved-regexp (regexp-opt art-fhicl-reserved 'words))
(setq art-fhicl-module-type-regexp (regexp-opt art-fhicl-module-type 'words))
(setq art-fhicl-keywords-prolog-regexp (regexp-opt art-fhicl-keywords-prolog 'words))
(setq art-fhicl-keyword-nil-regexp (regexp-quote "@nil"))
(setq art-fhicl-keyword-local-regexp (regexp-quote "@local"))
(setq art-fhicl-keyword-db-regexp (regexp-quote "@db"))

;; create the list for font-lock.
(setq art-fhicl-font-lock-keywords
      `(
	;; comment-face
        ;; ;; uses: match . highlighter expression
	(,"\\(?:^\\|\\s-*\\)\\(#[^include]\\|//\\).*$" . (0 font-lock-comment-face t ))
	;; keyword-face
	(,"^process_name[^\\._[:alnum:]]" . font-lock-keyword-face)
	(,"^source[^\\._[:alnum:]]" . font-lock-keyword-face)
	(,"^services[^\\._[:alnum:]]" . font-lock-keyword-face)
	(,"^physics[^\\._[:alnum:]]" . font-lock-keyword-face)
	(,"^outputs[^\\._[:alnum:]]" . font-lock-keyword-face)
	(,"trigger_paths[^\\._[:alnum:]]" . font-lock-keyword-face)
	(,"end_paths[^\\._[:alnum:]]" . font-lock-keyword-face)
	;; function-name-face
	(,"[[:space:]]producers[^\\._[:alnum:]]" . font-lock-function-name-face)
	(,"[[:space:]]analyzers[^\\._[:alnum:]]" . font-lock-function-name-face)
	(,"[[:space:]]filters[^\\._[:alnum:]]" . font-lock-function-name-face)
	(,"[[:space:]]user[^\\._[:alnum:]]" . font-lock-function-name-face)
	(,"[[:space:]]message[^\\._[:alnum:]]" . font-lock-function-name-face)
	(,"[[:space:]]TFileService[^\\._[:alnum:]]" . font-lock-function-name-face)
	(,"[[:space:]]RandomNumberGenerator[^\\._[:alnum:]]" . font-lock-function-name-face)
	;; type-face
	(,art-fhicl-module-type-regexp . font-lock-type-face )
	(,"[[:space:]]maxEvents[^\\._[:alnum:]]" . font-lock-type-face )
        (,"[[:space:]]skipEvents[^\\._[:alnum:]]" . font-lock-type-face )
	(,"[[:space:]]SelectEvents[^\\._[:alnum:]]" . font-lock-type-face )
	(,"[[:space:]]outputCommands[^\\._[:alnum:]]" . font-lock-type-face )
	(,"[[:space:]]fileNames?[^\\._[:alnum:]]" . font-lock-type-face)
	;; builtin-face
        (,"^#include[[:space:]]" . font-lock-builtin-face )
	(,art-fhicl-keywords-prolog-regexp . font-lock-builtin-face )
	;; constant-face
	(,art-fhicl-reserved-regexp   . font-lock-constant-face)
	(,art-fhicl-keyword-nil-regexp . font-lock-constant-face )
	(,art-fhicl-keyword-local-regexp . font-lock-constant-face )
	(,art-fhicl-keyword-db-regexp . font-lock-constant-face )
	;; variable-face
	(,"\\([[:space:]][[:alnum:]\\._]+[[:space:]]*\\):" 
         (1 font-lock-variable-name-face ))))

;; syntax table
(defvar art-fhicl-syntax-table nil "Syntax table for `art-fhicl-mode'.")
(setq art-fhicl-syntax-table
      (let ((synTable (make-syntax-table)))
	synTable))

;; indentation
(defvar art-fhicl-indent-offset 3
  "*Indentation offset for `art-fhicl-mode'.")

(defun art-fhicl-indent-line() 
  "Indent current line for `art-fhicl-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
	  (while t
	    (backward-up-list 1)
	    (when (looking-at "[[{]")
	      (setq indent-col (+ indent-col art-fhicl-indent-offset))))
	(error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[]}]") (>= indent-col art-fhicl-indent-offset))
	(setq indent-col (- indent-col art-fhicl-indent-offset))))
    (indent-line-to indent-col)))

;; define the major mode.
(define-derived-mode art-fhicl-mode fundamental-mode
  "art-fhicl-mode is a major mode for editing language art-fhicl."
  :syntax-table art-fhicl-syntax-table

  (make-local-variable 'art-fhicl-indent-offset)
  (set (make-local-variable 'indent-line-function) 'art-fhicl-indent-line)
  (setq font-lock-defaults '(art-fhicl-font-lock-keywords))
  (setq major-mode 'art-fhicl-mode)
  (setq mode-name "art-fhicl")
  (run-hooks 'art-fhicl-mode-hook))

(provide 'art-fhicl-mode)

;; -- end of file
