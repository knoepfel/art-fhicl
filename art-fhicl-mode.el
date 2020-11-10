;;; art-fhicl-mode.el --- Major mode for editing FHiCL files used with
;;; the art framework

;; Copyright (C) 2014 Fermilab

;; Author: Kyle Knoepfel <knoepfel@fnal.gov>
;;
;; Keywords: art fhicl framework
;; Version: 0.4.0

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
;;    (load "art-fhicl-mode")

;;; Known Bugs:
;;
;; (1) The highlighting for "process_name", "source", "services",
;;     "physics", and "outputs" is keyword-face ONLY if these identifiers
;;     occur at the start of a line.  If the indentation facility is
;;     enabled, this happens automatically.

(defvar art-fhicl-mode-hook nil)

;; define several classes of keywords
(setq art-fhicl-reserved '("true"
                           "false"
                           "infinity"))

;; create regex string for each class of keywords
(setq art-fhicl-reserved-regexp (regexp-opt art-fhicl-reserved 'words))
(setq art-fhicl-keyword-nil-regexp (regexp-quote "@nil"))

;; create the list for font-lock.
(setq art-fhicl-font-lock-keywords
      `(
        ;; comment-face
        (,"\\(?:^\\|\\s-*\\)\\(#\\|//\\).*$" . (0 font-lock-comment-face t ))
        ;; keyword-face
        (,"^\\(process_name\\)[[:space:]]*):" . (1 font-lock-keyword-face))
        (,"^\\(source\\)[[:space:]]*:" . (1 font-lock-keyword-face))
        (,"^\\(services\\)[[:space:]]*:" .(1 font-lock-keyword-face))
        (,"^\\(physics\\)[[:space:]]*:" . (1 font-lock-keyword-face))
        (,"^\\(outputs\\)[[:space:]]*:" . (1 font-lock-keyword-face))
        (,"^[[:space:]]+\\(trigger_paths\\)[[:space:]]*:*" . (1 font-lock-keyword-face))
        (,"^[[:space:]]+\\(end_paths\\)[[:space:]]*:" . (1 font-lock-keyword-face))
        ;; function-name-face
        (,"^[[:space:]]+\\(producers\\)[[:space:]]*:" . (1 font-lock-function-name-face))
        (,"^[[:space:]]+\\(analyzers\\)[[:space:]]*:" . (1 font-lock-function-name-face))
        (,"^[[:space:]]+\\(filters\\)[[:space:]]*:" . (1 font-lock-function-name-face))
        ;; builtin-face
        (,"^\\(^#include[[:space:]]\\)\\(\".*\"\\)" (1 font-lock-builtin-face t) (2 font-lock-string-face t) )
        (,"^BEGIN_PROLOG[[:space:]]" . font-lock-builtin-face)
        (,"^END_PROLOG[[:space:]]" . font-lock-builtin-face)
        ;; constant-face
        (,"\\(@local\\|@sequence\\|@table\\|@id\\)::" . (1 font-lock-constant-face))
        (,art-fhicl-reserved-regexp   . font-lock-constant-face)
        (,art-fhicl-keyword-nil-regexp . font-lock-constant-face )
        ;; variable-name-face
        (,"\\(^\\|[[:space:]]\\)\\([[:alnum:]\\._]+[[:space:]]*\\)\\(\\[[0-9]*\\]\\)*[[:space:]]*:"
         (2 font-lock-variable-name-face ))))

;; syntax table
(defvar art-fhicl-syntax-table nil "Syntax table for `art-fhicl-mode'.")
(setq art-fhicl-syntax-table
      (let ((synTable (make-syntax-table)))
        synTable))

;; indentation
(defvar art-fhicl-indent-offset 2
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

;; associate .fcl files with art-fhicl-mode
(add-to-list 'auto-mode-alist '("\\.fcl$" . art-fhicl-mode))

(provide 'art-fhicl-mode)
