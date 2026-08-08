;;; runbox-tramp-prefix.el --- TRAMP prefix widget and completion -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Antonio Romano

;; Author: Antonio Romano <cidra@posteo.it>
;; Keywords: convenience, internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This does the following:
;; 1) Defines a `runbox-tramp-prefix' type to be used for the
;;    `runbox-environment' custom variable.
;; 2) Defines a completion table to apply to the aforementioned type.
;;
;; This code uses page delimiters.  Use `whitespace-page-delimiters-mode' to see
;; them.
;;; Code:
(require 'tramp)

;;; Completion for TRAMP prefixes.
(defun runbox-tramp-prefix--method-p (str)
  "Non-nil if STR has a complete TRAMP method prefix followed by a colon.

Example: '/ssh:' or '/toolbox:'."
  (declare (side-effect-free t)
           (ftype (function (string) (or null integer))))
  (string-match-p
   (concat tramp-prefix-regexp
           "\\(" tramp-method-regexp "\\)"
           tramp-postfix-method-regexp)
   str))

(defun runbox-tramp-prefix-candidates (str)
  "Return completion candidates for STR, a partial TRAMP prefix."
  (declare (ftype (function (string) list)))
  (let ((non-essential t))
    (if (runbox-tramp-prefix--method-p str)
        (tramp-completion-handle-file-name-all-completions str "")
      (tramp-get-completion-methods nil))))

(add-to-list
 'completion-category-defaults
 '(runbox-tramp-prefix . ((styles basic partial-completion)
                          (cycle . nil)
                          (eager-display . t)
                          (eager-update . t))))

(defun runbox-tramp-prefix-completion-table (string pred action)
  "Completion table for TRAMP method/connection prefixes.

If ACTION is `lambda', this function treats STRING as a valid and
complete match if it is itself a well-formed TRAMP file name.
Completion category is `runbox-tramp-prefix'.  PRED and all other
ACTIONs are delegated to `complete-with-action' over the candidate list."
  (pcase action
    ('lambda (tramp-tramp-file-p string))
    ('metadata '(metadata . ((category . runbox-tramp-prefix))))
    (_ (complete-with-action action
                             (funcall #'runbox-tramp-prefix-candidates
                                      string)
                             string
                             pred))))

;;todo add runbox-tramp-prefix-cape and runbox-tramp-prefix-read(from minibuffer)

(defun runbox-read-tramp-prefix (&optional prompt default no-default)
  "Read a TRAMP prefix from the minibuffer.

PROMPT is used instead of the default prompt if non-nil.  DEFAULT is offered as
the default value unless NO-DEFAULT is non-nil."
  (declare (ftype (function (&optional (or null string)
                                       (or null string)
                                       (or null boolean))
                            (or null string))))
  (interactive)
  (let ((completion-no-auto-exit t))
    (completing-read
     (or prompt "TRAMP prefix: ")
     #'runbox-tramp-prefix-completion-table
     nil
     #'tramp-tramp-file-p
     nil
     'runbox-read-tramp-prefix-history
     (unless no-default default))))

;;; TRAMP prefix widget/type
(require 'wid-edit)
(defun runbox-tramp-prefix--validate (w)
  "Validation function for widget W of type `runbox-tramp-prefix'.

If non valid, widget W with a set `:error' parameter is returned, nil otherwise."
  (let ((v (widget-value w)))
    (if (tramp-tramp-file-p v)
        nil
      (widget-put w :error
                  (format "Not a valid TRAMP prefix: %S" v))
      w)))

(defun runbox-tramp-prefix--capf (&optional widget)
  "Completion-at-point function for `runbox-tramp-prefix' widgets.

  If WIDGET is nil, use the widget at point.  When that widget is of type
  `runbox-tramp-prefix' and point lies within its field, return completion data
  covering the field and marked `:exclusive' so no other CAPF is tried."
  (let ((widget (or widget (widget-at (point)))))
    (when (eq (widget-type widget) 'runbox-tramp-prefix)
      (let ((from (widget-field-start widget))
            (to   (widget-field-end widget)))
        (when (and from to (<= from (point) to))
          (list from to #'runbox-tramp-prefix-completion-table
                '(:exclusive 'yes)))))))

(defun runbox-tramp-prefix--prompt (_widget prompt default no-default)
  "Calls `runbox-read-tramp-prefix' for `runbox-tramp-prefix' widgets.

Returns the dissected tramp prefix.
Passes PROMPT, DEFAULT, NO-DEFAULT."
  (tramp-dissect-file-name
   (runbox-read-tramp-prefix prompt default no-default)))

;; Value-to-internal and value-to-external are kinda opaque to me.  copied
;; verbatim from widget 'symbol and kept it symple
(defun runbox-tramp-prefix--value-to-internal (_widget stored-val)
  "Convert STORED-VAL to its displayed form for `runbox-tramp-prefix'."
  (if (tramp-file-name-p stored-val)
      (tramp-make-tramp-file-name stored-val)
    stored-val))

(defun runbox-tramp-prefix--value-to-external (_widget displayed-val)
  "Convert DISPLAYED-VAL to its persisted form.  Used by `runbox-tramp-prefix'."
  (if (tramp-tramp-file-p displayed-val)
      (let ((vec (tramp-dissect-file-name displayed-val)))
        ;; strip localname as it is ignored anyway
        (setf (tramp-file-name-localname vec) nil))
    displayed-val))

(defun runbox-tramp-prefix--match (_widget val)
  "Checks if VAL is of type `tramp-file-name'.

Match predicate for `runbox-tramp-prefix'."
  (tramp-file-name-p val))

(defun runbox-tramp-prefix--action (&rest _)
  "Widget's `widget-field-activate' behavior.

Action to do when pressing
\\<widget-field-keymap>\\[widget-field-activate] on
`runbox-tramp-prefix' widgets.

Calls `widget-complete', since the default binding <M-TAB> is often
shadowed by the OS."
  (widget-complete))

(defun runbox-tramp-prefix--create (widget)
  "Create WIDGET of type `runbox-tramp-prefix'.

Delegates to `widget-default-create' and adds a
`completion-at-point' function that is scoped to
runbox-tramp-prefix widgets, if not already there."
  (widget-default-create widget)
  (add-hook 'completion-at-point-functions
            #'runbox-tramp-prefix--capf -72 t))

(define-widget 'runbox-tramp-prefix 'editable-field
  "Widget type that maps to a `tramp-file-name' struct.

Widget's internal value (what's shown to user) is a string representing
a TRAMP prefix (e.g. '/toolbox:Fedora:'. Completion and validation for
TRAMP prefixes is provided.

Widget's external value (what's stored in the variable of type
`runbox-tramp-prefix') is a `tramp-file-name' struct."
  :tag "TRAMP Prefix"
  :value nil
  :format "%{%t%}: %v"
  :validate #'runbox-tramp-prefix--validate
  :completions-function #'runbox-tramp-prefix--capf
  :prompt-value #'runbox-tramp-prefix--prompt
  :action #'runbox-tramp-prefix--action
  :create #'runbox-tramp-prefix--create
  :value-to-internal #'runbox-tramp-prefix--value-to-internal
  :value-to-external #'runbox-tramp-prefix--value-to-external)

(provide 'runbox-tramp-prefix)
;;; runbox-tramp-prefix.el ends here
