;;; runbox.el --- [Run] in Tool[Box] (and others) -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Antonio Romano

;; Author: Antonio Romano <cidra@posteo.it>
;; URL: https://github.com/antrmn/runbox.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: processes, convenience

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
;; This defines the main variable and the main logic for Runbox, along with
;; integration for some built-in command such as `compile' and `shell'.
;;
;; This code uses page delimiters ().  Use `whitespace-page-delimiters-mode'
;; or equivalent features to see them.

;;; Code:
;;;
(require 'tramp)
(require 'tramp-sh) ; for `tramp-maybe-open-connection'
(require 'runbox-tramp-prefix)

(defgroup runbox nil
  "[Run] in Tool[Box]."
  :prefix "runbox-"
  :group 'tools)

(defun runbox-help ()
  "Display the documentation for the Runbox feature."
  (interactive)
  (info "(runbox)Runbox"))

;;; Runbox environment definition and safety predicates
;;;###autoload
(defcustom runbox-safe-methods '("toolbox" "distrobox")
  "TRAMP methods deemed safe for file-local `runbox-environment'.

A `tramp-file-name' value whose method is in this list and whose hop
slot is nil is deemed safe to apply automatically."
  :type '(repeat string)
  :risky t)

;;;###autoload
(defun runbox--safe-method-p (val)
  "Return non-nil if VAL is a safe `tramp-file-name' according to its method.

VAL is a `tramp-file-name' struct.  This function checks if VAL's hop
slot is unset and if method slot is set to a value stored in the
`runbox-safe-methods' user variable."
  (declare (ftype (function (list) boolean)))
  (and-let* ((method (tramp-file-name-method val))
             (_ (length= (tramp-file-name-hop val) 0)))
    (member method runbox-safe-methods)))

;;;###autoload
(defcustom runbox-safe-environment-p-functions (list #'runbox--safe-method-p)
  "Functions that decide whether a `runbox-environment' value is safe.

Each function is called with one argument, a `tramp-file-name' value,
and should return non-nil if it is safe to use as a file-local value.

The first function to return non-nil makes the value safe; if none do,
the value is unsafe.  Functions need not handle non-`tramp-file-name'
values, which are rejected before the hook is run."
  :type 'hook
  :risky t)

;;;###autoload
(defun runbox-safe-environment-p (val)
  "Return non-nil if VAL is a safe file-local value for `runbox-environment'.

A nil value is always safe.  A `tramp-file-name' value is safe if one of
the functions in `runbox-safe-environment-p-functions' returns non-nil
for it.  Any other non-nil value is never safe; functions in
`runbox-safe-environment-p-functions' need only handle `tramp-file-name'
values."
  (declare (ftype (function (t) boolean)))
  (or (null val)
      (when (tramp-file-name-p val)
        (run-hook-with-args-until-success
         'runbox-safe-environment-p-functions val))))

;;;###autoload
(defcustom runbox-environment nil
  "Indicates where runbox commands will be routed to.

If set, holds a `tramp-file-name' struct representing a TRAMP prefix
prepended to `default-directory' when invoking a runbox wrapper.  The
LOCALNAME slot is ignored and should be left unset.

To set this variable it is recommended to use the Customize-provided
commands: `customize-variable', `customize-set-value' (to set locally),
`customize-set-variable', `customize-dirlocals'."
  :type '(choice (const :tag "None" nil) runbox-tramp-prefix)
  :safe #'runbox-safe-environment-p
  :require 'runbox
  :local t)

;;; Runbox bind mount definition
(defconst runbox-bind-mount (file-truename "~") ; Let's keep it constant for now
  "Local path visible (bind-mounted) inside the routed environment.

Defaults to the user's home directory.")

(defun runbox-under-bind-mount-p (&optional dir)
  "Return DIR if it is under `runbox-bind-mount'.
Return nil if `runbox-bind-mount' is nil or DIR is not under it.
If unset, DIR defaults to `default-directory'."
  (and-let* ((dir (or dir default-directory)))
    (and runbox-bind-mount
         (file-in-directory-p dir runbox-bind-mount)
         dir)))

;;; Runbox environment labeling
(defun runbox--tool-distro-box-p (vec)
  "Non-nil if VEC is a Toolbox or Distrobox `file-tramp-name'."
  (declare (ftype (function (t) boolean)))
  (or (equal (tramp-file-name-method vec) "toolbox")
      (equal (tramp-file-name-method vec) "distrobox")))

(defcustom runbox-environment-label-formatters
  '((runbox--tool-distro-box-p . tramp-file-name-host))
  "Alist of (PREDICATE-FN . FORMATTER-FN).

Both are functions of one argument, a `tramp-file-name' vector.  The
first entry whose PREDICATE-FN returns non-nil has its FORMATTER-FN
called to produce the label.

This is looked up by `runbox-environment-label'."
  ;; Maybe a :type 'hook would be better?
  :type '(alist :key-type (function :tag "Predicate")
                :value-type (function :tag "Formatter"))
  :risky t
  :group 'runbox)

(defun runbox-environment-label (env)
  "Return a pretty label for the tramp-file-name ENV.

Looks up ENV in `runbox-environment-label-formatters', using the first
formatter whose predicate matches.  If none match, falls back to the
original TRAMP prefix string without the leading slash and trailing
colon.

Signals `wrong-type-argument' if ENV is not a `tramp-file-name'."
  (declare (ftype (function (t) string)))
  (unless (tramp-file-name-p env)
    (signal 'wrong-type-argument (list 'tramp-file-name-p env)))
  (or (and-let* ((fun (assoc-default env
                                     runbox-environment-label-formatters
                                     #'funcall))
                 (_ (funcall fun env))))
      (substring
       (tramp-make-tramp-file-name env "")
       1 -1)))

(defvar runbox--labels-cache (make-hash-table :test 'eq :weakness 'key))
(defun runbox-environment-label-cached (env)
  "Same as `runbox-environment-label', but with result cached by ENV's identity.

Useful for usage in hot loops like mode-line rendering."
  (declare (ftype (function (t) string)))
  (with-memoization (gethash env runbox--labels-cache)
    (runbox-environment-label env)))

;;; Runbox Auto Mode
(defun runbox--runboxify-command ()
  "Converts next command `this-command' to its runbox wrapper equivalent, if any.

Used by `runbox-auto-mode' and `global-runbox-auto-mode'."
  ;; Limitation: only top-level command loop invocation are detected.
  ;; (call-interactively ...) is NOT detected, so is popup-menu and M-x and
  ;; transient.  Advising may be needed. I refrain from doing so for now.  What
  ;; can be done: the user may mark the command dispatcher as a runbox command
  ;; and provide some wrapper for it.
  (when-let* ((_ (null (runbox-invalid-p)))
              (new-cmd (function-get this-command 'runbox-wrapped-by)))
    (setq this-command new-cmd)))

(define-minor-mode runbox-auto-mode
  "Substitute any invoked command with its runbox wrapper, if one exists.

When this mode is on, invoking a command via a keybinding calls its
runbox-wrapped equivalent instead, when one is registered.  For
example, a keybinding bound to `shell' will interactively call
`runbox-shell' instead.

This only applies to commands invoked through top-level command
loop, i.e. keybindings: `M-x', transient menus, and commands
invoked via `x-popup-menu' are not substituted."
  ;;This mode stays enabled even when `runbox-environment' is unset or
  ;;`default-directory' is non-local; a lighter or keymap tied to this mode would
  ;;not indicate whether it is actually in effect.
  :lighter nil
  :keymap nil
  (if runbox-auto-mode
      (add-hook 'pre-command-hook #'runbox--runboxify-command)
    (remove-hook 'pre-command-hook #'runbox--runboxify-command)))

(define-globalized-minor-mode global-runbox-auto-mode
  runbox-auto-mode runbox-auto-mode)

(defun runbox-auto-describe-mode ()
  "Display documentation for `runbox-auto-mode'."
  (interactive)
  (describe-minor-mode-from-symbol 'runbox-auto-mode))

(defun runbox-auto-mode-toggle-at-window (event)
  "Toggle `runbox-auto-mode' in the window where EVENT occurred."
  (declare (interactive-only t))
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (call-interactively #'runbox-auto-mode)))

;;; Runbox Any Command
;; An alternative could be a keymap full of <remap>s?
(defun runbox-any-command ()
  "Run next command with runbox.

Act as a leader key: read the following key sequence and run its runbox
equivalent.  (Or run it, if it is itself a runbox command.)"
  (declare (interactive-only t))
  (interactive)
  (let* ((keys (read-key-sequence nil 'continue-echo))
         (command (key-binding keys)))
    (cond
     ((and command (function-get command 'runbox-wrapped-by))
      (call-interactively (function-get command 'runbox-wrapped-by)))
     ((and command (function-get command 'runbox))
      (call-interactively command))
     ((memq ;; If C-h is the last key, get help
       (aref keys (1- (length keys))) help-event-list)
      ;; Hard-coded! Sorry, which-keys users.
      (describe-bindings (substring keys 0 -1)))
     (t (message "%s is undefined" (key-description keys))))))

;;; Mode line (and menu) definition
(defvar-keymap runbox-menu-map
  :name "Runbox"
  :prefix 'runbox-menu-map
  :doc "General menu for Runbox.  By default used in `runbox-mode-line'."
  "<runbox-help>" '(menu-item "Help for this feature" runbox-help)
  "<separator>" '("--")
  "<runbox-auto-mode>"
  '(menu-item "Auto mode" runbox-auto-mode-toggle-at-window
              :button
              (:toggle . (buffer-local-value
                          'runbox-auto-mode
                          (window-buffer (posn-window
                                          (event-start
                                           last-nonmenu-event)))))))

;; Can't pass a variable containing a function for the help-echo property, so
;; here I am
(defmacro runbox--define-help-echo (name value &optional doc)
  "Define a custom variable for help echo and a function accessor for it.

Both the variable and the function are called NAME.
The function called NAME, when invoked, will evaluate the variable called NAME:

- If it is a function (of 3 arguments, see `(elisp) Special Properties')
  its return value is then returned.
- Otherwise, return it as it is.

VALUE is the default value of the variable called NAME.
DOC is the docstring added to the variable called NAME."
  (declare (doc-string 3)
           (indent defun))
  `(progn
     (defcustom ,name ,value
       ,(concat doc
               "\nIf set, this is either a string or a function of 3 arguments."
               "\n See Info node `(elisp) Special Properties' for more details.")
       :risky t
       :type '(choice (const :tag "None" nil)
                     (string :tag "Static text")
                     (function :tag "Function (window object pos)")))
     (defun ,name (window object pos)
       ,(format
         "Evaluate and return the help-echo string for this indicator.
Defined at variable `%s'."
        name)
       (and ,name
            (risky-local-variable-p ',name)
            (if (functionp ,name)
                (funcall ,name window object pos)
              ,name)))))

(defalias 'runbox--displayable-p (if (fboundp 'char-displayable-on-frame-p)
                                     #'char-displayable-on-frame-p
                                   #'char-displayable-p)
  "Use `char-displayable-on-frame-p' on Emacs 31+")

(defvar-keymap runbox-auto-mode-line-map
  :prefix 'runbox-auto-mode-line-map
  :doc "Map used for the `runbox-auto-mode-line' construct."
  "<mode-line> <mouse-1>" #'runbox-auto-mode-toggle-at-window
  "<mode-line> <mouse-2>" #'runbox-auto-describe-mode)

(runbox--define-help-echo runbox-auto-mode-line-help-echo
  (lambda (window _ __)
    (with-selected-window window
      (format
       "Runbox Auto Mode is %s
mouse-1: %s minor mode
mouse-2: show help for Runbox Auto Mode"
       (if runbox-auto-mode "on" "off")
       (if runbox-auto-mode "disable" "enable"))))
  "Help-echo string to use for the `runbox-auto-mode-line' construct.")

(defface runbox-auto-off-mode-line-face
  '((t))
  "Face for the runbox-auto mode-line indicator when disabled.")

(defface runbox-auto-on-mode-line-face
  '((t :inherit runbox-auto-off-mode-line-face))
  "Face for the runbox-auto mode-line indicator when enabled.")

(defcustom runbox-auto-off-mode-line-content
  '(:eval (if (runbox--displayable-p ?⬡) "⬡" "[-]"))
  "Construct providing text for `runbox-auto-mode' minor mode set to OFF.

By default used by `runox-auto-off-mode-line'.  All the properties ought
to be defined there."
  :type 'sexp
  :risky t)

(defcustom runbox-auto-on-mode-line-content
  '(:eval (if (runbox--displayable-p ?⬢) "⬢" "[*]"))
  "Construct providing text for `runbox-auto-mode' minor mode set to ON.

By default used by `runox-auto-on-mode-line'.  All the properties ought
to be defined there."
  :type 'sexp
  :risky t)

(defcustom runbox-auto-off-mode-line
  '(:propertize runbox-auto-off-mode-line-content
                face runbox-auto-off-mode-line-face
                mouse-face mode-line-highlight
                help-echo runbox-auto-mode-line-help-echo
                local-map runbox-auto-mode-line-map)
  "Construct defining the OFF state of the `runbox-auto-mode' minor mode.

By default contained by the `runbox-auto-mode-line' construct.
Its content (no properties) is defined in `runbox-auto-mode-line-content'."
  :type 'sexp
  :risky t)

(defcustom runbox-auto-on-mode-line
  '(:propertize runbox-auto-on-mode-line-content
                face runbox-auto-on-mode-line-face
                mouse-face mode-line-highlight
                help-echo runbox-auto-mode-line-help-echo
                local-map runbox-auto-mode-line-map)
  "Construct defining the ON state of the `runbox-auto-mode' minor mode.

By default contained by the `runbox-auto-mode-line' construct.
Its content (no properties) is defined in `runbox-auto-mode-line-content'."
  :type 'sexp
  :risky t)

(defcustom runbox-auto-mode-line
  '(runbox-auto-mode runbox-auto-on-mode-line runbox-auto-off-mode-line)
  "Construct defining the state of the `runbox-auto-mode' minor mode.

By default contained by `runbox-mode-line'."
  :type 'sexp
  :risky t)

(defcustom runbox-environment-mode-line-content
  '(:eval (runbox-environment-label-cached runbox-environment))
  "Construct providing the text for currently set `runbox-environment'.

By default, used by the `runbox-environment-mode-line' construct.  All
the properties ought to be defined there.

This construct assumes that `runbox-environment' is correctly set,
according to `runbox-invalid-p'."
  :type 'sexp
  :risky t)

(defface runbox-environment-mode-line-face
  '((t))
  "Face to use for the `runbox-environment-mode-line' construct.")

(runbox--define-help-echo runbox-environment-mode-line-help-echo
  (lambda (window _ __)
    (with-selected-window window
      (format
       "Runbox environment is set: %s
Bind mount: %s
mouse-1: Runbox menu"
       (tramp-make-tramp-file-name runbox-environment)
       runbox-bind-mount)))
  "Help-echo string to use for the `runbox-environment-mode-line' construct.")

(defvar-keymap runbox-environment-mode-line-map
  :prefix 'runbox-environment-mode-line-map
  :doc "Map used for the `runbox-environment-mode-line' construct."
  "<mode-line> <mouse-1>" 'runbox-menu-map)

(defcustom runbox-environment-mode-line
  '(:propertize runbox-environment-mode-line-content
                face runbox-environment-mode-line-face
                mouse-face mode-line-highlight
                help-echo runbox-environment-mode-line-help-echo
                local-map runbox-environment-mode-line-map)
  "Construct defining the currently set `runbox-environment' custom variable.

By default it is contained by `runbox-mode-line-format'.  Its
content (no properties) is defined by the
`rubnox-environment-mode-line-content' construct.

This construct assumes that `runbox-environment' is correctly set,
according to `runbox-invalid-p'."
  :type 'sexp
  :risky t)

(defcustom runbox-mode-line-format
  '(" " runbox-auto-mode-line runbox-environment-mode-line)
  "Construct defining the content of the top-level `runbox-mode-line' construct.

By default, it is contained by the top-level `runbox-mode-line'
construct which checks if `runbox-environment' is currently set.  By
default, it contains the following constructs:

- An indicator for the `runbox-auto-mode' minor mode.
- The label for the currently set `runbox-environment'.

This construct assumes that `runbox-environment' is correctly set,
according to `runbox-invalid-p'."
  :type 'sexp
  :risky t)

(defcustom runbox-mode-line
  '((:eval (unless (runbox-invalid-p) 'runbox-mode-line-format)))
  "Top-level mode-line construct for runbox.

By default, it is added to `mode-line-misc-info'.  By default, it checks
if `runbox-environment' is correctly set (according to
`runbox-invalid-p') and places its content which is defined according to
the `runbox-mode-line-format' user-customizable mode-line construct."
  :type 'sexp
  :risky t)

(add-to-list 'mode-line-misc-info 'runbox-mode-line 'append)

;;; Core logic
;; This contains the routing and assertion logic. Basically the building blocks
;; if needing to create an own runbox wrapper.

(defun runbox-invalid-p ()
  "Check `runbox-environment' and `default-directory' validity.

Return nil if everything is valid.  Otherwise return a symbol describing
the problem: `no-environment', `invalid-environment',
`no-default-directory', or `not-under-bind-mount'."
  (cond
   ((null runbox-environment) 'no-environment)
   ((not (tramp-file-name-p runbox-environment)) 'invalid-environment)
   ((null default-directory) 'no-default-directory)
   ((not (runbox-under-bind-mount-p default-directory)) 'not-under-bind-mount)
   (t nil)))

(defun runbox-assert ()
  "Validate `runbox-environment' and `default-directory' or throw `user-error'.
`runbox-environment' must be set as a `tramp-file-name' struct.
`default-directory' must be set and under `runbox-bind-mount'."
  (pcase (runbox-invalid-p)
    ('no-environment
     (user-error "No runbox environment set (runbox-environment is nil)"))
    ('invalid-environment
     (user-error "runbox-environment is not a valid `tramp-file-name': %S"
                 runbox-environment))
    ('no-default-directory
     (user-error "default-directory is nil"))
    ('not-under-bind-mount
     (user-error "default-directory is not under bind-mount"))
    (_ runbox-environment)))
 
;;;; Routing
(defun runbox-funcall (environment fn &rest args)
  "Call FN in ENVIRONMENT.

`runbox-environment' must be set, and `default-directory' must be a
local path.

FN is called with `default-directory' let-bound as
`runbox-environment''s TRAMP prefix prepended.  ARGS are passed to FN."
  (when (file-remote-p default-directory)
    (warn (format "`default-directory' should be local: %S" default-directory)))
  (let* ((non-essential nil)
         (tramp-verbose 0))
    (tramp-maybe-open-connection environment)
    ;; Beware of the old signature of tramp-make-tramp-file-name
    ;; (Accepts strings as first parameter)
    (let* ((default-directory (tramp-make-tramp-file-name environment
                                                         default-directory)))
      (apply fn args))))

(defvar runbox--ctx-token nil
  "Token identifying the innermost active `runbox-with-routed' invocation.

Bound dynamically.  Must remain nil globally.")

(defun runbox--maybe-runbox-funcall (ctx env fn &rest args)
  "Wraps FN in `runbox-funcall' only if CTX matches `runbox--ctx-token'.
Otherwise FN is called directly.  ENV, FN and ARGS are passed to
`runbox-funcall'.

Internal function."
  (if (eq ctx runbox--ctx-token)
      (let ((runbox--ctx-token nil))
        (apply #'runbox-funcall env fn args))
    (apply fn args)))

(defmacro runbox-with-routed (functions &rest body)
  "Evaluate BODY with FUNCTIONS shadowed to route through `runbox-funcall'.

FUNCTIONS is a list of unquoted symbol functions."
  (declare (indent 1))
  ;;cl-letf overrides functions globally for the duration of the macro.
  ;;this `runbox--token' "hack" (?) restricts the shadowing to the call site.
  `(let* ((runbox--ctx-token (gensym "runbox-token")))
     (cl-letf
         ,(mapcar (lambda (func)
                    `((symbol-function ',func)
                      (apply-partially #'runbox--maybe-runbox-funcall
                                       runbox--ctx-token
                                       runbox-environment
                                       (symbol-function ',func))))
                  functions)
       ,@body)))

;;;; Declare form
;; Runbox wrapper fun shall have the `(declare runbox FUN)' in their `defun'.
;; FUN is the unquoted symbol of the wrapped function. FUN can also be t: the
;; name will be inferred from the wrapper's name (e.g. `runbox-shell' wraps
;; `shell').
(eval-and-compile
  (defun runbox--declare-handle (name _arglist builtin)
    "Handler for the `runbox' declare clause used in `defun'.

NAME is the runbox wrapper function being defined. BUILTIN names the
built-in function it wraps: an unquoted symbol, or t to infer it by
stripping the `runbox-' prefix from NAME.

Registers the relationship for later lookup: sets NAME's `runbox'
property to BUILTIN, and BUILTIN's `runbox-wrapped-by' property to
NAME."
    (when-let*
        ((builtin
          (pcase builtin
            ('nil nil)
            ('t (let ((s (symbol-name name)))
                  (unless (string-prefix-p "runbox-" s)
                    (error (concat "Cannot infer builtin for `%s':"
                                   " missing `runbox-' prefix")
                           name))
                  (intern (string-remove-prefix "runbox-" s))))
            ((pred symbolp) builtin)
            (_ (error "Invalid")))))
      `(progn
         (function-put ',name 'runbox ',builtin)
         (function-put ',builtin 'runbox-wrapped-by ',name))))

  (add-to-list 'defun-declarations-alist
               (list 'runbox #'runbox--declare-handle)))

(defun runbox--help-fn (function)
  "Show FUNCTION's runbox wrapper/wrapped counterpart in `*Help*', if any.

Used in `help-fns-descrive-function-functions'."
  (when-let* ((builtin (get function 'runbox)))
    (insert
     (format-message "  This is a runbox wrapper around `%s'.\n" builtin)))
  (when-let* ((wrapper (get function 'runbox-wrapped-by)))
    (insert
     (format-message "  This function has a runbox wrapper: `%s'.\n" wrapper))))

(add-hook 'help-fns-describe-function-functions #'runbox--help-fn)

;;; `compile' integration
(declare-function project-root "project" (project))

;;;###autoload
(defun runbox-compile (command &optional comint)
  (declare (runbox t))
  (interactive
   (progn
     (runbox-assert)
     (advice-eval-interactive-spec (cadr (interactive-form 'compile)))))
  (runbox-assert)
  (runbox-with-routed (start-file-process)
    (compile command comint)))

;;;###autoload
(defun runbox-project-compile ()
  (declare (interactive-only runbox-compile)
           (runbox t))
  (interactive)
  (if (project-current)
      (progn
        (runbox-assert)
        (runbox-with-routed (start-file-process)
          (call-interactively #'project-compile)))
    (with-temp-buffer
      (let ((default-directory (project-root (project-current 'maybe-prompt))))
        (hack-dir-local-variables-non-file-buffer)
        (runbox-assert)
        (runbox-with-routed (start-file-process)
          (call-interactively #'project-compile))))))

;; TODO: set local compilation-parse-errors-filename-function

;;;###autoload
(defun runbox-recompile (&optional edit-command)
  (declare (runbox t))
  (interactive
   (progn
     (runbox-assert)
     (advice-eval-interactive-spec (cadr (interactive-form 'recompile)))))
  (runbox-assert)
  (runbox-with-routed (start-file-process)
    (recompile edit-command)))

;;;###autoload
(defun runbox-project-recompile ()
  (declare (interactive-only runbox-recompile)
           (runbox t))
  (interactive)
  (if (project-current)
      (runbox-assert)
    (runbox-with-routed (start-file-process)
      (call-interactively #'project-recompile))
    (with-temp-buffer
      (let ((default-directory (project-root (project-current 'maybe-prompt))))
        (hack-dir-local-variables-non-file-buffer)
        (runbox-assert)
        (runbox-with-routed (start-file-process)
          (call-interactively #'project-recompile))))))

;;; `shell' integration
(declare-function project-root "project" (project))

;; `shell' interactive logic minus but minus some logic:
;; 1. No default-directory prompting for remote buffers
;; 2. No prompting for remote shell path
;;;###autoload
(defun runbox-shell (&optional buffer file-name)
  (declare (runbox shell))
  (interactive
   (progn
     (runbox-assert)
     (let* ((buffer
             (and current-prefix-arg
		          (read-buffer "Shell buffer: "
			                   ;; If the current buffer is an inactive
			                   ;; shell buffer, use it as the default.
			                   (if (and (eq major-mode 'shell-mode)
				                        (null (get-buffer-process
				                               (current-buffer))))
				                   (buffer-name)
			                     (generate-new-buffer-name
                                  "*shell*")))))
            (file-name (with-connection-local-variables ;; TODO fix this(?)
                         (or explicit-shell-file-name
                             (getenv "ESHELL")
                             shell-file-name))))
       (list buffer file-name))))
  (runbox-with-routed (start-file-process)
    (shell buffer file-name)))

;;;###autoload
(defun runbox-project-shell ()
  (declare (runbox t))
  (interactive)
  (if (project-current)
      (progn
        (runbox-assert)
        (runbox-with-routed (start-file-process)
          (call-interactively #'project-shell)))
    (with-temp-buffer
      (let ((default-directory (project-root (project-current 'maybe-prompt))))
        (hack-dir-local-variables-non-file-buffer)
        (runbox-assert)
        (runbox-with-routed (start-file-process)
          (call-interactively #'project-shell))))))

;;; `shell-command' integration
(declare-function project-root "project" (project))
;; I hate the way i wrote this.  Shell-command is both an HANDLER and an
;; interactive function. This makes it quite troublesome to "runbox-ify".

;;;###autoload
(defun runbox-shell-command (command &optional output-buffer error-buffer)
  (declare (runbox t))
  (interactive
   (progn
     (runbox-assert)
     (advice-eval-interactive-spec (cadr (interactive-form 'shell-command)))))
  (runbox-assert)
  (runbox-funcall #'shell-command command output-buffer error-buffer)
  (let ((out-buf (get-buffer (or output-buffer
                                 shell-command-buffer-name)))
        (err-buf (get-buffer (or error-buffer
                                 shell-command-default-error-buffer)))
        (dir default-directory))
    (when out-buf
      (with-current-buffer out-buf
        (setq-local default-directory dir)))
    (when err-buf
      (with-current-buffer err-buf
        (setq-local default-directory dir)))))

;;;###autoload
(defun runbox-project-shell-command ()
  "Run `shell-command' in the current project's root directory."
  (declare (interactive-only runbox-shell-command)
           (runbox t))
  (interactive)
  (let ((dir default-directory))
    (if (project-current)
        (progn
          (runbox-assert)
          (call-interactively #'shell-command))
      (with-temp-buffer
        (let ((default-directory (project-root (project-current 'maybe-prompt))))
          (hack-dir-local-variables-non-file-buffer)
          (runbox-assert)
          (runbox-with-routed (start-file-process)
            (setq dir default-directory)
            (call-interactively #'shell-command)))))
    (let ((out-buf (get-buffer shell-command-buffer-name))
          (err-buf (get-buffer shell-command-default-error-buffer)))
      (when out-buf
        (with-current-buffer out-buf
          (setq-local default-directory dir)))
      (when err-buf
        (with-current-buffer err-buf
          (setq-local default-directory dir))))))

;;;###autoload
(defun runbox-async-shell-command (command &optional output-buffer error-buffer)
  (declare (runbox t))
  (interactive
   (progn
     (runbox-assert)
     (advice-eval-interactive-spec (cadr
                                    (interactive-form 'async-shell-command)))))
  (runbox-assert)
  (runbox-with-routed (shell-command)
    (async-shell-command command output-buffer error-buffer))
  (let ((out-buf (get-buffer (or output-buffer
                                 shell-command-buffer-name-async)))
        (err-buf (get-buffer (or error-buffer
                                 shell-command-default-error-buffer)))
        (dir default-directory))
    (when out-buf
      (with-current-buffer out-buf
        (setq-local default-directory dir)))
    (when err-buf
      (with-current-buffer err-buf
        (setq-local default-directory dir)))))

;;;###autoload
(defun runbox-project-async-shell-command ()
  (declare (interactive-only runbox-async-shell-command)
           (runbox t))
  (interactive)
  (let ((dir default-directory))
    (if (project-current)
        (progn
          (runbox-assert)
          (call-interactively #'async-shell-command))
      (with-temp-buffer
        (let ((default-directory (project-root (project-current 'maybe-prompt))))
          (hack-dir-local-variables-non-file-buffer)
          (runbox-assert)
          (runbox-with-routed (start-file-process)
            (setq dir default-directory)
            (call-interactively #'async-shell-command))))
      (let ((out-buf (get-buffer shell-command-buffer-name))
            (err-buf (get-buffer shell-command-default-error-buffer)))
        (when out-buf
          (with-current-buffer out-buf
            (setq-local default-directory dir)))
        (when err-buf
          (with-current-buffer err-buf
            (setq-local default-directory dir)))))))

;;; Other integrations
;; Search for other files "e.g. ./runbox-eat.el

(provide 'runbox)
;;; runbox.el ends here
