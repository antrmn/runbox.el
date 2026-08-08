;;; runbox-eglot.el --- Runbox integration for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Antonio Romano

;; Author: Antonio Romano <cidra@posteo.it>
;; Keywords: processes

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
;; Kept separate to avoid pulling in the Eglot dependency just from
;; requiring runbox.el.  Also to keep main code tidy.
;;; Code:

(require 'eglot)
(require 'runbox)


(defclass runbox-eglot-server-mixin ()
  ((runbox-eglot-server-environment
    :initarg :runbox-environment
    :initform nil
    :accessor runbox-eglot-server-environment))
  :documentation "Mixin class adding runbox-specific fields.

This can be used to inject custom fields and behaviour to whichever
class is passed to `eglot--connect'.")

(defun runbox-eglot-make-server-class (base-class)
  "Return a class that inherits from BASE-CLASS and `runbox-eglot-server-mixin'."
  (let ((new-class (intern (format "runbox-%s" base-class))))
    (unless (class-p new-class)
      (eval `(defclass ,new-class (,base-class runbox-eglot-server-mixin)
               ()
               :documentation
               ,(format "Runbox variant of `%s', routed through a runbox."
                        base-class))))
    new-class))

(cl-defmethod shared-initialize :around
  ((server runbox-eglot-server-mixin) slots)
  "Wraps `:process' in SLOTS to spawn the LSP server via TRAMP.

Rebinds `default-directory' to the TRAMP path before calling the original
process function."
  (cl-call-next-method
   server
   (if-let* ((process-fn (plist-get slots :process)))
       (plist-put slots :process
                  (runbox-funcall (plist-get slots :runbox-environment)
                                  process-fn))
     slots)))

;;;###autoload
(defun runbox-eglot (managed-major-modes project class contact language-ids
                                         &optional _interactive)
  (declare (runbox t))
  (interactive
   (progn (runbox-assert)
          (runbox-with-routed (executable-find)
            (advice-eval-interactive-spec (cadr (interactive-form 'eglot))))))
  (runbox-assert)
  (eglot managed-major-modes
         project
         (runbox-eglot-make-server-class class) ; Inject mixin in `class'
         (append contact ; Add new args to `class' constructor
                 (list :runbox-environment runbox-environment))
         language-ids))

;;;###autoload
(defun runbox-eglot-ensure ()
  (declare (runbox t))
  (let ((buffer (current-buffer)))
    (cl-labels
        ((maybe-connect
           ()
           (eglot--when-live-buffer buffer
             (remove-hook 'post-command-hook #'maybe-connect t)
             (unless eglot--managed-mode
               (condition-case-unless-debug oops
                   ;; =======Diff from original==========
                   (runbox-with-routed (executable-find)
                     (let ((contact (eglot--guess-contact)))
                       (setf (nth 2 contact)
                             (runbox-eglot-make-server-class (nth 2 contact)))
                       (apply #'eglot--connect contact)))
                 ;; ==========================
                 (error (eglot--warn (error-message-string oops))))))))
      (when buffer-file-name
        (add-hook 'post-command-hook #'maybe-connect 'append t)))))

;; This puts a TRAMP prefix in paths that are outside the bind mount.
(defun runbox--eglot-maybe-trampify (path)
  (let ((server (eglot-current-server)))
    (if (and server
             (object-of-class-p server 'runbox-eglot-server-mixin)
             (not (runbox-under-bind-mount-p path)))
        (tramp-make-tramp-file-name (tramp-ensure-dissected-file-name
                                     (runbox-eglot-server-environment server))
                                    path)
      path)))

;; I wonder why `eglot-uri-to-path' is not a method,
;; that would make overriding more convenient.
(advice-add 'eglot-uri-to-path :filter-return #'runbox--eglot-maybe-trampify)

(provide 'runbox-eglot)
;;; runbox-eglot.el ends here
