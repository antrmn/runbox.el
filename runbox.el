;;; runbox.el --- -*- lexical-binding: t; -*-

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

;; 

;;; Code:
(require 'eglot)

(defcustom runbox-bind-mount (expand-file-name "~")
  "Local path that the runbox container bind mounts.
Defaults to the user's home directory."
  :type 'directory
  :local t
  :group 'runbox)

(defcustom runbox-container nil
  "TRAMP backend and container to use as build environment.
A cons of (METHOD . NAME), e.g. (\"toolbox\" . \"fedora-toolbox-44\")"
  :type '(choice (const :tag "None" nil)
                 (cons (string :tag "TRAMP method")
                       (string :tag "Container name")))
  :local t
  :group 'runbox)

(defun runbox--trampify (dir)
  "Return DIR as a TRAMP path into the runbox container.
Requires `runbox-container' to be set."
  (let ((vec (make-tramp-file-name :method (car runbox-container)
                                   :host   (cdr runbox-container)))
        (non-essential nil))
    (tramp-maybe-open-connection vec)
    (tramp-make-tramp-file-name vec dir)))

(defun runbox--under-bind-mount-p (&optional dir)
  (let ((dir (or dir default-directory)))
    (and runbox-container
       runbox-bind-mount
       (string-prefix-p (file-truename runbox-bind-mount)
                        (file-truename dir)))))


(defun runbox--around-eglot-guess-contact (orig &rest args)
  "Advice around `eglot--guess-contact' to spawn the LSP server via TRAMP.
Rebinds `default-directory' to a TRAMP path when applicable, and restores
the local project in the returned contact to keep buffer association on the
host side.  ARGS are passed to ORIG unchanged."
  (let* ((project (eglot--current-project))
         (default-directory (if (runbox--under-bind-mount-p)
                                (runbox--trampify default-directory)
                              default-directory))
         (result (apply orig args)))
    (cons (car result)
          (cons project
                (cddr result)))))

(advice-add 'eglot--guess-contact :around #'runbox--around-eglot-guess-contact)


(cl-defmethod shared-initialize :around
  ((server eglot-lsp-server) slots)
  "Wraps `:process' in SLOTS to spawn the LSP server via TRAMP.
Rebinds `default-directory' to the TRAMP path before calling the original
process function, falling through unchanged if no TRAMP path is applicable."
  (cl-call-next-method
   server
   (if-let* ((process-fn (plist-get slots :process))
             (tramp-dir (when (runbox--under-bind-mount-p)
                          (runbox--trampify default-directory))))
       (plist-put slots :process
                  (lambda ()
                    (let ((default-directory tramp-dir))
                      (funcall process-fn))))
     slots)))

(defun runbox--maybe-trampify (path)
  (if (and (runbox--under-bind-mount-p) ;; AKA "runbox enabled"
           (not (file-remote-p path))
           (not (runbox--under-bind-mount-p path)))
      (runbox--trampify path)
    path))

(advice-add 'eglot-uri-to-path :filter-return #'runbox--maybe-trampify)

;; Compilation

;; TODO: what if compilation buffer shows paths that are exclusively from toolbx?
;; Compilation-search-path does work!!
(defun runbox--compilation-start-advice (orig command &rest args)
  (let ((compilation-process-setup-function
         (let ((outer-setup compilation-process-setup-function))
           (lambda ()
             ;; Should I call it after or before??
             (when outer-setup (funcall outer-setup))
             (setq-local default-directory
                         (or (runbox--trampify default-directory)
                             default-directory))))))
    (apply orig command args)))

(advice-add 'compilation-start :around #'runbox--compilation-start-advice)

;; Shell
(defun runbox-shell ()
  (interactive)
  (let ((local-dir default-directory)
        (default-directory (runbox--trampify default-directory)))
    (message "[runbox-shell] spawning with default-directory: %s" default-directory)
    (shell))
  (message "[runbox-shell] after shell, buffer: %s, default-directory: %s"
           (current-buffer) default-directory)
  (setq-local default-directory local-dir)
  (setq-local comint-file-name-prefix ""))


;; Term

(defun runbox-term ()
  (interactive)
  (let ((local-dir default-directory)
        (default-directory (runbox--trampify default-directory)))
    (term "/bin/bash")
    (setq-local default-directory local-dir)))


(provide 'runbox)
;;; runbox.el ends here
