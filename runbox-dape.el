;;; runbox-dape.el --- Runbox integration for Dape -*- lexical-binding: t; -*-

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
;; runbox-dape is autoloaded only if dape.el is installed.
;; 

;;; Code:
(require 'runbox)
(require 'dape nil 'noerror)

(unless (featurep 'dape)
  (error "Feature `runbox-dape' requires package `dape' to be installed"))


(declare-function dape "dape")
(defun runbox-dape (config &optional skip-compile)
  (interactive
   (progn
     (runbox-assert)
     (advice-eval-interactive-spec (cadr (interactive-form 'dape)))))
  (runbox-with-routed (make-process process-file-shell-command)
    (dape config skip-compile)))

;; https://emacs.stackexchange.com/questions/85834
;;;###autoload
(unless (fboundp 'runbox-dape)
  (if (or (fboundp 'dape)
          (locate-library "dape"))
      (autoload 'runbox-dape "runbox-dape" nil t)
    (with-eval-after-load "dape"
      (autoload 'runbox-dape "runbox-dape" nil t))))

(provide 'runbox-dape)
;;; runbox-dape.el ends here
