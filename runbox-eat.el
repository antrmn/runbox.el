;;; runbox-eat.el --- Runbox integration for Eat -*- lexical-binding: t; -*-

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
;; runbox-eat function is autoloaded only if eat.el is installed.
;; 

;;; Code:
(require 'eat nil 'noerror)
(require 'runbox)

(unless (featurep 'eat)
  (error "Feature `runbox-eat' requires package `eat' to be installed"))


(declare-function eat "eat")
(defun runbox-eat (&optional program arg)
  (interactive
   (progn
     (runbox-assert)
     ;;TODO getenv in remote (?)
     (advice-eval-interactive-spec (cadr (interactive-form 'eat)))))
  (runbox-assert)
  (runbox-with-routed (make-process)
    (eat program arg)))

;; https://emacs.stackexchange.com/questions/85834
;;;###autoload
(unless (fboundp 'runbox-eat)
  (if (or (fboundp 'eat)
          (locate-library "eat"))
      (autoload 'runbox-eat "runbox-eat" nil t)
    (with-eval-after-load "eat"
      (autoload 'runbox-eat "runbox-eat" nil t))))

(provide 'runbox-eat)
;;; runbox-eat.el ends here
