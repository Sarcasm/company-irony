;;; company-irony.el --- company-mode completion back-end for irony-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: convenience
;; Version: 0.1.0
;; URL: https://github.com/Sarcasm/company-irony/
;; Compatibility: GNU Emacs 24.1 and above.
;; Package-Requires: ((company "0.8.0") (irony "0.1.0") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add `company-irony' to `company-backends':
;;
;;     (add-to-list 'company-backends 'company-irony)
;;
;; Be careful of conflict with the irony-mode completion at point integration if
;; `company-capf' is used at the same time.

;;; Code:

(require 'irony-completion)

(require 'company)

(require 'cl-lib)

(defsubst company-irony--irony-candidate (candidate)
  (get-text-property 0 'company-irony candidate))

(defun company-irony--make-all-completions (prefix candidates)
  (cl-loop for candidate in candidates
           when (string-prefix-p prefix (car candidate))
           collect (propertize (car candidate) 'company-irony candidate)))

(defun company-irony-candidates-async (prefix callback)
  (funcall callback (company-irony--make-all-completions
                     prefix (irony-completion-candidates-at-point))))

(defun company-irony-candidates (prefix)
  (irony--aif (irony-completion-candidates-at-point)
      (company-irony--make-all-completions prefix it)
    (cons :async
          (lambda (callback)
            (irony-completion-candidates-at-point-async
             (lambda () ;; closure
               (company-irony-candidates-async prefix callback)))))))

;;;###autoload
(defun company-irony (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-irony))
    (prefix (and irony-completion-mode
                 ;;TODO: provide this via irony-completion
                 (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates (company-irony-candidates arg))
    (annotation (irony-completion-annotation
                 (company-irony--irony-candidate arg)))
    (meta (irony-completion-brief
           (company-irony--irony-candidate arg)))
    (post-completion (irony-completion-post-complete
                      (company-irony--irony-candidate arg)))
    (sorted t)))

(provide 'company-irony)
;;; company-irony.el ends here
