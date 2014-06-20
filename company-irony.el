;;; company-irony.el --- company-mode completion back-end for irony-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: convenience
;; Version: 0.1.0
;; URL: https://github.com/Sarcasm/company-irony/
;; Package-Requires: ((emacs "24.1") (company "0.8.0") (irony "0.1.0") (cl-lib "0.5"))

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

;; Usage:
;;
;;     (eval-after-load 'company
;;       '(add-to-list 'company-backends 'company-irony))
;;     (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;; Code:

(require 'irony-completion)

(require 'company)

(require 'cl-lib)

(defgroup company-irony nil
  "Company-mode completion back-end for Irony."
  :group 'company
  :group 'irony)

(defsubst company-irony--irony-candidate (candidate)
  (get-text-property 0 'company-irony candidate))

(defun company-irony--make-all-completions (prefix candidates)
  (cl-loop for candidate in candidates
           when (string-prefix-p prefix (car candidate))
           collect (propertize (car candidate) 'company-irony candidate)))

(defun company-irony-candidates-async (prefix callback)
  (funcall callback (company-irony--make-all-completions
                     prefix (irony-completion-candidates))))

(defun company-irony-candidates (prefix)
  (if (irony-completion-candidates-available-p)
      (company-irony--make-all-completions prefix
                                           (irony-completion-candidates))
    (cons :async
          (lambda (callback)
            (irony-completion-candidates-async
             (lambda () ;; closure, lexically bound
               (company-irony-candidates-async prefix callback)))))))

(defun irony-company-post-completion (candidate)
  ;; This check is necessary because Company triggers a 'post-completion even if
  ;; the candidate has just been typed without relying on the completion, but it
  ;; doesn't provide the full candidate information.
  (when candidate
    (irony-completion-post-complete candidate)))

(defun company-irony-prefix ()
  (let ((symbol-start (irony-completion-beginning-of-symbol)))
    (if symbol-start
        (let ((prefix (buffer-substring-no-properties symbol-start (point))))
          (save-excursion
            (goto-char symbol-start)
            (if (irony-completion-at-trigger-point-p)
                (cons prefix t)
              prefix)))
      'stop)))

;;;###autoload
(defun company-irony (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-irony))
    (prefix (and irony-completion-mode
                 (company-irony-prefix)))
    (candidates (company-irony-candidates arg))
    (annotation (irony-completion-annotation
                 (company-irony--irony-candidate arg)))
    (meta (irony-completion-brief
           (company-irony--irony-candidate arg)))
    (post-completion (irony-company-post-completion
                      (company-irony--irony-candidate arg)))
    (sorted t)))

;;;###autoload
(defun company-irony-setup-begin-commands ()
  "Include irony trigger commands to `company-begin-commands'.

This allow completion to be automatically triggered after member
accesses (obj.|, obj->|, ...)."
  (if (listp company-begin-commands)
      (set (make-local-variable 'company-begin-commands)
           (delete-dups
            (append company-begin-commands irony-completion-trigger-commands)))
    (display-warning 'company-irony
                     "`company-irony-setup-begin-commands' expects \
`company-begin-commands' to be a list!")))

(provide 'company-irony)
;;; company-irony.el ends here
