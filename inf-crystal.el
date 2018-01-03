;;; inf-crystal.el --- Run a Inferior-Crystal process in a buffer

;; Copyright (C) 2017 Brantou

;; Author: Brantou <brantou89@gmail.com>
;; URL: http://github.com/brantou/inf-crystal.el
;; Keywords: languages coffee
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; inf-crystal provides a REPL buffer connected to a icr subprocess.
;;

;;; Code:

(require 'comint)
(require 'crystal-mode)

(defgroup inf-crystal nil
  "Run inf-crystal process in a buffer"
  :group 'languages)

(defcustom inf-crystal-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'inf-crystal)

(defcustom inf-crystal-buffer-name "*inferior-crystal*"
  "Default buffer name for inf-crystal interpreter."
  :type 'string
  :group 'inf-crystal
  :safe 'stringp)

(defcustom inf-crystal-interpreter "icr"
  "Default crystal interpreter for inf-crystal."
  :type 'string
  :group 'inf-crystal)

(defvar inf-crystal-buffer  nil "The live inf-crystal process buffer.")

(defvar inf-crystal-prompt "icr([0-9]\\(\\.[0-9]+\\)+) >")

(define-derived-mode inf-crystal-mode comint-mode "INF-CRYSTAL"
  "Major mode for interacting with a inf-crystal process."
  :syntax-table crystal-mode-syntax-table
  (crystal-mode-variables)
  (setq-local font-lock-defaults '((crystal-font-lock-keywords)))
  (setq comint-prompt-regexp inf-crystal-prompt)
  (setq comint-process-echoes t)
  (setq comint-input-ignoredups t)
  (set (make-local-variable 'comint-prompt-read-only) inf-crystal-prompt-read-only))

;;;###autoload
(defun inf-crystal ()
  "Launch a crystal interpreter using `inf-crystal-interpreter' as an inferior mode."
  (interactive)
  (let ((proc-buffer (comint-check-proc inf-crystal-buffer-name)))
    (if proc-buffer
        (pop-to-buffer inf-crystal-buffer-name)
      (progn
        (set-buffer (apply 'make-comint-in-buffer
                           inf-crystal-interpreter
                           (get-buffer-create inf-crystal-buffer-name)
                           inf-crystal-interpreter
                           nil
                           '("--no-color")))
        (with-current-buffer inf-crystal-buffer-name
          (inf-crystal-mode)
          (setq inf-crystal-buffer (current-buffer)))
        (pop-to-buffer inf-crystal-buffer-name)))))

;;;###autoload
(defalias 'run-crystal 'inf-crystal)

(defun inf-crystal-proc()
  (unless (comint-check-proc inf-crystal-buffer-name)
    (inf-crystal))
  (get-buffer-process inf-crystal-buffer-name))

(defun crystal-switch-to-inf(eob-p)
  "Switch to the inf-crystal process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (unless inf-crystal-buffer
    (inf-crystal))
  (pop-to-buffer inf-crystal-buffer)
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun crystal-send-region-and-go (start end)
  "Send the current region to the inferior Crystal process.
Then switch to the process buffer."
  (interactive "r")
  (crystal-send-region start end)
  (crystal-switch-to-inf t))

(defun crystal-send-definition-and-go ()
  "Send the current definition to the inferior Crystal.
Then switch to the process buffer."
  (interactive)
  (crystal-send-definition)
  (crystal-switch-to-inf t))

;;(defun crystal-send-block-and-go ()
;;  "Send the current block to the inferior Crystal.
;;Then switch to the process buffer."
;;  (interactive)
;;  (crystal-send-block)
;;  (crystal-switch-to-inf t))

(defun crystal-send-definition ()
  "Send the current definition to the inferior crystal process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (crystal-beginning-of-defun)
      (crystal-send-region (point) end))))

(defun crystal-send-last-sexp ()
  "Send the previous sexp to the inferior crystal process."
  (interactive)
  (crystal-send-region (save-excursion (crystal-backward-sexp) (point)) (point)))

;;(defun crystal-send-block ()
;;  "Send the current block to the inferior Crystal process."
;;  (interactive)
;;  (save-excursion
;;    (crystal-end-of-block)
;;    (end-of-line)
;;    (let ((end (point)))
;;      (crystal-beginning-of-block)
;;      (crystal-send-region (point) end))))

(defun crystal-send-buffer ()
  "Send the current buffer to the inf-crystal process."
  (interactive)
  (save-restriction
    (widen)
    (crystal-send-region (point-min) (point-max))))

(defun crystal-send-line ()
  "Send the current line to the inf-crystal process."
  (interactive)
  (save-restriction
    (widen)
    (crystal-send-region (point-at-bol) (point-at-eol))))

(defun crystal-send-region (start end)
  "Send the current region to the inf-crystal process."
  (interactive "r")
  (let* ((string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (message "Sent: %s..." (match-string 1 string))
    (comint-send-string (inf-crystal-proc) string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string (inf-crystal-proc) "\n"))))

(provide 'inf-crystal)
;;; inf-coffee.el ends here
