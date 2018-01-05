;;; inf-crystal.el --- Run a Inferior-Crystal process in a buffer

;; Copyright (C) 2017 Brantou

;; Author: Brantou <brantou89@gmail.com>
;; URL: http://github.com/brantou/inf-crystal.el
;; Keywords: languages crystal
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (crystal-mode "0.1"))

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
;; ### Installation
;;
;; #### Via package.el
;;
;; TODO
;;
;; #### Manual
;;
;; If you're installing manually, you'll need to:
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;
;;    (autoload 'inf-crystal "inf-crystal" "Run an inferior Crystal process" t)
;;    (add-hook 'crystal-mode-hook 'inf-crystal-minor-mode)
;;
;; ### Usage
;;
;; Run one of the predefined interactive functions.
;;
;; See [Function Documentation](#function-documentation) for details.
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

(defvar inf-crystal-buffer  nil
  "*The live inf-crystal process buffer.

For information on running multiple processes in multiple buffers, see
the description of `inferior-lisp-buffer'.")

(defvar inf-crystal-mode-hook '()
  "Hook for customizing Inferior Crystal mode.")

(defcustom inf-crystal-prompt "icr([0-9]\\(\\.[0-9]+\\)+) >"
  "Regexp to recognize prompts in the Inferior Crystal mode."
  :type 'regexp
  :group 'inf-crystal)

(defvar inf-crystal-mode-map
  (let ((map (copy-keymap comint-mode-map)))
        (define-key map (kbd "C-x C-e") 'crystal-send-last-sexp)
        (define-key map (kbd "C-x C-d") 'inf-crystal-toggle-debug-mode)
        (define-key map (kbd "C-x C-p") 'inf-crystal-enable-paste-mode)
        (define-key map (kbd "C-x C-y") 'inf-crystal-disable-paste-mode)
        (define-key map (kbd "C-x C-r") 'inf-crystal-reset)
        map)
  "Mode map for `inf-crystal-mode'.")

(easy-menu-define
  inf-crystal-menu
  inf-crystal-mode-map
  "Inf Crystal Menu"
  '("Inf-Crystal"
    ["Eval Last Sexp" crystal-send-last-sexp t]
    "--"
    ["Enable paste mode" inf-crystal-enable-paste-mode t]
    ["Disable paste mode" inf-crystal-disable-paste-mode t]
    "--"
    ["Toggle debug mode" inf-crystal-toggle-debug-mode t]
    ["Reset repl" inf-crystal-reset t]))

(define-derived-mode inf-crystal-mode comint-mode "Inf-Crystal"
  "Major mode for interacting with an icr process."
  :syntax-table crystal-mode-syntax-table
  (crystal-mode-variables)
  (setq-local font-lock-defaults '((crystal-font-lock-keywords)))
  (setq comint-prompt-regexp inf-crystal-prompt)
  (setq comint-process-echoes t)
  (setq comint-input-ignoredups t)
  (setq comint-get-old-input (function inf-crystal-get-old-input))
  (add-hook 'comint-preoutput-filter-functions 'inf-crystal-preoutput-filter nil t)
  (set (make-local-variable 'comint-prompt-read-only) inf-crystal-prompt-read-only))

(defun inf-crystal-get-old-input()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (crystal-backward-sexp)
      (buffer-substring (point) end))))

(defun inf-crystal-preoutput-filter (output)
  "Filter paste mode OUTPUT."
  (if (and (string-match "Ctrl-D" output) (string-match "paste mode" output))
      "\n"
    output))

(defun inf-crystal-reset ()
  "Clear out all of the accumulated commands."
  (interactive)
  (comint-send-string (inf-crystal-proc) "reset\n"))

(defun inf-crystal-toggle-debug-mode ()
  "Toggle debug mode off and on.
In debug mode icr will print the code before executing it."
  (interactive)
  (comint-send-string (inf-crystal-proc) "debug\n"))

(defun inf-crystal-enable-paste-mode ()
  "Enable paste mode."
  (interactive)
  (comint-send-string (inf-crystal-proc) "paste\n"))

(defun inf-crystal-disable-paste-mode ()
  "Disable paste mode."
  (interactive)
  (with-current-buffer (inf-crystal-buffer)
    (comint-send-eof)))

;;;###autoload
(defun inf-crystal (cmd)
  "Launch a crystal interpreter in a buffer.
using `inf-crystal-interpreter'as an inferior mode."
  (interactive (list (if current-prefix-arg
                         (read-string "Run inf-crystal: " inf-crystal-interpreter)
                       inf-crystal-interpreter)))
  (if (not (comint-check-proc inf-crystal-buffer-name))
      (let ((cmdlist (split-string cmd))
            (name "crystal"))
        (unless (member "--no-color" cmdlist)
          (setq cmdlist (append cmdlist '("--no-color"))))
        (message (format "cmdlist: %s" cmdlist))
        (set-buffer (apply 'make-comint-in-buffer
                           name
                           (get-buffer-create inf-crystal-buffer-name)
                           (car cmdlist)
                           nil
                           (cdr cmdlist)))
        (inf-crystal-mode)))
  (setq inf-crystal-buffer inf-crystal-buffer-name)
  (pop-to-buffer inf-crystal-buffer-name))

;;;###autoload
(defalias 'run-crystal 'inf-crystal)

(defun inf-crystal-proc()
  "Returns the current inferior crystal process.

See variable `inf-crystal-buffer'."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inf-crystal-mode)
                                      (current-buffer)
                                    inf-crystal-buffer))))
    (or proc
        (error "No inf-crystal subprocess, see variable inf-crystal-buffer"))))

(defun inf-crystal-buffer()
  "Returns the current inferior crystal buffer."
  (let ((buf (if (derived-mode-p 'inf-crystal-mode)
                 (current-buffer)
               inf-crystal-buffer)))
    (or buf
        (error "No inf-crystal buffer, see variable inf-crystal-buffer"))))

(defun crystal-switch-to-inf(eob-p)
  "Switch to the inf-crystal process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-crystal-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inf-crystal-buffer t))))
        (pop-to-buffer inf-crystal-buffer))
    (run-crystal inf-crystal-interpreter))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun crystal-send-last-sexp ()
  "Send the previous sexp to the inferior crystal process."
  (interactive)
  (crystal-send-region (save-excursion (crystal-backward-sexp) (point)) (point)))

(defun crystal-send-line ()
  "Send the current line to the inf-crystal process."
  (interactive)
  (save-restriction
    (widen)
    (crystal-send-region (point-at-bol) (point-at-eol))))

;;(defun crystal-send-block ()
;;  "Send the current block to the inferior Crystal process."
;;  (interactive)
;;  (save-excursion
;;    (crystal-end-of-block)
;;    (end-of-line)
;;    (let ((end (point)))
;;      (crystal-beginning-of-block)
;;      (crystal-send-region (point) end))))
;;
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

(defun crystal-send-definition-and-go ()
  "Send the current definition to the inferior Crystal.
Then switch to the process buffer."
  (interactive)
  (crystal-send-definition)
  (crystal-switch-to-inf t))

(defun crystal-send-region (start end)
  "Send the current region to the inf-crystal process."
  (interactive "r")
  (let* ((string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (message "Sent: %s..." (match-string 1 string))
    (if (string-match ".\n+." string) ;Multiline
        (progn
          (comint-send-string (inf-crystal-proc) "paste\n")
          (comint-send-string (inf-crystal-proc) string)
          (with-current-buffer (inf-crystal-buffer)
            (comint-send-eof)))
      (comint-send-string (inf-crystal-proc) string))
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string (inf-crystal-proc) "\n"))))

(defun crystal-send-region-and-go (start end)
  "Send the current region to the inferior Crystal process.
Then switch to the process buffer."
  (interactive "r")
  (crystal-send-region start end)
  (crystal-switch-to-inf t))

(defun crystal-send-buffer ()
  "Send the current buffer to the inf-crystal process."
  (interactive)
  (save-restriction
    (widen)
    (crystal-send-region (point-min) (point-max))))

(defun crystal-send-buffer-and-go ()
  "Send the current buffer to the inf-crystal process.
Then switch to the process buffer."
  (interactive)
  (crystal-send-buffer)
  (crystal-switch-to-inf t))

(defvar inf-crystal-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'crystal-send-definition)
    (define-key map (kbd "C-x C-e") 'crystal-send-last-sexp)
    (define-key map (kbd "C-c C-l") 'crystal-send-line)
    (define-key map (kbd "C-c C-b") 'crystal-send-buffer)
    (define-key map (kbd "C-c M-b") 'crystal-send-buffer-and-go)
    (define-key map (kbd "C-c C-x") 'crystal-send-definition)
    (define-key map (kbd "C-c M-x") 'crystal-send-definition-and-go)
    (define-key map (kbd "C-c C-r") 'crystal-send-region)
    (define-key map (kbd "C-c M-r") 'crystal-send-region-and-go)
    (define-key map (kbd "C-c C-z") 'crystal-switch-to-inf)
    (define-key map (kbd "C-c C-s") 'inf-crystal)
    (easy-menu-define
      inf-crystal-minor-mode-menu
      map
      "Inferior Crystal Minor Mode Menu"
      '("Inf-Crystal"
        ["Send last expression" crystal-send-last-sexp t]
        ["Send line" crystal-send-line t]
        ["Send definition" crystal-send-definition t]
        ["Send buffer" crystal-send-buffer t]
        ["Send region" crystal-send-region t]
        "--"
        ["Start REPL" inf-crystal t]
        ["Switch to REPL" crystal-switch-to-inf t]
        ))
    map))

;;;###autoload
(define-minor-mode inf-crystal-minor-mode
  "Minor mode for interacting with the inferior process buffer.

The following commands are available:

\\{inf-crystal-minor-mode-map}"
  :lighter "Icr"
  :keymap inf-crystal-minor-mode-map)

;;;###autoload (add-hook 'crystal-mode-hook 'inf-crystal-minor-mode)

(provide 'inf-crystal)
;;; inf-crystal.el ends here
