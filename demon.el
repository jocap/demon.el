;;; demon.el --- Minor mode for Devil-like key sequences  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 John Ankarström

;; Author: John Ankarström <john@ankarstrom.se>
;; Maintainer: John Ankarström <john@ankarstrom.se>
;; Version: 0.1
;; Keywords: convenience, abbrev
;; URL: https://github.com/jocap/demon.el

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Inspired by devil.el by Susam Pal
;;; (https://github.com/susam/devil).


;;; Code:

(define-error 'demon--quit "Quit Demon")

(defvar demon-mode-map (make-sparse-keymap))
(defvar demon-activators (list ","))
(dolist (activator demon-activators)
  (define-key demon-mode-map activator #'demon))

(defvar demon-pre-regexps
  '(("C-g" . (lambda (keys) (keyboard-quit)))
    ("^, ," . (lambda (keys) (demon--do (insert ",")) (signal 'demon--quit nil)))
    ("^, SPC" . (lambda (keys) (demon--do (insert ", ")) (signal 'demon--quit nil)))
    ("^, RET" . (lambda (keys) (demon--do (insert ",\n")) (signal 'demon--quit nil)))
    (", m " . "C-M-")
    ("^, z " . "C-")
    (", - .*" . (lambda (keys)
		(setq keys (replace-regexp-in-string "^, - " "C-" keys))
		(setq keys (replace-regexp-in-string " \\([^ ]+\\)" " C-\\1" keys t))))
    (", \\. " . "M-")
    (", " . "C-")))

(defvar demon-post-regexps nil)

(defmacro demon--do (&rest body)
  `(if multiple-cursors-mode
       (mc/execute-command-for-all-cursors (lambda () (interactive) ,@body))
     ,@body))

;; Perhaps all replacements should be performed, and not just the
;; first that matches. Or perhaps demon--keys should be left intact,
;; if it does not match an existing command.

(defvar demon-repeats
  '(("^M-" "<" ">")
    ("^M-" "{" "}")
    ("^\\([CM]\\|C-M\\)-" "v")
    ("^\\([CM]\\|C-M\\)-" "a" "e")
    ("^\\([CM]\\|C-M\\)-" "n" "p")
    ("^\\([CM]\\|C-M\\)-" "f" "b")
    ("^\\([CM]\\|C-M\\)-" "k")
    ("^\\([CM]-\\|C-M-\\|C-x \\)" "DEL")
    ("^C-x " "[" "]")
    ("^C-x " "o")
    ("^C-" "l")
    ("^M-" "r")
    ("^M-" "y")
    ("^C-" "_" "?")))

(defvar demon--transient-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) t #'demon--next)
    map))
(defvar demon--auto-control nil)
(defvar demon--last-command nil)
(defvar demon--prefix-argument nil)
(defvar demon--keys "")

;;;###autoload
(define-minor-mode demon-mode
  "Local minor mode for Demon key sequences."
  :lighter " Demon"
  :keymap demon-mode-map)

;; TODO: term-mode support

;;;###autoload
(define-globalized-minor-mode
  global-demon-mode demon-mode demon-mode)

(defun demon (arg)
  (interactive "P")
  (setq demon--auto-control nil)
  (setq demon--last-command last-command)
  (setq demon--prefix-argument arg)
  (setq demon--keys (concat (key-description (this-command-keys)) " "))
  (set-transient-map demon--transient-map))

(defun demon--next ()
  (interactive)
  (setq demon--keys (concat demon--keys (key-description (this-command-keys)) " "))
  (condition-case nil
      (let ((desc (demon--translate-keys demon--keys)))
	(if demon--prefix-argument
	    (message "%S %s" demon--prefix-argument desc)
	  (message "%s" desc))
	(demon--try-keys desc))
    (demon--quit nil)))

(defun demon--translate-keys (keys)
  (dolist (regexps (list demon-pre-regexps demon-post-regexps))
    (dolist (regexp-action regexps)
      (let ((regexp (car regexp-action))
	    (action (cdr regexp-action)))
	(when (string-match-p regexp keys)
	  ;; Perform replacement or call custom function.
	  (if (stringp action)
	      (setq keys (replace-regexp-in-string regexp action keys))
	    (setq keys (replace-regexp-in-string regexp (funcall action keys) keys)))))))
  keys)

(defun demon--try-keys (keys)
  (setq keys (replace-regexp-in-string " $" "" keys))
  (let ((binding (condition-case nil (key-binding (kbd keys)) (error nil))))
    (cond ((commandp binding)
	   (demon--run binding)
	   (demon--try-repeat keys))
	  ((or (keymapp binding)
	       (string-match-p "[^-]-$" keys))
	   (set-transient-map demon--transient-map))
	  (t
	   (message "Demon: %s is undefined" keys)))))

(defun demon--run (command)
  (let ((current-prefix-arg demon--prefix-argument))
    (setq last-command demon--last-command)
    (setq this-command command)
    (setq this-original-command command)
    (call-interactively command t)))

(defun demon--try-repeat (keys)
  (setq keys (replace-regexp-in-string " $" "" keys))
  (catch 'match
    (dolist (prefix-suffixes demon-repeats)
      (let* ((prefix (car prefix-suffixes))
	     (suffixes (cdr prefix-suffixes))
	     (quoted-suffixes (mapcar #'regexp-quote suffixes))
	     (joined-suffixes (string-join quoted-suffixes "\\|"))
	     (regexp (concat "\\(" prefix "\\)" "\\(" joined-suffixes "\\)")))
	(save-match-data
	  (when (string-match regexp keys)
	    (let ((map (make-sparse-keymap))
		  (real-prefix (match-string 1 keys)))
	      (dolist (suffix suffixes)
		(when-let* ((keys (concat real-prefix suffix))
			    (binding (key-binding (kbd keys))))
		  (define-key map (kbd suffix) (lambda ()
						 (interactive)
						 (setq demon--last-command last-command)
						 (demon--show-repeat real-prefix suffixes)
						 (demon--run binding)))))
	      (unless (equal map '(keymap))
		(demon--show-repeat real-prefix suffixes)
		(set-transient-map map t)))
	    (throw 'match t)))))))

(defun demon--show-repeat (prefix suffixes)
  (if demon--prefix-argument
	    (message "%S %s%s" demon--prefix-argument prefix suffixes)
	  (message "%s%s" prefix suffixes)))

(provide 'demon)
;;; demon.el ends here
