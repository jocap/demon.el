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

(defvar demon-mode-map (make-sparse-keymap))
(defvar demon-activators (list "," "."))
(dolist (activator demon-activators)
  (define-key demon-mode-map activator #'demon))

(defvar demon-regexps
  '(("C-g" . keyboard-quit)
    ("^, ," . (lambda () (demon--do (insert ","))))
    ("^, SPC" . (lambda () (demon--do (insert ", "))))
    ("^, <return>" . (lambda () (demon--do (insert ",\n"))))
    (", \\." . "M-")
    ("^\\. \\." . (lambda () (demon--do (insert "."))))
    ("^\\. SPC" . (lambda () (demon--do (insert ". "))))
    ("^\\. <return>" . (lambda () (demon--do (insert ".\n"))))
    ("^\\. ," . (lambda () (demon--do (insert ".")) "C-"))
    ("^\\. \\\"" . (lambda () (demon--do (insert ".\""))))
    ("^\\." . "M-")
    (", \\." . "M-")
    ("^, m" . "C-M-")
    ("^, z" . "C-")
    ("^, -" . demon-auto-control)
    ("^," . "C-")
    (" ," . " C-")))

;; Perhaps all replacements should be performed, and not just the
;; first that matches. Or perhaps demon--keys should be left intact,
;; if it does not match an existing command.

(defvar demon-repeats
  '(("^M-" "<" ">")
    ("^\\([CM]\\|C-M\\)-" "v")
    ("^\\([CM]\\|C-M\\)-" "a" "e")
    ("^\\([CM]\\|C-M\\)-" "n" "p")
    ("^\\([CM]\\|C-M\\)-" "f" "b")
    ("^\\([CM]\\|C-M\\)-" "k")
    ("^\\([CM]-\\|C-M-\\|C-x \\)" "DEL")
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
  (setq demon--keys (key-description (this-command-keys)))
  (set-transient-map demon--transient-map))

(defun demon-auto-control ()
  (setq demon--auto-control t)
  "C-")

(defun demon--next ()
  (interactive)
  (demon--append-keys (this-command-keys))
  (when (demon--translate-keys)
    (if demon--prefix-argument
	(message "%S %s" demon--prefix-argument demon--keys)
      (message "%s" demon--keys))
    (demon--try-keys)))

(defun demon--append-keys (keys)
  (let ((description (key-description keys)))
    (when (and demon--auto-control (not (string= demon--keys "C-")))
      (setq description (concat "C-" description)))
    (setq demon--keys (concat demon--keys " " description))
    (demon--fix-keys)))

(defun demon--fix-keys ()
  (setq demon--keys (replace-regexp-in-string "\\([^-]\\)- " "\\1-" demon--keys))
  ;; (dolist (key '(("RET" . "<return>")
  ;; 		 ("DEL" . "<backspace>")
  ;; 		 ("SPC" . "<space>")
  ;; 		 ("TAB" . "<tab>")))
  ;;   (let* ((from (car key))
  ;; 	   (to (cdr key))
  ;; 	   (regexp (concat "\\([CASHs]-[^ ]*\\)" from)))
  ;;     (message "%S %S" demon--keys regexp)
  ;;     (setq demon--keys
  ;; 	    (replace-regexp-in-string regexp (concat "\\1" to) demon--keys t))))
  )

(defun demon--translate-keys ()
  (catch 'match
    (let ((keys (concat demon--keys " ")))
      (dolist (regexp-action demon-regexps)
	(let ((regexp (car regexp-action))
	      (action (cdr regexp-action)))
	  (when (string-match-p (concat regexp " ") keys)
	    (throw 'match
		   ;; Perform replacement or call custom function.
		   (if (stringp action)
		       (setq demon--keys
			     (replace-regexp-in-string regexp action demon--keys))
		     (setq demon--keys (funcall action)))))))))
  (when demon--keys
    (demon--fix-keys))
  demon--keys)

(defmacro demon--do (&rest body)
  `(if multiple-cursors-mode
       (mc/execute-command-for-all-cursors (lambda () (interactive) ,@body))
     ,@body))

(defun demon--try-keys ()
  (let ((binding (condition-case nil (key-binding (kbd demon--keys)) (error nil))))
    (cond ((commandp binding)
	   (let ((current-prefix-arg demon--prefix-argument))
	     (setq last-command demon--last-command)
	     (setq this-command binding)
	     (setq this-original-command binding)
	     (run-hooks 'pre-command-hook)
	     (call-interactively binding t))
	   (demon--try-repeat))
	  ((or (keymapp binding)
	       (string-match-p "[^-]-$" demon--keys))
	   (set-transient-map demon--transient-map))
	  (t
	   (message "Demon: %s is undefined" demon--keys)))))

(defun demon--try-repeat ()
  (catch 'match
    (dolist (prefix-suffixes demon-repeats)
      (let* ((prefix (car prefix-suffixes))
	     (suffixes (cdr prefix-suffixes))
	     (quoted-suffixes (mapcar #'regexp-quote suffixes))
	     (joined-suffixes (string-join quoted-suffixes "\\|"))
	     (regexp (concat "\\(" prefix "\\)" "\\(" joined-suffixes "\\)")))
	(save-match-data
	  (when (string-match regexp demon--keys)
	    (let ((map (make-sparse-keymap))
		  (real-prefix (match-string 1 demon--keys)))
	      (dolist (suffix suffixes)
		(when-let* ((keys (concat real-prefix suffix))
			    (binding (key-binding (kbd keys))))
		  (define-key map (kbd suffix) binding)))
	      (unless (equal map '(keymap))
		(set-transient-map map t)))
	    (throw 'match t)))))))

(provide 'demon)
;;; demon.el ends here
