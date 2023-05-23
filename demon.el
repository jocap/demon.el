;;; demon.el --- Minor mode for Devil-like key sequences  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 John Ankarström

;; Author: John Ankarström <john@ankarstrom.se>
;; Maintainer: John Ankarström <john@ankarstrom.se>
;; Version: 0.1
;; Keywords: convenience, abbrev
;; URL: https://github.com/jocap/demon

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

(defvar demon-activators (list "," "<"))
(dolist (activator demon-activators)
  (define-key demon-mode-map activator #'demon))

(defvar demon-regexps
  '(("C-g" . keyboard-quit)
    ("^, ," . (lambda () (insert ",")))
    ("^, <space>" . (lambda () (insert ", ")))
    ("^, <return>" . (lambda () (insert ",\n")))
    ("^< <" . (lambda () (insert "<")))
    ("^< <space>" . (lambda () (insert "< ")))
    ("^< <return>" . (lambda () (insert "<\n")))
    ("^<" . ",")
    (", \\." . "M-")
    ("^, m" . "C-M-")
    ("," . "C-")))

(defvar demon-repeats
  '(("^M-" "<" ">")
    ("^\\([CM]\\|C-M\\)-" "a" "e")
    ("^\\([CM]\\|C-M\\)-" "n" "p")
    ("^\\([CM]\\|C-M\\)-" "f" "b")
    ("^C-" "_" "?")))

(defvar demon-mode-map (make-sparse-keymap))
(defvar demon--transient-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) t #'demon--next)
    map))
(defvar demon--last-command nil)
(defvar demon--prefix-argument nil)
(defvar demon--keys "")

;;;###autoload
(define-minor-mode demon-mode
  "Local minor mode for Demon key sequences."
  :lighter " Demon"
  :keymap demon-mode-map)

;;;###autoload
(define-globalized-minor-mode
  global-demon-mode demon-mode demon-mode)

(defun demon (arg)
  (interactive "P")
  (setq demon--last-command last-command)
  (setq demon--prefix-argument arg)
  (setq demon--keys (key-description (this-command-keys)))
  (set-transient-map demon--transient-map))

(defun demon--next ()
  (interactive)
  (demon--append-keys (this-command-keys))
  (when (demon--translate-keys)
    (if demon--prefix-argument
	(message "%S %s" demon--prefix-argument demon--keys)
      (message "%s" demon--keys))
    (demon--try-keys)))

(defun demon--append-keys (keys)
  (setq demon--keys (concat demon--keys " " (key-description keys)))
  (setq demon--keys (string-replace "- " "-" demon--keys))
  (setq demon--keys (string-replace "RET" "<return>" demon--keys))
  (setq demon--keys (string-replace "SPC" "<space>" demon--keys))
  (setq demon--keys (string-replace "TAB" "<tab>" demon--keys)))

(defun demon--translate-keys ()
  (catch 'match
    (dolist (regexp-action demon-regexps)
      (let ((keys (concat demon--keys " "))
	    (regexp (car regexp-action))
	    (action (cdr regexp-action)))
	(when (string-match-p (concat regexp " ") keys)
	  (throw 'match
		 ;; Perform replacement or call custom function.
		 (if (stringp action)
		     (setq demon--keys
			   (replace-regexp-in-string regexp action demon--keys))
		   (setq demon--keys (funcall action))))))))
  (when demon--keys
    (setq demon--keys (string-replace "- " "-" demon--keys)))
  demon--keys)

(defun demon--try-keys ()
  (let ((binding (condition-case nil (key-binding (kbd demon--keys)) (error nil))))
    (cond ((commandp binding)
	   (let ((current-prefix-arg demon--prefix-argument))
	     (setq last-command demon--last-command)
	     (setq this-command binding)
	     (if multiple-cursors-mode
		 (mc/execute-command-for-all-cursors binding)
	       (call-interactively binding t)))
	   (demon--try-repeat))
	  ((or (not binding)
	       (keymapp binding)
	       (string-match-p "-$" demon--keys))
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
