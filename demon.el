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

;; Inspired by devil.el by Susam Pal (https://github.com/susam/devil).

;; Demon differs from Devil in two important ways. First, the key
;; translation is powered by regular expressions. Second, it supports
;; "associated" repeated keys -- e.g., after typing , n (C-n), one can
;; keep pressing n to keep issuiing C-n or p to issue C-p.

;; Regular expressions make advanced functionality possible, such as
;; the included "auto control" mode, whereby prefixing a command with
;; , - automatically adds a control modifier to all following key
;; presses. This makes it possible to issue C-x C-s by typing , - x s,
;; rather than , x , s.


;;; Code:

;; Functions in `demon-{pre|post}-regexps' may signal 'demon--quit to
;; cancel the currently entered Demon key.
(define-error 'demon--quit "Quit Demon")

(defvar demon-mode-map (make-sparse-keymap))

(defvar demon-activators (list ",")
  "List of keys that activate `demon' when `demon-mode' is active.")
(dolist (activator demon-activators)
  (define-key demon-mode-map activator #'demon)
  ;; TODO: support `query-replace-map'
  ;; (define-key query-replace-map activator #'demon)
  )
(define-key demon-mode-map (kbd "C-z") #'set-mark-command)

(defvar demon-pre-regexps
  '(("C-g" . keyboard-quit)

    ;; Insert literal comma.
    ("ESC" . (lambda () (signal 'demon--quit nil)))
    ("^, ," . (lambda () (demon--do (insert ",")) (signal 'demon--quit nil)))
    ("^, SPC" . (lambda () (demon--do (insert ", ")) (signal 'demon--quit nil)))
    ("^, RET" . (lambda () (demon--do (insert ",\n")) (signal 'demon--quit nil)))

    ;; Automatically add C- after prefix argument.
    ("^, \\(u \\|\\(?:[0-9] \\)+\\)\\([^0-9 ]+\\) " .
     (lambda ()
       (let ((arg (match-string 1 demon-current-keys))
	     (next (match-string 2 demon-current-keys)))
	 (cond
	  ((string= arg "u ") (setq demon--prefix-argument '(4)))
	  (t (setq demon--prefix-argument (string-to-number (string-replace " " "" arg)))))
	 (setq demon-current-keys (concat "C-" next)))))
    ("^, \\(u \\|\\(?:[0-9] \\)+\\)"  . (lambda () (setq demon-ignore-binding t)))

    ;; Translate modifiers.
    (", m n " . "C-M-")
    (", - .*" .
     (lambda ()
       (let ((string (match-string 0 demon-current-keys)))
	 (setq string (replace-regexp-in-string "^, - " "C-" string))
	 (setq string (replace-regexp-in-string " \\([^ ]+\\)" " C-\\1" string t))
	 (setq demon-current-keys (replace-match string t nil demon-current-keys)))))
    (", m " . "M-")
    (", \\. " . "C-M-")
    (", " . "C-"))
  "Association list of regular expressions and
replacements/functions that are applied to the keys entered when
`demon' is called. See also `demon-post-regexps'.

The list consists of cons cells, the car of which is a regular
expression and the cdr of which is either a string or a function.
If it is a string, then it is used as a replacement for the key
combination matched by the regular expression.

If it is a function, the function is called when the regular
expression matches. No replacement is made by default, but the
called function has access to the dynamically bound variables
`demon-current-regexp', containing the regular expression, and
`demon-current-keys', containing the key string currently
processed, which may be modified by the function.")

(defvar demon-post-regexps
  '(("\\([MsCASH]-\\)+[MsCASH]-" .
     (lambda ()
       "Fix incorrect modifier prefixes."
       (let ((string (match-string 0 demon-current-keys))
	     (modifiers))
	 (dolist (modifier '("A" "C" "H" "M" "S" "s"))
	   (when (string-match-p modifier string)
	     (push modifier modifiers)))
	 (setq demon-current-keys
	       (replace-regexp-in-string demon-current-regexp
					 (concat
					  (string-join (nreverse modifiers) "-")
					  "-")
					 demon-current-keys
					 t))))))
  "Association list of regular expressions and
replacements/functions that are applied after the application of
`demon-pre-regexps'.")

(defmacro demon--do (&rest body)
  `(if (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
       (mc/execute-command-for-all-cursors (lambda () (interactive) ,@body))
     ,@body))

(defvar demon-allow-between-repeats
  (list (kbd "TAB") (kbd "<tab>") (kbd "RET") (kbd "<return>"))
  "List of key bindings to allow between repeats (see
`demon-repeats') without cancelling the active repeat map.")

(defvar demon-repeats
  '(("^C-" "v" "V")
    ("^\\(M\\|C-M\\)-" "v")
    ("^\\([CM]\\|C-M\\)-" "a" "e")
    ("^\\([CM]\\|C-M\\)-" "n" "p")
    ("^\\([CM]\\|C-M\\)-" "f" "b")
    ("^\\([CM]\\|C-M\\)-" "k")
    ("^\\([CM]-\\|C-M-\\|C-x \\)" "DEL")
    ("^[CM]-" "y")
    ("^[CM]-" "d")
    ("^C-" "_" "?")
    ("^C-" "l")
    ("^C-" "SPC")
    ("^C-" "z")
    ("^M-" "<" ">")
    ("^M-" "(" ")")
    ("^M-" "{" "}")
    ("^M-" "r")
    ("^C-x " "[" "]")
    ("^C-x " "{" "}")
    ("^C-x " "o"))
  "Association list of prefixes and repetable suffixes.")

(defvar demon--transient-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) t #'demon--next)
    map))
(defvar demon--auto-control nil)
(defvar demon--last-command nil)
(defvar demon--prefix-argument nil)
(defvar demon--keys "")
(defvar demon--lighter " Demon")

(defvar demon-current-keys ""
  "The Demon key string currently processed, available to functions
in `demon-pre-regexps' and `demon-post-regexps'.")
(defvar demon-current-regexp ""
  "The currently matching regular expression, available to
functions in `demon-pre-regexps' and `demon-post-regexps'.")
(defvar demon-ignore-binding nil
  "When non-nil, Demon ignores the command bound to
`demon-current-keys'. This variable may be used by
`demon-pre-regexps' and `demon-post-regexps' to delay command
execution. It is reset to nil at each key press.")

;;;###autoload
(define-minor-mode demon-mode
  "Local minor mode for Demon key sequences."
  :lighter (:eval demon--lighter)
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
  (demon--show (demon--translate-keys demon--keys))
  (set-transient-map demon--transient-map))

(defun demon--end ())

(defun demon--show (desc)
  (if demon--prefix-argument
      (message "%S %s" demon--prefix-argument desc)
    (message "%s" desc)))

(defun demon--next ()
  (interactive)
  (setq demon--keys (concat demon--keys (key-description (this-command-keys)) " "))
  (setq demon-ignore-binding nil)
  (condition-case nil
      (let ((desc (demon--translate-keys demon--keys)))
	(demon--show desc)
	(demon--try-keys desc))
    (demon--quit (demon--end))
    (quit (demon--end))))

(defun demon--translate-keys (keys)
  (let ((demon-current-keys keys))
    (dolist (regexps (list demon-pre-regexps demon-post-regexps))
      (dolist (regexp-action regexps)
	(let ((demon-current-regexp (car regexp-action))
	      (action (cdr regexp-action))
	      (case-fold-search nil))
	  ;; Perform replacement or call custom function.
	  (save-match-data
	    (when (string-match demon-current-regexp demon-current-keys)
	      (if (stringp action)
		  (setq demon-current-keys
			(replace-regexp-in-string demon-current-regexp
						  action
						  demon-current-keys))
		(funcall action)))))))
    demon-current-keys))

(defun demon--key-binding (keys)
  (let ((start 0)
	(binding (key-binding (kbd keys)))
	(case-fold-search nil))
    (save-match-data
      (while (setq start
		   (string-match "\\(\\(?:[MsCASH]-\\)+\\)\\([A-Z]\\) " keys start))
	(let* ((modifiers (match-string 1 keys))
	       (key (downcase (match-string 2 keys)))
	       (shifted (replace-match (concat modifiers "S-" key) t t keys)))
	  (when-let (found (key-binding (kbd shifted)))
	    (setq keys shifted)
	    (setq binding found)))))
    binding))

(defun demon--try-keys (keys)
  (let ((binding (condition-case nil (demon--key-binding keys) (error nil))))
    (cond ((and (not demon-ignore-binding)
		(commandp binding))
	   (demon--run binding)
	   (unless (demon--try-repeat keys)
	     (demon--end)))
	  ((or demon-ignore-binding
	       (keymapp binding)
	       (string-match-p "[^-]-$" keys))
	   (set-transient-map demon--transient-map))
	  (t
	   (message "Demon: %sis undefined" keys)
	   (demon--end)))))

(defun demon--run (command)
  (let ((current-prefix-arg demon--prefix-argument))
    (setq last-command demon--last-command)
    (setq this-command command)
    (setq this-original-command command)
    (call-interactively command t)))

(defun demon--try-repeat (keys)
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
		(when-let* ((keys (concat real-prefix suffix " "))
			    (binding (demon--key-binding keys)))
		  (define-key map (kbd suffix)
		    (demon--do-repeat real-prefix suffixes binding))))
	      (if (equal map '(keymap))
		  (throw 'match nil)
		(force-mode-line-update)
		(dolist (key demon-allow-between-repeats)
		  (when-let ((binding (key-binding key)))
		    (define-key map key
		      (demon--do-repeat real-prefix suffixes binding))))
		(demon--show-repeat real-prefix suffixes)
		(let ((exit (set-transient-map map t #'demon--end)))
		  (define-key map (kbd "ESC") (lambda ()
						(interactive)
						(funcall exit)
						(demon--end))))))
	    (throw 'match t)))))))

(defun demon--do-repeat (real-prefix suffixes binding)
  (lambda ()
    (interactive)
    (setq demon--last-command last-command)
    (demon--show-repeat real-prefix suffixes)
    (demon--run binding)))

(defun demon--show-repeat (prefix suffixes)
  (if demon--prefix-argument
	    (message "%S %s%s" demon--prefix-argument prefix suffixes)
	  (message "%s%s" prefix suffixes)))

(provide 'demon)
;;; demon.el ends here
