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

(defvar demon-activators (list "," "'")
  "List of keys that activate `demon' when `demon-mode' is active.")

(defun demon--insert-literal (string)
  (lambda ()
    (when (not demon--prefix-argument)
      (dolist (key (split-string string "" t))
	(let ((last-command-event (string-to-char key)))
	  (call-interactively demon--original-command)))
      (signal 'demon--quit nil))))

(defvar demon-pre-regexps
  `(("C-g" . keyboard-quit)

    ;; Insert literal character.
    ("ESC" . (lambda () (signal 'demon--quit nil)))
    ("^, ," . ,(demon--insert-literal ","))
    ("^, SPC" . ,(demon--insert-literal ", "))
    ("^, RET" . ,(demon--insert-literal ",\n"))
    ("^' '" . ,(demon--insert-literal "'"))
    ("^' SPC" . ,(demon--insert-literal "' "))
    ("^' RET" . ,(demon--insert-literal "'\n"))

    ;; After +, interpret next input literally.
    ("\\+ \\([^ ]+\\) " . "\\1")
    ("\\+ " . "")

    ;; Translate modifiers.
    ("^[,'] - .*" .
     (lambda ()
       (let ((string (match-string 0 demon-current-keys)))
	 (if (string-prefix-p "," string)
	     (progn
	       (setq string (replace-regexp-in-string "^, - " "C-" string))
	       (setq string (replace-regexp-in-string " \\([^ ]+\\)" " C-\\1" string t)))
	   (setq string (replace-regexp-in-string "^' - " "M-" string))
	   (setq string (replace-regexp-in-string " \\([^ ]+\\)" " M-\\1" string t)))
	 (setq demon-current-keys (replace-match string t nil demon-current-keys)))))

    ("' " . "M-")
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

(defvar demon-allow-between-repeats
  (list (kbd "TAB")
	(kbd "<tab>")
	;; (kbd "RET")
	;; (kbd "<return>")
	(kbd "C-SPC"))
  "List of key bindings to allow between repeats (see
`demon-repeats') without cancelling the active repeat map.")

(defvar demon-repeats
  '(("^C-" "v" "V")
    ("^M-" "v")
    ("^C-M-" "v" "V")
    ("^\\([CM]\\|C-M\\)-" "a" "e")
    ("^\\([CM]\\|C-M\\)-" "n" "p")
    ("^\\([CM]\\|C-M\\)-" "f" "b")
    ("^\\([CM]\\|C-M\\)-" "k")
    ("^\\([CM]-\\|C-M-\\|C-x \\)" "DEL")
    ("^C-M-" "u" "d")
    ("^C-M-" "l")
    ("^[CM]-" "y")
    ("^[CM]-" "d")
    ("^C-" "_" "?")
    ("^C-" "s" "r")
    ("^C-" "l")
    ("^C-" "o")
    ("^C-" "SPC")
    ("^C-" "z")
    ("^C-" "K")
    ("^C-" "m")
    ("^C-" "j")
    ("^C-" "§")
    ("^M-" "<" ">")
    ("^\\(C-\\)?M-" "(" ")")
    ("^M-" "{" "}")
    ("^M-" "/")
    ("^M-" "r")
    ("^M-" "u")
    ("^M-" "c")
    ("^M-" "l")
    ("^C-x " "[" "]")
    ("^C-x " "{" "}")
    ("^C-x " "o")
    ("^C-x C-" "x")
    ("^C-c C-" "t")
    ("^C-c " "_" "?")
    ("^C-c w " "w" "a" "s" "d")
    ("^C-c C-" "n" "p"))
  "Association list of prefixes and repetable suffixes.")

(defvar demon--transient-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) t #'demon--next)
    map))
(defvar demon--original-command nil)
(defvar demon--last-command nil)
(defvar demon--prefix-argument nil)
(defvar demon--shifted nil)
(defvar demon--meaningfully-shifted nil)
(defvar demon--keys "")
(defvar demon--lighter " Demon")

(defvar demon-current-keys ""
  "The Demon key string currently processed, available to functions
in `demon-pre-regexps' and `demon-post-regexps'.")
(defvar demon-current-regexp ""
  "The currently matching regular expression, available to
functions in `demon-pre-regexps' and `demon-post-regexps'.")
(defvar demon-log-messages nil
  "When non-nil, Demon messages are logged.  Setting
`demon-log-messages' to nil is equivalent to setting
`message-log-max' to nil.")
(defvar demon-ignore-binding nil
  "When non-nil, Demon ignores the command bound to
`demon-current-keys'. This variable may be used by
`demon-pre-regexps' and `demon-post-regexps' to delay command
execution. It is reset to nil at each key press.")

;;;###autoload
(define-minor-mode demon-mode
  "Local minor mode for Demon key sequences."
  :lighter (:eval demon--lighter)
  (if demon-mode
      (progn
	(with-eval-after-load 'delsel
	  (advice-add 'delete-selection-pre-hook :around #'demon--selection-pre-hook))
	(advice-add 'self-insert-command :around #'demon--advice)
	(advice-add 'isearch-printing-char :around #'demon--advice)
	(advice-add 'Custom-no-edit :around #'demon--advice)
	(dolist (activator demon-activators)
	  (when (boundp 'embark-general-map)
	    (define-key embark-general-map activator #'demon--key-for-undefined))
	  ;; You could just as well simply advise `undefined'.
	  (when (eq (key-binding activator) #'undefined)
	    (local-set-key activator #'demon--key-for-undefined))))
    (dolist (activator demon-activators)
      (when (boundp 'embark-general-map)
	(define-key embark-general-map activator nil))
      (when (eq (key-binding activator) #'demon--key-for-undefined)
	(local-set-key activator #'undefined)))))

;;;###autoload
(define-globalized-minor-mode
  global-demon-mode demon-mode demon-mode)

(defun demon--selection-pre-hook (f &rest r)
  (unless (and demon-mode (member (this-command-keys) demon-activators))
    (apply f r)))

(defun demon--advice (f &rest r)
  (if (and demon-mode (member (this-command-keys) demon-activators)
	   (not (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)))
      (progn
	(setq demon--original-command f)
	(call-interactively #'demon))
    (apply f r)))

(defun demon--key-for-undefined ()
  (interactive)
  (setq demon--original-command #'undefined)
  (call-interactively #'demon))

;; TODO: term-mode support

(defun demon (arg)
  (interactive "P")
  (if (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
      (mc/execute-command-for-all-cursors #'self-insert-command)
    (setq demon--last-command last-command)
    (setq demon--prefix-argument arg)
    (setq demon--shifted nil)
    (setq demon--meaningfully-shifted nil)
    (setq demon--keys (concat (key-description (this-command-keys)) " "))
    (demon--activate)))

(defun demon--activate ()
  (demon--show (demon--translate-keys demon--keys))
  (set-transient-map demon--transient-map))

(defun demon--end ())

(defun demon--message (&rest r)
  (if demon-log-messages
      (apply #'message r)
    (let ((message-log-max nil))
      (apply #'message r))))

(defun demon--show (desc)
  (if demon--prefix-argument
      (demon--message "%S %s" demon--prefix-argument desc)
    (demon--message "%s" desc)))

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

(defun demon--key-binding (keys &optional no-state)
  (let* ((case-fold-search nil)
	 (parts (split-string keys "-"))
	 (shifted-keys (string-join (mapcar
				     (lambda (part)
				       (save-match-data
					 (when (string-match "^\\([A-Z]\\)\\( .*\\)" part)
					   (setq part (concat
						       "S-"
						       (downcase (match-string 1 part))
						       (match-string 2 part)))))
				       part)
				     parts)
				    "-"))
	 (shifted-binding (key-binding (kbd shifted-keys))))
    (unless no-state
      (setq demon--meaningfully-shifted (and shifted-binding t))
      (setq demon--shifted (string-match-p "^\\([A-Z]\\) " (car (last parts)))))
    (or shifted-binding
	(key-binding (kbd keys))
	(key-binding (kbd (string-replace "S-" "" shifted-keys))))))

(defun demon--unshift-keys (keys)
  (string-join (mapcar
		(lambda (part)
		  (save-match-data
		    (when (string-match "^\\([A-Z]\\)\\( .*\\)" part)
		      (setq part (concat
				  (downcase (match-string 1 part))
				  (match-string 2 part)))))
		  part)
		(split-string keys "-"))
	       "-"))

(defun demon--try-keys (keys)
  (let ((binding (condition-case nil (demon--key-binding keys) (error nil))))
    (when (and (not demon--meaningfully-shifted) demon--shifted)
      (setq keys (demon--unshift-keys keys)))
    (cond ((and (not demon-ignore-binding)
		(commandp binding))
	   (if (memq binding '(universal-argument digit-argument negative-argument))
	       (let ((modifiers (save-match-data
				  (let ((key (car (last (split-string keys)))))
				    (string-match "\\(\\(?:[MsCASH]-\\)+\\)" key)
				    (match-string 1 key)))))
		 (call-interactively binding t)
		 (setq demon--prefix-argument prefix-arg)
		 (setq demon--keys modifiers)
		 (demon--activate))
	     (demon--run keys binding)
	     (unless (demon--try-repeat keys)
	       (demon--end))))
	  ((or demon-ignore-binding
	       (keymapp binding)
	       (string-match-p "[^-]-$" keys))
	   (demon--activate))
	  (t
	   (demon--message "Demon: %s is undefined" (string-trim-right keys))
	   (demon--end)))))

(defvar demon--this-single-command-keys nil)
(defun demon--this-single-command-keys (&rest r)
  demon--this-single-command-keys)

(defun demon--run (keys command &optional shifted)
  (when isearch-mode
    (let ((demon--this-single-command-keys (key-parse keys)))
      (advice-add 'this-single-command-keys :around #'demon--this-single-command-keys)
      (isearch-pre-command-hook)
      (advice-remove 'this-single-command-keys #'demon--this-single-command-keys)))
  (let ((current-prefix-arg demon--prefix-argument))
    (setq last-command demon--last-command)
    (setq this-command command)
    (setq this-original-command command)
    (let ((this-command-keys-shift-translated (or shifted
						  (and (not demon--meaningfully-shifted)
						       demon--shifted))))
      (call-interactively command t))
    (setq demon--shifted nil)))

(defun demon--try-repeat (keys)
  (catch 'match
    (dolist (prefix-suffixes demon-repeats)
      (let* ((prefix (car prefix-suffixes))
	     (suffixes (cdr prefix-suffixes))
	     (shifted-suffixes (mapcar #'upcase suffixes))
	     (quoted-suffixes (mapcar #'regexp-quote suffixes))
	     (joined-suffixes (string-join quoted-suffixes "\\|"))
	     (regexp (concat "\\(" prefix "\\)" "\\(" joined-suffixes "\\)")))
	(save-match-data
	  (when (string-match regexp keys)
	    (let ((map (make-sparse-keymap))
		  (real-prefix (match-string 1 keys))
		  has-suffixes)
	      (dolist (suffix shifted-suffixes)
		(when-let* ((keys (concat real-prefix suffix " "))
			    (binding (demon--key-binding keys t)))
		  (define-key map (kbd suffix)
		    (demon--do-repeat real-prefix suffixes keys binding t))))
	      (dolist (suffix suffixes)
		(when-let* ((keys (concat real-prefix suffix " "))
			    (binding (demon--key-binding keys t)))
		  (define-key map (kbd suffix)
		    (demon--do-repeat real-prefix suffixes keys binding nil))
		  (setq has-suffixes t)))
	      (if (not has-suffixes)
		  (throw 'match nil)
		(force-mode-line-update)
		(dolist (key demon-allow-between-repeats)
		  (when-let ((binding (key-binding key)))
		    (define-key map key
		      (demon--do-repeat real-prefix suffixes key binding nil))))
		(demon--show-repeat real-prefix suffixes)
		(let ((exit (set-transient-map map t #'demon--end)))
		  (define-key map (kbd "<escape>") (lambda ()
						(interactive)
						(funcall exit)
						(demon--end))))))
	    (throw 'match t)))))))

(defun demon--do-repeat (real-prefix suffixes keys binding shifted)
  (lambda ()
    (interactive)
    (setq demon--last-command last-command)
    (demon--show-repeat real-prefix suffixes)
    (demon--run keys binding shifted)))

(defun demon--show-repeat (prefix suffixes)
  (if demon--prefix-argument
      (demon--message "%S %s%s" demon--prefix-argument prefix suffixes)
    (demon--message "%s%s" prefix suffixes)))

(provide 'demon)
;;; demon.el ends here
