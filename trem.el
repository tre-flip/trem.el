;;; trem.el --- my own modal mode

;; Author: treflip <do-you-really-need-it@nowhere.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/trem.el
;; Package-Requires: ((trem-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
;; MIT License

;;; Commentary:
;; Read the source. Based on kakoune.el, trem-fly-keys, ryo-modal.

;;; Code:
(require 'cl-lib)
(require 'org-macs)
(require 'seq)
(require 'avy)
(require 'subr-x)
(require 'expand-region)
(require 'the-org-mode-expansions)
(require 'multiple-cursors)

;; <<< BEGIN MODAL >>>

;; the following code is a hard fork of ryo-modal

(defvar trem-modal-mode-map (make-sparse-keymap)
  "General bindings in trem-modal-mode.
Major mode specific bindings will be bound to trem-<major-mode>-map instead.")

(defvar trem-modal-cursor-type t
  "Cursor type used in `trem-modal-mode'.  See description of `cursor-type'.")

(defvar trem-modal-bindings-list ()
  "A list of all the bindings in `trem-modal-mode'.
It is more convenient to view this using `trem-modal-bindings'.")

(defvar trem-modal--last-command nil)

(defun trem-modal-repeat ()
  "Repeat last executed command in `trem-modal-map' (or major mode variant).

If you do not want a command to be remembered by `trem-modal-repeat',
add :norepeat t as a keyword."
  (interactive)
  (when trem-modal--last-command
    (command-execute trem-modal--last-command nil nil t)))

(defvar trem-modal--non-repeating-commands '(trem-modal-repeat))

(defvar trem-modal-mode-keymaps nil
  "Holds a list of all trem major mode specific keymaps.")

;; For compability with multiple-cursors
(defvar mc/cmds-to-run-for-all nil)
(defvar mc/cmds-to-run-once nil)

(defun trem-modal-derived-keymaps ()
  "Get trem mode keymaps relevant to the current `major-mode' and/or minor-modes."
  (mapcar (lambda (mode)
            (eval (intern-soft (concat "trem-" (symbol-name mode) "-map"))))
          (seq-filter (lambda (elt) (or (derived-mode-p elt)
                                        (and (boundp elt) (symbol-value elt))))
                      trem-modal-mode-keymaps)))

(defun trem-modal-maybe-store-last-command ()
  "Update `trem-modal--last-command', if `this-command' is repeatable."
  (when trem-modal-mode
    (let ((cmd (lookup-key (apply 'append trem-modal-mode-map
                                  (trem-modal-derived-keymaps))
                           (this-command-keys))))
      (if (and (commandp cmd)
               (not (member cmd trem-modal--non-repeating-commands)))
          (setq trem-modal--last-command cmd)))))

;;;###autoload
(defun trem-modal-key (key target &rest args)
  "Bind KEY to TARGET in `trem-modal-mode.'

TARGET can be one of:

kbd-string   Pressing KEY will simulate TARGET as a keypress.
command      Calls TARGET interactively.
list         Each element of TARGET is sent to `trem-modal-key' again, with
             KEY as a prefix key.  ARGS are copied, except for :name.
             :name will be used by `which-key' (if installed) to name
             the prefix key, if `which-key-enable-extended-define-key'
             is t.
:hydra       If you have hydra installed, a new hydra will be created and
             bound to KEY.  The first element of ARGS should be a list
             containing the arguments sent to `defhydra'.

ARGS should be of the form [:keyword option]... if TARGET is a kbd-string
or a command.  The following keywords exist:

:name      A string, naming the binding.  If ommited get name from TARGET.
:exit      If t then exit `trem-modal-mode' after the command.
:read      If t then prompt for a string to insert after the command.
:mode      If set to a major or minor mode symbol (e.g. 'org-mode) the key will
           only be bound in that mode.
:norepeat  If t then do not become a target of `trem-modal-repeat'.
:then      Can be a quoted list of additional commands that will be run after
           the TARGET.  These will not be shown in the name of the binding.
           (use :name to give it a nickname).
:first     Similar to :then, but is run before the TARGET.
:mc-all    If t the binding's command will be added to `mc/cmds-to-run-for-all'.
           If 0 the binding's command will be added to `mc/cmds-to-run-once'.

If any ARGS other han :mode, :norepeat or :mc-all are given, a
new command named trem:<hash>:<name> will be created. This is to
make sure the name of the created command is unique."
  (cond
   ((listp target)
    (when (and (require 'which-key nil t)
               which-key-enable-extended-define-key
               (plist-get args :name))
      (let ((mode (plist-get args :mode)))
        (if mode
            (let ((map-name (format "trem-%s-map" mode)))
              (unless (intern-soft map-name)
                (set (intern map-name) (make-sparse-keymap))
                (set-keymap-parent (eval (intern map-name))
                                   trem-modal-mode-map)
                (add-to-list 'trem-modal-mode-keymaps mode))
              (define-key (eval (intern map-name)) (kbd key) `(,(plist-get args :name))))
          (define-key trem-modal-mode-map (kbd key) `(,(plist-get args :name))))))
    (mapc (lambda (x)
            ;; Merge :then lists
            (when (and (plist-get (cddr x) :then)
                       (plist-get args :then))
              (setf (cddr x) (plist-put (cddr x) :then (append (plist-get (cddr x) :then)
                                                               (plist-get args :then)))))
            ;; Merge :first lists
            (when (and (plist-get (cddr x) :first)
                       (plist-get args :first))
              (setf (cddr x) (plist-put (cddr x) :first (append (plist-get (cddr x) :first)
                                                                (plist-get args :first)))))
            (apply #'trem-modal-key `(,(concat key " " (car x))
                                     ,@(cdr x)
                                     ,@(org-plist-delete args :name))))
          target))
   ((and (require 'hydra nil t)
         (equal target :hydra))
    (apply #'trem-modal-key `(,key ,(eval `(defhydra ,@(car args))) ,@(cdr args))))
   ((and (symbolp target) (not (functionp target)))
    (error "`%s' isn't a function" (symbol-name target)))
   (t
    (let* ((name (or (plist-get args :name)
                     (if (stringp target)
                         target
                       (symbol-name target))))
           (hash (secure-hash 'md5 (format "%s%s" target args)))
           (docs
            (if (stringp target)
                (if (keymapp (key-binding (kbd target)))
                    (concat "Call keymap " target)
                  (format "%s → %s (`%s')\n\n%s%s"
                          (key-description (kbd key))
                          (key-description (kbd target))
                          (key-binding (kbd target))
                          (documentation (key-binding (kbd target)))
                          (mapconcat #'documentation (plist-get args :then) "\n")))
              (concat (documentation target)
                      (mapconcat #'documentation (plist-get args :then) "\n"))))
           (func
            (cond
             ((thread-first (org-plist-delete args :mode)
                (org-plist-delete :norepeat)
                (org-plist-delete :mc-all))
              (eval
               `(defun ,(intern (concat "trem:" hash ":" name)) ()
                  ,docs
                  (interactive)
                  (dolist (f (quote ,(plist-get args :first)))
                    (if (commandp f)
                        (let ((real-this-command f))
                          (call-interactively f))
                      (apply f nil)))
                  (if (and (stringp ',target)
                           (keymapp (key-binding (kbd ,target))))
                      (progn
                        (when ,(plist-get args :exit) (trem-modal-mode -1))
                        (setq unread-command-events (listify-key-sequence (kbd ',target))))
                    (let ((real-this-command
                           (if (stringp ',target)
                               (key-binding (kbd ,target))
                             ',target)))
                      (call-interactively real-this-command))
                    (dolist (f (quote ,(plist-get args :then)))
                      (if (commandp f)
                          (let ((real-this-command f))
                            (call-interactively f))
                        (apply f nil)))
                    (when ,(plist-get args :exit) (trem-modal-mode -1))
                    (when ,(plist-get args :read) (insert (read-string "Insert: ")))))))
             ((stringp target)
              (if (keymapp (key-binding (kbd target)))
                  ;; TODO: This doesn't seem to work with "keymaps inside of keymaps"
                  (lambda () (interactive)
                    (setq unread-command-events (listify-key-sequence (kbd target))))
                (key-binding (kbd target))))
             (t
              target)))
           (mode (plist-get args :mode)))
      (when (plist-get args :norepeat)
        (add-to-list 'trem-modal--non-repeating-commands func))
      (let ((mc-all (plist-get args :mc-all)))
        (when mc-all
          (if (and (equal mc-all 0) (not (memq func mc/cmds-to-run-for-all)))
              (add-to-list 'mc/cmds-to-run-once func)
            (and (not (memq func mc/cmds-to-run-once))
                 (add-to-list 'mc/cmds-to-run-for-all func)))))
      (if mode
          (let ((map-name (format "trem-%s-map" mode)))
            (unless (intern-soft map-name)
              (set (intern map-name) (make-sparse-keymap))
              (set-keymap-parent (eval (intern map-name))
                                 trem-modal-mode-map)
              (add-to-list 'trem-modal-mode-keymaps mode))
            (define-key (eval (intern map-name)) (kbd key) func))
        (define-key trem-modal-mode-map (kbd key) func))
      (add-to-list 'trem-modal-bindings-list `(,key ,name ,@args))))))

;;;###autoload
(defmacro trem-modal-keys (&rest args)
  "Bind several keys in `trem-modal-mode'.
Typically each element in ARGS should be of the form (key target [keywords]).
The target should not be quoted.
The first argument may be a list of keywords; they're applied to all keys:

  \(:exit t :then '(kill-region)).

See `trem-modal-key' for more information."
  (let ((kw-list
         (if (symbolp (caar args))
             (pop args)
           nil)))
    `(progn
       ,@(mapcar (lambda (x)
                   `(trem-modal-key ,(car x)
                                   ,(if (stringp (cadr x))
                                        (cadr x)
                                      `(quote ,(cadr x)))
                                   ,@(nthcdr 2 x)
                                   ,@kw-list))
                 args))))

;;;###autoload
(defmacro trem-modal-major-mode-keys (mode &rest args)
  "Bind several keys in `trem-modal-mode', but only if major mode is MODE.
ARGS is the same as `trem-modal-keys'."
  `(progn
     ,@(mapcar (lambda (x)
                 `(trem-modal-key ,(car x)
                                 (if ,(stringp (cadr x))
                                     ,(cadr x)
                                   (quote ,(cadr x)))
                                 ,@(nthcdr 2 x)
                                 :mode ,mode))
               args)))

;;;###autoload
(defun trem-modal-command-then-trem (binding &optional command keymap)
  "Define key BINDING to COMMAND in KEYMAP. Then activate `trem-modal-mode'.
If COMMAND is excluded, use what is bound to right now in KEYMAP.
If KEYMAP is excluded, use `current-global-map'."
  (let* ((keymap (or keymap (current-global-map)))
         (command (or command
                      (lookup-key keymap (kbd binding))
                      (user-error "No binding for '%s'" binding)))
         (name (symbol-name command))
         (hash (secure-hash 'md5 (format "%s-then-trem" command)))
         (docs (concat (documentation command)
                       "\n\nThen enter `trem-modal-mode'."))
         (func
          (eval
           `(defun ,(intern (concat "trem:" hash ":" name)) ()
              ,docs
              (interactive)
              (call-interactively ',command)
              (trem-modal-mode 1)))))
    (define-key keymap (kbd binding) func)))

;;;###autoload
(defun trem-modal-set-key (key command)
  "Give KEY a binding as COMMAND in `trem-modal-mode-map'.

This function is meant to be used interactively, if you want to
temporarily bind a key in trem.

See `global-set-key' for more info."
  (interactive "KSet trem key: \nCSet trem key %s to command: ")
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key trem-modal-mode-map key command))

;;;###autoload
(defun trem-modal-unset-key (key)
  "Remove `trem-modal-mode-map' binding of KEY.
KEY is a string or vector representing a sequence of keystrokes.

This function is meant to unbind keys set with `trem-modal-set-key'."
  (interactive "kUnset trem key: ")
  (define-key trem-modal-mode-map key nil))

;;;###autoload
(defun trem-modal-bindings ()
  "Display a buffer of all bindings in `trem-modal-mode'."
  (interactive)
  (let ((key-column-width 18)
        (command-column-width 40))
    (cl-flet ((trem-princ-bindings
               (bindings)
               (mapc (lambda (x)
                       (let ((keywords (nthcdr 2 x)))
                         (princ (concat (car x)
                                        (make-string (- key-column-width (length (car x))) ? )
                                        (cadr x)
                                        (if (plist-get keywords :exit) " → EXIT")
                                        (if (plist-get keywords :read) " → READ")
                                        "\n"))
                         (when (and (not (plist-get keywords :name))
                                    (plist-get keywords :then))
                           (princ (concat (make-string key-column-width ? )
                                          (mapconcat (lambda (x)
                                                       (concat " → " (symbol-name x)))
                                                     (plist-get keywords :then)
                                                     (concat "\n" (make-string key-column-width ? )))
                                          "\n")))))
                     bindings)))
      (with-output-to-temp-buffer "*trem-modal-bindings*"
        (princ (format "Key%s Bound to\n%s %s\n"
                       (make-string (- key-column-width 4) ? )
                       (make-string (1- key-column-width) ?-)
                       (make-string (1- command-column-width) ?-)))
        (setq trem-modal-bindings-list
              (sort trem-modal-bindings-list
                    (lambda (l r)
                      (string< (car l) (car r)))))
        (trem-princ-bindings (cl-remove-if (lambda (x)
                                            (plist-get (nthcdr 2 x) :mode))
                                          trem-modal-bindings-list))
        (let ((modes))
          (mapc (lambda (x)
                  (add-to-list 'modes (plist-get (nthcdr 2 x) :mode)))
                trem-modal-bindings-list)
          (mapc (lambda (x)
                  (when x
                    (princ (format "\n\n%s specific bindings\n%s\n"
                                   (symbol-name x)
                                   (make-string (+ key-column-width command-column-width) ?-)))
                    (trem-princ-bindings (cl-remove-if-not (lambda (binding)
                                                            (equal x (plist-get (nthcdr 2 binding) :mode)))
                                                          trem-modal-bindings-list))))
                modes))))))

;;;###autoload
(define-minor-mode trem-modal-mode
  "Toggle `trem-modal-mode'."
  nil " trem" trem-modal-mode-map
  (if trem-modal-mode
      (progn
        (add-hook 'post-command-hook #'trem-modal-maybe-store-last-command)
        (setq-local cursor-type trem-modal-cursor-type)
        (dolist (map (trem-modal-derived-keymaps))
          (make-local-variable 'minor-mode-overriding-map-alist)
          (push `(trem-modal-mode . ,map) minor-mode-overriding-map-alist)))
    (remove-hook 'post-command-hook #'trem-modal-maybe-store-last-command)
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'trem-modal-mode minor-mode-overriding-map-alist))
    (setq-local cursor-type (default-value 'cursor-type))))

;; use-package integration
(defun trem-modal--extract-commands-from (args)
  "Extract commands from ARGS to enable lazy loading for :trem."
  (let (commands)
    (cl-remove-duplicates
     (dolist (arg args commands)
       (let ((target (cadr arg)))
         (cond
          ((listp target)
           (setq commands (append (trem-modal--extract-commands-from target) commands)))
          ((equal target :hydra)
           (dolist (hydra-term (cadr (cl-third arg)))
             (when (and hydra-term
                        (listp hydra-term)
                        (cadr hydra-term))
               (push (cadr hydra-term) commands))))
          ((not (stringp target))
           (push target commands))))))))

(with-eval-after-load 'use-package-core
  ;; step 1: introduce trem-modal keyword before :bind
  (unless (member :trem use-package-keywords)
    (setq use-package-keywords (use-package-list-insert :trem use-package-keywords :bind)))

  ;; ensure deferred loading
  (when (boundp 'use-package-deferring-keywords)
    (add-to-list 'use-package-deferring-keywords :trem t))

  ;; step 2: normalize
  (defun use-package-normalize/:trem (_name _keyword args)
    "Apply lists of keywords to all keys following that list."
    (let (kwlist sanitized-args)
      (dolist (arg args sanitized-args)
        (cond
         ((symbolp (car arg))
          (setq kwlist arg))
         ((stringp (car arg))
          (push (append arg kwlist) sanitized-args))))))

  ;; step 3: handler
  (defun use-package-handler/:trem (name _keyword arglists rest state)
    "Use-package handler for :trem."

    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands
                                  (trem-modal--extract-commands-from arglists)))
       state)
     `((ignore ,@(mapcar (lambda (arglist)
                           (if (stringp (cadr arglist))
                               `(trem-modal-key ,(car arglist)
                                               ,(cadr arglist)
                                               ,@(nthcdr 2 arglist)
                                               :package ',name)
                             `(trem-modal-key ,(car arglist)
                                             (quote ,(cadr arglist))
                                             ,@(nthcdr 2 arglist)
                                             :package ',name)))
                         arglists))))))


;; <<< BEGIN SHELL >>>
(defun trem-shell-pipe ()
  "Run a shell command on each of the current regions separately and replace the current regions with its output."
  (interactive)
  (let ((command (read-string "Pipe: ")))
    (mc/for-each-cursor-ordered
     (shell-command-on-region (mc/cursor-beg cursor)
                              (mc/cursor-end cursor)
                              command
                              nil
                              1))))
(defun trem-shell-command ()
  "Run a shell command on each of the current regions separately and insert its output before the respective regions."
  (interactive)
  (mc/save-excursion
   (let ((command (read-string "Pipe: ")))
     (mc/for-each-cursor-ordered
      (mc/save-excursion
       (goto-char (mc/cursor-beg cursor))
       (insert
        (with-output-to-string
          (shell-command-on-region (mc/cursor-beg cursor)
                                   (mc/cursor-end cursor)
                                   command
                                   standard-output))))))))
;; <<< END SHELL >>>


;; <<< BEGIN UTILITIES >>>

(defun trem-goto-word-and-mark ()
  "Invoke avy to go to word, maybe mark it."
  (interactive)
  (commmaybe-execute #'avy-goto-word-1)
  (commmaybe-execute #'er/expand-region))

(defun trem-mark-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block."
  (interactive)
  (if (use-region-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

(defun trem-mark-line ()
  "Select current line, or select next line if called again."
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

;; extend it to scroll arbitrary amount of lines
(defun trem-scroll-up ()
  (interactive)
  (scroll-up 2))
(defun trem-scroll-down ()
  (interactive)
  (scroll-down 2))

(defun trem-set-mark-if-inactive () "Set the mark if it isn't active."
       (interactive)
       (unless (use-region-p) (set-mark (point))))

(defun trem-set-mark-here () "Set the mark at the location of the point."
       (interactive) (set-mark (point)))

(defun trem-backward-symbol (count)
  "Move backward COUNT times by symbol."
  (interactive "p")
  (forward-symbol (- count)))

(defun trem-backward-same-syntax (count)
  "Move backward COUNT times by same syntax blocks."
  (interactive "p")
  (forward-same-syntax (- count)))

 

(defun trem-replace-selection ()
  "Replace selection with killed text."
  (interactive)
  (if (use-region-p)
      (progn (delete-region (region-beginning) (region-end))
	         (yank))
    (progn (delete-region (point) (1+ (point)))
	       (yank))))

(defun trem-forward-sexp-maybe-mark ()
  (interactive)
  (sp-beginning-of-next-sexp)
  (backward-char)
  (when trem-marking-flag
    (sp-mark-sexp)))

(defun trem-delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun trem-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it."
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (trem-fly-delete-spaces)
        (progn (trem-fly-delete-spaces)
               (insert " ")))
      )
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (trem-fly-delete-spaces)
        (progn (trem-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (trem-fly-delete-spaces)
        (progn
          (trem-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (trem-fly-delete-spaces)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(defun trem-toggle-mark ()
  "Set mark if it's inactive, deactivate it if it's active."
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
      (command-execute #'set-mark-command)))

(defun trem-reformat-lines ( &optional @length)
  "Reformat current text block or selection into short lines or 1 long line.
When called for the first time, change to one long line. Second call change it to multiple short lines. Repeated call toggles.

If `universal-argument' is called first, use the number value for min length of line. By default, it's 70.
"
  (interactive)
  ;; This command symbol has a property “'is-longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let* (
         (@length (if @length
                      @length
                    (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column )))
         (is-longline-p
          (if (eq last-command this-command)
              (get this-command 'is-longline-p)
            nil))
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "move")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "move")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (progn
      (if current-prefix-arg
          (trem-reformat-to-multi-lines $p1 $p2 @length)
        (if is-longline-p
            (trem-reformat-to-multi-lines $p1 $p2 @length)
          (trem-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-longline-p (not is-longline-p)))))

(defun trem-reformat-whitespaces-to-one-space (@begin @end)
  "Replace whitespaces by one space."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (search-forward "\t" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (re-search-forward "  +" nil "move")
        (replace-match " ")))))

(defun trem-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.

URL `http://ergoemacs.org/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))

(defun trem-append-to-register-1 ()
  "Append current line or text selection to register 1.
When no selection, append current line, with newline char.
See also: `trem-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/emacs_copy_append.html'
Version 2015-12-08 2020-09-08"
  (interactive)
  (let ($p1 $p2)
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (append-to-register ?1 $p1 $p2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Appended to register 1: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(defun trem-paste-from-register-1 ()
  "Paste text from register 1.
See also: `trem-copy-to-register-1', `insert-register'.
URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defvar trem-brackets nil "string of left/right brackets pairs.")
(setq trem-brackets "()[]{}<>＜＞（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠")

(defvar trem-left-brackets '("\""  "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "〘")


  "List of left bracket chars.")
;; (progn
;; ;; make trem-left-brackets based on trem-brackets
;;   (setq trem-left-brackets '())
;;   (dotimes ($x (- (length trem-brackets) 1))
;;     (when (= (% $x 2) 0)
;;       (push (char-to-string (elt trem-brackets $x))
;;             trem-left-brackets)))
;;   (setq trem-left-brackets (reverse trem-left-brackets)))

(defvar trem-right-brackets '("\"" ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "〙")
  "list of right bracket chars.")
;; (progn
;;   (setq trem-right-brackets '())
;;   (dotimes ($x (- (length trem-brackets) 1))
;;     (when (= (% $x 2) 1)
;;       (push (char-to-string (elt trem-brackets $x))
;;             trem-right-brackets)))
;;   (setq trem-right-brackets (reverse trem-right-brackets)))

(defvar trem-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq trem-punctuation-regex "[!\?\"\.,`'#$%&*+:;=@^|~]+")

(defun trem-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `trem-punctuation-regex'

URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-forward trem-punctuation-regex nil t n))

(defun trem-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `trem-forward-punct'

URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-backward trem-punctuation-regex nil t n))

(defun trem-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `trem-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt trem-left-brackets) nil t))

(defun trem-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `trem-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt trem-right-brackets) nil t))

(defun trem-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `trem-left-brackets' and `trem-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt trem-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt trem-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun trem-change-bracket-pairs ( @from-chars @to-chars)
  "Change bracket pairs from one type to another.

For example, change all parenthesis () to square brackets [].

Works on selected text, or current text block.

When called in lisp program, @from-chars or @to-chars is a string of bracket pair. eg \"(paren)\",  \"[bracket]\", etc.
The first and last characters are used. (the middle is for convenience in ido selection.)
If the string contains “,2”, then the first 2 chars and last 2 chars are used, for example  \"[[bracket,2]]\".
If @to-chars is equal to string “none”, the brackets are deleted.

URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
Version 2020-11-01"
  (interactive
   (let (($bracketsList
          '("(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "\"ascii quote\""
            "[[double square,2]]"
            "“curly quote”"
            "‘single quote’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "『white corner』"
            "【lenticular】"
            "〖white lenticular〗"
            "〈angle〉"
            "《double angle》"
            "〔tortoise〕"
            "〘white tortoise〙"
            "⦅white paren⦆"
            "〚white square〛"
            "⦃white curly⦄"
            "〈pointing angle〉"
            "⦑ANGLE WITH DOT⦒"
            "⧼CURVED ANGLE⧽"
            "⟦math square⟧"
            "⟨math angle⟩"
            "⟪math DOUBLE ANGLE⟫"
            "⟮math FLATTENED PARENTHESIS⟯"
            "⟬math WHITE TORTOISE SHELL⟭"
            "❛HEAVY SINGLE QUOTATION MARK ORNAMENT❜"
            "❝HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT❞"
            "❨MEDIUM LEFT PARENTHESIS ORNAMENT❩"
            "❪MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT❫"
            "❴MEDIUM LEFT CURLY ORNAMENT❵"
            "❬MEDIUM LEFT-POINTING ANGLE ORNAMENT❭"
            "❮HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT❯"
            "❰HEAVY LEFT-POINTING ANGLE ORNAMENT❱"
            "none"
            )))
     (list
      (ido-completing-read "Replace this:" $bracketsList )
      (ido-completing-read "To:" $bracketsList ))))
  (let ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil)
               $fromLeft
               $fromRight
               $toLeft
               $toRight)
          (cond
           ((string-match ",2" @from-chars  )
            (progn
              (setq $fromLeft (substring @from-chars 0 2))
              (setq $fromRight (substring @from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring @from-chars 0 1))
              (setq $fromRight (substring @from-chars -1)))))
          (cond
           ((string-match ",2" @to-chars)
            (progn
              (setq $toLeft (substring @to-chars 0 2))
              (setq $toRight (substring @to-chars -2))))
           ((string-match "none" @to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring @to-chars 0 1))
              (setq $toRight (substring @to-chars -1)))))
          (cond
           ((string-match "markdown" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toLeft "FIXEDCASE" "LITERAL")))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toRight "FIXEDCASE" "LITERAL")))))))))))


(defun trem-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun trem-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2019-06-13"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (trem-fly-delete-spaces)
        (progn (trem-fly-delete-spaces)
               (insert " ")))
      )
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (trem-fly-delete-spaces)
        (progn (trem-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (trem-fly-delete-spaces)
        (progn
          (trem-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (trem-fly-delete-spaces)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))


(defun trem-reformat-lines ( &optional @length)
  "Reformat current text block or selection into short lines or 1 long line.
When called for the first time, change to one long line. Second call change it to multiple short lines. Repeated call toggles.
If `universal-argument' is called first, use the number value for min length of line. By default, it's 70."
  (interactive)
  ;; This command symbol has a property “'is-longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let* (
         (@length (if @length
                      @length
                    (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column )))
         (is-longline-p
          (if (eq last-command this-command)
              (get this-command 'is-longline-p)
            nil))
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "move")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "move")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (progn
      (if current-prefix-arg
          (trem-reformat-to-multi-lines $p1 $p2 @length)
        (if is-longline-p
            (trem-reformat-to-multi-lines $p1 $p2 @length)
          (trem-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-longline-p (not is-longline-p)))))


(defun trem-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection."
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))

(defun trem-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version 2016-06-18"
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   (t t)))

(defun trem-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `trem-user-buffer-q'."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (trem-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun trem-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `trem-user-buffer-q'."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (trem-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun trem-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun trem-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defun trem-clean-empty-lines ()
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'."
  (interactive)
  (let ($begin $end)
    (if (use-region-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))

(defun trem-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'."
  (interactive)
  (let ($begin $end)
    (if (use-region-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      (message "white space cleaned"))))

(defun trem-kill-forward-bracket-text ()
  (interactive)
  (backward-kill-sexp -1))

(defun trem-kill-backward-bracket-text ()
  (interactive)
  (backward-kill-sexp))

(defun trem-kill-backward ()
  "Kill selected text or char backward or bracket pair."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char -1)))

(defun trem-kill-forward ()
  "Kill selected text or char forward or bracket pair."
  (interactive)
  (if (use-region-p)
	(kill-region (region-beginning) (region-end))
    (delete-char 1)))

(defun trem-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.
• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
            (progn
              (skip-chars-backward "\n\t ")
              ;; (forward-char )
              )
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun trem-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.
• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "move" ))
    (end-of-line)))

(defun trem-insert-space ()
  (interactive)
  (insert " "))
;; <<< END UTILITIES >>>

;; <<< BEGIN BINDINGS >>>

;; SPC is a general modkey

;;;###autoload
(defun trem-setup-keybinds ()
  "Set up default trem keybindings for normal mode."
  (global-subword-mode 1)

  ;; commands that are repeated for all cursors
  (trem-modal-keys

   (:mc-all t)

   ;; quit
   ("g" "C-g" :norepeat t)
   ("SPC g" "C-g" :name "abort" :norepeat t)

   
   ;; movement keys  
   ("i" previous-line :norepeat t)
   ("j" backward-char :norepeat t)
   ("k" next-line     :norepeat t)
   ("l" forward-char  :norepeat t)

   ("m" trem-backward-left-bracket :norepeat t)
   ("." trem-forward-right-bracket :norepeat t)
   ("," avy-goto-word-1 :norepeat t)

   ("u" backward-word :norepeat t)
   ("o" forward-word :norepeat t)

   ("h" trem-beginning-of-line-or-block)
   (";" trem-end-of-line-or-block)
   
   ;; alternative movement
   ("SPC" (("i" beginning-of-buffer :norepeat t)    
	   ("k" end-of-buffer :norepeat t)))
    
     
   ;; fast marking
   ("d" trem-toggle-mark :norepeat t)
   ("e" er/expand-region :norepeat t)
   ("7" trem-mark-line :norepeat t)
   ("8" trem-mark-block :norepeat t)
   ("9" mark-whole-buffer :norepeat t)
   
  
   ;; recenter/focus, scrolling
   ("a" recenter-top-bottom :norepeat t)
   

   ;; fast text manipulation
   ("/" trem-kill-forward :norepeat t :exit t)
   
   ("f" trem-kill-forward :norepeat t)
   ("s" trem-kill-backward :norepeat t)

   ("w" trem-kill-backward-bracket-text :norepeat t)
   ("r" trem-kill-forward-bracket-text  :norepeat t)

   ("c" kill-ring-save :norepeat t)
   ("v" yank :norepeat t)
   ("SPC v" yank-pop)
   ("t" undo :norepeat t)
   ("y" trem-modal-repeat :norepeat t)
   

   ;; editing, general text manipulation
   ("z" comment-region)
   ("SPC z" uncomment-region)
   

   ;; fast execution
   ("x" execute-extended-command)
   
   ;; slow execution
   ("SPC" (("x" (("s" trem-shell-pipe)
		 ("e" eshell)
		 ("b" eval-buffer)
		 ("r" eval-region)))))
   

   ;; unused keys are blocked
   ("b" nil :norepeat t)
   ("p" nil :norepeat t)
   ("q" nil :norepeat t))

  ;; commands that aren't repeated for each cursor
  (trem-modal-keys
   (:mc-all 0)
   
   ;; fast window management
   ("1" make-frame)
   ("2" delete-window)
   ("3" other-window :norepeat t)
   ("4" split-window-right :norepeat t)
   ("5" split-window-below :norepeat t)
   ("6" delete-other-windows :norepeat t)

   
   ;; fast buffer management
   ("n" trem-next-user-buffer :norepeat t)
   
   ;; slow buffer and file management
   ("SPC f" (("o" find-file)
		 ("d" dired)
		 ("l" list-buffers)
		 ("k" kill-buffer)
		 ("s" save-buffer)
		 ("j" save-some-buffers))
	    :name "file/buffer")
   ))


;; <<< END BINDINGS >>>

(provide 'trem)
;;; trem.el ends here
