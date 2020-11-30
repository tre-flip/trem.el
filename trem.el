;;; trem.el --- my own modal mode

;; Author: treflip <do-you-really-need-it@nowhere.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/trem.el
;; Package-Requires: ((trem-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
;; MIT License

;;; Commentary:
;; Read the source. Based on kakoune.el.

;;; Code:
(require 'cl-lib)
(require 'org-macs)
(require 'seq)
(require 'subr-x)
(require 'smartparens)
(require 'expand-region)
(require 'the-org-mode-expansions)
(require 'multiple-cursors)

;; <<< BEGIN MODAL >>>

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
  "Bind KEY to TARGET in `trem-modal-mode'.

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


;; <<< BEGIN EXCHANGE >>> 
(defcustom trem-exchange-highlight-face 'highlight
  "Face used to highlight marked area."
  :type 'sexp
  :group 'trem-exchange)

(defvar trem-exchange--position nil "Text position which will be exchanged.")

(defvar trem-exchange--overlays nil "Overlays used to highlight marked area.")

(defun trem-exchange--highlight (beg end)
  "Highlight BEG to END for exchange."
  (let ((o (make-overlay beg end nil t nil)))
    (overlay-put o 'face trem-exchange-highlight-face)
    (add-to-list 'trem-exchange--overlays o)))

(defun trem-exchange--clean ()
  "Clean up after exchange."
  (setq trem-exchange--position nil)
  (mapc 'delete-overlay trem-exchange--overlays)
  (setq trem-exchange--overlays nil))

(defun trem-exchange (beg end)
  "Mark the region from BEG to END for exchange."
  (interactive "r")
  (let ((beg-marker (copy-marker beg t))
        (end-marker (copy-marker end nil)))
    (if (null trem-exchange--position)
        ;; call without trem-exchange--position set: store region
        (progn
          (setq trem-exchange--position (list (current-buffer) beg-marker end-marker))
          ;; highlight area marked to exchange
          (trem-exchange--highlight beg end))
      ;; secondary call: do exchange
      (cl-destructuring-bind
          (orig-buffer orig-beg orig-end) trem-exchange--position
        (trem-exchange--do-swap (current-buffer) orig-buffer
                                   beg-marker end-marker
                                   orig-beg orig-end
                                   #'delete-and-extract-region #'insert)))))

(defun trem-exchange--do-swap (curr-buffer orig-buffer curr-beg curr-end orig-beg
                                              orig-end extract-fn insert-fn)
  "This function does the real exchange work. Here's the detailed steps:

1. call EXTRACT-FN with ORIG-BEG and ORIG-END to extract ORIG-TEXT
from ORIG-BUFFER.
2. call EXTRACT-FN with CURR-BEG and CURR-END to extract CURR-TEXT
from CURR-BUFFER.
3. go to ORIG-BEG and then call INSERT-FN with CURR-TEXT.
4. go to CURR-BEG and then call INSERT-FN with ORIG-TEXT.
After step 2, the two markers of the same beg/end pair (curr or orig)
will point to the same position. So if ORIG-BEG points to the same position
of CURR-END initially, ORIG-BEG and CURR-BEG will point to the same position
before step 3. Because CURR-BEG is a marker which moves after insertion, the
insertion in step 3 will push it to the end of the newly inserted text,
thus resulting incorrect behaviour.
To fix this edge case, we swap two extracted texts before step 3 to
effectively reverse the (problematic) order of two `trem-exchange' calls."
  (if (eq curr-buffer orig-buffer)
      ;; in buffer exchange
      (let ((adjacent  (equal (marker-position orig-beg) (marker-position curr-end)))
            (orig-text (funcall extract-fn orig-beg orig-end))
            (curr-text (funcall extract-fn curr-beg curr-end)))
        ;; swaps two texts if adjacent is set
        (let ((orig-text (if adjacent curr-text orig-text))
              (curr-text (if adjacent orig-text curr-text)))
          (save-excursion
            (goto-char orig-beg)
            (funcall insert-fn curr-text)
            (goto-char curr-beg)
            (funcall insert-fn orig-text))))
    ;; exchange across buffers
    (let ((orig-text (with-current-buffer orig-buffer
                       (funcall extract-fn orig-beg orig-end)))
          (curr-text (funcall extract-fn curr-beg curr-end)))
      (save-excursion
        (with-current-buffer orig-buffer
          (goto-char orig-beg)
          (funcall insert-fn curr-text))
        (goto-char curr-beg)
        (funcall insert-fn orig-text))))
  (trem-exchange--clean))

(defun trem-exchange-cancel ()
  "Cancel current pending exchange."
  (interactive)
  (if (null trem-exchange--position)
      (message "No pending exchange")
    (trem-exchange--clean)
    (message "Exchange cancelled")))

;; <<< END EXCHANGE >>> 

;; <<< BEGIN UTILITIES >>>

(defun trem-mark-line ()
  "Select current line."
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

(defun trem-mark-block ()
  "Mark between lines"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

;; extend it to scroll arbitrary amount of lines
(defun trem-scroll-up ()
  (interactive)
  (scroll-up 2))
(defun trem-scroll-down ()
  (interactive)
  (scroll-down 2))

(defun trem-insert-mode () "Return to insert mode."
       (interactive)
       (trem-modal-mode 0))

(defun trem-set-mark-if-inactive () "Set the mark if it isn't active."
       (interactive)
       (unless (use-region-p) (set-mark (point))))

(defun trem-set-mark-here () "Set the mark at the location of the point."
       (interactive) (set-mark (point)))

(defun trem-deactivate-mark ()
  "Deactivate the mark.

Deactivate the mark unless mark-region-mode is active."
  (interactive)
  (unless rectangle-mark-mode (deactivate-mark)))

(defun trem-backward-symbol (count)
  "Move backward COUNT times by symbol."
  (interactive "p")
  (forward-symbol (- count)))

(defun trem-backward-same-syntax (count)
  "Move backward COUNT times by same syntax blocks."
  (interactive "p")
  (forward-same-syntax (- count)))

(defvar trem-last-t-or-f ?f
  "Using t or f command sets this variable.")

(defvar-local trem-last-char-selected-to " "
  "This variable is updated by trem-select-to-char.")

(defun trem-select-up-to-char (arg char)
  "Select up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect up to char: ")
  (setq trem-last-char-selected-to char)
  (setq trem-last-t-or-f ?t)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	    (search-forward (char-to-string char) nil nil arg)
	  (backward-char direction))
    (point)))

(defun trem-select-to-char (arg char)
  "Select up to, and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect to char: ")
  (setq trem-last-char-selected-to char)
  (setq trem-last-t-or-f ?f)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	    (search-forward (char-to-string char) nil nil arg))
    (point)))

(defun trem-select-again (&optional count)
  "Expand the selection COUNT times to whatever the last 't' command was."
  (interactive "p")
  (if (eq trem-last-t-or-f ?t)
      (trem-select-up-to-char count trem-last-char-selected-to)
    (trem-select-to-char count trem-last-char-selected-to)))

;; TODO MAKE USE OF SMART-PARENS!
(defun trem-d (count)
  "Kill selected text or COUNT chars."
  (interactive "p")
  (if (equal smartparens-strict-mode t)
      (if (use-region-p)
	  (sp-kill-region (region-beginning) (region-end))
	(sp-delete-char count))
    (if (use-region-p)
	(kill-region (region-beginning) (region-end))
      (delete-char count t)))) 

(defun trem-replace-char (char)
  "Replace selection with CHAR."
  (interactive "cReplace with char: ")
  (mc/execute-command-for-all-cursors
   (lambda () (interactive)
     (if (use-region-p)
         (progn (let ((region-size (- (region-end) (region-beginning))))
	              (delete-region (region-beginning) (region-end))
	              (mc/save-excursion
		           (insert-char char region-size t))))
       (progn (delete-region (point) (1+ (point)))
	          (mc/save-excursion
	           (insert-char char)))))))

(defun trem-replace-selection ()
  "Replace selection with killed text."
  (interactive)
  (if (use-region-p)
      (progn (delete-region (region-beginning) (region-end))
	         (yank))
    (progn (delete-region (point) (1+ (point)))
	       (yank))))

(defun trem-open-below (count)
  "Open COUNT lines under the cursor and go into insert mode."
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)))

(defun trem-open-above (count)
  "Open COUNT lines above the cursor and go into insert mode."
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)))

(defun trem-indent-right (count)
  "Indent the region or COUNT lines right to tab stop."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) 2)
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly beg end 2))))

(defun trem-indent-left (count)
  "Indent the region or COUNT lines left to tab stop."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) -2)
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly beg end -2))))

;; Until this function is accepted upstream, we inline it here
(defun mc/split-region (beg end search)
  "Split region each time SEARCH occurs between BEG and END.
This can be thought of as an inverse to `mc/mark-all-in-region'."
  (interactive "r\nsSplit on: ")
  (let ((case-fold-search nil))
    (if (string= search "")
        (user-error "Empty search term")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (push-mark beg)
        (while (search-forward search end t)
          (save-excursion
            (goto-char (match-beginning 0))
            (mc/create-fake-cursor-at-point))
          (push-mark (match-end 0)))
        (unless (= (point) end)
          (goto-char end))
        (mc/maybe-multiple-cursors-mode)))))

(defun trem-prev-window ()
   (interactive)
   (other-window -1))

;; <<< END UTILITIES >>>


;; <<< BEGIN BINDINGS >>>

;;;###autoload
(defun trem-setup-keybinds ()
  "Set up default trem keybindings for normal mode."
  (global-subword-mode 1)
  (unbind-key (kbd "M-v"))
  (trem-modal-keys

   (:mc-all t)

   ;; movement keys
   ("i" previous-line :norepeat t)
   ("j" backward-char :norepeat t)
   ("k" next-line     :norepeat t)
   ("l" forward-char  :norepeat t)
   ("o" forward-word  :norepeat t)
   ("u" backward-word :norepeat t)

   ;; recenter/focus, scrolling
   ("f" recenter-top-bottom :norepeat t)
   ("." trem-scroll-up :norepeat t)
   ("," trem-scroll-down :norepeat t)

   ;; killing/yanking, undoing, repeating
   ("d" trem-d :norepeat t)
   ("c" kill-ring-save :norepeat t)
   ("v" yank :norepeat t)
   ("V" yank-pop)
   ("t" undo :norepeat t)
   ("r" trem-modal-repeat :norepeat t)
   ("g" "C-g" :norepeat t) ;; universal quit

   ;; editing, general text manipulation (no marking or selection) <TODO>
   ("e" (("k" trem-open-above :norepeat t)
	 ("i" trem-open-below :norepeat t)
	 ("c" capitalize-dwim :norepeat t)
	 ("m" trem-d :exit t :name "modify")
	 ("o" open-line :exit t)
	 ("j" electric-newline-and-maybe-indent :exit t
	  :name "electric open")
	 ("s" split-line)
	 ("S" split-line :exit t)
	 ("u" upcase-dwim :norepeat t)
	 ("l" downcase-dwim :norepeat t)
	 ("h" hlt-highlight-region :norepeat t)
	 ("r" (("r" replace-regexp)
	       ("s" trem-replace-selection)
	       ("c" trem-replace-char))
	  :name "regex-operations")
	 ;; wrap selection into something (to be extended)
	 )
    :name "editing commands")

   ;; execution
   ("x" (("e" execute-extended-command :norepeat t)
	 ("t" eshell :norepeat t)
	 ("s" trem-shell-command :norepeat t)
	 ("p" trem-shell-pipe :norepeat t)))

   ;; navigation
   ("n" (("i" beginning-of-buffer :norepeat t)
	 ("g" "C-g" :name "quit" :norepeat t)
	 ("k" end-of-buffer :norepeat t)
	 ("j" beginning-of-line :norepeat t)
	 ("l" end-of-line :norepeat t)
	 ("n" goto-line :norepeat t)
	 ;; TODO ADD SMARTPARENS MOVEMENT
	 ("s" (("g" "C-g")
	       ("l" sp-next-sexp)
	       ("j" sp-previous-sexp)
	       ("i" sp-backward-up-sexp)
	       ("k" sp-down-sexp))
	  :name "smartparens movement ")
	 ))

   ;; marking
   ("m" (("m" set-mark-command :norepeat t)
	 ("b" mark-whole-buffer :norepeat t)
	 ("i" (("i" er/mark-inside-pairs)
	       ("k" er/mark-outside-quotes))
	  :name "inside")
 	 ("o" (("i" er/mark-outside-pairs)
	       ("k" er/mark-outside-quotes))
	  :name "outside")
 	 ("p" er/mark-paragraph)
	 ("s" er/mark-symbol)
	 ("ts" er/mark-text-sentence)
	 ("e" er/expand-region)
	 ("c" er/contract-region)
	 ("w" er/mark-word)
	 ("l" trem-mark-line)
	 ("B" trem-mark-block)
	 ;; TODO: MARK SEVERAL TEXT OBJECTS OF THE SAME CLASS, MARK LINE
	 )
    :name "marking")

   ("y" (("l" mc/edit-lines)
	 ("n" mc/insert-numbers)
	 ("c" mc/insert-letters))
    :name "multiple cursors")

   ;; numeric arguments
   ("0" "M-0" :norepeat t)
   ("1" "M-1" :norepeat t)
   ("2" "M-2" :norepeat t)
   ("3" "M-3" :norepeat t)
   ("4" "M-4" :norepeat t)
   ("5" "M-5" :norepeat t)
   ("6" "M-6" :norepeat t)
   ("7" "M-7" :norepeat t)
   ("8" "M-8" :norepeat t)
   ("9" "M-9" :norepeat t)
   ("-" "M--" :norepeat t))

  ;; put these here because they shouldn't be repeated for all cursors
  (trem-modal-keys
   (:mc-all 0)
   ;; search BORKED
   ("s" (("s" isearch-forward)
	 ("b" isearch-backward)))

   ;; buffer/frame related commands
   ("b" (("k" kill-buffer)
	 ("s" save-buffer)
	 ("e" eval-buffer)
	 ("a" (("s" save-some-buffers :norepeat t))
	  :name "all-buffers")
	 ("l" list-buffers :norepeat t)
	 ("o" find-file)
	 ("b" switch-to-buffer)))

   ;; exit modal mode
   ("<menu>" trem-modal-mode :norepeat t)

   ;; window management commands
   ("w" (("g" nil :norepeat t :name "abort")
	 ("h" split-window-below :norepeat t)
	 ("v" split-window-right :norepeat t)
	 ("d" delete-window :norepeat t)
	 ("e" enlarge-window)
	 ("s" shrink-window)
	 ("n" make-frame-command :norepeat t)
	 ("u" trem-prev-window)
	 ("o" other-window :norepeat t)))))


;; <<< END BINDINGS >>>

(provide 'trem)
;;; trem.el ends here
