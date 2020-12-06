;;; trem.el --- my own modal mode

;; Author: treflip <do-you-really-need-it@nowhere.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/trem.el
;; Package-Requires: ((trem-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
;; MIT License

;;; Commentary:
;; Read the source. Based on  xah-fly-keys, ryo-modal, trem.

;;; Code:
(require 'cl-lib)
(require 'org-macs)
(require 'seq)
(require 'avy)
(require 'expand-region)
(require 'the-org-mode-expansions)
(require 'multiple-cursors)
(require 'quail)

;; <<< BEGIN MODE >>>

(defgroup trem nil
  "Introduce native modal editing of your own design"
  :group  'editing
  :tag    "Trem"
  :prefix "trem-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/trem"))

;;;###autoload

(defvar trem-mode-map (make-sparse-keymap)
  "This is Trem mode map, used to translate your keys.")

;;;###autoload
(defun trem-define-key (actual-key target-key)
  "Register translation from ACTUAL-KEY to TARGET-KEY."
  (define-key
    trem-mode-map
    actual-key
    (defalias (make-symbol "trem-translation")
      (lambda ()
        (interactive)
        (let ((binding (key-binding target-key)))
          (unless (or (memq binding '(nil undefined))
                      (keymapp binding))
            (call-interactively binding))))
      `(format "This command translates %s into %s, which calls `%s'."
               (key-description ,actual-key)
               (key-description ,target-key)
               (key-binding     ,target-key)))))

;;;###autoload
(defun trem-define-kbd (actual-kbd target-kbd)
  "Register translation from ACTUAL-KBD to TARGET-KBD.
Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (trem-define-key (kbd actual-kbd) (kbd target-kbd)))

;;;###autoload
(defun trem-remove-key (key)
  "Unregister translation from KEY."
  (define-key trem-mode-map key nil))

;;;###autoload
(defun trem-remove-kbd (kbd)
  "Unregister translation from KBD.
Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (trem-remove-key (kbd kbd)))

;;;###autoload
(define-minor-mode trem-mode
  "Toggle the `trem-mode' minor mode.
With a prefix argument ARG, enable `trem-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.
This minor mode setups translation of key bindings according to
configuration created previously with `trem-define-key' and
`trem-define-keys'."
  nil "CMD" trem-mode-map
  (setq-local cursor-type
              (if trem-mode
                  trem-cursor-type
                (default-value 'cursor-type))))

(defun trem--maybe-activate ()
  "Activate `trem-mode' if current buffer is not minibuffer or blacklisted.
This is used by `trem-global-mode'."
  (unless (or (minibufferp)
              (member major-mode trem-excluded-modes))
    (trem-mode 1)))

;;;###autoload
(define-globalized-minor-mode trem-global-mode
  trem-mode
  trem--maybe-activate)

(defun trem--input-function-advice (fnc key)
  "Call FNC with KEY as argument only when `trem-mode' is disabled.
Otherwise use `list'."
  (funcall (if trem-mode #'list fnc) key))

(advice-add 'quail-input-method :around #'trem--input-function-advice)

;; <<< END MODE >>>


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
   ("-" trem-scroll-up :norepeat t)
   ("=" trem-scroll-down :norepeat t)

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
	     ("j" switch-to-buffer)
	     ("i" save-some-buffers))
    :name "file/buffer")
   ))


;; <<< END BINDINGS >>>

(provide 'trem)
;;; trem.el ends here
