;;; trem.el --- my own modal mode

;; Author: treflip <do-you-really-need-it@nowhere.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/trem.el
;; Package-Requires: ((trem-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
;; MIT License

;;; Commentary:
;; Read the source. Based on  xah-fly-keys, ryo-modal, modalka, kakoune.el.

;;; Code:
(require 'cl-lib)
(require 'org-macs)
(require 'seq)
(require 'avy)
(require 'helm)
(require 'helm-gtags)
(require 'expand-region)
(require 'the-org-mode-expansions)
(require 'multiple-cursors)

;; <<< BEGIN SPECIAL VARIABLES >>>

(defvar-local trem-eval-buffer-f #'eval-buffer)
(defvar-local trem-eval-region-f #'eval-region)
(defvar-local trem-shell "bash")

(defvar trem-excluded-modes nil)

;; <<< END SPECIAL VARIABLES >>>


;; <<< BEGIN MODE >>>
(defgroup trem nil
  "Introduce native modal editing of your own design"
  :group  'editing
  :tag    "Trem"
  :prefix "trem-"
  )

;;;###autoload
(defvar trem-mode-map (make-sparse-keymap)
  "This is Trem mode map, used to translate your keys.")
(define-prefix-command 'trem-mode-map-leadkey)
;; TODO: rewrite as a local keymap variable, that can be reassigned when a mode is switched
(define-prefix-command 'trem-mode-map-local)

(defvar trem-cursor-type 'box)

;;;###autoload
(define-minor-mode trem-mode
  "Toggle the `trem-mode' minor mode.
With a prefix argument ARG, enable `trem-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'."
  nil " CMD" trem-mode-map
  (if trem-mode
      (setq-local cursor-type trem-cursor-type)
    (setq-local cursor-type (default-value 'cursor-type))))

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

;; <<< END MODE >>>


;; <<< BEGIN UTILITIES >>>
(defun trem-append-at-eol ()
  "Go to end of line, format it to just one space at the end and leave CMD mode."
  (interactive)
  (end-of-line)
  (just-one-space)
  (trem-global-mode -1))

(defun trem-open-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (trem-global-mode -1))
(defun trem-open-below ()
  "Go to end of line, then newline-and-indent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (trem-global-mode -1))

(defun trem-shell-pipe ()
  "Run a shell command on each of the current regions separately and replace the current regions with its output."
  (interactive)
  (mc/for-each-cursor-ordered
     (shell-command-on-region (mc/cursor-beg cursor)
                              (mc/cursor-end cursor)
                              trem-shell
                              nil
                              1)))

(defun trem-set-eval-functions (reg buf)
  "Sets functions for region and buffer evaluation/compilation"
  (setq-local trem-eval-region-f reg)
  (setq-local trem-eval-buffer-f buf))

(defun trem-toggle-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower."
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

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

(defun trem-scroll-up ()
  (interactive)
  (scroll-up 2))
(defun trem-scroll-down ()
  (interactive)
  (scroll-down 2))
 
(defun trem-replace-selection ()
  "Replace selection with killed text."
  (interactive)
  (if (use-region-p)
      (progn (delete-region (region-beginning) (region-end))
	         (yank))
    (progn (delete-region (point) (1+ (point)))
	       (yank))))

(defun trem-delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem-toggle-mark ()
  "Set mark if it's inactive, deactivate it if it's active."
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
      (command-execute #'set-mark-command)))

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

(defun trem-append-to-register-1 ()
  "Append current line or text selection to register 1.
When no selection, append current line, with newline char.
See also: `trem-paste-from-register-1', `copy-to-register'."
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
See also: `trem-copy-to-register-1', `insert-register'."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defvar trem-brackets nil "string of left/right brackets pairs.")
(setq trem-brackets "()[]{}<>＜＞（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠")

(defvar trem-left-brackets '("\""  "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "〘")


  "List of left bracket chars.")

(defvar trem-right-brackets '("\"" ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "〙")
  "list of right bracket chars.")

(defvar trem-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq trem-punctuation-regex "[!\?\"\.,`'#$%&*+:;=@^|~]+")

(defun trem-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `trem-punctuation-regex'"
  (interactive "p")
  (re-search-forward trem-punctuation-regex nil t n))

(defun trem-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `trem-forward-punct'"
  (interactive "p")
  (re-search-backward trem-punctuation-regex nil t n))

(defun trem-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `trem-left-brackets'."
  (interactive)
  (re-search-backward (regexp-opt trem-left-brackets) nil t))

(defun trem-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `trem-right-brackets'."
  (interactive)
  (re-search-forward (regexp-opt trem-right-brackets) nil t))

(defun trem-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
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
If @to-chars is equal to string “none”, the brackets are deleted."
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
  "Delete all newline around cursor."
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem-reformat-to-multi-lines ( &optional @begin @end @min-length)
  "Replace spaces by a newline at places so lines are not long.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
If `universal-argument' is called first, use the number value for min length of line. By default, it's 70."
  (interactive)
  (let (
        $p1 $p2
        ($blanks-regex "\n[ \t]*\n")
        ($minlen (if @min-length
                     @min-length
                   (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column))))
    (if (and  @begin @end)
        (setq $p1 @begin $p2 @end)
      (if (use-region-p)
          (progn (setq $p1 (region-beginning) $p2 (region-end)))
        (save-excursion
          (if (re-search-backward $blanks-regex nil "move")
              (progn (re-search-forward $blanks-regex)
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward $blanks-regex nil "move")
              (progn (re-search-backward $blanks-regex)
                     (setq $p2 (point)))
            (setq $p2 (point))))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while
            (re-search-forward " +" nil "move")
          (when (> (- (point) (line-beginning-position)) $minlen)
            (replace-match "\n" )))))))

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

(defun trem-kill-forward-bracket-text ()
  (interactive)
  (backward-kill-sexp -1))

(defun trem-kill-backward-bracket-text ()
  (interactive)
  (backward-kill-sexp))

(defun trem-kill-backward ()
  "Kill selected text or char backward or bracket pair."
  (interactive)
  (cond
   ((use-region-p)
    (kill-region (region-beginning) (region-end)))
   ((or (looking-back (regexp-opt trem-left-brackets))
	(looking-back (regexp-opt trem-right-brackets)))
    (condition-case nil
	(progn
	  (backward-char)
	  (trem-delete-pair))
      ((error) (delete-char 1))))
   (t (delete-char -1))))
  
(defun trem-kill-forward ()
  "Kill selected text or char backward or bracket pair."
  (interactive)
  (cond
   ((use-region-p)
    (kill-region (region-beginning) (region-end)))
   ((or (looking-at (regexp-opt trem-left-brackets))
	(looking-at (regexp-opt trem-right-brackets)))
    (condition-case nil
	(trem-delete-pair)
      ((error) (delete-char 1))))
   (t (delete-char 1))))
  
(defun trem-delete-pair ()
  (interactive)
  (let ((!spos (point)))
    (trem-goto-matching-bracket)
    (if (< (point) !spos)
	(progn
	  (delete-pair)
	  (goto-char (- !spos 2)))
      (progn
	(goto-char !spos)
	(delete-pair)))))

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

(defun trem-change ()
  "Kill forward and exit CMD mode"
  (interactive)
  (trem-kill-forward)
  (trem-global-mode -1))

(defun trem-help-map ()
  "Display help for trem's single keystroke keymap"
  (interactive)
  (which-key-show-full-keymap 'trem-mode-map))

(defun trem-help-map-2 ()
  "Display help for trem's double keystroke keymap"
  (interactive)
  (which-key-show-full-keymap 'trem-mode-map-leadkey))

(defun trem-help-map-3 ()
  "Display help for trem's triple keystroke keymap"
  (interactive)
  (which-key-show-full-keymap 'trem-mode-map-3))

(defun trem-eval-buffer ()
  (interactive)
  (command-execute trem-eval-buffer-f))

(defun trem-eval-region ()
  (interactive)
  (command-execute trem-eval-region-f))

(defun trem-split-line-and-quit ()
  (interactive)
  (split-line)
  (trem-global-mode -1))

(defun trem-toggle-highlight ()
  (interactive)
  (let ((!overlays (overlays-at (point))))
    (if !overlays
	(progn
	  (message "found overlay")
	  (mapcar  #'delete-overlay !overlays))
      (overlay-put (make-overlay (region-beginning)
				 (region-end))
		   'face 'highlight))))

(defvar trem-key-pairs (cl-mapcar 'cons
			"qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"	
			"йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"))

(defun trem-bind-rus (m k d)
  (let ((c (cdr (assoc (aref k 0) trem-key-pairs))))
    (when c
      (define-key m (kbd (char-to-string c)) d))))

;; <<< END UTILITIES >>>


;; <<< BEGIN BINDINGS >>>

;; SPC is a general modkey

;;;###autoload
(defun trem-setup-keybinds ()
  "Set up default trem keybindings for normal mode."
  (global-subword-mode 1)

  ;; use flet to shorten define-key
  (cl-flet ((bnd-1 (k d)
		   (define-key trem-mode-map (kbd k) d)
		   (trem-bind-rus trem-mode-map k d))
	    (bnd-2 (k d)
		   (define-key 'trem-mode-map-leadkey (kbd k) d)
		   (trem-bind-rus 'trem-mode-map-leadkey k d))
	    (bnd-3 (k d)
		   (define-key 'trem-mode-map-3 k d)
		   (trem-bind-rus 'trem-mode-map-3 k d)))

    ;; ONE KEYSTROKE COMMANDS ;;

    ;; movement keys  
    (bnd-1 "i" #'previous-line)
    (bnd-1 "j" #'backward-char)
    (bnd-1 "k" #'next-line    )
    (bnd-1 "l" #'forward-char )

    (bnd-1 "m" #'trem-backward-left-bracket)
    (bnd-1 "." #'trem-forward-right-bracket)
    (bnd-1 "," #'avy-goto-word-1 )

    (bnd-1 "u" #'backward-word)
    (bnd-1 "o" #'forward-word)

    (bnd-1 "h" #'trem-beginning-of-line-or-block)
    (bnd-1 ";" #'trem-end-of-line-or-block)
    (bnd-1 "g" #'beginning-of-buffer)
    
    ;; fast marking
    (bnd-1 "d" #'trem-toggle-mark)
    (bnd-1 "e" #'er/expand-region)
    (bnd-1 "7" #'trem-mark-line)
    (bnd-1 "8" #'mark-paragraph)
    
    ;; recenter/focus, alt scrolling
    (bnd-1 "a" #'recenter-top-bottom)
    (bnd-1 "-" #'trem-scroll-down)
    (bnd-1 "=" #'trem-scroll-up)

    ;; fast text manipulation
    (bnd-1 "/" #'trem-change)
    (bnd-1 "f" #'trem-kill-forward)
    (bnd-1 "s" #'trem-kill-backward)
    (bnd-1 "w" #'trem-kill-backward-bracket-text)
    (bnd-1 "r" #'trem-kill-forward-bracket-text )
    (bnd-1 "c" #'kill-ring-save)
    (bnd-1 "v" #'yank)
    (bnd-1 "t" #'undo)
    (bnd-1 "y" #'trem-append-at-eol)
    (bnd-1 "z" #'comment-region)
    (bnd-1 "b" #'trem-toggle-case)
    (bnd-1 "9" #'trem-open-below)
    (bnd-1 "0" #'trem-open-above)
    (bnd-1 "p" #'trem-replace-selection)
    
    ;; fast execution
    (bnd-1 "x" #'helm-M-x)
    
    ;; fast window management
    (bnd-1 "1" #'make-frame)
    (bnd-1 "2" #'delete-window)
    (bnd-1 "3" #'other-window)
    (bnd-1 "4" #'split-window-right)
    (bnd-1 "5" #'split-window-below)
    (bnd-1 "6" #'delete-other-windows)
    (bnd-1 "<f5>" #'kill-buffer-and-window)

    ;; fast buffer management
    (bnd-1 "q" #'kill-buffer)
    
    ;; help for this keymap
    (bnd-1 "`" #'trem-help-map)

    ;; search
    (bnd-1 "n" #'helm-occur)
    (bnd-1 "<f7>" #'isearch-forward)
    (define-key isearch-mode-map (kbd "<f7>") #'isearch-done)
    (define-key isearch-mode-map (kbd "S-<f7>") #'isearch-cancel)
    (define-key isearch-mode-map (kbd "<f6>") #'isearch-repeat-backward)
    (define-key isearch-mode-map (kbd "<f8>") #'isearch-repeat-forward)

    ;; prefix for 2-keystroke commands
    (bnd-1 "SPC" 'trem-mode-map-leadkey)
    
    ;; TWO KEYSTROKE COMMANNDS ;;

    (bnd-2 "`" #'trem-help-map-2)
    
    ;; text manipulation
    (bnd-2 "v" #'yank-pop)
    (bnd-2 "r" #'query-replace)
    (bnd-2 "m" #'mc/edit-beginnings-of-lines)
    (bnd-2 "h" #'trem-toggle-highlight)
    (bnd-2 "7" #'trem-reformat-lines)
    (bnd-2 "d" #'mark-whole-buffer)
    
    ;; advanced navigation
    (bnd-2 "i" #'beginning-of-buffer)
    (bnd-2 "k" #'end-of-buffer)
    (bnd-2 "l" #'goto-line)
    (bnd-2 "g" #'end-of-buffer)

    ;; uncomment region
    (bnd-2 "z" #'uncomment-region)

    ;; mode specific buffer/region evaluation
    (bnd-2 "c" #'trem-eval-buffer)
    (bnd-2 "e" #'trem-eval-region)

    (bnd-2 "x" #'repeat)
    
    ;; buffer/file management
    (bnd-2 "o" #'helm-find-files)
    (bnd-2 "n" #'trem-next-emacs-buffer)
    (bnd-2 "j" #'switch-to-buffer)
    (bnd-2 "s" #'save-buffer)
    (bnd-2 "f" #'buffer-menu)
    
    ;; shells
    (bnd-2 "p" #'trem-shell-pipe)
    (bnd-2 "t" #'eshell)
    (bnd-2 "y" #'(lambda () (interactive) (vterm)))
    
    ))


;; <<< END BINDINGS >>>

(provide 'trem)
;;; trem.el ends here
