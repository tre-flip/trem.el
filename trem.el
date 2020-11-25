;;; trem.el --- my own modal mode

;; Author: treflip <do-you-really-need-it@nowhere.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/trem.el
;; Package-Requires: ((ryo-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
;; MIT License

;;; Commentary:
;; Read the source. Based on kakoune.el.

;;; Code:
(require 'cl-lib)
(require 'ryo-modal)
(require 'expand-region)
(reuiqre 'multiple-cursors)

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

(require 'cl-lib)
(require 'ryo-modal)
(require 'expand-region)
(require 'multiple-cursors)

(defun trem-insert-mode () "Return to insert mode."
       (interactive)
       (ryo-modal-mode 0))

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

(defun trem-x (count)
  "Select COUNT lines from the current line.

Note that trem's x doesn't behave exactly like this,
but I like this behavior better."
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line count))

(defun trem-X (count)
  "Extend COUNT lines from the current line."
  (interactive "p")
  (beginning-of-line)
  (unless (use-region-p) (set-mark (point)))
  (forward-line count))

(defun trem-d (count)
  "Kill selected text or COUNT chars."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char count t)))

(defun trem-p (count)
  "Yank COUNT times after the point."
  (interactive "p")
  (dotimes (_ count) (save-excursion (yank))))

(defun trem-downcase ()
  "Downcase region."
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-region (point) (+ 1 (point)))))

(defun trem-upcase ()
  "Upcase region."
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-region (point) (1+ (point)))))

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

(defun trem-o (count)
  "Open COUNT lines under the cursor and go into insert mode."
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)))

(defun trem-O (count)
  "Open COUNT lines above the cursor and go into insert mode."
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)))

(defun trem-join ()
  "Join the next line to the current one."
  (interactive) (join-line 1))

(defun trem-Y (count)
  "Copy to the end of COUNT lines."
  (interactive "p")
  (save-excursion
    (let ((cur (point)))
      (move-end-of-line count)
      (kill-ring-save cur (point)))))

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

(defun trem-gg (count)
  "Go to the beginning of the buffer or the COUNTth line."
  (interactive "p")
  (goto-char (point-min))
  (when count (forward-line (1- count))))

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

;; <<< END UTILITIES >>>


;; <<< BEGIN BINDINGS >>>

;;;###autoload
(defun trem-setup-keybinds ()
  "Set up default trem keybindings for normal mode."
  (global-subword-mode 1)
  (eval-after-load "text-mode"
    '(ryo-modal-major-mode-keys
      'text-mode
      ;; Region selectors
      (:mc-all t)
      ("M-i" (("w" er/mark-word)
              ("b" er/mark-inside-pairs)
              ("'" er/mark-inside-quotes)
              ("s" er/mark-text-sentence)
              ("p" er/mark-text-paragraph)))
      ("M-a" (("w" er/mark-symbol)
              ("b" er/mark-outside-pairs)
              ("'" er/mark-outside-quotes)
              ("s" er/mark-text-sentence)
              ("p" er/mark-text-paragraph)))))
  ;; (ryo-modal-major-mode-keys
  ;;  'prog-mode
  ;;  ("b" trem-backward-same-syntax :first '(trem-set-mark-here))
  ;;  ("B" trem-backward-same-syntax :first '(trem-set-mark-if-inactive))
  ;;  ("w" forward-same-syntax :first '(trem-set-mark-here))
  ;;  ("W" forward-same-syntax :first '(trem-set-mark-if-inactive)))
  (ryo-modal-keys
   ;; Basic keybindings
   (:mc-all t)
   ("a" forward-char :exit t)
   ("A" move-end-of-line :exit t)
   ("b" backward-word :first '(trem-set-mark-here))
   ("B" backward-word :first '(trem-set-mark-if-inactive))
   ("c" trem-d :exit t)
   ("C" kill-line :exit t)
   ("d" trem-d)
   ("D" kill-line)
   ("f" trem-select-to-char :first '(trem-set-mark-here))
   ("F" trem-select-to-char :first '(trem-set-mark-if-inactive))
   ;; use "g key" isntead of G
   ("g" (("h" beginning-of-line)
         ("j" end-of-buffer)
         ("k" beginning-of-buffer)
         ("g" trem-gg)
         ("l" end-of-line)
         ("i" back-to-indentation)) :first '(trem-deactivate-mark))
   ("G" (("h" beginning-of-line)
         ("j" end-of-buffer)
         ("k" beginning-of-buffer)
         ("g" trem-gg)
         ("l" end-of-line)
         ("i" back-to-indentation)) :first '(trem-set-mark-if-inactive))
   ("g f" find-file-at-point)
   ("G f" find-file-at-point)
   ("g x" trem-exchange)
   ("g X" trem-exchange-cancel)
   ("h" backward-char :first '(trem-deactivate-mark))
   ("H" backward-char :first '(trem-set-mark-if-inactive))
   ("i" trem-insert-mode)
   ("I" back-to-indentation :exit t)
   ("j" next-line :first '(trem-deactivate-mark))
   ("J" next-line :first '(trem-set-mark-if-inactive))
   ("k" previous-line :first '(trem-deactivate-mark))
   ("K" previous-line :first '(trem-set-mark-if-inactive))
   ("l" forward-char :first '(trem-deactivate-mark))
   ("L" forward-char :first '(trem-set-mark-if-inactive))
   ("o" trem-o :exit t)
   ("O" trem-O :exit t)
   ("p" trem-p)
   ("r" trem-replace-char)
   ("R" trem-replace-selection)
   ("t" trem-select-up-to-char :first '(trem-set-mark-here))
   ("T" trem-select-up-to-char :first '(trem-set-mark-if-inactive))
   ("w" forward-word :first '(trem-set-mark-here))
   ("W" forward-word :first '(trem-set-mark-if-inactive))
   ("M-w" forward-symbol :first '(trem-set-mark-here))
   ("M-W" forward-symbol :first '(trem-set-mark-if-inactive))
   ("x" trem-x)
   ("X" trem-X)
   ("y" kill-ring-save)
   ("Y" trem-Y)
   ("." trem-select-again :first '(trem-set-mark-if-inactive))
   ("M-;" exchange-point-and-mark)
   ("`" trem-downcase)
   ("~" trem-upcase)
   ("%" mark-whole-buffer)
   ("M-j" trem-join)
   ("[ [" backward-paragraph :first '(trem-set-mark-here))
   ("] ]" forward-paragraph :first '(trem-set-mark-here))
   (">" trem-indent-right)
   ("<" trem-indent-left)

   ;; Treat arrow keys the same as "hjkl"
   ("<down>" next-line :first '(trem-deactivate-mark))
   ("<S-down>" next-line :first '(trem-set-mark-if-inactive))
   ("<up>" previous-line :first '(trem-deactivate-mark))
   ("<S-up>" previous-line :first '(trem-set-mark-if-inactive))
   ("<right>" forward-char :first '(trem-deactivate-mark))
   ("<S-right>" forward-char :first '(trem-set-mark-if-inactive))
   ("<left>" backward-char :first '(trem-deactivate-mark))
   ("<S-left>" backward-char :first '(trem-set-mark-if-inactive))

   ;; Numeric arguments
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
   ("-" "M--" :norepeat t)

   ;; Multiple cursors
   ("s" mc/mark-all-in-region)
   ("S" mc/split-region)

   ;; Shell commands
   ("|" trem-shell-pipe)
   ("!" trem-shell-command))

  ;; put these here because they shouldn't be repeated for all cursors
  (ryo-modal-keys
   ("[ b" previous-buffer)
   ("] b" next-buffer)))

;; <<< END BINDINGS >>>

(provide 'trem)
;;; trem.el ends here
