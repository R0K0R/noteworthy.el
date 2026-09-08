;;; noteworthy-typst.el --- Smart editing for Typst  -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)
(require 'cl-lib)
(require 'typst-ts-mode)

(defvar noteworthy-typst-mode-map (make-sparse-keymap)
  "Keymap for noteworthy-typst-mode.")

(define-minor-mode noteworthy-typst-mode
  "Minor mode for Noteworthy Typst bindings."
  :init-value nil
  :lighter " NW"
  :keymap noteworthy-typst-mode-map)

(defun noteworthy-typst--incomplete-context-p ()
  "Guess whether point is in math, code or raw when the AST cannot say.

Half-typed constructs parse as ERROR nodes -- and while typing, that is
most of the time -- so falling back to \"assume markup\" made * and _ pair
inside a formula the moment you typed the opening $."
  (save-excursion
    (let* ((bol (line-beginning-position))
           (before (buffer-substring-no-properties bol (point)))
           (unescaped (lambda (ch)
                        (let ((n 0) (i 0))
                          (while (string-match (regexp-quote (string ch)) before i)
                            (setq i (1+ (match-beginning 0)))
                            (unless (and (> (match-beginning 0) 0)
                                         (eq (aref before (1- (match-beginning 0))) ?\\))
                              (setq n (1+ n))))
                          n))))
      (or (cl-oddp (funcall unescaped ?$))      ; inside a formula
          (cl-oddp (funcall unescaped ?`))      ; inside raw
          ;; a hash starts code and runs to the end of the expression
          (string-match-p "#[A-Za-z0-9_.-]*\\'" before)))))

(defun noteworthy-typst-markup-context-p ()
  "Return t if point is in a Markup context.
Traverses up the AST:
- Returns T if `content` or `source_file` is hit.
- Returns NIL if `math`, `raw`, `string`, a code form or a label is hit.
- Falls back to a textual guess when the parse is incomplete."
  (when (treesit-language-available-p 'typst)
    (let ((node (treesit-node-at (point)))
          (decision 'unknown))
      (while (and node (eq decision 'unknown))
        (let ((type (treesit-node-type node)))
          (cond
           ((member type '("math" "equation" "formula" "raw_span" "raw_blck"
                           "string" "comment" "label" "ref" "url"))
            (setq decision nil))
           ;; A half-typed construct: the tree cannot tell us, so read the text.
           ((equal type "ERROR")
            (setq decision (if (noteworthy-typst--incomplete-context-p) nil t)))
           ((member type '("content" "source_file"))
            (setq decision t))
           ((member type '("let" "set" "show" "import" "include" "for" "while" "if" "return"
                           "call" "field" "ident" "number" "bool" "none" "auto" "code"))
            (setq decision nil))))
        (setq node (treesit-node-parent node)))
      (cond ((eq decision t) t)
            ((null decision) nil)
            ;; Nothing matched at all -- still prefer the textual reading over
            ;; blindly assuming markup.
            (t (not (noteworthy-typst--incomplete-context-p)))))))

(defun noteworthy-typst-get-list-marker ()
  "Return the list prefix if on a list line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([ \t]*\\)\\([-+*]\\|[0-9]+\\.\\)\\([ \t]+\\)")
      (match-string 0))))

(defun noteworthy-typst-get-current-indent ()
  "Return the indentation of the current line as a string."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([ \t]*\\)")
        (match-string 1)
      "")))

(defun noteworthy-typst-get-indent-unit ()
  "Get the indentation unit for this buffer.
Uses `evil-shift-width` if available (set by dtrt-indent or config),
otherwise falls back to `tab-width` or 2 spaces."
  (make-string (or (and (boundp 'evil-shift-width) evil-shift-width)
                   tab-width
                   2)
               ?\s))

(defun noteworthy-typst-smart-newline ()
  "Handle newlines with smart indent detection."
  (interactive)
  (if (and (bound-and-true-p corfu-mode)
           (bound-and-true-p corfu--candidates))
      (call-interactively 'corfu-insert)
    (cond
     ;; Case 1: Expand empty brackets (|)
     ((and (memq (char-before) '(?\( ?\[ ?\{))
           (memq (char-after) '(?\) ?\] ?\})))
      (let ((base-indent (noteworthy-typst-get-current-indent))
            (indent-unit (noteworthy-typst-get-indent-unit)))
        (newline)
        (insert base-indent indent-unit)
        (save-excursion
          (newline)
          (insert base-indent))))

     ;; Case 2: At end of line with opening bracket → add indent
     ((and (eolp)
           (save-excursion
             (back-to-indentation)
             (looking-at ".*[[{(][ \t]*$")))
      (let ((base-indent (noteworthy-typst-get-current-indent))
            (indent-unit (noteworthy-typst-get-indent-unit)))
        (newline)
        (insert base-indent indent-unit)))

     ;; Case 3: Line ends with backslash (forced breakout) -> maintain indent
     ((and (eolp)
           (save-excursion
             (skip-chars-backward " \t")
             (eq (char-before) ?\\)))
      (let ((indent (noteworthy-typst-get-current-indent)))
        (newline)
        (insert indent)))

     ;; Case 4: Continue/End Lists
     ((and (eolp) (noteworthy-typst-get-list-marker))
      (let ((prefix (noteworthy-typst-get-list-marker))
            (current-line-empty-p (save-excursion
                                    (beginning-of-line)
                                    (looking-at-p "^[ \t]*[-+*0-9.]+[ \t]*$"))))
        (if current-line-empty-p
            (progn
              (beginning-of-line)
              (delete-region (point) (line-end-position))
              (newline-and-indent))
          (newline)
          (insert prefix))))

     ;; Case 4: Inside brackets - maintain current indent
     ((save-excursion
        (ignore-errors
          (backward-up-list 1)
          (memq (char-after) '(?\( ?\[ ?\{))))
      (let ((indent (noteworthy-typst-get-current-indent)))
        (newline)
        (insert indent)))

     ;; Default: maintain current indent
     (t 
      (let ((indent (noteworthy-typst-get-current-indent)))
        (newline)
        (insert indent))))))

(defun noteworthy-typst-dedent-line ()
  "Remove one level of indentation from the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((unit (length (noteworthy-typst-get-indent-unit))))
      (when (looking-at (format "^ \\{%d\\}" unit))
        (delete-char unit)))))

(defun noteworthy-typst-indent-line ()
  "Indent the current line by one level when it is a list item.

Typst nests lists by indentation, so TAB on a `-\=', `+\=' or `1.\=' line
should shift the whole item -- `indent-for-tab-command\=' does not, since
typst-ts-mode has no indentation rule that applies here.  Anywhere else,
fall back to the normal TAB behaviour."
  (interactive)
  (if (noteworthy-typst-get-list-marker)
      (save-excursion
        (beginning-of-line)
        (insert (noteworthy-typst-get-indent-unit)))
    (call-interactively (if (bound-and-true-p indent-for-tab-command)
                            #'indent-for-tab-command
                          #'indent-for-tab-command))))

(defun noteworthy-typst-smart-space ()
  "Insert space. If between $$, expand to $ $."
  (interactive)
  (insert " ")
  (when (and (eq (char-before (1- (point))) ?$)
             (eq (char-after) ?$))
    (insert " ")
    (backward-char 1)))

(defun noteworthy-typst-smart-pair (char)
  "Smart pairing for CHAR (* and _). Skip or pair based on context."
  (interactive)
  (cond
   ((eq (char-after) char)
    (forward-char 1))
   ((noteworthy-typst-markup-context-p)
    (insert char char)
    (backward-char 1))
   (t
    (insert char))))

(defun noteworthy-typst-smart-dollar ()
  "Smart $ insertion. Skip if on $, otherwise pair."
  (interactive)
  (if (eq (char-after) ?$)
      (forward-char 1)
    (insert "$$")
    (backward-char 1)))

(defun noteworthy-typst-smart-backtick ()
  "Smart backtick with 1-2-3 logic for code blocks."
  (interactive)
  (cond
   ;; Escape if not markup
   ((not (noteworthy-typst-markup-context-p))
    (insert "`"))
   
   ;; Case 1: Before two backticks (``|) -> Insert 3rd and Expand
   ((and (eq (char-before) ?`)
         (eq (char-before (1- (point))) ?`))
    (insert "`")
    (save-excursion
      (insert "\n\n```")
      (forward-line -1)
      ;; Optional: Indent inside block if needed, but usually not for raw
      ))

   ;; Case 2: Before one backtick (`|) -> Insert 2nd
   ((eq (char-before) ?`)
    (insert "`"))

   ;; Case 3: Empty or other -> Insert 1st (optionally pair)
   (t
    (insert "`"))))

(defun noteworthy-typst-smart-backspace ()
  "Delete matching pairs on backspace."
  (interactive)
  (let ((char-before (char-before))
        (char-after (char-after)))
    (cond
     ((and char-before char-after
           (or (and (eq char-before ?*) (eq char-after ?*))
               (and (eq char-before ?_) (eq char-after ?_))
               (and (eq char-before ?$) (eq char-after ?$))
               (and (eq char-before ?`) (eq char-after ?`))
               (and (eq char-before ?\() (eq char-after ?\)))
               (and (eq char-before ?\[) (eq char-after ?\]))
               (and (eq char-before ?\{) (eq char-after ?\}))))
      (delete-char -1)
      (delete-char 1))
     ((and char-before (eq char-before ?\s)
           char-after (eq char-after ?\s)
           (eq (char-before (1- (point))) ?$)
           (eq (char-after (1+ (point))) ?$))
      (delete-char -1)
      (delete-char 1))
     (t
      (delete-char -1)))))

(provide 'noteworthy-typst)

;;; noteworthy-typst.el ends here
