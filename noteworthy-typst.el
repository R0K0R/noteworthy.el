;;; noteworthy-typst.el --- Smart editing for Typst  -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)
(require 'typst-ts-mode)

(defvar noteworthy-typst-mode-map (make-sparse-keymap)
  "Keymap for noteworthy-typst-mode.")

(define-minor-mode noteworthy-typst-mode
  "Minor mode for Noteworthy Typst bindings."
  :init-value nil
  :lighter " NW"
  :keymap noteworthy-typst-mode-map)

(defun noteworthy-typst-markup-context-p ()
  "Return t if point is in a Markup context.
Traverses up the AST:
- Returns T if `content` or `source_file` is hit.
- Returns NIL if `math`, `raw`, `string` is hit."
  (when (treesit-language-available-p 'typst)
    (let ((node (treesit-node-at (point)))
          (decision 'unknown))
      (while (and node (eq decision 'unknown))
        (let ((type (treesit-node-type node)))
          (cond
           ((member type '("math" "equation" "raw_span" "raw_blck" "string" "comment"))
            (setq decision nil))
           ((member type '("content" "source_file"))
            (setq decision t))
           ((member type '("let" "set" "show" "import" "include" "for" "while" "if" "return"
                           "call" "field" "ident" "number" "bool" "none" "auto"))
            (setq decision nil))))
        (setq node (treesit-node-parent node)))
      (or (eq decision t) (eq decision 'unknown)))))

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

(defun noteworthy-typst-smart-newline ()
  "Insert newline with smart indentation for lists and brackets."
  (interactive)
  (if (and (bound-and-true-p corfu-mode)
           (bound-and-true-p corfu--candidates))
      (call-interactively 'corfu-insert)
    (cond
     ;; Case 1: Empty brackets like (|) or [|] or {|}
     ;; -> Expand to multi-line with indent
     ((and (memq (char-before) '(?\( ?\[ ?\{))
           (memq (char-after) '(?\) ?\] ?\})))
      (let ((base-indent (noteworthy-typst-get-current-indent)))
        (insert "\n")
        (insert base-indent)
        (insert "  ")
        (save-excursion
          (insert "\n")
          (insert base-indent))))

     ;; Case 2: At end of line in a list context
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

     ;; Case 3: Inside function call/brackets with content - maintain same indent
     ((save-excursion
        (let ((start (point)))
          (ignore-errors
            (backward-up-list 1)
            (and (memq (char-after) '(?\( ?\[ ?\{))
                 (< (point) start)))))
      (let ((prev-indent (noteworthy-typst-get-current-indent)))
        (newline)
        (insert prev-indent)))

     ;; Default: standard newline-and-indent
     (t (newline-and-indent)))))

(defun noteworthy-typst-dedent-line ()
  "Remove one level of indentation from the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  ")
      (delete-char 2))))

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
