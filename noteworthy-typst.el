;;; noteworthy-typst.el --- Smart editing for Typst in Noteworthy  -*- lexical-binding: t; -*-

(require 'treesit)
(require 'typst-ts-mode)

(defvar noteworthy-typst-mode-map (make-sparse-keymap)
  "Keymap for noteworthy-typst-mode.")

(define-minor-mode noteworthy-typst-mode
  "Minor mode for Noteworthy Typst bindings."
  :init-value nil
  :lighter " NW"
  :keymap noteworthy-typst-mode-map)

;;; Context Walker

(defun noteworthy-typst-markup-context-p ()
  "Return t if point is in a Markup context.
Traverses up the AST. 
- Stops and returns T if `content` or `source_file` is hit (Explicit Markup).
- Stops and returns NIL if `math`, `raw`, `string` is hit (Explicit Non-Markup).
- If `code` or other structures are hit, continues checking (might be nested)."
  (when (treesit-language-available-p 'typst)
    (let ((node (treesit-node-at (point)))
          (decision 'unknown))
      (while (and node (eq decision 'unknown))
        (let ((type (treesit-node-type node)))
          (cond
           ;; Explicit Non-Markup contexts -> NO
           ((member type '("math" "equation" "raw_span" "raw_blck" "string" "comment"))
            (setq decision nil))
           
           ;; Explicit Markup contexts -> YES
           ((member type '("content" "source_file"))
            (setq decision t))
           
           ;; Code constructs (if we hit these without hitting 'content' first, we are in code)
           ((member type '("let" "set" "show" "import" "include" "for" "while" "if" "return" 
                           "call" "field" "ident" "number" "bool" "none" "auto"))
            (setq decision nil))))
        (setq node (treesit-node-parent node)))
      ;; Default to T if we hit root or decision is explicitly T
      (or (eq decision t) (eq decision 'unknown)))))

;;; Smart Editing Commands

(defun noteworthy-typst-get-list-marker ()
  "Return the list prefix (indent + marker) if on a list line, or nil."
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
  "Insert newline with smart indentation.
Priority: 1) On list item - continue list marker
          2) Default newline-and-indent"
  (interactive)
  ;; If corfu (or another completion UI) is active, let it handle RET
  (if (and (bound-and-true-p corfu-mode)
           (bound-and-true-p corfu--candidates)) 
      (call-interactively 'corfu-insert)
    (cond
     ;; Case: Inside empty parenthesis `(|)` -> Expand to `( | )`
     ((and (memq (char-before) '(?\( ?\[ ?\{))
           (memq (char-after) '(?\) ?\] ?\})))
      (let ((base-indent (noteworthy-typst-get-current-indent)))
        (insert "\n")
        (insert base-indent)
        (insert "  ") ;; Add indent
        (save-excursion
          (insert "\n")
          (insert base-indent))))
          
     ;; List marker continuation (only if at end of line)
     ((and (eolp) (noteworthy-typst-get-list-marker))
      (let ((prefix (noteworthy-typst-get-list-marker))
            (current-line-empty-p (save-excursion
                                    (beginning-of-line)
                                    (looking-at-p "^[ \t]*[-+*0-9.]+[ \t]*$"))))
        (if current-line-empty-p
            ;; If current list item is empty, delete it (double enter behavior)
            (progn
              (beginning-of-line)
              (delete-region (point) (line-end-position))
              (newline-and-indent))
          ;; Otherwise continue list
          (newline)
          (insert prefix))))
          
     ;; Default
     (t (newline-and-indent)))))

(defun noteworthy-typst-dedent-line ()
  "Remove one level of indentation from the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  ")  ;; 2 spaces
      (delete-char 2))))

(defun noteworthy-typst-smart-space ()
  "Insert space. If between $$, expand to $ $."
  (interactive)
  (insert " ")
  (when (and (eq (char-before (1- (point))) ?$)
             (eq (char-after) ?$))
    ;; We are at $ |$
    ;; We want $ | $
    (insert " ")
    (backward-char 1)))

(defun noteworthy-typst-smart-pair (char)
  "Smart pairing for * and _ (Skip or Pair)."
  (interactive)
  (cond
   ;; If char-after is the same, skip over it (don't insert)
   ((eq (char-after) char)
    (forward-char 1))
   
   ;; Check Context: Only pair if strictly in Markup
   ((noteworthy-typst-markup-context-p)
    (insert char char)
    (backward-char 1))
    
   ;; Default (Non-Markup): Insert single char
   (t
    (insert char))))

(defun noteworthy-typst-smart-dollar ()
  "Smart $ insertion (Skip or Pair)."
  (interactive)
  (if (eq (char-after) ?$)
      (forward-char 1)
    (insert "$$")
    (backward-char 1)))

(defun noteworthy-typst-smart-backtick ()
  "Smart backtick 1-2-3-Skip logic."
  (interactive)
  (cond
   ;; Math: literal (using blacklist from walker indirectly)
   ((not (noteworthy-typst-markup-context-p))
    (insert "`"))
   
   ;; SKIP BLOCK: If looking at ```
   ((and (eq (char-after) ?`)
         (eq (char-after (1+ (point))) ?`)
         (eq (char-after (+ (point) 2)) ?`))
    (forward-char 3))

   ;; SKIP PAIR: If looking at `
   ((eq (char-after) ?`)
    (forward-char 1))

   ;; CREATE BLOCK: If we have 2 backticks behind us
   ((and (eq (char-before) ?`)
         (eq (char-before (1- (point))) ?`))
    ;; We have ``|. Transform to ```|```
    (insert "`")         ; Now ```
    (save-excursion
      (insert "\n```"))) ; Now ```|``` (cursor stays after first triplet)
   
   ;; CREATE PAIR: Default
   (t
    (insert "``")
    (backward-char 1))))

(defun noteworthy-typst-smart-backspace ()
  "Delete matching pairs on backspace."
  (interactive)
  (let ((char-before (char-before))
        (char-after (char-after)))
    (cond
     ;; Check if we're between matching pairs
     ((and char-before char-after
           (or (and (eq char-before ?*) (eq char-after ?*))
               (and (eq char-before ?_) (eq char-after ?_))
               (and (eq char-before ?$) (eq char-after ?$))
               (and (eq char-before ?`) (eq char-after ?`))
               (and (eq char-before ?\() (eq char-after ?\)))
               (and (eq char-before ?\[) (eq char-after ?\]))
               (and (eq char-before ?\{) (eq char-after ?\}))))
      ;; Delete both characters
      (delete-char -1)
      (delete-char 1))
     ;; Special case: $ $ display mode (space between)
     ((and char-before (eq char-before ?\s)
           char-after (eq char-after ?\s)
           (eq (char-before (1- (point))) ?$)
           (eq (char-after (1+ (point))) ?$))
      ;; Delete space and collapse $ $ to $$
      (delete-char -1)
      (delete-char 1))
     ;; Normal backspace
     (t
      (delete-char -1)))))

(provide 'noteworthy-typst)
