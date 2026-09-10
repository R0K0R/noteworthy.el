;;; noteworthy-typst.el --- Smart editing for Typst  -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)
(require 'cl-lib)
(require 'typst-ts-mode)

;; Optional: TAB dispatches to yasnippet when it is installed (see
;; `noteworthy-snippets').
(declare-function yas-expand "yasnippet")
(declare-function yas-next-field "yasnippet" (&optional arg))
(declare-function yas-prev-field "yasnippet" (&optional arg))
(declare-function yas-active-snippets "yasnippet" (&optional beg end))
(declare-function yas--templates-for-key-at-point "yasnippet")

(defvar noteworthy-typst-mode-map (make-sparse-keymap)
  "Keymap for noteworthy-typst-mode.")

(define-minor-mode noteworthy-typst-mode
  "Minor mode for Noteworthy Typst bindings."
  :init-value nil
  :lighter " NW"
  :keymap noteworthy-typst-mode-map)

;; Org-style structure editing.  Typst headings are "=" runs and lists nest by
;; indentation, so promote/demote/move all have direct meanings -- and these
;; are the keys muscle memory already knows from org-mode.
(let ((map noteworthy-typst-mode-map))
  (define-key map (kbd "M-<return>") #'noteworthy-typst-meta-return)
  (define-key map (kbd "M-RET")      #'noteworthy-typst-meta-return)
  ;; hjkl rather than the arrows: uppercase M-HJKL already scrolls the PDF,
  ;; so the lowercase row was free and keeps the hand home.
  (define-key map (kbd "M-l")        #'noteworthy-typst-demote)
  (define-key map (kbd "M-h")        #'noteworthy-typst-promote)
  (define-key map (kbd "M-j")        #'noteworthy-typst-move-line-down)
  (define-key map (kbd "M-k")        #'noteworthy-typst-move-line-up)
  (define-key map (kbd "C-c C-n")    #'noteworthy-typst-next-heading)
  (define-key map (kbd "C-c C-p")    #'noteworthy-typst-previous-heading)
  (define-key map (kbd ")")          #'noteworthy-typst-close-paren)
  (define-key map (kbd "]")          #'noteworthy-typst-close-bracket)
  (define-key map (kbd "}")          #'noteworthy-typst-close-brace)
  (define-key map (kbd "C-)")        #'noteworthy-typst-insert-close-paren)
  (define-key map (kbd "C-]")        #'noteworthy-typst-insert-close-bracket)
  (define-key map (kbd "C-}")        #'noteworthy-typst-insert-close-brace))

(defun noteworthy-typst--unclosed-openers ()
  "Positions of the unclosed `(\=', `[\=' and `{\=' before point, innermost first.
Nil inside a string or a comment, where brackets are not structure."
  (let ((ppss (syntax-ppss)))
    (unless (or (nth 3 ppss) (nth 4 ppss))
      (reverse (nth 9 ppss)))))

(defun noteworthy-typst--bracket-context ()
  "Context implied by the brackets still open around point, or nil.

Reading only the current line loses the `#\=' the moment a call spans
lines -- which is the ordinary shape of a cetz canvas:

  #canvas.cartesian-canvas(
    shape.line((1, 0), (1, 1), style: (stroke: (dash: \"dashed\"))),

By the time point is on the `dash:\=' line the `#\=' is four lines up, the
half-typed call is one big ERROR node so the AST cannot settle it either,
and the fallback called that markup -- where a quote does not pair."
  (let ((opens (noteworthy-typst--unclosed-openers)))
    (when opens
      (if (eq (char-after (car opens)) ?\[)
          ;; A content block puts point back in markup no matter how much
          ;; code encloses it -- `#figure(caption: [a |quote here])\='.
          'markup
        (let* ((outer (car (last opens)))
               (before (buffer-substring-no-properties
                        (save-excursion (goto-char outer) (line-beginning-position))
                        outer)))
          (when (string-match-p "#" before) 'code))))))

(defun noteworthy-typst--textual-context ()
  "Guess the context at point by reading the line before it.

Half-typed constructs parse as ERROR nodes -- and while typing, that is
most of the time -- so falling back to \"assume markup\" made * and _ pair
inside a formula the moment you typed the opening $.

Returns `math\=', `raw\=', `code\=' or `markup\='."
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
      (cond
       ((cl-oddp (funcall unescaped ?$)) 'math)     ; inside a formula
       ((cl-oddp (funcall unescaped ?`)) 'raw)      ; inside raw
       ;; Brackets first: they are the only thing that still reads correctly
       ;; once the construct spans lines.
       ((noteworthy-typst--bracket-context))
       ;; A hash starts a code expression that runs to the end of the
       ;; expression, so anything after it on the line is code -- unless a
       ;; bracket appeared, which means either a content block (back to
       ;; markup) or a call that has already closed.
       ((let ((hash (cl-position ?# before :from-end t)))
          (and hash
               (not (string-match-p "[][]" (substring before (1+ hash))))))
        'code)
       (t 'markup)))))

(defun noteworthy-typst--incomplete-context-p ()
  "Return non-nil when the text before point does not read as markup."
  (not (eq (noteworthy-typst--textual-context) 'markup)))

(defconst noteworthy-typst--context-by-node-type
  '(("math" . math) ("formula" . math) ("equation" . math)
    ("raw_span" . raw) ("raw_blck" . raw)
    ("string" . string) ("comment" . comment)
    ("label" . code) ("ref" . code) ("url" . code)
    ("content" . markup) ("source_file" . markup)
    ("code" . code))
  "Which context each decisive tree-sitter node type puts point in.

Only wrappers that settle the question belong here.  `call\=', `ident\=',
`group\=', `attach\=', `number\=' and the statement forms (`let\=', `import\=',
...) are deliberately absent: they occur under `formula\=' and under
`code\=' alike, so stopping at one reports whichever it happens to be
nested in.  `$ sqrt(x) $\=' parses as call > formula > math, and treating
`call\=' as decisive called that code.  Walking past them reaches the real
wrapper -- `code\=' encloses every statement form anyway, and a `content\='
block inside a call correctly lands back in markup.")

(defun noteworthy-typst--let-redisplay-claim-ranges ()
  "Let redisplay learn what a pending edit reparented, before we reparse.

`treesit-parser-changed-regions\=' forces a reparse and reports what moved,
and returns nil when the tree is already current.  Redisplay calls it, via
`treesit--pre-redisplay\=', to decide what needs refontifying -- and what
needs it is often far more than the text typed on.

Asking tree-sitter anything forces that same reparse.  The pairing gate
and every snippet condition do exactly that, from `post-self-insert-hook\=':
after the insert, before redisplay.  So we consumed the answer and
redisplay got nil.  The edit itself lands fine; only the faces go stale,
across whatever the edit reparented.  Typing a `(\=' inside a cetz block
reparents everything after it, so the visible region froze in the
previous tree\='s reading of it -- text correct, highlighting wrong.

Run the marker first and redisplay gets its ranges either way; our reparse
then costs nothing, the tree being current by the time we ask."
  (when (fboundp 'treesit--pre-redisplay)
    (ignore-errors (treesit--pre-redisplay))))

(defun noteworthy-typst-context ()
  "Return the syntactic context at point as a symbol.

One of `markup\=', `math\=', `raw\=', `string\=', `comment\=' or `code\='.
Walks up the AST until a node type decides it; an ERROR node, an
undecidable tree, or a missing grammar fall back to reading the text.

This is the single source of truth for the pairing gate and for the
`# condition:\=' of every snippet that ships with Noteworthy."
  (noteworthy-typst--let-redisplay-claim-ranges)
  (if (not (treesit-language-available-p 'typst))
      (noteworthy-typst--textual-context)
    (let ((node (treesit-node-at (point)))
          (result nil))
      (while (and node (null result))
        (let ((type (treesit-node-type node)))
          (setq result
                (if (equal type "ERROR")
                    (noteworthy-typst--textual-context)
                  (cdr (assoc type noteworthy-typst--context-by-node-type)))))
        (setq node (treesit-node-parent node)))
      (or result (noteworthy-typst--textual-context)))))

(defun noteworthy-typst-in (&rest contexts)
  "Return t when point is in one of CONTEXTS, else nil.
Meant for a snippet\='s `# condition:\=' header, which see in
`noteworthy-snippets\='."
  (and (memq (noteworthy-typst-context) contexts) t))

(defun noteworthy-typst-auto (&rest contexts)
  "Return `auto\=' when point is in one of CONTEXTS, else nil.

A snippet whose condition returns `auto\=' expands the moment its key is
typed -- the `A\=' option in Obsidian\='s LaTeX Suite.  It stays expandable
with TAB as well.

The key fires even glued to what precedes it, so `xsr\=' expands just like
`x sr\='.  That is the right default for postfix operators on a variable,
and the wrong one for anything spelled like the start of a longer word;
use `noteworthy-typst-auto-word\=' for those."
  (and (apply #'noteworthy-typst-in contexts) 'auto))

(defun noteworthy-typst-auto-word (&rest contexts)
  "Return `auto-word\=' when point is in one of CONTEXTS, else nil.

Like `noteworthy-typst-auto\=', but the key has to start a word -- the `w\='
option in Obsidian\='s LaTeX Suite.  Without it a key like `sin\=' would fire
in the middle of `arcsin\='."
  (and (apply #'noteworthy-typst-in contexts) 'auto-word))

(defun noteworthy-typst-markup-context-p ()
  "Return t if point is in a Markup context.
Deliberately nil when the Typst grammar is unavailable: without a parse
the pairing gate would rather do nothing than guess wrong."
  (when (treesit-language-available-p 'typst)
    (eq (noteworthy-typst-context) 'markup)))

(defun noteworthy-typst-math-kind ()
  "Return `block\=' or `inline\=' for the formula around point, else nil.

Typst decides this syntactically: a formula is displayed as a block when
whitespace directly follows the opening `$\=' and directly precedes the
closing one, and is set inline otherwise.

A formula being typed can answer provisionally, so prefer the reading
that is right either way -- `limits(lim)\=' renders byte-identically to
`lim\=' in block math, so guessing `inline\=' costs only verbosity."
  (let ((bounds (noteworthy-typst-dollar-bounds)))
    (when bounds
      (let ((after (char-after (1+ (car bounds))))
            (before (char-before (cdr bounds))))
        (if (and after before
                 (memq after '(?\s ?\t ?\n))
                 (memq before '(?\s ?\t ?\n)))
            'block
          'inline)))))

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

(defun noteworthy-typst--exit-indent ()
  "Indentation for the line that ends a list.

A list ends back in whatever encloses it: the body of a content block or
call when there is one, otherwise the left margin.  Carrying the item\='s
own indentation across is what left every following line indented once
the list was over -- and since the default branch of
`noteworthy-typst-smart-newline\=' copies the current line\='s indent, that
then propagated to every line after it."
  (let ((opens (noteworthy-typst--unclosed-openers)))
    (if (null opens)
        ""
      (save-excursion
        (goto-char (car opens))
        (concat (noteworthy-typst-get-current-indent)
                (noteworthy-typst-get-indent-unit))))))

(defun noteworthy-typst-smart-newline ()
  "Handle newlines with smart indent detection."
  (interactive)
  (if (and (bound-and-true-p corfu-mode)
           (bound-and-true-p corfu--candidates))
      (call-interactively 'corfu-insert)
    (cond
     ;; Case 1: Expand an empty pair, (|) or $|$
     ((or (and (memq (char-before) '(?\( ?\[ ?\{))
               (memq (char-after) '(?\) ?\] ?\})))
          ;; Typst display math is just an inline $...$ broken across lines,
          ;; so the same expansion turns one into the other.
          (and (eq (char-before) ?$) (eq (char-after) ?$)))
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

     ;; Lists are NOT continued here: `noteworthy-typst-meta-return' (M-RET)
     ;; owns that, following org, where RET is a plain newline.  An empty list
     ;; item is still cleaned up, since that is how you end a list.
     ;; `noteworthy-typst-get-list-marker' is deliberately NOT consulted: it
     ;; requires whitespace after the marker, so an empty item -- `+' alone,
     ;; which is exactly how you end a list -- did not look like a list line
     ;; at all.  This branch never fired, the item survived, and the default
     ;; branch below carried its indentation into every line that followed.
     ((and (eolp)
           (save-excursion (beginning-of-line)
                           (looking-at-p "^[ \t]*\\([-+*]\\|[0-9]+\\.\\)[ \t]*$")))
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      ;; Not `newline-and-indent': that re-indents to the list we are leaving.
      (newline)
      (insert (noteworthy-typst--exit-indent)))

     ;; Inside brackets - maintain current indent
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

(defun noteworthy-typst-heading-level ()
  "Return the heading level of the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(=+\\)[ \t]")
      (length (match-string 1)))))

(defun noteworthy-typst-meta-return ()
  "Insert a new list item or heading at the same level, like `org-meta-return'.

On a list line, a new item with the same marker (numbered lists count on).
On a heading, a new heading of the same level.  Anywhere else, a newline."
  (interactive)
  (let ((level (noteworthy-typst-heading-level))
        (marker (noteworthy-typst-get-list-marker)))
    (cond
     (level
      (end-of-line)
      (newline)
      (insert (make-string level ?=) " "))
     (marker
      (end-of-line)
      (newline)
      ;; keep numbering going for "1." style lists
      (if (string-match "\\`\\([ \t]*\\)\\([0-9]+\\)\\.\\([ \t]+\\)\\'" marker)
          (insert (match-string 1 marker)
                  (number-to-string (1+ (string-to-number (match-string 2 marker))))
                  "." (match-string 3 marker))
        (insert marker)))
     (t (newline)))))

(defun noteworthy-typst--shift-line (delta)
  "Promote or demote the current line by DELTA levels."
  (let ((level (noteworthy-typst-heading-level))
        (unit (noteworthy-typst-get-indent-unit)))
    (cond
     (level
      (save-excursion
        (beginning-of-line)
        (if (> delta 0)
            (insert "=")
          (when (> level 1) (delete-char 1)))))
     ((noteworthy-typst-get-list-marker)
      (save-excursion
        (beginning-of-line)
        (if (> delta 0)
            (insert unit)
          (when (looking-at (format "^ \\{%d\\}" (length unit)))
            (delete-char (length unit))))))
     (t (user-error "Not on a heading or list item")))))

(defun noteworthy-typst-demote ()
  "Add a level to the heading or list item at point (M-<right>)."
  (interactive)
  (noteworthy-typst--shift-line 1))

(defun noteworthy-typst-promote ()
  "Remove a level from the heading or list item at point (M-<left>)."
  (interactive)
  (noteworthy-typst--shift-line -1))

(defun noteworthy-typst-move-line-down ()
  "Move the current line down one, like `org-move-item-down'."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(defun noteworthy-typst-move-line-up ()
  "Move the current line up one, like `org-move-item-up'."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun noteworthy-typst-next-heading ()
  "Move to the next heading."
  (interactive)
  (end-of-line)
  (if (re-search-forward "^=+[ \t]" nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (message "No further heading")))

(defun noteworthy-typst-previous-heading ()
  "Move to the previous heading."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward "^=+[ \t]" nil t)
      (beginning-of-line)
    (goto-char (point-min))
    (message "No previous heading")))

(defun noteworthy-typst--snippet-active-p ()
  "Return non-nil when point sits inside a live snippet."
  (and (bound-and-true-p yas-minor-mode)
       (fboundp 'yas-active-snippets)
       (yas-active-snippets)))

(defun noteworthy-typst--expand-snippet-maybe ()
  "Expand the snippet key before point.
Return non-nil when a snippet was expanded, nil when no key matched."
  (and (bound-and-true-p yas-minor-mode)
       (fboundp 'yas--templates-for-key-at-point)
       (yas--templates-for-key-at-point)
       (progn (yas-expand) t)))

(defun noteworthy-typst-dedent-line ()
  "Step back a snippet field, or remove one level of indentation."
  (interactive)
  (if (noteworthy-typst--snippet-active-p)
      (yas-prev-field)
    (save-excursion
      (beginning-of-line)
      (let ((unit (length (noteworthy-typst-get-indent-unit))))
        (when (looking-at (format "^ \\{%d\\}" unit))
          (delete-char unit))))))

(defun noteworthy-typst-indent-line ()
  "Indent the current line by one level when it is a list item.

Typst nests lists by indentation, so TAB on a `-\=', `+\=' or `1.\=' line
should shift the whole item -- `indent-for-tab-command\=' does not, since
typst-ts-mode has no indentation rule that applies here.  Anywhere else,
fall back to the normal TAB behaviour.

Snippets come first: evil binds TAB in its insert state map, which
shadows `yas-minor-mode-map\=', so expansion has to be dispatched here or
it never happens in a Noteworthy buffer."
  (interactive)
  (cond
   ;; Same order yasnippet's own TAB uses: a trigger key wins over field
   ;; navigation, so nested expansion works.
   ((noteworthy-typst--expand-snippet-maybe))
   ((noteworthy-typst--snippet-active-p) (yas-next-field))
   ((noteworthy-typst-get-list-marker)
    (save-excursion
      (beginning-of-line)
      (insert (noteworthy-typst-get-indent-unit))))
   (t (call-interactively #'indent-for-tab-command))))

(declare-function sp-local-pair "smartparens")

(defcustom noteworthy-typst-close-delimiter-tabout t
  "Whether a closing delimiter jumps out of its group instead of inserting.

With this on, `)\=' in `( as|df )\=' leaves point at `( asdf )|\=' -- the
closer is already there, so typing one means \"I am done with this group\".
`]\=' and `}\=' do the same for their own kind.  `C-)\=', `C-]\=' and `C-}\='
always insert a literal one, as does `C-q\='."
  :type 'boolean
  :group 'noteworthy)

(defun noteworthy-typst--enclosing-end (open)
  "Return the position just past the innermost group opened by OPEN.
Nil when point is not inside one, when the innermost group is of some
other kind, or when it is unbalanced."
  (save-excursion
    (ignore-errors
      (backward-up-list 1 t t)
      (when (eq (char-after) open)
        (forward-list 1)
        (point)))))

(defun noteworthy-typst-close-delimiter (close open)
  "Jump past the enclosing OPEN group, or insert CLOSE when there is none.

Matching on kind is deliberate: `]\=' inside `#f(a|b)\=' inserts a bracket
rather than jumping out of the call, because the group it would close is
not the one point is in."
  (if-let* ((end (and noteworthy-typst-close-delimiter-tabout
                     (noteworthy-typst--enclosing-end open))))
      (goto-char end)
    (insert (char-to-string close))))

(defun noteworthy-typst-close-paren ()
  "Jump past the enclosing `()\=' group, or insert `)\='."
  (interactive)
  (noteworthy-typst-close-delimiter ?\) ?\())

(defun noteworthy-typst-close-bracket ()
  "Jump past the enclosing `[]\=' group, or insert `]\='."
  (interactive)
  (noteworthy-typst-close-delimiter ?\] ?\[))

(defun noteworthy-typst-close-brace ()
  "Jump past the enclosing `{}\=' group, or insert `}\='."
  (interactive)
  (noteworthy-typst-close-delimiter ?\} ?\{))

(defun noteworthy-typst-insert-close-paren ()
  "Insert a literal `)\=', never jumping out of a group."
  (interactive)
  (insert ")"))

(defun noteworthy-typst-insert-close-bracket ()
  "Insert a literal `]\=', never jumping out of a group."
  (interactive)
  (insert "]"))

(defun noteworthy-typst-insert-close-brace ()
  "Insert a literal `}\=', never jumping out of a group."
  (interactive)
  (insert "}"))

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

(defun noteworthy-typst-smart-quote ()
  "Smart `\"' insertion.

In math and code a quote is syntax -- \"...\" is literal text inside a
formula, and a string in code -- so it pairs there regardless of what sits
next to it.  smartparens refuses to pair when the next character is a word
character, which is exactly the common case ($ \"|text\" $).

In prose markup a quote is just a quote, so it stays a single character."
  (interactive)
  (cond
   ;; type over the closing quote
   ((eq (char-after) ?\")
    (forward-char 1))
   ((noteworthy-typst-markup-context-p)
    (insert "\""))
   (t
    (insert "\"\"")
    (backward-char 1))))

(defun noteworthy-typst-smart-dollar ()
  "Insert `$\=', pairing it only where that opens a new formula."
  (interactive)
  (cond
   ;; type over the closing $
   ((eq (char-after) ?$) (forward-char 1))
   ;; Already inside a formula, so this $ closes it.  Pairing here produced
   ;; $ x $|$ -- a closed formula plus a stray empty one.
   ((eq (noteworthy-typst-context) 'math) (insert "$"))
   (t (insert "$$") (backward-char 1))))

(defun noteworthy-typst--unescaped-dollar (direction)
  "Find the nearest unescaped `$\=' in DIRECTION (-1 back, 1 forward).
Returns its buffer position, or nil."
  (let (found)
    (while (and (not found)
                (if (< direction 0)
                    (search-backward "$" nil t)
                  (search-forward "$" nil t)))
      (let ((pos (if (< direction 0) (point) (1- (point)))))
        (unless (and (> pos (point-min)) (eq (char-before pos) ?\\))
          (setq found pos))))
    found))

(defun noteworthy-typst-dollar-bounds ()
  "Return (OPEN . CLOSE), the positions of the `$\=' delimiters around point.
Nil when point is not between a pair.  Works across lines, so it finds
block math as readily as inline."
  (let ((open (save-excursion (noteworthy-typst--unescaped-dollar -1)))
        (close (save-excursion (noteworthy-typst--unescaped-dollar 1))))
    (and open close (cons open close))))

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
               (and (eq char-before ?\") (eq char-after ?\"))
               (and (eq char-before ?') (eq char-after ?'))
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

(with-eval-after-load 'smartparens
  ;; In prose an apostrophe is punctuation -- "don't", "Euler's" -- and Typst
  ;; has no single-quoted string, so a ' pair is never what was meant.  This
  ;; is smartparens' doing, not ours: nothing here binds ' at all.
  (sp-local-pair 'typst-ts-mode "'" nil :actions nil))

(provide 'noteworthy-typst)

;;; noteworthy-typst.el ends here
