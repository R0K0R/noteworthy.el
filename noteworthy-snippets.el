;;; noteworthy-snippets.el --- yasnippet integration for Noteworthy  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ships Typst snippets with the package and teaches the Noteworthy TAB
;; bindings about them.
;;
;; The bindings are the reason this needs its own file.  `noteworthy-evil'
;; binds TAB and S-TAB inside evil's insert state map, which shadows
;; `yas-minor-mode-map' -- so yasnippet's own `yas-maybe-expand' never
;; runs in a Noteworthy buffer.  `noteworthy-typst-indent-line' therefore
;; dispatches to yasnippet itself, in the same order yasnippet would:
;; expand a key, else advance a field, else indent the list item.
;;
;; Field navigation inside a live snippet keeps working through
;; `yas-keymap', which rides on the snippet's overlay -- overlay keymaps
;; outrank evil's emulation maps, so that path needs no help.

;;; Code:

(require 'cl-lib)
(require 'noteworthy-typst)

(defvar yas-snippet-dirs)
(defvar yas-indent-line)
(defvar yas-buffer-local-condition)
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(declare-function yas-load-directory "yasnippet" (top-level-dir &optional use-jit interactive))
(declare-function yas-expand-snippet "yasnippet" (snippet &optional start end expand-env))
(declare-function yas--fetch "yasnippet" (table key))
(declare-function yas--get-snippet-tables "yasnippet" (&optional mode))
(declare-function yas--template-p "yasnippet" (obj))
(declare-function yas--template-content "yasnippet" (template))

(defun noteworthy-snippets--locate ()
  "Find the `snippets\=' directory that ships with this package.

Package managers do not all carry non-Lisp files along.  straight, for
one, builds a package by symlinking just its `.el\=' files into a build
directory, so looking beside the loaded file finds nothing -- but the
sibling symlink still points back at the checkout, which does have the
snippets.  Try the loaded file, its target, and the same two for the
`.el\=' next to a `.elc\='."
  (let* ((this (or load-file-name buffer-file-name default-directory))
         (el (concat (file-name-sans-extension this) ".el"))
         (dirs (delete-dups
                (mapcar #'file-name-directory
                        (list this (file-truename this)
                              el (file-truename el))))))
    (cl-loop for dir in dirs
             for candidate = (expand-file-name "snippets" dir)
             when (file-directory-p candidate) return candidate)))

(defvar noteworthy-snippets-directory (noteworthy-snippets--locate)
  "Directory holding the Typst snippets shipped with Noteworthy.
Nil when the package was installed without its `snippets\=' directory.")

;;; Context-selective and auto-expanding snippets

;; Every snippet that ships with Noteworthy carries a `# condition:' header
;; naming the contexts it belongs to, so the same key can mean different
;; things -- or nothing -- depending on where point sits:
;;
;;     # condition: (noteworthy-typst-in 'markup)        ; TAB, prose only
;;     # condition: (noteworthy-typst-in 'math 'code)    ; TAB, either
;;     # condition: (noteworthy-typst-auto 'math)        ; fires on its own
;;
;; The condition is evaluated at point, so it sees the live AST.  yasnippet
;; already filters candidates through it (`yas--filter-templates-by-condition'),
;; which is why plain TAB expansion needs no extra code here.
;;
;; Auto-expansion reuses the same machinery: `noteworthy-typst-auto' returns
;; the symbol `auto', and binding `yas-buffer-local-condition' to
;; (require-snippet-condition . auto) narrows a lookup to exactly those
;; snippets.  A snippet with no condition, or one returning t, is left alone.

(defcustom noteworthy-snippets-auto-expand t
  "Whether snippets marked `auto\=' expand as soon as their key is typed.
When nil they still expand on TAB like any other snippet."
  :type 'boolean
  :group 'noteworthy)

(defcustom noteworthy-snippets-auto-key-max-length 12
  "How far back to look for an auto-expanding snippet key."
  :type 'integer
  :group 'noteworthy)

(defconst noteworthy-snippets--identifier-rx "[[:alnum:]_-]"
  "What can be part of a Typst identifier.

Deliberately not `\\w\=': word syntax comes from the syntax table, where
`$\=' counts as a word constituent, so `$sq;\=' would look glued to a word
and never expand.  Character classes do not consult the syntax table.")

(defun noteworthy-snippets--key-boundary-ok-p (key start)
  "Return non-nil when KEY beginning at START is not glued to a word.

A key that begins like an identifier has to begin one, or an auto key
like `sin\=' would fire in the middle of `arcsin\='.  Keys that begin with
punctuation carry their own boundary, so they are always allowed."
  (or (not (string-match-p (concat "\\`" noteworthy-snippets--identifier-rx) key))
      (<= start (point-min))
      (not (string-match-p noteworthy-snippets--identifier-rx
                           (string (char-before start))))))

(defun noteworthy-snippets--lookup (key requirement)
  "Return the one live template for KEY under REQUIREMENT, or nil.

REQUIREMENT is the symbol a snippet\='s condition must return -- `auto\=' or
`auto-word\='.  An ambiguous key, matching more than one live template,
returns nil rather than prompting, which would be intolerable in the
middle of typing."
  (let* ((yas-buffer-local-condition
          ;; Quoted: yasnippet `eval's this variable when it is a cons, so
          ;; handing it the bare (require-snippet-condition . auto) makes it
          ;; try to *call* `require-snippet-condition'.  That errors on every
          ;; lookup -- caught and logged by `yas--funcall-condition', and then
          ;; papered over by an `or' that falls back to the raw value, so it
          ;; still works while spraying "Wrong type argument: listp, auto"
          ;; into the echo area on every keystroke.
          (list 'quote (cons 'require-snippet-condition requirement)))
         (hits (cl-mapcan (lambda (table) (yas--fetch table key))
                          (yas--get-snippet-tables))))
    (and (= (length hits) 1) (cdar hits))))

(defun noteworthy-snippets--auto-hit ()
  "Return (TEMPLATE START END) for an auto snippet key ending at point.

Candidates are tried longest-first, so `bmat;\=' wins over `mat;\='.  At each
length an `auto\=' snippet matches anywhere, an `auto-word\=' one only where
it starts a word."
  (let ((end (point))
        (floor (max (point-min)
                    (- (point) noteworthy-snippets-auto-key-max-length))))
    (cl-loop for start from floor below end
             for key = (buffer-substring-no-properties start end)
             for hit = (or (noteworthy-snippets--lookup key 'auto)
                           (and (noteworthy-snippets--key-boundary-ok-p key start)
                                (noteworthy-snippets--lookup key 'auto-word)))
             when hit return (list hit start end))))

;;; Regex triggers (the `r' option in Obsidian's LaTeX Suite)

;; yasnippet keys are literal strings looked up in a hash, so patterns like
;; "a letter followed by a digit" have no home there.  These rules are
;; matched separately, against the text ending at point, and their captures
;; are substituted into a template that is then handed to yasnippet -- so
;; tabstops still work in the expansion.

(defcustom noteworthy-snippets-regex-variables
  '(("GREEK" . "alpha\\|beta\\|gamma\\|delta\\|epsilon\\|zeta\\|eta\\|theta\\|\
iota\\|kappa\\|lambda\\|mu\\|nu\\|xi\\|pi\\|rho\\|sigma\\|tau\\|upsilon\\|\
phi\\|chi\\|psi\\|omega")
    ("LETTER" . "[A-Za-z]")
    ("DIGIT"  . "[0-9]"))
  "Named fragments usable as ${NAME} inside a rule's `:trigger'.
The LaTeX Suite equivalent of its snippet variables."
  :type '(alist :key-type string :value-type string)
  :group 'noteworthy)

(defcustom noteworthy-snippets-regex-rules
  '(;; x1 -> x_1, and alpha1 -> alpha_1, since the match is anchored at point
    (:trigger "\\(${LETTER}\\)\\(${DIGIT}\\)" :expand "\\1_\\2" :in (math))
    ;; @a -> alpha.  Typst spells the greek letters out, so this is purely
    ;; keystrokes; the table is the rule's own business.
    (:trigger "@\\([A-Za-z]\\)" :expand noteworthy-snippets-greek :in (math))
    ;; A digit or letter run raised or lowered as a group: x_12 -> x_(12)
    (:trigger "\\(${LETTER}\\)_\\(${DIGIT}${DIGIT}+\\)" :expand "\\1_(\\2)" :in (math))
    ;; Postfix accents: xhat -> hat(x).  Typst has no postfix form, so the
    ;; accent has to be typed before its argument -- this puts it back the
    ;; way you think of it.  Longest alternative first, so ddot beats dot.
    (:trigger "\\(${LETTER}\\)\\(ddot\\|dot\\|hat\\|bar\\|til\\|vec\\|arr\\|und\\|ovl\\)"
     :expand noteworthy-snippets-accent :in (math))
    ;; Postfix styles: bbR -> bb(R).  The captured name is the function.
    (:trigger "\\(bb\\|cal\\|frak\\|bold\\|upright\\|sans\\|mono\\)\\(${LETTER}\\)"
     :expand "\\1(\\2)$0" :in (math)))
  "Regex auto-expansions, tried in order after the keyed snippets.

Each rule is a plist:

  :trigger  a regexp matched against the text ending at point.  ${NAME} is
            replaced from `noteworthy-snippets-regex-variables' first.
  :expand   a yasnippet template, where \\1..\\9 are the trigger's capture
            groups; or a function of the capture list returning one, or nil
            to decline the match.
  :in       the contexts it applies to, as `noteworthy-typst-context'
            reports them."
  :type '(repeat plist)
  :group 'noteworthy)

(defconst noteworthy-snippets--greek
  '(("a" . "alpha") ("b" . "beta")  ("g" . "gamma") ("d" . "delta")
    ("e" . "epsilon") ("z" . "zeta") ("h" . "eta")  ("q" . "theta")
    ("i" . "iota")  ("k" . "kappa") ("l" . "lambda") ("m" . "mu")
    ("n" . "nu")    ("x" . "xi")    ("p" . "pi")    ("r" . "rho")
    ("s" . "sigma") ("t" . "tau")   ("u" . "upsilon") ("f" . "phi")
    ("c" . "chi")   ("y" . "psi")   ("w" . "omega")
    ("G" . "Gamma") ("D" . "Delta") ("Q" . "Theta") ("L" . "Lambda")
    ("X" . "Xi")    ("P" . "Pi")    ("S" . "Sigma") ("F" . "Phi")
    ("Y" . "Psi")   ("W" . "Omega"))
  "Single-letter shorthands for the Typst names of the greek letters.")

(defun noteworthy-snippets-greek (groups)
  "Expand the letter captured in GROUPS to a Typst greek letter name."
  (cdr (assoc (car groups) noteworthy-snippets--greek)))

(defconst noteworthy-snippets--accents
  '(("ddot" . "dot.double") ("dot" . "dot") ("hat" . "hat") ("bar" . "bar")
    ("til" . "tilde") ("vec" . "arrow") ("arr" . "arrow")
    ("und" . "underline")
    ("ovl" . "overline"))
  "Postfix accent triggers and the Typst function each one wraps with.

`vec\\=' gives `arrow\\=' on purpose: in Typst `vec()\\=' is a column vector, and
the arrow accent -- what LaTeX Suite spells `\\vec\\=' -- is `arrow()\\='.")

(defun noteworthy-snippets-accent (groups)
  "Wrap the letter in GROUPS with the accent function it was suffixed with."
  (let ((fn (cdr (assoc (nth 1 groups) noteworthy-snippets--accents))))
    (and fn (format "%s(%s)$0" fn (nth 0 groups)))))

(defun noteworthy-snippets--expand-variables (trigger)
  "Substitute ${NAME} fragments into TRIGGER."
  (replace-regexp-in-string
   "\\${\\([A-Z_]+\\)}"
   (lambda (m)
     (let ((name (substring m 2 -1)))
       (or (cdr (assoc name noteworthy-snippets-regex-variables))
           (progn (message "Noteworthy: unknown snippet variable ${%s}" name) m))))
   trigger t t))

(defun noteworthy-snippets--substitute (template groups)
  "Replace \\1..\\9 in TEMPLATE with the corresponding member of GROUPS."
  (replace-regexp-in-string
   "\\\\\\([1-9]\\)"
   (lambda (m) (or (nth (1- (string-to-number (substring m 1))) groups) ""))
   template t t))

(defun noteworthy-snippets--regex-hit ()
  "Return (TEMPLATE START END) for the first regex rule matching at point."
  (let ((context (noteworthy-typst-context))
        (limit (max (point-min)
                    (- (point) noteworthy-snippets-auto-key-max-length))))
    (cl-loop for rule in noteworthy-snippets-regex-rules
             when (memq context (plist-get rule :in))
             thereis
             (save-excursion
               (when (looking-back (noteworthy-snippets--expand-variables
                                    (plist-get rule :trigger))
                                   limit t)
                 ;; Capture the groups now: building the template runs more
                 ;; regexp machinery, which would clobber the match data.
                 (let* ((start (match-beginning 0))
                        (groups (cl-loop for i from 1 to 9
                                         collect (match-string i)))
                        (expand (plist-get rule :expand))
                        (template (if (functionp expand)
                                      (funcall expand groups)
                                    (noteworthy-snippets--substitute expand groups))))
                   (when template
                     (list template start (point)))))))))

(defun noteworthy-snippets--fieldless-p (template)
  "Non-nil when TEMPLATE has no tabstop other than $0.

Such a template produces a snippet that exits the instant it is created,
which is pure overhead -- and, expanded inside a live field, leaves
yasnippet's stacked-expansion bookkeeping inconsistent, so the *next*
keystroke fails an assertion.  These are inserted directly instead."
  (not (string-match-p "\\(?:\\`\\|[^\\\\]\\)\\$\\(?:[1-9]\\|{[1-9]\\)" template)))

(defun noteworthy-snippets--expand-plain (template start end)
  "Replace START..END with TEMPLATE, which has no fields.

Point lands where $0 was, or after the text.  Backslash escapes are
resolved here, since yasnippet never sees this text."
  (let ((out nil) (exit nil) (i 0) (n (length template)))
    (while (< i n)
      (let ((c (aref template i)))
        (cond
         ((and (eq c ?\\) (< (1+ i) n))
          (push (aref template (1+ i)) out)
          (setq i (+ i 2)))
         ((and (eq c ?$) (< (1+ i) n) (eq (aref template (1+ i)) ?0))
          (setq exit (length out) i (+ i 2)))
         (t (push c out) (setq i (1+ i))))))
    (setq out (concat (nreverse out)))
    (delete-region start end)
    (insert out)
    (when exit (goto-char (+ start exit)))))

(defun noteworthy-snippets-maybe-auto-expand ()
  "Expand an auto snippet whose key was just completed at point.
Runs from `post-self-insert-hook\='."
  (when (and noteworthy-snippets-auto-expand
             (bound-and-true-p yas-minor-mode)
             (bound-and-true-p noteworthy-typst-mode))
    ;; An error here would come back on every keystroke, and in a collab
    ;; session it would land in the middle of a shared buffer.  Report it
    ;; once and let typing continue.
    (condition-case err
        ;; A literal key is the more specific statement, so it wins over a
        ;; pattern that would also have matched here.
        (let ((hit (or (noteworthy-snippets--auto-hit)
                       (noteworthy-snippets--regex-hit))))
          (when hit
            ;; A keyed hit carries a `yas--template'; a regex rule a string.
            (let* ((template (nth 0 hit))
                   (content (if (yas--template-p template)
                                (yas--template-content template)
                              template)))
              (if (noteworthy-snippets--fieldless-p content)
                  (noteworthy-snippets--expand-plain content (nth 1 hit) (nth 2 hit))
                (yas-expand-snippet template (nth 1 hit) (nth 2 hit))))))
      (error
       (message "Noteworthy: auto-expansion failed: %s"
                (error-message-string err))))))

(defun noteworthy-snippets-setup ()
  "Turn on yasnippet for the current Noteworthy buffer."
  (when (and noteworthy-typst-mode (require 'yasnippet nil t))
    (yas-minor-mode 1)
    ;; typst-ts-mode indents from the tree-sitter parse, and a freshly
    ;; expanded snippet is usually half-written -- an unclosed content
    ;; block parses as an ERROR node and gets flattened to column zero.
    ;; Reindenting also rewrites lines the collab bridge then has to
    ;; re-send.  Keep the columns the snippet file already declares.
    (setq-local yas-indent-line 'fixed)
    ;; `fixed\=' re-indents each line of an expansion to the starting column,
    ;; and would use tabs to get there.  Typst does not care, but
    ;; `noteworthy-typst-dedent-line\=' matches leading spaces, and a tab
    ;; landing in a shared buffer is churn the bridge has to carry.
    (setq-local indent-tabs-mode nil)
    ;; Depth 90 so this runs AFTER smartparens' handler.  Expanding first
    ;; leaves sp looking at a buffer where our snippet has just inserted a
    ;; `(', which it then auto-pairs -- `lim;' became `lim_() -> oo)', an
    ;; unbalanced paren that looked for all the world like a bad snippet.
    (add-hook 'post-self-insert-hook
              #'noteworthy-snippets-maybe-auto-expand 90 t)))

(add-hook 'noteworthy-typst-mode-hook #'noteworthy-snippets-setup)

(with-eval-after-load 'yasnippet
  (if (not noteworthy-snippets-directory)
      (message "Noteworthy: snippets directory not found -- reinstall with `snippets\=' included")
    (add-to-list 'yas-snippet-dirs noteworthy-snippets-directory t)
    ;; Load lazily: the directory is only scanned the first time a Typst
    ;; buffer asks for a snippet.
    (yas-load-directory noteworthy-snippets-directory t)))

(provide 'noteworthy-snippets)

;;; noteworthy-snippets.el ends here
