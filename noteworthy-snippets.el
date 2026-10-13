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
(declare-function lsp-workspaces "lsp-mode" ())
(declare-function lsp--get-buffer-diagnostics "lsp-mode" ())
(declare-function lsp--range-to-region "lsp-mode" (range))
(declare-function lsp:diagnostic-message "lsp-protocol" (object))
(declare-function lsp:diagnostic-range "lsp-protocol" (object))
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
    ;; `d@q' -> `dif theta', `pd@q' -> `partial theta': the differentials
    ;; below want a single letter after the d, and a greek one arrives as
    ;; `@q'.  Before the `@' rule, which would otherwise take the `@q' and
    ;; leave `dtheta'.  Same boundary as those rules, put back by \\1.
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)\\(p?d\\)@\\([A-Za-z]\\)"
     :expand noteworthy-snippets-greek-differential :in (math))
    (:trigger "@\\([A-Za-z]\\)" :expand noteworthy-snippets-greek :in (math))
    ;; `alpha 1\=' -> `alpha_1\='.  The rule above only sees a single letter
    ;; against the digit, which caught `alpha1\=' back when `@a\=' expanded
    ;; without a trailing space.  Now that it ends with one, subscripting a
    ;; spelled-out greek letter has to step over that space.
    (:trigger "\\(${GREEK}\\) \\(${DIGIT}\\)" :expand "\\1_\\2" :in (math))
    ;; `@qarr' -> `arrow(theta)'.  The postfix accents below need a letter
    ;; glued to the accent, and `@q' leaves `theta ' with its space, so a
    ;; greek letter gets a rule of its own.  Only straight after the `@'
    ;; expansion, so a `theta' typed out and followed by `hat' later is left
    ;; alone.  `dot' is left out: `alpha dot beta' is the multiplication dot,
    ;; the far likelier thing to type there.
    (:trigger "\\(^\\|[^[:alpha:]]\\)\\([[:alpha:]]+\\) \\(ddot\\|hat\\|bar\\|til\\|vec\\|arr\\|und\\|ovl\\)"
     :expand noteworthy-snippets-greek-accent :in (math))
    ;; A digit or letter run raised or lowered as a group: x_12 -> x_(12)
    (:trigger "\\(${LETTER}\\)_\\(${DIGIT}${DIGIT}+\\)" :expand "\\1_(\\2)" :in (math))
    ;; `arrow(r) dot;' -> `dot(arrow(r))': the time derivative of a vector,
    ;; which the postfix accents below cannot reach -- they take a single
    ;; letter, and a vector ends in `)'.  The `;' says so; without it, `dot'
    ;; after a vector is the product, as in `arrow(a) dot arrow(b)'.  The
    ;; vector may hold one level of parentheses, `arrow(r_(i))'.
    (:trigger "\\(arrow(\\(?:[^()]\\|([^()]*)\\)*)\\) ?\\(ddot\\|dot\\);"
     :expand noteworthy-snippets-vector-dot :in (math) :max-length 40)
    ;; `pvecdot' -> `dot(arrow(p))': `pvec' has already become `arrow(p)',
    ;; and the dot typed straight against it, with no space, is the accent
    ;; -- the product is written with one, `arrow(a) dot arrow(b)'.
    (:trigger "\\(arrow(\\(?:[^()]\\|([^()]*)\\)*)\\)\\(ddot\\|dot\\)"
     :expand noteworthy-snippets-vector-dot :in (math) :max-length 40)
    ;; Postfix accents: xhat -> hat(x).  Typst has no postfix form, so the
    ;; accent has to be typed before its argument -- this puts it back the
    ;; way you think of it.  Longest alternative first, so ddot beats dot.
    (:trigger "\\(${LETTER}\\)\\(ddot\\|dot\\|hat\\|bar\\|til\\|vec\\|arr\\|und\\|ovl\\)"
     :expand noteworthy-snippets-accent :in (math))
    ;; Postfix styles: bbR -> bb(R).  The captured name is the function.
    (:trigger "\\(bb\\|cal\\|frak\\|bold\\|upright\\|sans\\|mono\\)\\(${LETTER}\\)"
     :expand "\\1(\\2)$0" :in (math))
    ;; `xsq\=' -> `x sqrt()\='.  The keyed `sq\=' has to start a word, so glued to
    ;; a variable it never fired at all -- and letting it fire glued without
    ;; the space would give `xsqrt(2)\=', one identifier Typst does not have
    ;; rather than x beside a root.  Putting the character back with a space
    ;; after it is the whole rule.
    ;;
    ;; Only a letter or a digit needs it.  After `)\=', `]\=' or an operator the
    ;; key already starts a word, so the keyed snippet matches first and
    ;; `(a+b)sqrt(2)\=' is what was wanted anyway.
    (:trigger "\\(${LETTER}\\|${DIGIT}\\)sq"
     :expand "\\1 sqrt($1)$0" :in (math))
    ;; Differentials: `dx ' -> `dif x '.  The delimiter is part of the
    ;; trigger, and that is the whole trick.  Firing on the letter instead
    ;; would expand the moment you typed the second character of `det',
    ;; `dim', `dots' or `delta' -- and of `dif' itself.  Waiting for the
    ;; delimiter means only a d followed by exactly one letter can match, so
    ;; every one of those is safe: they are three letters or more by the time
    ;; a delimiter arrives.  It is put back by \\2.
    ;; Partials, the same two shapes as the differentials below and listed
    ;; first for being the longer key.  They cannot actually collide: the
    ;; `d\=' rules need a non-alphanumeric in front of the d, and `pd\=' puts a
    ;; p there.
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)pd\\(${LETTER}\\)\\([ ,)]\\)"
     :expand "\\1partial \\2\\3" :in (math))
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)pd\\(${LETTER}\\);"
     :expand "\\1(partial \\2)" :in (math))
    ;; The boundary is spelled out rather than written `\\b\=': `$\=' has word
    ;; syntax in the standard table, so `$dx\=' reads as one word and `\\b\='
    ;; never matches after the dollar -- the differential at the very start
    ;; of a formula, which is where it most often sits.  Group 1 is that
    ;; boundary character, put straight back.
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)d\\(${LETTER}\\)\\([ ,)]\\)"
     :expand "\\1dif \\2\\3" :in (math))
    ;; `dx;' -> `(dif x)', for where the differential needs bracketing --
    ;; inside a fraction or under a power.  `dv;' and friends already build
    ;; the whole derivative, and being keyed they are tried first, so `dv;'
    ;; stays the fraction and only the other letters reach this rule.
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)d\\(${LETTER}\\);"
     :expand "\\1(dif \\2)" :in (math))
    ;; `dx/' -> `(dif x)/', and `pdx/' -> `(partial x)/'.  A slash makes a
    ;; fraction of the operand in front of it, and `dif x/' would put only
    ;; the x on top.  The slash is kept.
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)pd\\(${LETTER}\\)/"
     :expand "\\1(partial \\2)/" :in (math))
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)d\\(${LETTER}\\)/"
     :expand "\\1(dif \\2)/" :in (math)))
  "Regex auto-expansions, tried in order after the keyed snippets.

Each rule is a plist:

  :trigger  a regexp matched against the text ending at point.  ${NAME} is
            replaced from `noteworthy-snippets-regex-variables' first.
  :expand   a yasnippet template, where \\1..\\9 are the trigger's capture
            groups; or a function of the capture list returning one, or nil
            to decline the match.
  :max-length  how far back the trigger may reach, if further than
            `noteworthy-snippets-auto-key-max-length'.
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
  "Expand the letter captured in GROUPS to a Typst greek letter name.

Trailing space included: Typst spells these out, so `alpha\=' run against
whatever you type next is a different identifier -- `@ab\=' would have to
become `alphab\='.  The space is the separator you would type anyway."
  (when-let* ((name (cdr (assoc (car groups) noteworthy-snippets--greek))))
    (concat name " ")))

(defun noteworthy-snippets-greek-differential (groups)
  "Expand GROUPS -- boundary, `d' or `pd', letter -- to a greek differential."
  (when-let* ((greek (noteworthy-snippets-greek (list (nth 2 groups)))))
    (concat (nth 0 groups)
            (if (equal (nth 1 groups) "pd") "partial " "dif ")
            greek)))

(defconst noteworthy-snippets--accents
  '(("ddot" . "dot.double") ("dot" . "dot") ("hat" . "hat") ("bar" . "bar")
    ("til" . "tilde") ("vec" . "arrow") ("arr" . "arrow")
    ("und" . "underline")
    ("ovl" . "overline"))
  "Postfix accent triggers and the Typst function each one wraps with.

`vec\\=' gives `arrow\\=' on purpose: in Typst `vec()\\=' is a column vector, and
the arrow accent -- what LaTeX Suite spells `\\vec\\=' -- is `arrow()\\='.")

(defun noteworthy-snippets-vector-dot (groups)
  "Put the dot accent named in GROUPS over the vector in GROUPS."
  (format "%s(%s)$0"
          (if (equal (nth 1 groups) "ddot") "dot.double" "dot")
          (nth 0 groups)))

(defun noteworthy-snippets-accent (groups)
  "Wrap the letter in GROUPS with the accent function it was suffixed with.
Declines `ddot' right after a closing parenthesis: that is a vector's
`ddot;' (`noteworthy-snippets-vector-dot') being typed, not a variable d
with a dot."
  (let ((fn (cdr (assoc (nth 1 groups) noteworthy-snippets--accents))))
    (and fn
         (not (and (equal (nth 0 groups) "d") (equal (nth 1 groups) "dot")
                   (save-excursion
                     (goto-char (- (point) 4))
                     (looking-back ") ?" (max (point-min) (- (point) 2))))))
         (format "%s(%s)$0" fn (nth 0 groups)))))

(defvar-local noteworthy-snippets--last-end nil
  "Where the last auto-expansion left point, as a marker.")

(defun noteworthy-snippets-greek-accent (groups)
  "Wrap the greek letter in GROUPS with its accent, right after `@'.
GROUPS is the character before the name, put back as it was, the name and
the accent.  Declines unless the name is one `@' gives and it and its
space are what the last expansion inserted, with nothing typed since but
the accent itself.

The name is matched as a whole word and looked up, not matched against
${GREEK}: that lists `eta' before `theta', so `theta arr' would wrap only
the `eta', and it has no capitals."
  (let ((accent (nth 2 groups)))
    (and (rassoc (nth 1 groups) noteworthy-snippets--greek)
         (markerp noteworthy-snippets--last-end)
         (eq (marker-buffer noteworthy-snippets--last-end) (current-buffer))
         (= (marker-position noteworthy-snippets--last-end)
            (- (point) (length accent)))
         (concat (nth 0 groups) (noteworthy-snippets-accent (cdr groups))))))

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
  (let ((context (noteworthy-typst-context)))
    (cl-loop for rule in noteworthy-snippets-regex-rules
             when (memq context (plist-get rule :in))
             thereis
             (save-excursion
               (when (looking-back (noteworthy-snippets--expand-variables
                                    (plist-get rule :trigger))
                                   (max (point-min)
                                        (- (point)
                                           (or (plist-get rule :max-length)
                                               noteworthy-snippets-auto-key-max-length)))
                                   t)
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
                (yas-expand-snippet template (nth 1 hit) (nth 2 hit)))
              (if (markerp noteworthy-snippets--last-end)
                  (set-marker noteworthy-snippets--last-end (point))
                (setq noteworthy-snippets--last-end (point-marker))))))
      (error
       (message "Noteworthy: auto-expansion failed: %s"
                (error-message-string err))))))

;;; Auto-spacing run-together names (`costheta' -> `cos theta')

;; In Typst math a run of letters is one name: `costheta' is not cos of
;; theta but an unknown variable, a compile error, and the LaTeX habit of
;; writing names together makes it easy to type.  When a word is finished
;; with a space or a newline, a word that is no name Typst knows but
;; splits wholly into names it does know gets the spaces put in.

(defcustom noteworthy-snippets-auto-space t
  "Whether math words like `costheta' are split into `cos theta' as you type.
See `noteworthy-snippets-maybe-space-word'."
  :type 'boolean
  :group 'noteworthy)

(defconst noteworthy-snippets--math-names
  (append
   ;; Typst's math operators
   '("arccos" "arcsin" "arctan" "arg" "cos" "cosh" "cot" "coth" "csc" "csch"
     "ctg" "deg" "det" "dim" "exp" "gcd" "lcm" "hom" "id" "im" "inf" "ker"
     "lg" "lim" "liminf" "limsup" "ln" "log" "max" "min" "mod" "Pr" "sec"
     "sech" "sin" "sinc" "sinh" "sup" "tan" "tanh" "tg" "tr")
   ;; Greek letters, both cases where Typst has them
   '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota"
     "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma" "tau"
     "upsilon" "phi" "chi" "psi" "omega"
     "Gamma" "Delta" "Theta" "Lambda" "Xi" "Pi" "Sigma" "Upsilon" "Phi"
     "Psi" "Omega")
   ;; Symbols and functions commonly written next to a name
   '("dif" "partial" "nabla" "infinity" "oo" "sum" "product" "integral"
     "dot" "times" "plus" "minus" "arrow" "hat" "bar" "tilde" "sqrt" "abs"
     "norm" "vec" "mat" "cases" "frac" "binom" "eq" "approx" "dots" "cdots"
     "ell" "hbar" "planck"))
  "Names `noteworthy-snippets-maybe-space-word' splits a word into.")

(defconst noteworthy-snippets--typst-math-scope
  '("AA" "Alpha" "BB" "Beta" "CC" "Chi" "DD" "Delta" "Dif" "Digamma" "EE"
    "Epsilon" "Eta" "FF" "GG" "Gamma" "HH" "II" "Im" "Iota" "JJ" "KK"
    "Kappa" "LL" "Lambda" "MM" "Mu" "NN" "Nu" "OO" "Omega" "Omicron" "PP"
    "Phi" "Pi" "Pr" "Psi" "QQ" "RR" "Re" "Rho" "SS" "Sha" "Sigma" "TT"
    "Tau" "Theta" "UU" "Upsilon" "VV" "WW" "XX" "Xi" "YY" "ZZ" "Zeta"
    "abs" "accent" "acute" "afghani" "aleph" "alpha" "amp" "and" "angle"
    "angstrom" "angzarr" "approx" "arccos" "arcsin" "arctan" "arg" "arrow"
    "arrowhead" "arrows" "ast" "asymp" "at" "attach" "backslash" "bag"
    "baht" "ballot" "bar" "bb" "because" "beta" "beth" "binom" "bitcoin"
    "bold" "bot" "bowtie" "brace" "bracket" "breve" "bullet" "cal"
    "cancel" "caret" "caron" "cases" "cc" "cedi" "ceil" "cent" "checkmark"
    "chevron" "chi" "circle" "class" "co" "colon" "comma" "complement"
    "compose" "control" "convolve" "copyleft" "copyright" "corner" "cos"
    "cosh" "cot" "coth" "crossmark" "csc" "csch" "ctg" "currency" "dagger"
    "daleth" "dash" "deg" "degree" "delta" "det" "diaer" "diameter"
    "diamond" "die" "dif" "digamma" "dim" "display" "div" "divides"
    "dollar" "dong" "dorome" "dot" "dotless" "dots" "dram" "earth" "ell"
    "ellipse" "emptyset" "epsilon" "eq" "equation" "equiv" "errorbar"
    "eta" "euro" "excl" "exists" "exp" "fence" "flat" "floor" "floral"
    "forall" "forces" "frac" "frak" "frown" "gamma" "gcd" "gender" "gimel"
    "gradient" "grave" "gt" "guarani" "harpoon" "harpoons" "hash" "hat"
    "hexa" "hom" "hourglass" "hryvnia" "hyph" "id" "im" "image" "in" "inf"
    "infinity" "inline" "integral" "inter" "interleave" "interrobang"
    "iota" "italic" "join" "jupiter" "kappa" "ker" "kip" "lambda"
    "laplace" "lari" "lat" "lcm" "lg" "lim" "liminf" "limits" "limsup"
    "lira" "ln" "log" "lozenge" "lr" "lrm" "lt" "macron" "maltese" "manat"
    "mapsfrom" "mapsto" "mars" "mat" "max" "med" "mercury" "mid" "min"
    "minus" "miny" "mod" "models" "mono" "mu" "multimap" "mustache"
    "nabla" "naira" "natural" "neptune" "norm" "not" "note" "nothing" "nu"
    "numero" "omega" "omicron" "oo" "op" "or" "original" "overbrace"
    "overbracket" "overline" "overparen" "overshell" "parallel"
    "parallelogram" "paren" "partial" "pataca" "pee" "penta" "percent"
    "permille" "permyriad" "perp" "peso" "phi" "pi" "pilcrow" "planck"
    "plus" "pound" "power" "prec" "prime" "primes" "product" "prop" "psi"
    "qed" "quad" "quest" "quote" "ratio" "rect" "refmark" "rest" "rho"
    "riel" "riyal" "rlm" "root" "round" "ruble" "rupee" "sans" "saturn"
    "scr" "script" "scripts" "sec" "sech" "section" "semi" "serif" "sha"
    "sharp" "shekel" "shell" "sigma" "sin" "sinc" "sinh" "slash" "smash"
    "smile" "smt" "som" "space" "spacebar" "sqrt" "square" "sscript"
    "star" "stretch" "subset" "succ" "suit" "sum" "sun" "sup" "supset"
    "tack" "taka" "taman" "tan" "tanh" "tau" "tenge" "text" "tg"
    "therefore" "theta" "thick" "thin" "tilde" "times" "tiny" "togrog"
    "top" "tr" "trademark" "triangle" "underbrace" "underbracket"
    "underline" "underparen" "underscore" "undershell" "union" "upright"
    "upsilon" "uranus" "vec" "venus" "wide" "without" "wj" "won" "wreath"
    "xi" "xor" "yen" "yuan" "zero" "zeta" "zwj" "zwnj" "zws")
  "Every name defined in Typst's math scope, as of Typst 0.15.1.
A word that is one of these is never split, however it decomposes: `wide\='
is the wide space, not `w id e\='.  Regenerate with
  typst eval \"dictionary(math).keys().sorted()\"")

(defun noteworthy-snippets--split-word (word &optional unknown)
  "WORD split into known names and single letters, or nil.
Splits only a word that is not itself a known name or a name in Typst's
math scope, into the fewest pieces, of which at least one is a known
name of two or more letters -- so `abc' is left as typed.  With UNKNOWN,
Typst has said WORD is undefined, so it splits even into letters alone:
`xy' becomes `x y'.  Never splits off a lone leading `d', which is the
differential: `dtheta' is for `d@q'."
  (let* ((n (length word))
         (names noteworthy-snippets--math-names)
         ;; best[i] = fewest-piece split of word[0,i), as a list of pieces
         (best (make-vector (1+ n) nil)))
    (unless (or (member word names)
                (member word noteworthy-snippets--typst-math-scope))
      (aset best 0 '())
      (dotimes (i n)
        (let ((prefix (aref best i)))
          (when (or (= i 0) prefix)
            (dolist (len (number-sequence 1 (- n i)))
              (let ((piece (substring word i (+ i len)))
                    (end (+ i len)))
                (when (or (member piece names) (= len 1))
                  (let ((cand (append prefix (list piece)))
                        (have (aref best end)))
                    (when (or (null have) (< (length cand) (length have)))
                      (aset best end cand)))))))))
      (let ((pieces (aref best n)))
        (and (> (length pieces) 1)
             (or unknown
                 (seq-some (lambda (p) (and (> (length p) 1) (member p names))) pieces))
             (not (equal (car pieces) "d"))
             pieces)))))

(defun noteworthy-snippets--defined-name-p (word)
  "Non-nil when WORD is bound with `let' somewhere in this buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "\\_<let[ \t]+" (regexp-quote word) "\\_>") nil t)))

(defvar-local noteworthy-snippets--pending-words nil
  "Words waiting for the language server to say whether Typst knows them.
Each is (START END WORD TIME): markers around WORD, and when it was typed.")

(defvar noteworthy-snippets--pending-buffers nil
  "Buffers with entries in `noteworthy-snippets--pending-words'.")

(defconst noteworthy-snippets--pending-timeout 30
  "Seconds a word waits for the language server before it is dropped.
The first compile after opening a big file takes a while -- 14 s for a
page of math-noteworthy with its plots -- and every later one is fast.")

(defun noteworthy-snippets--lsp-p ()
  "Non-nil when a language server reports this buffer's diagnostics."
  (and (bound-and-true-p lsp-mode)
       (fboundp 'lsp-workspaces)
       (lsp-workspaces)))

(defun noteworthy-snippets--math-word-before (end)
  "Bounds (START . END) of the math word ending at END, or nil.
A run of two or more letters, not after `.' (a field, as in `arrow.r'),
`#', `_', `\\' or `@', nor glued to a letter or digit before it."
  (save-excursion
    (goto-char end)
    (skip-chars-backward "[:alpha:]")
    (let ((start (point)))
      (and (>= (- end start) 2)
           (not (memq (char-before start) '(?. ?# ?_ ?\\ ?@)))
           (not (and (char-before start)
                     (string-match-p "[[:alnum:]]" (string (char-before start)))))
           (progn (goto-char end) (eq (noteworthy-typst-context) 'math))
           (cons start end)))))

(defun noteworthy-snippets--replace-word (start end pieces)
  "Replace START..END with PIECES, a space between each."
  (save-excursion
    (goto-char start)
    (delete-region start end)
    (insert (mapconcat #'identity pieces " "))))

(defun noteworthy-snippets--consider-word (end)
  "Split the math word ending at END, now or once Typst has judged it.
With a language server, the word waits for its diagnostics: Typst says
`unknown variable' about exactly the words that are not defined -- by a
`let' here, in an imported template, or by Typst itself -- and only those
are split.  Without one, words are split by the names this file knows."
  (when-let* ((bounds (noteworthy-snippets--math-word-before end)))
    (let ((word (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (cond
       ((member word noteworthy-snippets--typst-math-scope))
       ((noteworthy-snippets--lsp-p)
        (push (list (copy-marker (car bounds)) (copy-marker (cdr bounds))
                    word (float-time))
              noteworthy-snippets--pending-words)
        (cl-pushnew (current-buffer) noteworthy-snippets--pending-buffers)
        ;; Typst has usually judged the word already, while it was typed.
        (noteworthy-snippets--settle-pending))
       ((> (length word) 2)
        (let ((pieces (noteworthy-snippets--split-word word)))
          (when (and pieces (not (noteworthy-snippets--defined-name-p word)))
            (noteworthy-snippets--replace-word (car bounds) (cdr bounds) pieces))))))))

(defun noteworthy-snippets--unknown-variables ()
  "Regions Typst reports as an unknown variable, as (START END NAME)."
  (let (found)
    (dolist (diag (lsp--get-buffer-diagnostics))
      (let ((message (lsp:diagnostic-message diag)))
        (when (string-match "\\`unknown variable: \\([[:alpha:]]+\\)" message)
          (let ((region (lsp--range-to-region (lsp:diagnostic-range diag))))
            (push (list (car region) (cdr region) (match-string 1 message))
                  found)))))
    found))

(defun noteworthy-snippets--settle-pending ()
  "Split the pending words Typst has called unknown; drop stale ones."
  (when noteworthy-snippets--pending-words
    (let ((unknown (noteworthy-snippets--unknown-variables))
          (now (float-time))
          keep)
      (dolist (entry noteworthy-snippets--pending-words)
        (pcase-let ((`(,start ,end ,word ,time) entry))
          (cond
           ;; Edited since, or gone: nothing to fix any more.
           ((not (equal (buffer-substring-no-properties start end) word))
            (set-marker start nil) (set-marker end nil))
           ((member (list (marker-position start) (marker-position end) word)
                    unknown)
            (let ((pieces (noteworthy-snippets--split-word word t)))
              (when pieces
                (noteworthy-snippets--replace-word start end pieces)))
            (set-marker start nil) (set-marker end nil))
           ((> (- now time) noteworthy-snippets--pending-timeout)
            (set-marker start nil) (set-marker end nil))
           (t (push entry keep)))))
      (setq noteworthy-snippets--pending-words (nreverse keep))))
  (unless noteworthy-snippets--pending-words
    (setq noteworthy-snippets--pending-buffers
          (delq (current-buffer) noteworthy-snippets--pending-buffers))))

(defun noteworthy-snippets--on-diagnostics ()
  "Settle the pending words in every buffer that has some.
Runs from `lsp-diagnostics-updated-hook', which is not called from any
particular buffer."
  (dolist (buffer noteworthy-snippets--pending-buffers)
    (if (buffer-live-p buffer)
        (with-current-buffer buffer (noteworthy-snippets--settle-pending))
      (setq noteworthy-snippets--pending-buffers
            (delq buffer noteworthy-snippets--pending-buffers)))))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-diagnostics-updated-hook #'noteworthy-snippets--on-diagnostics))

(defun noteworthy-snippets-maybe-space-word ()
  "Split the math word just finished, if it runs names together.
The word is finished by a space or a newline.  Runs from
`post-self-insert-hook'; see `noteworthy-snippets--consider-word'."
  (when (and noteworthy-snippets-auto-space
             (bound-and-true-p noteworthy-typst-mode)
             (memq last-command-event '(?\s ?\n ?\r)))
    (noteworthy-snippets--consider-word (1- (point)))))

(defun noteworthy-snippets-maybe-space-word-at-dollar ()
  "Split the math word the closing `$' just finished, as `$xy$'.
Runs from `post-command-hook': `noteworthy-typst-smart-dollar' inserts the
`$' itself, or steps over the paired one, so no self-insert is seen."
  (when (and noteworthy-snippets-auto-space
             (eq this-command 'noteworthy-typst-smart-dollar)
             (eq (char-before) ?$))
    (noteworthy-snippets--consider-word (1- (point)))))

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
              #'noteworthy-snippets-maybe-auto-expand 90 t)
    ;; After the expansions, so a snippet that fires on the same space
    ;; (`dx ' -> `dif x ') has already had its say.
    (add-hook 'post-self-insert-hook
              #'noteworthy-snippets-maybe-space-word 91 t)
    (add-hook 'post-command-hook
              #'noteworthy-snippets-maybe-space-word-at-dollar nil t)))

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
