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
(declare-function lsp-feature? "lsp-mode" (feature))
(declare-function lsp-request-async "lsp-mode" (method params callback &rest args))
(declare-function lsp--text-document-position-params "lsp-mode" (&optional identifier position))
(declare-function yas-load-directory "yasnippet" (top-level-dir &optional use-jit interactive))
(declare-function yas-expand-snippet "yasnippet" (snippet &optional start end expand-env))
(declare-function yas-exit-all-snippets "yasnippet" ())
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
like `sin\=' would fire in the middle of `arcsin\='.  After a `.\=' it is a
symbol's variant, as in `subset.sq\=', and no word of its own either.  Keys
that begin with punctuation carry their own boundary, so they are always
allowed."
  (or (not (string-match-p (concat "\\`" noteworthy-snippets--identifier-rx) key))
      (<= start (point-min))
      (not (string-match-p (concat noteworthy-snippets--identifier-rx "\\|\\.")
                           (string (char-before start))))))

(defun noteworthy-snippets--in-variant-p (start)
  "Non-nil when START is inside a symbol variant like `amp.inv\='.
A key that fires anywhere, glued to what it follows, would otherwise
take the `inv\=' of `amp.inv\=' or the `rd\=' of `space.third\='.  The
word has to start with a letter, so the `5\=' of `2.5sr\=' is no variant."
  (save-excursion
    (goto-char start)
    (skip-chars-backward "[:alnum:]_")
    (and (eq (char-before) ?.)
         (looking-at-p "[[:alpha:]]")
         (> (1- (point)) (point-min))
         (string-match-p "[[:alnum:]_]" (string (char-before (1- (point))))))))

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
             for hit = (or (and (not (noteworthy-snippets--in-variant-p start))
                                (noteworthy-snippets--lookup key 'auto))
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
phi\\|chi\\|psi\\|omega\\|Gamma\\|Delta\\|Theta\\|Lambda\\|Xi\\|Pi\\|\
Sigma\\|Upsilon\\|Phi\\|Psi\\|Omega")
    ;; An accent or a style already applied, holding up to three levels of
    ;; parentheses: `hat(x)', `dot(arrow(r_(i)))'.
    ("ACCENTED" . "\\(?:dot\\.double\\|dot\\|hat\\|bar\\|tilde\\|arrow\\|\
underline\\|overline\\|bb\\|cal\\|frak\\|bold\\|upright\\|sans\\|mono\\)\
(\\(?:[^()]\\|(\\(?:[^()]\\|(\\(?:[^()]\\|([^()]*)\\)*)\\)*)\\)*)")
    ("ACCENTED_BODY" . "\\(?:[^()]\\|(\\(?:[^()]\\|(\\(?:[^()]\\|([^()]*)\\)*)\\)*)\\)*")
    ("LETTER" . "[A-Za-z]")
    ("DIGIT"  . "[0-9]"))
  "Named fragments usable as ${NAME} inside a rule's `:trigger'.
The LaTeX Suite equivalent of its snippet variables."
  :type '(alist :key-type string :value-type string)
  :group 'noteworthy)

(defcustom noteworthy-snippets-regex-rules
  '(;; `int' fires as soon as it is typed, which is the point of it --
    ;; but `inter' and `interleave' begin with it.
    ;; What is typed straight after the expansion shows it was the longer
    ;; word, and takes the expansion back: `integraler' -> `inter'.
    (:trigger "\\(integral\\)\\(er\\)"
     :expand noteworthy-snippets-unexpand-word :in (math) :max-length 20)
    ;; x1 -> x_1, and alpha1 -> alpha_1, since the match is anchored at point
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
    ;; `**' -> `dot', the product, which typed glued would be the accent.
    (:trigger "\\*\\*" :expand noteworthy-snippets-product-dot :in (math))
    ;; `alpha 1\=' -> `alpha_1\='.  `@a1' needs no rule of its own: the rule
    ;; above sees the `a1' it ends in.  Typed with a space, subscripting a
    ;; spelled-out greek letter has to step over it.
    (:trigger "\\(${GREEK}\\) \\(${DIGIT}\\)" :expand "\\1_\\2" :in (math))
    ;; `ddot' -> `dot.double()' over what it follows, with or without a
    ;; space: `xddot', `x ddot', `@qddot', `pvec ddot', `xhat ddot'.  Unlike
    ;; `dot' it is never the product, so it needs no `;'.  Ahead of the
    ;; postfix accents, which would read `xddot' as x beside `dot(d)'.
    (:trigger "\\(${ACCENTED}\\) ?\\(ddot\\)"
     :expand noteworthy-snippets-nested-accent :in (math) :max-length 80)
    (:trigger "\\(^\\|[^[:alnum:]_.#@]\\)\\([[:alpha:]]+\\) ?ddot"
     :expand "\\1dot.double(\\2)$0" :in (math) :max-length 40)
    ;; `@qarr' -> `arrow(theta)', `@qdot' -> `dot(theta)'.  The postfix
    ;; accents below take a single letter, so a greek letter gets a rule of
    ;; its own.  Only straight after the `@' expansion, so a `theta' typed
    ;; out and followed by `hat' later is left alone.  Glued, `dot' is the
    ;; accent as everywhere else; the product is `**'.
    (:trigger "\\(^\\|[^[:alpha:]]\\)\\([[:alpha:]]+\\)\\(dot\\|hat\\|bar\\|til\\|vec\\|arr\\|und\\|ovl\\)"
     :expand noteworthy-snippets-greek-accent :in (math))
    ;; A digit or letter run raised or lowered as a group: x_12 -> x_(12)
    (:trigger "\\(${LETTER}\\)_\\(${DIGIT}${DIGIT}+\\)" :expand "\\1_(\\2)" :in (math))
    ;; `dot;' -> `dot()' over what it follows: `arrow(r) dot;', `@qdot;',
    ;; `x dot;', `xhat dot;'.  `dot' after something with a space is the
    ;; product, as in `arrow(a) dot arrow(b)', until the `;' says otherwise.
    (:trigger "\\(${ACCENTED}\\) ?\\(dot\\);"
     :expand noteworthy-snippets-nested-accent :in (math) :max-length 80)
    (:trigger "\\(^\\|[^[:alnum:]_.#@]\\)\\([[:alpha:]]+\\) ?dot;"
     :expand "\\1dot(\\2)$0" :in (math) :max-length 40)
    ;; Accents stack: one typed glued after another, or after a style, goes
    ;; over it -- `xhatdot' -> `dot(hat(x))', `pvechat' -> `hat(arrow(p))',
    ;; `bbRtil' -> `tilde(bb(R))'.  Glued, `dot' is no product.
    (:trigger "\\(${ACCENTED}\\)\\(ddot\\|dot\\|hat\\|bar\\|til\\|vec\\|arr\\|und\\|ovl\\)"
     :expand noteworthy-snippets-nested-accent :in (math) :max-length 80)
    ;; A `;' typed after a glued `dot' that already expanded is the same
    ;; `dot;' and is dropped.
    (:trigger "\\(dot(${ACCENTED_BODY})\\);"
     :expand noteworthy-snippets-dot-semicolon :in (math) :max-length 80)
    ;; `rdot' -> `dot(r)', `udot' -> `dot(u)'.  The glued power `rd' and
    ;; subscript `ud' fire on their second letter, before the `ot' can show
    ;; it was r or u with a dot; `ot' or `dot' typed straight into the
    ;; `^()' or `_()' left takes it back.
    (:trigger "\\([_^]\\)(\\(d?\\)ot"
     :expand noteworthy-snippets-rd-dot :in (math))
    ;; The same for `sq': `square' typed out leaves `sqrt(ua)', nothing
    ;; you would put in a root.
    (:trigger "sqrt(ua"
     :expand noteworthy-snippets-square :in (math))
    ;; Postfix accents: xhat -> hat(x).  Typst has no postfix form, so the
    ;; accent has to be typed before its argument -- this puts it back the
    ;; way you think of it.  Longest alternative first, so ddot beats dot.
    (:trigger "\\(${LETTER}\\)\\(ddot\\|dot\\|hat\\|bar\\|til\\|vec\\|arr\\|und\\|ovl\\)"
     :expand noteworthy-snippets-postfix-accent :in (math))
    ;; Postfix styles: bbR -> bb(R).  The captured name is the function.
    ;; Only as a word of its own, not the end of `focal' or `local'.
    (:trigger "\\(^\\|[^[:alpha:]_.]\\)\\(bb\\|cal\\|frak\\|bold\\|upright\\|sans\\|mono\\)\\(${LETTER}\\)"
     :expand "\\1\\2(\\3)$0" :in (math))
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
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)pd\\(${LETTER}\\)\\([ ,]\\)"
     :expand "\\1partial \\2\\3" :in (math))
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)pd\\(${LETTER}\\);"
     :expand "\\1partial \\2 " :in (math))
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)\\(pd\\)\\(${LETTER}\\))"
     :expand noteworthy-snippets-bracketed-differential :in (math))
    ;; The boundary is spelled out rather than written `\\b\=': `$\=' has word
    ;; syntax in the standard table, so `$dx\=' reads as one word and `\\b\='
    ;; never matches after the dollar -- the differential at the very start
    ;; of a formula, which is where it most often sits.  Group 1 is that
    ;; boundary character, put straight back.
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)d\\(${LETTER}\\)\\([ ,]\\)"
     :expand "\\1dif \\2\\3" :in (math))
    ;; `dx;' -> `dif x ', for not typing the space, and `dx)' -> `(dif x)',
    ;; for where the differential needs bracketing -- inside a fraction or
    ;; under a power.  `dv;' and friends already build the whole
    ;; derivative, and being keyed they are tried first, so `dv;' stays the
    ;; fraction and only the other letters reach these rules.
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)d\\(${LETTER}\\);"
     :expand "\\1dif \\2 " :in (math))
    (:trigger "\\(^\\|[^[:alnum:]_-]\\)\\(d\\)\\(${LETTER}\\))"
     :expand noteworthy-snippets-bracketed-differential :in (math))
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

No space either side, so `@a_1\=', `@a^2\=' and `@a)\=' come out tight.
Glued to a letter, `x@a\=' or `@ab\=', the word splitting puts the space
in, as after any expansion."
  (cdr (assoc (car groups) noteworthy-snippets--greek)))

(defun noteworthy-snippets-product-dot (_groups)
  "`dot\=' for the `**\=' just typed.
Glued to a letter either side, `a**b\=', the word splitting spaces it."
  "dot")

(defun noteworthy-snippets-bracketed-differential (groups)
  "Expand GROUPS -- boundary, `d\=' or `pd\=', letter -- to `(dif x)\='.
When the `)\=' typed closes a `(\=' before it -- `e^(dx)\=', or one the
editor paired and stepped over -- that pair already brackets it, and
it is `e^(dif x)\='."
  (let ((dif (format "%s %s" (if (equal (nth 1 groups) "pd") "partial" "dif")
                     (nth 2 groups))))
    ;; `syntax-ppss' leaves point where it parsed to.
    (if (> (car (save-excursion (syntax-ppss (1- (point))))) 0)
        (format "%s%s)" (nth 0 groups) dif)
      (format "%s(%s)" (nth 0 groups) dif))))

(defun noteworthy-snippets-greek-differential (groups)
  "Expand GROUPS -- boundary, `d' or `pd', letter -- to a greek differential."
  (when-let* ((greek (cdr (assoc (nth 2 groups) noteworthy-snippets--greek))))
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

(defun noteworthy-snippets-nested-accent (groups)
  "Put the accent keyed in GROUPS over the accented thing in GROUPS."
  (format "%s(%s)$0"
          (cdr (assoc (nth 1 groups) noteworthy-snippets--accents))
          (nth 0 groups)))

(defvar-local noteworthy-snippets--last-end nil
  "Where the last auto-expansion left point, as a marker.")

(defun noteworthy-snippets-rd-dot (groups)
  "Turn the `^(ot\=' left by `rd\=' then `ot\=' into `dot(r)\='.
Or the `_(ot\=' of `udot\=' into `dot(u)\='.  GROUPS holds the `^\=' or
`_\=', then the `d\=' of `rddot\=', if any.  Only while the `^()\=' is
the one the last expansion left, with nothing typed since but the rest
of the accent; the empty field's `)\=' after point goes with it."
  (let ((double (equal (nth 1 groups) "d"))
        (letter (if (equal (nth 0 groups) "^") "r" "u")))
    (when (and (noteworthy-snippets--just-expanded-p (if double 3 2))
               (eq (char-after) ?\)))
      ;; Exiting moves point to the snippet's exit, past the `)'.
      (save-excursion (yas-exit-all-snippets))
      (list (format "%s(%s)$0" (if double "dot.double" "dot") letter)
            (1+ (point))))))

(defvar noteworthy-snippets--taken-back nil
  "Set by an expansion that gives back the word being typed, as `squa'.
That word is not finished, so the splitting after the expansion leaves it.")

(defun noteworthy-snippets-square (_groups)
  "Turn `sqrt(ua\=' back into the `squa\=' typed.
Only while the root is the one the last expansion left, with nothing
typed into it since."
  (when (and (noteworthy-snippets--just-expanded-p 2)
             (eq (char-after) ?\)))
    (save-excursion (yas-exit-all-snippets))
    (setq noteworthy-snippets--taken-back t)
    (list "squa$0" (1+ (point)))))

(defconst noteworthy-snippets--word-keys
  '(("integral" . "int"))
  "Expansions of keyed snippets that begin longer words, and their keys.")

(defun noteworthy-snippets-unexpand-word (groups)
  "Put back the key whose expansion GROUPS has letters glued to.
Only straight after the expansion: `integraler\=' typed then is `inter\='."
  (and (noteworthy-snippets--just-expanded-p (length (nth 1 groups)))
       (setq noteworthy-snippets--taken-back t)
       (concat (cdr (assoc (nth 0 groups) noteworthy-snippets--word-keys))
               (nth 1 groups))))

(defun noteworthy-snippets-dot-semicolon (groups)
  "Drop the `;' typed right after a dot accent that has already expanded.
`xdot;' and `pvecdot;' expand before the `;' arrives; it was meant as
the same `dot;' as everywhere else, not as a semicolon."
  (and (noteworthy-snippets--just-expanded-p 1)
       (concat (nth 0 groups) "$0")))

(defun noteworthy-snippets--just-expanded-p (n)
  "Non-nil if the last auto-expansion ended N characters before point."
  (and (markerp noteworthy-snippets--last-end)
       (eq (marker-buffer noteworthy-snippets--last-end) (current-buffer))
       (= (marker-position noteworthy-snippets--last-end) (- (point) n))))

(defun noteworthy-snippets-accent (groups)
  "Wrap the letter in GROUPS with the accent function it was suffixed with.
Declines `ddot' right after a closing parenthesis: that is a vector's
`ddot' (`noteworthy-snippets-nested-accent') being typed, not a variable d
with a dot."
  (let ((fn (cdr (assoc (nth 1 groups) noteworthy-snippets--accents))))
    (and fn
         (not (and (equal (nth 0 groups) "d") (equal (nth 1 groups) "dot")
                   (save-excursion
                     (goto-char (- (point) 4))
                     (looking-back ") ?" (max (point-min) (- (point) 2))))))
         (format "%s(%s)$0" fn (nth 0 groups)))))

(defun noteworthy-snippets-postfix-accent (groups)
  "`noteworthy-snippets-accent\=', for a letter that is a word of its own.
The `o\=' of `pound\=' is no variable to underline, nor the `n\=' of
`arrow.r.narrow\=' one to arrow."
  (let ((before (char-before (- (point) (length (nth 1 groups)) 1))))
    (and (not (and before (string-match-p "[[:alpha:].]" (string before))))
         (noteworthy-snippets-accent groups))))

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
         (when-let* ((wrapped (noteworthy-snippets-accent (cdr groups))))
           (concat (nth 0 groups) wrapped)))))

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
        ;; `Dx ' is no differential, nor is `DD' -- Typst's double-struck D.
        (case-fold-search nil))
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
                   ;; An expander may also take text after point, as
                   ;; (TEMPLATE END).
                   (cond ((consp template) (list (car template) start (cadr template)))
                         (template (list template start (point))))))))))

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

(defvar noteworthy-snippets-auto-space)

(defun noteworthy-snippets--split-lines (beg end &optional keep)
  "Split every run-together math word on the lines from BEG to END.
After a snippet expansion: an expansion is no keystroke, so the splitting
would only see a word when the next delimiter is typed -- and `xsqrt(|)'
has none coming before the root is filled in.  The whole line, not just
the word the expansion ran into, so words finished some other way, or
never, are caught too.  One undo step for all of them.  With KEEP, not
the word ending at point, which is still being typed."
  (when noteworthy-snippets-auto-space
    (let ((typing (point)))
    (save-excursion
      (let ((lo (progn (goto-char beg) (line-beginning-position))))
        (goto-char end)
        (end-of-line)
        (noteworthy-snippets--as-undo-step
         (lambda ()
           ;; Right to left, so a split never moves the words still to come.
           (while (> (progn (skip-chars-backward "^[:alpha:]" lo) (point)) lo)
             (let ((word-end (point))
                   (word-start (progn (skip-chars-backward "[:alpha:]" lo) (point))))
               (unless (and keep (= word-end typing))
                 (noteworthy-snippets--consider-word word-end))
               (goto-char word-start))))))))))

(defvar noteworthy-snippets--auto-expanding nil
  "Non-nil while `noteworthy-snippets-maybe-auto-expand' expands, which
splits the line itself once the expansion is its own undo step.")

(defun noteworthy-snippets--split-after-yas (&rest _)
  "Split the line a snippet expanded on by hand (TAB) just went into.
Advice after `yas-expand-snippet'."
  (when (and (bound-and-true-p noteworthy-typst-mode)
             (not noteworthy-snippets--auto-expanding))
    (condition-case err
        (noteworthy-snippets--split-lines (point) (point))
      (error (message "Noteworthy: splitting after a snippet failed: %s"
                      (error-message-string err))))))

(with-eval-after-load 'yasnippet
  (advice-add 'yas-expand-snippet :after #'noteworthy-snippets--split-after-yas))

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
        (let* ((noteworthy-snippets--taken-back nil)
               (hit (or (noteworthy-snippets--auto-hit)
                        (noteworthy-snippets--regex-hit))))
          (when hit
            ;; A keyed hit carries a `yas--template'; a regex rule a string.
            (let* ((template (nth 0 hit))
                   (content (if (yas--template-p template)
                                (yas--template-content template)
                              template)))
              (let ((noteworthy-snippets--auto-expanding t))
                (noteworthy-snippets--as-undo-step
                 (lambda ()
                   (if (noteworthy-snippets--fieldless-p content)
                       (noteworthy-snippets--expand-plain content (nth 1 hit) (nth 2 hit))
                     (yas-expand-snippet template (nth 1 hit) (nth 2 hit))))))
              (noteworthy-snippets--split-lines (nth 1 hit) (point)
                                               noteworthy-snippets--taken-back)
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
;;
;; What Typst knows comes from the language server when there is one:
;; tinymist completes every name in scope at a math word -- Typst's own,
;; the template's, this file's `let's -- and keeps doing so through syntax
;; errors.  The list is fetched while Emacs is idle and kept, so a word is
;; judged the moment it is finished, never later.

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

(defconst noteworthy-snippets--never-pieces '("at" "co" "wj" "pee")
  "Names in scope that a word is never split into.
Typst's `@\=', `c/o\=', word joiner and sound-recording copyright:
letters that turn up inside ordinary words far more often than they
are meant.")

(defvar noteworthy-snippets--scope-names)

(defun noteworthy-snippets--name-piece-p (piece)
  "Non-nil when PIECE is a name a word may be split into.
The names the language server has given, when it has -- so `xinA\=' is
`x in A\=' and a `let\=' from the template is found inside a word too --
and always `noteworthy-snippets--math-names\='."
  (or (member piece noteworthy-snippets--math-names)
      (and noteworthy-snippets--scope-names
           (> (length piece) 1)
           (gethash piece noteworthy-snippets--scope-names)
           (not (member piece noteworthy-snippets--never-pieces)))))

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
                (when (or (= len 1) (noteworthy-snippets--name-piece-p piece))
                  (let ((cand (append prefix (list piece)))
                        (have (aref best end)))
                    (when (or (null have) (< (length cand) (length have)))
                      (aset best end cand)))))))))
      (let* ((pieces (aref best n))
             (named (seq-some (lambda (p) (and (> (length p) 1)
                                                (noteworthy-snippets--name-piece-p p)))
                              pieces)))
        (and (> (length pieces) 1)
             (if named
                 ;; `cos t h t a' is `costheta' misspelt, not a cosine of
                 ;; four variables.  Three letters after a name is as far
                 ;; as a real product goes: `cosxyz'.
                 (not (noteworthy-snippets--letter-run-p pieces 4))
               ;; Letters alone only for a word Typst does not know, `xy'
               ;; or `xyzw' -- and not one a letter off a name, which is a
               ;; typo (`sni', `lamda') that splitting would hide.
               (and unknown
                    (not (noteworthy-snippets--near-name-p word))))
             (not (equal (car pieces) "d"))
             pieces)))))

(defun noteworthy-snippets--letter-run-p (pieces n)
  "Non-nil when PIECES has N single letters in a row."
  (let ((run 0) found)
    (dolist (p pieces)
      (setq run (if (= (length p) 1) (1+ run) 0))
      (when (>= run n) (setq found t)))
    found))

(defun noteworthy-snippets--one-deletion-p (long short)
  "Non-nil when deleting one character from LONG gives SHORT."
  (and (= (length long) (1+ (length short)))
       (cl-loop for i to (length short)
                thereis (and (string= (substring long 0 i) (substring short 0 i))
                             (string= (substring long (1+ i)) (substring short i))))))

(defun noteworthy-snippets--near-name-p (word)
  "Non-nil when WORD is one edit from a Typst name.
`sni' and `tna' are `sin' and `tan' mistyped, not three variables.  A
letter left out or added only from four letters on, `lamda', so `xy' is
never taken for `xi' nor `pir' for `pi'."
  (or (and (>= (length word) 4)
           (seq-some (lambda (name)
                       (or (noteworthy-snippets--one-deletion-p name word)
                           (noteworthy-snippets--one-deletion-p word name)))
                     noteworthy-snippets--typst-math-scope))
      (noteworthy-snippets--substitution-p word)))

(defun noteworthy-snippets--substitution-p (word)
  "Non-nil when WORD is a Typst name as long as it with two neighbours swapped.
Or with one letter wrong, from four letters on: three letters one off a
name are as often variables, `abc\=' beside `abs\='."
  (and (>= (length word) 3)
       (seq-some (lambda (name)
                   (and (= (length name) (length word))
                        (let ((diff (cl-loop for a across word for b across name
                                             count (/= a b))))
                          (or (and (= diff 1) (>= (length word) 4))
                              ;; two neighbours swapped
                              (and (= diff 2)
                                   (cl-loop for i below (1- (length word))
                                            thereis (and (/= (aref word i) (aref name i))
                                                         (= (aref word i) (aref name (1+ i)))
                                                         (= (aref word (1+ i)) (aref name i)))))))))
                 noteworthy-snippets--typst-math-scope)))

(defun noteworthy-snippets--defined-name-p (word)
  "Non-nil when WORD is bound with `let' somewhere in this buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "\\_<let[ \t]+" (regexp-quote word) "\\_>") nil t)))

(defvar-local noteworthy-snippets--scope-names nil
  "Names in scope in this buffer's math, as the language server completes
them: a hash table, or nil until it has answered.")

(defvar-local noteworthy-snippets--scope-tick nil
  "`buffer-chars-modified-tick' when `noteworthy-snippets--scope-names' was
last asked for.")

(defun noteworthy-snippets--lsp-p ()
  "Non-nil when a language server is attached to this buffer."
  (and (bound-and-true-p lsp-mode)
       (fboundp 'lsp-workspaces)
       (lsp-workspaces)))

(defun noteworthy-snippets--math-word-before (end)
  "Bounds (START . END) of the math word ending at END, or nil.
A run of two or more letters, not after `.' (a field, as in `arrow.r'),
`#', `_', `\\' or `@', nor glued to a letter before it.  After a digit it
is a coefficient's variables: `2xy' is `2x y'."
  (save-excursion
    (goto-char end)
    (skip-chars-backward "[:alpha:]")
    (let ((start (point)))
      (and (>= (- end start) 2)
           (not (memq (char-before start) '(?. ?# ?_ ?\\ ?@)))
           (not (and (char-before start)
                     (string-match-p "[[:alpha:]]" (string (char-before start)))))
           (progn (goto-char end) (eq (noteworthy-typst-context) 'math))
           (cons start end)))))

(defvar-local noteworthy-snippets--splits nil
  "Recent splits, newest first, each (START END WORD TICK).
START and END are markers around the split text, WORD what it replaced,
TICK `buffer-chars-modified-tick' right after.")

(defconst noteworthy-snippets--splits-max 50
  "How many splits `noteworthy-snippets-undo-split' can walk back through.")

(defvar evil-undo-list-pointer)
(defvar undo-auto--last-boundary-cause)
(declare-function evil-start-undo-step "evil-common" (&optional continue))
(declare-function evil-end-undo-step "evil-common" (&optional continue))

(defvar noteworthy-snippets--in-undo-step nil
  "Non-nil inside `noteworthy-snippets--as-undo-step', whose step a nested
call joins: a line's splits are one step, not one per word.")

(defun noteworthy-snippets--as-undo-step (fn)
  "Call FN as an undo step of its own, between what came before and after.
So `u' walks back through a split or a snippet expansion like any other
change: what was typed after it, then it alone, then what was typed
before -- for an expansion, back to the key that triggered it.

Two things would otherwise swallow it.  Evil makes one step of a whole
insert, so that step is closed before FN and a fresh one opened after.
And the next keystroke would amalgamate into it -- `self-insert-command'
drops the boundary in front of it when the last command was one too --
so the record of what caused the last boundary is reset to a command,
which nothing amalgamates with.  Emacs does not update it itself here:
it only does so when it adds a boundary, and FN's is already there."
  (if noteworthy-snippets--in-undo-step
      (funcall fn)
    (let ((in-evil-step (bound-and-true-p evil-undo-list-pointer))
          (noteworthy-snippets--in-undo-step t))
      (if in-evil-step (evil-end-undo-step) (undo-boundary))
      (funcall fn)
      (undo-boundary)
      (setq undo-auto--last-boundary-cause 'command)
      (when in-evil-step (evil-start-undo-step)))))

(defun noteworthy-snippets--replace-word (start end pieces)
  "Replace START..END with PIECES, a space between each.
An undo step of its own: see `noteworthy-snippets--as-undo-step'."
  (let ((word (buffer-substring-no-properties start end))
        (end (copy-marker end t)))
    (save-excursion
      ;; Only the spaces are inserted, right to left, so the text around
      ;; them is never touched: the word may run into a snippet still being
      ;; filled in, `x' against `sqrt(|)', whose fields would not survive
      ;; their text being deleted and put back.
      (noteworthy-snippets--as-undo-step
       (lambda ()
         (let ((pos start) cuts)
           (dolist (piece (butlast pieces))
             (setq pos (+ pos (length piece)))
             (push pos cuts))
           (dolist (cut cuts)
             (goto-char cut)
             (insert " ")))))
      (push (list (copy-marker start) end word
                  (buffer-chars-modified-tick))
            noteworthy-snippets--splits)
      (when-let* ((old (nthcdr noteworthy-snippets--splits-max
                               noteworthy-snippets--splits)))
        (dolist (entry old)
          (set-marker (nth 0 entry) nil) (set-marker (nth 1 entry) nil))
        (setcdr (nthcdr (1- noteworthy-snippets--splits-max)
                        noteworthy-snippets--splits)
                nil)))))

(defun noteworthy-snippets--split-intact-p (entry)
  "Non-nil when the split in ENTRY still reads as it was left."
  (pcase-let ((`(,start ,end ,word ,_) entry))
    (and (marker-buffer start)
         (let ((text (buffer-substring-no-properties start end)))
           (and (string-search " " text)
                (equal (replace-regexp-in-string " " "" text) word))))))

(defun noteworthy-snippets--unsplit (entry)
  "Put back the word of the split ENTRY, as it was typed.
Nothing is remembered: the word splits again if it is finished again."
  (pcase-let ((`(,start ,end ,word ,_) entry))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert word))
    (set-marker start nil) (set-marker end nil)
    (setq noteworthy-snippets--splits (delq entry noteworthy-snippets--splits))
    (message "Unsplit `%s'" word)
    t))

(defun noteworthy-snippets--intact-splits ()
  "The splits that still read as they were left, newest first.
Forgets the rest."
  (let (keep)
    (dolist (entry noteworthy-snippets--splits)
      (if (noteworthy-snippets--split-intact-p entry)
          (push entry keep)
        (set-marker (nth 0 entry) nil) (set-marker (nth 1 entry) nil)))
    (setq noteworthy-snippets--splits (nreverse keep))))

(defun noteworthy-snippets-undo-split ()
  "Undo the most recent auto-space split.
Only the split: what was typed since stays."
  (interactive)
  (let ((entry (car (noteworthy-snippets--intact-splits))))
    (unless entry (user-error "No split to undo"))
    (noteworthy-snippets--unsplit entry)))

(defun noteworthy-snippets-unsplit-at-point ()
  "Undo the auto-space split under the cursor, or else the nearest on its line.
Any split still intact, however long ago -- only the split is taken back,
and the word goes back as it was typed."
  (interactive)
  (let* ((pt (point))
         (splits (noteworthy-snippets--intact-splits))
         (entry (or (seq-find (lambda (e) (<= (nth 0 e) pt (nth 1 e))) splits)
                    (car (sort (seq-filter
                                (lambda (e)
                                  (and (>= (nth 0 e) (line-beginning-position))
                                       (<= (nth 1 e) (line-end-position))))
                                splits)
                               (lambda (a b)
                                 (< (min (abs (- pt (nth 0 a))) (abs (- pt (nth 1 a))))
                                    (min (abs (- pt (nth 0 b))) (abs (- pt (nth 1 b)))))))))))
    (unless entry (user-error "No split on this line"))
    (noteworthy-snippets--unsplit entry)))

(defun noteworthy-snippets--consider-word (end)
  "Split the math word ending at END if it is no name in scope.
With the language server's names, any word that is not one of them is
split -- `xy' too, which is two variables.  Without them, only words that
split into known names and are not bound by a `let' in this file."
  (when-let* ((bounds (noteworthy-snippets--math-word-before end)))
    (let ((word (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (cond
       ((member word noteworthy-snippets--typst-math-scope))
       (noteworthy-snippets--scope-names
        (unless (or (gethash word noteworthy-snippets--scope-names)
                    ;; Defined since the names were fetched, or below the
                    ;; word, where Typst's scope does not reach yet.
                    (noteworthy-snippets--defined-name-p word))
          (when-let* ((pieces (noteworthy-snippets--split-word word t)))
            (noteworthy-snippets--replace-word (car bounds) (cdr bounds) pieces))))
       ((> (length word) 2)
        (let ((pieces (noteworthy-snippets--split-word word)))
          (when (and pieces (not (noteworthy-snippets--defined-name-p word)))
            (noteworthy-snippets--replace-word (car bounds) (cdr bounds) pieces))))))))

(defun noteworthy-snippets--scope-probe ()
  "A position just after a one-letter math word, as the `x' in `x + 1', or nil.
Completing there gives every name in scope; after the first letter of a
longer word the server filters by it.  The word nearest point before it,
else after it, so the names are the ones in scope here."
  (let ((re "\\(?:^\\|[^[:alnum:]_.#@\\]\\)\\([[:alpha:]]\\)\\(?:[^[:alnum:]_]\\|$\\)")
        found)
    (save-excursion
      (cl-loop repeat 50
               while (and (not found) (re-search-backward re nil t))
               do (save-excursion
                    (goto-char (match-end 1))
                    (when (eq (noteworthy-typst-context) 'math)
                      (setq found (point))))))
    (save-excursion
      (cl-loop repeat 50
               while (and (not found) (re-search-forward re nil t))
               do (save-excursion
                    (goto-char (match-end 1))
                    (when (eq (noteworthy-typst-context) 'math)
                      (setq found (point))))
               ;; The character after the letter may begin the next match.
               do (goto-char (match-end 1))))
    found))

(defun noteworthy-snippets--json-get (object key)
  "KEY of a JSON OBJECT as lsp-mode parsed it, a hash table or a plist."
  (if (hash-table-p object)
      (gethash (substring (symbol-name key) 1) object)
    (plist-get object key)))

(defun noteworthy-snippets--store-scope (result)
  "Keep the names in the completion RESULT as this buffer's scope."
  (let ((items (if (or (vectorp result) (and (listp result) (not (keywordp (car result)))))
                   result
                 (noteworthy-snippets--json-get result :items)))
        (names (make-hash-table :test #'equal)))
    (seq-doseq (item items)
      (when-let* ((label (noteworthy-snippets--json-get item :label)))
        (puthash label t names)))
    ;; An empty answer is a probe that went wrong, not a scope with nothing
    ;; in it: Typst alone has hundreds of names.
    (when (> (hash-table-count names) 0)
      (setq noteworthy-snippets--scope-names names))))

(defun noteworthy-snippets--refresh-scope ()
  "Ask the language server for the names in scope, if the buffer changed."
  (when (and noteworthy-snippets-auto-space
             (bound-and-true-p noteworthy-typst-mode)
             (noteworthy-snippets--lsp-p)
             ;; Not while the server is still starting up.
             (lsp-feature? "textDocument/completion")
             (not (eql noteworthy-snippets--scope-tick (buffer-chars-modified-tick))))
    (when-let* ((pos (noteworthy-snippets--scope-probe)))
      (setq noteworthy-snippets--scope-tick (buffer-chars-modified-tick))
      (let ((buffer (current-buffer)))
        (lsp-request-async
         "textDocument/completion"
         (append (save-excursion
                   (goto-char pos)
                   (lsp--text-document-position-params))
                 '(:context (:triggerKind 1)))
         (lambda (result)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (noteworthy-snippets--store-scope result))))
         :mode 'alive)))))

(defvar noteworthy-snippets--scope-timer nil
  "Idle timer running `noteworthy-snippets--refresh-scope'.")

(defun noteworthy-snippets--refresh-scope-idle ()
  "Refresh the scope of the current buffer; errors are only reported."
  (condition-case err
      (noteworthy-snippets--refresh-scope)
    (error (message "Noteworthy: fetching math names failed: %s"
                    (error-message-string err)))))

(defun noteworthy-snippets-maybe-space-word ()
  "Split the math word just finished, if it runs names together.
The word is finished by anything that is no letter, digit, `.\=' or `@\='
-- a space, but just as well the `^\=' of `costheta^2\=' or the `+\=' of
`ab+c\=', so a word never waits for a space.  Not by `@\=', which begins a
greek letter that may belong to the word, as in `pd@q\='; nor by a
character a snippet has already taken: `dx;\=' is no word and a `;\='.  Runs from
`post-self-insert-hook'; see `noteworthy-snippets--consider-word'."
  (when (and noteworthy-snippets-auto-space
             (bound-and-true-p noteworthy-typst-mode)
             (characterp last-command-event)
             (eq (char-before) last-command-event)
             (not (string-match-p "[[:alnum:].@]" (string last-command-event))))
    (noteworthy-snippets--consider-word (1- (point)))))

(defun noteworthy-snippets-maybe-space-word-at-dollar ()
  "Split the math word the closing `$' just finished, as `$xy$'.
Runs from `post-command-hook': `noteworthy-typst-smart-dollar' inserts the
`$' itself, or steps over the paired one, so no self-insert is seen."
  (when (and noteworthy-snippets-auto-space
             (eq this-command 'noteworthy-typst-smart-dollar)
             (eq (char-before) ?$))
    (noteworthy-snippets--consider-word (1- (point)))))

(defvar evil-state)

(defvar-local noteworthy-snippets--typed-word nil
  "(START END WORD) of the word the last command typed, if point ended one.")

(defun noteworthy-snippets--insert-state-p ()
  "Whether this buffer is typing: evil's insert state, or no evil at all."
  (or (not (bound-and-true-p evil-local-mode)) (eq evil-state 'insert)))

(defun noteworthy-snippets-maybe-space-left-word ()
  "Split the word just typed if this command finished it by leaving it.
Whatever ends the typing does: RET, ESC, TAB out of `$...$', an arrow
key, a click.  A typed delimiter is `noteworthy-snippets-maybe-space-word's;
moving back into the word, changing it, or undoing is no finishing.
Runs from `post-command-hook', and notes the word a typed letter leaves
point after for the next command."
  (let ((typed noteworthy-snippets--typed-word))
    ;; No command at all, as at the end of a keyboard macro: nothing left.
    (when this-command (setq noteworthy-snippets--typed-word nil))
    (when (and this-command noteworthy-snippets-auto-space
               (bound-and-true-p noteworthy-typst-mode))
      (if (eq this-command 'self-insert-command)
          (when (and (noteworthy-snippets--insert-state-p)
                     (char-before)
                     (string-match-p "[[:alpha:]]" (string (char-before))))
            (let ((end (point)))
              (save-excursion
                (skip-chars-backward "[:alpha:]")
                (setq noteworthy-snippets--typed-word
                      (list (point) end (buffer-substring-no-properties (point) end))))))
        (pcase-let ((`(,start ,end ,word) typed))
          (when (and typed
                     (not (and (symbolp this-command)
                               (string-match-p "undo" (symbol-name this-command))))
                     (<= end (point-max))
                     (equal (buffer-substring-no-properties start end) word)
                     (not (and (char-after end)
                               (string-match-p "[[:alpha:]]" (string (char-after end)))))
                     (or (not (noteworthy-snippets--insert-state-p))
                         (< (point) start) (> (point) end)))
            (noteworthy-snippets--consider-word end)))))))

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
              #'noteworthy-snippets-maybe-space-word-at-dollar nil t)
    (add-hook 'post-command-hook #'noteworthy-snippets-maybe-space-left-word nil t)
    (unless noteworthy-snippets--scope-timer
      (setq noteworthy-snippets--scope-timer
            (run-with-idle-timer 0.5 t #'noteworthy-snippets--refresh-scope-idle)))))

(define-key noteworthy-typst-mode-map (kbd "C-c C-u") #'noteworthy-snippets-unsplit-at-point)

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
