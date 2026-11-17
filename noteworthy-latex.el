;;; noteworthy-latex.el --- The Noteworthy typing layer for LaTeX  -*- lexical-binding: t; -*-

;;; Commentary:
;; The same typing layer `noteworthy-snippets' gives Typst, for AUCTeX's
;; `LaTeX-mode': keyed snippets that expand as the key is typed, regex rules
;; for the shapes a key cannot spell (postfix accents, subscripts,
;; differentials), takebacks, and one undo step per expansion.
;;
;; Turn it on from the mode hook:
;;
;;     (add-hook 'LaTeX-mode-hook #'noteworthy-latex-setup)
;;
;; What is typed, and what comes out, in math (`\(...\)', `$...$',
;; `equation', `align', ...):
;;
;;   @a        \alpha            @Q        \Theta
;;   xhat      \hat{x}           xhatdot   \dot{\hat{x}}
;;   @qdot     \dot{\theta}      v^2ovl    \overline{v^2}
;;   bbR       \mathbb{R}        x1        x_1       x12   x_{12}
;;   dx<SPC>   \,\mathrm{d}x     kdx;      k\,\mathrm{d}x
;;   costheta<SPC>   \cos\theta<SPC>
;;
;; The context comes from `texmathp'.  Typed in a command name (`\sqrt'),
;; in a `\label{...}', or in a comment, nothing fires.
;;
;; Two things have no Typst counterpart, and exist because LaTeX control
;; words end at the first non-letter:
;;
;; - The control-word guard.  A letter typed straight after an expansion that
;;   ended in a control word gets a space first, so `@ax' is `\alpha x' and
;;   never `\alphax'.  A few letters are held back: the start of an accent
;;   (`@qdot') or of a word an expansion was cut out of (`int' + `er').
;; - Auto-backslash.  A word of known command names, finished by a delimiter,
;;   gets its backslashes: `sin(' is `\sin(', `costheta ' is `\cos\theta '.

;;; Code:

(require 'cl-lib)
(require 'noteworthy-snippets)

(defvar noteworthy-latex-mode)
(defvar noteworthy-snippets--taken-back)
(defvar noteworthy-snippets--last-end)
(defvar noteworthy-snippets-auto-space)
(defvar noteworthy-snippets-protect-expansion)
(defvar evil-surround-pairs-alist)
(declare-function texmathp "texmathp" ())
(declare-function TeX-in-comment "tex" ())
(declare-function yas-next-field "yasnippet" (&optional arg))
(declare-function yas-exit-all-snippets "yasnippet" ())
(declare-function yas--template-content "yasnippet" (template))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(defgroup noteworthy-latex nil
  "Noteworthy typing layer for LaTeX."
  :group 'noteworthy)

(defcustom noteworthy-latex-auto-backslash t
  "Whether a finished math word like `costheta' gets its backslashes."
  :type 'boolean
  :group 'noteworthy-latex)

;;; Context

(defconst noteworthy-latex--argument-commands
  (concat "\\\\\\(?:label\\|[A-Za-z]*ref\\|[A-Za-z]*cite[A-Za-z]*\\|begin\\|end"
          "\\|input\\|include\\|includegraphics\\|usepackage\\|documentclass"
          "\\|url\\|href\\|bibliography\\|bibliographystyle\\)"
          "\\*?\\(?:\\[[^]]*\\]\\)*")
  "Commands whose braces hold names, not math.")

(defun noteworthy-latex--in-argument-p ()
  "Non-nil when point is inside the braces of a name-taking command."
  (save-excursion
    (let ((open (nth 1 (syntax-ppss))) found)
      (while (and open (not found))
        (goto-char open)
        (if (and (eq (char-after) ?{)
                 (looking-back noteworthy-latex--argument-commands
                               (line-beginning-position)))
            (setq found t)
          (setq open (nth 1 (syntax-ppss open)))))
      found)))

(defun noteworthy-latex--typing-command-p ()
  "Non-nil when point is in a control word being typed, `\\sq|'.
Not when the letters before point are an expansion's control word with
more glued on, `\\alpha' and the `dot' of `@qdot' -- the guard sorts those."
  (save-excursion
    (let ((point (point)))
      (skip-chars-backward "A-Za-z")
      (and (eq (char-before) ?\\)
           ;; `\\' is a line break, not the start of a word.
           (cl-oddp (- (point) (save-excursion (skip-chars-backward "\\\\") (point))))
           (not (and (markerp noteworthy-snippets--last-end)
                     (eq (marker-buffer noteworthy-snippets--last-end) (current-buffer))
                     (< (point) (marker-position noteworthy-snippets--last-end))
                     (<= (marker-position noteworthy-snippets--last-end) point)))))))

(defun noteworthy-latex-context ()
  "The context at point: `comment', `command', `arg', `math' or `text'.
`command' is a control word being typed and `arg' the name in a
`\\label{}'; no snippet or rule belongs to either."
  (cond
   ((if (fboundp 'TeX-in-comment) (TeX-in-comment) (nth 4 (syntax-ppss))) 'comment)
   ((noteworthy-latex--typing-command-p) 'command)
   ((noteworthy-latex--in-argument-p) 'arg)
   ((and (fboundp 'texmathp) (ignore-errors (texmathp))) 'math)
   (t 'text)))

;;; Expanders

(defun noteworthy-latex--esc (string)
  "STRING as yasnippet template text: backslash, dollar and backquote escaped."
  (replace-regexp-in-string "[\\$`]" "\\\\\\&" string))

(defconst noteworthy-latex--accents
  '(("ddot" . "ddot") ("dot" . "dot") ("hat" . "hat") ("bar" . "bar")
    ("til" . "tilde") ("vec" . "vec") ("arr" . "vec")
    ("und" . "underline") ("ovl" . "overline"))
  "Postfix accent triggers and the LaTeX command each one wraps with.")

(defconst noteworthy-latex--styles
  '(("bb" . "mathbb") ("cal" . "mathcal") ("frak" . "mathfrak") ("bold" . "mathbf"))
  "Style triggers and their commands.")

(defun noteworthy-latex--command (fn arg)
  "Raw LaTeX for command FN applied to ARG."
  (format "\\%s{%s}" fn arg))

(defun noteworthy-latex--script-char-before (n)
  "The character N characters before the text just matched, when a script.
N is how long the match is, from point back."
  (let ((c (char-before (- (point) n))))
    (and c (memq c '(?^ ?_)) c)))

(defun noteworthy-latex-greek (groups)
  "Expand the letter in GROUPS to a greek control word."
  (when-let* ((name (cdr (assoc (car groups) noteworthy-snippets--greek))))
    (noteworthy-latex--esc (concat "\\" name))))

(defun noteworthy-latex--differential (boundary kind var &optional tight)
  "Raw LaTeX for a differential of VAR after BOUNDARY.
KIND is `d' or `pd'.  A thin space comes before `d', unless TIGHT or
what precedes is an opening bracket or a slash, where it has no business."
  (concat boundary
          (if (equal kind "pd")
              (concat "\\partial " var)
            (concat (if (or tight (member boundary '("/" "(" "{" "[")))
                        "" "\\,")
                    "\\mathrm{d}" var))))

(defun noteworthy-latex-greek-differential (groups)
  "Expand GROUPS -- boundary, `d' or `pd', letter -- to a greek differential."
  (when-let* ((greek (cdr (assoc (nth 2 groups) noteworthy-snippets--greek))))
    (noteworthy-latex--esc
     (noteworthy-latex--differential (nth 0 groups) (nth 1 groups)
                                     (concat "\\" greek)))))

(defun noteworthy-latex-differential (groups)
  "Expand GROUPS -- boundary, `d' or `pd', letter, delimiter.
A space or a comma is kept; `;' is a space, for not typing it; `/' makes
a fraction of what is before it and the differential goes tight."
  (let ((delim (nth 3 groups)))
    (noteworthy-latex--esc
     (concat (noteworthy-latex--differential (nth 0 groups) (nth 1 groups)
                                             (nth 2 groups) (equal delim "/"))
             (if (equal delim ";") " " delim)))))

(defun noteworthy-latex--unclosed-paren-p ()
  "Non-nil when a `(' on this line before the character at point has no `)'."
  (let ((depth 0))
    (save-excursion
      (let ((end (1- (point))))
        (goto-char (line-beginning-position))
        (while (< (point) end)
          (let ((c (char-after)))
            (unless (eq (char-before) ?\\)
              (cond ((eq c ?\() (cl-incf depth))
                    ((eq c ?\)) (cl-decf depth)))))
          (forward-char 1))))
    (> depth 0)))

(defun noteworthy-latex-bracketed-differential (groups)
  "Expand GROUPS -- boundary, `d' or `pd', letter -- when `)' was typed.
Bracketed, unless that `)' closes a `(' already open."
  (let ((dif (noteworthy-latex--differential "" (nth 1 groups) (nth 2 groups))))
    (noteworthy-latex--esc
     (if (noteworthy-latex--unclosed-paren-p)
         (format "%s%s)" (nth 0 groups) dif)
       (format "%s(%s)" (nth 0 groups) dif)))))

(defun noteworthy-latex-glued-differential (groups)
  "Expand GROUPS -- character, `d' or `pd', letter, `;' or `)' -- for `kdx;'."
  (let ((dif (noteworthy-latex--differential "" (nth 1 groups) (nth 2 groups))))
    (if (equal (nth 3 groups) ";")
        (noteworthy-latex--esc (concat (nth 0 groups) dif " "))
      (noteworthy-latex--esc
       (if (noteworthy-latex--unclosed-paren-p)
           (format "%s%s)" (nth 0 groups) dif)
         (format "%s(%s)" (nth 0 groups) dif))))))

(defun noteworthy-latex-nested-accent (groups)
  "Put the accent keyed in GROUPS over the accented thing in GROUPS."
  (let* ((fn (cdr (assoc (nth 1 groups) noteworthy-latex--accents)))
         (raw (noteworthy-latex--command fn (nth 0 groups))))
    (noteworthy-latex--esc
     (if (noteworthy-latex--script-char-before
          (+ (length (nth 0 groups)) (length (nth 1 groups))))
         (concat "{" raw "}")
       raw))))

(defun noteworthy-latex-accent (groups)
  "Wrap the letter in GROUPS with the accent it was suffixed with.
A letter that is a word of its own: not the tail of a control word or a
longer word.  Straight after `^' or `_' the result is braced."
  (let* ((fn (cdr (assoc (nth 1 groups) noteworthy-latex--accents)))
         (before (char-before (- (point) (length (nth 1 groups)) 1))))
    (and fn
         (not (and before (string-match-p "[[:alpha:].\\\\]" (string before))))
         (let ((raw (noteworthy-latex--command fn (nth 0 groups))))
           (noteworthy-latex--esc
            (if (memq before '(?^ ?_)) (concat "{" raw "}") raw))))))

(defun noteworthy-latex--greek-command-p (command)
  "Non-nil when COMMAND, backslash and all, is one `@' gives."
  (and (string-prefix-p "\\" command)
       (rassoc (substring command 1) noteworthy-snippets--greek)))

(defun noteworthy-latex-greek-accent (groups)
  "Wrap the greek letter in GROUPS with its accent, right after `@'.
Declines unless the letter is the one the last expansion inserted, with
nothing typed since but the accent itself."
  (let ((accent (nth 1 groups)))
    (and (noteworthy-latex--greek-command-p (nth 0 groups))
         (noteworthy-snippets--just-expanded-p (length accent))
         (when-let* ((fn (cdr (assoc accent noteworthy-latex--accents))))
           (noteworthy-latex--esc (noteworthy-latex--command fn (nth 0 groups)))))))

(defun noteworthy-latex-scripted-accent (groups)
  "Wrap the atom and scripts in GROUPS with the accent suffixed to them.
GROUPS is what precedes the scripts -- an accented thing, a control word
or a run of letters -- the scripts, and the accent.  Of a run of letters
the atom is the last: `kv^2' is v's, and `k' stays in front."
  (when-let* ((fn (cdr (assoc (nth 2 groups) noteworthy-latex--accents))))
    (let* ((run (nth 0 groups))
           (atom (if (string-prefix-p "\\" run) run (substring run -1)))
           (rest (substring run 0 (- (length run) (length atom)))))
      (noteworthy-latex--esc
       (concat rest (noteworthy-latex--command fn (concat atom (nth 1 groups))))))))

(defun noteworthy-latex-style (groups)
  "Expand GROUPS -- boundary, style, letter -- to the style command."
  (noteworthy-latex--esc
   (concat (nth 0 groups)
           (noteworthy-latex--command (cdr (assoc (nth 1 groups) noteworthy-latex--styles))
                                      (nth 2 groups)))))

(defun noteworthy-latex--subscriptable-p (n)
  "Non-nil when the letter N characters before point may take a subscript.
It may when it is a letter of its own or ends a greek control word, but
not when it ends `\\sqrt' or `\\frac', where the digit is an argument."
  (and (not (memq (char-before (- (point) n 1)) '(?^ ?_)))
       (save-excursion
         (goto-char (- (point) n))
         (let ((end (point)))
           (skip-chars-backward "A-Za-z")
           (or (not (eq (char-before) ?\\))
               (noteworthy-latex--greek-command-p
                (buffer-substring-no-properties (1- (point)) end)))))))

(defun noteworthy-latex-subscript (groups)
  "Expand GROUPS -- letter, digit -- to `x_1'."
  (and (noteworthy-latex--subscriptable-p (length (nth 1 groups)))
       (noteworthy-latex--esc (format "%s_%s" (nth 0 groups) (nth 1 groups)))))

(defun noteworthy-latex-subscript-group (groups)
  "Expand GROUPS -- letter, digits -- to `x_{12}'."
  (noteworthy-latex--esc (format "%s_{%s}" (nth 0 groups) (nth 1 groups))))

(defconst noteworthy-latex--word-keys '(("int" . "int") ("nabla" . "del"))
  "Expansions of keyed snippets that begin longer words, and their keys.")

(defun noteworthy-latex-unexpand-word (groups)
  "Put back the key whose expansion GROUPS has letters glued to.
GROUPS is the command and the letters; only straight after the expansion:
`int' then `er' is `inter', `del' then `t' is `delt'."
  (and (noteworthy-snippets--just-expanded-p (length (nth 1 groups)))
       (setq noteworthy-snippets--taken-back t)
       (noteworthy-latex--esc
        (concat (cdr (assoc (nth 0 groups) noteworthy-latex--word-keys))
                (nth 1 groups)))))

(defun noteworthy-latex-pmat (_groups)
  "The `pmat;' snippet, for the `at;' typed after the `\\pm' that `pm' made."
  (when (noteworthy-snippets--just-expanded-p 3)
    (setq noteworthy-snippets--taken-back t)
    (when-let* ((template (noteworthy-snippets--lookup "pmat;" 'auto)))
      (yas--template-content template))))

(defun noteworthy-latex-rd-dot (groups)
  "Turn the `^{ot' left by `rd' then `ot' into `\\dot{r}'.
Or the `_{ot' of `udot' into `\\dot{u}'; GROUPS is the mark and the `d' of
`ddot', if any.  Only while the braces are the ones the last expansion
left, with nothing typed since but the rest of the accent."
  (let ((double (equal (nth 1 groups) "d"))
        (letter (if (equal (nth 0 groups) "^") "r" "u")))
    (when (and (noteworthy-snippets--just-expanded-p (if double 3 2))
               (eq (char-after) ?}))
      (save-excursion (yas-exit-all-snippets))
      (list (noteworthy-latex--esc
             (noteworthy-latex--command (if double "ddot" "dot") letter))
            (1+ (point))))))

(defun noteworthy-latex-square (_groups)
  "Turn `\\sqrt{ua' back into the `squa' typed."
  (when (and (noteworthy-snippets--just-expanded-p 2)
             (eq (char-after) ?}))
    (save-excursion (yas-exit-all-snippets))
    (setq noteworthy-snippets--taken-back t)
    (list "squa" (1+ (point)))))

(defun noteworthy-latex-sqrt (_groups)
  "Turn the `rt' typed into `\\sqrt{}' by `sq' and `rt' -- `sqrt' -- into a root.
Only while the braces are the ones the last expansion left."
  (when (and (noteworthy-snippets--just-expanded-p 2)
             (eq (char-after) ?}))
    (save-excursion (yas-exit-all-snippets))
    (list "\\\\sqrt{$1}$0" (1+ (point)))))

;;; Rules

(defconst noteworthy-latex--accent-words "ddot\\|dot\\|hat\\|bar\\|til\\|vec\\|arr\\|und\\|ovl"
  "Alternatives for the postfix accent triggers; `ddot' ahead of `dot'.")

(defconst noteworthy-latex--body2
  "\\(?:[^{}]\\|{\\(?:[^{}]\\|{[^{}]*}\\)*}\\)*"
  "Braced text holding up to two levels of braces.")

(defcustom noteworthy-latex-regex-variables
  `(("LETTER" . "[A-Za-z]")
    ("DIGIT"  . "[0-9]")
    ("ACCENT" . ,noteworthy-latex--accent-words)
    ;; An accent or style already applied, `\hat{x}', `\dot{\vec{r_{i}}}'.
    ("ACCENTED" . ,(concat "\\\\\\(?:ddot\\|dot\\|hat\\|bar\\|tilde\\|vec\\|"
                           "underline\\|overline\\|mathbb\\|mathcal\\|mathfrak\\|mathbf\\)"
                           "{" noteworthy-latex--body2 "}"))
    ;; Not a differential after these: `\d', `x_d', `e^d'.
    ("NOT_WORD" . "[^[:alnum:]\\\\_^]"))
  "`${NAME}' fragments for `noteworthy-latex-regex-rules'."
  :type '(alist :key-type string :value-type string)
  :group 'noteworthy-latex)

(defcustom noteworthy-latex-regex-rules
  '(;; The word an expansion was cut out of, put back: `int' then `er'.
    (:trigger "\\\\\\(int\\)\\(er\\)"
     :expand noteworthy-latex-unexpand-word :in (math) :max-length 20)
    (:trigger "\\\\\\(nabla\\)\\(t\\)"
     :expand noteworthy-latex-unexpand-word :in (math) :max-length 20)
    ;; `pm' is `\pm' before `at;' says it was the matrix.
    (:trigger "\\\\pmat;"
     :expand noteworthy-latex-pmat :in (math) :max-length 20)
    ;; x1 -> x_1
    (:trigger "\\(${LETTER}\\)\\(${DIGIT}\\)"
     :expand noteworthy-latex-subscript :in (math))
    ;; d@q -> \,\mathrm{d}\theta, pd@q -> \partial \theta
    (:trigger "\\(^\\|${NOT_WORD}\\)\\(p?d\\)@\\([A-Za-z]\\)"
     :expand noteworthy-latex-greek-differential :in (math))
    ;; @a -> \alpha
    (:trigger "@\\([A-Za-z]\\)" :expand noteworthy-latex-greek :in (math))
    ;; Accents stack: `xhatdot' -> \dot{\hat{x}}, `bbRtil' -> \tilde{\mathbb{R}}.
    (:trigger "\\(${ACCENTED}\\)\\(${ACCENT}\\)"
     :expand noteworthy-latex-nested-accent :in (math) :max-length 80)
    ;; `@qdot' -> \dot{\theta}: only straight after the `@' expansion.
    (:trigger "\\(\\\\[A-Za-z]+?\\)\\(${ACCENT}\\)"
     :expand noteworthy-latex-greek-accent :in (math) :max-length 30)
    ;; x_12 -> x_{12}
    (:trigger "\\(${LETTER}\\)_\\(${DIGIT}${DIGIT}+\\)"
     :expand noteworthy-latex-subscript-group :in (math))
    ;; `rd' and `ud' fire before `rdot' and `udot' are typed out.
    (:trigger "\\([_^]\\){\\(d?\\)ot"
     :expand noteworthy-latex-rd-dot :in (math))
    ;; `sq' and `square', `sqrt'.
    (:trigger "\\\\sqrt{ua"
     :expand noteworthy-latex-square :in (math))
    (:trigger "\\\\sqrt{rt"
     :expand noteworthy-latex-sqrt :in (math))
    ;; An accent covers the atom before it with its scripts: `v^2ovl' ->
    ;; \overline{v^2}, `x_{ij}hat' -> \hat{x_{ij}}.  A one-letter script is
    ;; left to the rule below: `e^xhat' is `e^{\hat{x}}'.
    (:trigger "\\(${ACCENTED}\\|\\\\[A-Za-z]+\\|[[:alpha:]]+\\)\\(\\(?:[_^]\\(?:[0-9]+\\|{[^{}]*}\\)\\)+\\)\\(${ACCENT}\\)"
     :expand noteworthy-latex-scripted-accent :in (math) :max-length 80)
    ;; Postfix accents: xhat -> \hat{x}
    (:trigger "\\(${LETTER}\\)\\(${ACCENT}\\)"
     :expand noteworthy-latex-accent :in (math))
    ;; Styles: bbR -> \mathbb{R}
    (:trigger "\\(^\\|[^[:alpha:]_.\\\\]\\)\\(bb\\|cal\\|frak\\|bold\\)\\(${LETTER}\\)"
     :expand noteworthy-latex-style :in (math))
    ;; Differentials: `dx ' -> `\,\mathrm{d}x '.  The delimiter is part of
    ;; the trigger, so `det', `dim' and `delta' are out of its way.
    (:trigger "\\(^\\|${NOT_WORD}\\)\\(p?d\\)\\(${LETTER}\\)\\([ ,;/]\\)"
     :expand noteworthy-latex-differential :in (math))
    (:trigger "\\(^\\|${NOT_WORD}\\)\\(p?d\\)\\(${LETTER}\\))"
     :expand noteworthy-latex-bracketed-differential :in (math))
    ;; Glued to what it multiplies: `kdx;' -> `k\,\mathrm{d}x ', `kdx)'.
    (:trigger "\\([[:alnum:]]\\)\\(pd\\|d\\)\\(${LETTER}\\)\\([;)]\\)"
     :expand noteworthy-latex-glued-differential :in (math)))
  "Regex auto-expansions for LaTeX, in the format of
`noteworthy-snippets-regex-rules'."
  :type '(repeat plist)
  :group 'noteworthy-latex)

;;; The control-word guard

(defconst noteworthy-latex--glue-words
  '("ddot" "dot" "hat" "bar" "til" "vec" "arr" "und" "ovl" "er" "at")
  "Words that may follow an expansion's control word with nothing between.
The accents, the rest of `inter' and the `at' of `pmat;'.")

(defvar-local noteworthy-latex--pending nil
  "Non-nil when letters are held glued to an expansion's control word.")

(defun noteworthy-latex--control-word-before-p (pos)
  "Non-nil when a control word ends at POS."
  (save-excursion
    (goto-char pos)
    (let ((end (point)))
      (skip-chars-backward "A-Za-z")
      (and (< (point) end) (eq (char-before) ?\\)))))

(defun noteworthy-latex--glued-run ()
  "The letters glued straight after the last expansion's control word.
A cons (START . END), or nil when there are none."
  (when (and (markerp noteworthy-snippets--last-end)
             (eq (marker-buffer noteworthy-snippets--last-end) (current-buffer)))
    (let ((start (marker-position noteworthy-snippets--last-end)))
      (when (and start (<= start (point-max))
                 (noteworthy-latex--control-word-before-p start))
        (let ((end (save-excursion (goto-char start)
                                   (skip-chars-forward "A-Za-z") (point))))
          (and (> end start) (cons start end)))))))

(defun noteworthy-latex--separate (start)
  "Put the space between the control word ending at START and what follows."
  (save-excursion (goto-char start) (insert " "))
  (setq noteworthy-latex--pending nil))

(defun noteworthy-latex-guard-control-word ()
  "Space a letter typed straight after an expansion's control word.
Runs from `post-self-insert-hook', after the expansions.  A letter that
could be the start of a word the rules are waiting for is let through;
the next letter that proves it was not, a delimiter, or leaving the
insertion (see `noteworthy-latex--flush') makes the space."
  (when (and noteworthy-latex-mode (characterp last-command-event)
             (eq (char-before) last-command-event))
    (when-let* ((run (noteworthy-latex--glued-run)))
      (cond
       ;; The typed character extends the run.
       ((= (cdr run) (point))
        (let ((glued (buffer-substring-no-properties (car run) (cdr run))))
          (if (cl-some (lambda (word) (string-prefix-p glued word))
                       noteworthy-latex--glue-words)
              (setq noteworthy-latex--pending t)
            (noteworthy-latex--separate (car run)))))
       ;; A delimiter ends it.
       ((= (cdr run) (1- (point)))
        (noteworthy-latex--separate (car run)))))))

(defun noteworthy-latex--flush ()
  "Space the letters still held glued, when point has left them.
Runs from `post-command-hook', for what is no keystroke: RET, ESC, an
arrow key."
  (when noteworthy-latex--pending
    (unless (eq this-command 'self-insert-command)
      (setq noteworthy-latex--pending nil)
      (unless (and (symbolp this-command)
                   (string-match-p "undo\\|redo" (symbol-name this-command)))
        (when-let* ((run (noteworthy-latex--glued-run)))
          (noteworthy-latex--separate (car run)))))))

;;; Auto-backslash

(defconst noteworthy-latex--math-names
  (append
   '("sin" "cos" "tan" "cot" "sec" "csc" "sinh" "cosh" "tanh" "coth"
     "arcsin" "arccos" "arctan" "log" "ln" "lg" "exp"
     "lim" "liminf" "limsup" "max" "min" "sup" "inf"
     "det" "dim" "ker" "gcd" "arg" "deg")
   '("alpha" "beta" "gamma" "delta" "epsilon" "varepsilon" "zeta" "eta"
     "theta" "vartheta" "iota" "kappa" "lambda" "mu" "nu" "xi" "pi" "varpi"
     "rho" "varrho" "sigma" "varsigma" "tau" "upsilon" "phi" "varphi"
     "chi" "psi" "omega"
     "Gamma" "Delta" "Theta" "Lambda" "Xi" "Pi" "Sigma" "Upsilon" "Phi"
     "Psi" "Omega"))
  "Names `noteworthy-latex-maybe-backslash' puts a backslash before.")

(defun noteworthy-latex--split-names (word)
  "WORD as a list of known names, the fewest, or nil if it is not wholly names.
Of equal splits the one with the longer first name."
  (let* ((n (length word))
         (memo (make-vector (1+ n) 'unknown)))
    (cl-labels
        ((solve (i)
           (if (= i n)
               '()
             (let ((cached (aref memo i)))
               (if (not (eq cached 'unknown))
                   cached
                 (let (best)
                   (cl-loop for len from (- n i) downto 2
                            for piece = (substring word i (+ i len))
                            when (member piece noteworthy-latex--math-names)
                              do (let ((rest (solve (+ i len))))
                                   (when (or rest (= (+ i len) n))
                                     (let ((cand (cons piece rest)))
                                       (when (or (null best)
                                                 (< (length cand) (length best)))
                                         (setq best cand))))))
                   (aset memo i best)
                   best))))))
      (solve 0))))

(defun noteworthy-latex--math-word-before (end)
  "Bounds (START . END) of the word of letters ending at END, or nil.
Not glued to a letter before it, nor after `\\', `@', `#' or `.'."
  (save-excursion
    (goto-char end)
    (skip-chars-backward "A-Za-z")
    (let ((start (point)))
      (and (>= (- end start) 2)
           (not (memq (char-before start) '(?\\ ?@ ?# ?.)))
           (progn (goto-char end) (eq (noteworthy-latex-context) 'math))
           (cons start end)))))

(defun noteworthy-latex--consider-word (end)
  "Put backslashes in the math word ending at END if it is wholly names."
  (when-let* ((bounds (noteworthy-latex--math-word-before end))
              (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (pieces (noteworthy-latex--split-names word)))
    ;; Whatever the last expansion left is not this word's business now.
    (when (and (markerp noteworthy-snippets--last-end)
               (eq (marker-buffer noteworthy-snippets--last-end) (current-buffer))
               (<= (car bounds) (marker-position noteworthy-snippets--last-end) (cdr bounds)))
      (set-marker noteworthy-snippets--last-end nil))
    (noteworthy-snippets--as-undo-step
     (lambda ()
       (save-excursion
         (let ((pos (car bounds)) cuts)
           (dolist (piece pieces)
             (push pos cuts)
             (setq pos (+ pos (length piece))))
           ;; Right to left, so an insertion never moves the ones to come.
           (dolist (cut cuts)
             (goto-char cut)
             (insert "\\"))))))))

(defun noteworthy-latex-maybe-backslash ()
  "Give the math word just finished its backslashes, if it is wholly names.
Finished by anything that is no letter or digit, `@' excepted, which
begins a greek letter.  Runs from `post-self-insert-hook', after the
expansions and the guard: a word a rule has taken is no word any more."
  (when (and noteworthy-latex-auto-backslash
             noteworthy-latex-mode
             (characterp last-command-event)
             (eq (char-before) last-command-event)
             (not (string-match-p "[[:alnum:]@]" (string last-command-event))))
    (condition-case err
        (noteworthy-latex--consider-word (1- (point)))
      (error (message "Noteworthy: adding backslashes failed: %s"
                      (error-message-string err))))))

;;; The mode

(defvar noteworthy-latex-mode-map (make-sparse-keymap)
  "Keymap of `noteworthy-latex-mode'.")

(defun noteworthy-latex-tab ()
  "Expand the snippet key before point, else the next field, else indent.
Evil binds TAB in its insert state map, which shadows
`yas-minor-mode-map', so expansion is dispatched here."
  (interactive)
  (cond
   ((noteworthy-typst--expand-snippet-maybe))
   ((noteworthy-typst--snippet-active-p) (yas-next-field))
   (t (call-interactively #'indent-for-tab-command))))

(with-eval-after-load 'evil
  (evil-define-key* 'insert noteworthy-latex-mode-map
                    (kbd "TAB") #'noteworthy-latex-tab
                    (kbd "<tab>") #'noteworthy-latex-tab))

(defun noteworthy-latex--enable ()
  "Wire the typing layer into this buffer."
  (setq-local noteworthy-snippets-context-function #'noteworthy-latex-context
              noteworthy-snippets-buffer-rules noteworthy-latex-regex-rules
              noteworthy-snippets-buffer-variables noteworthy-latex-regex-variables
              ;; `\alpha' is a word, and so is `\\'.
              noteworthy-snippets--identifier-rx "[[:alnum:]\\\\]"
              ;; `pm' + `at;' is no `mat;'.
              noteworthy-snippets-protect-expansion t
              ;; Typst's splitting of run-together words.
              noteworthy-snippets-auto-space nil)
  (noteworthy-snippets-setup-auto)
  (add-hook 'post-self-insert-hook #'noteworthy-latex-guard-control-word 91 t)
  (add-hook 'post-self-insert-hook #'noteworthy-latex-maybe-backslash 92 t)
  (add-hook 'post-command-hook #'noteworthy-latex--flush nil t))

(defun noteworthy-latex--disable ()
  "Take the typing layer out of this buffer again."
  (dolist (var '(noteworthy-snippets-context-function
                 noteworthy-snippets-buffer-rules
                 noteworthy-snippets-buffer-variables
                 noteworthy-snippets--identifier-rx
                 noteworthy-snippets-protect-expansion
                 noteworthy-snippets-auto-space))
    (kill-local-variable var))
  (remove-hook 'post-self-insert-hook #'noteworthy-snippets-maybe-auto-expand t)
  (remove-hook 'post-self-insert-hook #'noteworthy-latex-maybe-backslash t)
  (remove-hook 'post-self-insert-hook #'noteworthy-latex-guard-control-word t)
  (remove-hook 'post-command-hook #'noteworthy-latex--flush t))

;;;###autoload
(define-minor-mode noteworthy-latex-mode
  "The Noteworthy typing layer for AUCTeX's `LaTeX-mode'.
Snippets and rules expand as they are typed, in math only unless the
snippet says otherwise; see the commentary in `noteworthy-latex.el'."
  :init-value nil
  :lighter " NW"
  :keymap noteworthy-latex-mode-map
  (if noteworthy-latex-mode
      (noteworthy-latex--enable)
    (noteworthy-latex--disable)))

;;;###autoload
(defun noteworthy-latex-setup ()
  "Turn on `noteworthy-latex-mode'.  Meant for `LaTeX-mode-hook'."
  (noteworthy-latex-mode 1))

(provide 'noteworthy-latex)

;;; noteworthy-latex.el ends here
