# noteworthy.el

An Emacs editing environment for the [Noteworthy](https://github.com/sihooleebd/noteworthy) academic framework for [Typst](https://typst.app/).

This package provides smart editing features, automated workspace layout, and seamless preview integration tailored for developing Noteworthy projects — a powerful Typst framework for creating educational textbooks, lecture notes, and technical documentation.

## Features

- **Smart Editing** (`noteworthy-typst-mode`)
  - Context-aware pair expansion (`*`, `_`, `$`, backticks)
  - Smart newline with list continuation and bracket expansion
  - Intelligent backspace that deletes matching pairs

- **Workspace Layout** (`noteworthy-init`)
  - Treemacs file explorer (left)
  - Main editor (center-top, 75%)
  - Terminal (center-bottom, 25%)
  - Live preview via xwidget (right) — falls back to external browser if unavailable
  - Optional PDF reference window

- **Preview Integration**
  - Automatic xwidget detection
  - Jump-to-source from preview (`M-o`)
  - Remote PDF scrolling (`M-H/J/K/L`)

- **Evil Support** (Optional)
  - Vim-style `o`/`O` with list continuation
  - All smart editing features work in insert mode

## Requirements

- Emacs 29.1+ (with Tree-sitter support)
- [typst-ts-mode](https://codeberg.org/meow_king/typst-ts-mode)
- [typst-preview](https://github.com/havarddj/typst-preview.el)
- [treemacs](https://github.com/Alexander-Miller/treemacs)
- [pdf-tools](https://github.com/vedang/pdf-tools)
- [tinymist](https://github.com/Myriad-Dreamin/tinymist) (LSP server for Typst)

## Installation

### Using straight.el

```elisp
(straight-use-package
 '(noteworthy :type git :host github :repo "YOUR_USERNAME/noteworthy"))

(require 'noteworthy)
```

### Using use-package with straight

```elisp
(use-package noteworthy
  :straight (:type git :host github :repo "YOUR_USERNAME/noteworthy")
  :config
  (setq noteworthy-terminal-shell "/usr/bin/fish")) ;; Optional
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/R0K0R/noteworthy.git ~/.emacs.d/site-lisp/noteworthy
   ```

2. Add to your `init.el`:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/site-lisp/noteworthy")
   (require 'noteworthy)
   ```

## Usage

### Start a Noteworthy Session

```elisp
M-x noteworthy-init
```

Or from command line:
```bash
emacs --noteworthy-path /path/to/project --pdf-path /path/to/reference.pdf
```

### Configuration

```elisp
;; Custom shell for the terminal pane (default: bash)
(setq noteworthy-terminal-shell "/usr/bin/fish")
```

## Keybindings

### Insert Mode (Typst files)

| Key | Action |
|-----|--------|
| `SPC` | Smart space — `$$` → `$ \| $` |
| `RET` | Smart newline — expands `(\|)` and `$\|$`, clears an empty list marker |
| `*` | Smart pair — inserts `**` in markup |
| `_` | Smart pair — inserts `__` in markup |
| `$` | Smart dollar — pairs only where it opens a formula, else closes one |
| `` ` `` | Smart backtick — 1→2→3 expansion |
| `DEL` | Smart backspace — deletes pairs, quotes included |
| `TAB` | Expand snippet, else next field, else indent list item |
| `S-TAB` | Previous snippet field, else dedent list item |
| `)` `]` `}` | Jump past the enclosing group — `( as\|df )` → `( asdf )\|` |
| `C-)` `C-]` `C-}` | Insert a literal closer (so does `C-q`) |

### Structure Editing (org-style)

| Key | Action |
|-----|--------|
| `M-RET` | New list item or heading at the same level |
| `M-h` / `M-l` | Promote / demote — `=` count on headings, indent on items |
| `M-k` / `M-j` | Move the line up / down |
| `C-c C-n` / `C-c C-p` | Next / previous heading |

### Normal Mode (with Evil)

| Key | Action |
|-----|--------|
| `o` | Smart open below — continues lists |
| `O` | Smart open above — continues lists |
| `M-o` | Jump to preview position |
| `ci$` `ca$` `di$` `ya$` … | `$...$` as a text object, inline or block |
| `M-H/J/K/L` | Scroll PDF remotely |

## File Structure

```
noteworthy/
├── noteworthy.el          # Main entry point
├── noteworthy-typst.el    # Smart editing logic
├── noteworthy-layout.el   # Workspace management
├── noteworthy-preview.el  # Preview abstraction
├── noteworthy-evil.el     # Evil integration (optional)
├── noteworthy-snippets.el # yasnippet integration
├── snippets/              # Typst snippets, by context
└── README.md
```

## Snippets

Snippets are context-aware: each one declares where it applies, so the same
key can mean different things — or nothing — depending on where point sits.
`def` expands to a `#definition` block in prose but is inert inside `$...$`;
`mat` expands only inside math.

The context comes from `noteworthy-typst-context`, which walks the
tree-sitter AST and falls back to reading the line when the parse is
incomplete (which, while typing, is most of the time). It returns one of
`markup`, `math`, `raw`, `string`, `comment` or `code` — the same function
that gates `*`/`_`/`"` pairing, so snippets and pairing can never disagree.

### Blocks

Every block has two keys. The plain one gives the common case — a title and
a body, `thm` for `#theorem("Title")[...]`. Capitalising the last letter
gives the same block with every field: `thM` adds the number and the label,
so it can be numbered by hand and referenced from anywhere with `@label`.

| plain | full | block |
|-------|------|-------|
| `thm`  | `thM`  | theorem |
| `def`  | `deF`  | definition |
| `note` | `notE` | note |
| `exa`  | `exA`  | example |
| `ana`  | `anA`  | analysis |
| `notn` | `notN` | notation |
| `eqn`  | `eqN`  | equation |
| `sol`  | `soL`  | solution |
| `prf`  | `prF`  | proof |

The number defaults to `auto`, which means "keep counting" — leave it and
the block numbers itself. A proof takes no number, since it never prints
one, so `prF` fills in the title and label only.

### Writing your own

Drop a file in `snippets/typst-ts-mode/` with a `# condition:` header:

```
# -*- mode: snippet -*-
# name: vector
# key: vec
# condition: (noteworthy-typst-in 'math)
# --
vec(${1:a}, ${2:b})$0
```

| Condition | LaTeX Suite option | Behaviour |
|-----------|--------------------|-----------|
| `(noteworthy-typst-in 'markup)` | `t` | Expands on `TAB`, in prose only |
| `(noteworthy-typst-in 'math)` | `m` | Expands on `TAB`, in math only |
| `(noteworthy-typst-in 'math 'code)` | — | Either |
| `(noteworthy-typst-auto 'math)` | `mA` | Expands **as it is typed**, even mid-word |
| `(noteworthy-typst-auto-word 'math)` | `mAw` | As typed, but only at a word boundary |
| *(omitted)* | — | Expands anywhere, on `TAB` |

An auto snippet still expands on `TAB` too. Set
`noteworthy-snippets-auto-expand` to nil to turn auto-expansion off without
touching the snippets.

`auto` fires glued to whatever precedes it, so a key `sr` turns `xsr` into
`x^2`. That is what you want for a postfix operator, and what you must not
use for a key spelled like the start of a longer word — `sin` would fire
inside `arcsin`. Use `auto-word` for those.

Also note that `SPC`, `*`, `_`, `$`, `` ` `` and `"` insert directly rather
than through `self-insert-command`, so a key ending in one of them never
reaches the auto-expansion hook.

## Regex Triggers

Patterns live in `noteworthy-snippets-regex-rules` rather than in snippet
files, because a yasnippet key is a literal string looked up in a hash. A
rule is matched against the text ending at point, its captures are
substituted into a template, and the result is handed to yasnippet — so
tabstops still work in the expansion.

```elisp
(setq noteworthy-snippets-regex-rules
      '((:trigger "\\(${LETTER}\\)\\(${DIGIT}\\)" :expand "\\1_\\2" :in (math))
        (:trigger "@\\([A-Za-z]\\)" :expand noteworthy-snippets-greek :in (math))))
```

| Key | Meaning |
|-----|---------|
| `:trigger` | Regexp matched at point. `${NAME}` comes from `noteworthy-snippets-regex-variables` |
| `:expand` | Template with `\1`..`\9` for captures; or a function of the capture list returning one, or nil to decline |
| `:in` | Contexts it applies to |

Rules are tried in order, after the keyed snippets — a literal key is the
more specific statement, so it wins over a pattern that would also match.

The shipped rules give you:

| Type | Get |
|------|-----|
| `x1`, `alpha1` | `x_1`, `alpha_1` — anchored at point, so the greedy match takes one letter |
| `x_12` | `x_(12)` |
| `@a`, `@W` | `alpha`, `Omega` |
| `xhat` `xbar` `xdot` `xddot` `xtil` `xvec` | `hat(x)` `bar(x)` `dot(x)` `dot.double(x)` `tilde(x)` `arrow(x)` |
| `bbR` `calL` `boldv` | `bb(R)` `cal(L)` `bold(v)` |

`'` is deliberately *not* paired: in prose an apostrophe is punctuation, and
Typst has no single-quoted string. That is a smartparens rule, disabled for
`typst-ts-mode`; nothing here binds `'`.

Plus the keyed ones: `int` → `integral` and `prod` → `product` (the two short
names Typst does not have), `sr` `cb` `inv` for powers, `sum;` `int;` `prod;`
`lim;` for big operators with bounds, `mat;` `bmat;` `cases;` `vec;` for
templates, and `pdv;` `dv;` `pdvn;` `dvn;` for derivatives.

Because `int` expands as you type it, you type `int` and never `integral` —
typing the full word gives `integralegral`. `C-/` undoes an expansion.

### Tabstops

A snippet drops point at its first field, `TAB` moves to the next, `S-TAB`
back. Repeat a field number to mirror it — `pdvn;` uses that so the order is
typed once and appears in both places:

```
pdvn;   ->  (partial^n f)/(partial x^n)
             typing f TAB x TAB 3
            (partial^3 f)/(partial x^3)
```

Every placeholder is a field, so nothing is fixed to the defaults shown.

Postfix accents are the biggest thing worth taking from LaTeX Suite: Typst
has no postfix form, so the accent must be typed *before* its argument, and
this puts it back in the order you think of it. Note `xvec` gives
`arrow(x)`, not `vec(x)` — in Typst `vec()` is a column vector.

### Porting from Obsidian LaTeX Suite

Most of the model carries over: modes become contexts, `A` becomes `auto`,
`w` becomes `auto-word`, `r` becomes a regex rule, and tabstops are
yasnippet fields already.

What does **not** carry over is most of the *content*, because Typst math
is not LaTeX. These all need no snippet at all:

| LaTeX Suite | Typst | Verdict |
|-------------|-------|---------|
| `//` → `\frac{}{}` | `a/b` is native | skip |
| `->` `=>` `!=` `>=` `<=` `<->` `\|->` `~~` | all native shorthands | skip |
| `\sum` `\int` `\alpha` `\times` | spelled `sum`, `integral`, `alpha`, `times` | skip |
| `\text{...}` | `"..."`, and `"` already pairs in math | skip |
| `\left( \right)` | delimiters auto-scale, plus `lr()` | skip |
| `mk` inline math | `$` already pairs | skip |
| `\hat{x}` `\dot{x}` `\vec{x}` | `hat(x)` — still prefix, still needs the parens | **port** |
| `\mathbb{R}` `\mathcal{L}` | `bb(R)` — same | **port** |
| `sr` `cb` `inv` postfix powers | no equivalent | **port** |
| `x1` → `x_1` | no equivalent | **port** |
| `@a` → greek | `alpha` is only 5 keys, but 2 is fewer | **port** |
| `\pdv` `\dv` derivatives | `(partial f)/(partial x)` is verbose | **port** |
| matrix / cases templates | `mat()` `cases()` still want fields | **port** |

Roughly half of a LaTeX Suite config is dead weight here, because Typst
already spells the operators the way you would type them. What survives is
the part that *wraps* an argument — Typst is prefix-and-parenthesised
everywhere, and that is exactly what a postfix trigger fixes.

## Contributing## Contributing

Contributions are welcome! Please open an issue or pull request.

## License

GPL-3.0-or-later

## Related

- [Noteworthy](https://github.com/sihooleebd/noteworthy) — The Typst framework this editor is built for
- [typst-ts-mode](https://codeberg.org/meow_king/typst-ts-mode) — Tree-sitter mode for Typst
- [typst-preview.el](https://github.com/havarddj/typst-preview.el) — Live preview for Typst
