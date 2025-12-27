# Noteworthy

A specialized Emacs workflow for technical writing in [Typst](https://typst.app/).

Noteworthy provides smart editing features, automated workspace layout, and seamless preview integration for a distraction-free writing experience.

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
   git clone https://github.com/YOUR_USERNAME/noteworthy.git ~/.emacs.d/site-lisp/noteworthy
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
| `RET` | Smart newline — indent in brackets, continue lists |
| `*` | Smart pair — inserts `**` in markup |
| `_` | Smart pair — inserts `__` in markup |
| `$` | Smart dollar — math mode pairing |
| `` ` `` | Smart backtick — 1→2→3 expansion |
| `DEL` | Smart backspace — deletes pairs |

### Normal Mode (with Evil)

| Key | Action |
|-----|--------|
| `o` | Smart open below — continues lists |
| `O` | Smart open above — continues lists |
| `M-o` | Jump to preview position |
| `M-H/J/K/L` | Scroll PDF remotely |

## File Structure

```
noteworthy/
├── noteworthy.el          # Main entry point
├── noteworthy-typst.el    # Smart editing logic
├── noteworthy-layout.el   # Workspace management
├── noteworthy-preview.el  # Preview abstraction
├── noteworthy-evil.el     # Evil integration (optional)
└── README.md
```

## Contributing

Contributions are welcome! Please open an issue or pull request.

## License

GPL-3.0-or-later

## Acknowledgments

- [typst-ts-mode](https://codeberg.org/meow_king/typst-ts-mode) by meow_king
- [typst-preview.el](https://github.com/havarddj/typst-preview.el) by havarddj
