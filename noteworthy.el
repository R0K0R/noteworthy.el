;;; noteworthy.el --- Noteworthy workflow for Typst  -*- lexical-binding: t; -*-

;; Author: r0k0r
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (typst-ts-mode "0.1") (dtrt-indent))
;; Keywords: typst, languages, tools
;; URL: https://github.com/R0K0R/noteworthy.el

;;; Commentary:
;; Emacs editing environment for the Noteworthy academic framework for Typst:
;; the typing layer (smart pairs, newline, structure editing), snippets,
;; the same layer for LaTeX, Evil bindings and clipboard image paste.
;; See: https://github.com/sihooleebd/noteworthy
;;
;; Opening a project -- the window layout with tree, terminal, preview and
;; PDF, adding pages, quitting, and working on a Syncworthy server -- is
;; `noteworthy-workspace', a separate package built on emacs-collab.  The
;; names `noteworthy-init', `noteworthy-quit', `noteworthy-new-page' and
;; the like are defined there; without it they only say so (see the end of
;; this file).

;;; Code:

(defgroup noteworthy nil
  "Noteworthy workflow configuration."
  :group 'languages
  :prefix "noteworthy-")

(defvar noteworthy-project-root nil
  "Root of the Noteworthy project being edited, or nil.
Set by the workspace (`noteworthy-workspace') before it opens the first page;
the Typst mode hook below roots typst-preview there.")

(defvar noteworthy-master-file nil
  "The file the project's preview compiles (templates/core/parser.typ), or nil.")

(defvar noteworthy-pdf-buffer nil
  "The reference PDF buffer of the open workspace, or nil.
What `noteworthy-pdf-scroll' (M-H/J/K/L) scrolls.  Set by the workspace.")

(require 'noteworthy-typst)
(require 'noteworthy-snippets)
(require 'noteworthy-latex)
(require 'noteworthy-image)

(with-eval-after-load 'evil
  (require 'noteworthy-evil))

(defvar noteworthy--dynamic-inputs-cache (make-hash-table :test #'equal)
  "ROOT -> (STAMP . INPUTS) for `noteworthy-typst-get-dynamic-inputs'.")

(defun noteworthy--dynamic-inputs-stamp (root)
  "Modification times the inputs of ROOT depend on.
noteworthy.py, the files in config/, and content/ and its chapter
directories, whose times change as pages come and go."
  (let ((content (expand-file-name "content" root))
        (config (expand-file-name "config" root)))
    (mapcar (lambda (f) (file-attribute-modification-time (file-attributes f)))
            (append (list (expand-file-name "noteworthy.py" root) content)
                    (and (file-directory-p config) (directory-files config t "\\`[^.]"))
                    (and (file-directory-p content) (directory-files content t "\\`[0-9]"))))))

(defun noteworthy-typst-get-dynamic-inputs (root)
  "The typst inputs noteworthy.py --print-inputs gives for ROOT, cached.
Running it takes two Python start-ups, ~0.4 s, and it ran for every Typst
buffer opened -- twice in a launch.  The result is kept until one of the
files it depends on changes (`noteworthy--dynamic-inputs-stamp')."
  (let ((stamp (noteworthy--dynamic-inputs-stamp root))
        (hit (gethash root noteworthy--dynamic-inputs-cache)))
    (if (and hit (equal (car hit) stamp))
        (cdr hit)
      (let ((inputs (noteworthy--run-dynamic-inputs root)))
        (puthash root (cons stamp inputs) noteworthy--dynamic-inputs-cache)
        inputs))))

(defun noteworthy--run-dynamic-inputs (root)
  "Run noteworthy.py --print-inputs in ROOT using a temp wrapper script."
  (let ((script (expand-file-name "noteworthy.py" root)))
    (if (file-exists-p script)
        (let* ((default-directory root)
               (wrapper-file (make-temp-file "noteworthy-wrapper-" nil ".py"))
               (wrapper-code (concat
                              "import subprocess, shlex, sys, os\n"
                              "script = \"" (file-name-nondirectory script) "\"\n"
                              "try:\n"
                              "    if os.path.exists(script):\n"
                              "        out = subprocess.check_output(['python3', script, '--print-inputs'], text=True, stderr=subprocess.DEVNULL).strip()\n"
                              "        if out:\n"
                              "            for arg in shlex.split(out):\n"
                              "                print(arg)\n"
                              "except Exception as e:\n"
                              "    pass\n"))
               (output (progn
                         (with-temp-file wrapper-file (insert wrapper-code))
                         (shell-command-to-string (format "python3 %s" wrapper-file)))))
          (delete-file wrapper-file)
          (if (not (string-empty-p output))
              (split-string output "\n" t)
            nil))
      nil)))

(add-hook 'typst-ts-mode-hook
          (lambda ()
            (when (bound-and-true-p noteworthy-project-root)
              (setq-local typst-preview-default-dir noteworthy-project-root)
              (let ((dynamic-args (noteworthy-typst-get-dynamic-inputs noteworthy-project-root)))
                (when dynamic-args
                  (setq-local typst-preview-cmd-options
                              (append (default-value 'typst-preview-cmd-options)
                                      dynamic-args)))))
            (when (bound-and-true-p noteworthy-master-file)
              (setq-local typst-preview--master-file noteworthy-master-file))))

(add-hook 'typst-ts-mode-hook #'noteworthy-typst-mode)

;;; The workspace is another package

;; Commands that moved to `noteworthy-workspace'.  When it is installed it
;; is loaded on first use and defines them for real; until then each says
;; what is missing.  `fboundp' keeps these from replacing the real ones if
;; the workspace was loaded first.
(defun noteworthy--workspace-call (command args)
  "Load `noteworthy-workspace' and run COMMAND with ARGS, or say it is missing."
  (let ((stub (symbol-function command)))
    (if (and (require 'noteworthy-workspace nil t)
             (not (eq (symbol-function command) stub)))
        (if args (apply command args) (call-interactively command))
      (user-error "`%s' is part of noteworthy-workspace, which is not installed"
                  command))))

(dolist (command '(noteworthy-init noteworthy-quit noteworthy-new-page
                   noteworthy-refresh-preview noteworthy-typst-send-position
                   noteworthy-toggle-log noteworthy-preview-start))
  (unless (fboundp command)
    (let ((command command))
      (defalias command
        (lambda (&rest args)
          (interactive)
          (noteworthy--workspace-call command args))
        "Defined by `noteworthy-workspace'; this stub loads it, or says it is missing."))))

(provide 'noteworthy)

;;; noteworthy.el ends here
