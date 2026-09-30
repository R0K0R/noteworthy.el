;;; noteworthy.el --- Noteworthy workflow for Typst  -*- lexical-binding: t; -*-

;; Author: r0k0r
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (typst-ts-mode "0.1") (typst-preview "0.1") (dtrt-indent) (treemacs "2.9") (pdf-tools "1.0") (evil) (vterm))
;; Keywords: typst, languages, tools
;; URL: https://github.com/R0K0R/noteworthy.el

;;; Commentary:
;; Emacs editing environment for the Noteworthy academic framework for Typst.
;; See: https://github.com/sihooleebd/noteworthy

;;; Code:

(defgroup noteworthy nil
  "Noteworthy workflow configuration."
  :group 'languages
  :prefix "noteworthy-")

(defcustom noteworthy-terminal-shell nil
  "Shell executable to use for the Noteworthy terminal.
If nil, defaults to bash or $SHELL."
  :type '(choice (const :tag "Default" nil)
                 (file :tag "Shell path")))

(defcustom noteworthy-pdf-width nil
  "Width of the PDF window in columns.
If nil, defaults to 35% of the frame width."
  :type '(choice (const :tag "Dynamic (35%)" nil) integer))

(require 'noteworthy-typst)
(require 'noteworthy-preview)
(require 'noteworthy-layout)
(require 'noteworthy-snippets)
(require 'noteworthy-image)
;; Installs its fix on load; see its Commentary for the paint bug it cures.
(require 'noteworthy-webkit)

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

(provide 'noteworthy)

;;; noteworthy.el ends here
