;;; noteworthy.el --- Noteworthy workflow for Typst  -*- lexical-binding: t; -*-

;; Author: r0k0r
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (typst-ts-mode "0.1") (typst-preview "0.1") (treemacs "2.9") (pdf-tools "1.0"))
;; Keywords: typst, languages, tools
;; URL: https://github.com/R0K0R/noteworthy.el

;;; Commentary:
;; A specialized workflow for technical writing in Typst.

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

(require 'noteworthy-typst)
(require 'noteworthy-preview)
(require 'noteworthy-layout)

(with-eval-after-load 'evil
  (require 'noteworthy-evil))

(defun noteworthy-typst-get-dynamic-inputs (root)
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
