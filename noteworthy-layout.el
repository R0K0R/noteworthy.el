;;; noteworthy-layout.el --- Workspace layout for Noteworthy  -*- lexical-binding: t; -*-

;;; Code:

(require 'treemacs)
(require 'pdf-tools)
(require 'noteworthy-preview)

(declare-function pdf-view-page-size "pdf-view" ())
(declare-function pdf-view-fit-height-to-window "pdf-view" ())
(declare-function pdf-view-fit-width-to-window "pdf-view" ())

(defvar noteworthy-master-file nil
  "The master file for the current noteworthy project.")

(defvar noteworthy-project-root nil
  "The root directory of the current noteworthy project.")

(defun noteworthy-init (&optional project-dir pdf-path-arg)
  "Initialize Noteworthy workspace with PROJECT-DIR and optional PDF-PATH-ARG.
Sets up treemacs, editor, terminal, preview, and PDF windows."
  (interactive
   (let ((dir (read-directory-name "Noteworthy project: ")))
     (list dir
           (read-file-name "Secondary PDF (optional): " dir nil nil))))

  (let* ((raw-dir (or project-dir
                      (read-directory-name "Noteworthy project: ")))
         (dir (file-truename raw-dir))
         (pdf-file (cond (pdf-path-arg pdf-path-arg)
                         ((and (stringp pdf-path-arg) (string-empty-p pdf-path-arg)) nil)
                         (t nil))))

    (delete-other-windows)
    (setq noteworthy-project-root dir)

    (let ((parser-path (expand-file-name "templates/parser.typ" dir)))
      (if (file-exists-p parser-path)
          (progn
            (find-file parser-path)
            (setq noteworthy-master-file parser-path))
        (let ((typ-files (directory-files-recursively dir "\\.typ$")))
          (if typ-files
              (progn
                (find-file (car typ-files))
                (setq noteworthy-master-file (car typ-files)))
            (find-file (expand-file-name "main.typ" dir))
            (setq noteworthy-master-file (expand-file-name "main.typ" dir))))))

    (let ((editor-window (selected-window)))
      (set-window-parameter editor-window 'noteworthy-editor t)

      (treemacs)
      (let ((default-directory dir))
        (treemacs-add-and-display-current-project-exclusively))
      (select-window editor-window)

      (when (noteworthy-xwidget-available-p)
        (let* ((preview-width (floor (* 0.25 (frame-width))))
               (current-width (window-total-width editor-window))
               (preview-window (split-window editor-window (- current-width preview-width) 'right)))
          (set-window-parameter preview-window 'noteworthy-preview t)
          (select-window preview-window)
          (when (fboundp 'typst-preview-mode)
            (typst-preview-mode 1))))

      (select-window editor-window)
      (split-window-below (floor (* 0.75 (window-height))))
      (other-window 1)
      (let ((default-directory dir)
            (shell (if (and (boundp 'noteworthy-terminal-shell) noteworthy-terminal-shell)
                       noteworthy-terminal-shell
                     (or (executable-find "bash") (getenv "SHELL")))))
        (ansi-term shell))

      (select-window editor-window)
      (when (and pdf-file
                 (stringp pdf-file)
                 (not (string-empty-p pdf-file))
                 (not (file-directory-p pdf-file))
                 (file-exists-p pdf-file))
        (let* ((pdf-target-width (floor (* 0.3 (frame-width))))
               (pdf-window (split-window-right (- (window-total-width) pdf-target-width))))
          (select-window pdf-window)
          (find-file pdf-file)
          (when (bound-and-true-p pdf-view-mode)
            (run-with-timer 0.1 nil
                            (lambda (win)
                              (when (window-live-p win)
                                (with-selected-window win
                                  (let* ((page-size (pdf-view-page-size))
                                         (page-w (car page-size))
                                         (page-h (cdr page-size))
                                         (page-aspect (/ (float page-w) page-h))
                                         (win-w (window-pixel-width))
                                         (win-h (window-pixel-height))
                                         (win-aspect (/ (float win-w) win-h)))
                                    (if (< page-aspect win-aspect)
                                        (progn
                                          (let ((target-w (floor (* win-h page-aspect))))
                                            (window-resize win (- target-w win-w) t t)
                                            (pdf-view-fit-height-to-window)))
                                      (pdf-view-fit-width-to-window))))))
                            pdf-window))))

      (select-window editor-window)
      (message "Noteworthy initialized: %s" dir))))

(provide 'noteworthy-layout)

;;; noteworthy-layout.el ends here
