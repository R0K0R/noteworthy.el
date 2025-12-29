;;; noteworthy-layout.el --- Workspace layout for Noteworthy  -*- lexical-binding: t; -*-

(require 'treemacs)
(require 'pdf-tools)
(require 'noteworthy-preview)
(require 'vterm)



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
    
    ;; Set global project variables
    (setq-default noteworthy-project-root dir)
    (setq noteworthy-project-root dir)

    ;; Load project-specific settings (if any), overriding defaults/globals
    (noteworthy-load-settings dir)

    ;; Find master file
    (let ((master-path
           (let ((parser-path (expand-file-name "templates/parser.typ" dir)))
             (if (file-exists-p parser-path)
                 parser-path
               (let ((typ-files (directory-files-recursively dir "\\.typ$")))
                 (if typ-files
                     (car typ-files)
                   (expand-file-name "main.typ" dir)))))))
      
      (setq-default noteworthy-master-file master-path)
      (setq noteworthy-master-file master-path)
      (find-file master-path))

    (let ((editor-window (selected-window)))
      (set-window-parameter editor-window 'noteworthy-editor t)

      ;; 1. Setup Treemacs (Left)
      (treemacs)
      (let ((default-directory dir))
        (treemacs-add-and-display-current-project-exclusively))
      
      ;; 2. Setup Terminal (Bottom of Editor)
      ;; 2. Setup Terminal (Bottom of Editor)
      (select-window editor-window)
      (let ((term-window (split-window-below (floor (* 0.75 (window-height))))))
        (select-window term-window)
        (let ((default-directory dir)
              (shell-cmd (if (and (boundp 'noteworthy-terminal-shell) noteworthy-terminal-shell)
                             noteworthy-terminal-shell
                           (or (executable-find "bash") (getenv "SHELL")))))
          ;; Configure vterm to run our specific command
          (let ((cmd (if (listp shell-cmd)
                         (mapconcat #'identity shell-cmd " ")
                       shell-cmd))
                (old-shell (if (boundp 'vterm-shell) vterm-shell nil)))
            (setq vterm-shell cmd)
            (unwind-protect
                (let ((display-buffer-alist nil))
                  ;; Force vterm to usage current window
                  (vterm))
              (when old-shell (setq vterm-shell old-shell))))))

      (select-window editor-window)
      
      ;; 3. Start Typst Preview (First)
      (when (and (noteworthy-xwidget-available-p)
                 (fboundp 'typst-preview-start))
        (let ((buf (current-buffer)))
          (run-with-timer 0.1 nil
                          (lambda ()
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (setq-local noteworthy-project-root dir)
                                (setq-local noteworthy-master-file noteworthy-master-file)
                                (typst-preview-start t)))))))

      (select-window editor-window)

      ;; 4. Setup PDF Window (Right - Delayed to allow Preview to claim space)
      (when (and pdf-file
                 (stringp pdf-file)
                 (not (string-empty-p pdf-file))
                 (not (file-directory-p pdf-file)))
        ;; Ensure pdf-tools is ready
        (unless (bound-and-true-p pdf-view-mode)
          (if (fboundp 'pdf-tools-install)
              (pdf-tools-install)
            (message "Noteworthy: pdf-tools not found!")))
        
        (run-with-timer 0.6 nil
                        (lambda (ed-win pdf-f)
                          (when (window-live-p ed-win)
                            (select-window ed-win)
                            (let* ((pdf-window (split-window ed-win nil 'right))
                                   (target-width (or noteworthy-pdf-width (round (* 0.35 (frame-width)))))
                                   (current-width (window-total-width pdf-window))
                                   (delta (- target-width current-width)))
                              (set-window-parameter pdf-window 'noteworthy-pdf t)
                              (when (/= delta 0)
                                (ignore-errors (window-resize pdf-window delta t)))
                              (select-window pdf-window)
                              (find-file pdf-f)
                              ;; Only fit zoom to width, do not resize window
                              (when (bound-and-true-p pdf-view-mode)
                                (run-with-timer 0.1 nil
                                                (lambda (win)
                                                  (when (window-live-p win)
                                                    (with-selected-window win
                                                      (pdf-view-fit-width-to-window))))
                                                pdf-window)))))
                        editor-window pdf-file))

      (message "Noteworthy initialized: %s" dir))))

(defvar noteworthy-preview-width nil
  "Width of the preview window in columns.
If nil, defaults to 35% of the frame width.")

(defvar noteworthy-settings-file (expand-file-name "noteworthy-settings.el" user-emacs-directory)
  "File to store Noteworthy window layout settings.")

(defun noteworthy-track-window-sizes (&optional frame)
  "Track size changes for Noteworthy windows and update variables."
  (when (and (boundp 'noteworthy-project-root) noteworthy-project-root)
    (let ((wins (window-list frame)))
      (dolist (w wins)
        (cond
         ((window-parameter w 'noteworthy-pdf)
          (setq noteworthy-pdf-width (window-total-width w)))
         ((window-parameter w 'noteworthy-preview)
          (setq noteworthy-preview-width (window-total-width w))))))))

(add-hook 'window-size-change-functions #'noteworthy-track-window-sizes)

(defun noteworthy-load-settings (&optional root)
  "Load settings from project root or fallback to global `noteworthy-settings.el`."
  (let* ((root-file (and root (expand-file-name ".noteworthy-layout" root)))
         (file (if (and root-file (file-exists-p root-file))
                   root-file
                 noteworthy-settings-file)))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (condition-case nil
            (let ((settings (read (current-buffer))))
              (when (plist-get settings :pdf-width)
                (setq noteworthy-pdf-width (plist-get settings :pdf-width)))
              (when (plist-get settings :preview-width)
                (setq noteworthy-preview-width (plist-get settings :preview-width)))
              (message "Noteworthy: Loaded layout from %s" file))
          (error (message "Error loading noteworthy settings from %s" file)))))))

(defun noteworthy-save-config ()
  "Save Noteworthy configuration variables.
Saves to BOTH `noteworthy-settings.el` (global fallback)
AND `.noteworthy-layout` in the project root (if active)."
  (let ((settings (list :preview-width (or noteworthy-preview-width
                                           (round (* 0.35 (frame-width))))
                        :pdf-width (or noteworthy-pdf-width
                                       (round (* 0.35 (frame-width)))))))
    
    ;; 1. Save Global Fallback
    (with-temp-file noteworthy-settings-file
      (insert ";; Noteworthy global settings - fallback\n")
      (let ((print-length nil) (print-level nil))
        (prin1 settings (current-buffer)))
      (insert "\n"))

    ;; 2. Save Project Local (if active)
    (when (and (bound-and-true-p noteworthy-project-root)
               (file-directory-p noteworthy-project-root))
      (let ((local-file (expand-file-name ".noteworthy-layout" noteworthy-project-root)))
        (with-temp-file local-file
          (insert ";; Noteworthy project settings\n")
          (let ((print-length nil) (print-level nil))
            (prin1 settings (current-buffer)))
          (insert "\n"))))))

;; Load global settings immediately on startup
(noteworthy-load-settings)

;; Save config on Emacs exit
(add-hook 'kill-emacs-hook #'noteworthy-save-config)

(defun noteworthy-toggle-log ()
  "Toggle the dedicated terminal window between vterm and Typst log."
  (interactive)
  (let* ((log-buffer-name "*ws-typst-server*")
         (vterm-buffer-name "*vterm*")
         ;; Find the window displaying either vterm or log
         (target-window (cl-find-if (lambda (w)
                                      (with-selected-window w
                                        (let ((bname (buffer-name)))
                                          (or (string= bname log-buffer-name)
                                              (string= bname vterm-buffer-name)
                                              (eq major-mode 'vterm-mode)))))
                                    (window-list))))
    (if target-window
        (with-selected-window target-window
          (let ((current (buffer-name)))
            (cond
             ((or (string= current log-buffer-name)
                  (string-suffix-p "typst-server*" current))
              (if (get-buffer vterm-buffer-name)
                  (switch-to-buffer vterm-buffer-name)
                (message "Noteworthy: vterm buffer not found")))
             (t
              (if (get-buffer log-buffer-name)
                  (switch-to-buffer log-buffer-name)
                (message "Noteworthy: Log buffer %s does not exist." log-buffer-name))))))
      (message "Noteworthy: No terminal/log window found."))))

(provide 'noteworthy-layout)
