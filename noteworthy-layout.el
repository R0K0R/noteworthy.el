;;; noteworthy-layout.el --- Workspace layout for Noteworthy  -*- lexical-binding: t; -*-

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
      (select-window editor-window)
      (split-window-below (floor (* 0.75 (window-height))))
      (other-window 1)
      (let ((default-directory dir)
            (shell (if (and (boundp 'noteworthy-terminal-shell) noteworthy-terminal-shell)
                       noteworthy-terminal-shell
                     (or (executable-find "bash") (getenv "SHELL")))))
        (ansi-term shell))

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
        (run-with-timer 0.6 nil
                        (lambda (ed-win pdf-f)
                          (when (window-live-p ed-win)
                            (select-window ed-win)
                            (let* ((pdf-cols (max 10 (or noteworthy-pdf-width
                                                         (round (* 0.35 (frame-width))))))
                                   (pdf-window (split-window ed-win pdf-cols 'right)))
                              (set-window-parameter pdf-window 'noteworthy-pdf t)
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

(defun noteworthy-load-settings ()
  "Load settings from `noteworthy-settings.el`."
  (when (file-exists-p noteworthy-settings-file)
    (with-temp-buffer
      (insert-file-contents noteworthy-settings-file)
      (condition-case nil
          (let ((settings (read (current-buffer))))
            (when (plist-get settings :pdf-width)
              (setq noteworthy-pdf-width (plist-get settings :pdf-width)))
            (when (plist-get settings :preview-width)
              (setq noteworthy-preview-width (plist-get settings :preview-width))))
        (error (message "Error loading noteworthy settings"))))))

(defun noteworthy-save-config ()
  "Save Noteworthy configuration variables to `noteworthy-settings.el`."
  (let ((settings (list :preview-width (or noteworthy-preview-width
                                           (round (* 0.35 (frame-width))))
                        :pdf-width (or noteworthy-pdf-width
                                       (round (* 0.35 (frame-width)))))))
    (with-temp-file noteworthy-settings-file
      (insert ";; Noteworthy settings - auto-generated, do not edit\n")
      (let ((print-length nil)
            (print-level nil))
        (prin1 settings (current-buffer)))
      (insert "\n"))))

;; Load settings immediately
(noteworthy-load-settings)

;; Save config on Emacs exit
(add-hook 'kill-emacs-hook #'noteworthy-save-config)

(provide 'noteworthy-layout)
