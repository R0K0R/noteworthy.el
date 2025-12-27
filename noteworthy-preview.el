;;; noteworthy-preview.el --- Preview abstraction for Noteworthy  -*- lexical-binding: t; -*-

(require 'typst-preview)

(defun noteworthy-preview-browser-setup ()
  "Configure preview browser based on system capabilities.
Uses xwidget if available, otherwise falls back to default external browser."
  (if (featurep 'xwidget-internal)
      (setq typst-preview-browser 'xwidget)
    (setq typst-preview-browser 'default)))

(defun noteworthy-xwidget-available-p ()
  "Return t if xwidget preview is available."
  (featurep 'xwidget-internal))

;; Force xwidget to open in a right side window if using xwidgets
(defun noteworthy-xwidget-in-side-window (orig-fun &rest args)
  "Advice to make xwidget-webkit open in a right side window."
  (let ((current-window (selected-window)))
    ;; Create or get the right side window
    (let* ((side-window (or (window-with-parameter 'noteworthy-preview t)
                            ;; Fallback only if no tagged window exists
                            (split-window (frame-root-window) 
                                          (floor (* 0.25 (frame-width)))
                                          'right)))
           (current-width (window-total-width side-window)))
      
      ;; Ensure window parameter is set
      (set-window-parameter side-window 'noteworthy-preview t)
      
      (select-window side-window)
      (apply orig-fun args)
      
      ;; Re-dedicate and return check
      (set-window-dedicated-p side-window t)
      (select-window current-window))))

(advice-add 'xwidget-webkit-browse-url :around #'noteworthy-xwidget-in-side-window)

;; Fix hyperlinks opening in preview window - advise find-file to use editor window
(defvar noteworthy--editor-window nil
  "Reference to the main editor window.")

(defun noteworthy-save-editor-window ()
  "Save reference to current window as editor window."
  (setq noteworthy--editor-window (selected-window)))

(add-hook 'typst-ts-mode-hook #'noteworthy-save-editor-window)

(defun noteworthy-find-file-in-editor (orig-fun &rest args)
  "Advice to make find-file open in editor window when invoked from xwidget."
  (if (and noteworthy--editor-window
           (window-live-p noteworthy--editor-window)
           (derived-mode-p 'xwidget-webkit-mode))
      (progn
        (select-window noteworthy--editor-window)
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'find-file :around #'noteworthy-find-file-in-editor)

;; Override typst-preview's jump function to solve duplicate buffer issues completely
(advice-add 'typst-preview--goto-file-position :override
            (lambda (file-name position)
              "Jump to position in FILE-NAME, reusing existing buffers/windows."
              (let* ((true-path (file-truename file-name))
                     ;; Robustly find existing buffer by checking truename of all buffers
                     (buffer (cl-find-if (lambda (b)
                                           (when-let ((bfn (buffer-file-name b)))
                                             (string= (file-truename bfn) true-path)))
                                         (buffer-list))))
                
                ;; 1. Ensure we have the buffer
                (unless buffer
                  (setq buffer (find-file-noselect true-path)))
                
                ;; 2. Select the correct window to display it in
                (let ((editor-win (or (cl-find-if (lambda (w) (window-parameter w 'noteworthy-editor))
                                                  (window-list))
                                      ;; Fallback: Try to use the largest window that isn't the preview
                                      (get-largest-window))))
                  (if (and editor-win (window-live-p editor-win))
                      (select-window editor-win)
                    ;; Fallback: just use selected
                    nil))
                
                ;; 3. Switch to buffer in the confirmed window
                (switch-to-buffer buffer)
                
                ;; 4. Move point
                (goto-char (point-min))
                (let ((line (if (vectorp position) (aref position 0) (car position)))
                      (col (if (vectorp position) (aref position 1) (cadr position))))
                  (forward-line line)
                  (forward-char col))
                (recenter))))

;; Safe Websocket parsing
(defun noteworthy-safe-parse-message (orig-fun sock frame)
   "Advice to safely handle websocket messages, catching any errors."
   (condition-case err
       (apply orig-fun (list sock frame))
     (error (message "Typst Preview Websocket Error (safely ignored): %s" (error-message-string err)))))

(advice-add 'typst-preview--parse-message :around #'noteworthy-safe-parse-message)

;; Configure browser on load
(noteworthy-preview-browser-setup)

(provide 'noteworthy-preview)
