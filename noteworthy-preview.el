;;; noteworthy-preview.el --- Preview abstraction for Noteworthy  -*- lexical-binding: t; -*-

(require 'typst-preview)

(defun noteworthy-preview-browser-setup ()
  "Configure preview browser based on system capabilities.
Uses xwidget if available, otherwise falls back to default external browser."
  (if (featurep 'xwidget-internal)
      (setq typst-preview-browser "xwidget")
    (setq typst-preview-browser "default")))

(defun noteworthy-xwidget-available-p ()
  "Return t if xwidget preview is available."
  (featurep 'xwidget-internal))

(defun noteworthy-xwidget-in-side-window (orig-fun &rest args)
  "Advice to make xwidget-webkit open in a right side window."
  (let ((current-window (selected-window)))
    (let* ((side-window (or (window-with-parameter 'noteworthy-preview t)
                            (split-window (frame-root-window) nil 'right)))
           (target-width (if (and (boundp 'noteworthy-preview-width) noteworthy-preview-width)
                             noteworthy-preview-width
                           (round (* 0.35 (frame-width)))))
           (current-width (window-total-width side-window))
           (delta (- target-width current-width)))
      (set-window-parameter side-window 'noteworthy-preview t)
      (when (/= delta 0)
        (ignore-errors (window-resize side-window delta t)))
      (select-window side-window)
      (apply orig-fun args)
      (set-window-dedicated-p side-window t)
      (select-window current-window))))

(advice-add 'xwidget-webkit-browse-url :around #'noteworthy-xwidget-in-side-window)

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

(advice-add 'typst-preview--goto-file-position :override
            (lambda (file-name position)
              "Jump to position in FILE-NAME, reusing existing buffers/windows."
              (let* ((true-path (file-truename file-name))
                     (buffer (cl-find-if (lambda (b)
                                           (when-let* ((bfn (buffer-file-name b)))
                                             (string= (file-truename bfn) true-path)))
                                         (buffer-list))))
                (unless buffer
                  (setq buffer (find-file-noselect true-path)))
                (let ((editor-win (or (cl-find-if (lambda (w) (window-parameter w 'noteworthy-editor))
                                                  (window-list))
                                      (get-largest-window))))
                  (if (and editor-win (window-live-p editor-win))
                      (select-window editor-win)
                    nil))
                (switch-to-buffer buffer)
                (goto-char (point-min))
                (let ((line (if (vectorp position) (aref position 0) (car position)))
                      (col (if (vectorp position) (aref position 1) (cadr position))))
                  (forward-line line)
                  (forward-char col))
                (recenter))))

(defun noteworthy-safe-parse-message (orig-fun sock frame)
   "Advice to safely handle websocket messages, catching any errors."
   (condition-case err
       (apply orig-fun (list sock frame))
     (error (message "Typst Preview Websocket Error (safely ignored): %s" (error-message-string err)))))

(advice-add 'typst-preview--parse-message :around #'noteworthy-safe-parse-message)

(defun noteworthy-typst-send-position ()
  "Send current position to typst preview (jump to source).
Safe version that works for both master and included files."
  (interactive)
  (condition-case err
      (cond
       ((and (boundp 'typst-preview--local-master)
             typst-preview--local-master
             (fboundp 'typst-preview--master-socket)
             (typst-preview--master-socket typst-preview--local-master))
        (typst-preview-send-position)
        (message "Sent position to local master."))
       ((and (boundp 'typst-preview--active-masters)
             typst-preview--active-masters)
        (let* ((master (car typst-preview--active-masters))
               (socket (typst-preview--master-socket master)))
          (if socket
              (let ((msg (json-encode `(("event" . "panelScrollTo")
                                        ("filepath" . ,(file-truename buffer-file-name))
                                        ("line" . ,(1- (line-number-at-pos)))
                                        ("character" . ,(max 1 (current-column)))))))
                (websocket-send-text socket msg)
                (message "Sent position to global typst session"))
            (message "Found active master but no socket connected"))))
       (t (message "No active typst-preview session to send position to")))
    (error (message "typst-preview error: %s" (error-message-string err)))))

(noteworthy-preview-browser-setup)

(provide 'noteworthy-preview)
