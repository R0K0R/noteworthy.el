;;; noteworthy-preview.el --- Preview abstraction for Noteworthy  -*- lexical-binding: t; -*-

(require 'typst-preview)
;; Deliberately NOT `with-lsp-workspace': a macro must be available at
;; byte-compile time or the call is left as a function call and dies at runtime.
;; It is just a let on `lsp--cur-workspace', so bind that and need no macro.
(defvar lsp--cur-workspace)

(defun noteworthy-preview-browser-setup ()
  "Configure preview browser based on system capabilities.
Uses xwidget if available, otherwise falls back to default external browser."
  (if (featurep 'xwidget-internal)
      (setq typst-preview-browser "xwidget")
    (setq typst-preview-browser "default")))

(defcustom noteworthy-preview-repaint-method 'js
  "How to make the preview xwidget repaint.

Emacs only blits an xwidget when its window is touched, so a preview that
has just recompiled sits stale until the mouse crosses it.  xwidget.el has
no damage-to-redisplay path, so there is nothing to configure -- the page
has to be asked to produce a new frame.

`js\=' nudges the page itself (scroll a pixel and back, plus a throwaway
transform): ~0.01ms, and it yields a genuinely new frame.
`light\=' only marks the window dirty and redisplays: free, but Emacs will
happily redraw the stale pixmap, so it often changes nothing.
`resize\=' forces a full WebKit relayout: ~780ms per call on a remote
preview -- enough to make typing stutter.  Last resort."
  :type '(choice (const :tag "Nudge the page from JS (free)" js)
                 (const :tag "Redisplay the window (free, often ineffective)" light)
                 (const :tag "Resize the widget (slow, last resort)" resize))
  :group 'noteworthy)

(defcustom noteworthy-preview-repaint-interval 0.5
  "Seconds between preview repaint ticks."
  :type 'number
  :group 'noteworthy)

(defvar noteworthy-preview--repaint-timer nil)

(defun noteworthy-preview--xwidget-windows ()
  "Return (BUFFER . WINDOW) pairs for displayed webkit xwidgets."
  (delq nil
        (mapcar (lambda (buf)
                  (when (string-match-p "xwidget-webkit" (buffer-name buf))
                    (let ((win (get-buffer-window buf t)))
                      (and win (cons buf win)))))
                (buffer-list))))

(defun noteworthy-preview-repaint ()
  "Make the preview xwidget repaint now."
  (interactive)
  (dolist (pair (noteworthy-preview--xwidget-windows))
    (let ((buf (car pair)) (win (cdr pair)))
      (with-current-buffer buf
        (let ((xw (ignore-errors (xwidget-webkit-current-session))))
          (when xw
            (pcase noteworthy-preview-repaint-method
              ('js
               (ignore-errors
                 (xwidget-webkit-execute-script
                  xw (concat "(function(){var e=document.scrollingElement||document.documentElement;"
                             "var y=e.scrollTop;e.scrollTop=y+1;e.scrollTop=y;"
                             "var b=document.body;if(b){b.style.transform='translateZ(0)';"
                             "requestAnimationFrame(function(){b.style.transform='';});}})();"))))
              ('resize
               (let ((w (window-pixel-width win)) (h (window-pixel-height win)))
                 (ignore-errors (xwidget-resize xw (max 1 (1- w)) h))
                 (ignore-errors (xwidget-resize xw w h))))
              (_ nil)))))
      (force-window-update win)))
  (when (memq noteworthy-preview-repaint-method '(light resize))
    (redisplay t)))

(defun noteworthy-preview--repaint-tick ()
  "Repaint if a preview is on screen, otherwise do nothing."
  (when (noteworthy-preview--xwidget-windows)
    (noteworthy-preview-repaint)))

;;;###autoload
(define-minor-mode noteworthy-preview-repaint-mode
  "Keep the preview xwidget repainting on its own.
Without this the preview only updates when the mouse happens to cross it."
  :global t
  :group 'noteworthy
  (when (timerp noteworthy-preview--repaint-timer)
    (cancel-timer noteworthy-preview--repaint-timer)
    (setq noteworthy-preview--repaint-timer nil))
  (when noteworthy-preview-repaint-mode
    (setq noteworthy-preview--repaint-timer
          (run-with-timer noteworthy-preview-repaint-interval
                          noteworthy-preview-repaint-interval
                          #'noteworthy-preview--repaint-tick))))

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

;;; LSP-hosted preview -------------------------------------------------------
;; typst-preview.el spawns its own tinymist and talks to it over a websocket.
;; When tinymist is already running as the LSP there is no reason for a second
;; one, and the websocket masters are exactly what made M-o unreliable: the
;; scroll only worked once a master had been established for that file.
;; tinymist.scrollPreview names the file in the event, so any workspace can
;; carry it.

(declare-function lsp-workspaces "lsp-mode")
(declare-function lsp-request "lsp-mode" (method params &rest args))
(declare-function lsp-request-async "lsp-mode" (method params callback &rest args))
(declare-function lsp--session-workspaces "lsp-mode" (session))
(declare-function lsp--workspace-server-id "lsp-mode" (workspace))
(declare-function lsp-session "lsp-mode")

(defcustom noteworthy-preview-data-port 23627
  "Port for the LSP-hosted preview's data plane."
  :type 'integer :group 'noteworthy)

(defcustom noteworthy-preview-control-port 23628
  "Port for the LSP-hosted preview's control plane."
  :type 'integer :group 'noteworthy)

(defvar noteworthy-preview-id "default_preview"
  "Task id tinymist gives the preview started by `noteworthy-preview-start'.")

(defun noteworthy-preview--tinymist-workspace ()
  "A live tinymist workspace, whatever buffer we are called from.
Not `lsp-workspaces' alone: a just-opened content file has none yet, and
that is precisely when M-o used to do nothing."
  (or (car (ignore-errors (lsp-workspaces)))
      (seq-find (lambda (w)
                  (string-match-p "tinymist"
                                  (format "%s" (lsp--workspace-server-id w))))
                (ignore-errors (lsp--session-workspaces (lsp-session))))))

(defun noteworthy-preview--source-buffer ()
  "The buffer whose point the preview should follow.
The preview pane itself visits no file, so fall back to the last .typ one."
  (if buffer-file-name
      (current-buffer)
    (seq-find (lambda (b)
                (let ((f (buffer-local-value 'buffer-file-name b)))
                  (and f (string-suffix-p ".typ" f))))
              (buffer-list))))

;;;###autoload
(defun noteworthy-preview-start ()
  "Start the preview in the tinymist that is already running as the LSP."
  (interactive)
  (let* ((ws (noteworthy-preview--tinymist-workspace))
         (root (or (bound-and-true-p noteworthy-project-root)
                   (when-let* ((d (locate-dominating-file
                                   (or default-directory "") "noteworthy.py")))
                     (expand-file-name d))
                   default-directory))
         (main (or (bound-and-true-p noteworthy-master-file)
                   (expand-file-name "templates/core/parser.typ" root))))
    (unless ws (user-error "No tinymist LSP -- open a .typ file in the project"))
    (let ((lsp-response-timeout 30))
      (let ((lsp--cur-workspace ws))
        (lsp-request "workspace/executeCommand"
                     (list :command "tinymist.doStartPreview"
                           :arguments
                           (vector (vector "--data-plane-host"
                                           (format "127.0.0.1:%d" noteworthy-preview-data-port)
                                           "--control-plane-host"
                                           (format "127.0.0.1:%d" noteworthy-preview-control-port)
                                           "--invert-colors" "never"
                                           "--root" (directory-file-name root)
                                           main)))))
      (message "Preview hosted on port %d" noteworthy-preview-data-port))))

(defun noteworthy-typst-send-position ()
  "Send current position to typst preview (jump to source).
Safe version that works for both master and included files."
  (interactive)
  (let ((src (noteworthy-preview--source-buffer)))
    (unless src (user-error "No Typst buffer to scroll from"))
    (unless (eq src (current-buffer)) (set-buffer src)))
  (condition-case err
      (cond
       ;; Prefer the LSP: no second tinymist, and it works from a buffer whose
       ;; own server has not started yet.
       ((noteworthy-preview--tinymist-workspace)
        (let ((lsp--cur-workspace (noteworthy-preview--tinymist-workspace)))
          (lsp-request-async
           "workspace/executeCommand"
           (list :command "tinymist.scrollPreview"
                 :arguments (vector noteworthy-preview-id
                                    (list :event "panelScrollTo"
                                          :filepath (file-truename buffer-file-name)
                                          :line (1- (line-number-at-pos))
                                          :character (max 0 (- (point) (line-beginning-position))))))
           #'ignore :mode 'detached
           :error-handler (lambda (e) (message "Preview scroll refused: %s" e))))
        (message "Preview -> %s:%d" (file-name-nondirectory buffer-file-name)
                 (line-number-at-pos)))
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

;; The preview cannot repaint itself; without this it only updates when the
;; mouse happens to cross it.  A tick is a no-op when nothing is displayed.
(when (featurep 'xwidget-internal)
  (noteworthy-preview-repaint-mode 1))

(provide 'noteworthy-preview)
