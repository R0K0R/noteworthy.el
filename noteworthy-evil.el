;;; noteworthy-evil.el --- Evil bindings for Noteworthy  -*- lexical-binding: t; -*-

(require 'evil)
(require 'noteworthy-typst)

;; Smart Open commands (vim-specific logic)
(defun noteworthy-typst-smart-o ()
  "Open line below with smart indentation."
  (interactive)
  (cond
   ;; List marker
   ((noteworthy-typst-get-list-marker)
    (let ((prefix (noteworthy-typst-get-list-marker)))
      (end-of-line)
      (newline)
      (insert prefix)
      (evil-insert-state)))
   ;; Default
   (t (evil-open-below 1))))

(defun noteworthy-typst-smart-O ()
  "Open line above. Continue list if on list item, else use default evil-open-above."
  (interactive)
  (let ((list-prefix (noteworthy-typst-get-list-marker)))
    (if list-prefix
        (progn
          (beginning-of-line)
          (newline)
          (forward-line -1)
          (insert list-prefix)
          (evil-insert-state))
      ;; Use default Evil behavior for proper indentation
      (evil-open-above 1))))

;; Keybindings setup
(defun noteworthy-evil-setup ()
  "Apply Evil bindings for Noteworthy Typst mode."
  (with-eval-after-load 'evil
    (evil-make-overriding-map noteworthy-typst-mode-map 'insert)
    (evil-define-key 'insert noteworthy-typst-mode-map
      (kbd "SPC") #'noteworthy-typst-smart-space
      (kbd "RET") #'noteworthy-typst-smart-newline
      (kbd "<return>") #'noteworthy-typst-smart-newline
      "*" (lambda () (interactive) (noteworthy-typst-smart-pair ?*))
      "_" (lambda () (interactive) (noteworthy-typst-smart-pair ?_))
      "$" #'noteworthy-typst-smart-dollar
      "`" #'noteworthy-typst-smart-backtick
      (kbd "DEL") #'noteworthy-typst-smart-backspace
      (kbd "<backspace>") #'noteworthy-typst-smart-backspace
      (kbd "TAB") #'indent-for-tab-command
      (kbd "<backtab>") #'noteworthy-typst-dedent-line)
    
    ;; Bind 'o' in normal mode for list continuation
    (evil-define-key 'normal noteworthy-typst-mode-map
      "o" #'noteworthy-typst-smart-o
      "O" #'noteworthy-typst-smart-O)
    
    ;; Bind remote preview/pdf commands if available
    (evil-define-key 'normal noteworthy-typst-mode-map
      (kbd "M-o") #'noteworthy-typst-send-position)
    (evil-define-key 'insert noteworthy-typst-mode-map
      (kbd "M-o") #'noteworthy-typst-send-position)))

;; Apply setup immediately
(noteworthy-evil-setup)

(provide 'noteworthy-evil)
