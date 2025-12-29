;;; noteworthy-evil.el --- Evil bindings for Noteworthy  -*- lexical-binding: t; -*-

(require 'evil)
(require 'noteworthy-typst)

(defun noteworthy-typst-smart-o ()
  "Open line below with smart indentation."
  (interactive)
  (cond
   ((noteworthy-typst-get-list-marker)
    (let ((prefix (noteworthy-typst-get-list-marker)))
      (end-of-line)
      (newline)
      (insert prefix)
      (evil-insert-state)))
   (t (evil-open-below 1))))

(defun noteworthy-typst-smart-O ()
  "Open line above.
Continue list if on list item, else use default evil-open-above."
  (interactive)
  (let ((list-prefix (noteworthy-typst-get-list-marker)))
    (if list-prefix
        (progn
          (beginning-of-line)
          (newline)
          (forward-line -1)
          (insert list-prefix)
          (evil-insert-state))
      (evil-open-above 1))))

(defun noteworthy-evil-setup ()
  "Apply Evil bindings for Noteworthy Typst mode."
  (with-eval-after-load 'evil

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
    (evil-define-key 'normal noteworthy-typst-mode-map
      "o" #'noteworthy-typst-smart-o
      "O" #'noteworthy-typst-smart-O
      (kbd "M-o") #'noteworthy-typst-send-position)
    (evil-define-key 'insert noteworthy-typst-mode-map
      (kbd "M-o") #'noteworthy-typst-send-position)

    ;; Remote PDF Scrolling (Alt+Shift+hjkl)
    (evil-define-key '(normal insert) noteworthy-typst-mode-map
      (kbd "M-J") (lambda () (interactive) (noteworthy-pdf-scroll 'down))
      (kbd "M-K") (lambda () (interactive) (noteworthy-pdf-scroll 'up))
      (kbd "M-H") (lambda () (interactive) (noteworthy-pdf-scroll 'left))
      (kbd "M-H") (lambda () (interactive) (noteworthy-pdf-scroll 'left))
      (kbd "M-L") (lambda () (interactive) (noteworthy-pdf-scroll 'right))
      
      ;; Toggle Log (Alt+t l)
      (kbd "M-t l") #'noteworthy-toggle-log)))

(defun noteworthy-pdf-scroll (direction)
  "Scroll the visible PDF window in DIRECTION."
  (let ((pdf-window (cl-find-if (lambda (w)
                                  (with-selected-window w
                                    (eq major-mode 'pdf-view-mode)))
                                (window-list))))
    (if pdf-window
        (with-selected-window pdf-window
          (cond
           ((eq direction 'up) (pdf-view-previous-line-or-previous-page 5))
           ((eq direction 'down) (pdf-view-next-line-or-next-page 5))
           ((eq direction 'left) (image-backward-hscroll 20))
           ((eq direction 'right) (image-forward-hscroll 20))))
      (message "No PDF window found to scroll"))))

(noteworthy-evil-setup)

(provide 'noteworthy-evil)
