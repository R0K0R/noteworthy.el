;;; noteworthy-evil.el --- Evil bindings for Noteworthy  -*- lexical-binding: t; -*-

(require 'evil)
(require 'noteworthy-typst)

(defun noteworthy-typst-smart-o ()
  "Open line below with smart indentation."
  (interactive)
  (cond
   ;; List context
   ((noteworthy-typst-get-list-marker)
    (let ((prefix (noteworthy-typst-get-list-marker)))
      (end-of-line)
      (newline)
      (insert prefix)
      (evil-insert-state)))
   
   ;; At end of line with opening bracket → add indent
   ((save-excursion
      (back-to-indentation)
      (looking-at ".*[[{(][ \t]*$"))
    (let ((base-indent (noteworthy-typst-get-current-indent))
          (indent-unit (noteworthy-typst-get-indent-unit)))
      (end-of-line)
      (newline)
      (insert base-indent indent-unit)
      (evil-insert-state)))
   
   ;; Inside brackets - maintain indent
   ((save-excursion
      (ignore-errors
        (backward-up-list 1)
        (memq (char-after) '(?\( ?\[ ?\{))))
    (let ((indent (noteworthy-typst-get-current-indent)))
      (end-of-line)
      (newline)
      (insert indent)
      (evil-insert-state)))
   
   ;; Default: maintain current indent
   (t
    (let ((indent (noteworthy-typst-get-current-indent)))
      (end-of-line)
      (newline)
      (insert indent)
      (evil-insert-state)))))

(defun noteworthy-typst-smart-O ()
  "Open line above with smart indentation."
  (interactive)
  (cond
   ;; List context
   ((noteworthy-typst-get-list-marker)
    (let ((prefix (noteworthy-typst-get-list-marker)))
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (insert prefix)
      (evil-insert-state)))
   
   ;; At start of line with closing bracket → add indent
   ((save-excursion
      (back-to-indentation)
      (looking-at "[]})]"))
    (let ((base-indent (noteworthy-typst-get-current-indent))
          (indent-unit (noteworthy-typst-get-indent-unit)))
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (insert base-indent indent-unit)
      (evil-insert-state)))
   
   ;; Inside brackets - maintain indent
   ((save-excursion
      (ignore-errors
        (backward-up-list 1)
        (memq (char-after) '(?\( ?\[ ?\{))))
    (let ((indent (noteworthy-typst-get-current-indent)))
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (insert indent)
      (evil-insert-state)))
   
   ;; Default: maintain current indent
   (t
    (let ((indent (noteworthy-typst-get-current-indent)))
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (insert indent)
      (evil-insert-state)))))

(defun noteworthy-evil-setup ()
  "Apply Evil bindings for Noteworthy Typst mode."
  (with-eval-after-load 'evil

    (evil-define-key 'insert noteworthy-typst-mode-map
      (kbd "SPC") #'noteworthy-typst-smart-space
      (kbd "RET") #'noteworthy-typst-smart-newline
      (kbd "<return>") #'noteworthy-typst-smart-newline
      "*" (lambda () (interactive) (noteworthy-typst-smart-pair ?*))
      "_" (lambda () (interactive) (noteworthy-typst-smart-pair ?_))
      "\"" #'noteworthy-typst-smart-quote
      "$" #'noteworthy-typst-smart-dollar
      "`" #'noteworthy-typst-smart-backtick
      (kbd "DEL") #'noteworthy-typst-smart-backspace
      (kbd "<backspace>") #'noteworthy-typst-smart-backspace
      (kbd "TAB") #'noteworthy-typst-indent-line
      (kbd "<backtab>") #'noteworthy-typst-dedent-line
      ;; Evil's insert state map shadows the plain mode map, so ) has to be
      ;; bound here too or the literal insertion keeps winning.
      ")" #'noteworthy-typst-close-paren
      "]" #'noteworthy-typst-close-bracket
      "}" #'noteworthy-typst-close-brace
      (kbd "C-)") #'noteworthy-typst-insert-close-paren
      (kbd "C-]") #'noteworthy-typst-insert-close-bracket
      (kbd "C-}") #'noteworthy-typst-insert-close-brace)

    ;; `p' still pastes text; it only diverts when the clipboard actually
    ;; holds an image, so the key keeps its ordinary meaning.
    (evil-define-key 'normal noteworthy-typst-mode-map
      "p" #'noteworthy-image-paste-or-yank)

    ;; $...$ as a text object, so ci$ / ya$ / di$ work the way they do on
    ;; brackets.  These are evil's global inner/outer maps -- $ has no default
    ;; binding in either, so this only adds.
    (evil-define-text-object noteworthy-evil-inner-dollar (count &optional beg end type)
      "Select the contents of the surrounding $...$."
      (let ((b (noteworthy-typst-dollar-bounds)))
        (unless b (user-error "Not inside $...$"))
        (evil-range (1+ (car b)) (cdr b))))

    (evil-define-text-object noteworthy-evil-a-dollar (count &optional beg end type)
      "Select the surrounding $...$, delimiters included."
      (let ((b (noteworthy-typst-dollar-bounds)))
        (unless b (user-error "Not inside $...$"))
        (evil-range (car b) (1+ (cdr b)))))

    (define-key evil-inner-text-objects-map "$" #'noteworthy-evil-inner-dollar)
    (define-key evil-outer-text-objects-map "$" #'noteworthy-evil-a-dollar)

    ;; Normal mode bindings
    (evil-define-key 'normal noteworthy-typst-mode-map
      "o" #'noteworthy-typst-smart-o
      "O" #'noteworthy-typst-smart-O)

    ;; Explicitly bind M-o separately to ensure it registers
    (evil-define-key 'normal noteworthy-typst-mode-map
      (kbd "M-o") #'noteworthy-typst-send-position)
    
    ;; Backup: Bind in the minor mode map directly (Evil allows fallthrough to this)
    (define-key noteworthy-typst-mode-map (kbd "M-o") #'noteworthy-typst-send-position)

    (evil-define-key 'insert noteworthy-typst-mode-map
      (kbd "M-o") #'noteworthy-typst-send-position)


    ;; Structure editing on the home row.  Bound here as well as in the plain
    ;; mode map because evil's state maps shadow it.
    (evil-define-key '(normal insert) noteworthy-typst-mode-map
      (kbd "M-l") #'noteworthy-typst-demote
      (kbd "M-h") #'noteworthy-typst-promote
      (kbd "M-j") #'noteworthy-typst-move-line-down
      (kbd "M-k") #'noteworthy-typst-move-line-up
      (kbd "M-<return>") #'noteworthy-typst-meta-return
      (kbd "M-RET") #'noteworthy-typst-meta-return)

    ;; Remote PDF Scrolling (Alt+Shift+hjkl)
    (evil-define-key '(normal insert) noteworthy-typst-mode-map
      (kbd "M-J") (lambda () (interactive) (noteworthy-pdf-scroll 'down))
      (kbd "M-K") (lambda () (interactive) (noteworthy-pdf-scroll 'up))
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
