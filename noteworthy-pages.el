;;; noteworthy-pages.el --- Add pages and chapters to a Noteworthy project  -*- lexical-binding: t; -*-

;;; Commentary:

;; A page is two things that have to agree: an entry in
;; config/hierarchy.json, which gives its title, and a file
;; content/<chapter>/<page>.typ.  They are matched by position -- the
;; i-th chapter of the hierarchy is the i-th chapter folder in numeric
;; order, the j-th page the j-th file in it -- so adding one by hand means
;; editing the JSON at the right index, creating the file under the right
;; number, and restarting the preview, whose folder lists are fixed when it
;; starts (noteworthy.py --print-inputs).  `noteworthy-new-page' does all
;; three.

;;; Code:

(require 'json)
(require 'seq)

(defvar noteworthy-project-root)
(defvar typst-preview--active-masters)
(defvar typst-preview--active-buffers)
(defvar typst-preview-cmd-options)
(defvar typst-preview--master-file)
(defvar noteworthy-master-file)
(declare-function noteworthy-typst-get-dynamic-inputs "noteworthy" (root))
(declare-function typst-preview-start "typst-preview" (&optional open-browser))
(declare-function typst-preview--send-buffer-on-type "typst-preview")
(declare-function typst-preview--master-process "typst-preview")
(declare-function typst-preview--master-socket "typst-preview")
(declare-function websocket-close "websocket")
(declare-function noteworthy--editor-window "noteworthy-preview")

(defconst noteworthy-pages-default-header
  "#import \"../../templates/templater.typ\": *\n"
  "What a new page starts with when no other page shows the project's own.")

(defun noteworthy-pages--root ()
  "The project root.
`noteworthy-project-root', or the directory holding noteworthy.py."
  (let ((root (or (bound-and-true-p noteworthy-project-root)
                  (locate-dominating-file (or buffer-file-name default-directory) "noteworthy.py"))))
    (unless (and root (file-exists-p (expand-file-name "config/hierarchy.json" root)))
      (user-error "Not in a Noteworthy project (no config/hierarchy.json)"))
    (file-name-as-directory (expand-file-name root))))

(defun noteworthy-pages--numbered (dir regexp)
  "Numbers of the entries of DIR matching REGEXP, ascending."
  (and (file-directory-p dir)
       (sort (mapcar (lambda (f) (string-to-number (file-name-base f)))
                     (directory-files dir nil regexp))
             #'<)))

(defun noteworthy-pages--chapters (root)
  "Chapter folder numbers of ROOT, in hierarchy order."
  (seq-filter (lambda (n) (file-directory-p (expand-file-name (format "content/%d" n) root)))
              (noteworthy-pages--numbered (expand-file-name "content" root) "\\`[0-9]+\\'")))

(defun noteworthy-pages--pages (root chapter)
  "Page file numbers of CHAPTER in ROOT, in hierarchy order."
  (noteworthy-pages--numbered (expand-file-name (format "content/%d" chapter) root)
                              "\\`[0-9]+\\.typ\\'"))

(defun noteworthy-pages--read-hierarchy (root)
  "config/hierarchy.json of ROOT, as a list of alists."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "config/hierarchy.json" root))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun noteworthy-pages--write-hierarchy (root hierarchy)
  "Write HIERARCHY to ROOT's config/hierarchy.json, indented as the file is."
  (let ((file (expand-file-name "config/hierarchy.json" root)))
    (with-temp-buffer
      (insert (json-serialize (vconcat (mapcar #'noteworthy-pages--vectorize hierarchy))))
      (let ((json-encoding-default-indentation "  "))
        (json-pretty-print-buffer))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (write-region nil nil file nil 'silent))
    ;; A buffer visiting it would otherwise go on showing the old text.
    (when-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf (revert-buffer t t t)))))

(defun noteworthy-pages--vectorize (value)
  "VALUE with its lists of objects as vectors, which `json-serialize' wants."
  (cond ((and (consp value) (consp (car value)) (symbolp (caar value)))
         (mapcar (lambda (kv) (cons (car kv) (noteworthy-pages--vectorize (cdr kv)))) value))
        ((consp value) (vconcat (mapcar #'noteworthy-pages--vectorize value)))
        ((null value) [])
        (t value)))

(defun noteworthy-pages--insert-at (list index elt)
  "LIST with ELT inserted before position INDEX."
  (append (seq-take list index) (list elt) (seq-drop list index)))

(defun noteworthy-pages--position (numbers n)
  "How many of the ascending NUMBERS are below N: where N goes."
  (seq-count (lambda (m) (< m n)) numbers))

(defun noteworthy-pages--header (root chapter)
  "The first line of a sibling page in CHAPTER, else of any page, else the default."
  (let ((page (or (car (directory-files (expand-file-name (format "content/%d" chapter) root) t "\\`[0-9]+\\.typ\\'"))
                  (car (directory-files-recursively (expand-file-name "content" root) "\\`[0-9]+\\.typ\\'")))))
    (or (and page
             (with-temp-buffer
               (insert-file-contents page nil 0 400)
               (goto-char (point-min))
               (and (looking-at "#import [^\n]*\n") (match-string 0))))
        noteworthy-pages-default-header)))

(defun noteworthy-pages--read-number (prompt taken default)
  "Read a number with PROMPT, not one of TAKEN, offering DEFAULT."
  (let ((n (read-number prompt default)))
    (while (or (< n 1) (/= n (truncate n)) (memq n taken))
      (setq n (read-number (format "%d is taken or invalid. %s" n prompt) default)))
    (truncate n)))

;;;###autoload
(defun noteworthy-new-page ()
  "Add a page, in an existing chapter or a new one, and open it.
Asks for the chapter -- or \"New chapter...\" with its number, title and
summary -- then the page's number and title.  The number defaults to the
next free one; any free number may be given, and the hierarchy entry goes
where that number sorts, which is where the build will look for it.
Writes config/hierarchy.json, creates content/<chapter>/<page>.typ, opens
it, and refreshes the preview."
  (interactive)
  (let* ((root (noteworthy-pages--root))
         (hierarchy (noteworthy-pages--read-hierarchy root))
         (chapters (noteworthy-pages--chapters root))
         (choices (append
                   (seq-map-indexed
                    (lambda (ch i)
                      (cons (format "%s  %s" (or (nth i chapters) "?") (alist-get 'title ch)) i))
                    hierarchy)
                   '(("New chapter..." . new))))
         (pick (cdr (assoc (completing-read "Chapter: " choices nil t) choices)))
         chapter-index chapter)
    (when (/= (length chapters) (length hierarchy))
      (unless (y-or-n-p (format "hierarchy.json has %d chapters but content/ has %d folders; continue? "
                                (length hierarchy) (length chapters)))
        (user-error "Aborted")))
    (if (eq pick 'new)
        (let* ((n (noteworthy-pages--read-number "Chapter number: " chapters
                                                 (1+ (apply #'max 0 chapters))))
               (title (read-string "Chapter title: "))
               (summary (read-string "Chapter summary (optional): ")))
          (setq chapter n
                chapter-index (noteworthy-pages--position chapters n)
                hierarchy (noteworthy-pages--insert-at
                           hierarchy chapter-index
                           `((title . ,title) (summary . ,summary) (pages . nil))))
          (make-directory (expand-file-name (format "content/%d" n) root) t))
      (setq chapter-index pick
            chapter (nth pick chapters)))
    (unless chapter (user-error "Chapter %d has no folder in content/" (1+ chapter-index)))
    (let* ((pages (noteworthy-pages--pages root chapter))
           (n (noteworthy-pages--read-number "Page number: " pages (1+ (apply #'max 0 pages))))
           (title (read-string "Page title: "))
           (entry (nth chapter-index hierarchy))
           (file (expand-file-name (format "content/%d/%d.typ" chapter n) root)))
      (setf (alist-get 'pages entry)
            (noteworthy-pages--insert-at (alist-get 'pages entry)
                                         (noteworthy-pages--position pages n)
                                         `((title . ,title))))
      (setf (nth chapter-index hierarchy) entry)
      (with-temp-file file (insert (noteworthy-pages--header root chapter) "\n"))
      (noteworthy-pages--write-hierarchy root hierarchy)
      (let ((win (and (fboundp 'noteworthy--editor-window) (noteworthy--editor-window))))
        (if (window-live-p win)
            (progn (select-window win) (find-file file))
          (find-file file)))
      (goto-char (point-max))
      (noteworthy-refresh-preview)
      (message "Added %d.%d %s" chapter n title))))

(defun noteworthy-pages--stop-preview ()
  "Stop every typst-preview server, whichever buffer started it.
`typst-preview-stop' only stops the server from the master file's own
buffer; started from a page, as the layout does, it merely detaches."
  (when (boundp 'typst-preview--active-masters)
    (dolist (m typst-preview--active-masters)
      (ignore-errors (websocket-close (typst-preview--master-socket m)))
      (ignore-errors (delete-process (typst-preview--master-process m))))
    (setq typst-preview--active-masters nil
          typst-preview--active-buffers nil)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (local-variable-p 'typst-preview--local-master)
          (kill-local-variable 'typst-preview--local-master)
          (remove-hook 'after-change-functions #'typst-preview--send-buffer-on-type t))))))

;;;###autoload
(defun noteworthy-refresh-preview ()
  "Restart the preview with the project's chapters and pages as they are now.
The preview is given its folder lists when it starts, so a page or chapter
added since is missing from it -- or, worse, the next page's content shows
under its title -- until it restarts."
  (interactive)
  (let* ((root (noteworthy-pages--root))
         (src (if (and buffer-file-name (string-suffix-p ".typ" buffer-file-name))
                  (current-buffer)
                (seq-find (lambda (b)
                            (let ((f (buffer-local-value 'buffer-file-name b)))
                              (and f (string-suffix-p ".typ" f) (string-prefix-p root (expand-file-name f)))))
                          (buffer-list)))))
    (unless src (user-error "No Typst buffer of this project to preview from"))
    (unless (require 'typst-preview nil t) (user-error "typst-preview is not installed"))
    (noteworthy-pages--stop-preview)
    (with-current-buffer src
      (let ((inputs (noteworthy-typst-get-dynamic-inputs root)))
        (setq-local typst-preview-cmd-options
                    (append (default-value 'typst-preview-cmd-options) inputs)))
      (when (bound-and-true-p noteworthy-master-file)
        (setq-local typst-preview--master-file noteworthy-master-file))
      (save-selected-window (typst-preview-start t)))))

(provide 'noteworthy-pages)

;;; noteworthy-pages.el ends here
