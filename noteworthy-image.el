;;; noteworthy-image.el --- Paste clipboard images into Typst  -*- lexical-binding: t; -*-

;;; Commentary:

;; Paste an image straight out of the clipboard: the bytes are written next
;; to the current file and an `#image(...)' call is inserted pointing at
;; them.  Files are named by a hash of their content, so pasting the same
;; screenshot twice reuses one file instead of growing a second copy.
;;
;; The buffer is usually a TRAMP file (the project lives on the collab
;; server) while the clipboard is local, so the bytes are read here and
;; written through TRAMP -- which is what puts them where the compiler,
;; the preview and every other client can see them.

;;; Code:

(require 'cl-lib)

(declare-function noteworthy-typst-context "noteworthy-typst")

(defgroup noteworthy-image nil
  "Clipboard image pasting for Noteworthy."
  :group 'noteworthy)

(defcustom noteworthy-image-directory "images"
  "Directory for pasted images, relative to the file being edited.
Created on demand.  `content/8/1.typ' with the default puts images in
`content/8/images/', which is also how Typst resolves the relative path
in the `#image()' call -- relative to the file holding it, so it keeps
working when `parser.typ' includes the page."
  :type 'string
  :group 'noteworthy-image)

(defcustom noteworthy-image-hash-length 12
  "How many hex characters of the content hash to use in a file name.
Long enough that a collision needs a deliberate effort, short enough to
read.  Changing this does not rename images already pasted."
  :type 'integer
  :group 'noteworthy-image)

(defcustom noteworthy-image-insert-format "#image(\"%s\")"
  "Format for the inserted call; %s is the path relative to the file.
Set it to something like \"#figure(image(\\\"%s\\\", width: 80%%), caption: [])\"
to get a captioned figure instead.  A %% is a literal percent."
  :type 'string
  :group 'noteworthy-image)

(defconst noteworthy-image--types
  '(("image/png"     . "png")
    ("image/jpeg"    . "jpg")
    ("image/webp"    . "webp")
    ("image/gif"     . "gif")
    ("image/svg+xml" . "svg"))
  "Clipboard MIME types we accept, best first, mapped to a file extension.")

;; ------------------------------------------------------------------
;; Reading the clipboard

(defun noteworthy-image--backend ()
  "Return (LIST-CMD . READ-CMD-BUILDER) for this session's clipboard, or nil.
Wayland and X11 need different tools, and neither is guaranteed to be
installed -- returning nil lets the caller fall through to a normal paste
rather than error at someone who just pressed `p'."
  (cond
   ((executable-find "wl-paste")
    (cons (list "wl-paste" "--list-types")
          (lambda (type) (list "wl-paste" "--no-newline" "--type" type))))
   ((executable-find "xclip")
    (cons (list "xclip" "-selection" "clipboard" "-t" "TARGETS" "-o")
          (lambda (type) (list "xclip" "-selection" "clipboard" "-t" type "-o"))))))

(defun noteworthy-image--available-type ()
  "Return (MIME . EXT) for the best image type in the clipboard, or nil."
  (when-let* ((backend (noteworthy-image--backend)))
    (let ((types (ignore-errors
                   (with-temp-buffer
                     (when (zerop (apply #'call-process (caar backend) nil t nil
                                         (cdar backend)))
                       (buffer-string))))))
      (when types
        (cl-find-if (lambda (pair)
                      (string-match-p (regexp-quote (car pair)) types))
                    noteworthy-image--types)))))

(defun noteworthy-image--read (mime)
  "Return the clipboard\='s MIME data as a unibyte string, or nil."
  (when-let* ((backend (noteworthy-image--backend)))
    (let ((cmd (funcall (cdr backend) mime)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'binary))
          (when (zerop (apply #'call-process (car cmd) nil t nil (cdr cmd)))
            (and (> (buffer-size) 0) (buffer-string))))))))

(defun noteworthy-image-grab ()
  "Return (DATA . EXT) for an image in the clipboard, or nil.

Reads rather than merely asking what is on offer.  A Wayland clipboard
keeps advertising the types of a source client that has since exited, so
`--list-types\=' says `image/png\=' long after the data stopped being
servable and the read comes back empty.  Treating that as \"no image\"
is what lets `p\=' stay a paste key instead of erroring."
  (when-let* ((type (noteworthy-image--available-type))
              (data (noteworthy-image--read (car type))))
    (cons data (cdr type))))

(defun noteworthy-image-in-clipboard-p ()
  "Non-nil when the clipboard holds an image that can actually be read."
  (and (noteworthy-image-grab) t))

;; ------------------------------------------------------------------
;; Writing it out

(defun noteworthy-image--target-dir ()
  "Absolute directory pasted images belong in, for the current buffer."
  (let ((base (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-directory)))
    (expand-file-name noteworthy-image-directory base)))

;;;###autoload
(defun noteworthy-image-paste ()
  "Save the clipboard image beside this file and insert an `#image()' call.
The file is named after a hash of its bytes, so pasting the same image
again reuses the file already there."
  (interactive)
  (let ((grabbed (noteworthy-image-grab)))
    (unless grabbed
      (user-error "No readable image in the clipboard"))
    (let ((data (car grabbed)))
      (let* ((name (format "%s.%s"
                           (substring (secure-hash 'sha256 data)
                                      0 noteworthy-image-hash-length)
                           (cdr grabbed)))
             (dir (noteworthy-image--target-dir))
             (path (expand-file-name name dir))
             (relative (concat (file-name-as-directory noteworthy-image-directory)
                               name))
             (existed (file-exists-p path)))
        (unless existed
          (make-directory dir t)
          ;; Binary, or Emacs helpfully mangles the bytes on the way to a
          ;; TRAMP host -- and a corrupted PNG only shows up much later, as
          ;; a compile error in someone else's preview.
          (let ((coding-system-for-write 'binary))
            (write-region data nil path nil 'silent)))
        (insert (format noteworthy-image-insert-format relative))
        (message "Noteworthy: %s %s (%s)"
                 (if existed "reused" "wrote")
                 relative
                 (file-size-human-readable (length data)))
        path))))

;;;###autoload
(defun noteworthy-image-paste-or-yank ()
  "Paste a clipboard image, else do whatever paste normally does.
Bound to `p' so the key keeps its meaning for ordinary text; only an
image in the clipboard diverts it."
  (interactive)
  (if (noteworthy-image-grab)
      (noteworthy-image-paste)
    (call-interactively
     (cond ((and (bound-and-true-p evil-mode) (fboundp 'evil-paste-after))
            #'evil-paste-after)
           (t #'yank)))))

(provide 'noteworthy-image)

;;; noteworthy-image.el ends here
