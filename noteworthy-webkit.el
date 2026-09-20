;;; noteworthy-webkit.el --- Make WebKitGTK paint a typst preview right  -*- lexical-binding: t; -*-

;;; Commentary:

;; One paint bug, fixed once, for every way a typst preview reaches an
;; xwidget: `typst-preview-mode', plain Noteworthy, and Noteworthy collab.
;;
;; With tinymist's partial rendering on, a page outside the viewport is not
;; drawn as SVG but as a `<foreignObject class="typst-svg-mixin-canvas">'
;; holding a `<canvas>', inside that page's transformed `<g>'.  WebKitGTK
;; paints such a foreignObject ignoring the ancestor transform -- at the SVG
;; origin, at the canvas's own size.  Every far page therefore paints over the
;; first one, and the last painted wins: the document's last page, oversized,
;; at the top of the preview.  The placeholders render lazily, which is why
;; it arrives a few seconds after the page does.  Chrome and Firefox apply the
;; transform and never show it, on the same page from the same server.
;;
;; The DOM is correct throughout; only the paint is wrong.  So the fix is to
;; keep those placeholders out of the paint, which costs a far page its
;; low-resolution stand-in until it scrolls into view -- nothing anyone will
;; see.  Verified in a headless WebKitGTK view against two alternatives that
;; keep them visible (a compositing layer on the canvas, `will-change' on the
;; groups); neither helps.
;;
;; Emacs 31 has no user-stylesheet API for xwidgets, so the rule goes into the
;; page as it loads.  `xwidget-webkit-callback' is the one place every load
;; passes through, whichever package asked for it, so the advice lives there
;; and nothing needs a timer.  It looks for the viewer's own container before
;; touching a page, so a browser you open for anything else is left alone.

;;; Code:

(require 'xwidget nil t)

(defgroup noteworthy-webkit nil
  "Fixes for typst previews in an Emacs xwidget."
  :group 'noteworthy)

(defcustom noteworthy-webkit-fix-enabled t
  "Whether to hide partial rendering's canvas placeholders in xwidget previews.
See `noteworthy-webkit-fix-css' for what and why."
  :type 'boolean
  :group 'noteworthy-webkit)

(defconst noteworthy-webkit-fix-css
  "foreignObject.typst-svg-mixin-canvas { visibility: hidden; }"
  "The rule that keeps WebKitGTK from painting far pages over the first.")

(defconst noteworthy-webkit--inject-script
  (format "(function(){\
if(!document.getElementById('typst-app'))return 'not-typst';\
if(document.getElementById('nw-webkit-fix'))return 'present';\
var s=document.createElement('style');s.id='nw-webkit-fix';\
s.textContent=%S;document.head.appendChild(s);return 'injected';})();"
          noteworthy-webkit-fix-css)
  "Add the rule once, and only to a page that is a typst preview.")

(defun noteworthy-webkit-inject-fix (&optional xwidget)
  "Put `noteworthy-webkit-fix-css' into XWIDGET's page, if it is a typst preview.
XWIDGET defaults to the current buffer's session.  Idempotent; a page that
is not the viewer is left untouched."
  (interactive)
  (when-let* ((xw (or xwidget (ignore-errors (xwidget-webkit-current-session)))))
    (ignore-errors
      (xwidget-webkit-execute-script xw noteworthy-webkit--inject-script))))

(defun noteworthy-webkit-inject-fix-everywhere ()
  "Apply the fix to every displayed xwidget-webkit buffer.
For a preview that was open before this file was loaded."
  (interactive)
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "xwidget-webkit" (buffer-name buf)))
      (with-current-buffer buf
        (noteworthy-webkit-inject-fix)))))

(defun noteworthy-webkit--on-load (xwidget event-type)
  "After `xwidget-webkit-callback': inject the fix once a page has loaded.
Emacs reports both progress and completion as `load-changed'; the fourth
element of `last-input-event' says which, and only the finish has a
document to add a rule to."
  (when (and noteworthy-webkit-fix-enabled
             (eq event-type 'load-changed)
             (equal (ignore-errors (nth 3 last-input-event)) "load-finished")
             (buffer-live-p (xwidget-buffer xwidget)))
    (noteworthy-webkit-inject-fix xwidget)))

;;;###autoload
(defun noteworthy-webkit-setup ()
  "Install the fix for every xwidget page load from now on."
  (interactive)
  (when (fboundp 'xwidget-webkit-callback)
    (advice-add 'xwidget-webkit-callback :after #'noteworthy-webkit--on-load)))

(defun noteworthy-webkit-teardown ()
  "Stop injecting the fix into new page loads."
  (interactive)
  (advice-remove 'xwidget-webkit-callback #'noteworthy-webkit--on-load))

;; Installed on load: the whole point is not needing to be asked, by any of
;; the three packages that open a preview.
(noteworthy-webkit-setup)

(provide 'noteworthy-webkit)
;;; noteworthy-webkit.el ends here
