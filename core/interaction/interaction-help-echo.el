;;; interaction-help-echo.el --- Help-echo + org-marker post-command hooks  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Two complementary post-command hooks that decorate the row at point:
;;
;;   • `aq--install-help-echo' — calls a user-supplied function on the
;;     current vtable object and centers its return value in the echo
;;     area.  Suppressed transiently by `aq--message' via
;;     `aq--suppress-help-echo-until' so confirmation messages are not
;;     immediately overwritten.
;;
;;   • `aq--install-org-marker' — calls a user-supplied function on the
;;     current vtable object expecting an Org marker; sets it as a
;;     text-property on the line so `org-agenda-mode-map' bindings
;;     (RET, TAB, I, o) can navigate to the linked heading.
;;
;; Plus the `actionable-query-resolve-org-markers' helper for views
;; that want to bulk-resolve external IDs (e.g., Jira tickets) to
;; Org headings in the user's notes file.

;;; Code:

(require 'state-region-ctx)    ; `aq--suppress-help-echo-until'

(declare-function vtable-current-object "vtable")

;;; ─── help-echo ──────────────────────────────────────────────────────────────

(defun aq--center-message (str)
  "Fill STR to the frame width and center each line, for display via `message'.
Existing newlines in STR are honoured as hard breaks: each line is wrapped and
centered independently, so a multi-part help-echo stays multi-line instead of
collapsing into one reflowed paragraph."
  (let ((width (frame-width)))
    (mapconcat
     (lambda (line)
       (with-temp-buffer
         (insert (string-trim line))
         (let ((fill-column width)) (fill-region (point-min) (point-max)))
         (center-region (point-min) (point-max))
         (string-trim-right (buffer-string) "\n")))
     (split-string str "\n")
     "\n")))

(defvar-local aq--last-help-echo-obj nil
  "Most-recently messaged vtable object; guards against redundant `aq--center-message' calls.")

(defun aq--install-help-echo (help-echo-fn)
  "Install a post-command hook that messages HELP-ECHO-FN on the current row.
HELP-ECHO-FN receives the raw vtable object and should return a string.
Suppressed while `aq--suppress-help-echo-until' is in the future."
  (add-hook 'post-command-hook
            (lambda ()
              (when (> (float-time) aq--suppress-help-echo-until)
                (when-let ((obj (vtable-current-object)))
                  (unless (eq obj aq--last-help-echo-obj)
                    (setq aq--last-help-echo-obj obj)
                    (when-let ((msg (funcall help-echo-fn obj)))
                      (message "%s" (aq--center-message msg)))))))
            nil :local))

;;; ─── org-marker ─────────────────────────────────────────────────────────────

(defvar-local aq--last-org-marker-obj nil
  "Most-recently processed vtable object for org-marker; skips redundant `funcall org-fn' calls.")

(defun aq--install-org-marker (org-fn)
  "Install a post-command hook that sets org-marker on the current vtable row.
ORG-FN is (lambda (it) …) returning an org marker or nil.  When a live
marker is returned, `org-marker' and `org-hd-marker' are set as text
properties on the current line — enabling standard `org-agenda-mode-map'
navigation (RET, TAB, I, o) to jump to the linked Org heading."
  (add-hook 'post-command-hook
            (lambda ()
              (when-let ((obj (vtable-current-object)))
                (unless (eq obj aq--last-org-marker-obj)
                  (setq aq--last-org-marker-obj obj)
                  (when-let* ((marker (funcall org-fn obj))
                              ((and (markerp marker) (marker-buffer marker))))
                    (let ((inhibit-read-only t))
                      (put-text-property (line-beginning-position)
                                         (line-end-position)
                                         'org-marker marker)
                      (put-text-property (line-beginning-position)
                                         (line-end-position)
                                         'org-hd-marker marker))))))
            nil :local))

;;;###autoload
(defun actionable-query-resolve-org-markers (objects key-fn &optional fallback-fn)
  "Return a hash-table mapping each object in OBJECTS to an Org marker.
KEY-FN is called with each object and should return a string (e.g. a
Jira ticket ID) to search for as a substring of any heading in
`org-default-notes-file'.  Pass 1 searches for the first heading
whose text contains that key.  FALLBACK-FN, if non-nil, is called
with (object table) for every object still unmapped after Pass 1 —
it may insert additional entries by calling
`(puthash object marker table)'."
  (let ((table (make-hash-table :test 'eq)))
    ;; Pass 1 — key-based heading search.
    (when (and org-default-notes-file
               (file-exists-p org-default-notes-file))
      (with-current-buffer (find-file-noselect org-default-notes-file)
        (save-excursion
          (dolist (obj objects)
            (let ((key (funcall key-fn obj)))
              (when (stringp key)
                (goto-char (point-min))
                (when (re-search-forward
                       (concat "^\\*+ .*" (regexp-quote key)) nil t)
                  (org-back-to-heading t)
                  (puthash obj (point-marker) table))))))))
    ;; Pass 2 — caller-supplied fallback for unresolved objects.
    (when fallback-fn
      (dolist (obj objects)
        (unless (gethash obj table)
          (funcall fallback-fn obj table))))
    table))

(provide 'interaction-help-echo)
;;; interaction-help-echo.el ends here
