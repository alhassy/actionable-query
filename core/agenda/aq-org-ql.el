;;; aq-org-ql.el --- :org-ql preset: actionable-query views over org-ql queries  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Registers `:org-ql' as a `actionable-query-defview-def-keyword' preset, in
;; similar spirit to `:gerrit-query'/`:jira-query' in org-agenda-gerrit.el ---
;; one keyword, defaulting `:objects'/`:columns'/`:org'/`:actions' so a view
;; over local org files is a one-liner:
;;
;;   (actionable-query-defview my-urgent-todos "🔥 Urgent TODOs"
;;     :org-ql '(and (todo) (priority "A")))
;;
;; QUERY-FORM is an unevaluated `org-ql' query sexp, matched against
;; `(org-agenda-files)' by default (override with `:org-ql-files').
;; Each match becomes a plist carrying heading/todo/priority/tags/
;; deadline/scheduled plus the `org-marker' itself, so `:org' (RET in
;; the `*-Org QL View-*' buffer's prose) and the default `RET' action
;; (jump to heading) both work out of the box.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-ql)

(defun aq--org-ql-timestamp (element prop)
  "Return the raw-value string of ELEMENT's PROP timestamp, or nil."
  (when-let ((ts (org-element-property prop element)))
    (org-element-property :raw-value ts)))

(defun aq--org-ql-item (element)
  "Convert an `org-ql' ELEMENT (fetched via :action \\='element-with-markers) to a plist."
  (let ((marker (org-element-property :org-marker element)))
    (list :marker     marker
          :heading    (or (org-element-property :raw-value element) "")
          :todo       (org-element-property :todo-keyword element)
          :priority   (org-element-property :priority element)
          :tags       (org-element-property :tags element)
          :deadline   (aq--org-ql-timestamp element :deadline)
          :scheduled  (aq--org-ql-timestamp element :scheduled)
          :closed     (aq--org-ql-timestamp element :closed)
          :created    (and marker (org-entry-get marker "CREATED"))
          :file       (and marker (buffer-file-name (marker-buffer marker))))))

(defun aq--org-ql-fetch (query files)
  "Run QUERY (an unevaluated org-ql sexp) over FILES; return a list of plists.
Synchronous --- org-ql queries scan local org files already held in Emacs's
buffers/disk, so there is nothing to fetch over the network."
  (mapcar #'aq--org-ql-item
          (org-ql-select files query :action 'element-with-markers)))

(defvar aq-org-ql-columns
  (list
   (list :name "Todo" :width 6 :align 'center
         :getter (lambda (o &rest _) (or (plist-get o :todo) ""))
         :formatter (lambda (v &rest _)
                      (propertize v 'face (if (member v '("DONE" "CANCELLED"))
                                               '(:foreground "gray60")
                                             '(:foreground "orange red" :weight bold)))))
   (list :name "Pri" :width 4 :align 'center
         :getter (lambda (o &rest _) (if-let ((p (plist-get o :priority))) (char-to-string p) "")))
   (list :name "Heading" :width 55
         :getter (lambda (o &rest _) (plist-get o :heading)))
   (list :name "Deadline/Scheduled" :width 14
         :getter (lambda (o &rest _) (or (plist-get o :deadline) (plist-get o :scheduled) ""))
         :displayer (lambda (v w _) (propertize (truncate-string-to-width v w)
                                            'face '(:height 0.8 :foreground "gray50"))))
   (list :name "Tags" :width 20
         :getter (lambda (o &rest _) (string-join (plist-get o :tags) " "))))
  "Standard vtable column specs for `:org-ql' views.")

(defun aq--org-ql-jump (o)
  "Jump to ITEM O's heading in its file, in the other window."
  (if-let ((marker (plist-get o :marker)))
      (if (buffer-live-p (marker-buffer marker))
          (progn (pop-to-buffer (marker-buffer marker))
                 (goto-char marker)
                 (org-fold-show-context)
                 (recenter))
        (user-error "Buffer for this heading no longer exists --- file may have been reverted"))
    (user-error "No marker for this item")))

(defvar aq-org-ql-actions
  (list
   (list "RET" "Jump to heading" #'aq--org-ql-jump)
   (list "w"   "Copy heading to kill-ring"
         (lambda (o)
           (let ((s (or (plist-get o :heading) "")))
             (kill-new s)
             (message "Copied: %s" s))))
   (list "c"   "Capture as org TODO (linked back to original)"
         (lambda (o)
           (let* ((marker (plist-get o :marker))
                  (heading (or (plist-get o :heading) ""))
                  (link (if (and marker (buffer-live-p (marker-buffer marker)))
                            (org-with-point-at marker (org-store-link nil))
                          heading)))
             (org-capture-string (format "* TODO %s" link) "t")))))
  "Standard actions for `:org-ql' views.")

(actionable-query-defview-def-keyword :org-ql (query-form)
  "Default the common kwargs of an `org-ql'-sourced view.
QUERY-FORM is an org-ql query sexp (unevaluated, e.g. \\='(and (todo)
(deadline :to today))), matched against `(org-agenda-files)' at
view-open time.
Overridden by any explicit kwarg at the `actionable-query-defview' call site."
  `(:columns   aq-org-ql-columns
    :objects   (lambda () (aq--org-ql-fetch ',query-form (org-agenda-files)))
    :org-deserializer       (lambda (o) (plist-get o :marker))
    :actions   aq-org-ql-actions))

;; NB: the file is `org-ql.el' but the *feature* is `actionable-query-org-ql'
;; --- `org-ql' is the third-party query library, so providing that symbol
;; here would shadow it and break every `(require 'org-ql)'.
(provide 'actionable-query-org-ql)
;;; aq-org-ql.el ends here
