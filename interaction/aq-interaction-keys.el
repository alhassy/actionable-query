;;; aq-interaction-keys.el --- Centralised keymaps + action→keymap helpers  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; All actionable-query key dispatch lives here:
;;
;;   • `aq--vtable-keymap' — installed as `:keymap' on every
;;     `make-vtable' call.  Re-binds `g' to actionable-query's refresh
;;     fn so it goes through `aq--refresh-fn' (and thus the spinner /
;;     async-fetch path) rather than vtable's own `vtable-revert'.
;;
;;   • `aq--region-keymap' — text-property keymap on spliced regions.
;;     Carries `g/=/m/U/B/?/M-up/M-down' so a view spliced into a host
;;     buffer reacts to the same keys as a dedicated buffer.
;;
;;   • `aq--actions->vtable' / `aq--actions->region-keymap' — convert
;;     a view's ACTIONS triples (key desc fn) into the formats vtable
;;     and the region-keymap each expect.
;;
;;   • `aq--install-host-action-keys' — installs ACTIONS as buffer-
;;     local bindings via `minor-mode-overriding-map-alist' so they
;;     beat regular minor-mode keymaps (notably `electric-indent-mode'
;;     intercepting RET).

;;; Code:

(require 'aq-state-cache)         ; `actionable-query-refresh-current-view'

(declare-function vtable-current-object              "vtable")
(declare-function actionable-query-mark-row          "aq-interaction-bulk")
(declare-function actionable-query-unmark-all        "aq-interaction-bulk")
(declare-function actionable-query-bulk-action-interactive "aq-interaction-bulk")
(declare-function aq-region-refresh                  "aq-render-splice")
(declare-function aq-region-filter                   "aq-render-splice")
(declare-function aq-region-popup                    "aq-render-splice")
(declare-function aq-region-row-up                   "aq-render-splice")
(declare-function aq-region-row-down                 "aq-render-splice")

(defvar aq--vtable-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'actionable-query-refresh-current-view)
    map)
  "Vtable-scoped keymap: makes `g' refresh via actionable-query, not vtable.
Installed as `:keymap' on every `make-vtable' call emitted by
`actionable-query-defview'.  `vtable' sets `vtable-map' as its parent, so the
other default bindings (S, {, }, M-<left>, M-<right>) keep working.")

(defvar aq--region-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")        #'aq-region-refresh)
    (define-key map (kbd "=")        #'aq-region-filter)
    (define-key map (kbd "m")        #'actionable-query-mark-row)
    (define-key map (kbd "U")        #'actionable-query-unmark-all)
    (define-key map (kbd "B")        #'actionable-query-bulk-action-interactive)
    (define-key map (kbd "M-<up>")   #'aq-region-row-up)
    (define-key map (kbd "M-<down>") #'aq-region-row-down)
    (define-key map (kbd "?")        #'aq-region-popup)
    map)
  "Keymap installed on every spliced view region — composed with vtable's
own keymap (which drives `:actions').  Bindings here only take effect when
point sits inside a splice, because the keymap is attached as a `keymap'
text-property over the region.")

(defvar-local aq--host-actions-active nil
  "Non-nil once `aq--install-host-action-keys' has armed this buffer.
Buffer-local; used as the alist key in `minor-mode-overriding-map-alist'.")

(defun aq--actions->vtable (actions)
  "Convert ACTIONS triples (key desc fn) to a flat vtable :actions list (key fn …).
Entries whose key fails `key-valid-p' are silently omitted — they still appear
in the `?' transient popup, but vtable cannot bind multi-char sequences."
  (cl-loop for (key _desc fn) in actions
           when (key-valid-p key)
           append (list key fn)))

(defun aq--actions->region-keymap (actions)
  "Build a sparse keymap from ACTIONS triples (key desc fn) for use in spliced regions.
Each binding reads the object under point via `vtable-current-object' and calls FN —
mirroring vtable's own action dispatch, but applied directly in the host buffer."
  (let ((map (make-sparse-keymap)))
    (dolist (triple actions)
      (let ((key (car triple))
            (fn  (caddr triple)))
        (when (key-valid-p key)
          (keymap-set map key
                      (let ((f fn))
                        (lambda ()
                          (interactive)
                          (funcall f (vtable-current-object))))))))
    map))

(defun aq--install-host-action-keys (actions)
  "Install ACTIONS keys as buffer-local bindings in the host buffer.
Each key dispatches to the matching action when point is inside a spliced
region, and falls through to the buffer's normal binding otherwise.
Uses `minor-mode-overriding-map-alist' so the binding sits above regular
minor-mode keymaps — robust against packages like `electric-indent-mode'
that would otherwise intercept keys like RET."
  (let ((map (make-sparse-keymap)))
    (dolist (triple actions)
      (let ((key (car triple))
            (fn  (caddr triple)))
        (when (key-valid-p key)
          (let ((f fn) (k key))
            (keymap-set map key
                        (lambda ()
                          (interactive)
                          (let ((obj (vtable-current-object)))
                            (if obj
                                (funcall f obj)
                              (let* ((keys (kbd k))
                                     (cmd  (let ((aq--host-actions-active nil))
                                             (key-binding keys t))))
                                (if (commandp cmd)
                                    (call-interactively cmd)
                                  (user-error "No binding for %s outside a spliced view" k)))))))))))
    (setq-local aq--host-actions-active t)
    (push (cons 'aq--host-actions-active map)
          minor-mode-overriding-map-alist)))

(provide 'aq-interaction-keys)
;;; aq-interaction-keys.el ends here
