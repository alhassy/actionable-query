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
(declare-function aq-region-ctx-actions              "aq-state-region-ctx")
(declare-function actionable-query-mark-row          "aq-interaction-bulk")
(declare-function actionable-query-unmark-all        "aq-interaction-bulk")
(declare-function actionable-query-bulk-action-interactive "aq-interaction-bulk")
(declare-function aq-region-refresh                  "aq-render-splice")
(declare-function aq-region-filter                   "aq-render-splice")
(declare-function aq-region-popup                    "aq-render-splice")
(declare-function aq-region-row-up                   "aq-render-splice")
(declare-function aq-region-row-down                 "aq-render-splice")
(declare-function aq-region-edit-cell                "aq-render-splice")
(declare-function aq-region-toggle-heart             "aq-render-splice")
(declare-function aq-region-toggle-hearted-only      "aq-render-splice")
(declare-function aq-region-resurrect                "aq-render-splice")
(declare-function aq--ctx-at-point                   "aq-state-region-ctx")
(declare-function vtable-sort-by-current-column      "vtable")
(declare-function vtable-narrow-current-column       "vtable")
(declare-function vtable-widen-current-column        "vtable")
(declare-function vtable-previous-column             "vtable")
(declare-function vtable-next-column                 "vtable")
(declare-function aq-agenda-todo                     "agenda")
(declare-function aq-agenda-schedule                 "agenda")
(declare-function aq-agenda-deadline                 "agenda")
(declare-function aq-agenda-set-priority             "agenda")
(declare-function aq-agenda-set-tags                 "agenda")
(declare-function aq-agenda-set-effort               "agenda")
(declare-function aq-agenda-clock-in                 "agenda")
(declare-function aq-agenda-clock-out                "agenda")
(declare-function aq-agenda-archive                  "agenda")
(declare-function aq-agenda-kill-subtree             "agenda")

(defvar aq--vtable-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'actionable-query-refresh-current-view)
    map)
  "Vtable-scoped keymap: makes `g' refresh via actionable-query, not vtable.
Installed as `:keymap' on every `make-vtable' call emitted by
`actionable-query-defview'.  `vtable' sets `vtable-map' as its parent, so the
other default bindings (S, {, }, M-<left>, M-<right>) keep working.")

(declare-function aq-agenda--marker-or-create "agenda")

(defun aq-nav-goto-row-heading ()
  "Go to the Org tree behind the current row (RET / TAB).
If the row has no Org heading yet, one is created (via
`aq-agenda--marker-or-create', honouring the view's `:org-serializer')
and the row's marker updated --- so RET always lands you on a real tree,
no per-view RET binding needed.

Acts only on real data rows.  On a non-row line (a group heading, prose,
blank line) there is no `vtable-object', so rather than mint a spurious tree
we fold the section via `org-cycle' --- restoring the expected TAB behaviour."
  (interactive)
  (if (not (and (fboundp 'vtable-current-object) (vtable-current-object)))
      ;; Not on a data row: behave like TAB (fold the section).
      (when (fboundp 'org-cycle) (org-cycle))
   (let ((m (if (fboundp 'aq-agenda--marker-or-create)
               (aq-agenda--marker-or-create)
             ;; Fallback if agenda.el isn't loaded: marker from the row only.
             (when-let* ((obj (and (fboundp 'vtable-current-object) (vtable-current-object)))
                         ((consp obj)) (mk (plist-get obj :marker)) ((markerp mk)))
               mk))))
    (if (and (markerp m) (marker-buffer m))
        (progn (pop-to-buffer (marker-buffer m))
               ;; Widen if narrowed --- the heading may sit outside the active
               ;; restriction (e.g. a narrowed my-life.org), so jumping there
               ;; would otherwise land on hidden or wrong content.
               (when (buffer-narrowed-p) (widen))
               (goto-char m)
               (when (fboundp 'org-fold-show-context) (org-fold-show-context))
               (recenter))
      (message "No Org entry for this row")))))

(defvar-local aq--resurrect-fn nil
  "Buffer-local closure that un-snoozes this view and re-renders.
Set by `actionable-query-defview'; invoked by `actionable-query-resurrect-current-view'
so `R' works buffer-wide, not just on a vtable row.")

(defun actionable-query-resurrect-current-view ()
  "Resurrect snoozed items in the current actionable-query view (buffer-wide `R')."
  (interactive)
  (if aq--resurrect-fn
      (funcall aq--resurrect-fn)
    (user-error "No actionable-query resurrect fn in this buffer")))

(defvar aq--standalone-nav-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"          #'next-line)
    (define-key map "p"          #'previous-line)
    (define-key map (kbd "RET")  #'aq-nav-goto-row-heading)
    (define-key map (kbd "TAB")  #'aq-nav-goto-row-heading)
    (define-key map "g"          #'actionable-query-refresh-current-view)
    (define-key map "R"          #'actionable-query-resurrect-current-view)
    (define-key map "q"          #'quit-window)
    map)
  "Base nav keymap for standalone actionable-query view buffers.
Replaces the navigation `org-agenda-mode' used to supply (RET/TAB/n/p/q/g/R)
now that standalone views render in read-only `org-mode' instead of
`org-agenda-mode'.  `g'/`R' dispatch through buffer-local `aq--refresh-fn'/
`aq--resurrect-fn', so they work buffer-wide --- not only on a vtable row.
Installed as the local map's parent in `aq--enter-view-mode'.")

(defun aq--region-dispatcher (cmd)
  "Wrap CMD so it runs (via `call-interactively', so prefix args flow) only
inside a spliced region; outside one it signals a clear `user-error'.
Used to build `aq--region-keymap' from `aq--region-key-table'."
  (lambda ()
    (interactive)
    (if (aq--ctx-at-point)
        (call-interactively cmd)
      (user-error "Not inside a spliced view"))))

(defconst aq--region-key-table
  '(;; view / structural --- already region-aware or point-local
    ("g"          . aq-region-refresh)
    ("="          . aq-region-filter)
    ("?"          . aq-region-popup)
    ("m"          . actionable-query-mark-row)
    ("U"          . actionable-query-unmark-all)
    ("B"          . actionable-query-bulk-action-interactive)
    ("M-<up>"     . aq-region-row-up)
    ("M-<down>"   . aq-region-row-down)
    ("S"          . vtable-sort-by-current-column)
    ("{"          . vtable-narrow-current-column)
    ("}"          . vtable-widen-current-column)
    ("M-<left>"   . vtable-previous-column)
    ("M-<right>"  . vtable-next-column)
    ;; org-agenda suite --- point-local via the row's marker (created on demand)
    ("RET"        . aq-nav-goto-row-heading)
    ("TAB"        . aq-nav-goto-row-heading)
    ("t"          . aq-agenda-todo)
    ("C-c C-s"    . aq-agenda-schedule)
    ("C-c C-d"    . aq-agenda-deadline)
    (","          . aq-agenda-set-priority)
    (":"          . aq-agenda-set-tags)
    ("E"          . aq-agenda-set-effort)
    ("I"          . aq-agenda-clock-in)
    ("O"          . aq-agenda-clock-out)
    ("$"          . aq-agenda-archive)
    ("C-k"        . aq-agenda-kill-subtree)
    ;; per-region state via `aq-region-ctx'
    ("e"          . aq-region-edit-cell)
    ("h"          . aq-region-toggle-heart)
    ("H"          . aq-region-toggle-hearted-only)
    ("R"          . aq-region-resurrect))
  "Alist (KEY . COMMAND) of the full key suite available inside a splice region.
Mirrors the `?'-popup inventory.  `q'/`Q' are deliberately absent --- they
would quit-window or erase the *host* buffer.  A view's own `:actions' keys
need no entry here: they ride a higher-priority keymap composed at splice
time (see `aq--actions->region-keymap'), so user-defined keys get region
support automatically.")

(defconst aq--region-key-aliases
  '(("RET" "<return>") ("TAB" "<tab>"))
  "Extra event spellings to bind for a `aq--region-key-table' key.
A GUI Emacs delivers `<return>'/`<tab>', which org-mode binds in its own
local map; Emacs only collapses them to `RET'/`TAB' when the event form is
unbound.  Claiming both spellings keeps RET/TAB region-scoped in a host
buffer.")

(defvar aq--region-keymap
  (let ((map (make-sparse-keymap)))
    (dolist (pair aq--region-key-table map)
      (keymap-set map (car pair) (aq--region-dispatcher (cdr pair)))
      (dolist (alias (cdr (assoc (car pair) aq--region-key-aliases)))
        (keymap-set map alias (aq--region-dispatcher (cdr pair))))))
  "Keymap layered onto every spliced region as a `keymap' text-property.
Built from `aq--region-key-table'.  This is a *secondary* path: org-mode
fontification strips the `keymap' text-property on the first redisplay, so
the same keys are *also* installed via `aq--install-host-standard-keys' in a
buffer-local `minor-mode-overriding-map-alist' entry, which fontification
cannot touch.  Both resolve the same commands region-scoped (see
`aq--region-standard-dispatch').")

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

(defvar-local aq--host-action-keys nil
  "All keys ever registered by `aq--install-host-action-keys' in this buffer.
A union across every spliced view, so the one shared dispatch map (below)
recognises a key regardless of which view introduced it.")

(defun aq--host-action-dispatch (key)
  "Run the action bound to KEY for whichever view's region point is in.
Reads `aq--region-ctx' at point to find the live ACTIONS list for the
splice under point — so one shared keymap correctly serves several
co-existing spliced views, instead of the last-installed view's
actions permanently shadowing everyone else's."
  (let* ((ctx     (get-text-property (point) 'aq--region-ctx))
         (actions (and ctx (aq-region-ctx-actions ctx)))
         (triple  (and actions (assoc key actions)))
         (fn      (and triple (caddr triple)))
         (obj     (and fn (vtable-current-object))))
    (if (and fn obj)
        (funcall fn obj)
      (let ((cmd (let ((aq--host-actions-active nil))
                   (key-binding (kbd key) t))))
        (if (commandp cmd)
            (call-interactively cmd)
          (user-error "No binding for %s outside a spliced view" key))))))

(defun aq--install-host-action-keys (actions)
  "Register ACTIONS' keys as buffer-local bindings in the host buffer.
Each key dispatches via `aq--host-action-dispatch', which resolves the
correct view's action from `aq-region-ctx' at point — so multiple
spliced views co-existing in one buffer each get their own bindings,
rather than the most recently installed view's actions overwriting
the others.  Installed (once) via `minor-mode-overriding-map-alist' so
the bindings sit above regular minor-mode keymaps — robust against
packages like `electric-indent-mode' that would otherwise intercept
keys like RET."
  (let ((new-keys (cl-loop for (key _desc _fn) in actions
                           when (and (key-valid-p key) (not (member key aq--host-action-keys)))
                           collect key)))
    (when new-keys
      (setq aq--host-action-keys (append new-keys aq--host-action-keys))
      (let ((map (make-sparse-keymap)))
        (dolist (key aq--host-action-keys)
          (let ((k key))
            (keymap-set map k (lambda () (interactive) (aq--host-action-dispatch k)))
            ;; Bind the GUI `<return>'/`<tab>' spellings too (see
            ;; `aq--region-key-aliases'), so a view's RET/TAB action isn't
            ;; shadowed by org-mode's own `<return>' binding in a host buffer.
            (dolist (alias (cdr (assoc k aq--region-key-aliases)))
              (keymap-set map alias (lambda () (interactive) (aq--host-action-dispatch k))))))
        (if aq--host-actions-active
            (setcdr (assq 'aq--host-actions-active minor-mode-overriding-map-alist) map)
          (setq-local aq--host-actions-active t)
          (push (cons 'aq--host-actions-active map)
                minor-mode-overriding-map-alist))))))

;; ── Standard region keys via the host override-map ──────────────────────────
;;
;; The text-property `aq--region-keymap' is fragile: `org-mode' fontification
;; strips the `keymap' property off the spliced span on the first redisplay, so
;; bindings carried only there silently die.  The view-`:actions' path above
;; sidesteps this by living in `minor-mode-overriding-map-alist', which
;; fontification cannot touch.  We give the *standard* key suite the same
;; treatment: one buffer-local override map whose keys dispatch by reading
;; `aq--ctx-at-point' --- fire inside a region, fall through to the host's own
;; binding (self-insert in prose) outside one.

(defvar-local aq--host-standard-active nil
  "Non-nil once `aq--install-host-standard-keys' has armed this buffer.
Buffer-local; the alist key in `minor-mode-overriding-map-alist'.")

(defun aq--region-standard-dispatch (key cmd)
  "Run CMD for KEY when point is inside a spliced region, else fall through.
Outside a region, re-resolve and invoke the host's own binding for KEY so
the keystroke behaves normally (e.g. self-inserts in surrounding prose).
`last-command-event' is rebound to the key's final event before the
fall-through, so self-inserting commands insert the right character ---
under programmatic dispatch it would otherwise be the synthetic event."
  (if (aq--ctx-at-point)
      (call-interactively cmd)
    (let* ((seq  (kbd key))
           (host (let ((aq--host-standard-active nil))
                   (key-binding seq t))))
      (if (commandp host)
          (let ((last-command-event (aref seq (1- (length seq)))))
            (call-interactively host))
        (let ((last-command-event (aref seq (1- (length seq)))))
          (call-interactively #'self-insert-command))))))

(defun aq--install-host-standard-keys ()
  "Install the standard region key suite as a buffer-local override map.
Idempotent per host buffer.  Mirrors `aq--install-host-action-keys' but for
the view-independent `aq--region-key-table'; both maps coexist in
`minor-mode-overriding-map-alist'."
  (unless aq--host-standard-active
    (setq-local aq--host-standard-active t)
    (let ((map (make-sparse-keymap)))
      (dolist (pair aq--region-key-table)
        (let ((k (car pair)) (cmd (cdr pair)))
          (when (key-valid-p k)
            (let ((binding (lambda () (interactive)
                             (aq--region-standard-dispatch k cmd))))
              (keymap-set map k binding)
              ;; A GUI sends `<return>'/`<tab>', not the TTY `RET'/`TAB'; org
              ;; binds those event forms in its local map, so they shadow us
              ;; unless we claim them explicitly (Emacs only falls
              ;; `<return>'→`RET' when `<return>' is unbound).
              (dolist (alias (cdr (assoc k aq--region-key-aliases)))
                (keymap-set map alias binding))))))
      (push (cons 'aq--host-standard-active map)
            minor-mode-overriding-map-alist))))

(provide 'aq-interaction-keys)
;;; aq-interaction-keys.el ends here
