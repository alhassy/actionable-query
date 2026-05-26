;;; actionable-query.el --- Turn any query into an interactive, actionable view  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy
;; Version: 0.3

;;; Commentary:

;; `actionable-query' turns any data source — shell output, HTTP
;; responses, bookmark lists, curated static lists, async feeds,
;; JSON payloads, you name it — into an interactive, actionable
;; buffer view.  Think of it as `dired' for files and `proced' for
;; processes, but generalised: define a query, render its results,
;; and bind your own actions to RET and hover.
;;
;; We reuse `org-agenda-mode' as a rendering substrate so RET
;; visits, `g' refreshes, `q' quits, and marker-based navigation
;; all behave the way Emacs users already expect.  You never need
;; to think about org-agenda while using this package — that's
;; purely an implementation detail.
;;
;; See README.org for a tour of worked examples (RSS feeds, YouTube
;; favourites, git-log standups, `gh search' CLI output, bookmark
;; dashboards, JSON+hierarchy.el, Gnus, gcalcli).
;;
;; Usage:
;;
;;   (defquery
;;     :title "My awesome section"
;;     :items (--map (format "Item %s" it) (number-sequence 1 3))
;;     :on-return (message-box "You're looking at %s" it)
;;     :remove-item-on-return t
;;     :on-hover (propertize (format "Are you really going to do %s today?" it) 'face 'bold))
;;
;; Full keyword reference:
;;
;;   :title STRING
;;       Section heading rendered with `org-agenda-structure' face. Required.
;;
;;   :items FORM
;;       Evaluated at agenda build time; returns a list of items.
;;       Items may be plain strings or arbitrary structs — see :item-to-string.
;;
;;   :items-async (lambda (callback) ASYNC-BODY)
;;       Alternative to :items for async retrieval. The lambda is called with a
;;       CALLBACK of type (items → unit); call (funcall callback items) when
;;       results are ready. The section shows "⏳ Loading…" until results land,
;;       then redraws the agenda automatically. Results are cached per calendar
;;       day; call `actionable-query-invalidate-cache' to force a refresh.
;;
;;   :item-to-string FORM
;;       `it' is bound to the raw item; FORM returns the display string.
;;       Default: identity (assumes items are already strings).
;;
;;   :on-return FORM
;;       Evaluated when the user presses RET on an item. `it' = raw item.
;;
;;   :on-hover FORM
;;       Evaluated to produce the help-echo tooltip string. `it' = raw item.
;;
;;   :org FORM
;;       `it' = raw item; FORM returns an Org marker or nil.
;;       When a live marker is returned, `org-marker' and `org-hd-marker' are
;;       set on the line so that standard org-agenda commands (RET, TAB, I, o)
;;       navigate to the linked Org heading.
;;
;;   :remove-item-on-return BOOL
;;       When non-nil, dismiss the item for the rest of today on RET.
;;
;;   :show-if FORM
;;       Predicate evaluated at render time (no `it' binding — section-level).
;;       The section is skipped entirely when FORM returns nil.
;;       Default: t (always shown).
;;
;;   :view STRING-OR-LIST
;;       Name (or list of names) of the view(s) this section belongs to.
;;       Absent ⇒ the section is *universal* and renders in every agenda
;;       buffer, including ones opened outside any named view — this is
;;       the correct choice for toolbars and other cross-view chrome.
;;       When present, the section only renders when the active view
;;       (see `actionable-query-open-view') matches.  A list-valued
;;       `:view' matches any member.
;;
;; Named views:
;;
;;   A "view" is a named grouping of sections, registered via
;;   `actionable-query-define-view' and opened via
;;   `actionable-query-open-view'.  Two buffer models are supported:
;;
;;   Attach mode (`:buffer' nil)
;;       The view's `:open' thunk does what it wants — typically calls
;;       `org-agenda' with a custom-commands key — and our finalize
;;       hook renders matching sections into whatever buffer results.
;;       Use when the view attaches to an existing agenda view
;;       (e.g. wrapping the daily `C-c a' dashboard).
;;
;;   Dedicated mode (`:buffer' non-nil STRING)
;;       The library creates/reuses a buffer with that name, installs
;;       `org-agenda-mode' (so RET/TAB/t/I/o/q work), runs `:open'
;;       inside it, then calls `(org-agenda-finalize)' to render the
;;       matching sections.  `g'/`r' refresh by re-running
;;       `actionable-query-open-view' on the same name.
;;       Use when the view is a standalone dashboard with no
;;       corresponding `org-agenda-custom-commands' entry (e.g.
;;       curl-fed Jira tickets, a transient "just show me reviews
;;       needed" slice).
;;
;;   Example — attach mode:
;;
;;     (actionable-query-define-view
;;      :name "daily"
;;      :description "The daily focus dashboard (wraps C-c a)"
;;      :open (lambda () (org-agenda nil "a")))
;;
;;     (defquery
;;      :title "Inbox"
;;      :view  "daily"
;;      :items ...)
;;
;;     (actionable-query-open-view "daily")
;;
;;   Example — dedicated mode:
;;
;;     (actionable-query-define-view
;;      :name   "jira"
;;      :buffer "*Jira: my tickets*"
;;      :open   (lambda ()
;;                (insert "Fetched at " (format-time-string "%H:%M") "\n")))
;;
;;     (defquery
;;      :title "Needs review"
;;      :view  "jira"
;;      :items-async (lambda (cb) (my-jira-fetch cb)))
;;
;;     (actionable-query-open-view "jira")
;;
;;   :buttons FORM
;;       FORM is evaluated at render time (no `it' binding) and must
;;       return a list of button specs.  Each spec is a plist:
;;
;;         (:title STRING                   ; required
;;          :action URL-STRING-OR-NULLARY-FN ; required
;;          :location SYMBOL                 ; optional; default 'here
;;          :foreground-color STRING         ; optional; default cyan
;;          :background-color STRING         ; optional; default inherit
;;          :weight SYMBOL                   ; optional; default 'bold
;;          :slant SYMBOL                    ; optional; default 'italic
;;          :face PLIST-OR-SYMBOL            ; optional; escapes the pill style
;;          :echo STRING)                    ; optional; explicit tooltip
;;
;;       `:action' of type string → opened via `browse-url'.
;;       `:action' of type function → called with no arguments.
;;       `:location' is one of:
;;
;;         'here    Render beside the section title (default).
;;         'top     Render in the top button row; SKIP the section
;;                  header.  Useful for buttons that conceptually
;;                  belong to the whole agenda, not a single section.
;;                  Buttons declared `'top' across multiple sections
;;                  are deduped by (:title . :action) equality —
;;                  declare once, render once.
;;         'bottom  Same as 'top but at the end of the buffer.
;;
;;       `:foreground-color' and `:background-color' override the
;;       pill's fg/bg for a single button — handy for calling
;;       attention.  `:weight' and `:slant' override the pill's
;;       typography (defaults: bold italic).
;;
;;       `:face' is the escape hatch: when supplied, it REPLACES the
;;       pill-style face entirely (all other face-tweaking keys are
;;       ignored).  Pass either a face symbol (e.g. `:face 'warning'
;;       to reuse the theme's warning face) or a full face plist
;;       (e.g. `:face (:foreground "red" :weight bold)').  For the
;;       plist attribute vocabulary, see Info node `(elisp) Face
;;       Attributes' — the same attributes Emacs uses everywhere
;;       else faces appear.
;;
;;       `:echo' is the `help-echo' tooltip shown on hover.  When
;;       omitted, the tooltip is derived from `:action': URL strings
;;       show the URL; named functions show the first line of their
;;       docstring; anonymous lambdas get a generic fallback.  Supply
;;       `:echo' explicitly when you want a tooltip that's more
;;       descriptive than the docstring (or when the action is a
;;       lambda with no docstring to surface).
;;
;;       Unknown plist keys cause an error at load time (catches typos
;;       like :forground-color before they confuse you at render time).
;;
;;       Example:
;;         :buttons (list
;;                   (list :title "🔗 View In Browser"
;;                         :action "https://example.com/q/...")
;;                   (list :title "🔄 Refresh"
;;                         :action #'my-refresh-fn
;;                         :location 'top
;;                         :foreground-color "orange"
;;                         :echo "Re-fetch the data feeding this section."))

;;; Code:

(require 'cl-lib)
(require 'org-agenda)

;;; View identity
;;
;; A "view" is a named grouping of sections — e.g. "daily", "standup",
;; "morning brief".  Sections can declare membership via `:view' on
;; `defquery'; absent `:view' means the section is
;; *universal* (renders on every agenda buffer, as it did before views
;; existed).  Views are opened via `actionable-query-open-view', which
;; binds the dynamic variable and then dispatches to the view's `:open'
;; thunk.  The finalize hook stashes the active view name buffer-locally
;; so subsequent `org-agenda-redo' calls (`g' in the agenda) preserve
;; view identity — modelled on `org-ql-view-title' from org-ql-view.el.

(defvar-local actionable-query--view nil
  "Name of the view rendered in this agenda buffer, or nil.
Set by `actionable-query--insert-all' from the dynamic
`actionable-query--current-view' on first render; read on subsequent
renders so `org-agenda-redo' (e.g. pressing `g') keeps the same view
filter.  Buffer-local by `defvar-local'.")

(defvar actionable-query--current-view nil
  "Dynamic view name in force for the next finalize pass, or nil.
Bound by `actionable-query-open-view' around its `:open' thunk.  The
finalize hook promotes it to `actionable-query--view' (buffer-local)
so refresh works after the binding unwinds.")

(defvar actionable-query-views nil
  "Alist of named views: (NAME . PLIST).
Each PLIST may contain:
  :name STRING         required — matches the alist key, used for lookup
  :open FUNCTION       required — nullary thunk that opens the view
                                   (e.g. calls `org-agenda' with a key)
  :buffer STRING       optional — when non-nil, the view renders into a
                                   dedicated buffer with that name (future
                                   extension; nil for the common case)
  :description STRING  optional — shown in `completing-read' prompt")

(defconst actionable-query--view-valid-keys
  '(:name :open :buffer :description)
  "The only plist keys a view spec may carry.
Unknown keys cause `actionable-query-define-view' to error — catches
typos like `:opne' at declaration time.")

(defun actionable-query-define-view (&rest spec)
  "Register or update a named view.
SPEC is a plist with these keys:

  :name STRING         required — matches the alist key, used for lookup
  :open FUNCTION       required — nullary thunk that opens the view;
                                   e.g. `(lambda () (org-agenda nil \"a\"))'
                                   or `(lambda () (my-curl-then-finalize))'
  :buffer STRING       optional — target buffer name for a dedicated-buffer
                                   view; nil means the view attaches to
                                   whatever buffer `:open' produces
                                   (currently: attach-only mode is supported)
  :description STRING  optional — shown in the `completing-read' prompt

Upserts into `actionable-query-views' keyed by :name, replacing any
prior entry of the same name.  Returns the normalised plist."
  (let ((rest spec))
    (while rest
      (let ((k (car rest)))
        (unless (memq k actionable-query--view-valid-keys)
          (error "Unknown view key %S — valid keys: %S"
                 k actionable-query--view-valid-keys))
        (setq rest (cddr rest)))))
  (let ((name (plist-get spec :name))
        (open (plist-get spec :open)))
    (unless (stringp name)
      (error "View :name must be a string — got %S" name))
    (unless (functionp open)
      (error "View :open must be a function — got %S" open))
    (setf (alist-get name actionable-query-views nil nil #'equal) spec)
    spec))

(defvar actionable-query-view-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-agenda-mode-map)
    (define-key map "g" #'actionable-query--refresh-dedicated)
    (define-key map "r" #'actionable-query--refresh-dedicated)
    map)
  "Keymap for dedicated-buffer views.
Parents to `org-agenda-mode-map' so RET/TAB/t/I/o/q all behave as in a
regular agenda; overrides `g' and `r' to re-run the view opener rather
than `org-agenda-redo' (which would fail — there's no
`org-agenda-redo-command' in a dedicated buffer).")

(defun actionable-query--refresh-dedicated ()
  "Re-open the view rendered in this dedicated buffer.
Reads the buffer-local `actionable-query--view' and re-runs
`actionable-query-open-view' on it.  Bound to `g'/`r' in
`actionable-query-view-map'."
  (interactive)
  (let ((name actionable-query--view))
    (unless name
      (user-error "Not in a dedicated view buffer — no view name stashed"))
    (actionable-query-open-view name)))

(defun actionable-query--open-dedicated (view)
  "Render VIEW (a plist) into its dedicated `:buffer'.

Creates/reuses the buffer named by `:buffer', installs
`org-agenda-mode' (so RET/TAB/t/I/o/q work), erases prior content,
stashes the view name buffer-locally, runs `:open' for any preamble
insertion, then calls `(org-agenda-finalize)' to trigger the regular
finalize hook — which drives `actionable-query--insert-all' and
renders the view's sections.  Finally installs
`actionable-query-view-map' so `g'/`r' refresh by re-opening."
  (let* ((name    (plist-get view :name))
         (bufname (plist-get view :buffer))
         (open    (plist-get view :open))
         (buffer  (get-buffer-create bufname)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (eq major-mode 'org-agenda-mode)
          (org-agenda-mode))
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (erase-buffer))
        ;; Stash immediately so anything running before --insert-all
        ;; (e.g. other finalize-hook consumers) sees the correct view.
        (setq-local actionable-query--view name)
        (let ((actionable-query--current-view name))
          (let ((inhibit-read-only t))
            (funcall open))
          ;; Drive the regular finalize pipeline; this is what fires
          ;; `org-agenda-finalize-hook', and thus our `--insert-all'.
          (org-agenda-finalize))
        (use-local-map actionable-query-view-map)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun actionable-query-open-view (&optional name)
  "Open the named view NAME, registered via `actionable-query-define-view'.
Interactively, prompt via `completing-read' over `actionable-query-views'.

Binds the dynamic `actionable-query--current-view' around the view's
`:open' thunk so the finalize hook that fires from `:open' sees the view
name and renders only sections whose `:view' matches (or sections with
no `:view', which are universal).  The hook then stashes the name in
the buffer-local `actionable-query--view' so subsequent
`org-agenda-redo' calls (e.g. pressing `g') preserve the filter.

When the view's `:buffer' is non-nil, dispatches to
`actionable-query--open-dedicated' which creates/reuses that buffer,
installs `org-agenda-mode', runs `:open' inside it, and drives
`(org-agenda-finalize)' to render the matching sections.  In dedicated
mode `g'/`r' call back here to refresh.

When `:buffer' is nil (attach mode), just funcall `:open' with the
dynamic view flag bound — the thunk typically calls `org-agenda' to
produce the shared `*Org Agenda*' buffer, and the finalize hook picks
up the flag from there."
  (interactive
   (list (completing-read
          "View: "
          (mapcar (lambda (entry)
                    (let ((plist (cdr entry)))
                      (if-let ((desc (plist-get plist :description)))
                          (format "%s — %s" (car entry) desc)
                        (car entry))))
                  actionable-query-views)
          nil t)))
  ;; Strip the " — description" suffix in case user picked a decorated
  ;; candidate from completing-read.
  (let* ((name (if (string-match " — " name)
                   (substring name 0 (match-beginning 0))
                 name))
         (view (alist-get name actionable-query-views nil nil #'equal)))
    (unless view
      (user-error "No view named %S — known views: %S"
                  name (mapcar #'car actionable-query-views)))
    (if (plist-get view :buffer)
        (actionable-query--open-dedicated view)
      ;; Attach mode: dynamic flag in scope, funcall :open.
      (let ((actionable-query--current-view name))
        (funcall (plist-get view :open))))))

;;; View-membership predicate

(defun actionable-query--belongs-to-view-p (section view)
  "Non-nil when SECTION should render under VIEW.

A section with no `:view' is *universal* — renders in every view, and
also in plain agenda buffers opened without a named view (VIEW nil).
A section with `:view STRING' renders only when VIEW equals STRING.
A section with `:view (LIST …)' renders when VIEW is any member.

When VIEW is nil (no named view active — e.g. someone opened a plain
`org-agenda-list' outside our dispatcher), universal sections still
render; view-scoped sections do not."
  (let ((declared (plist-get section :view)))
    (cond
     ((null declared) t)                            ; universal
     ((null view) nil)                              ; scoped, no active view
     ((stringp declared) (equal declared view))
     ((listp declared)   (member view declared))
     (t (error "Invalid :view value %S — expected string or list of strings"
               declared)))))

(defun actionable-query--current-view-name ()
  "Return the active view name, or nil when no named view is in play.
Buffer-local `actionable-query--view' wins (survives `g'-refresh);
falls back to the dynamic `actionable-query--current-view' set by
`actionable-query-open-view' for the very first render."
  (or actionable-query--view
      actionable-query--current-view))

;;; Registry

(defvar actionable-query--registry nil
  "List of registered section plists in declaration order (newest first).
Each plist carries internal keys: :title :items-fn :items-async-fn
:item-to-string-fn :on-return-fn :on-hover-fn :org-fn
:remove-on-return :show-if-fn :buttons-fn :view.")

(defun actionable-query--register (&rest spec)
  "Upsert a section SPEC (plist) into `actionable-query--registry', keyed by :title."
  (let ((title (plist-get spec :title)))
    (setq actionable-query--registry
          (cons spec
                (cl-remove-if (lambda (s) (equal (plist-get s :title) title))
                              actionable-query--registry)))))

;;; Per-day dismissal cache

(defvar actionable-query--dismissed (make-hash-table :test #'equal)
  "Hash-table: (TITLE . DATE-STRING) → list of dismissed raw items.
DATE-STRING is \"%Y-%m-%d\", so the cache resets each day.")

(defun actionable-query--dismissed-p (title item)
  "Return non-nil if ITEM in section TITLE was dismissed today."
  (member item (gethash (cons title (format-time-string "%Y-%m-%d"))
                        actionable-query--dismissed)))

(defun actionable-query--dismiss (title item)
  "Mark raw ITEM in section TITLE as dismissed for today."
  (let ((key (cons title (format-time-string "%Y-%m-%d"))))
    (puthash key
             (cons item (gethash key actionable-query--dismissed))
             actionable-query--dismissed)))

;;; Async item cache

(defvar actionable-query--async-cache (make-hash-table :test #'equal)
  "Hash-table: TITLE → plist (:items LIST :date DATE-STRING).
Populated by async callbacks; invalidated when DATE-STRING ≠ today.")

(defun actionable-query--async-cached-items (title)
  "Return cached items for TITLE if from today, else nil."
  (when-let ((entry (gethash title actionable-query--async-cache)))
    (when (equal (plist-get entry :date) (format-time-string "%Y-%m-%d"))
      (plist-get entry :items))))

(defun actionable-query--async-store (title items)
  "Store ITEMS for TITLE in the async cache, stamped with today's date."
  (puthash title
           (list :items items :date (format-time-string "%Y-%m-%d"))
           actionable-query--async-cache))

(defun actionable-query-invalidate-cache (&optional title)
  "Invalidate the async item cache.
When TITLE is given, invalidate only that section; otherwise clear all."
  (if title
      (remhash title actionable-query--async-cache)
    (clrhash actionable-query--async-cache)))

;;; Rendering

(defun actionable-query--insert (section)
  "Render one SECTION into the current agenda buffer."
  (let* ((title            (plist-get section :title))
         (items-fn         (plist-get section :items-fn))
         (items-async-fn   (plist-get section :items-async-fn))
         (item-to-str-fn   (or (plist-get section :item-to-string-fn) #'identity))
         (hover-fn         (plist-get section :on-hover-fn))
         (on-return-fn     (plist-get section :on-return-fn))
         (org-fn           (plist-get section :org-fn))
         (remove-on-ret    (plist-get section :remove-on-return))
         (show-if-fn       (or (plist-get section :show-if-fn) (lambda () t)))
         (buttons-fn       (plist-get section :buttons-fn)))
    (when (funcall show-if-fn)
    (cond
     ;; ── Async path ──────────────────────────────────────────────────────────
     (items-async-fn
      (let ((cached (actionable-query--async-cached-items title)))
        (if cached
            (actionable-query--render-items title cached item-to-str-fn
                                              hover-fn on-return-fn org-fn
                                              remove-on-ret buttons-fn)
          ;; No cache yet — show placeholder and kick off fetch.
          (goto-char (point-max))
          (insert "\n"
                  (propertize (format "⏳ %s — loading…" title)
                              'face 'org-agenda-structure)
                  "\n")
          (funcall items-async-fn
                   (let ((ttl title))
                     (lambda (items)
                       (actionable-query--async-store ttl items)
                       ;; Redraw from the timer context so we're not inside
                       ;; the finalize hook.
                       (run-at-time 0 nil #'org-agenda-redo)))))))
     ;; ── Sync path ───────────────────────────────────────────────────────────
     (items-fn
      (let ((items (cl-remove-if
                    (lambda (item) (actionable-query--dismissed-p title item))
                    (funcall items-fn))))
        (actionable-query--render-items title items item-to-str-fn
                                          hover-fn on-return-fn org-fn
                                          remove-on-ret buttons-fn)))))))

(defconst actionable-query--button-valid-keys
  '(:title :action :location :foreground-color :background-color
    :weight :slant :face :echo)
  "The only plist keys a `:buttons' spec may carry.
Unknown keys cause `--parse-button' to error — catches typos like
`:forground-color' at load time.

Curated set — this is the \"pragmatic middle ground\" between strict
validation (catch typos loud) and forwarding arbitrary properties to
`insert-text-button' (which would couple us to Emacs' button
internals).  New keys are added deliberately, with clean semantics,
not by opening the floodgates.")

(defun actionable-query--parse-button (spec)
  "Validate SPEC and return it normalised (all defaulted keys filled in).

SPEC is a plist with these keys:

  :title STRING              required — the button label
  :action URL-OR-FN          required — URL string or nullary function
  :location SYMBOL           optional — 'here (default) / 'top / 'bottom
  :foreground-color STRING   optional — overrides
                                        `actionable-query-button-foreground'
  :background-color STRING   optional — background colour (default: inherit)
  :weight SYMBOL             optional — face weight override
                                        (e.g. 'normal, 'bold); default 'bold
  :slant SYMBOL              optional — face slant override
                                        (e.g. 'normal, 'italic); default 'italic
  :face FACE                 optional — either a face symbol (e.g.
                                        'warning, 'error, 'success,
                                        'shadow) or a face plist like
                                        (:foreground \"red\" :weight
                                        bold).  When supplied,
                                        REPLACES the pill-style
                                        defaults entirely; other
                                        face-tweaking keys are
                                        ignored.  See Info node
                                        `(elisp) Face Attributes' for
                                        the full plist vocabulary
  :echo STRING               optional — explicit `help-echo' tooltip;
                                        when omitted, the tooltip is
                                        derived from `:action' (URL
                                        string → the URL; named
                                        function → first line of
                                        docstring; else a generic
                                        fallback)

Errors loud on: non-plist shape, missing `:title' / `:action', unknown
keys, invalid `:location' value, unsupported `:action' type."
  (unless (and (listp spec) (keywordp (car spec)))
    (error "Button spec %S must be a plist starting with a keyword" spec))
  ;; Unknown-key check — walk every key, bail on first foreign keyword.
  (let ((rest spec))
    (while rest
      (let ((k (car rest)))
        (unless (memq k actionable-query--button-valid-keys)
          (error "Unknown button key %S — valid keys: %S"
                 k actionable-query--button-valid-keys))
        (setq rest (cddr rest)))))
  (let ((title    (plist-get spec :title))
        (action   (plist-get spec :action))
        (location (or (plist-get spec :location) 'here))
        (fg       (plist-get spec :foreground-color))
        (bg       (plist-get spec :background-color))
        (weight   (plist-get spec :weight))
        (slant    (plist-get spec :slant))
        (face     (plist-get spec :face))
        (echo     (plist-get spec :echo)))
    (unless title
      (error "Button spec missing required :title — got %S" spec))
    (unless action
      (error "Button spec missing required :action — got %S" spec))
    (unless (memq location '(here top bottom))
      (error "Invalid :location %S — expected here/top/bottom" location))
    (unless (or (stringp action) (functionp action))
      (error "Button :action %S is neither a URL string nor a function" action))
    (when (and echo (not (stringp echo)))
      (error "Button :echo %S must be a string" echo))
    (when (and fg (not (stringp fg)))
      (error "Button :foreground-color %S must be a string" fg))
    (when (and bg (not (stringp bg)))
      (error "Button :background-color %S must be a string" bg))
    (when (and weight (not (symbolp weight)))
      (error "Button :weight %S must be a symbol (e.g. 'normal, 'bold)" weight))
    (when (and slant (not (symbolp slant)))
      (error "Button :slant %S must be a symbol (e.g. 'normal, 'italic)" slant))
    (when (and face (not (or (listp face) (symbolp face))))
      (error "Button :face %S must be a face plist or symbol" face))
    (list :title title
          :action action
          :location location
          :foreground-color fg
          :background-color bg
          :weight weight
          :slant slant
          :face face
          :echo echo)))

(defvar actionable-query-button-foreground "cyan"
  "Default foreground colour for section buttons.
Individual buttons override via `:foreground-color' in their spec.")

(defun actionable-query--insert-button (spec)
  "Insert one normalised button SPEC at point.
SPEC is the plist returned by `actionable-query--parse-button' —
all defaulted keys populated.  Inserts a leading space so buttons
arriving in a row don't collide with each other or with the section
title."
  (let* ((title  (plist-get spec :title))
         (action (plist-get spec :action))
         (fn (if (stringp action)
                 (lambda (_pos) (browse-url action))
               (lambda (_pos) (funcall action))))
         ;; Tooltip precedence: explicit `:echo' wins; otherwise
         ;; derive from `:action' (URL / docstring / generic).
         (help-echo (or (plist-get spec :echo)
                        (cond
                         ((stringp action) action)
                         ((and (symbolp action) (documentation action))
                          ;; Leading line of the function's docstring.
                          (car (split-string (documentation action) "\n")))
                         ((symbolp action) (format "Run %s" action))
                         (t "Run button action"))))
         ;; Face composition.  Explicit `:face' replaces the pill
         ;; defaults entirely — caller's choice to escape the style.
         ;; Otherwise start with the pill defaults (italic + bold +
         ;; released-button box; no `face 'button' because that reads
         ;; as underlined link, not a button) and layer the
         ;; per-attribute overrides on top.
         (face-plist
          (or (plist-get spec :face)
              (let ((fg     (or (plist-get spec :foreground-color)
                                actionable-query-button-foreground))
                    (bg     (plist-get spec :background-color))
                    (weight (or (plist-get spec :weight) 'bold))
                    (slant  (or (plist-get spec :slant)  'italic)))
                (append (list :foreground fg)
                        (when bg (list :background bg))
                        (list :weight weight
                              :slant  slant
                              :box '(:line-width 2 :style released-button)))))))
    (insert " ")
    (insert-text-button title
                        'action fn
                        'follow-link t
                        'face face-plist
                        'mouse-face 'highlight
                        'help-echo help-echo)))

;;;###autoload
(defun actionable-query-insert-button (spec)
  "Insert a single button SPEC at point.
SPEC is a plist; see the `:buttons' docs at the top of this file for
the accepted keys and their defaults.

This is the same rendering machinery the library uses for section
`:buttons'.  Exposed for callers who want to splice a stylistically
consistent button into their own agenda-customisation code (e.g.
bespoke `org-agenda-finalize-hook' prose, one-off focus-line buttons).
Unlike the `:buttons' path, no location routing happens here — the
caller is responsible for point placement and newlines."
  (actionable-query--insert-button
   (actionable-query--parse-button spec)))

(defun actionable-query--collect-buttons (location &optional view)
  "Return the list of button plists registered for LOCATION across all sections.
LOCATION is 'top or 'bottom.  Deduped by (title . action) equality so
that a button declared identically by N sections renders once.  Order
follows the section registry (newest first, i.e. declaration order).

When VIEW is non-nil, buttons from view-scoped sections contribute only
when the section's `:view' matches VIEW; universal sections (no `:view')
always contribute.  When VIEW is nil, only universal sections contribute
— scoped sections' buttons are suppressed, matching the same rule used
for item rendering."
  (let (seen out)
    (dolist (section (reverse actionable-query--registry))
      (when (actionable-query--belongs-to-view-p section view)
        (when-let* ((buttons-fn (plist-get section :buttons-fn))
                    (specs      (funcall buttons-fn)))
          (dolist (raw specs)
            (let* ((btn (actionable-query--parse-button raw))
                   (key (cons (plist-get btn :title) (plist-get btn :action))))
              (when (and (eq (plist-get btn :location) location)
                         (not (member key seen)))
                (push key seen)
                (push btn out)))))))
    (nreverse out)))

(defun actionable-query--insert-button-row (location)
  "Insert the collected buttons for LOCATION at point, followed by a newline.
Does nothing when no buttons are registered for LOCATION.  LOCATION is
'top or 'bottom."
  (let ((buttons (actionable-query--collect-buttons location)))
    (when buttons
      (dolist (btn buttons)
        (actionable-query--insert-button btn))
      (insert "\n"))))

(defun actionable-query--render-items (title items item-to-str-fn
                                               hover-fn on-return-fn org-fn
                                               remove-on-ret &optional buttons-fn)
  "Insert the TITLE heading and numbered ITEMS into the agenda buffer.
BUTTONS-FN, if non-nil, is a nullary thunk returning a list of plist
button specs (see `actionable-query--parse-button').  Only buttons
whose `:location' is 'here render beside the title; 'top/'bottom
buttons are collected and rendered in their own rows elsewhere in the
buffer."
  (when items
    (goto-char (point-max))
    (insert "\n" (propertize title 'face 'org-agenda-structure))
    (when buttons-fn
      (dolist (raw (funcall buttons-fn))
        (let ((btn (actionable-query--parse-button raw)))
          (when (eq (plist-get btn :location) 'here)
            (actionable-query--insert-button btn)))))
    (insert "\n")
    (let ((n 0))
      (dolist (item items)
        (cl-incf n)
        (let ((start (point)))
          (insert (format "%d. %s\n" n (funcall item-to-str-fn item)))
          (let* ((end (1- (point)))
                 (map (make-sparse-keymap)))
            (set-keymap-parent map org-agenda-mode-map)
            ;; ── RET ──
            (define-key map (kbd "RET")
              (let ((i item) (tt title))
                (lambda ()
                  (interactive)
                  (when on-return-fn (funcall on-return-fn i))
                  (when remove-on-ret
                    (actionable-query--dismiss tt i)
                    (org-agenda-redo)))))
            (put-text-property start end 'keymap map)
            ;; ── help-echo ──
            (when hover-fn
              (put-text-property start end 'help-echo (funcall hover-fn item)))
            ;; ── stash raw item ──
            (put-text-property start end 'actionable-query--item item)
            (put-text-property start end 'actionable-query--title title)
            ;; ── org-marker (when :org fn returns a live marker) ──
            (when org-fn
              (when-let ((marker (funcall org-fn item)))
                (when (and (markerp marker) (marker-buffer marker))
                  (put-text-property start end 'org-marker marker)
                  (put-text-property start end 'org-hd-marker marker))))))))))

(defun actionable-query--insert-all ()
  "Insert all registered sections at the end of the Org-agenda buffer.

Computes the current view name (from the buffer-local stash on refresh,
or the dynamic binding set by `actionable-query-open-view' on first
render) and filters the registry to sections whose `:view' matches (or
is absent — universal sections render in every view).  Stashes the
view name buffer-locally so subsequent `org-agenda-redo' calls (e.g.
`g') keep the same filter.

The top-row buttons (collected from matching sections' 'top
declarations, deduped) are rendered first; then each matching section;
finally the bottom-row buttons.  Top/bottom rows land at `point-max'
at the moment this hook runs, which means they sit *below* anything
other finalize hooks have already inserted at the buffer top —
acceptable while `init.org' still owns its own top row."
  (let ((view (actionable-query--current-view-name)))
    ;; Stash view name so `g'-refresh sees it after the dynamic binding
    ;; from `actionable-query-open-view' has unwound.
    (setq-local actionable-query--view view)
    (goto-char (point-max))
    (let ((top-buttons (actionable-query--collect-buttons 'top view)))
      (when top-buttons
        (insert "\n")
        (dolist (btn top-buttons)
          (actionable-query--insert-button btn))
        (insert "\n")))
    (dolist (section (reverse actionable-query--registry))
      (when (actionable-query--belongs-to-view-p section view)
        (actionable-query--insert section)))
    (goto-char (point-max))
    (let ((bottom-buttons (actionable-query--collect-buttons 'bottom view)))
      (when bottom-buttons
        (insert "\n")
        (dolist (btn bottom-buttons)
          (actionable-query--insert-button btn))
        (insert "\n")))))

(add-hook 'org-agenda-finalize-hook #'actionable-query--insert-all)

;;; Public macro

(defmacro defquery (&rest spec)
  "Register a custom Org-agenda section.  See Commentary for full keyword docs."
  (let ((title            (plist-get spec :title))
        (items-form       (plist-get spec :items))
        (items-async-form (plist-get spec :items-async))
        (item-to-str-form (plist-get spec :item-to-string))
        (on-return-form   (plist-get spec :on-return))
        (on-hover-form    (plist-get spec :on-hover))
        (org-form         (plist-get spec :org))
        (remove-on-return (plist-get spec :remove-item-on-return))
        (show-if-form     (plist-get spec :show-if))
        (show-if-present  (plist-member spec :show-if))
        (buttons-form     (plist-get spec :buttons))
        (view-form        (plist-get spec :view)))
    `(actionable-query--register
      :title             ,title
      ,@(when view-form
          `(:view ,view-form))
      ,@(when items-form
          `(:items-fn (lambda () ,items-form)))
      ,@(when items-async-form
          `(:items-async-fn (lambda (callback) ,items-async-form)))
      ,@(when item-to-str-form
          `(:item-to-string-fn (lambda (it) ,item-to-str-form)))
      ,@(when on-return-form
          `(:on-return-fn (lambda (it) ,on-return-form)))
      ,@(when on-hover-form
          `(:on-hover-fn (lambda (it) ,on-hover-form)))
      ,@(when org-form
          `(:org-fn (lambda (it) ,org-form)))
      :remove-on-return  ,remove-on-return
      :show-if-fn        ,(if show-if-present
                              `(lambda () ,show-if-form)
                            `(lambda () t))
      ,@(when buttons-form
          `(:buttons-fn (lambda () ,buttons-form))))))

(provide 'actionable-query)
;;; actionable-query.el ends here
