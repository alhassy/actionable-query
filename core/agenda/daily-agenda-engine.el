;;; daily-agenda-engine.el --- defquery sections spliced into the C-c a agenda  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; The engine behind `C-c a'.  Unlike `actionable-query-defview' (which
;; renders standalone vtable buffers registered in `org-ql-views'), this
;; splices `org-ql' sections *into* an `org-agenda-mode' buffer via
;; `org-agenda-finalize-hook' --- so `C-c a' stays a real agenda buffer
;; (RET/TAB/t/I/o, `g'-refresh) with our extra sections appended.
;;
;; Three pieces, all consumed from init.org:
;;
;;   `defquery'                      — register one section (title + sync
;;                                     `:items' + `:item-to-string'/`:org'/
;;                                     `:show-if'/`:buttons'/`:view').
;;   `actionable-query-define-view'  — name a view (e.g. "daily") whose
;;     + `-open-view'                  `:open' thunk opens an agenda; the
;;                                     finalize hook then renders only the
;;                                     sections whose `:view' matches.
;;   `actionable-query-insert-button'— splice one styled button at point
;;                                     (used for one-off focus-line buttons).
;;
;; This is the synchronous slice of an older, larger engine: the async
;; item cache, dedicated-buffer mode, and per-day dismissal cache it once
;; carried are unused by `C-c a' and have been axed.

;;; Code:

(require 'cl-lib)
(require 'org-agenda)

;;; ─── named views ─────────────────────────────────────────────────────────────

(defvar actionable-query--current-view nil
  "Dynamic view name in force for the next finalize pass, or nil.
Bound by `actionable-query-open-view' around its `:open' thunk.  The
finalize hook promotes it to `actionable-query--view' (buffer-local)
so refresh works after the binding unwinds.")

(defvar-local actionable-query--view nil
  "Buffer-local view name, stashed by the finalize hook so `g'-refresh
keeps the same `:view' filter after the dynamic binding has unwound.")

(defvar actionable-query-views nil
  "Alist of named views: (NAME . PLIST), PLIST carrying `:name'/`:open'/
`:description'.")

(defconst actionable-query--view-valid-keys
  '(:name :open :description)
  "The only plist keys a view spec may carry; unknown keys error (catches typos).")

(defun actionable-query-define-view (&rest spec)
  "Register or update a named view.
SPEC is a plist with `:name' STRING (required), `:open' FUNCTION (required,
a nullary thunk that opens an agenda, e.g. `(lambda () (org-agenda nil \"a\"))'),
and optional `:description' for the `completing-read' prompt.  Upserts into
`actionable-query-views' keyed by :name; returns the normalised plist."
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

(defun actionable-query-open-view (&optional name)
  "Open the named view NAME, registered via `actionable-query-define-view'.
Interactively, prompt via `completing-read' over `actionable-query-views'.

Binds the dynamic `actionable-query--current-view' around the view's
`:open' thunk so the finalize hook fired from `:open' sees the view name
and renders only sections whose `:view' matches (or sections with no
`:view', which are universal).  The hook then stashes the name in the
buffer-local `actionable-query--view' so subsequent `org-agenda-redo'
calls (e.g. pressing `g') preserve the filter."
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
  (let* ((name (if (string-match " — " name)
                   (substring name 0 (match-beginning 0))
                 name))
         (view (alist-get name actionable-query-views nil nil #'equal)))
    (unless view
      (user-error "No view named %S — known views: %S"
                  name (mapcar #'car actionable-query-views)))
    (let ((actionable-query--current-view name))
      (funcall (plist-get view :open)))))

(defun actionable-query--belongs-to-view-p (section view)
  "Non-nil when SECTION should render under VIEW.
A section with no `:view' is universal (renders everywhere).  A section
with `:view STRING' renders only when VIEW equals STRING; with `:view
\(LIST …)' when VIEW is any member.  When VIEW is nil, only universal
sections render."
  (let ((declared (plist-get section :view)))
    (cond
     ((null declared) t)
     ((null view) nil)
     ((stringp declared) (equal declared view))
     ((listp declared)   (member view declared))
     (t (error "Invalid :view value %S — expected string or list of strings"
               declared)))))

(defun actionable-query--current-view-name ()
  "Return the active view name, or nil.
Buffer-local `actionable-query--view' wins (survives `g'-refresh); falls
back to the dynamic `actionable-query--current-view' set on first render."
  (or actionable-query--view
      actionable-query--current-view))

;;; ─── section registry ────────────────────────────────────────────────────────

(defvar actionable-query--registry nil
  "List of registered section plists in declaration order (newest first).
Each carries internal keys: :title :items-fn :item-to-string-fn
:on-return-fn :on-hover-fn :org-fn :show-if-fn :buttons-fn :view.")

(defun actionable-query--register (&rest spec)
  "Upsert a section SPEC (plist) into `actionable-query--registry', keyed by :title."
  (let ((title (plist-get spec :title)))
    (setq actionable-query--registry
          (cons spec
                (cl-remove-if (lambda (s) (equal (plist-get s :title) title))
                              actionable-query--registry)))))

;;; ─── buttons ─────────────────────────────────────────────────────────────────

(defconst actionable-query--button-valid-keys
  '(:title :action :location :foreground-color :background-color
    :weight :slant :face :echo)
  "The only plist keys a `:buttons' spec may carry; unknown keys error.")

(defvar actionable-query-button-foreground "cyan"
  "Default foreground colour for section buttons.
Individual buttons override via `:foreground-color' in their spec.")

(defun actionable-query--parse-button (spec)
  "Validate SPEC and return it normalised (all defaulted keys filled in).
See the `:buttons' keys in `actionable-query--button-valid-keys'.  Errors
loud on a non-plist shape, missing `:title'/`:action', unknown keys, an
invalid `:location', or an unsupported `:action' type."
  (unless (and (listp spec) (keywordp (car spec)))
    (error "Button spec %S must be a plist starting with a keyword" spec))
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
    (list :title title :action action :location location
          :foreground-color fg :background-color bg
          :weight weight :slant slant :face face :echo echo)))

(defun actionable-query--insert-button (spec)
  "Insert one normalised button SPEC at point (leading space, pill style)."
  (let* ((title  (plist-get spec :title))
         (action (plist-get spec :action))
         (fn (if (stringp action)
                 (lambda (_pos) (browse-url action))
               (lambda (_pos) (funcall action))))
         (help-echo (or (plist-get spec :echo)
                        (cond
                         ((stringp action) action)
                         ((and (symbolp action) (documentation action))
                          (car (split-string (documentation action) "\n")))
                         ((symbolp action) (format "Run %s" action))
                         (t "Run button action"))))
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
SPEC is a plist; see `actionable-query--button-valid-keys' for the keys.
This is the same machinery the library uses for section `:buttons' ---
exposed for callers splicing a stylistically consistent button into their
own agenda code (e.g. bespoke `org-agenda-finalize-hook' prose).  No
location routing happens here; the caller places point and newlines."
  (actionable-query--insert-button
   (actionable-query--parse-button spec)))

(defun actionable-query--collect-buttons (location &optional view)
  "Return the button plists registered for LOCATION ('top/'bottom) across sections.
Deduped by (title . action); order follows declaration order.  When VIEW is
non-nil, view-scoped sections contribute only on a `:view' match; universal
sections always contribute."
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

;;; ─── rendering into the agenda buffer ────────────────────────────────────────

(defun actionable-query--render-items (title items item-to-str-fn
                                              hover-fn on-return-fn org-fn
                                              &optional buttons-fn)
  "Insert the TITLE heading and numbered ITEMS into the agenda buffer.
Each item carries a RET binding (parented on `org-agenda-mode-map') that
funcalls ON-RETURN-FN, a help-echo from HOVER-FN, the raw item, and ---
when ORG-FN yields a live marker --- `org-marker'/`org-hd-marker' so the
standard agenda nav (RET/TAB/t/I/o) jumps to the Org heading.  BUTTONS-FN,
if non-nil, supplies `:location 'here' buttons rendered beside the title."
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
            (define-key map (kbd "RET")
              (let ((i item))
                (lambda () (interactive)
                  (when on-return-fn (funcall on-return-fn i)))))
            (put-text-property start end 'keymap map)
            (when hover-fn
              (put-text-property start end 'help-echo (funcall hover-fn item)))
            (put-text-property start end 'actionable-query--item item)
            (put-text-property start end 'actionable-query--title title)
            (when org-fn
              (when-let ((marker (funcall org-fn item)))
                (when (and (markerp marker) (marker-buffer marker))
                  (put-text-property start end 'org-marker marker)
                  (put-text-property start end 'org-hd-marker marker))))))))))

(defun actionable-query--insert (section)
  "Render one SECTION into the current agenda buffer (synchronous `:items')."
  (let* ((title          (plist-get section :title))
         (items-fn       (plist-get section :items-fn))
         (item-to-str-fn (or (plist-get section :item-to-string-fn) #'identity))
         (hover-fn       (plist-get section :on-hover-fn))
         (on-return-fn   (plist-get section :on-return-fn))
         (org-fn         (plist-get section :org-fn))
         (show-if-fn     (or (plist-get section :show-if-fn) (lambda () t)))
         (buttons-fn     (plist-get section :buttons-fn)))
    (when (and items-fn (funcall show-if-fn))
      (actionable-query--render-items title (funcall items-fn) item-to-str-fn
                                      hover-fn on-return-fn org-fn buttons-fn))))

(defun actionable-query--insert-all ()
  "Insert all registered sections at the end of the Org-agenda buffer.
Filters the registry to sections whose `:view' matches the active view
(universal sections always render), stashing the view name buffer-locally
so `g'-refresh keeps the filter.  Top-row buttons render first, then each
matching section, then bottom-row buttons."
  (let ((view (actionable-query--current-view-name)))
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

;;; ─── public macro ────────────────────────────────────────────────────────────

(defmacro defquery (&rest spec)
  "Register a custom Org-agenda section spliced into `C-c a'.
Keys: `:title' STRING; `:items' FORM (a list of items, evaluated at agenda
build time); `:item-to-string' FORM (`it' bound, default `identity');
`:on-return' FORM (`it' bound); `:on-hover' FORM (`it' bound); `:org' FORM
\(`it' bound, yields a live marker for RET/TAB nav); `:show-if' FORM (skip
the section when nil); `:view' STRING-OR-LIST (membership; absent = universal);
`:buttons' FORM (a list of button plists)."
  (let ((title            (plist-get spec :title))
        (items-form       (plist-get spec :items))
        (item-to-str-form (plist-get spec :item-to-string))
        (on-return-form   (plist-get spec :on-return))
        (on-hover-form    (plist-get spec :on-hover))
        (org-form         (plist-get spec :org-upsert))
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
      ,@(when item-to-str-form
          `(:item-to-string-fn (lambda (it) ,item-to-str-form)))
      ,@(when on-return-form
          `(:on-return-fn (lambda (it) ,on-return-form)))
      ,@(when on-hover-form
          `(:on-hover-fn (lambda (it) ,on-hover-form)))
      ,@(when org-form
          `(:org-fn (lambda (it) ,org-form)))
      :show-if-fn        ,(if show-if-present
                              `(lambda () ,show-if-form)
                            `(lambda () t))
      ,@(when buttons-form
          `(:buttons-fn (lambda () ,buttons-form))))))

(provide 'daily-agenda-engine)
;;; daily-agenda-engine.el ends here
