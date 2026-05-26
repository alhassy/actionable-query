;;; whats-app.el --- WhatsApp views for Emacs via actionable-query-defview  -*- lexical-binding: t; -*-
;;
;; Prerequisites (one-time setup):
;;   1. cd ~/actionable-query && npm install @whiskeysockets/baileys qrcode-terminal
;;   2. M-x whatsapp/auth   — scan QR code; session saved to whats-app/whatsapp-session/
;;   3. Done.  All subsequent calls are fully headless — no browser, no GUI.
;;

(require 'actionable-query)

;;; ─── paths & constants ───────────────────────────────────────────────────────

(defconst whatsapp-cli-path
  (expand-file-name "whatsapp-cli.js"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the headless WhatsApp Node.js bridge script.")

(defconst whatsapp-session-dir
  (expand-file-name "whatsapp-session/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory where Baileys persists the WhatsApp session credentials.")

;;; ─── low-level helpers ───────────────────────────────────────────────────────

(defun whatsapp--node-argv (subcmd &rest args)
  "Build argv list: node WHATSAPP-CLI-PATH SUBCMD ARGS…"
  `("node" ,whatsapp-cli-path ,subcmd ,@args))

(defun whatsapp--parse-json (raw)
  "Parse RAW (JSON string from whatsapp-cli.js stdout) into Emacs objects.
Returns a list of plists, one per contact / message."
  (condition-case err
      (let ((parsed (json-parse-string (string-trim raw)
                                       :object-type 'plist
                                       :array-type  'list
                                       :null-object  nil
                                       :false-object nil)))
        ;; CLI always returns an array at the top level for list commands.
        (if (listp parsed) parsed (list parsed)))
    (json-parse-error
     (message "whatsapp-cli JSON parse error: %s\nRaw: %.200s" (cadr err) raw)
     nil)))

(defun whatsapp--cli-async (argv callback)
  "Spawn ARGV (list) asynchronously; pipe stderr away and deliver parsed JSON to CALLBACK.
Baileys emits log noise to stderr — isolating it prevents contaminating stdout JSON."
  (let ((buf (generate-new-buffer " *whatsapp-cli*")))
    (make-process
     :name     "whatsapp-cli"
     :command  argv
     :buffer   buf
     :stderr   (make-pipe-process :name "whatsapp-cli-err"
                                  :buffer (generate-new-buffer " *whatsapp-cli-err*")
                                  :sentinel (lambda (p _)
                                              (when (eq (process-status p) 'closed)
                                                (kill-buffer (process-buffer p)))))
     :sentinel (lambda (proc _)
                 (when (eq (process-status proc) 'exit)
                   (let ((raw (with-current-buffer (process-buffer proc) (buffer-string))))
                     (kill-buffer (process-buffer proc))
                     (funcall callback (whatsapp--parse-json raw))))))))

(defun whatsapp--send-async (jid msg &optional on-done)
  "Send MSG to JID via whatsapp-cli.js; show confirmation or error message."
  (let ((buf (generate-new-buffer " *whatsapp-send*")))
    (make-process
     :name    "whatsapp-send"
     :command (list "node" whatsapp-cli-path "send" jid msg)
     :buffer  buf
     :stderr  (make-pipe-process :name "whatsapp-send-err"
                                 :buffer (generate-new-buffer " *whatsapp-send-err*")
                                 :sentinel (lambda (p _)
                                             (when (eq (process-status p) 'closed)
                                               (kill-buffer (process-buffer p)))))
     :sentinel
     (lambda (proc _)
       (when (eq (process-status proc) 'exit)
         (let* ((raw      (with-current-buffer (process-buffer proc) (buffer-string)))
                (json-str (when (string-match "{[^}]*}" raw) (match-string 0 raw)))
                (result   (and json-str
                               (condition-case _ (json-parse-string json-str :object-type 'plist) (error nil)))))
           (kill-buffer (process-buffer proc))
           (if (plist-get result :ok)
               (aq--message "WhatsApp: sent to %s" jid)
             (aq--message "WhatsApp send failed: %s" (string-trim raw)))
           (when on-done (funcall on-done result))))))))


(defun whatsapp--format-ts (iso)
  "Reformat ISO timestamp \"2026-05-09T14:30:00.000Z\" to \"May 09 14:30\"."
  (if (or (null iso) (string-empty-p iso)) ""
    (condition-case _
        (format-time-string "%b %d %H:%M" (date-to-time iso))
      (error (substring iso 0 (min 10 (length iso)))))))

(defun whatsapp--snooze-until-friday ()
  "Return a YYYY-MM-DD string for the coming Friday (next week if today is Friday)."
  (let* ((dow  (string-to-number (format-time-string "%u"))) ; 1=Mon … 7=Sun
         (days (if (= dow 5) 7 (mod (- 5 dow) 7))))
    (format-time-string "%Y-%m-%d" (time-add nil (* days 24 60 60)))))

(defun whatsapp--day-message (name)
  "Build a day-of-week greeting for NAME, randomly chosen from day-specific phrases."
  (let* ((day (string-to-number (format-time-string "%u"))) ; 1=Mon … 7=Sun
         (phrases
          (pcase day
            (1 ["Magnificent Monday to you, %s! 🌟"
                "Monday Mubarak, %s — may this week open with blessings."
                "A Marvel of a Monday, %s — the week is yours to shape! 💪"])
            (2 ["Terrific Tuesday, %s! ✨"
                "Tuesday Takeover, %s — may you conquer what yesterday left undone."
                "Two-terrific-Tuesday, %s — one day wiser than Monday! 😄"])
            (3 ["Wonderful Wednesday, %s! 🌿"
                "O Lord, today is Wednesday/Al-Arbaʿa — I ask You for 4 things for %s: health, joy, ease, and nearness to You. 🤲"
                "Midweek magic, %s — you've made it halfway! 🎯"])
            (4 ["Tremendous Thursday, %s! 🚀"
                "Al-Khamis Mubarak, %s — may the eve of Jummah bring you peace and anticipation. 🌙"
                "Thor's Day, %s — strike your goals with full force today! ⚡"])
            (5 ["Jummah Mubaraka, %s! 🌙"
                "Happy Friday, %s — may your Jummah be blessed and your heart at rest. 🤲"
                "Blessed Friday, %s — the best day of the week is yours! 💐"])
            (6 ["Splendid Saturday, %s! 🌸"
                "Weekend Mubarak, %s — rest, recharge, and rejoice. 😌"
                "Saturday Serenity, %s — the world can wait a little. ☕"])
            (7 ["Serene Sunday, %s! ☀️"
                "Sunday Salaam, %s — a day of rest and reflection before the week renews."
                "Sunny Sunday, %s — may it be as bright as you deserve! 🌻"])
            (_ ["%s — have a wonderful day! 🌟"]))))
    (format (seq-random-elt phrases) name)))

(defun whatsapp--open-chat (o)
  "Open the WhatsApp desktop app directly to the chat for O."
  (let ((phone (replace-regexp-in-string "[^0-9]" "" (or (plist-get o :phone) ""))))
    (browse-url (format "https://wa.me/%s" phone))))

(defun whatsapp--first-name (full-name)
  "Return the first space-delimited token of FULL-NAME."
  (car (split-string full-name)))

(defun whatsapp--action-funny (o)
  "Send a funny message for contact object O, with immediate and completion feedback."
  (let* ((name (whatsapp--first-name (or (plist-get o :name) (plist-get o :phone) "?")))
         (jid  (plist-get o :jid)))
    (aq--message "Sending funny to %s…" name)
    (whatsapp--send-async jid (whatsapp--funny-message name)
      (lambda (result)
        (if (plist-get result :ok)
            (progn (whatsapp--open-chat o)
                   (aq--message "Funny sent to %s!" name))
          (aq--message "Failed to send funny to %s." name))))))

(defun whatsapp--funny-message (name)
  "Build a random ASCII-art greeting addressed to NAME."
  (let ((art   (fortune :kind 'joke))
        (emoji (seq-random-elt [😁 💐 🌇 🥳 🥸 🤲 🚴 🫎 🍉 🍁])))
    (format "Jummah Mubaraka %s %s\n```\n%s\n```" name emoji art)))

(defun whatsapp--session-exists-p ()
  "Return non-nil if a Baileys session exists (auth performed at least once)."
  (file-exists-p (expand-file-name "creds.json" whatsapp-session-dir)))

(defun whatsapp--ensure-session ()
  "Warn the user if no session exists and suggest running `whatsapp/auth'."
  (unless (whatsapp--session-exists-p)
    (message "No WhatsApp session found — run M-x whatsapp/auth first.")))

;;; ─── M-x whatsapp/auth ───────────────────────────────────────────────────────

(defun whatsapp/auth ()
  "Authenticate with WhatsApp via QR code (one-time setup).
Opens a *whatsapp-auth* buffer running the CLI in auth mode.
Scan the QR code with WhatsApp → Settings → Linked Devices → Link a Device.
The session is saved to whatsapp-session/ and all future calls are headless."
  (interactive)
  (let ((buf (get-buffer-create "*whatsapp-auth*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "WhatsApp authentication — scan the QR code below.\n\n"))
    (display-buffer buf)
    (make-process
     :name    "whatsapp-auth"
     :command (list "node" whatsapp-cli-path "auth")
     :buffer  buf
     :filter  (lambda (proc str)
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-max))
                  (insert str)))
     :sentinel
     (lambda (proc _)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer (process-buffer proc)
           (goto-char (point-max))
           (insert "\n[Process exited]\n"))
         (aq--message "WhatsApp auth complete — session saved."))))))

;;; ─── View 1: Contacts ────────────────────────────────────────────────────────

(defconst whatsapp--contacts-view "📱 WhatsApp Contacts")

(defun whatsapp--fetch-contacts (callback)
  "Fetch contacts from whatsapp-cli.js and deliver to CALLBACK."
  (whatsapp--ensure-session)
  (whatsapp--cli-async (whatsapp--node-argv "contacts")
                       callback))

(actionable-query-defview whatsapp/contacts "📱 WhatsApp Contacts"
  :prose "RET to message · h to heart · H to toggle hearted-only · d to send day greeting + snooze until Friday · f for a Friday funny."
  :columns
  `((:name "Name"
           :width 30
           :getter    (lambda (o &rest _) (or (plist-get o :name) ""))
           :formatter (lambda (v &rest _)
                        (propertize v 'face '(:foreground "deep sky blue" :weight bold))))
    (:name "Phone"
           :width 20
           :getter    (lambda (o &rest _) (or (plist-get o :phone) ""))
           :displayer (lambda (v w _)
                        (propertize (truncate-string-to-width v w)
                                    'face '(:foreground "gray60")))))
  :objects  #'whatsapp--fetch-contacts
  :hearting t
  :row-colors '("alice blue" "lavender")
  :help-echo (lambda (o) (format "%s · %s" (or (plist-get o :name) "") (or (plist-get o :phone) "")))
  :actions
  `(("RET" "Send a message"
     ,(lambda (o)
        (let* ((name (or (plist-get o :name) (plist-get o :phone) "?"))
               (jid  (plist-get o :jid))
               (msg  (read-string (format "Message to %s: " name))))
          (unless (string-empty-p (string-trim msg))
            (whatsapp--send-async jid msg)))))
    ("c" "Copy JID to kill-ring"
     ,(lambda (o)
        (kill-new (plist-get o :jid))
        (aq--message "Copied: %s" (plist-get o :jid))))
    ("d" "Send day greeting + snooze until Friday"
     ,(lambda (o)
        (let* ((name (whatsapp--first-name (or (plist-get o :name) (plist-get o :phone) "?")))
               (jid  (plist-get o :jid))
               (msg  (whatsapp--day-message name))
               (fri  (whatsapp--snooze-until-friday)))
          (aq--message "Sending greeting to %s…" name)
          (whatsapp--send-async jid msg
            (lambda (result)
              (when (plist-get result :ok)
                (whatsapp--open-chat o)
                (aq--dismiss-until whatsapp--contacts-view (aq--obj-id o) fri)
                (when-let ((tbl (vtable-current-table)))
                  (vtable-remove-object tbl o))
                (aq--message "Sent & snoozed until Friday (%s)!" fri)))))))
    ("f" "Send a funny for the day"
     ,(lambda (o) (whatsapp--action-funny o)))))

;;; ─── View 2: Unread messages ─────────────────────────────────────────────────

(defun whatsapp--fetch-unread (callback)
  "Async 1-arg fn: fetch unread chats from whatsapp-cli.js and deliver to CALLBACK.
Each object is a plist: :jid :name :phone :snippet :timestamp :count."
  (whatsapp--ensure-session)
  (whatsapp--cli-async (whatsapp--node-argv "unread")
                       callback))

(defun whatsapp--unread-prose ()
  "Header prose for the unread view."
  "Unread WhatsApp messages — press RET to reply, 'd' to snooze, 'o' to open in app, 'f' for a funny.")

(actionable-query-defview whatsapp/unread "💬 WhatsApp Unread"
  :prose (whatsapp--unread-prose)
  :columns
  `((:name "From"
           :width 22
           :getter    (lambda (o &rest _) (or (plist-get o :name) (plist-get o :phone) ""))
           :formatter (lambda (v &rest _)
                        (propertize v 'face '(:foreground "green3" :weight bold))))
    (:name "#"
           :width 3
           :getter    (lambda (o &rest _) (number-to-string (or (plist-get o :count) 0)))
           :displayer (lambda (v w _)
                        (propertize (truncate-string-to-width v w)
                                    'face '(:foreground "tomato" :weight bold))))
    (:name "Last message"
           :width 50
           :getter    (lambda (o &rest _) (or (plist-get o :snippet) "")))
    (:name "Time"
           :width 13
           :getter    (lambda (o &rest _) (whatsapp--format-ts (plist-get o :timestamp)))
           :displayer (lambda (v w _)
                        (propertize (truncate-string-to-width v w)
                                    'face '(:height 0.8 :foreground "gray50")))))
  :objects      #'whatsapp--fetch-unread
  :snooze-period 'tomorrow
  :auto-refresh  "5 minutes"
  :row-colors    '("mint cream" "honeydew")
  :help-echo     (lambda (o)
                   (format "%s — \"%s\""
                           (or (plist-get o :name) (plist-get o :phone) "")
                           (or (plist-get o :snippet) "")))
  :actions
  `(("RET" "Reply"
     ,(lambda (o)
        (let* ((name (or (plist-get o :name) (plist-get o :phone) "?"))
               (jid  (plist-get o :jid))
               (msg  (read-string (format "Reply to %s: " name))))
          (unless (string-empty-p (string-trim msg))
            (whatsapp--send-async jid msg
              (lambda (_)
                ;; Optimistically remove from the unread list on successful reply
                (when-let ((tbl (vtable-current-table)))
                  (vtable-remove-object tbl o))))))))
    ("o" "Open chat in WhatsApp app"
     ,(lambda (o) (whatsapp--open-chat o)))
    ("c" "Copy last message to kill-ring"
     ,(lambda (o)
        (kill-new (or (plist-get o :snippet) ""))
        (aq--message "Copied: %s" (plist-get o :snippet))))
    ("f" "Send a funny for the day"
     ,(lambda (o) (whatsapp--action-funny o)))))

(provide 'whats-app)
;;; whats-app.el ends here
