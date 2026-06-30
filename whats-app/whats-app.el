;;; whats-app.el --- WhatsApp views for Emacs via actionable-query-defview  -*- lexical-binding: t; -*-
;;
;; Prerequisites (one-time setup):
;;   1. cd ~/actionable-query && npm install @whiskeysockets/baileys qrcode-terminal
;;   2. M-x whatsapp/auth   — scan QR code; session saved to whats-app/whatsapp-session/
;;   3. Done.  All subsequent calls are fully headless — no browser, no GUI.
;;

(require 'actionable-query)

(declare-function aq--heart-p "aq-state-dismissal")

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
  (let* ((buf     (generate-new-buffer " *whatsapp-send*"))
         (err-buf (generate-new-buffer " *whatsapp-send-err*")))
    (make-process
     :name    "whatsapp-send"
     :command (list "node" whatsapp-cli-path "send" jid msg)
     :buffer  buf
     :stderr  (make-pipe-process :name "whatsapp-send-err"
                                 :buffer err-buf
                                 :sentinel (lambda (p _)
                                             (when (eq (process-status p) 'closed)
                                               (kill-buffer (process-buffer p)))))
     :sentinel
     (lambda (proc _)
       (when (eq (process-status proc) 'exit)
         (let* ((raw      (with-current-buffer (process-buffer proc) (buffer-string)))
                (err      (with-current-buffer err-buf (buffer-string)))
                (json-str (when (string-match "{[^}]*}" raw) (match-string 0 raw)))
                (result   (and json-str
                               (condition-case _ (json-parse-string json-str :object-type 'plist) (error nil)))))
           (kill-buffer (process-buffer proc))
           (cond
            ((plist-get result :ok)
             (aq--message "WhatsApp: sent to %s" jid))
            ((string-match-p "Session rejected\\|Run: node .*auth" err)
             (message "WhatsApp session expired — grab your phone, open WhatsApp → Settings → Linked Devices → Link a Device, and scan the QR code that's about to appear, then retry.")
             (whatsapp/auth))
            (t
             (aq--message "WhatsApp send failed: %s"
                          (string-trim (if (string-empty-p (string-trim raw)) err raw)))))
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
            ;; Monday dua source: https://www.duas.org/monday.htm
            (1 ["Magnificent Monday to you, %s! 🌟"
                "Monday Mubarak, %s — may this week open with blessings."
                "A Marvel of a Monday, %s — the week is yours to shape! 💪"
                "%s, a Monday dua for you:\nاَللَّهُمَّ أَوْلِنِي فِي كُلِّ يَومِ ٱثْنَيْنِ نِعْمَتَيْنِ مِنْكَ ثِنْتَيْنِ سَعَادَةً فِي أَوَّلِهِ بِطَاعَتِكَ وَنِعْمَةً فِي آخِرِهِ بِمَغْفِرَتِكَ\nGod, give me two gifts every Monday: a great start doing what You ask, and a clean slate by the end. 🤲"])
            ;; Tuesday dua source: https://www.duas.org/tuesday.htm
            (2 ["Terrific Tuesday, %s! ✨"
                "Tuesday Takeover, %s — may you conquer what yesterday left undone."
                "Two-terrific-Tuesday, %s — one day wiser than Monday! 😄"
                "%s, a Tuesday dua for you:\nاَللَّهُمَّ وَهَبْ لِي فِي ٱلثُّلاثَاءِ ثَلاثاً لاَ تَدَعْ لِي ذَنْباً إِلاَّ غَفَرْتَهُ وَلاَ غَمّاً إِلاَّ أَذْهَبْتَهُ وَلاَ عَدُوّاً إِلاَّ دَفَعْتَهُ\nGod, give me three things every Tuesday: forgive every sin I carry, lift every worry I'm under, and turn back every enemy against me. 🤲"])
            ;; Wednesday dua source: https://www.duas.org/wednesday.htm
            (3 ["Wonderful Wednesday, %s! 🌿"
                "Midweek magic, %s — you've made it halfway! 🎯"
                "%s, a Wednesday dua for you:\nاَللَّهُمَّ ٱقْضِ لِي فِي ٱلأَرْبِعَاءِ اَرْبَعاً إِجْعَلْ قُوَّتِي فِي طَاعَتِكَ وَنَشَاطِي فِي عِبَادَتِكَ وَرَغْبَتِي فِي ثَوَابِكَ وَزُهْدِي فِيمَا يُوجِبُ لِي أَلِيمَ عِقَابِكَ\nGod, give me four things every Wednesday: strength to obey You, energy to worship You, hunger for Your reward, and the sense to walk away from anything that earns Your punishment. 🤲"])
            ;; Thursday dua source: https://www.duas.org/thursday.htm
            (4 ["Tremendous Thursday, %s! 🚀"
                "Al-Khamis Mubarak, %s — may the eve of Jummah bring you peace and anticipation. 🌙"
                "Thor's Day, %s — strike your goals with full force today! ⚡"
                "%s, a Thursday dua for you:\nاَللَّهُمَّ ٱقْضِ لِي فِي ٱلْخَمِيسِ خَمْساً لاَ يَتَّسِعُ لَهَا إِلاَّ كَرَمُكَ وَلاَ يُطِيقُهَا إِلاَّ نِعَمُكَ سَلامَةً أَقْوَىٰ بِهَا عَلَىٰ طَاعَتِكَ وَعِبَادَةً أَسْتَحِقُّ بِهَا جَزِيلَ مَثُوبَتِكَ وَسَعَةً فِي ٱلْحَالِ مِنَ ٱلرِّزْقِ ٱلْحَلالِ وَأَنْ تُؤْمِنَنِي فِي مَوَاقِفِ ٱلْخَوْفِ بِأَمْنِكَ وَتَجْعَلَنِي مِنْ طَوَارِقِ ٱلْهُمُومِ وَٱلْغُمُومِ فِي حِصْنِكَ\nGod, give me five things every Thursday, things only Your generosity can cover and only Your favor can sustain: the health to obey You, the worship that earns Your reward, an honest living with room to breathe, safety wherever fear finds me, and shelter from worry and grief. 🤲"])
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

(defun whatsapp--action-funny (o view-name)
  "Send a day-aware greeting + joke to contact object O, snoozing it in VIEW-NAME until Friday on success."
  (let* ((name (whatsapp--first-name (or (plist-get o :name) (plist-get o :phone) "?")))
         (jid  (plist-get o :jid))
         (msg  (whatsapp--funny-message name))
         (fri  (whatsapp--snooze-until-friday)))
    (if (y-or-n-p (format "[Send the following] %s " msg))
        (progn
          (aq--message "Sending greeting to %s…" name)
          (whatsapp--send-async jid msg
            (lambda (result)
              (if (plist-get result :ok)
                  (progn (whatsapp--open-chat o)
                         (aq--dismiss-until view-name (aq--obj-id o) fri)
                         (when-let ((tbl (vtable-current-table)))
                           (vtable-remove-object tbl o))
                         (aq--message "Sent & snoozed until Friday (%s)!" fri))
                (aq--message "Failed to send greeting to %s." name)))))
      (message "Cancelled."))))

(defun whatsapp--funny-message (name)
  "Build a day-aware greeting for NAME with a joke appended."
  (let ((art   (fortune :kind 'joke))
        (emoji (seq-random-elt [😁 💐 🌇 🥳 🥸 🤲 🚴 🫎 🍉 🍁])))
    (format "%s %s\n```\n%s\n```" (whatsapp--day-message name) emoji art)))

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
  :prose "RET to message · h to heart · H to toggle hearted-only · f to send day greeting + funny, snooze until Friday."
  :columns
  `((:name "♥" :width 3 :align center
           ;; Leftmost heart indicator: ❤️ for hearted contacts, faint · else.
           ;; Width 3: the ❤️ emoji (heart + variation selector) renders ~2
           ;; cells, so a width-2 column truncates it to an ellipsis.
           :getter (lambda (o &rest _)
                     (if (aq--heart-p whatsapp--contacts-view o)
                         "❤️"
                       (propertize "·" 'face '(:foreground "gray70")))))
    (:name "Name"
           :width 24
           :getter    (lambda (o &rest _) (or (plist-get o :name) ""))
           :formatter (lambda (v &rest _)
                        (propertize v 'face '(:foreground "deep sky blue" :weight bold))))
    (:name "Last message"  ; no :width -> auto-size to widest snippet
           :getter    (lambda (o &rest _)
                        (let ((s  (plist-get o :snippet))
                              (ts (plist-get o :timestamp)))
                          (cond
                           ;; Real text from the last message.
                           ((and s (not (string-empty-p s))) s)
                           ;; A recent exchange exists but the last message had
                           ;; no text (media/sticker/voice) --- note it + when.
                           ((and ts (not (string-empty-p ts)))
                            (format "🖼️ media · %s" (substring ts 0 10)))
                           ;; No conversation yet --- fall back to the phone.
                           (t (concat "☎ " (or (plist-get o :phone) ""))))))
           :displayer (lambda (v w _)
                        (propertize (truncate-string-to-width v w nil nil "…")
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
    ("f" "Send day greeting + funny, snooze until Friday"
     ,(lambda (o) (whatsapp--action-funny o whatsapp--contacts-view)))))

;;; ─── View 2: Unread messages ─────────────────────────────────────────────────

(defun whatsapp--fetch-unread (callback)
  "Async 1-arg fn: fetch unread chats from whatsapp-cli.js and deliver to CALLBACK.
Each object is a plist: :jid :name :phone :snippet :timestamp :count."
  (whatsapp--ensure-session)
  (whatsapp--cli-async (whatsapp--node-argv "unread")
                       callback))

(defconst whatsapp--unread-view "💬 WhatsApp Unread")

(defun whatsapp--unread-prose ()
  "Header prose for the unread view."
  "Unread WhatsApp messages — press RET to reply, 'o' to open in app, 'f' for a day greeting + funny.")

(actionable-query-defview whatsapp/unread whatsapp--unread-view
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
    ("f" "Send day greeting + funny, snooze until Friday"
     ,(lambda (o) (whatsapp--action-funny o whatsapp--unread-view)))))

(provide 'whats-app)
;;; whats-app.el ends here
