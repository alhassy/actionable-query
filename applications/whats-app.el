;;; whats-app.el --- WhatsApp views for Emacs via actionable-query-defview  -*- lexical-binding: t; -*-
;;
;; Prerequisites (one-time setup):
;;   1. wuzapi binary --- auto-installed via Homebrew on first M-x whatsapp/auth.
;;   2. Install wasabi + its acp dep in Emacs:
;;        (package-vc-install '(acp    :url "https://github.com/xenodium/acp"))
;;        (package-vc-install '(wasabi :url "https://github.com/xenodium/wasabi"))
;;   3. M-x whatsapp/auth  --- scans QR code; session saved by wuzapi.
;;   4. Done.  All subsequent calls are fully headless.
;;

(require 'actionable-query)
(require 'wasabi)

;;; ─── wuzapi auto-install ─────────────────────────────────────────────────────

(defun whatsapp--ensure-wuzapi ()
  "Install wuzapi via Homebrew if not already on PATH.
Blocks with a compile buffer until brew finishes, then errors if still missing."
  (unless (executable-find "wuzapi")
    (message "wuzapi not found --- installing via Homebrew (one-time)…")
    (let ((exit (call-process "brew" nil "*wuzapi-install*" t
                              "install" "asternic/wuzapi/wuzapi")))
      (unless (zerop exit)
        ;; tap may be missing on a fresh machine
        (call-process "brew" nil "*wuzapi-install*" t
                      "tap" "asternic/wuzapi")
        (call-process "brew" nil "*wuzapi-install*" t
                      "install" "asternic/wuzapi/wuzapi")))
    (unless (executable-find "wuzapi")
      (error "wuzapi install failed --- see *wuzapi-install* for details"))))

(declare-function aq--heart-p "aq-state-dismissal")

(defun whatsapp--ensure-wasabi ()
  "Start wasabi if its `*Wasabi*' buffer isn't up yet, so the views are
self-contained --- no manual `M-x wasabi' needed before `C-c d'.  Returns
non-nil once the buffer exists.  Note wasabi populates its cache
asynchronously, so the first fetch after a cold start may still be empty."
  (unless (get-buffer "*Wasabi*")
    (save-window-excursion (wasabi)))
  (get-buffer "*Wasabi*"))

;;; ─── low-level adapter ───────────────────────────────────────────────────────

(defun whatsapp--jid->phone (jid)
  "Format a real phone JID (`NNNN@s.whatsapp.net') as +NNNN, else \"\".
`@lid' (linked-device) and other opaque JIDs are not phone numbers ---
their digits are an internal id, so we return \"\" rather than a bogus +id."
  (if (and jid (string-suffix-p "@s.whatsapp.net" jid))
      (let ((digits (replace-regexp-in-string "@.*" "" jid)))
        (if (string-empty-p digits) "" (concat "+" digits)))
    ""))

(defun whatsapp--contact-name (info)
  "Human name for a wasabi contact INFO alist, or nil when it has none.
Prefers the full name, then push-name, then first/business name; a blank
or missing value counts as no name.  Nil signals `whatsapp--wasabi-contact->plist'
to drop the entry --- a nameless contact (very common for `@lid'
linked-device JIDs) has nothing to show but its raw JID."
  (seq-some (lambda (k)
              (let ((v (map-elt info k)))
                (and (stringp v) (> (length (string-trim v)) 0) (string-trim v))))
            '(:full-name :push-name :first-name :business-name)))

(defun whatsapp--wasabi-contact->plist (entry)
  ;; entry → (:jid :name :phone :snippet :timestamp), or nil when nameless.
  (with-current-buffer (wasabi--buffer)
    (let* ((jid  (symbol-name (car entry)))
           (name (whatsapp--contact-name (cdr entry))))
      (when name
        (let* ((chat (seq-find (lambda (c) (string= (map-elt c :chat-jid) jid))
                               (map-elt (wasabi--state) :chats-index)))
               (ts   (and chat (map-elt chat :last-updated))))
          (list :jid jid :name name :phone (whatsapp--jid->phone jid)
                :snippet nil :timestamp ts))))))

(defun whatsapp--contacts-from-wasabi ()
  "List of contact plists pulled from wasabi's in-memory cache."
  (whatsapp--ensure-wasabi)
  (condition-case err
      (with-current-buffer (wasabi--buffer)
        (let ((contacts (map-elt (wasabi--state) :contacts)))
          (delq nil
                (mapcar (lambda (entry)
                          ;; Filter groups and broadcast lists.
                          (let ((jid (symbol-name (car entry))))
                            (unless (or (string-suffix-p "@g.us" jid)
                                        (string= jid "status@broadcast"))
                              (whatsapp--wasabi-contact->plist entry))))
                        contacts))))
    (error
     (message "whatsapp/contacts: wasabi not ready yet — run M-x wasabi first (%s)" (cadr err))
     nil)))

(defun whatsapp--recent-from-wasabi ()
  "List of recent-chat plists (sorted newest-first) from wasabi's chat index."
  (whatsapp--ensure-wasabi)
  (condition-case err
      (with-current-buffer (wasabi--buffer)
        (let* ((index (map-elt (wasabi--state) :chats-index))
               (personal (seq-filter
                          (lambda (c)
                            (let ((jid (map-elt c :chat-jid)))
                              (and jid
                                   (not (string-suffix-p "@g.us" jid))
                                   (not (string= jid "status@broadcast")))))
                          index)))
          (mapcar (lambda (c)
                    (list :jid       (map-elt c :chat-jid)
                          :name      (map-elt c :display-name)
                          :phone     (whatsapp--jid->phone (map-elt c :chat-jid))
                          :snippet   nil
                          :timestamp (map-elt c :last-updated)))
                  personal)))
    (error
     (message "whatsapp/recent: wasabi not ready yet — run M-x wasabi first (%s)" (cadr err))
     nil)))

(defun whatsapp--send (jid msg &optional on-done)
  "Send MSG to JID via wasabi; call ON-DONE with t/nil on completion."
  (with-current-buffer (wasabi--buffer)
    (wasabi--send-chat-send-text-request
     :phone jid :body msg
     :on-success (lambda (_) (when on-done (funcall on-done t)))
     :on-failure (lambda (_) (when on-done (funcall on-done nil))))))

(defun whatsapp--format-ts (ts)
  "Reformat ISO/wuzapi timestamp TS to \"May 09 14:30\"."
  (if (or (null ts) (string-empty-p ts)) ""
    (condition-case _
        (format-time-string "%b %d %H:%M"
                            (or (ignore-errors (parse-iso8601-time-string ts))
                                (date-to-time ts)))
      (error (substring ts 0 (min 10 (length ts)))))))

;;; ─── helpers shared by both views ───────────────────────────────────────────

(defun whatsapp--snooze-until-friday ()
  "Return YYYY-MM-DD for the coming Friday (next week if today is Friday)."
  (let* ((dow  (string-to-number (format-time-string "%u")))
         (days (if (= dow 5) 7 (mod (- 5 dow) 7))))
    (format-time-string "%Y-%m-%d" (time-add nil (* days 24 60 60)))))

(defun whatsapp--day-message (name)
  "Build a day-of-week greeting for NAME."
  (let* ((day (string-to-number (format-time-string "%u")))
         (phrases
          (pcase day
            (1 ["Magnificent Monday to you, %s! 🌟"
                "Monday Mubarak, %s — may this week open with blessings."
                "A Marvel of a Monday, %s — the week is yours to shape! 💪"
                "%s, a Monday dua for you:\nاَللَّهُمَّ أَوْلِنِي فِي كُلِّ يَومِ ٱثْنَيْنِ نِعْمَتَيْنِ مِنْكَ ثِنْتَيْنِ سَعَادَةً فِي أَوَّلِهِ بِطَاعَتِكَ وَنِعْمَةً فِي آخِرِهِ بِمَغْفِرَتِكَ\nGod, give me two gifts every Monday: a great start doing what You ask, and a clean slate by the end. 🤲"])
            (2 ["Terrific Tuesday, %s! ✨"
                "Tuesday Takeover, %s — may you conquer what yesterday left undone."
                "Two-terrific-Tuesday, %s — one day wiser than Monday! 😄"
                "%s, a Tuesday dua for you:\nاَللَّهُمَّ وَهَبْ لِي فِي ٱلثُّلاثَاءِ ثَلاثاً لاَ تَدَعْ لِي ذَنْباً إِلاَّ غَفَرْتَهُ وَلاَ غَمّاً إِلاَّ أَذْهَبْتَهُ وَلاَ عَدُوّاً إِلاَّ دَفَعْتَهُ\nGod, give me three things every Tuesday: forgive every sin I carry, lift every worry I'm under, and turn back every enemy against me. 🤲"])
            (3 ["Wonderful Wednesday, %s! 🌿"
                "Midweek magic, %s — you've made it halfway! 🎯"
                "%s, a Wednesday dua for you:\nاَللَّهُمَّ ٱقْضِ لِي فِي ٱلأَرْبِعَاءِ اَرْبَعاً إِجْعَلْ قُوَّتِي فِي طَاعَتِكَ وَنَشَاطِي فِي عِبَادَتِكَ وَرَغْبَتِي فِي ثَوَابِكَ وَزُهْدِي فِيمَا يُوجِبُ لِي أَلِيمَ عِقَابِكَ\nGod, give me four things every Wednesday: strength to obey You, energy to worship You, hunger for Your reward, and the sense to walk away from anything that earns Your punishment. 🤲"])
            (4 ["Tremendous Thursday, %s! 🚀"
                "Al-Khamis Mubarak, %s — may the eve of Jummah bring you peace and anticipation. 🌙"
                "Thor's Day, %s — strike your goals with full force today! ⚡"
                "%s, a Thursday dua for you:\nاَللَّهُمَّ ٱقْضِ لِي فِي ٱلْخَمِيسِ خَمْساً لاَ يَتَّسِعُ لَهَا إِلاَّ كَرَمُكَ وَلاَ يُطِيقُهَا إِلاَّ نِعَمُكَ سَلامَةً أَقْوَىٰ بِهَا عَلَىٰ طَاعَتِكَ وَعِبَادَةً أَسْتَحِقُّ بِهَا جَزِيلَ مَثُوبَتِكَ وَسَعَةً فِي ٱلْحَالِ مِنَ ٱلرِّزْقِ ٱلْحَلالِ وَأَنْ تُؤْمِنَنِي فِي مَوَاقِفِ ٱلْخَوْفِ بِأَمْنِكَ وَتَجْعَلَنِي مِنْ طَوَارِقِ ٱلْهُمُومِ وَٱلْغُمُومِ فِي حِصْنِكَ\nGod, give me five things every Thursday: the health to obey You, the worship that earns Your reward, an honest living with room to breathe, safety wherever fear finds me, and shelter from worry and grief. 🤲"])
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
  "Open WhatsApp desktop to O's chat via wa.me URL."
  (let ((phone (replace-regexp-in-string "[^0-9]" "" (or (plist-get o :phone) ""))))
    (browse-url (format "https://wa.me/%s" phone))))

(defun whatsapp--first-name (full-name)
  "First space-delimited token of FULL-NAME."
  (car (split-string full-name)))

(defun whatsapp--funny-message (name)
  "Day-aware greeting for NAME with a joke appended."
  (let ((art   (fortune :kind 'joke))
        (emoji (seq-random-elt [😁 💐 🌇 🥳 🥸 🤲 🚴 🫎 🍉 🍁])))
    (format "%s %s\n```\n%s\n```" (whatsapp--day-message name) emoji art)))

(defun whatsapp--action-funny (o view-name)
  "Send a greeting + joke to O, snooze in VIEW-NAME until Friday on success."
  (let* ((name (whatsapp--first-name (or (plist-get o :name) (plist-get o :phone) "?")))
         (jid  (plist-get o :jid))
         (msg  (whatsapp--funny-message name))
         (fri  (whatsapp--snooze-until-friday)))
    (if (y-or-n-p (format "[Send the following] %s " msg))
        (progn
          (aq--message "Sending greeting to %s…" name)
          (whatsapp--send jid msg
            (lambda (ok)
              (if ok
                  (progn (whatsapp--open-chat o)
                         (aq--dismiss-until view-name (aq--obj-id o) fri)
                         (when-let ((tbl (vtable-current-table)))
                           (vtable-remove-object tbl o))
                         (aq--message "Sent & snoozed until Friday (%s)!" fri))
                (aq--message "Failed to send greeting to %s." name)))))
      (message "Cancelled."))))

;;; ─── M-x whatsapp/auth ───────────────────────────────────────────────────────

(defun whatsapp/auth ()
  "Ensure wuzapi is installed, then start wasabi (shows QR if needed)."
  (interactive)
  (whatsapp--ensure-wuzapi)
  (wasabi))

;;; ─── View 1: Contacts ────────────────────────────────────────────────────────

(defconst whatsapp--contacts-view "📱 WhatsApp Contacts")

(defun whatsapp--fetch-contacts (callback)
  ;; ponytail: sync pull from wasabi cache; callback receives list immediately.
  (funcall callback (whatsapp--contacts-from-wasabi)))

(actionable-query-defview whatsapp/contacts "📱 WhatsApp Contacts"
  :prose "RET to open chat in wasabi · h to heart · H to toggle hearted-only · f to send day greeting + funny, snooze until Friday."
  :columns
  `((:name "♥" :width 3 :align center
           :getter (lambda (o &rest _)
                     (if (aq--heart-p whatsapp--contacts-view o)
                         "❤️"
                       (propertize "·" 'face '(:foreground "gray70")))))
    (:name "Name"
           :width 24
           :getter    (lambda (o &rest _) (or (plist-get o :name) ""))
           :formatter (lambda (v &rest _)
                        (propertize v 'face '(:foreground "deep sky blue" :weight bold))))
    (:name "Last seen"
           :getter    (lambda (o &rest _)
                        ;; wasabi only knows a last-interaction time for chats
                        ;; you've actually opened; the vast majority of contacts
                        ;; have none, so say so plainly rather than show a bogus
                        ;; blank/phone.
                        (let ((ts (plist-get o :timestamp)))
                          (if (and ts (not (string-empty-p (whatsapp--format-ts ts))))
                              (whatsapp--format-ts ts)
                            "Haven't talked to this person in ages!")))
           :displayer (lambda (v w _)
                        (propertize (truncate-string-to-width v w nil nil "…")
                                    'face '(:foreground "gray60")))))
  :objects  #'whatsapp--fetch-contacts
  :hearting t
  :row-colors '("alice blue" "lavender")
  :help-echo (lambda (o) (format "%s · %s" (or (plist-get o :name) "") (or (plist-get o :phone) "")))
  :actions
  `(("RET" "Open chat in wasabi"
     ,(lambda (o)
        (let ((jid  (plist-get o :jid))
              (name (or (plist-get o :name) (plist-get o :phone))))
          (run-at-time 0 nil
            (lambda ()
              (wasabi)
              (with-current-buffer (wasabi--buffer)
                (wasabi--send-chat-history-request
                 :chat-jid jid :contact-name name)))))))
    ("c" "Copy JID to kill-ring"
     ,(lambda (o)
        (kill-new (plist-get o :jid))
        (aq--message "Copied: %s" (plist-get o :jid))))
    ("f" "Send day greeting + funny, snooze until Friday"
     ,(lambda (o) (whatsapp--action-funny o whatsapp--contacts-view)))))

;;; ─── View 2: Recent chats ────────────────────────────────────────────────────

(defconst whatsapp--recent-view "💬 WhatsApp Recent")

(defun whatsapp--fetch-recent (callback)
  ;; ponytail: sync pull; wasabi's index is already sorted newest-first.
  (funcall callback (whatsapp--recent-from-wasabi)))

(actionable-query-defview whatsapp/recent whatsapp--recent-view
  :prose "Recent WhatsApp chats — RET to reply, 'o' to open in app, 'f' for a day greeting + funny."
  :columns
  `((:name "From"
           :width 22
           :getter    (lambda (o &rest _) (or (plist-get o :name) (plist-get o :phone) ""))
           :formatter (lambda (v &rest _)
                        (propertize v 'face '(:foreground "green3" :weight bold))))
    (:name "Last active"
           :width 13
           :getter    (lambda (o &rest _) (whatsapp--format-ts (plist-get o :timestamp)))
           :displayer (lambda (v w _)
                        (propertize (truncate-string-to-width v w)
                                    'face '(:height 0.8 :foreground "gray50")))))
  :objects      #'whatsapp--fetch-recent
  :snooze-period 'tomorrow
  :auto-refresh  "5 minutes"
  :row-colors    '("mint cream" "honeydew")
  :help-echo     (lambda (o) (or (plist-get o :name) (plist-get o :phone) ""))
  :actions
  `(("RET" "Send a message"
     ,(lambda (o)
        (let* ((name (or (plist-get o :name) (plist-get o :phone) "?"))
               (jid  (plist-get o :jid))
               (msg  (read-string (format "Reply to %s: " name))))
          (unless (string-empty-p (string-trim msg))
            (whatsapp--send jid msg
              (lambda (ok)
                (when (and ok (vtable-current-table))
                  (vtable-remove-object (vtable-current-table) o))))))))
    ("o" "Open chat in WhatsApp app"
     ,(lambda (o) (whatsapp--open-chat o)))
    ("c" "Copy JID to kill-ring"
     ,(lambda (o)
        (kill-new (plist-get o :jid))
        (aq--message "Copied: %s" (plist-get o :jid))))
    ("f" "Send day greeting + funny, snooze until Friday"
     ,(lambda (o) (whatsapp--action-funny o whatsapp--recent-view)))))

;; Backwards-compat alias --- dashboard.el calls whatsapp/contacts which still works.
;; whatsapp/unread was the old name for the second view.
(defalias 'whatsapp/unread 'whatsapp/recent)

(provide 'whats-app)
;;; whats-app.el ends here
