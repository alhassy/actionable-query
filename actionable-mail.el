;;; actionable-mail.el --- Gmail INBOX view via actionable-query-defview  -*- lexical-binding: t; -*-
;;
;; Prerequisites (one-time setup):
;;   1. Generate a Gmail App Password at https://myaccount.google.com/apppasswords
;;   2. Add to ~/.netrc:   machine imap.gmail.com login <user> password <app-password>
;;   3. chmod 600 ~/.netrc
;;
;; The entry in ~/.netrc should look exactly like:
;;   machine imap.gmail.com login alhassy@gmail.com password abcdabcdabcdabcd
;; (16 chars, no spaces, no quotes — strip spaces Google's UI adds).

(require 'actionable-query)

;;; ─── IMAP message body rendering ────────────────────────────────────────────

(defvar amail--imap-body-cache (make-hash-table :test #'equal)
  "Hash: UID string → HTML/plain body string, populated during header fetch.")

(defun amail--imap-open-body (uid)
  "Render Gmail message UID in eww, using cache if available."
  (unless (and uid (not (string-empty-p uid)))
    (user-error "No UID for this message"))
  (if-let ((cached (gethash uid amail--imap-body-cache)))
      (amail--imap-render-body cached)
    (let ((buf (get-buffer-create (format "*actionable-mail-%s*" uid))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "⏳ Fetching message…" 'face '(:foreground "gray50")))))
      (pop-to-buffer buf)
      (aq--cli-async
       (list "python3" "-c"
             (format "
import imaplib, netrc, email, sys

creds = netrc.netrc().authenticators('imap.gmail.com')
user, _, password = creds
M = imaplib.IMAP4_SSL('imap.gmail.com')
M.login(user, password)
M.select('INBOX', readonly=True)
_, data = M.uid('fetch', b'%s', '(BODY.PEEK[])')
M.logout()

raw = data[0][1]
msg = email.message_from_bytes(raw)
html = None
plain = None
for part in msg.walk():
    ct = part.get_content_type()
    if ct == 'text/html' and html is None:
        html = part.get_payload(decode=True).decode('utf-8', errors='replace')
    elif ct == 'text/plain' and plain is None:
        plain = part.get_payload(decode=True).decode('utf-8', errors='replace')
print(html or plain or '')
" uid))
       #'identity
       (lambda (body)
         (kill-buffer buf)
         (amail--imap-render-body body))))))

(defun amail--imap-render-body (body)
  "Write BODY to a temp file and open it in eww."
  (let ((tmp (make-temp-file "actionable-mail-" nil ".html")))
    (with-temp-file tmp (insert body))
    (eww-open-file tmp)))

;;; ─── Unread Gmail via IMAP ───────────────────────────────────────────────────

(defconst amail--imap-fetch-script
  "
import imaplib, netrc, email, email.header, sys, re, base64

creds = netrc.netrc().authenticators('imap.gmail.com')
user, _, password = creds
M = imaplib.IMAP4_SSL('imap.gmail.com')
M.login(user, password)
M.select('INBOX', readonly=True)
_, data = M.uid('search', None, 'ALL')
uids = list(reversed(data[0].split()))
if not uids:
    M.logout(); sys.exit(0)
uid_set = b','.join(uids)

# FLAGS rides alongside headers in one FETCH — no extra round-trip.
_, hdr_msgs = M.uid('fetch', uid_set,
                    '(FLAGS BODY.PEEK[HEADER.FIELDS (FROM SUBJECT DATE MESSAGE-ID)])')

def decode_h(h):
    parts = email.header.decode_header(h or '')
    out = []
    for b, enc in parts:
        out.append(b.decode(enc or 'utf-8') if isinstance(b, bytes) else b)
    return ' '.join(' '.join(s.splitlines()) for s in out).replace('\t', ' ')

def extract_body(raw):
    msg = email.message_from_bytes(raw)
    html = plain = None
    for part in msg.walk():
        ct = part.get_content_type()
        if ct == 'text/html' and html is None:
            html = part.get_payload(decode=True).decode('utf-8', errors='replace')
        elif ct == 'text/plain' and plain is None:
            plain = part.get_payload(decode=True).decode('utf-8', errors='replace')
    return html or plain or ''

# Pass 1: emit header TSV lines; collect unread UIDs for a targeted body fetch.
unread_uids = []
for part in hdr_msgs:
    if not isinstance(part, tuple): continue
    envelope = part[0].decode()
    m = re.search(r'UID (\\d+)', envelope)
    uid = m.group(1) if m else ''
    fm = re.search(r'FLAGS \\(([^)]*)\\)', envelope)
    flags = fm.group(1) if fm else ''
    unread = 0 if '\\\\Seen' in flags else 1
    if unread and uid:
        unread_uids.append(uid.encode('ascii'))
    msg = email.message_from_bytes(part[1])
    print('\\t'.join([
        'UID:'     + uid,
        'SUBJECT:' + decode_h(msg['Subject']),
        'FROM:'    + decode_h(msg['From']),
        'DATE:'    + (msg['Date'] or ''),
        'MID:'     + (msg['Message-ID'] or ''),
        'UNREAD:'  + str(unread),
    ]))

# Pass 2: fetch bodies *only* for unread messages. Read-message bodies are
# lazily fetched on demand by `amail--imap-open-body' on cache miss.
bodies = {}
if unread_uids:
    _, body_msgs = M.uid('fetch', b','.join(unread_uids), '(BODY.PEEK[])')
    for part in body_msgs:
        if not isinstance(part, tuple): continue
        m = re.search(r'UID (\\d+)', part[0].decode())
        if m:
            bodies[m.group(1)] = extract_body(part[1])
M.logout()

# Body section: each entry is \"BODY:<uid>:<base64-encoded-body>\"
# Base64 avoids embedded newlines breaking the line-oriented parser.
for uid, body in bodies.items():
    encoded = base64.b64encode(body.encode('utf-8')).decode('ascii')
    print('BODY:' + uid + ':' + encoded)
"
  "Python script fetching all INBOX headers (with \\Seen flag) and unread-only bodies in one IMAP session.")

(defun amail--imap-parse-tsv (raw)
  "Parse output of `amail--imap-fetch-script' into plists.
Header lines are TAB-separated fields with KEY:value prefixes.
Body lines have the form BODY:<uid>:<base64> and are decoded into
`amail--imap-body-cache' as a side-effect."
  (clrhash amail--imap-body-cache)
  (let (headers)
    (dolist (line (split-string raw "\n" t))
      (if (string-prefix-p "BODY:" line)
          (let* ((rest  (substring line 5))
                 (colon (string-search ":" rest))
                 (uid   (substring rest 0 colon))
                 (b64   (substring rest (1+ colon))))
            (puthash uid (decode-coding-string (base64-decode-string b64) 'utf-8)
                     amail--imap-body-cache))
        (let ((plist (cl-loop with p = nil
                              for field in (split-string line "\t")
                              do (cond
                                  ((string-prefix-p "UID:"     field) (setq p (plist-put p :uid        (substring field 4))))
                                  ((string-prefix-p "SUBJECT:" field) (setq p (plist-put p :subject    (substring field 8))))
                                  ((string-prefix-p "FROM:"    field) (setq p (plist-put p :from       (substring field 5))))
                                  ((string-prefix-p "DATE:"    field) (setq p (plist-put p :date       (substring field 5))))
                                  ((string-prefix-p "MID:"     field) (setq p (plist-put p :message-id (substring field 4))))
                                  ((string-prefix-p "UNREAD:"  field) (setq p (plist-put p :unread     (equal (substring field 7) "1")))))
                              finally return p)))
          (when (plist-get plist :uid)
            (push plist headers)))))
    (nreverse headers)))

(defun amail--html-to-org (html)
  (shell-command-to-string
   (format "pandoc -f html -t org <<EOF\n%s\nEOF"
           (s-replace "`" "~" (s-replace "$" "\\$" html)))))

(defun amail--imap-archive (uid)
  (aq--cli-async
   (list "python3" "-c"
         (format "
import imaplib, netrc
creds = netrc.netrc().authenticators('imap.gmail.com')
user, _, password = creds
M = imaplib.IMAP4_SSL('imap.gmail.com')
M.login(user, password)
M.select('INBOX')
M.uid('copy', b'%s', '[Gmail]/All Mail')
M.uid('store', b'%s', '+FLAGS', '(\\\\Deleted)')
M.expunge()
M.logout()
print('ok')
" uid uid))
   #'identity
   (lambda (result)
     (unless (string-prefix-p "ok" (string-trim result))
       (aq--message "Archive may have failed for UID %s: %s" uid result)))))

(defun amail--imap-trash (uid)
  "Move Gmail message UID to [Gmail]/Trash (recoverable via browser)."
  (aq--cli-async
   (list "python3" "-c"
         (format "
import imaplib, netrc
creds = netrc.netrc().authenticators('imap.gmail.com')
user, _, password = creds
M = imaplib.IMAP4_SSL('imap.gmail.com')
M.login(user, password)
M.select('INBOX')
M.uid('copy', b'%s', '[Gmail]/Trash')
M.uid('store', b'%s', '+FLAGS', '(\\\\Deleted)')
M.expunge()
M.logout()
print('ok')
" uid uid))
   #'identity
   (lambda (result)
     (if (string-prefix-p "ok" (string-trim result))
         (aq--message "Trashed UID %s — recoverable from Gmail Trash." uid)
       (aq--message "Trash may have failed for UID %s: %s" uid result)))))

(defun amail--sanitize-email-from (from)
  "Strip display-name quotes and angle-bracket address from a FROM header value."
  (replace-regexp-in-string
   "\\`\"\\|\"\\'" ""
   (replace-regexp-in-string " *<[^>]*>" "" (or from ""))))

(defun amail--imap-fetch-inbox (callback)
  "Async 1-arg fn: fetch all of Gmail INBOX, grouped into Unread and Read.
Reads credentials from ~/.netrc (machine imap.gmail.com).
Delivers a grouped plist — (\"Unread (N)\" unread-list \"Inbox — read (M)\"
read-list) — so actionable-query's grouped-render path draws two titled vtables.
Bodies are pre-fetched only for unread messages; read-message bodies are
lazily fetched on demand by `amail--imap-open-body' on cache miss."
  (aq--cli-async
   (list "python3" "-c" amail--imap-fetch-script)
   #'amail--imap-parse-tsv
   (lambda (flat)
     (let (unread read)
       (dolist (m flat)
         (if (plist-get m :unread) (push m unread) (push m read)))
       (setq unread (nreverse unread)
             read   (nreverse read))
       (funcall callback
                (list (format "📬 Unread (%d)" (length unread)) unread
                      (format "📭 Inbox — read (%d)" (length read)) read))))))


(actionable-query-defview actionable-mail/gmail-inbox "📧 Gmail INBOX"
  :columns `((:name "Date"
                    :width 12
                    :getter    (lambda (o &rest _) (aq--format-pubdate (plist-get o :date)))
                    :displayer (lambda (v w _)
                                 (propertize (truncate-string-to-width v w)
                                             'face '(:height 0.8 :foreground "gray50"))))
             (:name "From"
                    :width 20
                    :getter    (lambda (o &rest _)
                                 (amail--sanitize-email-from (plist-get o :from)))
                    :formatter (lambda (v &rest _)
                                 (propertize v 'face '(:foreground "forest green" :weight bold))))
             (:name "Subject"
                    :width 55
                    :getter (lambda (o &rest _) (or (plist-get o :subject) ""))))
  :objects  #'amail--imap-fetch-inbox
  ;; :snooze-period 'forever
  ;; :auto-refresh  "5 minutes"
  :row-colors       '("thistle" "thistle1" "thistle2" "thistle3")
  :help-echo (lambda (o) (format "From: %s · %s" (amail--sanitize-email-from (plist-get o :from)) (plist-get o :date)))
  :actions `(("RET" "Open body in Emacs (org-mode)"
              ,(lambda (o)
                 (amail--imap-open-body (plist-get o :uid))))
             ("d" "Move to Trash (recoverable)"
              ,(lambda (o)
                 (let ((tbl  (vtable-current-table))
                       (uid  (plist-get o :uid))
                       (subj (plist-get o :subject)))
                   (when (yes-or-no-p (format "Trash \"%s\"? " subj))
                     (vtable-remove-object tbl o)
                     (amail--imap-trash uid)))))
             ("a" "Archive — move to All Mail (no confirmation)"
              ,(lambda (o)
                 (let* ((tbl    (vtable-current-table))
                        (tagged (plist-put (copy-sequence o) :subject
                                           (concat "[intentionally-kept] "
                                                   (or (plist-get o :subject) "")))))
                   (vtable-update-object tbl o tagged)
                   (run-at-time 0.8 nil
                                (lambda ()
                                  (vtable-remove-object tbl tagged)
                                  (amail--imap-archive (plist-get o :uid)))))))
             ("c" "Capture email as org TODO with full body"
              ,(lambda (o)
                 (let* ((uid        (plist-get o :uid))
                        (subject    (or (plist-get o :subject) "email"))
                        (html       (gethash uid amail--imap-body-cache ""))
                        (org-body   (if (string-empty-p html) ""
                                      (amail--html-to-org html)))
                        (gmail-url  (format "https://mail.google.com/mail/u/0/#inbox/%s" uid))
                        (capture-str (format "* TODO %s\n[[%s][Open in Gmail]]\n\n%s"
                                             subject gmail-url org-body)))
                   (org-capture-string capture-str "t"))))
             ("w" "Copy subject to kill-ring"
              ,(lambda (o)
                 (let ((s (or (plist-get o :subject) "")))
                   (kill-new s)
                   (aq--message "Copied: %s" s))))
             ("W" "Open in Gmail web"
              ,(lambda (o)
                 (let ((mid (plist-get o :message-id)))
                   (if (string-empty-p (or mid ""))
                       (browse-url "https://mail.google.com/mail/u/0/#inbox")
                     (browse-url (format "https://mail.google.com/mail/u/0/#search/rfc822msgid%%3A%s"
                                         (url-hexify-string mid)))))))))

(provide 'actionable-mail)
;;; actionable-mail.el ends here
