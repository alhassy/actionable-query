;;; org-agenda-gerrit.el --- Gerrit/Jira views via actionable-query-defview -*- lexical-binding: t; -*-


(require 'actionable-query)
(require 'easy-access)
(require 'auth-source)              ; Jira PAT lookup (~/.authinfo.gpg)
(require 'url-parse)                ; `url-host' for the auth-source key
;; Generic graph primitives (BFS edge-map + Union-Find) live in the sibling
;; graph.el; ensure its directory is on the load-path before requiring.
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'graph)
(eval-when-compile (require 'easy-access) (easy-access-mode 1))
(easy-access-mode 1)

;; ── Configuration vars (set these in private.el / init.org) ──────────────────

(defvar org-agenda-gerrit-ssh-command "/usr/bin/ssh"
  "Path to the SSH binary for Gerrit queries.")

(defvar org-agenda-gerrit-ssh-host "gerrit"
  "SSH host alias for Gerrit, as defined in ~/.ssh/config.")

(defvar org-agenda-gerrit-base-url ""
  "Base URL for Gerrit web UI links, e.g. \"https://gerrit.example.com\".")

(defvar org-agenda-gerrit-project-path "/c/+/"
  "URL fragment between base URL and change number.")

(defvar org-agenda-gerrit-jira-base-url ""
  "Base URL for Jira ticket links, e.g. \"https://jira.example.com/browse\".")

(defvar org-agenda-gerrit-user user-login-name
  "Gerrit/Jira username for ownership and assignment queries.")

(defvar org-agenda-gerrit-jira-token nil
  "Jira Personal Access Token for REST calls, or nil to look it up.
When nil, `org-agenda-gerrit--jira-token' falls back to `auth-source'
\(e.g. ~/.authinfo or ~/.authinfo.gpg) keyed on the Jira host.

Why a PAT and not a password?  Jira Data Center authenticates the REST
API through Seraph; a few failed basic-auth logins (e.g. a stale ~/.netrc
password replayed on every weekly-review run) trip a CAPTCHA lock, after
which curl gets `403 X-Authentication-Denied-Reason: CAPTCHA_CHALLENGE'
forever --- even though the browser still works via its session cookie.
A bearer PAT takes the OAuth code path instead, which the CAPTCHA lock
never touches.

Reproduction steps (future-Musa, when the token expires or 401s):

  1. Generate a PAT in Jira:
       avatar (top-right) -> Profile -> Personal Access Tokens
       -> Create token -> copy it.
  2. Store it for `auth-source' in ~/.authinfo (chmod 600), as ONE line
     keyed on the host of `org-agenda-gerrit-jira-base-url':
       machine jira.example.com login <user> password <PAT>
     (Prefer ~/.authinfo.gpg if you want it encrypted at rest.)
  3. Remove any stale `password' line for the same host from ~/.netrc,
     so basic-auth is never replayed and can't re-arm the CAPTCHA lock.
  4. `M-x auth-source-forget-all-cached' (or restart Emacs) so the new
     secret is picked up; verify with
       (org-agenda-gerrit--query-jira (org-agenda-gerrit--done-jql) \"summary\").
  5. Sanity-check the raw endpoint from a shell:
       curl -s -o /dev/null -w '%s' -H 'Authorization: Bearer <PAT>' \\
         https://jira.example.com/rest/api/2/myself
     A 200 means the PAT is good; 401 OAuth means regenerate.")

(defun org-agenda-gerrit--jira-host ()
  "Host portion of `org-agenda-gerrit-jira-base-url' (for auth-source lookup)."
  (url-host (url-generic-parse-url (org-agenda-gerrit--jira-rest-base))))

(defun org-agenda-gerrit--jira-token ()
  "Return the Jira PAT: `org-agenda-gerrit-jira-token' or an auth-source secret.
Looks up `auth-source' on the Jira host (any login) when the variable is
unset; signals a clear error if neither yields a token."
  (or org-agenda-gerrit-jira-token
      (let* ((found (car (auth-source-search :host (org-agenda-gerrit--jira-host)
                                             :require '(:secret) :max 1)))
             (secret (plist-get found :secret)))
        (cond ((functionp secret) (funcall secret))
              (secret secret)
              (t (user-error
                  "No Jira token: set `org-agenda-gerrit-jira-token' or add a %s entry to ~/.authinfo.gpg"
                  (org-agenda-gerrit--jira-host)))))))

(defun org-agenda-gerrit--jira-curl-args (url)
  "Return the curl argv for a Jira REST GET of URL, authed via bearer PAT.
Replaces the old `-n'/netrc basic-auth path (CAPTCHA-locked on this
instance)."
  (list "-s" "-H" (format "Authorization: Bearer %s" (org-agenda-gerrit--jira-token))
        url))

(defvar org-agenda-gerrit-jira-ticket-regex nil
  "Regex to extract Jira ticket IDs from commit messages.
Must have one capture group yielding the ticket ID.  Nil disables extraction.")

(defvar org-agenda-gerrit--bridge-max-depth 5
  "Max BFS hops when resolving bridge changes between stacks.")


;; ── work-item struct ──────────────────────────────────────────────────────────

(cl-defstruct (org-agenda-gerrit-item (:constructor org-agenda-gerrit-item-create))
  jira title tip-url reviewers reviewer-users author reporter urgent
  age stack-size max-patchsets comment-count avg-shirt-size
  ci-status has-code-review blocked-by-parent)

;; ── JSON parsing ──────────────────────────────────────────────────────────────

(defun org-agenda-gerrit--parse-json (raw)
  "Parse RAW (newline-delimited JSON from Gerrit SSH) into change alists."
  (->> (s-lines raw)
       (-filter #'s-present?)
       (-map (lambda (line) (ignore-errors (json-read-from-string line))))
       (-filter #'identity)
       (-filter (lambda (obj) (not (assoc 'type obj))))))

;; ── Jira ticket extraction ────────────────────────────────────────────────────

(defun org-agenda-gerrit--extract-jira-tickets (change)
  "Return a single-element list of the primary Jira ticket in CHANGE, or nil."
  (when org-agenda-gerrit-jira-ticket-regex
    (let ((msg (or ('commitMessage change) "")))
      (-when-let (matches (->> (s-match-strings-all org-agenda-gerrit-jira-ticket-regex msg)
                               (-map #'cadr)))
        (last matches)))))

(defun org-agenda-gerrit--stack-jira-tickets (stack)
  "Return Jira tickets shared by every change in STACK (intersection, falling back to tip)."
  (let ((all (-map #'org-agenda-gerrit--extract-jira-tickets stack)))
    (when all
      (or (-reduce-from
           (lambda (acc tix) (-intersection acc tix))
           (car all) (cdr all))
          (-1 all)))))

(defun org-agenda-gerrit--my-open-change-jira-ids ()
  "Return the set of Jira IDs referenced by any of my open Gerrit changes."
  (->> (org-agenda-gerrit--query-sync "status:open owner:self -is:abandoned")
       (-mapcat #'org-agenda-gerrit--extract-jira-tickets)
       -uniq))

(defun org-agenda-gerrit--my-open-change-jira-ids-async (callback)
  "Async variant: deliver my open changes' Jira IDs to CALLBACK; never blocks."
  (org-agenda-gerrit--query-async
   "status:open owner:self -is:abandoned"
   (lambda (changes)
     (funcall callback
              (->> changes
                   (-mapcat #'org-agenda-gerrit--extract-jira-tickets)
                   -uniq)))))

;; ── Jira title cache ──────────────────────────────────────────────────────────

(defvar org-agenda-gerrit--jira-title-cache (make-hash-table :test 'equal)
  "Cache: Jira ticket ID → summary string.  Populated by batch fetch.")

;; Org-tree association is delegated to core: each view's `:org-upsert'
;; (built by `org-agenda-gerrit--org-upsert') content-joins the item's Jira ID
;; against the headings in `org-default-notes-file' --- finding an existing tree
;; or minting one --- via `actionable-query-upsert-org-tree'.  No local marker
;; cache to build/invalidate any more.

(defun org-agenda-gerrit--jira-rest-base ()
  "Derive the Jira REST base from `org-agenda-gerrit-jira-base-url'."
  (replace-regexp-in-string "/browse\\'" "" org-agenda-gerrit-jira-base-url))

(defun org-agenda-gerrit--fetch-jira-titles (tickets)
  "Batch-fetch Jira summaries for TICKETS into `org-agenda-gerrit--jira-title-cache'."
  (let ((uncached (-filter (lambda (tk) (not (gethash tk org-agenda-gerrit--jira-title-cache)))
                           tickets)))
    (when uncached
      (let* ((jql (format "key in (%s)" (s-join "," uncached)))
             (url (format "%s/rest/api/2/search?jql=%s&fields=summary&maxResults=%d"
                          (org-agenda-gerrit--jira-rest-base)
                          (url-hexify-string jql)
                          (length uncached)))
             (timer-idle-list nil) (timer-list nil)
             (raw (with-output-to-string
                    (with-current-buffer standard-output
                      (apply #'call-process "curl" nil t nil
                             (org-agenda-gerrit--jira-curl-args url))))))
        (org-agenda-gerrit--parse-jira-titles raw)))))

(defun org-agenda-gerrit--parse-jira-titles (raw)
  "Stash Jira summaries from RAW JSON search response into the title cache."
  (let ((json-obj (ignore-errors (json-read-from-string raw))))
    (when json-obj
      (dolist (issue (append ('issues json-obj) nil))
        (let ((key ('key issue))
              (summary ('fields 'summary issue)))
          (when (and key summary)
            (puthash key summary org-agenda-gerrit--jira-title-cache)))))))

(defun org-agenda-gerrit--fetch-jira-titles-async (tickets callback)
  "Async variant of `org-agenda-gerrit--fetch-jira-titles'.
Batch-fetch summaries for TICKETS into the title cache via a
non-blocking `curl' process, then invoke CALLBACK with no args.
When every ticket is already cached (or TICKETS is empty), CALLBACK
runs immediately --- no process is spawned."
  (let ((uncached (-filter (lambda (tk) (not (gethash tk org-agenda-gerrit--jira-title-cache)))
                           tickets)))
    (if (not uncached)
        (funcall callback)
      (let* ((jql (format "key in (%s)" (s-join "," uncached)))
             (url (format "%s/rest/api/2/search?jql=%s&fields=summary&maxResults=%d"
                          (org-agenda-gerrit--jira-rest-base)
                          (url-hexify-string jql)
                          (length uncached)))
             (buf  (generate-new-buffer " *neato-jira-titles*"))
             (proc (apply #'start-process "neato-jira-titles" buf "curl"
                          (org-agenda-gerrit--jira-curl-args url))))
        (set-process-sentinel
         proc
         (lambda (p _)
           (when (memq (process-status p) '(exit signal))
             (let ((raw (with-current-buffer (process-buffer p) (buffer-string))))
               (kill-buffer (process-buffer p))
               (org-agenda-gerrit--parse-jira-titles raw)
               (funcall callback)))))))))

(defun org-agenda-gerrit--get-jira-title (ticket)
  "Return the cached Jira summary for TICKET, or nil."
  (gethash ticket org-agenda-gerrit--jira-title-cache))

(defun org-agenda-gerrit--jira-search-url (jql &optional fields max)
  "Build the Jira REST /search URL for JQL, FIELDS (default summary), MAX (50)."
  (format "%s/rest/api/2/search?jql=%s&fields=%s&maxResults=%d"
          (org-agenda-gerrit--jira-rest-base)
          (url-hexify-string jql)
          (or fields "summary")
          (or max 50)))

(defun org-agenda-gerrit--parse-jira-issues (raw)
  "Parse RAW Jira /search JSON into a list of issue alists."
  (let ((json-obj (ignore-errors (json-read-from-string raw))))
    (when json-obj (append ('issues json-obj) nil))))

(defun org-agenda-gerrit--query-jira (jql &optional fields)
  "Run JQL against Jira via curl (bearer PAT); return list of issue alists."
  (let* ((url (org-agenda-gerrit--jira-search-url jql fields))
         (timer-idle-list nil) (timer-list nil)
         (raw (with-output-to-string
                (with-current-buffer standard-output
                  (apply #'call-process "curl" nil t nil
                         (org-agenda-gerrit--jira-curl-args url))))))
    (org-agenda-gerrit--parse-jira-issues raw)))

(defun org-agenda-gerrit--query-jira-async (jql callback &optional fields)
  "Async variant of `org-agenda-gerrit--query-jira'.
Delivers the parsed list of issue alists to CALLBACK; never blocks."
  (let* ((url  (org-agenda-gerrit--jira-search-url jql fields))
         (buf  (generate-new-buffer " *neato-jira-query*"))
         (proc (apply #'start-process "neato-jira-query" buf "curl"
                      (org-agenda-gerrit--jira-curl-args url))))
    (set-process-sentinel
     proc
     (lambda (p _)
       (when (memq (process-status p) '(exit signal))
         (let ((raw (with-current-buffer (process-buffer p) (buffer-string))))
           (kill-buffer (process-buffer p))
           (funcall callback (org-agenda-gerrit--parse-jira-issues raw))))))))

;; ── Stack grouping ────────────────────────────────────────────────────────────

(defun org-agenda-gerrit--collect-neighbor-numbers (change)
  "Return deduplicated list of change numbers adjacent to CHANGE."
  (let (nums)
    (dolist (dep (append ('dependsOn change) nil))
      (push ('number dep) nums))
    (dolist (nb (append ('neededBy change) nil))
      (push ('number nb) nums))
    (-uniq nums)))

(defun org-agenda-gerrit--ssh-query-args (query-string &optional extra-flags)
  "Build the argv for a Gerrit SSH query of QUERY-STRING with EXTRA-FLAGS."
  (let* ((default-flags '("--current-patch-set" "--dependencies"
                          "--commit-message" "--comments"))
         (flags (or extra-flags default-flags)))
    `(,org-agenda-gerrit-ssh-host "gerrit" "query" "--format=JSON"
      ,@flags "--" ,query-string)))

(defun org-agenda-gerrit--query-sync (query-string &optional extra-flags)
  "Synchronous Gerrit SSH query; return list of change alists."
  (let* ((args (org-agenda-gerrit--ssh-query-args query-string extra-flags))
         (timer-idle-list nil) (timer-list nil)
         (raw (with-output-to-string
                (with-current-buffer standard-output
                  (apply #'call-process org-agenda-gerrit-ssh-command nil t nil args)))))
    (org-agenda-gerrit--parse-json raw)))

(defun org-agenda-gerrit--query-async (query-string callback &optional extra-flags)
  "Async variant of `org-agenda-gerrit--query-sync'.
Delivers the parsed list of change alists to CALLBACK; never blocks."
  (let* ((args (org-agenda-gerrit--ssh-query-args query-string extra-flags))
         (buf  (generate-new-buffer " *neato-gerrit-query*"))
         (proc (apply #'start-process "neato-gerrit-query" buf
                      org-agenda-gerrit-ssh-command args)))
    (set-process-sentinel
     proc
     (lambda (p _)
       (when (memq (process-status p) '(exit signal))
         (let ((raw (with-current-buffer (process-buffer p) (buffer-string))))
           (kill-buffer (process-buffer p))
           (funcall callback (org-agenda-gerrit--parse-json raw))))))))

;; The graph bookkeeping (BFS edge-map growth + Union-Find) is generic and
;; lives in `graph.el'.  Here we only adapt: a Gerrit change's id is its
;; `number'; its neighbors are `--collect-neighbor-numbers'; and a frontier
;; of ids is fetched from Gerrit by number.  Everything domain-specific
;; (the queries, the alist keys) stays in this file.

(defun org-agenda-gerrit--frontier-query (frontier)
  "Return the Gerrit query string matching the change numbers in FRONTIER."
  (s-join " OR " (-map (lambda (n) (format "change:%d" n)) frontier)))

(defun org-agenda-gerrit--changes-by-number (changes)
  "Return a hash-table mapping each change's `number' to the change alist."
  (let ((by-num (make-hash-table :test 'eql)))
    (dolist (c changes) (puthash ('number c) c by-num))
    by-num))

(defun org-agenda-gerrit--change-number (c) "Change number of C." ('number c))

(defun org-agenda-gerrit--build-full-edge-map (changes)
  "BFS edge map over CHANGES; return (EDGES PRESENT) hash-tables.
Synchronous --- retained for any blocking caller; the async pipeline
uses `org-agenda-gerrit--build-full-edge-map-async'."
  (-let* ((by-num (org-agenda-gerrit--changes-by-number changes))
          ((edges present visited)
           (graph-edge-map-seed
            (-map #'org-agenda-gerrit--change-number changes)
            (lambda (n) (org-agenda-gerrit--collect-neighbor-numbers (gethash n by-num))))))
    (cl-loop
     repeat org-agenda-gerrit--bridge-max-depth
     do (let ((frontier (graph-bfs-frontier edges visited)))
          (when (null frontier) (cl-return))
          (graph-edge-map-absorb
           (org-agenda-gerrit--query-sync (org-agenda-gerrit--frontier-query frontier)
                                          '("--dependencies"))
           #'org-agenda-gerrit--change-number
           #'org-agenda-gerrit--collect-neighbor-numbers
           edges visited)))
    (list edges present)))

(defun org-agenda-gerrit--build-full-edge-map-async (changes callback)
  "Async BFS edge map over CHANGES; deliver (EDGES PRESENT) to CALLBACK.
Each BFS depth level is one async Gerrit query whose sentinel fires
the next level, so the whole expansion runs off the input path ---no
`call-process' blocks Emacs while the user types."
  (-let* ((by-num (org-agenda-gerrit--changes-by-number changes))
          ((edges present visited)
           (graph-edge-map-seed
            (-map #'org-agenda-gerrit--change-number changes)
            (lambda (n) (org-agenda-gerrit--collect-neighbor-numbers (gethash n by-num))))))
    (cl-labels
        ((step (depth)
           (let ((frontier (graph-bfs-frontier edges visited)))
             (if (or (>= depth org-agenda-gerrit--bridge-max-depth)
                     (null frontier))
                 (funcall callback (list edges present))
               (org-agenda-gerrit--query-async
                (org-agenda-gerrit--frontier-query frontier)
                (lambda (intermediates)
                  (graph-edge-map-absorb intermediates
                                         #'org-agenda-gerrit--change-number
                                         #'org-agenda-gerrit--collect-neighbor-numbers
                                         edges visited)
                  (step (1+ depth)))
                '("--dependencies"))))))
      (step 0))))

(defun org-agenda-gerrit--stacks-from-edge-map (changes edges present)
  "Group CHANGES into dependency stacks given (EDGES PRESENT); base-first.
Delegates the Union-Find to `graph-union-find' (which works on numbers)
then maps the resulting number-components back to change alists."
  (let ((by-num (org-agenda-gerrit--changes-by-number changes)))
    (-map (lambda (component) (-map (lambda (n) (gethash n by-num)) component))
          (graph-union-find (-map #'org-agenda-gerrit--change-number changes)
                            edges present))))

(defun org-agenda-gerrit--group-into-stacks (changes)
  "Group CHANGES into dependency stacks via Union-Find; return list of stacks (base-first)."
  (if (null changes) nil
    (-let [(edges present) (org-agenda-gerrit--build-full-edge-map changes)]
      (org-agenda-gerrit--stacks-from-edge-map changes edges present))))

(defun org-agenda-gerrit--group-into-stacks-async (changes callback)
  "Async variant of `org-agenda-gerrit--group-into-stacks'.
Builds the edge map without blocking, then delivers the stacks to CALLBACK."
  (if (null changes)
      (funcall callback nil)
    (org-agenda-gerrit--build-full-edge-map-async
     changes
     (lambda (edge-map)
       (-let [(edges present) edge-map]
         (funcall callback
                  (org-agenda-gerrit--stacks-from-edge-map changes edges present)))))))

;; ── Metrics ───────────────────────────────────────────────────────────────────

(defun org-agenda-gerrit--strip-commit-tags (subject)
  "Strip leading [module] prefixes from SUBJECT."
  (if (string-match "\\`\\[.*?\\] *" subject)
      (substring subject (match-end 0))
    subject))

(defun org-agenda-gerrit--shirt-size-value (change)
  "Map CHANGE to numeric shirt size 1–5, or nil."
  (let* ((ps  ('currentPatchSet change))
         (ins ('sizeInsertions ps))
         (del ('sizeDeletions ps)))
    (when (and ins del)
      (let ((total (+ ins del)))
        (cond ((<= total 10)   1)
              ((<= total 50)   2)
              ((<= total 250)  3)
              ((<= total 1000) 4)
              (t               5))))))

(defun org-agenda-gerrit--shirt-size-label (value)
  "Convert numeric shirt size VALUE to label string."
  (pcase (round value)
    (1 "XS") (2 "S") (3 "M") (4 "L") (_ "XL")))

(defun org-agenda-gerrit--avg-shirt-size (stack)
  "Average numeric shirt size across STACK, or nil."
  (let ((vals (-keep #'org-agenda-gerrit--shirt-size-value stack)))
    (when vals (/ (-sum vals) (float (length vals))))))

(defun org-agenda-gerrit--ci-status (change)
  "Return `pass', `fail', or nil for CHANGE's Verified vote."
  (let* ((approvals (append ('currentPatchSet 'approvals change) nil))
         (verified (-first (lambda (a) (equal "Verified" ('type a))) approvals)))
    (when verified
      (let ((val (string-to-number (or ('value verified) "0"))))
        (if (< val 0) 'fail 'pass)))))

(defun org-agenda-gerrit--has-code-review (change owner-username)
  "Return non-nil if CHANGE has a positive Code-Review from a non-owner."
  (let ((approvals (append ('currentPatchSet 'approvals change) nil)))
    (-any-p (lambda (a)
              (and (equal "Code-Review" ('type a))
                   (> (string-to-number (or ('value a) "0")) 0)
                   (not (equal owner-username ('by 'username a)))))
            approvals)))

(defun org-agenda-gerrit--blocked-by-parent (change stack)
  "Return non-nil if CHANGE depends on an unmerged parent in STACK."
  (let ((deps (append ('dependsOn change) nil))
        (stack-nums (-map (lambda (c) ('number c)) stack)))
    (-any-p (lambda (d) (member ('number d) stack-nums)) deps)))

(defun org-agenda-gerrit--format-age (epoch)
  "Format EPOCH into a human-readable age string."
  (when epoch
    (let* ((seconds (- (float-time) epoch))
           (days (floor (/ seconds 86400))))
      (cond
       ((>= days 365) (format "%dy" (/ days 365)))
       ((>= days 30)  (format "%dmo" (/ days 30)))
       ((>= days 7)   (format "%dw" (/ days 7)))
       ((>= days 1)   (format "%dd" days))
       (t "today")))))

;; ── Reviewer extraction ───────────────────────────────────────────────────────

(defun org-agenda-gerrit--extract-reviewer-user-map (stack)
  "Return alist of (display-name . username) for reviewers in STACK."
  (let* ((owners (->> stack
                      (-map (lambda (c) ('owner 'username c)))
                      (-filter #'identity)
                      -uniq))
         (human-reviewers
          (->> stack
               (-mapcat
                (lambda (c)
                  (->> (append ('allReviewers c) nil)
                       (-filter
                        (lambda (rv)
                          (let ((name ('name rv))
                                (uname ('username rv)))
                            (and name uname
                                 (s-contains-p " " name)
                                 (not (member uname owners))))))
                       (-map (lambda (rv) (cons ('name rv) ('username rv)))))))
               (-filter #'identity)))
         (negative-voters
          (->> stack
               (-mapcat
                (lambda (c)
                  (->> (append ('currentPatchSet 'approvals c) nil)
                       (-filter
                        (lambda (a)
                          (and (equal "Code-Review" ('type a))
                               (< (string-to-number (or ('value a) "0")) 0))))
                       (-map (lambda (a)
                               (let ((by ('by a)))
                                 (when-let ((name ('name by))
                                            (uname ('username by)))
                                   (cons name uname))))))))
               (-filter #'identity))))
    (cl-remove-duplicates
     (append human-reviewers negative-voters)
     :key #'cdr :test #'equal)))

(defun org-agenda-gerrit--extract-reviewer-names (stack)
  "Return reviewer display name strings for STACK."
  (let ((with-usernames (-map #'car (org-agenda-gerrit--extract-reviewer-user-map stack)))
        (owners (->> stack
                     (-map (lambda (c) ('owner 'username c)))
                     (-filter #'identity)
                     -uniq))
        (name-only
         (->> stack
              (-mapcat
               (lambda (c)
                 (->> (append ('allReviewers c) nil)
                      (-filter
                       (lambda (rv)
                         (let ((name ('name rv))
                               (uname ('username rv)))
                           (and name (not uname)
                                (s-contains-p " " name)))))
                      (-map (lambda (rv) ('name rv))))))
              (-filter #'identity)
              -uniq)))
    (ignore owners)
    (-uniq (append with-usernames name-only))))

(defun org-agenda-gerrit--format-reviewer-names (names)
  "Format NAMES into natural English with Oxford comma, or nil when empty.
Returning nil on no NAMES lets callers `(or (…format-reviewer-names …) FALLBACK)'."
  (pcase (length names)
    (0 nil)
    (1 (0 names))
    (2 (format "%s and %s" (0 names) (1 names)))
    (_ (format "%s, and %s" (s-join ", " (butlast names)) (-1 names)))))

;; ── work-item construction ────────────────────────────────────────────────────

(defun org-agenda-gerrit--item-from-stack (stack)
  "Convert Gerrit dependency STACK into a `org-agenda-gerrit-item'."
  (let* ((tip       (-1 stack))
         (jira      (0 (org-agenda-gerrit--stack-jira-tickets stack)))
         (jira-title (when jira (org-agenda-gerrit--get-jira-title jira))))
    (org-agenda-gerrit-item-create
     :jira  jira
     :title (or jira-title
                (when-let ((subj ('subject tip)))
                  (org-agenda-gerrit--strip-commit-tags subj))
                "untitled")
     :tip-url (format "%s%s%s"
                      org-agenda-gerrit-base-url org-agenda-gerrit-project-path
                      ('number tip))
     :reviewers     (org-agenda-gerrit--extract-reviewer-names stack)
     :reviewer-users (org-agenda-gerrit--extract-reviewer-user-map stack)
     :author        ('owner 'name tip)
     :age           ('lastUpdated tip)
     :stack-size    (length stack)
     :max-patchsets (when stack
                      (-max (-map (lambda (c)
                                    (or ('currentPatchSet 'number c) 1))
                                  stack)))
     :comment-count (-sum (-map (lambda (c) (length ('comments c))) stack))
     :avg-shirt-size (org-agenda-gerrit--avg-shirt-size stack)
     :ci-status      (org-agenda-gerrit--ci-status tip)
     :has-code-review (org-agenda-gerrit--has-code-review tip ('owner 'username tip))
     :blocked-by-parent (org-agenda-gerrit--blocked-by-parent tip stack))))

(defun org-agenda-gerrit--items-from-stack (stack &optional by-author)
  "Split STACK into one item per group (by ticket, or ticket×author when BY-AUTHOR)."
  (let* ((key-fn
          (if by-author
              (lambda (c)
                (cons (0 (org-agenda-gerrit--extract-jira-tickets c))
                      ('owner 'username c)))
            (lambda (c) (0 (org-agenda-gerrit--extract-jira-tickets c)))))
         (ticket-of (if by-author #'caar #'car))
         (grouped (-group-by key-fn stack))
         (non-nil-groups (-filter (lambda (g) (funcall ticket-of g)) grouped))
         (groups (or non-nil-groups grouped)))
    (-map (lambda (group) (org-agenda-gerrit--item-from-stack (cdr group)))
          groups)))

(defun org-agenda-gerrit--item-from-jira-issue (issue)
  "Convert Jira REST ISSUE alist into a `org-agenda-gerrit-item'."
  (let* ((key      ('key issue))
         (summary  ('fields 'summary issue))
         (updated  ('fields 'updated issue))
         (reporter ('fields 'reporter 'displayName issue))
         (age      (when (stringp updated)
                     (ignore-errors (float-time (date-to-time updated))))))
    (org-agenda-gerrit-item-create
     :jira     key
     :title    (or summary "untitled")
     :tip-url  nil
     :reviewers nil
     :author   nil
     :reporter reporter
     :age      age)))

;; ── Jira URLs ─────────────────────────────────────────────────────────────────

(defun org-agenda-gerrit--jira-browse-url (ticket)
  "Return the Jira browse URL for TICKET."
  (format "%s/%s" org-agenda-gerrit-jira-base-url ticket))

(defun org-agenda-gerrit--format-jira-link (ticket)
  "Format TICKET as an Org bracketed link via `org-agenda-gerrit-jira-base-url'."
  (format "[[%s/%s][%s]]" org-agenda-gerrit-jira-base-url ticket ticket))

;; ── Standup serializer ──────────────────────────────────────────────────────
;;
;; When RET / C-c C-s mints an Org heading from a Gerrit/Jira row, we want the
;; headline to already read like a standup line: a clickable ticket, the clean
;; title, then `:: <why it is in my queue today>'.  ACTION carries that "why",
;; with %s standing for the person it concerns (the change owner, or whoever's
;; feedback we owe).  Full names survive into the heading on purpose --- the
;; `s' key pipes the day through `my/copy-as-slack', which maps "Kyle Blocher"
;; to "@kyle" via `my\slack-name-map'.

(defun org-agenda-gerrit--standup-person (o)
  "Best person to mention for item O: the change owner, else its reviewer(s)."
  (or (org-agenda-gerrit-item-author o)
      (org-agenda-gerrit--format-reviewer-names
       (org-agenda-gerrit-item-reviewers o))))

(defun org-agenda-gerrit--review-action (o)
  "Standup verb for a review row: \"Review NAME's [[tip][latest work]]\".
\"latest work\" links to the change's tip patch when known, so a click
lands on the diff to review and others can see what is under review."
  (let* ((who (or (org-agenda-gerrit--standup-person o) "their"))
         (url (org-agenda-gerrit-item-tip-url o))
         (work (if url (format "[[%s][latest work]]" url) "latest work")))
    (format "Review %s's %s" who work)))

(defun org-agenda-gerrit--feedback-action (o)
  "Standup verb for a feedback-owed row: \"Address NAMES' [[tip][latest feedback]]\".
NAMES are the change's *reviewers* --- the people whose feedback I owe a reply to
(this view is `owner:self', so the author is me and irrelevant) --- joined by
`org-agenda-gerrit--format-reviewer-names'.  \"latest feedback\" links to the
change's tip patch when known."
  (let* ((who (or (org-agenda-gerrit--format-reviewer-names
                   (org-agenda-gerrit-item-reviewers o))
                  "the reviewers"))
         (url (org-agenda-gerrit-item-tip-url o))
         (fb  (if url (format "[[%s][latest feedback]]" url) "latest feedback")))
    (format "Address %s's %s" who fb)))

(defun org-agenda-gerrit--standup-title (action)
  "Build a `row → standup-title-string' fn for `:org-upsert''s `:title-spec'.
ACTION is the trailing clause after `:: ', e.g. \"Review %s's latest
work\"; a single %s, if present, is filled with the relevant person
\(see `org-agenda-gerrit--standup-person').  ACTION may also be a
function of the item, for views whose verb depends on the row.
The resulting title reads `[[jira]] title :: verb'."
  (lambda (o)
    (let* ((jira  (org-agenda-gerrit-item-jira o))
           (title (or (org-agenda-gerrit-item-title o) "untitled"))
           (link  (if jira (org-agenda-gerrit--format-jira-link jira) title))
           (verb  (cond ((functionp action) (funcall action o))
                        ((string-match-p "%s" action)
                         (format action (or (org-agenda-gerrit--standup-person o)
                                            "this work")))
                        (t action))))
      (format "%s %s :: %s" link title verb))))

(defun org-agenda-gerrit--parent-title (o)
  "Title for a work-item's *parent* Org tree: `[[jira]] title' (no verb).
One parent per Jira ticket; the per-day work lives in dated children under it."
  (let* ((jira  (org-agenda-gerrit-item-jira o))
         (title (or (org-agenda-gerrit-item-title o) "untitled")))
    (if jira (format "%s %s" (org-agenda-gerrit--format-jira-link jira) title) title)))

(defun org-agenda-gerrit--org-upsert (action)
  "Build an `:org-upsert' fn for a Gerrit/Jira view: a parent tree per ticket,
with a fresh dated child each day.

Returns (lambda (item) → marker).  The parent tree is keyed by the bare Jira ID
\(`[[jira]] title', created once); under it a child keyed `JIRA@YYYY-MM-DD' holds
today's work, titled `[[jira]] title :: ACTION' (see
`org-agenda-gerrit--standup-title').  Re-visiting the same ticket the same day
re-finds that child; a new day makes a new child under the same parent.  ACTION
is the standup verb clause.  Returns the CHILD marker --- what RET / C-c C-s act
on."
  (let ((child-title-fn (org-agenda-gerrit--standup-title action)))
    (lambda (o)
      (let ((jira (org-agenda-gerrit-item-jira o)))
        (actionable-query-upsert-org-child
         :parent-key        jira
         :parent-title-spec (org-agenda-gerrit--parent-title o)
         :child-key         (format "%s@%s" jira (format-time-string "%Y-%m-%d"))
         :child-title-spec  (funcall child-title-fn o))))))

;; ── Async fetch (Gerrit SSH) ──────────────────────────────────────────────────

(defun org-agenda-gerrit--fetch-async (query-string callback &optional by-author)
  "Fire async Gerrit SSH query; deliver `org-agenda-gerrit-item' list to CALLBACK.
When BY-AUTHOR is non-nil, stacks are split per ticket×author
\(used for reviews-needed)."
  (let* ((buf  (generate-new-buffer " *neato-gerrit*"))
         (args (list org-agenda-gerrit-ssh-host "gerrit" "query"
                     "--format=JSON" "--current-patch-set"
                     "--all-reviewers" "--dependencies"
                     "--commit-message" "--comments" "--" query-string))
         (proc (apply #'start-process "neato-gerrit" buf
                      org-agenda-gerrit-ssh-command args)))
    (set-process-sentinel
     proc
     (lambda (p _)
       (when (memq (process-status p) '(exit signal))
         (let* ((raw     (with-current-buffer (process-buffer p) (buffer-string)))
                (changes (org-agenda-gerrit--parse-json raw)))
           (kill-buffer (process-buffer p))
           ;; Grouping runs a BFS over change dependencies; its edge-map
           ;; expansion is now async, so the bridge-resolution queries no
           ;; longer block the sentinel.  Likewise the Jira-title and
           ;; urgent-id fetches below are async --- the whole tail runs off
           ;; the input path (which used to freeze Emacs mid weekly-review
           ;; questionnaire).
           (org-agenda-gerrit--group-into-stacks-async
            changes
            (lambda (stacks)
              (let ((items   (-mapcat
                              (lambda (s)
                                (org-agenda-gerrit--items-from-stack s by-author))
                              stacks))
                    (all-ids (->> stacks
                                  (-mapcat (lambda (s)
                                             (-mapcat #'org-agenda-gerrit--extract-jira-tickets s)))
                                  -uniq)))
                (org-agenda-gerrit--fetch-jira-titles-async
                 all-ids
                 (lambda ()
                   ;; Stamp urgent flag on items whose Jira appears in the urgent-JQL result.
                   (org-agenda-gerrit--urgent-ids-async
                    (lambda (urgent-ht)
                      (dolist (item items)
                        (when (and (org-agenda-gerrit-item-jira item)
                                   (gethash (org-agenda-gerrit-item-jira item) urgent-ht))
                          (setf (org-agenda-gerrit-item-urgent item) t)))
                      (funcall callback items))))))))))))))

(defun org-agenda-gerrit--urgent-ids-async (callback)
  "Fetch urgent-JQL Jira ticket IDs asynchronously; deliver hash-table to CALLBACK."
  (let ((url (format "%s/rest/api/2/search?jql=%s&fields=summary&maxResults=100"
                     (org-agenda-gerrit--jira-rest-base)
                     (url-hexify-string (org-agenda-gerrit--urgent-jql)))))
    (aq--cli-async
     (cons "curl" (org-agenda-gerrit--jira-curl-args url))
     (lambda (raw)
       (let* ((json-obj (ignore-errors (json-read-from-string raw)))
              (issues   (when json-obj (append (assoc-default 'issues json-obj) nil)))
              (ht       (make-hash-table :test 'equal)))
         (dolist (issue issues)
           (let ((key (assoc-default 'key issue)))
             (when key (puthash key t ht))))
         ht))
     callback)))

;; ── Async fetch (Jira) ────────────────────────────────────────────────────────

(defun org-agenda-gerrit--fetch-jira-async (jql callback)
  "Fetch Jira JQL results via async curl subprocess; deliver items to CALLBACK."
  (let ((url (format "%s/rest/api/2/search?jql=%s&fields=summary,status,updated,reporter&maxResults=50"
                     (org-agenda-gerrit--jira-rest-base)
                     (url-hexify-string jql))))
    (aq--cli-async
     (cons "curl" (org-agenda-gerrit--jira-curl-args url))
     (lambda (raw)
       (let* ((json-obj (ignore-errors (json-read-from-string raw)))
              (issues   (when json-obj (append (assoc-default 'issues json-obj) nil))))
         (-map #'org-agenda-gerrit--item-from-jira-issue
               (or issues nil))))
     callback)))

;; ── JQL strings ──────────────────────────────────────────────────────────────

(defconst org-agenda-gerrit--jira-active-statuses
  '("In Progress" "In Review"))

(defun org-agenda-gerrit--jira-status-clause ()
  (s-join ", " (-map (lambda (s) (format "\"%s\"" s))
                     org-agenda-gerrit--jira-active-statuses)))

(defun org-agenda-gerrit--urgent-jql ()
  (let* ((now (decode-time))
         (mon (nth 4 now))
         (yr  (mod (nth 5 now) 100))
         (next-mon (1+ mon))
         (next-yr  (if (> next-mon 12) (1+ yr) yr))
         (next-mon (if (> next-mon 12) 1 next-mon)))
    (format "(Urgency = \"1 month\" OR fixVersion = \"%d.%d\") AND resolution = Unresolved AND assignee = %s"
            next-yr next-mon org-agenda-gerrit-user)))

(defun org-agenda-gerrit--jira-active-jql ()
  (format "assignee = %s AND resolution = Unresolved AND status in (%s) AND updated >= -14d"
          org-agenda-gerrit-user (org-agenda-gerrit--jira-status-clause)))

(defun org-agenda-gerrit--assigned-jql ()
  (format "assignee = %s AND resolution = Unresolved AND status not in (%s) AND updated >= -14d"
          org-agenda-gerrit-user (org-agenda-gerrit--jira-status-clause)))

(defun org-agenda-gerrit--done-jql ()
  (format "assignee = %s AND statusCategory = Done AND resolved >= -7d ORDER BY updated DESC"
          org-agenda-gerrit-user))

;; ── Help-echo (3-layer tooltip) ───────────────────────────────────────────────

(defun org-agenda-gerrit--help-echo (item &optional action-text)
  "Return a 3-layer tooltip for ITEM: ACTION + META + NUDGE.
ACTION-TEXT is the top-line action string; falls back to a generic hint."
  (let* ((urgent      (org-agenda-gerrit-item-urgent item))
         (age-epoch   (org-agenda-gerrit-item-age item))
         (days-old    (when age-epoch (floor (/ (- (float-time) age-epoch) 86400))))
         (stale       (and days-old (>= days-old 14)))
         (very-stale  (and days-old (>= days-old 30)))
         (reviewers   (org-agenda-gerrit-item-reviewers item))
         (rv-str      (when reviewers (org-agenda-gerrit--format-reviewer-names reviewers)))
         (stack-size  (or (org-agenda-gerrit-item-stack-size item) 1))
         (max-ps      (or (org-agenda-gerrit-item-max-patchsets item) 1))
         (comments    (or (org-agenda-gerrit-item-comment-count item) 0))
         (shirt-size  (org-agenda-gerrit-item-avg-shirt-size item))
         (num-rv      (length reviewers))
         (ci          (org-agenda-gerrit-item-ci-status item))
         (ci-red      (eq ci 'fail))
         (ci-green    (eq ci 'pass))
         (has-cr      (org-agenda-gerrit-item-has-code-review item))
         (blocked     (org-agenda-gerrit-item-blocked-by-parent item))
         (large       (and shirt-size (>= shirt-size 4)))
         (many-ps     (> max-ps 4))
         (many-cmts   (> comments (* 5 stack-size)))
         (solo-rv     (= num-rv 1))
         (many-rv     (> num-rv 2))
         (needs-rebase (and days-old (> days-old 7)))
         (action       (or action-text "Open this item."))
         (meta
          (concat
           (when (> stack-size 1) (format " [%d changes]" stack-size))
           (when shirt-size      (format " [size %s]" (org-agenda-gerrit--shirt-size-label shirt-size)))
           (when (> max-ps 1)    (format " [ps %d]" max-ps))
           (when (> comments 0)  (format " [%d comment%s]" comments (if (= comments 1) "" "s")))
           (when (> num-rv 1)    (format " [%d reviewers]" num-rv))
           (when ci-red   " [CI ✗]")
           (when blocked  " [blocked]")
           (when urgent   " [URGENT]")))
         (nudge
          (cond
           (ci-red   " 🔴 CI is red — fix the build before anything else.")
           (blocked  " 🔗 Blocked by an unmerged parent — land the dependency first.")
           ((and many-ps many-cmts stale)
            " 🧱 Stuck — schedule a sync discussion, split the change, or rethink the approach.")
           ((and many-ps many-cmts)
            " ⚠ High churn + heavy comments — escalate to a synchronous conversation.")
           ((and very-stale large many-ps)
            " ⚠ Old, large, and churning — abandon and re-open as smaller pieces.")
           (large
            (format " 💡 Size %s — split into a smaller stack; small changes = faster reviews."
                    (org-agenda-gerrit--shirt-size-label shirt-size)))
           ((and many-cmts (not many-ps))
            " 💡 Lots of comments but few patchsets — spin off unrelated cleanup as follow-up work.")
           ((and solo-rv stale)
            " 👥 Only one reviewer and going stale — add another reviewer or re-assign.")
           (many-rv
            (format " 👥 %d reviewers — consider narrowing to 1–2 primary reviewers." num-rv))
           ((and ci-green (not has-cr))
            (format " ✅ CI green but no Code-Review — ask %s for final approval."
                    (or rv-str "a reviewer")))
           (needs-rebase
            " 🔄 Older than a week — rebase to stay current with the target branch.")))
         (text
          (concat
           (propertize action 'face 'bold)
           (when (length> meta 0)
             (concat "\n" (propertize meta 'face 'shadow)))
           (when nudge
             (let ((nudge-face
                    (cond
                     ((or ci-red blocked)                          '(bold (:foreground "red")))
                     ((or (and many-ps many-cmts)
                          (and very-stale large many-ps))          '(bold (:foreground "orange red")))
                     (t                                            '(bold (:foreground "steel blue"))))))
               (concat "\n" (propertize nudge 'face nudge-face)))))))
    (let ((width (frame-width)))
      (mapconcat
       (lambda (line)
         (let* ((len (string-width line))
                (pad (max 0 (/ (- width len) 2))))
           (concat (make-string pad ?\s) line)))
       (split-string text "\n")
       "\n"))))

;; ── Shared columns & actions ──────────────────────────────────────────────────

(defun org-agenda-gerrit--col-age      (o &rest _) (if-let ((e (org-agenda-gerrit-item-age o))) (org-agenda-gerrit--format-age e) ""))
(defun org-agenda-gerrit--col-jira     (o &rest _) (or (org-agenda-gerrit-item-jira o) ""))
(defun org-agenda-gerrit--col-size     (o &rest _) (if-let ((s (org-agenda-gerrit-item-avg-shirt-size o))) (org-agenda-gerrit--shirt-size-label s) ""))
(defun org-agenda-gerrit--col-reporter (o &rest _) (or (org-agenda-gerrit-item-reporter o) ""))

(defvar org-agenda-gerrit-columns
  (list
   (list :name "Age"    :width  7 :align 'center
         :getter   (lambda (o &rest _) (org-agenda-gerrit--col-age o))
         :displayer (lambda (v w _)
                      (propertize (truncate-string-to-width v w)
                                  'face '(:height 0.8 :foreground "gray50"))))
   (list :name "Jira"   :width 12 :align 'center
         :getter   (lambda (o &rest _) (org-agenda-gerrit--col-jira o))
         :formatter (lambda (v &rest _)
                      (propertize v 'face '(:foreground "forest green" :weight bold))))
   (list :name "Author" :width 14 :align 'center
         :getter   (lambda (o &rest _) (or (org-agenda-gerrit-item-author o) "")))
   (list :name "PS"     :width  4 :align 'center
         :getter   (lambda (o &rest _)
                     (if-let ((ps (org-agenda-gerrit-item-max-patchsets o)))
                         (number-to-string ps) "")))
   (list :name "Size"   :width  6 :align 'center
         :getter   (lambda (o &rest _) (org-agenda-gerrit--col-size o)))
   (list :name "Subject" :width 62
         :getter   (lambda (o &rest _)
                     (concat (if (org-agenda-gerrit-item-urgent o) "🔴 " "")
                             (or (org-agenda-gerrit-item-title o) ""))))))

(defun org-agenda-gerrit--open-url (o)
  (if-let ((u (org-agenda-gerrit-item-tip-url o)))
      (browse-url u)
    (aq--message "No URL for this item.")))

(defun org-agenda-gerrit--open-jira (o)
  (if-let ((j (org-agenda-gerrit-item-jira o)))
      (browse-url (org-agenda-gerrit--jira-browse-url j))
    (aq--message "No Jira ticket for this item.")))

(defun org-agenda-gerrit--capture (o)
  (let* ((url   (or (org-agenda-gerrit-item-tip-url o) ""))
         (jira  (org-agenda-gerrit-item-jira o))
         (title (or (org-agenda-gerrit-item-title o) ""))
         (label (if jira (format "%s ∷ %s" jira title) title))
         (link  (if (string-empty-p url) label
                  (format "[[%s][%s]]" url label))))
    (org-capture-string (format "* TODO %s" link) "t")))

(defvar org-agenda-gerrit-actions
  (list
   (list "RET" "Open change in browser"
         (lambda (o) (org-agenda-gerrit--open-url o)))
   (list "j"   "Open Jira ticket in browser"
         (lambda (o) (org-agenda-gerrit--open-jira o)))
   (list "w"   "Copy title to kill-ring"
         (lambda (o)
           (let ((s (or (org-agenda-gerrit-item-title o) "")))
             (kill-new s)
             (aq--message "Copied: %s" s))))
   (list "c"   "Capture as org TODO"
         (lambda (o) (org-agenda-gerrit--capture o)))))

;; Jira-flavoured defaults: a Jira ticket has no Gerrit change URL, so `RET`
;; jumps to the ticket instead; and `Author` / `PS` / `Size` columns are
;; Gerrit-only noise — dropped in favour of `Reporter`.
(defvar org-agenda-gerrit-jira-columns
  (list
   (list :name "Age" :width 7 :align 'center
         :getter    #'org-agenda-gerrit--col-age
         :displayer (lambda (v w _)
                      (propertize (truncate-string-to-width v w)
                                  'face '(:height 0.8 :foreground "gray50"))))
   (list :name "Jira" :width 12 :align 'center
         :getter    #'org-agenda-gerrit--col-jira
         :formatter (lambda (v &rest _)
                      (propertize v 'face '(:foreground "forest green" :weight bold))))
   (list :name "Reporter" :width 18 :align 'center
         :getter #'org-agenda-gerrit--col-reporter)
   (list :name "Subject" :width 60
         :getter (lambda (o &rest _) (or (org-agenda-gerrit-item-title o) "")))))

(defvar org-agenda-gerrit-jira-actions
  (cons (list "RET" "Open Jira ticket in browser"
              (lambda (o) (org-agenda-gerrit--open-jira o)))
        (cdr org-agenda-gerrit-actions)))

;; ── Help-echo helper & presets ────────────────────────────────────────────────

(defun org-agenda-gerrit--help-echo-tiered (&rest plist)
  "Return a `:help-echo'-shaped lambda for a staleness-tiered tooltip.
PLIST accepts `:fresh', `:stale' (≥14 days), `:very-stale' (≥30 days).
Each value is either a string or a one-arg function of ITEM returning
a string (useful for weaving reviewer names into the message).
Falls back up the chain: very-stale → stale → fresh."
  (let ((fresh      (plist-get plist :fresh))
        (stale      (plist-get plist :stale))
        (very-stale (plist-get plist :very-stale)))
    (lambda (item)
      (let* ((age-epoch (org-agenda-gerrit-item-age item))
             (days-old  (when age-epoch
                          (floor (/ (- (float-time) age-epoch) 86400))))
             (pick      (cond
                         ((and days-old (>= days-old 30)) (or very-stale stale fresh))
                         ((and days-old (>= days-old 14)) (or stale fresh))
                         (t                                fresh)))
             (text      (cond ((functionp pick) (funcall pick item))
                              ((stringp pick)   pick)
                              (t                nil))))
        (org-agenda-gerrit--help-echo item text)))))

(defun org-agenda-gerrit--help-echo-with (action-text)
  "Return a `:help-echo'-shaped lambda that pins ACTION-TEXT as the top line.
ACTION-TEXT may be a string or a one-arg function of ITEM returning a string."
  (lambda (item)
    (org-agenda-gerrit--help-echo item
                             (cond ((functionp action-text) (funcall action-text item))
                                   (t                       action-text)))))

(actionable-query-defview-def-keyword :gerrit-query (query-form)
  "Default the common kwargs of a Gerrit-sourced view.
QUERY-FORM is a form (typically a string literal) evaluated at
view-open time to yield a Gerrit query.  To split stacks per
ticket×author (for `reviews-needed' views), also pass
`:by-author t' at the call site.
Overridden by any explicit kwarg at the `actionable-query-defview' call site."
  `(:columns       org-agenda-gerrit-columns
    :objects       (lambda (cb) (org-agenda-gerrit--fetch-async ,query-form cb nil))
    :org-upsert    (org-agenda-gerrit--org-upsert "Make progress on this work")
    :help-echo     #'org-agenda-gerrit--help-echo
    :snooze-period 'forever
    :auto-refresh  "1 day"
    :actions       org-agenda-gerrit-actions))

(actionable-query-defview-def-keyword :gerrit-query-by-author (query-form)
  "Like `:gerrit-query', but split stacks per ticket×author.
Used by the Reviews-Needed view, where multiple reviewers' changes
on the same ticket should each surface as a separate row."
  `(:columns       org-agenda-gerrit-columns
    :objects       (lambda (cb) (org-agenda-gerrit--fetch-async ,query-form cb t))
    :org-upsert    (org-agenda-gerrit--org-upsert #'org-agenda-gerrit--review-action)
    :help-echo     #'org-agenda-gerrit--help-echo
    :snooze-period 'forever
    :auto-refresh  "1 day"
    :actions       org-agenda-gerrit-actions))

(actionable-query-defview-def-keyword :jira-query (jql-thunk-form)
  "Default the common kwargs of a Jira-sourced view.
JQL-THUNK-FORM evaluates to a zero-arg function returning the JQL
string at open time (so queries referencing the current month stay
fresh).
Overridden by any explicit kwarg at the `actionable-query-defview' call site."
  `(:columns       org-agenda-gerrit-jira-columns
    :objects       (lambda (cb)
                     (org-agenda-gerrit--fetch-jira-async (funcall ,jql-thunk-form) cb))
    :org-upsert    (org-agenda-gerrit--org-upsert "Start on this work")
    :help-echo     #'org-agenda-gerrit--help-echo
    :snooze-period 'forever
    :auto-refresh  "1 day"
    :actions       org-agenda-gerrit-jira-actions))

;; ── Views ─────────────────────────────────────────────────────────────────────

;; 1. Changes where I'm blocking someone (attention set, not my changes)
(actionable-query-defview oag-reviews-needed "👀 Gerrit: Reviews Needed"
  :gerrit-query-by-author "attention:self status:open -is:abandoned -owner:self"
  :row-colors   '("misty rose" "lavender blush" "seashell")
  ;; RET / C-c C-s mints a rich heading: linked Jira ticket + title
  ;; :: Review NAME's [[tip-url][latest work]] --- via `--review-action',
  ;; which links the verb to the change's tip patch.
  :org-upsert (org-agenda-gerrit--org-upsert
                   #'org-agenda-gerrit--review-action)
  :help-echo    (org-agenda-gerrit--help-echo-tiered
                 :fresh      "Open their change and leave a Code-Review vote."
                 :stale      "Stale review — prioritise it today."
                 :very-stale "This review is rotting — Slack nudge or offer to pair."))

;; 2. My changes in the attention set (reviewer left feedback)
;; Author column is always self, so swap it for Reviewer — the person whose
;; feedback we owe a reply to. The bespoke `w' replaces the default
;; copy-title-to-kill-ring with a Slack-ready acknowledgement.
(actionable-query-defview oag-my-changes-needing-action "🔧 Gerrit: My Changes Needing Action"
  :gerrit-query "attention:self status:open -is:abandoned owner:self"
  :org-upsert (org-agenda-gerrit--org-upsert #'org-agenda-gerrit--feedback-action)
  :row-colors   '("linen" "seashell" "old lace")
  :columns      (list
                 (list :name "Age"      :width  7 :align 'center
                       :getter   (lambda (o &rest _) (org-agenda-gerrit--col-age o))
                       :displayer (lambda (v w _)
                                    (propertize (truncate-string-to-width v w)
                                                'face '(:height 0.8 :foreground "gray50"))))
                 (list :name "Jira"     :width 12 :align 'center
                       :getter   (lambda (o &rest _) (org-agenda-gerrit--col-jira o))
                       :formatter (lambda (v &rest _)
                                    (propertize v 'face '(:foreground "forest green" :weight bold))))
                 (list :name "Reviewer" :width 18 :align 'center
                       :getter   (lambda (o &rest _)
                                   (or (org-agenda-gerrit--format-reviewer-names
                                        (org-agenda-gerrit-item-reviewers o))
                                       "")))
                 (list :name "PS"       :width  4 :align 'center
                       :getter   (lambda (o &rest _)
                                   (if-let ((ps (org-agenda-gerrit-item-max-patchsets o)))
                                       (number-to-string ps) "")))
                 (list :name "Size"     :width  6 :align 'center
                       :getter   (lambda (o &rest _) (org-agenda-gerrit--col-size o)))
                 (list :name "Subject"  :width 62
                       :getter   (lambda (o &rest _)
                                   (concat (if (org-agenda-gerrit-item-urgent o) "🔴 " "")
                                           (or (org-agenda-gerrit-item-title o) "")))))
  :actions      (let ((build-text
                       (lambda (o)
                         (let* ((jira        (org-agenda-gerrit-item-jira o))
                                (title       (or (org-agenda-gerrit-item-title o) ""))
                                (tip-url     (or (org-agenda-gerrit-item-tip-url o) ""))
                                (reviewer    (or (org-agenda-gerrit--format-reviewer-names
                                                  (org-agenda-gerrit-item-reviewers o))
                                                 "the reviewer"))
                                (jira-link   (if jira
                                                 (format "[[%s/%s][%s]]"
                                                         org-agenda-gerrit-jira-base-url jira jira)
                                               title))
                                (gerrit-link (if (string-empty-p tip-url)
                                                 "this work"
                                               (format "[[%s][this work]]" tip-url))))
                           (format "%s %s :: addressing %s's feedback on %s."
                                   jira-link title reviewer gerrit-link)))))
                  (append
                   (list (list "w" "Copy as Slack feedback acknowledgement"
                               (lambda (o)
                                 (let ((text (funcall build-text o)))
                                   (my/copy-as-slack text)
                                   (aq--message "Copied as Slack: %s" text))))
                         (list "c" "Capture as org TODO with Slack-ready title"
                               (lambda (o)
                                 (org-capture-string
                                  (format "* TODO %s" (funcall build-text o)) "t"))))
                   (cl-remove-if (lambda (a) (member (car a) '("w" "c")))
                                 org-agenda-gerrit-actions)))
  :help-echo    (org-agenda-gerrit--help-echo-tiered
                 :fresh      "Address the feedback and re-publish."
                 :stale      (lambda (i)
                               (format "Feedback from %s is going stale — address it soon."
                                       (or (org-agenda-gerrit--format-reviewer-names
                                            (org-agenda-gerrit-item-reviewers i))
                                           "reviewers")))
                 :very-stale (lambda (i)
                               (format "Feedback from %s has waited a month — address or abandon."
                                       (or (org-agenda-gerrit--format-reviewer-names
                                            (org-agenda-gerrit-item-reviewers i))
                                           "reviewers")))))

;; 3. My open changes awaiting review (not in attention set, not WIP)
(actionable-query-defview oag-please-review-my-work "⏳ Gerrit: Please Review My Work"
  :gerrit-query "status:open owner:self -is:abandoned -is:wip -attention:self"
  :row-colors   '("honeydew" "honeydew1" "honeydew2")
  :org-upsert (org-agenda-gerrit--org-upsert "Nudge %s to review this")
  :columns      (list
                 (list :name "Age"      :width  7 :align 'center
                       :getter   (lambda (o &rest _) (org-agenda-gerrit--col-age o))
                       :displayer (lambda (v w _)
                                    (propertize (truncate-string-to-width v w)
                                                'face '(:height 0.8 :foreground "gray50"))))
                 (list :name "Jira"     :width 12 :align 'center
                       :getter   (lambda (o &rest _) (org-agenda-gerrit--col-jira o))
                       :formatter (lambda (v &rest _)
                                    (propertize v 'face '(:foreground "forest green" :weight bold))))
                 (list :name "Reviewer" :width 18 :align 'center
                       :getter   (lambda (o &rest _)
                                   (or (org-agenda-gerrit--format-reviewer-names
                                        (org-agenda-gerrit-item-reviewers o))
                                       "")))
                 (list :name "PS"       :width  4 :align 'center
                       :getter   (lambda (o &rest _)
                                   (if-let ((ps (org-agenda-gerrit-item-max-patchsets o)))
                                       (number-to-string ps) "")))
                 (list :name "Size"     :width  6 :align 'center
                       :getter   (lambda (o &rest _) (org-agenda-gerrit--col-size o)))
                 (list :name "Subject"  :width 62
                       :getter   (lambda (o &rest _)
                                   (concat (if (org-agenda-gerrit-item-urgent o) "🔴 " "")
                                           (or (org-agenda-gerrit-item-title o) "")))))
  :actions      (let ((build-text
                       (lambda (o)
                         (let* ((jira        (org-agenda-gerrit-item-jira o))
                                (title       (or (org-agenda-gerrit-item-title o) ""))
                                (tip-url     (or (org-agenda-gerrit-item-tip-url o) ""))
                                (reviewer    (or (org-agenda-gerrit--format-reviewer-names
                                                  (org-agenda-gerrit-item-reviewers o))
                                                 ""))
                                (jira-link   (if jira
                                                 (format "[[%s/%s][%s]]"
                                                         org-agenda-gerrit-jira-base-url jira jira)
                                               title))
                                (gerrit-link (if (string-empty-p tip-url)
                                                 "this work"
                                               (format "[[%s][this work]]" tip-url))))
                           (format "%s %s :: %s please review %s."
                                   jira-link title reviewer gerrit-link)))))
                  (append
                   (list (list "w" "Copy as Slack review request"
                               (lambda (o)
                                 (let ((text (funcall build-text o)))
                                   (my/copy-as-slack text)
                                   (aq--message "Copied as Slack: %s" text))))
                         (list "c" "Capture as org TODO with Slack-ready title"
                               (lambda (o)
                                 (org-capture-string
                                  (format "* TODO %s" (funcall build-text o)) "t"))))
                   (cl-remove-if (lambda (a) (member (car a) '("w" "c")))
                                 org-agenda-gerrit-actions)))
  :help-echo    (org-agenda-gerrit--help-echo-tiered
                 :fresh      (lambda (i)
                               (format "Awaiting %s — patience, or a gentle ping."
                                       (or (org-agenda-gerrit--format-reviewer-names
                                            (org-agenda-gerrit-item-reviewers i))
                                           "reviewers")))
                 :stale      (lambda (i)
                               (format "Two weeks without review — send %s a Slack nudge."
                                       (or (org-agenda-gerrit--format-reviewer-names
                                            (org-agenda-gerrit-item-reviewers i))
                                           "reviewers")))
                 :very-stale (lambda (i)
                               (format "A month without review — escalate or re-assign %s."
                                       (or (org-agenda-gerrit--format-reviewer-names
                                            (org-agenda-gerrit-item-reviewers i))
                                           "the reviewer")))))

;; 4. My WIP changes (not ready for review)
(actionable-query-defview oag-work-in-progress "🚧 Gerrit: Work In Progress"
  :gerrit-query "status:open owner:self -is:abandoned is:wip -attention:self"
  :row-colors   '("light cyan" "azure" "alice blue")
  :org-upsert (org-agenda-gerrit--org-upsert "Finish this WIP")
  :help-echo    (org-agenda-gerrit--help-echo-tiered
                 :fresh      "🌱 Is this ready? Self-review, ensure right reviewers, keep stacks small."
                 :stale      "Idle for weeks — self-review, add context, then resume."
                 :very-stale "Dead weight? Abandon in Gerrit or commit to finishing it."))

;; 5. Urgent Jira tickets not yet started
(actionable-query-defview oag-jira-urgent-not-started "📋 Jira: Urgent Not Yet Started"
  :jira-query #'org-agenda-gerrit--urgent-jql
  :row-colors '("lemon chiffon" "light goldenrod yellow" "cornsilk")
  :org-upsert (org-agenda-gerrit--org-upsert "Scope and start this urgent work")
  :help-echo  (org-agenda-gerrit--help-echo-with
               (lambda (i)
                 (if (org-agenda-gerrit-item-urgent i)
                     "Urgent and unstarted — create a Gerrit change today."
                   "Not yet started — scope it, create a first patchset, keep stacks small."))))

;; 6. Jira in-flight with no Gerrit changes
(actionable-query-defview oag-jira-active-no-gerrit "⚠️ Jira: Active But No Gerrit Changes"
  :jira-query #'org-agenda-gerrit--jira-active-jql
  :row-colors '("misty rose" "lavender blush" "seashell")
  :org-upsert (org-agenda-gerrit--org-upsert
                   "Reconcile: push a draft citing this ticket, or fix its status")
  :help-echo  (org-agenda-gerrit--help-echo-with
               "Jira says active but no Gerrit changes — stale #progress? Fix the status.")
  :prose   (insert (propertize
                    (concat
                     "\n\nThese tickets are marked In Progress or In Review in Jira, yet\n"
                     "none of my open Gerrit changes reference them.  For each row,\n"
                     "pick exactly one:\n"
                     "  • Push a draft change that cites the ticket in its footer, or\n"
                     "  • Move the ticket back to To Do / Blocked — the status is lying, or\n"
                     "  • Reassign it, if someone else is actually carrying the work.\n"
                     "Leaving a ticket here is a promise you are silently breaking.\n\n\n")
                    'face '(:foreground "gray40" :slant italic)))
  ;; Override :objects to subtract tickets already backed by an open Gerrit change.
  :objects (lambda (cb)
             (org-agenda-gerrit--fetch-jira-async
              (org-agenda-gerrit--jira-active-jql)
              (lambda (items)
                (org-agenda-gerrit--my-open-change-jira-ids-async
                 (lambda (covered)
                   (funcall cb
                            (-remove (lambda (o)
                                       (member (org-agenda-gerrit-item-jira o) covered))
                                     items))))))))

;; 7. Assigned Jira tickets (not in-flight)
(actionable-query-defview oag-jira-assigned-to-me "📌 Jira: Assigned To Me"
  :jira-query #'org-agenda-gerrit--assigned-jql
  :row-colors '("lavender" "ghost white" "alice blue")
  :org-upsert (org-agenda-gerrit--org-upsert
                   "Decide: start this, or push back on scope")
  :help-echo  (org-agenda-gerrit--help-echo-with
               "Assigned to you — decide: start working, or push back on scope."))

;; 8. Done in the past 7 days
(actionable-query-defview oag-jira-done-this-week "✅ Jira: Done This Week"
  :jira-query #'org-agenda-gerrit--done-jql
  :row-colors '("honeydew" "honeydew1" "honeydew2")
  :org-upsert (org-agenda-gerrit--org-upsert "Shipped this")
  :help-echo  (org-agenda-gerrit--help-echo-with
               "Jira says Done — nothing to do, just a record of what shipped."))

;; 9. Morning standup — the four lenses I want before deciding today's work.
;;    Composes the decision-driving subset of the views above into a single
;;    buffer.  WIP, Please-Review-My-Work, Assigned, and Done are intentionally
;;    omitted — they belong to other rituals (weekly review, end-of-day Jira
;;    comments).  Each constituent view is also independently invokable.
(actionable-query-defview oag-morning-standup "🌅 Morning Standup"
                          :objects '()
                          :prose
                          (progn
                            (insert (propertize
                                     (concat "\n"
                                             "Four lenses on what to do today, in priority order:\n"
                                             "  1. Whom am I blocking?           (review their work)\n"
                                             "  2. Whose feedback do I owe?      (address it, then re-publish)\n"
                                             "  3. What's urgent and unstarted?  (pick one, scope it, push a draft)\n"
                                             "  4. Where do Jira and Gerrit disagree?  (the status is lying — fix it)\n\n")
                                     'face '(:foreground "gray40" :slant italic)))
                            (insert "\n* 1. 👀 Whom am I blocking?\n\n")
                            (oag-reviews-needed :insert 'fetch-latest)
                            (insert "\n\n* 2. 🔧 Feedback I owe\n\n")
                            (oag-my-changes-needing-action :insert 'fetch-latest)
                            (insert "\n\n* 3. 📋 Urgent and unstarted\n\n")
                            (oag-jira-urgent-not-started :insert 'fetch-latest)
                            (insert "\n\n* 4. ⚠️ Jira says active, Gerrit disagrees\n\n")
                            (oag-jira-active-no-gerrit :insert 'fetch-latest)))

;; ── writing back to Jira ────────────────────────────────────────────────────

(cl-defun org-agenda-gerrit-jira-post-comment (ticket comment)
  "Post COMMENT string as a comment on Jira TICKET via the REST API.
Authed with the bearer PAT (like every other Jira call here --- the old
`-n'/netrc path is CAPTCHA-locked on this instance).  Returns the new
comment's id string on success, nil on failure."
  (let* ((url     (format "%s/rest/api/2/issue/%s/comment"
                          (org-agenda-gerrit--jira-rest-base) ticket))
         (payload (json-encode `((body . ,comment))))
         (raw     (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "curl" nil t nil
                                    "-s" "-X" "POST"
                                    "-H" (format "Authorization: Bearer %s"
                                                 (org-agenda-gerrit--jira-token))
                                    "-H" "Content-Type: application/json"
                                    "-d" payload url))))
         (json-obj (ignore-errors (json-read-from-string raw))))
    (and json-obj (alist-get 'id json-obj))))

(provide 'org-agenda-gerrit)
;;; org-agenda-gerrit.el ends here
