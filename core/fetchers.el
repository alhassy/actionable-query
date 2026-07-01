;;; fetchers.el --- Generic async fetch + parse helpers for actionable-query  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; The provider-agnostic plumbing every async `actionable-query-defview'
;; data source leans on:
;;
;;   `aq--cli-async'      — spawn a shell command, parse its stdout, deliver.
;;   `aq--strip-html'     — strip tags + truncate an HTML snippet.
;;   `aq--format-pubdate' — normalise a date string to YYYY-MM-DD.
;;
;; These are core because they are not feed-specific: `aq--cli-async'
;; backs `mail.el' and `org-agenda-gerrit.el', and `aq--format-pubdate'
;; is shared by mail + dashboard.  The RSS/Atom feed parsers that build
;; on them live in `applications/rss.el'.

;;; Code:

(require 'cl-lib)

(defun aq--cli-async (command parser callback)
  "Spawn COMMAND asynchronously and deliver parsed objects to CALLBACK.
COMMAND is a string; it is split on whitespace to form the argv.
PARSER is a function (raw-stdout-string → list-of-objects); defaults to
splitting stdout on newlines, yielding one string per non-empty line.
CALLBACK is the 1-arg actionable-query deliver function."
  (let ((argv (if (listp command) command (split-string command))))
    (make-process
     :name (car argv)
     :command argv
     :buffer (generate-new-buffer " *actionable-query-cli*")
     :sentinel
     (lambda (proc _)
       (when (eq (process-status proc) 'exit)
         (let ((raw (with-current-buffer (process-buffer proc) (buffer-string))))
           (kill-buffer (process-buffer proc))
           (funcall callback
                    (funcall (or parser (lambda (s) (split-string s "\n" t)))
                             raw))))))))

(defun aq--strip-html (str &optional max-width)
  "Strip HTML tags from STR and truncate to MAX-WIDTH (default 200)."
  (truncate-string-to-width
   (replace-regexp-in-string "<[^>]\\{1,\\}>" "" str)
   (or max-width 200)))

(defun aq--format-pubdate (date-string)
  "Format an RSS pubDate string as YYYY-MM-DD, or return DATE-STRING as-is."
  (or (and (stringp date-string)
           (ignore-errors
             (format-time-string "%Y-%m-%d" (date-to-time date-string))))
      date-string ""))

(provide 'fetchers)
;;; fetchers.el ends here
