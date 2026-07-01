;;; celsius-fahrenheit-convertor.el --- Weather widget for the actionable-query dashboard  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; A tiny "what's it like outside" widget atop the dashboard, as
;; `dashboard/weather'.  We work with Americans and never internalised
;; Fahrenheit, so every row shows BOTH units side by side ---the point of an
;; /actionable/ query: not a bare number, but one we can act on.  Seeded from
;; our current location (wttr.in geolocates by IP, no API key), fetched async
;; via `curl' so the dashboard opens instantly, and cached
;; (`dashboard--weather-cache') so later opens reuse it; `g'/`G' refetch.  The
;; rows double as a °C↔°F converter (`dashboard--weather-set-temp').

;;; Code:

(require 'actionable-query)

(defvar dashboard--weather-cache nil
  "Cached weather plist (:c :f :feels-c :feels-f :desc :place), or nil.
Populated by the async fetch in `dashboard/weather'; cleared by `G'.")

(defun dashboard--weather-parse (json-string)
  "Parse wttr.in `?format=j1' JSON-STRING into a weather plist, or nil on garbage."
  (ignore-errors
    (let* ((d    (json-parse-string json-string :object-type 'alist))
           (cur  (elt (alist-get 'current_condition d) 0))
           (area (elt (alist-get 'nearest_area d) 0))
           (name (lambda (k) (alist-get 'value (elt (alist-get k area) 0)))))
      (list :c       (alist-get 'temp_C cur)
            :f       (alist-get 'temp_F cur)
            :feels-c (alist-get 'FeelsLikeC cur)
            :feels-f (alist-get 'FeelsLikeF cur)
            :desc    (alist-get 'value (elt (alist-get 'weatherDesc cur) 0))
            :place   (string-join (delq nil (list (funcall name 'areaName)
                                                  (funcall name 'region)))
                                  ", ")))))

(defun dashboard--weather-fetch (callback)
  "Curl wttr.in for the current location async, then call CALLBACK with two rows.
Each row is a plist (:label :c :f).  A failed/missing curl yields no rows
\(the prose-bottom then says so) rather than freezing or erroring the view."
  (if (not (executable-find "curl"))
      (funcall callback nil)
    (let ((buf (generate-new-buffer " *dashboard-weather*")))
      (make-process
       :name "dashboard-weather" :buffer buf :noquery t
       :command '("curl" "-s" "--max-time" "10" "wttr.in/?format=j1")
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((w (dashboard--weather-parse
                     (with-current-buffer buf (buffer-string)))))
             (kill-buffer buf)
             (setq dashboard--weather-cache w)
             (funcall callback
                      (when w (list (dashboard--weather-row w)))))))))))

(defun dashboard--weather-row (w)
  "Build the single weather vtable row plist from weather plist W."
  (list :c (plist-get w :feels-c) :f (plist-get w :feels-f)
        :desc (plist-get w :desc) :place (plist-get w :place)))

(defun dashboard--weather-objects (callback)
  "Async `:objects': reuse `dashboard--weather-cache' if present, else fetch."
  (if dashboard--weather-cache
      (funcall callback (list (dashboard--weather-row dashboard--weather-cache)))
    (dashboard--weather-fetch callback)))

(defun dashboard--weather-emoji (desc)
  "Map a wttr.in DESC string to a weather emoji (best-effort, default 🌤️)."
  (let ((d (downcase (or desc ""))))
    (cond ((string-match-p "thunder\\|storm" d) "⛈️")
          ((string-match-p "snow\\|sleet\\|ice"  d) "❄️")
          ((string-match-p "rain\\|drizzle\\|shower" d) "🌧️")
          ((string-match-p "fog\\|mist\\|haze"   d) "🌫️")
          ((string-match-p "overcast\\|cloud"    d) "☁️")
          ((string-match-p "clear\\|sunny"       d) "☀️")
          (t "🌤️"))))

(defun dashboard--weather-advice (celsius)
  "One actionable line for CELSIUS (a number-ish string), in the README's voice."
  (let ((c (string-to-number (or celsius "15"))))
    (cond
     ((< c 0)  "Frost abroad --- coat, hat, and gloves; tarry not outside.")
     ((< c 10) "A chill prevails --- let a jacket be thy companion.")
     ((< c 20) "Mild and temperate --- go forth in peace.")
     ((< c 30) "The sun smiles gently --- a fair day; dress light.")
     (t        "Great heat besets the land --- drink water, seek the shade."))))

(defun dashboard--weather-set-temp (o new unit)
  "Set row O's UNIT (`:c' or `:f') to NEW, recomputing the sibling unit.
NEW may carry a stray degree suffix (\"72°F\"); we read the leading number.
Lets the weather rows double as a °C↔°F converter (the README's example):
type a Fahrenheit a colleague quoted and read the Celsius you understand."
  (unless (string-match "-?[0-9]+\\.?[0-9]*" new)
    (user-error "Temperature must be a number, got %S" new))
  (let* ((n (string-to-number (match-string 0 new)))
         (c (if (eq unit :c) n (/ (* (- n 32) 5.0) 9)))
         (f (if (eq unit :f) n (+ (* c 9.0 (/ 1.0 5)) 32))))
    (plist-put o :c (format "%d" (round c)))
    (plist-put o :f (format "%d" (round f)))))

(actionable-query-defview dashboard/weather "🌡️ Weather"
  :auto-refresh "1 hour"
  :objects #'dashboard--weather-objects
  :actions `(("G" "Refetch the weather now (g reuses the cache)"
              ,(lambda (_o)
                 (setq dashboard--weather-cache nil)
                 (actionable-query-refresh-current-view)
                 (message "Refetching weather…"))))
  ;; Header-line (not a buffer row): empty column names print no blank line,
  ;; so the weather sits on the world-clock's line.  The unit stays visible
  ;; per-row (the getter appends °C/°F).
  :use-header-line t
  :no-footer t
  :columns `((:name "" :width 3
                    :getter ,(lambda (o &rest _)
                               (propertize (dashboard--weather-emoji (plist-get o :desc))
                                           'help-echo (format "%s in %s"
                                                              (or (plist-get o :desc) "?")
                                                              (or (plist-get o :place) "?")))))
             (:name "" :width 6 :align right :editable t
                    :getter ,(lambda (o &rest _) (format "%s°C" (plist-get o :c)))
                    :setter ,(lambda (o new) (dashboard--weather-set-temp o new :c)))
             (:name "" :width 6 :align right :editable t
                    :getter ,(lambda (o &rest _) (format "%s°F" (plist-get o :f)))
                    :setter ,(lambda (o new) (dashboard--weather-set-temp o new :f))))
  :prose-bottom
  (let ((w dashboard--weather-cache))
    (insert (if w
                (propertize (format "%s in %s.  %s"
                                    (plist-get w :desc) (plist-get w :place)
                                    (dashboard--weather-advice (plist-get w :c)))
                            'face 'success)
              (propertize "Fetching weather… (needs curl on PATH)"
                          'face '(:foreground "gray50"))))))

(provide 'celsius-fahrenheit-convertor)
;;; celsius-fahrenheit-convertor.el ends here
