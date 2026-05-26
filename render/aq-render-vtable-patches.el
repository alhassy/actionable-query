;;; aq-render-vtable-patches.el --- Centred vtable column headers  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; A surgical `:after' advice on `vtable--insert-header-line' that
;; rewrites any column header marked `:align center' so its text sits
;; centred inside its column cell — without disturbing sort indicators,
;; indicator padding, or column dividers.  Loaded eagerly by the
;; `actionable-query' entry file; no public functions of its own.

;;; Code:

(require 'vtable)
(declare-function vtable--get-value    "vtable")
(declare-function vtable--indicator    "vtable")
(declare-function vtable--char-width   "vtable")

(defun aq--center-vtable-headers (table widths spacer)
  "After-advice on `vtable--insert-header-line': center any column header whose
`:align' is `center', rewriting only its [name + fill-space] region and leaving
sort indicators, indicator-pad, and dividers intact."
  (save-excursion
    (goto-char (point-min))
    (seq-do-indexed
     (lambda (column index)
       (when (eq (vtable-column-align column) 'center)
         (when-let* ((match      (text-property-search-forward
                                  'vtable-column index #'eql))
                     (cell-start (prop-match-beginning match))
                     (cell-end   (prop-match-end match)))
           (let* ((raw-name        (vtable-column-name column))
                  (name-end        (+ cell-start (length raw-name)))
                  (name-str        (buffer-substring cell-start name-end))
                  (name-px         (string-pixel-width name-str))
                  (col-width       (elt widths index))
                  (last            (= index (1- (length (vtable-columns table)))))
                  (indicator       (vtable--indicator table index))
                  (indicator-width (string-pixel-width indicator))
                  (indicator-lead  (/ (vtable--char-width table) 2.0))
                  (fill-width      (+ (- col-width name-px indicator-width indicator-lead)
                                      (if last 0 spacer)))
                  (left-pad        (/ fill-width 2.0))
                  (right-pad       (- fill-width left-pad))
                  (fill-end        (next-single-property-change
                                    name-end 'display nil cell-end)))
             (delete-region cell-start fill-end)
             (goto-char cell-start)
             (insert (propertize " " 'display `(space :width (,left-pad)))
                     name-str
                     (propertize " " 'display `(space :width (,right-pad))))
             (put-text-property cell-start (point) 'vtable-column index)))))
     (vtable-columns table))))

(advice-add 'vtable--insert-header-line :after #'aq--center-vtable-headers)

(provide 'aq-render-vtable-patches)
;;; aq-render-vtable-patches.el ends here
