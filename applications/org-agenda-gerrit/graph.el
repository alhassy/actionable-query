;;; graph.el --- Tiny generic graph primitives: BFS edge-map + Union-Find  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; A handful of dependency-graph primitives, deliberately ignorant of any
;; domain.  Nodes are integer ids; an "edge map" is a hash-table mapping
;; id → list-of-neighbor-ids.  Two jobs:
;;
;;   1. Grow an edge map outward by BFS, fetching neighbors for the
;;      unexplored frontier (`graph-bfs-frontier' + `graph-edge-map-*').
;;      The fetch is the caller's --- graph.el only does the bookkeeping ---
;;      so the same stepping logic serves a synchronous loop or an async
;;      callback chain.
;;
;;   2. Partition a node set into connected components via Union-Find,
;;      following the edge map (`graph-union-find').
;;
;; Nothing here touches I/O, alists, or any project's data model.  Callers
;; adapt their records to plain ids by way of an accessor function.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun graph-edge-map-seed (ids id->neighbors)
  "Seed (EDGES PRESENT VISITED) hash-tables from IDS.
ID->NEIGHBORS maps an id to its list of neighbor ids; it is consulted
once per seed id.  PRESENT marks the original IDS (the nodes we are
actually grouping); VISITED marks every id whose neighbors are known,
so BFS never re-fetches them."
  (let ((present (make-hash-table :test 'eql))
        (edges   (make-hash-table :test 'eql))
        (visited (make-hash-table :test 'eql)))
    (dolist (id ids)
      (puthash id t present)
      (puthash id t visited)
      (puthash id (funcall id->neighbors id) edges))
    (list edges present visited)))

(defun graph-bfs-frontier (edges visited)
  "Return the unvisited neighbor ids reachable from EDGES, minus VISITED.
This is the next BFS layer: every neighbor mentioned in EDGES whose own
neighbors we have not yet fetched."
  (let (ids)
    (maphash (lambda (_id nbrs) (dolist (nb nbrs) (push nb ids))) edges)
    (seq-filter (lambda (id) (not (gethash id visited))) (seq-uniq ids))))

(defun graph-edge-map-absorb (frontier-nodes node->id node->neighbors edges visited)
  "Fold FRONTIER-NODES into EDGES, marking each id VISITED.
FRONTIER-NODES are freshly-fetched records; NODE->ID and NODE->NEIGHBORS
extract an id and its neighbor ids from each.  Mutates EDGES and VISITED."
  (dolist (node frontier-nodes)
    (let ((id (funcall node->id node)))
      (puthash id t visited)
      (puthash id (funcall node->neighbors node) edges))))

(defun graph-union-find (ids edges present)
  "Partition IDS into connected components by Union-Find over EDGES.
EDGES maps id → neighbor ids.  Only neighbors in PRESENT join a
component directly; neighbors outside PRESENT are traversed as bridges
(their own neighbors are followed) but never themselves grouped.
Returns a list of components, each a list of ids, the list itself
sorted by descending max-id (newest component first) and each
component sorted ascending."
  (let ((parent (make-hash-table :test 'eql)))
    (dolist (id ids) (puthash id id parent))
    (cl-labels
        ((uf-find (x)
           (let ((p (gethash x parent x)))
             (if (eql p x) x
               (let ((root (uf-find p)))
                 (puthash x root parent) root))))
         (uf-union (a b)
           (let ((ra (uf-find a)) (rb (uf-find b)))
             (unless (eql ra rb) (puthash ra rb parent)))))
      ;; Walk each node's reachable set; union same-component PRESENT nodes,
      ;; hop through bridge nodes (absent from PRESENT) to find more.
      (dolist (id ids)
        (let ((queue (list id))
              (seen  (make-hash-table :test 'eql)))
          (puthash id t seen)
          (while queue
            (let ((cur (pop queue)))
              (dolist (nb (gethash cur edges))
                (unless (gethash nb seen)
                  (puthash nb t seen)
                  (if (gethash nb present)
                      (uf-union id nb)
                    (push nb queue))))))))
      ;; Collect ids by representative, then sort.
      (let ((groups (make-hash-table :test 'eql))
            comps)
        (dolist (id ids)
          (let ((root (uf-find id)))
            (puthash root (cons id (gethash root groups)) groups)))
        (maphash (lambda (_root members)
                   (push (sort (copy-sequence members) #'<) comps))
                 groups)
        (sort comps (lambda (a b) (> (car (last a)) (car (last b)))))))))

(defun graph--demo ()
  "Self-check: connected nodes group, bridges link, isolates stay alone.
Run with \\[eval-expression] (graph--demo) or `emacs --batch -l graph.el \\
--eval (graph--demo)'."
  (let ((edges   (make-hash-table :test 'eql))
        (present (make-hash-table :test 'eql)))
    (dolist (n '(1 2 3 4 5)) (puthash n t present))
    (puthash 1 '(2) edges) (puthash 2 '(1) edges)   ; 1-2 directly linked
    (puthash 3 '()  edges)                           ; 3 isolated
    (puthash 4 '(99) edges) (puthash 5 '(99) edges)  ; 4,5 linked via bridge 99
    (puthash 99 '(4 5) edges)                        ; 99 absent from PRESENT
    (let ((comps (graph-union-find '(1 2 3 4 5) edges present)))
      ;; Newest-stack-first ordering, members ascending.
      (cl-assert (equal comps '((4 5) (3) (1 2))) t "graph-union-find grouping")
      (message "graph--demo: OK %S" comps))))

(provide 'graph)
;;; graph.el ends here
