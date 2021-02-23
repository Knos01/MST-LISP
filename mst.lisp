;;;; -*- Mode: Lisp -*-

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex_keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

;usare (inspect *hash-table*) per visualizzarne il contenuto

;;;; is-graph/1

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

;;;; new graph/1

(defun new-graph (graph-id)
 (if (null graph-id)
      (format t "Errore: ~S non e' un nome valido" graph-id)
  (or (gethash graph-id *graphs*)
  (setf (gethash graph-id *graphs*) graph-id))))

;;;; delete graph/1

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (maphash (lambda (k v) 
             (if (equal (second k) graph-id) 
                 (remhash k *vertices*))
           ) *vertices*)
  (maphash (lambda (k v) 
             (if (equal (second k) graph-id) 
                 (remhash k *arcs*))
           ) *arcs*))

;;;; new vertex/2

(defun new-vertex (graph-id vertex-id)
  (if (is-graph graph-id)
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
        (list 'vertex graph-id vertex-id)
  (format t "Il grafo ~S non esiste" graph-id))))

;;;; graph vertices/1

(defun graph-vertices (graph-id)
  (if (not (is-graph graph-id))
      (format t "Il grafo ~S non esiste" graph-id)
    (let ((vertex-rep-list ()))
        (maphash (lambda (k v)
                   (if (equal (second k) graph-id)
                       (push v vertex-rep-list))
                   ) *vertices*)
        vertex-rep-list )))


;;;; new arc/4

(defun new-arc (graph-id vertex-id-1 vertex-id-2 &optional (weight 1))
  (cond ((and (gethash (list 'vertex graph-id vertex-id-1) *vertices*)
              (gethash (list 'vertex graph-id vertex-id-2) *vertices*)) 
    (and
      (setf (gethash (list 'arc graph-id vertex-id-1 vertex-id-2 weight) *arcs*)
            (list 'arc graph-id vertex-id-1 vertex-id-2 weight))
      (setf (gethash (list 'arc graph-id vertex-id-2 vertex-id-1 weight) *arcs*)
            (list 'arc graph-id vertex-id-2 vertex-id-1 weight))
      ))))

(defun find-arc (g u v)
  (or (gethash (list g u v) *arcs*)
      (gethash (list g v u) *arcs*)))

;;;; graph-arcs/1

(defun graph-arcs (graph-id)
  (if (is-graph graph-id)
      (let ((arc-rep-list ()))
        (maphash (lambda (k v)
                   (if (equal (second k) graph-id)
                       (push v arc-rep-list))
                   ) *arcs*)
        arc-rep-list)))

;;;; graph-vertex-neighbours

(defun graph-vertex-neighbors (graph-id vertex-id)
  (if (gethash (list 'vertex graph-id vertex-id) *vertices*)
      (let ((arc-rep-list ()))
        (maphash (lambda (k v)
                   (if (and (equal (second k) graph-id)
                            (equal (third k) vertex-id))
                       (push v arc-rep-list))
                   ) *arcs*)
        arc-rep-list)))

;;; graph-vertex-adjacent 

(defun graph-vertex-adjacent (graph-id vertex-id)
  (if (gethash (list 'vertex graph-id vertex-id) *vertices*)
      (let ((vertex-rep-list ()))
        (maphash (lambda (k v)
                   (if (and (equal (second k) graph-id)
                            (equal (third k) vertex-id))
                       (push (list 'vertex graph-id (fourth v)) vertex-rep-list))
                   ) *arcs*)
        vertex-rep-list)))

;;;; graph-print

(defun graph-print (graph-id)
  (if (is-graph graph-id)
      (append (graph-vertices graph-id) (graph-arcs graph-id))
    ))


;;; new-heap

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

;;;; heap extract 

(defun heap-extract (heap-id k v)
  (new-heap)
  (cond
; l'heap è vuoto ritorno l'heap
  ((= (funcall 'heap-size heap-id) 0) heap-id)
; l'heap ha solo un nodo, tolgo il nodo e ritorno l'heap
  ((= (funcall 'heap-size heap-id) 1)) 
   ((remhash (v hashtable))(remhash (k hashtable)) 
    heap-id)
; altrimenti tolgo il nodo e chiamo la fix-heap  
  (t ((remhash v hashtable) (remhash k hastable)) funcall 'fix-heap (heap-id k v)))

;;;; fix heap

(defun fix-heap (heap-id k v)
(cond 
 i = 0
; prendo la radice di v
  (vertex-previous (g v) root)
; metto l'ultimo elemento nella radice
 last = (setf (root 0))
;trovo i 2 figli della radice
 left = (+ (ash i 1) 1)
 right = (+ (ash i 1) 2)
; scelgo qual è il migliore 
(setf
 (cond
  ((< leftk rightk) leftk)
  (t (rightk))) newk)
;richiamo la fix heap solo se v ha figli (???)
 ((and (null left) (null right) heap-id))
 (t (fix-heap (heap-id root newk)))))

;;; TEST

(new-graph 'my-graph)
(new-vertex 'my-graph 'v)
(new-vertex 'my-graph 'u)
(new-vertex 'my-graph 'z)
(new-arc 'my-graph 'u 'v)
(new-arc 'my-graph 'u 'z)

;;;; end of file -- mst.lisp



