(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex_keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heap* (make-hash-table :test #'equal))

;;;; is graph

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

;;;; new graph

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
  (setf (gethash graph-id *graphs*) graph-id)))

;;;; delete graph

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*))

;;;; new vertex

(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
  (list 'vertex graph-id vertex-id)))

;;;; graph vertices

(defun graph-vertices (graph-id)))

;;;; new arc

(defun new-arc (graph-id vertex-id vertex-id &optional weight)
  (setf (gethash (list 'arc graph-id arc-id) *arcs*)
  (list 'arc graph-id arc-id)))

;;;; graph arcs

;;; cos'è vertex rep list????

;;;; graph vertex neighbors


;;;; graph vertex adjacent



;;;; graph print



;;;; mst



;;;; heap

;;;; new-heap

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (seatf (gethash heap-id *heaps*)
      (list 'heap heap-id 0 (make-array capcity)))))

(defgeneric heap-size (heap))

(defgeneric is-empty-heap-p (heap))

;;;; delete heap

(defun delete-heap (heap-id)
  (remhash heap-id *heaps*))

;;;; heap empty 

(defun heap-empty (heap-id)
  (cond 
  (t (aref 'heap-id 1) 0)))

;;;; heap not empty

(defun heap-not-empty (heap-id)
  (t (not 'heap-empty)))

;;;; heap-insert

(defun heap-insert (heap-id k v)
  )


