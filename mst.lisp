;;;; -*- Mode: Lisp -*-

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex_keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *heap-entries* (make-hash-table :test #'equal)) 
;;;; chiave (heap-id V) valore (K Pos Parent)

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
  (if (is-graph graph-id)
      (and 
       (remhash graph-id *graphs*)
       (maphash (lambda (k v) 
                  (if (equal (second k) graph-id) 
                      (remhash k *vertices*))
                  ) *vertices*)
       (maphash (lambda (k v) 
                  (if (equal (second k) graph-id) 
                      (remhash k *arcs*))
                  ) *arcs*))))

;;;; new vertex/2

(defun new-vertex (graph-id vertex-id)
  (if (is-graph graph-id)
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
        (list 'vertex graph-id vertex-id))
  (format t "Il grafo ~S non esiste" graph-id)))

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


;;;; new-heap

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

;;;; is-heap

(defun is-heap (heap-id)
  (gethash heap-id *heaps*))

;;;; heap-delete

(defun heap-delete (heap-id)
  (if (is-heap heap-id)    
      (remhash heap-id *heaps*)))

;;;; heap-empty

(defun heap-empty (heap-id)
  (if (is-heap heap-id)
      (= (third (gethash heap-id *heaps*)) 0)))

;;;; heap-not-empty

(defun heap-not-empty (heap-id)
  (if (is-heap heap-id)
      (> (third (gethash heap-id *heaps*)) 0)))

(defun actual-heap (heap-id)
  (fourth (gethash heap-id *heaps*)))

(defun heap-head (heap-id)
  (if (is-heap heap-id)
      (aref (actual-heap heap-id) 0)))

; get-key from *heap-entries*

(defun get-key (heap-id V) 
  (first (gethash (list heap-id V) *heap-entries*)))

;get-pos from *heap-entries*

(defun get-pos (heap-id V)
  (second (gethash (list heap-id V) *heap-entries*)))
      
; ritorna V del parent data la pos di un figlio
(defun get-parent (heap-id pos)
  (second (aref 
           (actual-heap heap-id)
           (get-pos-parent pos)
           )))

; ritorna la coppia (K V) del figlio sx data la pos del padre

(defun get-left-child (heap-id pos)
  (aref
   (actual-heap heap-id)
   (get-pos-left-child pos)
  ))

; ritorna la coppia (K V) del figlio dx data la pos del padre

(defun get-right-child (heap-id pos)
  (aref
   (actual-heap heap-id)
   (get-pos-right-child pos)
   ))

; ritorna V del figlio sx dato la pos del padre

(defun get-left-child-value (heap-id pos)
  (second (get-left-child heap-id pos)))

; ritorna V del figlio dx dato la pos del padre

(defun get-right-child-value (heap-id pos)
  (second (get-right-child heap-id pos)))

(defun get-pos-parent (pos)
  (max 
   (floor (/ (1- pos) 2)) 
   0)
  )

(defun get-pos-left-child (pos)
  (1+ (* 2 pos)))

(defun get-pos-right-child (pos)
  (+ 2 (* 2 pos)))

(defun heap-size (heap-id)
  (third (gethash heap-id *heaps*)))

(defun incr-size (heap-id)
  (setf 
   (gethash heap-id *heaps*) 
   (list 'heap heap-id (1+ (heap-size heap-id)) (actual-heap heap-id)))) 

(defun decr-size (heap-id)
  (setf
   (gethash heap-id *heaps*)
   (list 'heap heap-id (1- (heap-size heap-id)) (actual-heap heap-id))))

;;;; heap-insert - TO DO RITORNARE UN BOOLEANO

(defun heap-insert (heap-id K V)
  (if (is-heap heap-id)
      (or
       (cond ((gethash (list heap-id V) *heap-entries*) ; se il vertice e' dentro HEAP
              (if (> (get-key heap-id V) K) ; se Kold > Knew
                  (and ;qua devo sostituire con la heap-modify-key
                   (setf (gethash (list heap-id V) *heap-entries*) ; mod heap-entries
                         (list K (get-pos heap-id V) (get-parent heap-id (get-pos heap-id V))))
                  ; mod actual-heap
                   (setf (aref (actual-heap heap-id)
                               (get-pos heap-id V)) 
                         (list K V)) 
                   (heapify heap-id (heap-size heap-id) (get-pos heap-id V))
                   )))
             (t (and ; aggiungo un nuovo heap entry 
                 (incr-size heap-id)
                 (setf (aref (actual-heap heap-id) (1- (heap-size heap-id))) (list K V))
                 (setf (gethash (list heap-id V) *heap-entries*) ;aggiunge entry
                       (list K (1- (heap-size heap-id)) (get-parent heap-id (1- (heap-size heap-id)))))
                 (heapify heap-id (heap-size heap-id) (1- (heap-size heap-id)))
                 )))t)))

(defun heapify (heap-id s i)
  (and
   (cond ((= (heap-size heap-id) 0) (format t "Caso base"))
        ; se il figlio e' minore di parent, swap
         ((< 
           (first (aref (actual-heap heap-id) i)) ; figlio
           (first (aref (actual-heap heap-id) (get-pos-parent i))) ;padre
           )
         ;swap
          (and 
           (swap heap-id i (get-pos-parent i))
           (heapify heap-id s (get-pos-parent i))
           ))
         ;se sono uguali ma con figlio con valore lessic. minore
         ((and
           (=
            (first (aref (actual-heap heap-id) i))
            (first (aref (actual-heap heap-id) (get-pos-parent i)))
            )
           (string-lessp
            (second (aref (actual-heap heap-id) i))
            (second (aref (actual-heap heap-id) (get-pos-parent i)))
            )
           t)
          (swap heap-id i (get-pos-parent i)))
         )) )
        

        
(defun swap (heap-id pos-child pos-parent)
  (let ((value-child (aref (actual-heap heap-id) pos-child)) ; (K V) child
        (value-parent (aref (actual-heap heap-id) pos-parent)) ; (K V) parent
        )
    (and 
     (setf (aref (actual-heap heap-id) pos-child) ; modifico child
           value-parent)
     (setf (aref (actual-heap heap-id) pos-parent) ; modifico parent
           value-child)
     (setf (gethash (list heap-id (second value-child)) *heap-entries*) ; set new parent
           (list (first value-child) pos-parent (get-parent heap-id pos-parent)))
     (setf (gethash (list heap-id (second value-parent)) *heap-entries*) ;set new child
           (list (first value-parent) pos-child (get-parent heap-id pos-child)))
     (fix-parent-children heap-id pos-parent)
     )))

; sistema nella heap-entries il campo parent dei figli

(defun fix-parent-children (heap-id pos-parent)
  (and
   (if (not (null (gethash (list heap-id (get-left-child-value heap-id pos-parent)) *heap-entries*)))
       (setf (third (gethash 
                     (list heap-id (get-left-child-value heap-id pos-parent)) 
                     *heap-entries*)) ;sistemo figlio sx
             (second (aref (actual-heap heap-id) pos-parent))))
   (if (not (null (gethash (list heap-id (get-right-child-value heap-id pos-parent)) *heap-entries*)))
       (setf (third (gethash
                     (list heap-id (get-right-child-value heap-id pos-parent))
                     *heap-entries*)) ;sistemo figlio dx
             (second (aref (actual-heap heap-id) pos-parent))))
   ))
   
; USO ADJUST-ARRAY PER CAMBIARE LA DIMENSIONE

;;;; heap extract 

;(defun heap-extract (heap-id k v)
;  (new-heap)
;  (cond
; l'heap  vuoto ritorno l'heap
;  ((= (funcall 'heap-size heap-id) 0) heap-id)
; l'heap ha solo un nodo, tolgo il nodo e ritorno l'heap
;  ((= (funcall 'heap-size heap-id) 1)) 
;   ((remhash (v hashtable))(remhash (k hashtable)) 
;    heap-id)
; altrimenti tolgo il nodo e chiamo la fix-heap  
;  (t ((remhash v hashtable) (remhash k hastable)) funcall 'fix-heap (heap-id k v))))

;;;; fix heap

;(defun fix-heap (heap-id k v)
;(cond 
; i = 0
; prendo la radice di v
;  (vertex-previous (g v) root)
; metto l'ultimo elemento nella radice
; last = (setf (root 0))
;trovo i 2 figli della radice
; left = (+ (ash i 1) 1)
; right = (+ (ash i 1) 2)
; scelgo qual  il migliore 
;(setf
; (cond
;  ((< leftk rightk) leftk)
;  (t (rightk))) newk)
;richiamo la fix heap solo se v ha figli (???)
; ((and (null left) (null right) heap-id))
; (t (fix-heap (heap-id root newk)))))

;;; TEST

(new-graph 'my-graph)
(new-vertex 'my-graph 'v)
(new-vertex 'my-graph 'u)
(new-vertex 'my-graph 'z)
(new-arc 'my-graph 'u 'v)
(new-arc 'my-graph 'u 'z)
(new-heap 'my-heap)
(heap-insert 'my-heap 3 'a)

;;;; end of file -- mst.lisp