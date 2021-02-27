; Kobril Christian 856448, Cicalla Eleonora 851649

;;;; -*- Mode: Lisp -*-

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *heap-entries* (make-hash-table :test #'equal)) 

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
          (setf (gethash 
                 (list 'arc graph-id vertex-id-1 vertex-id-2 weight) *arcs*)
                (list 'arc graph-id vertex-id-1 vertex-id-2 weight))
          (setf (gethash 
                 (list 'arc graph-id vertex-id-2 vertex-id-1 weight) *arcs*)
                (list 'arc graph-id vertex-id-2 vertex-id-1 weight))
          ))))

;;;; find-arc/3 - trova l'arco tra u e v del grafo g

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

;;;; graph-vertex-neighbors/2 

(defun graph-vertex-neighbors (graph-id vertex-id)
  (if (gethash (list 'vertex graph-id vertex-id) *vertices*)
      (let ((arc-rep-list ()))
        (progn
          (maphash (lambda (k v)
                     (if (and (equal (second k) graph-id)
                              (equal (third k) vertex-id))
                         (push v arc-rep-list))
                     ) *arcs*)
          (setf arc-rep-list 
                (sort (copy-list arc-rep-list) #'string-lessp :key 'fourth))
          )
        arc-rep-list)))

;;; graph-vertex-adjacent/2 

(defun graph-vertex-adjacent (graph-id vertex-id)
  (if (gethash (list 'vertex graph-id vertex-id) *vertices*)
      (let ((vertex-rep-list ()))
        (maphash (lambda (k v)
                   (if (and (equal (second k) graph-id)
                            (equal (third k) vertex-id))
                      (push (list 'vertex graph-id (fourth v)) vertex-rep-list))
                   ) *arcs*)
        vertex-rep-list)))

;;;; graph-print/1

(defun graph-print (graph-id)
  (if (is-graph graph-id)
      (append (graph-vertices graph-id) (graph-arcs graph-id))
    ))



;;;; HEAP



;;;; new-heap/2

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

;;;; is-heap/1

(defun is-heap (heap-id)
  (gethash heap-id *heaps*))

;;;; heap-delete/1

(defun heap-delete (heap-id)
  (if (is-heap heap-id)    
      (remhash heap-id *heaps*)))

;;;; heap-empty/1

(defun heap-empty (heap-id)
  (if (is-heap heap-id)
      (= (third (gethash heap-id *heaps*)) 0)))

;;;; heap-not-empty/1

(defun heap-not-empty (heap-id)
  (if (is-heap heap-id)
      (> (third (gethash heap-id *heaps*)) 0)))

;;;; actual-heap/1

(defun actual-heap (heap-id)
  (fourth (gethash heap-id *heaps*)))

;;;; heap-head/1

(defun heap-head (heap-id)
  (if (is-heap heap-id)
      (aref (actual-heap heap-id) 0)))

; get-key/2  da *heap-entries*

(defun get-key (heap-id V) 
  (first (gethash (list heap-id V) *heap-entries*)))

; get-pos/2 da *heap-entries*

(defun get-pos (heap-id V)
  (second (gethash (list heap-id V) *heap-entries*)))
      
; get-parent/2 ritorna V del padre data la posizione di un figlio
(defun get-parent (heap-id pos)
  (second (aref 
           (actual-heap heap-id)
           (get-pos-parent pos)
           )))

; get-left-child/2 ritorna la coppia (K V) del figlio sx data la pos del padre

(defun get-left-child (heap-id pos)
  (aref
   (actual-heap heap-id)
   (get-pos-left-child pos)
   ))

; get-right-child/2 ritorna la coppia (K V) del figlio dx data la pos del padre

(defun get-right-child (heap-id pos)
  (aref
   (actual-heap heap-id)
   (get-pos-right-child pos)
   ))

; get-left-child-value/2 ritorna V del figlio sx dato la posizione del padre

(defun get-left-child-value (heap-id pos)
  (second (get-left-child heap-id pos)))

; get-right-child-value/ 2 ritorna V del figlio dx dato la posizione del padre

(defun get-right-child-value (heap-id pos)
  (second (get-right-child heap-id pos)))

; get-pos-parent/1 ritorna la posizione del padre data la posizione di un figlio

(defun get-pos-parent (pos)
  (max 
   (floor (/ (1- pos) 2)) 
   0)
  )

; get-pos-left-child/1 ritorna posizione figlio sx data quella del padre

(defun get-pos-left-child (pos)
  (1+ (* 2 pos)))

; get-pos-left-child/1 ritorna posizione figlio sx data quella del padre

(defun get-pos-right-child (pos)
  (+ 2 (* 2 pos)))

; heap-size/1

(defun heap-size (heap-id)
  (third (gethash heap-id *heaps*)))

; incr-size/1 incrementa la size dell'heap di 1

(defun incr-size (heap-id)
  (setf 
   (gethash heap-id *heaps*) 
   (list 'heap heap-id (1+ (heap-size heap-id)) (actual-heap heap-id)))) 

; decr-size/1 decrementa la size dell'heap di 1

(defun decr-size (heap-id)
  (setf
   (gethash heap-id *heaps*)
   (list 'heap heap-id (1- (heap-size heap-id)) (actual-heap heap-id))))

; get-key-from-arc/1 ritorna la chiave dato un arco
(defun get-key-from-arc (arc)
  (fifth arc))

; get-value-from-arc/1 ritorna il valore del secondo vertice dato un arco

(defun get-value-from-arc (arc)
  (fourth arc))

; heap-insert-from-list/3 dati degli archi vs inserisce nell'heap

(defun heap-insert-from-list (heap-id graph-id vs)
 (cond ((= (heap-size heap-id) (first (array-dimensions (actual-heap heap-id))))
         (format t "L'ARRAY E' PIENO")
         )
        ; se null finisco
        ((null vs) ())
        ;se K in vertex-key è inf aggiungo
        ((= (mst-vertex-key graph-id (get-value-from-arc (first vs)))
                           MOST-POSITIVE-DOUBLE-FLOAT)
         (progn
           (heap-insert heap-id 
                        (get-key-from-arc (first vs)) 
                        (get-value-from-arc (first vs))
                        )
           (heap-insert-from-list heap-id graph-id (rest vs))
           ))
        (t (heap-insert-from-list heap-id graph-id (rest vs)))
        ))

;;;; heap-insert/3                              

(defun heap-insert (heap-id K V)
  (if (is-heap heap-id)
      (or
       (cond ((gethash (list heap-id V) *heap-entries*)
              (if (> (get-key heap-id V) K) ; se Kold > Knew
                  (heap-modify-key heap-id K (get-key heap-id V) V)
                ))
             (t (and ; aggiungo un nuovo heap entry 
               (incr-size heap-id)
               (setf (aref (actual-heap heap-id) (1- (heap-size heap-id)))
                     (list K V))
               (setf (gethash (list heap-id V) *heap-entries*) ;aggiunge entry
                       (list K (1- (heap-size heap-id)) 
                             (get-parent heap-id (1- (heap-size heap-id)))))
                 (heapify heap-id (heap-size heap-id) (1- (heap-size heap-id)))
                 )))t)))

;;;; heapify/3 sistema l'heap a partire dall'ultima foglia

(defun heapify (heap-id s i)
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
         ))

; heap-modify-key/4
     
(defun heap-modify-key (heap-id new-key old-key V)
  (if (and (is-heap heap-id) (= (get-key heap-id V) old-key))
      (and
       (setf (gethash (list heap-id V) *heap-entries*) ; mod heap-entries
             (list new-key (get-pos heap-id V) 
                   (get-parent heap-id (get-pos heap-id V))))
        ; mod actual-heap
       (setf (aref (actual-heap heap-id)
                   (get-pos heap-id V)) 
             (list new-key V)) 
       (heapify heap-id (heap-size heap-id) (get-pos heap-id V))
       )))
      
; swap/3 scambia due elementi (padre e figlio) all'interno dell'heap 
       
(defun swap (heap-id pos-child pos-parent)
  (let ((value-child (aref (actual-heap heap-id) pos-child)) ; (K V) child
        (value-parent (aref (actual-heap heap-id) pos-parent)) ; (K V) parent
        )
    (and 
     (setf (aref (actual-heap heap-id) pos-child) ; modifico child
           value-parent)
     (setf (aref (actual-heap heap-id) pos-parent) ; modifico parent
           value-child)
     (setf (gethash (list heap-id (second value-child)) *heap-entries*)
         (list (first value-child) pos-parent (get-parent heap-id pos-parent)))
     (setf (gethash (list heap-id (second value-parent)) *heap-entries*)
         (list (first value-parent) pos-child (get-parent heap-id pos-child)))
     (fix-parent-children heap-id pos-parent)
     )))

; fix-parent-children/2 sistema nella heap-entries il campo parent dei figli

(defun fix-parent-children (heap-id pos-parent)
  (and
   (if (not (null (gethash (list 
                            heap-id (get-left-child-value heap-id pos-parent))
                           *heap-entries*)))
       (setf (third (gethash 
                     (list heap-id (get-left-child-value heap-id pos-parent)) 
                     *heap-entries*)) ;sistemo figlio sx
             (second (aref (actual-heap heap-id) pos-parent))))
   (if (not (null (gethash (list 
                            heap-id (get-right-child-value heap-id pos-parent))
                           *heap-entries*)))
       (setf (third (gethash
                     (list heap-id (get-right-child-value heap-id pos-parent))
                     *heap-entries*)) ;sistemo figlio dx
             (second (aref (actual-heap heap-id) pos-parent))))
   ))
   


;;;; heap extract/1

(defun heap-extract (heap-id) 
  (if (and (is-heap heap-id) (heap-not-empty heap-id))
      (let ((head (heap-head heap-id)))
        (cond ((= (heap-size heap-id) 1) ;se la dim e' 1 cancello tutto
               (and
                (not (setf (aref (actual-heap heap-id) 0) nil))
                (decr-size heap-id)
                (clrhash *heap-entries*)
                )
               )
              (t 
               (and
                (setf ; metto foglia nella radice
                 (aref (actual-heap heap-id) 0)
                 (aref (actual-heap heap-id) (1- (heap-size heap-id))))
                ; eliminare la root in heap-entries
                (remhash (list heap-id (second head))
                         *heap-entries*)
                 (not (setf ;cancello la foglia
                 (aref (actual-heap heap-id) (1- (heap-size heap-id)))
                 nil))
                (decr-size heap-id)
                (fix-heap heap-id (heap-size heap-id) 0)
                ))) head)))


; fix-heap/3 sistema l'heap a partire dalla radice

(defun fix-heap (heap-id s i)
  (let ((kl (get-key heap-id (get-left-child-value heap-id i)))
        (kr (get-key heap-id (get-right-child-value heap-id i)))
        (km (first (aref (actual-heap heap-id) i)))
        )
    (cond 
     ((and ;se kr e' il minore 
       (not (null kr))
       (= (min kl kr km) kr)
       )
      (and
       (swap heap-id (get-pos-right-child i) i)
       (fix-heap heap-id s (get-pos-right-child i))
       ))
     ((and ;se kl e' il minore 
       (not (null kr))
       (= (min kl kr km) kl)
       )
      (and
       (swap heap-id (get-pos-left-child i) i)
       (fix-heap heap-id s (get-pos-left-child i))
       )
      )
     ((and
       (not (null kl))
       (< kl km)
       )
          ; no figlio dx e kl e' il minore
      (swap heap-id (get-pos-left-child i) i))
     ((and
       (not (null kl))
       (<= km kl)
       (string-lessp 
        (get-left-child-value heap-id i) ; value figlio sx
        (second (aref (actual-heap heap-id) i)) ;value padre
        )
       )
      (swap heap-id (get-pos-left-child i) i)))
     ))

;;;; heap-print/1

(defun heap-print (heap-id)
  (format t "~A ~%" heap-id)
  (format t "SIZE ~A" (heap-size heap-id))
  (recur-heap-print heap-id 0))

(defun recur-heap-print (heap-id x)
  (if (< x (heap-size heap-id))
  (and (print (aref (actual-heap heap-id) x))
  (recur-heap-print heap-id (1+ x)))))  


;;;; MST

; mst-prim/2

(defun mst-prim (graph-id source)
  (if (is-graph graph-id)
      (progn
       (reset)
       (set-inf graph-id)
       (setf (gethash (list graph-id source) *vertex-keys*)
             0)
       (new-heap 'heap)
       (heap-insert-from-list 'heap graph-id 
       (graph-vertex-neighbors graph-id source))
       (recursive-mst-prim 'heap graph-id))
    )
  )

; recursive-mst-prim/2 parte ricorsiva dell'mst prim

(defun recursive-mst-prim (heap-id graph-id)
  (cond ((heap-empty heap-id) ())
        (t (let ((head (heap-head heap-id)))
             (progn
               (heap-extract heap-id) 
               (setf (gethash (list graph-id (second head)) *vertex-keys*)
                     (first head))
               (find-min-arc graph-id 
               (sort-arc (graph-vertex-neighbors graph-id (second head))) 
               (second head))
               (heap-insert-from-list heap-id graph-id
               (graph-vertex-neighbors graph-id (second head)))
               (recursive-mst-prim heap-id graph-id)
               )
             ))))

; find-min-arc/3 trova l'arco minimo con un vertice nell'mst
; dato un grafo e i neighbors di V

(defun find-min-arc (graph-id Lvs V)
  (cond ((= (mst-vertex-key graph-id (get-value-from-arc (first Lvs))) 
            MOST-POSITIVE-DOUBLE-FLOAT) ;se non e' visitato
         (find-min-arc graph-id (rest Lvs) V))
        (t (and
            (setf (gethash (list graph-id V) *previous*)
                 (get-value-from-arc (first Lvs)))
            )
        )))

; sort-arc/1 ordina una lista di archi per peso crescente

(defun sort-arc (arcs)
  (sort (copy-list arcs) #'< :key #'fifth)
  )

; set-inf/1 imposta tutti i vertex key con chiave 
; a MOST-POSITIVE-DOUBLE-FLOAT
        
(defun set-inf (graph-id)
  (maphash (lambda (k v)
             (if (equal (second k) graph-id)
                 (setf (gethash (list graph-id (third v)) *vertex-keys*)
                       MOST-POSITIVE-DOUBLE-FLOAT)))
           *vertices*))

; mst-get/2 ritorna pre-order-mst

(defun mst-get (graph-id source)
  (if (is-graph graph-id)
      (let ((pre-order-mst (make-array 1 :fill-pointer 0 :adjustable t)))
        (progn
          (visit-mst graph-id source 
                     (get-mst-children graph-id source)
                     pre-order-mst)
          ) pre-order-mst
        )))

; visit-mst/4 visita in pre-order l'mst e ritorna il pre-order-mst

(defun visit-mst (graph-id parent vertexes pre-order-mst)
  (cond ((null (first vertexes)) ())
        (t
         (progn
           (if (not (find (first vertexes) pre-order-mst))
               (vector-push-extend (first vertexes) pre-order-mst)) 
           (cond ((null (get-mst-children graph-id (fourth (first vertexes))))
                  (visit-mst graph-id parent (rest vertexes) pre-order-mst))
                 (t (visit-mst graph-id
                               (fourth (first vertexes))
                               (get-mst-children graph-id
                                                 (fourth (first vertexes)))
                               pre-order-mst)
                    )
                 )
           (visit-mst graph-id parent (rest vertexes) pre-order-mst)
           )
         )
        )
  )
           
           
; get-mst-children/2 dato un parent ricava i figli
; all'interno del mst come lista di archi
      
(defun get-mst-children (graph-id v-parent)
  (let ((children ()))
    (setf children 
          (sort 
           (remove-if-not #'is-children 
                          (graph-vertex-neighbors graph-id v-parent)
                          )
           #'< :key 'fifth))
          children))

; is-children/1 dato un arco verifica che il quarto valore sia il padre nell'mst

(defun is-children (arc)
  (equal (mst-previous (second arc) (fourth arc)) (third arc))
  )

         
; mst-vertex-key/2

(defun mst-vertex-key (graph-id V)
  (if (gethash (list 'vertex graph-id V) *vertices*)
      (gethash (list graph-id V) *vertex-keys*)
  ))

; mst-previous/2

(defun mst-previous (graph-id V)
  (if (gethash (list 'vertex graph-id V) *vertices*)
      (gethash (list graph-id V) *previous*)
    ))
            

; reset/0 cancella tutte le hashtable utilizzate durante l'mst prim
(defun reset ()
  (and
   (clrhash *heaps*)
   (clrhash *heap-entries*)
   (clrhash *vertex-keys*)
   (clrhash *previous*)
   ))
               
               
                                                                              
;;; TEST

(new-graph 'my-graph)
(new-vertex 'my-graph 'a)
(new-vertex 'my-graph 'b)
(new-vertex 'my-graph 'c)
(new-vertex 'my-graph 'd)
(new-vertex 'my-graph 'e)
(new-vertex 'my-graph 'f)
(new-vertex 'my-graph 'g)
(new-arc 'my-graph 'a 'b 2)
(new-arc 'my-graph 'a 'c 3)
(new-arc 'my-graph 'a 'd 3)
(new-arc 'my-graph 'b 'e 3)
(new-arc 'my-graph 'b 'c 4)
(new-arc 'my-graph 'c 'd 5)
(new-arc 'my-graph 'c 'e 1)
(new-arc 'my-graph 'c 'f 6)
(new-arc 'my-graph 'd 'f 7)
(new-arc 'my-graph 'f 'e 8)
(new-arc 'my-graph 'f 'g 9)

(new-graph 'graph)
(new-vertex 'graph 'a)
(new-vertex 'graph 'b)
(new-vertex 'graph 'c)
(new-vertex 'graph 'd)
(new-vertex 'graph 'e)
(new-vertex 'graph 'f)
(new-vertex 'graph 'g)
(new-vertex 'graph 'h)
(new-vertex 'graph 'i)
(new-arc 'graph 'a 'b 4)
(new-arc 'graph 'a 'h 8)
(new-arc 'graph 'b 'h 11)
(new-arc 'graph 'b 'c 8)
(new-arc 'graph 'c 'i 2)
(new-arc 'graph 'c 'd 7)
(new-arc 'graph 'c 'f 4)
(new-arc 'graph 'd 'e 9)
(new-arc 'graph 'd 'f 14)
(new-arc 'graph 'e 'f 10)
(new-arc 'graph 'f 'g 2)
(new-arc 'graph 'g 'i 6)
(new-arc 'graph 'g 'h 1)
(new-arc 'graph 'i 'h 7)



(new-heap 'my-heap)
(heap-insert 'my-heap 3 'a)
(heap-insert 'my-heap 1 'b)
(heap-insert 'my-heap 6 'c)
(heap-insert 'my-heap 5 'd)
(heap-insert 'my-heap 2 'e)
(heap-insert 'my-heap 4 'f)

;;;; end of file -- mst.lisp