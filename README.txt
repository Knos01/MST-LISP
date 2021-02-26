Kobril Christian 856448, Cicalla Eleonora 851649

; is-graph

Questa funzione ritorna il graph-id stesso se questo grafo è già stato creato, oppure NIL se no. Una sua implementazione è semplicemente.


; new-graph

Questa funzione genera un nuovo grafo e lo inserisce nel data base (ovvero nella hash-table) dei grafi.


; delete-graph

Rimuove l’intero grafo dal sistema (vertici archi etc); ovvero rimuove tutte le istanze presenti nei data base (ovvero nelle hash-tables) del sistema.


; new-vertex

Aggiunge un nuovo vertice vertex-id al grafo graph-id. Notate come la rappresentazione di un vertice associ un vertice ad un grafo (o più).


; graph-vertices

Questa funzione torna una lista di vertici del grafo.


; new-arc

Questa funzione aggiunge un arco del grafo graph-id nella hash-table *arcs*.


; find-arc

???


; graph-arcs

Questo funzione ritorna una lista una lista di tutti gli archi presenti in graph-id.


; graph-vertex-neighbors

Questa funzione ritorna una lista arc-rep-list contenente gli archi che portano ai vertici N immediatamente raggiungibili da vertex-id.


; graph-vertex-adjacent

Questa funzione ritorna una lista vertex-rep-list contenente i vertici adiacenti a vertex-id.


; graph-print

Questa funzione stampa alla console dell’interprete Common Lisp una lista dei vertici e degli archi del grafo graph-id.

; new-heap

Questa funzione inserisce un nuovo heap nella hash-table *heaps*.


; is-heap

Questa funzione controlla se l'heap passato in input esiste.


; heap-delete

Rimuove tutto lo heap indicizzato da heap-id.


; heap-empty

Questo predicato è vero quando lo heap heap-id non contiene elementi.


; heap-not-empty

Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.


; actual-heap

???


; heap-head

La funzione heap-head ritorna una lista di due elementi dove K è la chiave minima e V il valore associato.


; get-key

Questa funzione ritorna la chiave di un nodo dato l'heap a cui appartiene e il suo valore.


; get-pos

Questa funzione ritorna la posizione di un nodo dato l'heap a cui appartiene e il suo valore.


; get-parent

Questa funione ritorna il valore del parent data la posizione di un figlio.


; get-left-child

Questa funzione ritorna la coppia (K V) del figlio sinistro data la posizione del padre.


; get-right-child

Questa funzione ritorna la coppia (K V) del figlio destro data la posizione del padre.


; get-left-child-value

Questa funzione ritorna il valore del figlio sinistro data la posizione del padre.


; get-right-chlid-value

Questa funzione ritorna il valore del figlio derstro data la posizione del padre.


; get-pos-parent

Questa funzione ritorna la posizione del padre di un nodo.


; get-pos-left-child

Questa funzione ritorna la posizione del figlio sinistro data quella del padre.

; get-pos-right-child

Questa funzione ritorna la posizione del figlio destro data quella del padre.


; heap-size

Questa funzione ritorna la dimensione corrente dello heap.

; incr-size

Questa funzione incrementa la dimensione dell'heap di uno.


; decr-size

Questa funzione decrementa la dimensione dell'heap di uno.


; mst-prim 

Questo predicato genera un mst prim. 


; set-inf

Questo è un predicato usato nel predicato mst_prim che imposta tutte le chiavi dei vertici di un grafo ad infinito.


; heap-insert

La funzione heap-insert inserisce l’elemento V nello heap heap-id con chiave K.


; heapify

Questo predicato ha il compito di assicurare il
rispetto della proprietà fondamentale degli Heap. Cioè, che il
valore di ogni nodo non sia inferiore di quello dei propri figli


; heap-modify-key

La funzone heap-modify-key sostituisce la chiave OldKey (associata al valore V) con NewKey.


; swap

Questa funzione scambia la posizione del nodo padre con quella del nodo figlio.


; fix-parent-children

Questa funzione sistema nella heap-entries il campo parent dei figli.


; heap-extract

La funzione heap-extract ritorna la lista con K, V e con K minima; la coppia è rimossa dallo heap heap-id.


; fix-heap

Questa è un heapifiy partendo dalla root.


; heap-print

Questa funzione stampa sulla console lo stato interno dello heap heap-id.


; recur-heap-print

Funzione ausiliaria di heap-print
