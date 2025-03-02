#lang eopl
;Johan Andres Ceballos Tabarez 202372229



;; 2.
;; down:
;; Propósito:
;; L -> L’ : Procedimiento que transforma una lista de símbolos L
;; en otra lista donde cada elemento de L se encuentra dentro de 
;; una lista anidada, aumentando un nivel de paréntesis.
;;
;; <lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define (down L)
  (if (null? L)
      '()
      (cons (list (car L)) (down (cdr L)))))

;; 5.
;; list-index:
;; Propósito:
;; P x L -> N | #f : Procedimiento que busca el primer elemento
;; de una lista L que satisface el predicado P y devuelve su índice
;; (desde una posición inicial 0). Si ningún elemento cumple el
;; predicado, devuelve #f.
;;
;; <lista> := ()
;;        := (<valor-de-scheme> <lista>)
;;
;; <predicado> := procedimiento que toma un valor y devuelve #t o #f

(define list-index
  (lambda (pred lst)
    ;; Función auxiliar recursiva que lleva un contador para rastrear el índice.
    (letrec ((list-index-aux
              (lambda (lst index)
                (cond
                  ((null? lst) #f)                             
                  ((pred (car lst)) index)                     
                  (else (list-index-aux (cdr lst) (+ index 1))) 
                ))))
      (list-index-aux lst 0))))


;; 8.
;; mapping:
;; Propósito:
;; (F L1 L2) -> L’ : Procedimiento que recibe una función unaria F y dos listas de números L1 y L2 de igual tamaño.
;; Devuelve una lista de pares (a, b) donde a pertenece a L1, b pertenece a L2, y se cumple que F(a) = b.
;;
;; <lista> := ()
;;        := (<número> <lista>)

(define (mapping F L1 L2)
  ;; Función auxiliar recursiva que filtra los pares (a, b) tales que F(a) = b, recorriendo ambas listas en paralelo.
  (define (filtrar L1 L2)
    (cond
      ((null? L1) '())  
      ((= (F (car L1)) (car L2)) 
       (cons (list (car L1) (car L2)) (filtrar (cdr L1) (cdr L2))))
      (else (filtrar (cdr L1) (cdr L2))))) 
  (filtrar L1 L2))


;; 11.
;; zip:
;; Propósito:
;; (F, L1, L2) -> L’ : Procedimiento que toma una función binaria F 
;; y dos listas L1 y L2 del mismo tamaño, y retorna una nueva lista 
;; donde cada elemento en la posición n-ésima es el resultado de aplicar 
;; F a los elementos en la posición n-ésima de L1 y L2.
;;
;; <lista> := ()
;;        := (<valor-de-scheme> <lista>)


(define (zip F L1 L2)
  (if (null? L1)
      '()
      (cons (F (car L1) (car L2))
            (zip F (cdr L1) (cdr L2)))))


;; 14.
;; path:
;; Propósito:
;; (n x bst) -> L : Procedimiento que recibe un número entero n y un árbol
;; binario de búsqueda bst (representado con listas) y retorna una lista de
;; direcciones ('left' o 'right') indicando el camino desde la raíz hasta n.
;; Si n está en la raíz, retorna '().
;; Si n es menor que la raíz, se continúa por la sublista izquierda ('left').
;; Si n es mayor que la raíz, se continúa por la sublista derecha ('right').
;;
;; <bst> := ()
;;                 := (número <árbol-binario> <árbol-binario>)
;; <L> := ('left | 'right)*
;; <n> := número entero


(define (path n bst)
  ;; Función auxiliar recursiva que encuentra el camino hasta n en bst, retornando una lista de 'left' y 'right'.
  (define (helper current-bst)
    (cond
      [(null? current-bst) '()]  
      [(= n (car current-bst)) '()] 
      [(< n (car current-bst))
       (cons 'left (helper (cadr current-bst)))]
      [else  
       (cons 'right (helper (caddr current-bst)))]))
  
  (helper bst))


;; 17.
;; prod-scalar-matriz:
;; Propósito:
;; L x L -> L : Procedimiento que recibe una matriz representada como una lista de listas 
;; y un vector representado como una lista, y devuelve una nueva matriz donde cada fila 
;; se ha multiplicado elemento a elemento con el vector.
;;
;; <matriz> := () 
;;          := (<fila> <matriz>)
;; <fila> := () 
;;       := (<numero> <fila>)
;; <vector> := () 
;;         := (<numero> <vector>)

(define (prod-scalar-matriz mat vec)
  ;; Función auxiliar recursiva que multiplica una fila elemento a elemento con el vector dado.
  (define (multiply-row row vec)
    (if (null? row)
        '()
        (cons (* (car row) (car vec))
              (multiply-row (cdr row) (cdr vec)))))
  
  (if (null? mat)
      '()
      (cons (multiply-row (car mat) vec)
            (prod-scalar-matriz (cdr mat) vec))))


;; Pruebas
;; Pruebas punto 2, función: down
(display "Pruebas punto 2, función: down") 
(newline)
(display (down '(a b c d))) ;; ((a) (b) (c) (d))
(newline)
(display (down '(1 (2 (3 4)) 5))) ;; ((1) ((2 (3 4))) (5))
(newline)
(display (down '())) ;; ()
(newline)

;; Pruebas punto 5, función: list-index
(display "Pruebas punto 5, función: list-index") 
(newline)
(display (list-index even? '(1 3 4 5))) ;; 2
(newline)
(display (list-index even? '(a b 3 (2 4) 6))) ;; 4
(newline)
(display (list-index string? '(1 2 "hola" 4 "mundo"))) ;; 2
(newline)

;; Pruebas punto 8, función: mapping
(display "Pruebas punto 8, función: mapping") 
(newline)
(display (mapping (lambda (d) (if (even? d) d 0)) '(1 2 3 4 5 6) '(0 2 0 4 0 6))) ;; ((1 0) (2 2) (3 0) (4 4) (5 0) (6 6))
(newline)
(display (mapping (lambda (d) 42) '(1 2 3 4 5) '(42 42 42 42 42))) ;; ((1 42) (2 42) (3 42) (4 42) (5 42))
(newline)
(display (mapping (lambda (d) (* d 2)) '() '())) ;; ()
(newline)

;; Pruebas punto 11, función: zip
(display "Pruebas punto 11, función: zip") 
(newline)
(display (zip - '(3 8 2) '(1 2 2))) ;; (2 6 0)
(newline)
(display (zip * '(0 1 2 3) '(4 0 5 6))) ;; (0 0 10 18)
(newline)
(display (zip + '(1 2 3 4) '(10 20 30 40))) ;; (11 22 33 44)
(newline)

;; Pruebas punto 14, función: path
(display "Pruebas punto 14, función: path") 
(newline)
(display (path 12 '(14 (7 () (12 () ())) 
               (26 (20 (17 () ()) ()) 
                   (31 () ()))))) ;; (left right)
(newline)
(display (path 31 '(14 (7 () (12 () ()))  
               (26 (20 (17 () ()) ()) 
                   (31 () ()))))) ;; (right right)
(newline)
(display (path 14 '(14 (7 () (12 () ()))  
               (26 (20 (17 () ()) ()) 
                   (31 () ()))))) ;; ()
(newline)

;; Pruebas punto 17, función: prod-scalar-matriz
(display "Pruebas punto 17, función: prod-scalar-matriz") 
(newline)
(display (prod-scalar-matriz '((3 4) (5 6)) '(2 2))) ;; ((6 8) (10 12))
(newline)
(display (prod-scalar-matriz '() '(2 3))) ;; ()
(newline)
(display (prod-scalar-matriz '((2 2 2 2)) '(1 2 3 4)))  ;; ((2 4 6 8))
(newline)
