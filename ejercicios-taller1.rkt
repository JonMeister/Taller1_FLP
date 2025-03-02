;; Taller 1: Definición recursiva de programas e inducción
;; Integrantes grupo #15:
;; Jonathan Aristizabal - 2322626
;; Andrey Quiceno -
;; Johan Ceballos
;; Fecha: 01-03-2025

#lang eopl

;; Punto 1)
;; invert :
;; Propósito:
;; L → L' : Procedimiento que intercambia los elementos de cada par 
;; dentro de una lista de pares.
;;
;; <lista-pares> := ()
;;               := (<par> <lista-pares>)
;; <par> := (<valor> <valor>)


(define (invert L)
  (if (null? L)
      '()
      (cons (list (cadar L) (caar L)) (invert (cdr L))))); Intercambia la posición car por la cadr de la lista

;; Pruebas
(display "Pruebas punto 1, función: invert")
(newline)
(display (invert '()))
(newline)
(display (invert '((a 1) (a 2) (1 b) (2 b))))
(newline)
(display (invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo"))))
(newline)
(display (invert '(("es" "racket") ("genial" "muy") (17 29) (81 o))))
(newline)
(display "---------------------------------------------\n")
(newline)

;; Punto 2)
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

;; Pruebas
(display "Pruebas punto 2, función: down") 
(newline)
(display (down '(a b c d))) ;; ((a) (b) (c) (d))
(newline)
(display (down '(1 (2 (3 4)) 5))) ;; ((1) ((2 (3 4))) (5))
(newline)
(display (down '())) ;; ()
(newline)
(display "---------------------------------------------\n")
(newline)

;; Punto 3). list-set :
;; Propósito:
;; N x S x L -> L’ : Procedimiento que reemplaza el elemento 
;; en la posición N de una lista L por un símbolo S.
;;
;; <lista> := ()  
;;         := (<valor-de-scheme> <lista>)
(define list-set
  (lambda (lst n x)
    (cond
      [(null? lst) '()]  
      [(zero? n) (cons x (cdr lst))] 
      [else (cons (car lst) (list-set (cdr lst) (- n 1) x))])))

;; Pruebas
(display "Pruebas punto 3, función: list-set")
(newline)
(display (list-set '(a b c d) 2 'x))  ;; Devuelve '(a b x d)
(newline)
(display (list-set '(p q r) 0 'z))    ;; Devuelve '(z q r)
(newline)
(display (list-set '(w x y z) 3 'k))  ;; Devuelve '(w x y k)
(newline)
(display "---------------------------------------------\n")
(newline)



;; Punto 4)
;; filter-in :
;; Propósito:
;; P × L → L' : Procedimiento que filtra los elementos de la lista L
;; que cumplen con el predicado P.
;;
;; <lista> := ()
;;         := (<elemento> <lista>)
;; <predicado> := función booleana que evalúa cada elemento

(define (filter-in P L)
  (cond
    [(null? L) '()]  ; Si la lista está vacía, retorna una lista vacía
    [(and (not (pair? (car L))) (P (car L)))  ; Si el primer elemento satisface P y no es una lista anidada
     (cons (car L) (filter-in P (cdr L)))]
    [else (filter-in P (cdr L))]))  ; Si no cumple la condición, sigue con el resto de la lista

;; Pruebas
(display "Pruebas punto 4, función: filter-in")
(newline)
(display (filter-in number? '(a 2 (1 3) b 7)))
(newline)
(display (filter-in symbol? '(a (b c) 17 foo)))
(newline)
(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3))))
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 5)
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

;; Pruebas
(display "Pruebas punto 5, función: list-index") 
(newline)
(display (list-index even? '(1 3 4 5))) ;; 2
(newline)
(display (list-index string? '(1 2 "hola" 4 "mundo"))) ;; 2
(newline)
(display "---------------------------------------------\n")
(newline)

;; Punto 6). swapper:
;; Propósito:
;; S x S x L -> L’ : Procedimiento que intercambia todas las 
;; ocurrencias de un símbolo S1 por S2 y viceversa en una 
;; lista de símbolos L.
;;
;; <lista> := ()
;;        := (<valor-de-scheme> <lista>)
(define swapper
  (lambda (e1 e2 lst)
    (cond
      [(null? lst) '()] 
      [(equal? e1 (car lst)) (cons e2 (swapper e1 e2 (cdr lst)))]
      [(equal? e2 (car lst)) (cons e1 (swapper e1 e2 (cdr lst)))] 
      [else (cons (car lst) (swapper e1 e2 (cdr lst)))])))

;; Pruebas
(display "Pruebas punto 6, función: swapper")
(newline)
(display (swapper 'a 'b '(a b c a d b)))  ;; Debería devolver '(b a c b d a)
(newline)
(display (swapper 'x 'y '(x y x y z)))    ;; Debería devolver '(y x y x z)
(newline)
(display (swapper '1 '2 '(1 2 3 1 2 1)))  ;; Debería devolver '(2 1 3 2 1 2)
(newline)
(display "---------------------------------------------\n")
(newline)



;; Punto 7)
;; cartesian-product :
;; Propósito:
;; L1 × L2 → L' : Procedimiento que genera el producto cartesiano
;; entre dos listas de símbolos sin repeticiones.
;;
;; <lista> := ()
;;         := (<símbolo> <lista>)
;; <producto cartesiano> := lista de pares (tuplas)

(define (cartesian-product L1 L2)
  (if (null? L1)
      '()
      (append (map (lambda (y) (list (car L1) y)) L2) ; Crea pares (car L1, y) para cada y en L2
              (cartesian-product (cdr L1) L2)))) ; Llama recursivamente con el resto de L1 menos la cabeza y L2

;; Pruebas
(display "Pruebas punto 7, función: cartesian-product")
(newline)
(display (cartesian-product '(a b c) '(x y)))
(newline)
(display (cartesian-product '(p q r) '(5 6 7)))
(newline)
(display "---------------------------------------------\n")
(newline)

;; Punto 8)
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

;; Pruebas
(display "Pruebas punto 8, función: mapping") 
(newline)
(display (mapping (lambda (d) (if (even? d) d 0)) '(1 2 3 4 5 6) '(0 2 0 4 0 6))) ;; ((1 0) (2 2) (3 0) (4 4) (5 0) (6 6))
(newline)
(display (mapping (lambda (d) 42) '(1 2 3 4 5) '(42 42 42 42 42))) ;; ((1 42) (2 42) (3 42) (4 42) (5 42))
(newline)
(display (mapping (lambda (d) (* d 2)) '() '())) ;; ()
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 9). inversions :
;; Propósito:
;; L -> N : Procedimiento que cuenta el número de inversiones en una lista de números L.
;; Una inversión ocurre cuando un elemento aparece antes que otro menor que él.
;;
;; <lista> := ()  
;;         := (<número> <lista>)
(define inversions
  (lambda (L)
    (define coincidencias
      (lambda (item lst)
        (if (null? lst)
            0
            (+ (if (and (pair? lst) (> item (car lst))) 1 0)
               (coincidencias item (cdr lst))))))
    (if (null? L)
        0
        (+ (coincidencias (car L) (cdr L)) (inversions (cdr L))))))

;; Pruebas
(display "Pruebas punto 9, función: inversions") ;; Reemplaza "X" con el número correcto del punto
(newline)
(display (inversions '(5 3 2 1)))  ;; Devuelve 6 → (5-3, 5-2, 5-1, 3-2, 3-1, 2-1)
(newline)
(display (inversions '(1 2 3 4)))  ;; Devuelve 0 → (ya está ordenada)
(newline)
(display (inversions '(4 3 2 1)))  ;; Devuelve 6 → (todas las inversiones posibles)
(newline)
(display (inversions '(2 1 3)))    ;; Devuelve 1 → (solo 2-1)
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 10)
;; up :
;; Propósito:
;; L → L' : Procedimiento que elimina un nivel de paréntesis de cada
;; elemento en el nivel más alto de la lista.
;;
;; <lista> := ()
;;         := (<elemento> <lista>)
;; <elemento> := átomo | lista

(define (up L)
  (if (null? L)
      '()
      (append (if (list? (car L)) (car L) (list (car L))) ; Si el primer elemento es una lista, lo deja así. Si no, lo convierte en una lista
              (up (cdr L))))) ; Llamada recursiva con el resto de la lista


;; Pruebas
(display "Pruebas punto 10, función: up")
(newline)
(display (up '((1 2) (3 4))))
(newline)
(display (up '((x (y)) z)))
(newline)
(display (up '((a (b c)) ((d e) f) g)))
(newline)
(display "---------------------------------------------\n")
(newline)

;; Punto 11)
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

;; Pruebas
(display "Pruebas punto 11, función: zip") 
(newline)
(display (zip - '(3 8 2) '(1 2 2))) ;; (2 6 0)
(newline)
(display (zip * '(0 1 2 3) '(4 0 5 6))) ;; (0 0 10 18)
(newline)
(display (zip + '(1 2 3 4) '(10 20 30 40))) ;; (11 22 33 44)
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 12). filter-acum :
;; Propósito:
;; N x N x (N x N -> N) x N x (N -> Bool) -> N : Procedimiento que aplica una función 
;; de acumulación F sobre los valores de un rango [a, b] que cumplen con un filtro dado.
;;
;; <rango> := N N 
;; <función-acumulación> := (N x N -> N)
;; <filtro> := (N -> Bool)
(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)
        acum  ;; Caso base: cuando a es mayor que b, devuelve el acumulador.
        (filter-acum (+ a 1) b F 
                     (F (if (filter a) a 0) acum)  ;; Aplica F solo si filter es verdadero.
                     filter))))

;; Pruebas
(display "Pruebas punto 12, función: filter-acum")
(newline)
(display (filter-acum 1 5 + 0 even?))  ;; Devuelve 6 → Suma de los números pares: 2 + 4
(newline)
(display (filter-acum 1 5 * 1 odd?))   ;; Devuelve 15 → Producto de los impares: 1 * 3 * 5
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 13)
;; operate:
;; Propósito:
;; Irators × Irands → Número : Procedimiento que aplica sucesivamente 
;; cada función binaria de irators a los valores en irands.
;;
;; <lista-operadores> := ()
;;                    := (<operador-binario> <lista-operadores>)
;; <lista-operandos> := (<número> <lista-operandos>)

(define (operate lrators lrands)
  (if (null? lrators)
      (car lrands) ; Si no hay más operadores, devuelve el último operando
      (operate (cdr lrators) ; Usa el primer operando de lrators para multiplicar el priimer y segundo término de lrands
               (cons ((car lrators) (car lrands) (cadr lrands)) ; Devuelve una lista con el resultado de la operación y la lista sin los dos primeros elementos
                     (cddr lrands)))))

; Pruebas
(display "Pruebas punto 13, función: operate")
(newline)
(display (operate (list + * + - *) '(1 2 8 4 11 6)))
(newline)
(display (operate (list *) '(4 5))) 
(newline)
(display (operate (list + -) '(10 5 2)))
(newline)
(display (operate (list -) '(10 5 2)))
(newline)
(display "---------------------------------------------\n")
(newline)

;; Punto 14)
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

;; Pruebas
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
(display "---------------------------------------------\n")
(newline)


;; Punto 15). count-odd-and-even :
;; Propósito:
;; A -> (N N) : Procedimiento que cuenta la cantidad de números pares e impares  
;; en un árbol binario A.
;;
;; <árbol> := ()  
;;         := (<número> <árbol> <árbol>)  

(define (count-odd-and-even L)
  (cond
    [(null? L) '(0 0)]  
    [(number? (car L))  
     (list (+ (if (even? (car L)) 1 0) (car (count-odd-and-even (cdr L))))
           (+ (if (odd? (car L)) 1 0)  (cadr (count-odd-and-even (cdr L)))))]

    [else  
     (list (+ (car (count-odd-and-even (car L))) (car (count-odd-and-even (cdr L))))
           (+ (cadr (count-odd-and-even (car L))) (cadr (count-odd-and-even (cdr L)))))]))

;; Pruebas
(display "Pruebas punto 15, función: count-odd-and-even")
(newline)
(display (count-odd-and-even '(5 (3 (2 () ()) (1 () ())) (8 (6 () ()) (9 () ())))))  
;; Devuelve '(3 3) → 3 pares (2, 6, 8) y 3 impares (1, 3, 5, 9)
(newline)
(display (count-odd-and-even '(4 (2 () ()) (6 () ()))))  
;; Devuelve '(3 0) → 3 pares (4, 2, 6) y 0 impares
(newline)
(display (count-odd-and-even '(7 (3 (1 () ()) (5 () ())) (9 (11 () ()) (13 () ())))))  
;; Devuelve '(0 6) → 0 pares y 6 impares (1, 3, 5, 7, 9, 11, 13)
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 16)
;; Operar-binarias:
;; Propósito:
;; <OperacionB> → <int>
;; Evalúa una operación binaria representada en la estructura definida por la gramática.
;;
;; <OperacionB> := <int>
;;              := (<OperacionB> 'suma <OperacionB>)
;;              := (<OperacionB> 'resta <OperacionB>)
;;              := (<OperacionB> 'multiplica <OperacionB>)
(define (Operar-binarias operacionB)
  (cond
    [(number? operacionB) operacionB] ; Si es un número, se devuelve tal cual
    [(and (list? operacionB) (= (length operacionB) 3)) ; Verifica que sea una lista de longitud 3
     (let ([left (car operacionB)] ; Asigna los valores de la lista a las variables temporales left, op y right
           [op (cadr operacionB)]
           [right (caddr operacionB)])
       (cond                            
         [(eq? op 'suma) (+ (Operar-binarias left) (Operar-binarias right))]  ; Opera con las 3 operaciones establecidas de manera recursiva
         [(eq? op 'resta) (- (Operar-binarias left) (Operar-binarias right))]
         [(eq? op 'multiplica) (* (Operar-binarias left) (Operar-binarias right))]
         [else '(error "Operador inválido")]))]
    [else '(error "Expresión inválida")])) ; Manejo de error para expresiones no válidas

;; Pruebas
(display "Pruebas punto 16, función: Operar-binarias")
(newline)
(display (Operar-binarias 4)) ; 4
(newline)
(display (Operar-binarias '(2 suma 9))) ; 2 + 9 = 11
(newline)
(display (Operar-binarias '(2 resta 9))) ; 2 - 9 = 7
(newline)
(display (Operar-binarias '(2 multiplica 9))) ; 2 x 9 = 18
(newline)
(display (Operar-binarias '((2 multiplica 3) suma (5 resta 1)))) ; 2 x 3 + 5 - 1 = 10
(newline)
(display (Operar-binarias '((2 multiplica (4 suma 1)) multiplica ((2 multiplica 4) resta 1)))) ; 2 x (4+1) x (2 x 4 -1) = 70
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 17)
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
(display "Pruebas punto 17, función: prod-scalar-matriz") 
(newline)
(display (prod-scalar-matriz '((3 4) (5 6)) '(2 2))) ;; ((6 8) (10 12))
(newline)
(display (prod-scalar-matriz '() '(2 3))) ;; ()
(newline)
(display (prod-scalar-matriz '((2 2 2 2)) '(1 2 3 4)))  ;; ((2 4 6 8))
(newline)
(display "---------------------------------------------\n")
(newline)


;; Punto 18). pascal :
;; Propósito:
;; N -> L : Procedimiento que genera la fila N del triángulo de Pascal.
;;
;; <fila> := ()  
;;        := (<número> <fila>)  
(define (pascal N)
  (define (sumar-adyacentes lst last)
    (if (null? (cdr lst))
        (list last)  ;; Agrega el último 1 al final de la fila.
        (cons (+ (car lst) (cadr lst)) (sumar-adyacentes (cdr lst) last))))  
  (if (= N 1)
      '(1)  ;; Caso base: la primera fila es '(1)
      (cons 1 (sumar-adyacentes (pascal (- N 1)) 1))))

;; Pruebas
(display "Pruebas punto 18, función: pascal")
(newline)
(display (pascal 1))  ;; Devuelve '(1)
(newline)
(display (pascal 2))  ;; Devuelve '(1 1)
(newline)
(display (pascal 3))  ;; Devuelve '(1 2 1)
(newline)
(display (pascal 4))  ;; Devuelve '(1 3 3 1)
(newline)
(display (pascal 5))  ;; Devuelve '(1 4 6 4 1)
(newline)
(display "---------------------------------------------\n")
(newline)