#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))

; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.

(define (mul M V acc)
  (if (null? M)
      (reverse acc)
      (mul (cdr M) V (cons (dot-product (car M) V) acc))))
  
(define (multiply M V)
  (mul M V null))


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

;; Gaseste randul pe care se afla n si ult element de pe acel rand.
;; @return: perechea (row-index . last-element)
(define (get-lvl-helper n row-index last-elem)
  (if (<= n last-elem)
      (cons row-index last-elem)
      (get-lvl-helper n (add1 row-index) (+ (expt 3 (add1 row-index)) last-elem))))

;; functie WRAPPER
(define (get-lvl n)
  (get-lvl-helper n 0 1))



;; Calculeaza primul element de pe randul unde se afla n.
;; @return: valoarea primului element
(define (get-first-elem-helper lvl-pair)
  (add1 (- (cdr lvl-pair) (expt 3 (car lvl-pair)))))  ; last-elem - 3 ^ row-index + 1

;; functie WRAPPER
;; Ne folosim de perechea (row-index . last-elem), rezultate din apelul functiei (get-lvl n).
(define (get-first-elem n)
  (get-first-elem-helper (get-lvl n)))




;; Determina marginea din stanga a intervalului.
;; @interval-no: numarul intervalului - {1, 2, 3}
;; @first: elementul de inceput al randului
;; @step: cate elemente sunt intr-un interval
;; @return: marginea din stanga a intervalului
(define (interval-left-margin interval-no first step n)
  (+ first (* (sub1 interval-no) step)))  ; first + (interval-no - 1) * step

;; Determina marginea din dreapta a intervalului.
;; @interval-no: numarul intervalului - {1, 2, 3}
;; @first: elementul de inceput al randului
;; @step: cate elemente sunt intr-un interval
;; @return: marginea din dreapta a intervalului
(define (interval-right-margin interval-no first step n)
  (sub1 (+ (interval-left-margin interval-no first step n) step)))

;; Determina daca n apartine intervalului.
;; @interval-no: numarul intervalului - {1, 2, 3}
;; @first: elementul de inceput al randului
;; @step: cate elemente sunt intr-un interval
;; @return:  daca n apartine intervalului - {#t, #f}
(define (contains-in-interval interval-no first step n)
  (and (>= n (interval-left-margin interval-no first step n))
       (<= n (interval-right-margin interval-no first step n))))



;; Afla care sunt transformarile aplicate, in ordine inversata.
;; @row: randul pe care ne aflam
;; @first: primul element de pe rand
;; @acc: acumulator, vector de transformari aplicate

;; (/ (expt 3 row) 3) = 3 ^ row / 3 -> step-ul randului, cat inseamna o treime din rand
(define (get-transformations-helper row first n acc)
  (cond
    ((zero? row) acc) ; se opreste cand ajungem la tripletul (3,4,5), pe randul 0
    ((contains-in-interval 1 first (/ (expt 3 row) 3) n) ; daca n este in prima treime
     (get-transformations-helper (sub1 row) (interval-left-margin 1 first (/ (expt 3 row) 3) n) n (cons 1 acc))) ; se adauga 1 la rezultat si se apeleaza recursiv
    ((contains-in-interval 2 first (/ (expt 3 row) 3) n) ; daca n este in a doua treime
     (get-transformations-helper (sub1 row) (interval-left-margin 2 first (/ (expt 3 row) 3) n) n (cons 2 acc))) ; se adauga 2 la rezultat si se apeleaza recursiv
    ((contains-in-interval 3 first (/ (expt 3 row) 3) n) ; daca n este in a treia treime
     (get-transformations-helper (sub1 row) (interval-left-margin 3 first (/ (expt 3 row) 3) n) n (cons 3 acc))))) ; se adauga 3 la rezultat si se apeleaza recursiv
  
;; functie WRAPPER
(define (get-transformations n)
  (reverse (get-transformations-helper (car (get-lvl n)) (get-first-elem n) n null)))

; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations-helper Ts ppt acc)
  (cond
    ((null? Ts) acc)
    ((equal? (car Ts) 1) (apply-matrix-transformations-helper (cdr Ts) ppt (multiply T1 acc)))
    ((equal? (car Ts) 2) (apply-matrix-transformations-helper (cdr Ts) ppt (multiply T2 acc)))
    ((equal? (car Ts) 3) (apply-matrix-transformations-helper (cdr Ts) ppt (multiply T3 acc)))))

(define (apply-matrix-transformations Ts ppt)
  (apply-matrix-transformations-helper Ts ppt ppt))


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))
