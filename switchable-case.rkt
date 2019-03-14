#lang racket/gui
(provide (all-defined-out))

(define LCASES '())    ; la liste de tous les cases (variable de classe)
(define NBCASES 0)     ; pour ne pas recalculer la longueur chaque fois...

; méthode permettant de mofier LCASES et NBCASES depuis un module externe
(define (reset-LCASES) 
  (set! LCASES '()))

(define (reset-NBCASES)
  (set! NBCASES 0))


; la classe des case cliquables
(define switchable-case%
  (class object%
    (init-field (parent 'an-instrument%) (x 0) (y 0) (r 0)
                (rank 0) (state #f)  (color "black"))
    (define/public (getX)x)
    (define/public (getY) y)
    (define/public (getR) r)
    (define/public (getColor) color)
    (define/public (get-rank) rank)
    (define/public (get-state) state)  ; permet de savoir si la case est allumée ou éteinte 
    (define/public (get-parent)  parent)  ; le parent est un instrument 
    (define/public (set-color c) (set! color c))
    (define/public (Switch-Color)
      (set! state (not state))
      (if state
          (send this set-color "white")
          (send this set-color "black")))
    (define/public (setX! nx) (set! x nx))
    (define/public (setY! ny) (set! y ny))
    (define/public (draw-with dc)
      (send dc set-brush (send the-brush-list find-or-create-brush color 'solid)) ; crée le pinceau de la bonne couleur
      (send dc draw-rectangle (- x r) (- y r) (* 2 r) (* 2 r)))
    (set! LCASES (cons this LCASES))
    (set! NBCASES (add1 NBCASES))
    (super-new))) ; appel au constructeur de la super-classe (objet%)