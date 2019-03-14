#lang racket
(provide (all-defined-out)
         (all-from-out rsound))

(define SYSTEM '?)

(require racket/gui rsound)

; sélection de l'api suivant le système
(case (system-type)
  ['unix (begin (set! SYSTEM 'unix)(host-api 'paALSA))] ; paOSS sur les pc du petit Valrose
  ['windows (begin (set! SYSTEM 'windows )(host-api 'paWASAPI))])

; permet de savoir si l'utilisateur a cliqué sur une case
(define (trouve-case x y L)
  (define (dans-case? x y c)
    (<= (+ (sqr (- x (send c getX))) (sqr (- y (send c getY))))
        (sqr (send c getR))))
  (cond
    [(empty? L) #f]
    [(dans-case? x y (car L)) (car L)]
    [else (trouve-case x y (cdr L))]))

; macro de la boucle while
(define-syntax while
  (syntax-rules ()
    ((while expr e1 e2 ...) (let ()
                              (define (iter)
                                (when expr e1 e2 ... (iter)))
                              (iter)))))

