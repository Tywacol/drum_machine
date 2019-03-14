#lang racket
(require rsound)
(provide (all-defined-out))

(define INTERVAL 0.25)
(define start 0)
(define ps (make-pstream))
(define (mute-ps)
  (pstream-set-volume! ps 0))

(define (set-INTERVAL i)
  (set! INTERVAL i))

(define (reset-start)
  (set! start (pstream-current-frame ps)))

; extrait une partition d'une liste de case switchables
(define (make-partition L)
  (cond [(null? L) null]
        [(send (car L) get-state)
         (cons (list (send (send (car L) get-parent) get-sound)
                     (* (seconds->frames INTERVAL) (send (car L) get-rank)))
               (make-partition (cdr L)))]
        [else (make-partition (cdr L))]))

; joue une partition
(define (play-on-loop part)
  (set! start (pstream-current-frame ps))
  (for ([note part])
    (pstream-queue ps (car note) (+ start (cadr note)))))

(define (seconds->frames sec)
  (* 44100 sec))

(define (frames->seconds f)
  (/ f 44100))

(define (export filename part nbloops)
  ; crée le fichier "filename.wav" contenant nbloops répétitions de la boucle
  (define base ; base silencieuse sur laquelle on insère chaque son
    (silence (seconds->frames (* nbloops 8))))
  
  (when (not (null? part))
    (define loop ; boucle unique 
      (assemble part))
    (for ([i (in-range nbloops)]) ; on ajoute à la base la boucle autant de fois que nécessaire à la bonne place
      (if (= i 0)
          (set! base (rs-overlay base loop))
          (set! base (rs-overlay base
                                 (rs-append (silence (seconds->frames (* i 8)))
                                            loop))))))
  (rs-write base (format "~a.wav" filename)))
