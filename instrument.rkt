#lang racket
(provide (all-defined-out))

; classe modélisant un instrument par son nom et un sample
(define instrument% 
  (class object%
    (init-field (name "") (sound "file.wav"))
    
    (define/public (get-name)
      name)
    
    (define/public (get-sound)
      sound)
    
    (super-new)))
