; Coded by :                      
;  ______             _               _    _         _  _           __ 
; |  ____|           | |             | |  (_)       | || |         /_ |
; | |__  __ _  _ __  | |_  __ _  ___ | |_  _   ___  | || |_  ______ | |
; |  __|/ _` || '_ \ | __|/ _` |/ __|| __|| | / __| |__   _||______|| |
; | |  | (_| || | | || |_| (_| |\__ \| |_ | || (__     | |          | |
; |_|   \__,_||_| |_| \__|\__,_||___/ \__||_| \___|    |_|          |_|
;                                                                                                                                                                                                                                                 

#lang racket/gui

(require "src/utils.rkt" "src/switchable-case.rkt" "src/make-play-partition.rkt" "src/instrument.rkt" "src/frames.rkt")

(require racket/system)

; importations des sons
(define snd-hihat (rs-read "samples/hihat.wav"))
(define snd-kick (rs-read "samples/kick.wav")) 
(define snd-openhat (rs-read "samples/openhat.wav")) 
(define snd-snare (rs-read "samples/snare.wav")) 
(define snd-tom (rs-read "samples/tom.wav"))
(define snd-ride (rs-read "samples/ride.wav"))

; création des instruments
(define kick (new instrument% (name "kick") (sound snd-kick)))
(define hihat (new instrument% (name "hihat") (sound snd-hihat)))
(define snare (new instrument% (name "snare") (sound snd-snare)))
(define openhat (new instrument% (name "openhat") (sound snd-openhat)))
(define tom (new instrument% (name "tom") (sound snd-tom)))
(define ride (new instrument% (name "ride") (sound snd-ride)))

(define SIZE 975) ; taille de CASE-CANVAS
(define SIZEIMG 100) ; taille des vignettes d'instrument
(define ROWS 6)
(define COLUMNS 32)
(define RADIUS 10)
(define i 0) ; numéro de la colonne jouée

(define play #f)

; couleurs des cases
(define NOTCLICKED-COLOR "black") ; par défaut
(define CLICKED-COLOR "white")
(define ANIM-CLICKED-COLOR "green") ; lors de l'animation
(define ANIM-NOTCLICKED-COLOR "red")

(define firstLoop #t) ; si on n'as pas encore lancé le programme
;
(define VEC (build-vector COLUMNS (lambda (i) null)))
(for ([case LCASES])
  (vector-set! VEC
               (send case get-rank)
               (cons case (vector-ref VEC (send case get-rank)))))

(define part (make-partition LCASES))


; thread permettant de pauser la pstream
(define t1 (thread (lambda ()
                     (while play
                            (when (or (>= (pstream-current-frame ps) (+ start (* 8 44100))) firstLoop) ; temps d'une partition = 8 secondes
                              (play-on-loop part)
                              (when firstLoop (set! firstLoop #f)))
                            (anim-color i VEC ANIM-CLICKED-COLOR  ANIM-NOTCLICKED-COLOR) ; on anime tout la colonne i
                            (sleep INTERVAL) ; INTERVAL = TEMPO de la portée
                            (anim-color i VEC CLICKED-COLOR NOTCLICKED-COLOR) ; remise des couleurs par défaut après la lecture
                            (set! i (modulo (add1 i) COLUMNS))))))

(thread-suspend t1) ; au début on ne joue  pas


; changes les couleurs des cases cochées de la colonne i
(define (anim-color i vector clickedColor notClickedColor)
  (for ([case (vector-ref VEC i)])
    (cond [(send case get-state)
           (begin (send case set-color clickedColor)
                  (send case draw-with (send CASES-CANVAS get-dc)))] ; on redessine une case à la fois pour les performances
          [else (begin
                  (send case set-color notClickedColor)
                  (send case draw-with (send CASES-CANVAS get-dc)))])))

; méthode permettant de remettre démarrer ou redémarrer l'édition
(define (init)
  ; on remet LCASES NBCASES et start dans leur version initiale depuis ce module
  (reset-LCASES)   
  (reset-NBCASES)   
  (reset-start)
  (set! firstLoop #t)
  (set! i 0)
  (set! part (make-partition LCASES))
  (pstream-set-volume! ps 1) 
  (for ([i (in-range ROWS)]) ; création de la partition vierge
    (for ([j (in-range COLUMNS)])
      (new switchable-case%
           (x (+ 15 (* 30 j)))
           (y (+ 70 (* SIZEIMG i)))
           (r RADIUS)
           (rank j)
           (state #f)
           (parent (case i ; le parent dépend de la ligne
                     ((0) ride)
                     ((1) hihat)
                     ((2) openhat)
                     ((3) snare)
                     ((4) tom)
                     ((5) kick))))))
  (set! VEC (build-vector COLUMNS (lambda (i) null))) ; initialisation de la colonne à jouer courante
  (for ([case LCASES])
    (vector-set! VEC
                 (send case get-rank)
                 (cons case (vector-ref VEC (send case get-rank))))))

;------------GUI---------------
;Car nous ne pouvons pas tout mettre dans le frames.rkt, problème de cycle de require

(define CASE-HPANEL (new horizontal-panel% (parent FRAME)))

(define BUTTONPANEL (new horizontal-panel% (parent FRAME) (alignment '(center center))))

(define INSTRU-VPANEL (new vertical-panel%  (parent CASE-HPANEL)))

; une sous-classe de canvas% qui gere la souris
(define MOUSE-CANVAS%
  (class canvas%               
    (define case-or-#f #f)              ; la case courante choisi par la souris. Inspiré du cours/TP 6.
    (define delta-x 0)
    (define delta-y 0)
    (define/override (on-event evt)    ; pour gerer la souris (pour le clavier, redefinir on-char)
      (case (send evt get-event-type) 
        ((left-down) (define x (send evt get-x)) 
                     (define y (send evt get-y))
                     (set! case-or-#f (trouve-case x y LCASES))
                     (when case-or-#f ; si l'utilisateur à cliqué sur une case
                       (pstream-play ps (send (send case-or-#f get-parent) get-sound)) ; on la joue
                       (send case-or-#f Switch-Color)
                       (send this refresh-now)))
        ((left-up) (set! case-or-#f #f)))) ; relachement de la case courante
    (super-new)))

(define CASES-CANVAS 
  (new MOUSE-CANVAS% 
       (parent CASE-HPANEL) 
       (style '(border))
       (min-width SIZE)
       (paint-callback     ; action : redessine toutes les cases
        (lambda (c dc)
          (for ([case (in-list LCASES)])
            (send case draw-with dc))))))

(define INSTRU-CANVAS 
  (new canvas% 
       (parent INSTRU-VPANEL) 
       (style '(border))
       (min-width SIZEIMG)
       (min-height (* 6 SIZEIMG))
       (paint-callback     ; action : dessiner toutes les images
        (lambda (c dc)
          (for ([i (in-range ROWS)])
            (send dc draw-bitmap (read-bitmap (format "img/~a.png" i)) 0 (* SIZEIMG i)))))))

; lance la lecture
(define START-BUTTON
  (new button%
       (parent BUTTONPANEL)
       (label "Play")
       (callback (λ (b e)
                   (reset-start)
                   (pstream-set-volume! ps 1)
                   (set! firstLoop #t) ; 
                   (set! part (make-partition LCASES))
                   (set! play #t)
                   (thread-resume t1)
                   ; on bloque les autre boutons pendant la lecture
                   (send START-BUTTON enable #f)
                   (send REFRESH-BUTTON enable #f)
                   (send RANDOM-BUTTON enable #f)))))

; stop la lecture courante
(define STOP-BUTTON
  (new button%
       (parent BUTTONPANEL)
       (label "Stop")
       (callback (λ (b e)
                   (when play ; si on arrête en cours de lecture, on attend jusqu'à la fin avant de pouvoir relancer la lecture afin d'éviter de faire se superposer les sons
                     (send STOP-BUTTON enable #f)
                     (sleep
                      (frames->seconds
                       (+ start ; le temps restant dans la boucle
                          (seconds->frames 8)
                          (- (pstream-current-frame ps)))))
                     (send STOP-BUTTON enable #t))
                   (thread-suspend t1)
                   (set! play #f)
                   (send START-BUTTON enable #t)
                   (send REFRESH-BUTTON enable #t)
                   (send RANDOM-BUTTON enable #t)
                   (anim-color i VEC CLICKED-COLOR NOTCLICKED-COLOR)
                   (set! i 0)
                   (send CASES-CANVAS refresh-now)))))

; crée une partition aléatoire
(define RANDOM-BUTTON
  (new button%
       (parent BUTTONPANEL)
       (label "Random")
       (callback (λ (b e)
                   (thread-suspend t1) ; suspend la lecture (en cours ou non)
                   (init) ; remise du programme dans l'état initial
                   (for ([case LCASES])
                     (when (equal? (random 4) 0) ; chaque note n'a qu'une chance sur 4 d'être jouée
                       (send case Switch-Color)))
                   (send CASES-CANVAS refresh-now)))))

; remet le programme dans l'état initial
(define REFRESH-BUTTON
  (new button%
       (parent BUTTONPANEL)
       (label "Initialize")
       (callback (λ (b e)
                   (init)
                   (send CASES-CANVAS refresh-now)))))

; démarre la procédure d'exportation
(define RECORD-BUTTON
  (new button%
       (parent BUTTONPANEL)
       (label "Record")
       (callback (λ (b e)
                   (send CHECK-FRAME show #t)))))

; exporte la partition dans un fichier au nom spécifié
(define RECORD-TRUE
  (new button%
       (parent RECORD-HPANEL2)
       (label "record")
       (callback (lambda (b ctrl-evt)
                   (begin
                     (send RECORD-FRAME show #f)
                     (define name (send TEXT-FIELD get-value))
                     (if (equal? name "") ;un fichier sans nom est un fichier qui n'est pas exportait
                         (begin (send RECORD-FRAME create-status-line) (send RECORD-FRAME set-status-text "Erreur : Merci de donner un nom à votre fichier") ; Ligne de statut expliquant l'erreur
                                (send RECORD-FRAME show #t) (send ERROR-FRAME show #t))  
                         (begin (export name part nb-loops)(check-recorded name)))))))) ;Il n'y a pas de message d'erreur pour une partition vide !! Nous pouvons enregistrer du silence

;Maintenant, on test si nous avons bien record, nous allons donc show des frames en fonction du résultat

(define (check-recorded name)
  (if (or (and (equal? SYSTEM 'windows)
               (equal? (system (format "if exist \"~a.wav\" (exit 0) else (exit 1)"
                                       name))
                       #t))
          (system (format "find ~a.wav" name))) ; on lance directement la commande pour unix
      
      (begin
        (send FRAME show #t)
        (send CHECK-RECORDED-OK show #t) 
        (when (send RECORD-FRAME has-status-line?) ;Si l'erreur du fichier sans nom est déclenché, il faut bien évidemment l'enlever si il y a réussite, sinon la ligne de status reste jusqu'au redémarrage
          (send RECORD-FRAME set-status-text ""))) ;Je n'ai pas trouver comment faire pour juste désactiver la status-line
      (send ERROR-FRAME show #t)))

; -------- Lancement du programme  --------

(init) ; initialisation des variables
(send FRAME show #t)
(send FRAME on-close) ; la methode on-close de FRAME à été redéfinie pour mute la pstream en sortie 
(send CASES-CANVAS refresh-now) ; affichage de la partition vierge

