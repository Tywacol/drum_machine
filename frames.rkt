#lang racket/gui
(require "make-play-partition.rkt")
(provide (all-defined-out))

;Frame principale, elle est mise ici pour pemettre d'enpêcher son accès pendant que l'on fait un record
(define FRAME 
  (new (class frame% ;Il nous faut donc redefinir la classe frame%
       (super-new)
          (define/augment (on-close) ;Redefinition de la croix rouge : Met "fin" a la playstream en cas de fermeture du logiciel
         (mute-ps)))
       (label "The Fantastic 4-1's fantastic drum machine") 
       (stretchable-width #f)
       (stretchable-height #f)))

;---------------------------------------------------------------
;Frame de confirmation, elle ouvre le Recording tool tout en fermant la frame principale quand la réponse est positive de la part de l'utilisateur
(define HEIGHT-SIZE-CHECK-FRAME 100)
(define WIDTH-SIZE-CHECK-FRAME 375)
(define RECORD-FRAME-WIDTH 400)
(define RECORD-FRAME-HEIGHT 200)

(define CHECK-FRAME
  (new frame%
       (label "Record")
       (min-width WIDTH-SIZE-CHECK-FRAME)
       (min-height HEIGHT-SIZE-CHECK-FRAME)
       (stretchable-width #f)
       (stretchable-height #f)))

(define VPANNEL-CHECK-FRAME
  (new vertical-panel%
       (parent CHECK-FRAME)
       (alignment '(center center))))

(define CHECK-MESSAGE
  (new message%
       (parent VPANNEL-CHECK-FRAME) (label "Avez-vous bien terminé la saisie de votre partition ?")
       (stretchable-width #f)
       (stretchable-height #f)))

(define HPANNEL-CHECK
  (new horizontal-panel%
       (parent VPANNEL-CHECK-FRAME)
       (alignment '(center center))))

(define POSITIVE-BUTTON
  (new button%
       (parent HPANNEL-CHECK) (label "oui")
       (callback (lambda (b ctrl-evt)
                   (begin                              ; "Fermeture" de la frame principale ainsi que celle de confirmation pour faire place au tool de record
                     (send FRAME show #f)
                     (send CHECK-FRAME show #f) 
                     (send RECORD-FRAME show #t))))))

(define NEGATIVE-BUTTON
  (new button%
       (parent HPANNEL-CHECK) (label "non")
       (callback (lambda (b ctrl-evt)
                   (send CHECK-FRAME show #f)))))      ; Saisie de la partition non terminée, on ferme uniquement la frame de confirmation

;---------------------------------------------------------------
;Tool de record

;ATTENTION : Et si on ferme avec la croix rouge la RECORD-FRAME ?? il faut remettre la frame principale !
(define (kill) (send FRAME show #t))

(define RECORD-FRAME 
  (new (class frame% ;Il nous faut donc redefinir la classe frame%
       (super-new)
          (define/augment (on-close) ;Redefinition de la croix rouge : Remet la frame principale
         (kill)))
       (label "Recording tool")
       (min-width RECORD-FRAME-WIDTH)
       (min-height RECORD-FRAME-HEIGHT)
       (stretchable-width #f)
       (stretchable-height #f)))

(define RECORD-VPANNEL-SEPARATION
  (new vertical-panel%
       (parent RECORD-FRAME)
       (min-height 40)
       (alignment '(center center))
       (stretchable-width #f)
       (stretchable-height #f)))

(define RECORD-VPANEL
  (new vertical-panel%
       (parent RECORD-FRAME)
       (alignment '(center center))
       (stretchable-width #f)
       (stretchable-height #f)))

(define TEXT-FIELD
  (new text-field% 
       (parent RECORD-VPANEL)
       (min-width 300)
       (stretchable-width #f)
       (stretchable-height #f)
       (label "Donnez le nom du fichier : ") ;L'extension est .wav dans la fonction d'exportation, pas besoin de préciser l'extension
       (init-value "file")))    ;Valeur par défaut

(define nb-loops 1)
(define RECORD-CHOICE
  (new choice%	 
       (label "Nombre de boucles :")	 
       (choices (list "1" "2" "3" "4" "5")) ;n'ayant pas trouver de make-list avec des string de numéro croissants, il doit y avoir une solution tout de même
       (parent RECORD-VPANEL)
       (callback (lambda (b evnt) (set! nb-loops (string->number (send b get-string-selection))))) ;nous récupérons le nombre d'iteration voulu
       (stretchable-width #f)	 
       (stretchable-height #f)))

(define RECORD-VPANNEL-SEPARATION2
  (new vertical-panel%
       (parent RECORD-VPANEL)
       (min-height 25)
       (alignment '(center center))
       (stretchable-width #f)
       (stretchable-height #f)))

(define RECORD-HPANEL
  (new horizontal-panel%
       (parent RECORD-VPANEL)
       (alignment '(center center))))

;Nous mettons ici un triple HPANEL pour garder l'ordre des boutons record puis cancel, le bouton record étant dans le main, problème de cycle de require

(define RECORD-HPANEL2
  (new horizontal-panel%
       (parent RECORD-HPANEL)
       (alignment '(center center))))

(define RECORD-HPANEL3
  (new horizontal-panel%
       (parent RECORD-HPANEL)
       (alignment '(center center))))


(define RECORD-CANCEL
  (new button%
       (parent RECORD-HPANEL3)
       (label "back")
       (callback (lambda (b ctrl-evt) (send RECORD-FRAME show #f) (kill))))) ;cancel : nous repassons sur la frame principale

;---------------------------------------------------------------
;Frame permettant de valider le record, voir la fonction (check-recorded name) du fichier main

(define CHECK-RECORDED-OK
  (new frame%
       (label "Record Manager")
       (stretchable-width #f)
       (stretchable-height #f)))

(define CHECK-RECORDED-OK-VPANNEL
  (new vertical-panel%
       (parent CHECK-RECORDED-OK)
       (alignment '(center center))
       (vert-margin 25)	 
       (horiz-margin 40)
       (stretchable-width #f)
       (stretchable-height #f)))

(define CHECK-RECORDED-OK-HPANNEL
  (new horizontal-panel%
       (parent CHECK-RECORDED-OK-VPANNEL)
       (alignment '(center center))
       (vert-margin 25)	 
       (horiz-margin 25)
       (stretchable-width #f)
       (stretchable-height #f)))

(define CHECK-RECORDED-OK-ICON 
  (new canvas% 
       (parent CHECK-RECORDED-OK-HPANNEL) 
       (style '(transparent))
       (min-width 32) ;Icone en 32x32 pixels
       (min-height 32)
       (paint-callback     
        (lambda (c dc)
          (send dc draw-bitmap (read-bitmap "img/ok.png") 0 0))))) ;Permet de mettre une petit icone de validation


(define CHECK-RECORDED-OK-MESSAGE
  (new message%
       (parent CHECK-RECORDED-OK-HPANNEL) (label "Sound exported")
       (stretchable-width #f)
       (stretchable-height #f)))

(define CHECK-RECORDED-OK-BUTTON
  (new button%
       (parent CHECK-RECORDED-OK-VPANNEL) (label "ok")
       (callback (lambda (b ctrl-evt) (send CHECK-RECORDED-OK show #f))))) ;Exportation réussit ! retour vers la frame principale assurée par la fonction de vérification

;---------------------------------------------------------------
;Oops ! Le fichier d'exportation n'est pas à sa place après son exportation ??                 ;Le message d'erreur n'inclus pas la partition vide
;Pour tout erreur de ce type, n'hésitez pas à contacter les membres du groupe Fantastic 4 - 1

;A savoir :
;;Merci de vérifier les droits en écriture au niveau du fichier courrant
;;Merci de bien préciser un nom à votre fichier
;;Possible problème de commande console ? Possible maj de votre ordinateur avec des commandes shells différentes ?
;;Voir la fonction (check-recorded name)

(define ERROR-FRAME
  (new frame%
       (label "Error")
       (stretchable-width #f)
       (stretchable-height #f)))

(define ERROR-FRAME-VPANNEL
  (new vertical-panel%
       (parent ERROR-FRAME)
       (alignment '(center center))
       (stretchable-width #f)
       (stretchable-height #f)))

(define ERROR-FRAME-HPANNEL
  (new horizontal-panel%
       (parent ERROR-FRAME-VPANNEL)
       (alignment '(center center))
       (vert-margin 25)	 
       (horiz-margin 25)
       (stretchable-width #f)
       (stretchable-height #f)))


(define ERROR-ICON 
  (new canvas% 
       (parent ERROR-FRAME-HPANNEL) 
       (style '(transparent))
       (min-width 25) ;Icone de taille 25
       (min-height 25)
       (paint-callback
        (lambda (c dc)
          (send dc draw-bitmap (read-bitmap "img/error25.png") 0 0))))) ;Permet de mettre une petit icone d'erreur

(define ERROR-FRAME-MESSAGE
  (new message%
       (parent ERROR-FRAME-HPANNEL) (label "Oops ! Something wrong")
       (stretchable-width #f)
       (stretchable-height #f)))

(define ERROR-FRAME-BUTTON
  (new button%
       (parent ERROR-FRAME-VPANNEL) (label "ok")
       (callback (lambda (b ctrl-evt) (send ERROR-FRAME show #f)))))

;normalement la frame d'erreur ne devrait pas être utile