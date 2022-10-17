;-----------------------------------------------------------------------------------------;
; ---- Reprise du lisp propos� par Zebulon_ (Forum - CadXP - Lisp bande d'�tanch�it�) ----;
;-----------------------------------------------------------------------------------------;

(vl-load-com)

;; MAKEGROUP
;; Cr�e un groupe sans nom avec les entit�s contenues dans la liste
;;
;; Argument
;; lst : liste des entit�s (ename)
;;
;; Retour
;; le groupe cr�� (ename) ou nil

(defun makegroup (lst / dict ind)
 (setq 
   dict (dictsearch (namedobjdict) "ACAD_GROUP")
   ind "GRP1"
 )
 (while (member (cons 3 ind) dict)
   (setq ind (strcat "GRP" (itoa (1+ (atoi (substr ind 4))))))
 )
 (dictadd
   (cdr (assoc -1 dict))
   ind
   (entmakex
     (append
       (list
         '(0 . "GROUP") '(100 . "AcDbGroup") '(300 . "TALUS") '(70 . 1) '(71 . 1)
       )
       (mapcar (function (lambda (x) (cons 340 x))) lst)
     )
   )
 )
)

(defun cs:line (PO PF / AcDoc Space)
 (setq AcDoc (vla-get-activeDocument (vlax-get-acad-object))
       Space (if (= (getvar "CVPORT") 1)
               (vla-get-PaperSpace AcDoc)
               (vla-get-ModelSpace AcDoc) 
             )
 )
 (vla-addLine
   Space
   (vlax-3d-point PO)
   (vlax-3d-point PF)
 )
)




(defun c:L1 (/ CHEM D PAS LCHEM PK PM PT FDER PTDERIV PTG PTD LN LGR ptt k p)
 ;; saisir les �l�ments de construction

 (setq CHEM (vlax-ename->vla-object (car (entsel))))
 (setq D (distance p1 p2))
 (setq PAS (* D 6))

 ;; faire 2 d�calages
 (setq OF1 (vla-offset CHEM D))
 (setq LGR (cons (entlast) LGR))
 (setq OF2 (vla-offset CHEM (* -1 D)))
 (setq LGR (cons (entlast) LGR))

 ;; calculer la longueur du chemin
 (setq LCHEM (vlax-curve-getDistAtPoint CHEM (vlax-curve-getEndPoint CHEM)))

 (setq PK 0)
 (setq k 2)
 (while (< PK LCHEM)
   ;; d�terminer le param�tre au pk courant
   (setq PM (vlax-curve-getParamAtDist CHEM PK))
   ;; d�terminer le point au pk courant
   (setq PT (vlax-curve-getPointAtDist CHEM PK))
   ;; d�terminer la d�riv�e premi�re
   (setq FDER (vlax-curve-getfirstderiv CHEM PM))
   ;; d�terminer le point qui construit la tangente � la courbe au point PT
   (setq PTDERIV (mapcar '+ PT FDER))
    ;------------------------------------------------------------;
    ; ---- Cr�e une liste de point selon le pas div. par 2 ------;
    ;------------------------------------------------------------;
   	(setq ptt (polar PT (angle PT PTDERIV) (/ PAS 2)))
	(cond
		((= 0 (rem k 2)) (setq lst (cons ptt lst)))
	)
	(setq k (1+ k))		
    ;------------------------- fin ------------------------------;
   ;; d�terminer les points perpendiculaires � PT � une distance D
   (setq PTG (polar PT (+ (angle PT PTDERIV) (/ pi 2)) D))
   (setq PTD (polar PT (- (angle PT PTDERIV) (/ pi 2)) D))
   ;; tracer la ligne
   (setq LN (cs:line PTD PTG))
   (setq LGR (cons (entlast) LGR))
   ;; passer au pk suivant
   (setq PK (+ PK PAS))
 )	

 (makegroup LGR)
 (vla-erase CHEM)

(command "_-hatch" "p" "solid" "cou" "rouge")						
(foreach p lst (command p))
 (setq lst ())
(command "")
 (princ)
)