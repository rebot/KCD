;; Exporteer polylijnen

(Defun c:PPPOLY (/ ent fh fn hnd itm num obj pnt sset v vexx)
  ;; helper to get 3dpoly coordinates
  (Defun 3dpoly-verts (en / elist  lst vex)
    ;; cdr: verkrijg het tweede element
    ;; entget: geeft eigenschappen terug van de entiteit als dotted lists
    ;; mapcar: voert de functie 'cdr' uit op elk element in de lijst
    ;; member: geeft de lijst terug vanaf de positie waar het argument gevonden wordt
    ;; -> wordt er niks gevonden, dan geeft member 'nil' terug
    (if (member "AcDb3dPolyline" (mapcar 'cdr (entget en)))
      (progn
        ;; entnext: de volgende entiteit in de database kan bestaan uit subentiteiten
        ;; TIP: extracting the -2 group from that entity, which is the main entity's name
        (setq vex (entnext en))
        (setq elist (entget vex))
        (while (= (cdr (assoc 0 elist)) "VERTEX")
          ;; cons: voeg het 1ste argument vooraan aan de lijst toe die je als tweede element opgaf
          ;; -> indien de argumenten twee atoms zijn, dan wordt een dotted list terug gegeven
          ;; trans: zet het punt om van UCS naar WCS
          (setq lst (cons (trans (cdr (assoc 10 elist)) 1 0) lst))
        	(setq vex (entnext vex))
        	(setq elist (entget vex))
  	    )
      )
    )
    (reverse lst)
  )

  ;;________________________________________________;;


  (setq sset "_X"
    (ssget '(
              (-4 . "<OR")
                (0 . "POINT")
                (-4 . "<AND")
                  (0 . "*POLYLINE")
                  (-4 . "&")(70 . 8)
                (-4 . "AND>")
              (-4 . "OR>")
            )
    )
  )

  (if sset
    (progn
      ;; sslength: bewaar in 'num' het aantal 3D polylines dat we selecteerden
      (setq itm 0 num (sslength sset))
      ;; getfiled: vraag een bestandsnaam op
      (setq fn (getfiled "Exporteer punten" "" "txt" 1))
      (if (/= fn nil)
        (progn
          ;; open: het argument 'w' wordt gebruikt om aan te geven dat we willen overschrijven
          (setq fh (open fn "w"))
          (while (< itm num)
            (setq hnd (ssname sset itm))
            (setq ent (entget hnd))
            (setq obj (cdr (assoc 0 ent)))
            ;; cond: wordt hier als een soort switch gebruikt
            (cond
              ((eq obj "POINT")
                (setq pnt (cdr (assoc 10 ent)))
                (setq pnt (trans pnt 0 1))
                ;; rtos: 2de argument = de mode (2 = decimaal) - 3de argument = precisie
                (write-line (strcat (rtos (car pnt) 2 3) ","
                               (rtos (cadr pnt) 2 3) ","
                               (rtos (caddr pnt) 2 3)) fh)

              )
              ((eq obj "POLYLINE")
        	      (setq v hnd)
        	      (setq vexx (3dpoly-verts v ))
        	      (foreach pnt vexx
        		         (write-line (strcat (rtos (car pnt) 2 3) ","
                        (rtos (cadr pnt) 2 3) ","
                        (rtos (cadr pnt) 2 3)) fh)
                )
              )
              (t nil)
            )
            (setq itm (1+ itm))
          )
          (close fh)
        )
      )
    )
  )
  (princ)
)
