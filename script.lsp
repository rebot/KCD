;; Exporteer polylijnen

(Defun c:PPPOLY (/ ent fh fn hnd itm num obj pnt sset v vexx xval sortonx yval sortony xmax xmin ymax ymin zval kword width height bottom top left right)
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

  (Defun average (values)
    (/ (apply '+ values)(length values))
  )

  ;;________________________________________________;;

  (Defun swap ( lst idx idy / itm rtn)
    (setq itm 0)
    (repeat (length lst)
      (cond
        ((= itm idx) (setq rtn (cons (nth idy lst) rtn)))
        ((= itm idy) (setq rtn (cons (nth idx lst) rtn)))
        (t (setq rtn (cons (nth itm lst) rtn)))
      )
      (setq itm (1+ itm))
    )
    (setq rtn (reverse rtn))
  )

  ;;________________________________________________;;

  (Defun sort (values / left right itm num chg nval nnval)
    (setq itm 0 chg 1 num (length values))
    (if (>= num 1)
      (progn
        (while (/= chg 0)
          (setq chg 0 itm 0)
          (repeat (1- num)
            (progn
              (setq nval (nth itm values))
              (setq nnval (nth (1+ itm) values))
              (if (> nval nnval)
                (progn
                  (setq values (swap values itm (1+ itm)))
                  (setq chg (1+ chg))
                )
              )
              (setq itm (1+ itm))
            )
          )
        )
      )
    )
    (setq values values)
  )

  ;;________________________________________________;;

  (Defun filter ( lst elem ineq param )
      (vl-remove-if-not '(lambda (x) (apply ineq (list (nth elem x) param))) lst)
  )

  ;;________________________________________________;;

  (setq sset
    (ssget "_X" '(
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

  (setvar 'attdia 0)

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
            (write-line "" fh)
            ;; cond: wordt hier als een soort switch gebruikt
            (cond
              ((eq obj "POINT")
                (setq pnt (cdr (assoc 10 ent)))
                (setq pnt (trans pnt 0 1))
                ;; rtos: 2de argument = de mode (2 = decimaal) - 3de argument = precisie
                (write-line (strcat (rtos (car pnt) 2 4) ","
                               (rtos (cadr pnt) 2 4) ","
                               (rtos (caddr pnt) 2 4)) fh)

              )
              ((eq obj "POLYLINE")
        	      (setq v hnd)
        	      (setq vexx (3dpoly-verts v ))
        	      (foreach pnt vexx
        		         (write-line (strcat (rtos (car pnt) 2 4) ","
                        (rtos (cadr pnt) 2 4) ","
                        (rtos (caddr pnt) 2 4)) fh)
                )

                ;; plaats block op de figuur

                (setq xval (mapcar 'car vexx))
                (setq yval (mapcar 'cadr vexx))
                (setq zval (mapcar 'caddr vexx))

                (setq xmin (apply 'min xval) xmax (apply 'max xval))
                (setq ymin (apply 'min yval) ymax (apply 'max yval))

                (setq sortonx (vl-sort vexx '(lambda (a b) (< (car a) (car b)))))
                (setq sortony (vl-sort vexx '(lambda (a b) (< (cadr a) (cadr b)))))

                (setq width (- xmax xmin))
                (setq height (- ymax ymin))

                (setq bottom (filter (filter (filter sortonx 0 '> (+ xmin (/ width 10))) 1 '< (+ ymin (/ height 2))) 0 '< (- xmax (/ width 10))))
                (setq top (filter (filter (filter sortonx 0 '> (+ xmin (/ width 10))) 1 '> (- ymax (/ height 2))) 0 '< (- xmax (/ width 10))))
                (setq left (filter (filter (filter sortony 1 '> (+ ymin (/ height 10))) 0 '< (+ xmin (/ width 2))) 1 '< (- ymax (/ height 10))))
                (setq right (filter (filter (filter sortony 1 '> (+ ymin (/ height 10))) 0 '> (- xmax (/ width 2))) 1 '< (- ymax (/ height 10))))

                (if (> (length bottom) 1)
                  (progn
                    (setq kword
                      (strcat
                        (rtos (car (nth (fix (/ (length bottom) 2)) bottom)) 2 5) ","
                        (rtos (cadr (nth (fix (/ (length bottom) 2)) bottom)) 2 5) ","
                        (rtos (caddr (nth (fix (/ (length bottom) 2)) bottom)) 2 1)
                      )
                    )
                    (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
                  )
                )

                (if (> (length top) 1)
                  (progn
                    (setq kword
                      (strcat
                        (rtos (car (nth (fix (/ (length top) 2)) top)) 2 5) ","
                        (rtos (cadr (nth (fix (/ (length top) 2)) top)) 2 5) ","
                        (rtos (caddr (nth (fix (/ (length top) 2)) top)) 2 1)
                      )
                    )
                    (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
                  )
                )

                (if (> (length left) 1)
                  (progn
                    (setq kword
                      (strcat
                        (rtos (car (nth (fix (/ (length left) 2)) left)) 2 5) ","
                        (rtos (cadr (nth (fix (/ (length left) 2)) left)) 2 5) ","
                        (rtos (caddr (nth (fix (/ (length left) 2)) left)) 2 1)
                      )
                    )
                    (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
                  )
                )

                (if (> (length right) 1)
                  (progn
                    (setq kword
                      (strcat
                        (rtos (car (nth (fix (/ (length right) 2)) right)) 2 5) ","
                        (rtos (cadr (nth (fix (/ (length right) 2)) right)) 2 5) ","
                        (rtos (caddr (nth (fix (/ (length right) 2)) right)) 2 1)
                      )
                    )
                    (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
                  )
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

  (setvar 'attdia 1)
  (princ)
)
