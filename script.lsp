;; Exporteer polylijnen

(Defun c:KTDKolommen (/ selectie blocks puntenlijst niv n ptn i)

  (Defun bouwpuntenlijst (selectie / sset itm num hnd ent obj punten lijst)
    ;; Selecteer alle punten binnen de huidige selectie

    (setq sset
      (ssget "_P" '(
                (-4 . "<AND")
                  (0 . "*POLYLINE")
                  (-4 . "&")(70 . 8)
                (-4 . "AND>")
              )
      )
    )

    (if sset
      (progn
        ;; sslength: bewaar in 'num' het aantal 3D polylines dat we selecteerden
        (setq itm 0 num (sslength sset))

        (while (< itm num)
          (setq hnd (ssname sset itm))
          (setq ent (entget hnd))
          (setq obj (cdr (assoc 0 ent)))
          ;; cond: wordt hier als een soort switch gebruikt
          (cond
            ((eq obj "POLYLINE")
      	      (setq punten (punten3dpoly hnd))
              (setq lijst (append punten lijst))
            )
            (t nil)
          )

          (setq itm (1+ itm))
        )
      )
    )

    (reverse lijst)
  )

  ;;________________________________________________;;

  (Defun punten3dpoly (en / elist  lst vex)
    (if (member "AcDb3dPolyline" (mapcar 'cdr (entget en)))
      (progn
        (setq vex (entnext en))
        (setq elist (entget vex))
        (while (= (cdr (assoc 0 elist)) "VERTEX")
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

  (Defun filter ( lst elem ineq param )
      (vl-remove-if-not '(lambda (x) (apply ineq (list (nth elem x) param))) lst)
  )

  ;;________________________________________________;;

  (Defun round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
  )

  ;;________________________________________________;;

  (Defun eenvleugjemaggie ( lst / zval sortonx sortony xmin xmax ymin ymax width height bottom top left right kword)

    (setq zval (mapcar 'caddr lst))

    (setq sortonx (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
    (setq sortony (vl-sort lst '(lambda (a b) (< (cadr a) (cadr b)))))

    (setq xmin (car (car sortonx)) xmax (car (nth (1- (length sortonx)) sortonx)))
    (setq ymin (cadr (car sortony)) ymax (cadr (nth (1- (length sortony)) sortony)))

    (princ (strcat "xmin: " (rtos xmin 2 5) " xmax: " (rtos xmax 2 5)))
    (princ (strcat "ymin: " (rtos xmin 2 5) " ymax: " (rtos xmax 2 5)))

    ;; Definieer de breedte en hoogte van de kolommen
    (setq width (- xmax xmin))
    (setq height (- ymax ymin))

    ;; Maak een selectie van de zijkanten
    (setq bottom (filter (filter (filter sortonx 0 '> (+ xmin (/ width 8))) 1 '< (+ ymin (/ height 2))) 0 '< (- xmax (/ width 8))))
    (setq top (filter (filter (filter sortonx 0 '> (+ xmin (/ width 8))) 1 '> (- ymax (/ height 2))) 0 '< (- xmax (/ width 8))))
    (setq left (filter (filter (filter sortony 1 '> (+ ymin (/ height 8))) 0 '< (+ xmin (/ width 2))) 1 '< (- ymax (/ height 8))))
    (setq right (filter (filter (filter sortony 1 '> (+ ymin (/ height 8))) 0 '> (- xmax (/ width 2))) 1 '< (- ymax (/ height 8))))

    ;; Wanneer de zijkant leeg is, dan wordt er een block geplaatst op een halve hoogte of breedte van het minimum

    ;; BOTTOM - Plaatsen block aan de ZUID zijde
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
      (progn
        (setq kword
          (strcat
            (rtos (/ (+ xmax xmin) 2) 2 5) ","
            (rtos (- ymin (/ height 2)) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )

    ;; TOP - Plaatsen block aan de NOORD zijde
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
      (progn
        (setq kword
          (strcat
            (rtos (/ (+ xmax xmin) 2) 2 5) ","
            (rtos (+ ymax (/ height 2)) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )

    ;; LEFT - Plaatsen block aan de WEST zijde
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
      (progn
        (setq kword
          (strcat
            (rtos (- xmin (/ width 2)) 2 5) ","
            (rtos (/ (+ ymax ymin) 2) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )

    ;; RIGHT - Plaatsen block aan de OOST zijde
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
      (progn
        (setq kword
          (strcat
            (rtos (+ xmax (/ width 2)) 2 5) ","
            (rtos (/ (+ ymax ymin) 2) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )
  )

  ;;________________________________________________;;

  (Defun hocuspocus ( lst / sortonx sortony xmin xmax ymin ymax kword)

    (setq sortonx (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
    (setq sortony (vl-sort lst '(lambda (a b) (< (cadr a) (cadr b)))))

    (setq xmin (car (car sortonx)) xmax (car (nth (1- (length sortonx)) sortonx)))
    (setq ymin (cadr (car sortony)) ymax (cadr (nth (1- (length sortony)) sortony)))

    (princ (strcat "xmin: " (rtos xmin 2 5) " xmax: " (rtos xmax 2 5)))
    (princ (strcat "ymin: " (rtos xmin 2 5) " ymax: " (rtos xmax 2 5)))

    (setq kword
      (strcat
        (rtos (/ (+ xmin xmax) 2) 2 5) ","
        (rtos (/ (+ ymin ymax) 2) 2 5) ",0"
      )
    )
    (command "-INSERT" "PT" kword "0.5" "0.5" "" "B" 2 0)
  )

  ;;________________________________________________;;

  (princ "Selecteer de kolom")
  (setq selectie (ssget))
  (if selectie
    (progn
      (initget "Yes No")
      (if (eq (getkword "\nAlle bestaande 'PT' blocks zullen worden verwijderd. Wenst u door te gaan? [Yes/No] <No>: ") "Yes")
        (progn
          (setq puntenlijst (bouwpuntenlijst selectie))

          ;; Verwijder bestaande blocks
          (setq blocks (ssget "_P" (list '(0 . "INSERT") (cons 2 "PT"))))
          (command "ERASE" blocks "")

          ;; Deel de polylijnen op in groepen
          (setq niv (vl-sort (mapcar 'round (mapcar 'caddr puntenlijst)) '<))

          (setvar 'attdia 0)
          ;(setvar "CMDECHO" 0)

          (setq i 0)

          (foreach n niv
            (setq ptn puntenlijst)
            (setq ptn (filter ptn 2 '>= (- n 0.5)))
            (setq ptn (filter ptn 2 '< (+ n 0.5)))

            (eenvleugjemaggie ptn)

            (if (= i 1) (hocuspocus ptn))

            (setq i (1+ i))
          )

          ;(setvar "CMDECHO" 1)
          (setvar 'attdia 1)

        )
      )
    )
  )
)
