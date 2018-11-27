;; Exporteer polylijnen

(Defun c:KCDKolommen (/ selectie puntenlijst niv n ptn)

  (Defun bouwpuntenlijst (selectie / itm num hnd ent punten lijst zcoord lyr)
    ;; Selecteer alle punten binnen de huidige selectie

    (if selectie
      (progn
        ;; sslength: bewaar in 'num' het aantal 3D polylines dat we selecteerden
        (setq itm 0 num (sslength selectie))

        (while (< itm num)
          (setq hnd (ssname selectie itm))
          (setq ent (entget hnd))
          ;; cond: wordt hier als een soort switch gebruikt
          (cond
            ((member "AcDb3dPolyline" (mapcar 'cdr ent))
      	      (setq punten (punten3dpoly hnd))
              (setq lijst (append punten lijst))

              (setq zcoord (unique (mapcar '(lambda (x) (roundm x 0.1)) (mapcar 'caddr punten))))
              (setq lyr (strcat "kolommen " (rtos (car zcoord) 2 2)))
              (command "._layer" "_M" lyr "")

              (entmod (subst (cons 8 lyr) (assoc 8 ent) ent))
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

  (Defun punten3dpoly (en / elist lst vex)
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

  (defun unique ( l )
    (if l (cons (car l) (unique (vl-remove (car l) (cdr l)))))
  )

  ;;________________________________________________;;

  (Defun round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
  )

  ;;________________________________________________;;

  (defun roundm ( n m )
    (* m (fix ((if (minusp n) - +) (/ n (float m)) 0.5)))
  )

  ;;________________________________________________;;

  (Defun verwijderblocks ( selectie / itm num)

    (if selectie
      (progn
        (setq itm 0 num (sslength selectie))

        (while (< itm num)
          (if (and
                (eq (cdr (assoc 0 (entget (ssname selectie itm)))) "INSERT")
                (eq (cdr (assoc 2 (entget (ssname selectie itm)))) "PT")
              )
              (command "ERASE" (ssname selectie itm) "")
          )
          (setq itm (1+ itm))
        )
      )
    )

    (princ)
  )

  ;;________________________________________________;;

  (Defun eenvleugjemaggie ( lst / subniv subn lng zval sortonx sortony xmin xmax ymin ymax width height bottom top left right kword zcoord lyr)

    ;;__________ DEZE CODE IS OPTIONEEL - KIEST HET BELANGRIJKSTE SUBVLAK ______________;;
    (setq subniv (unique (mapcar '(lambda (x) (roundm x 0.1)) (mapcar 'caddr lst))))

    (foreach subn subniv
      (setq lng (cons (filter (filter lst 2 '>= (- subn 0.05)) 2 '< (+ subn 0.05)) lng))
    )

    (setq lng (vl-sort lng '(lambda (a b) (> (length a) (length b)))))
    (setq lst (nth 0 lng))
    ;;__________ DEZE CODE IS OPTIONEEL - KIEST HET BELANGRIJKSTE SUBVLAK ______________;;

    (setq zval (mapcar 'caddr lst))

    (setq sortonx (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
    (setq sortony (vl-sort lst '(lambda (a b) (< (cadr a) (cadr b)))))

    (setq xmin (apply 'min (mapcar 'car lst)))
    (setq xmax (apply 'max (mapcar 'car lst)))
    (setq ymin (apply 'min (mapcar 'cadr lst)))
    (setq ymax (apply 'max (mapcar 'cadr lst)))

    (princ (strcat "\nxmin: " (rtos xmin 2 5) " xmax: " (rtos xmax 2 5)))
    (princ (strcat "\nymin: " (rtos xmin 2 5) " ymax: " (rtos xmax 2 5)))

    ;; Definieer de breedte en hoogte van de kolommen
    (setq width (- xmax xmin))
    (setq height (- ymax ymin))

    (princ (strcat "\nwidth: " (rtos width 2 3) "height: " (rtos height 2 3)))

    ;; Maak een selectie van de zijkanten
    (setq bottom (filter (filter (filter sortonx 0 '> (+ xmin (/ width 8))) 1 '< (+ ymin (/ height 2))) 0 '< (- xmax (/ width 8))))
    (setq top (filter (filter (filter sortonx 0 '> (+ xmin (/ width 8))) 1 '> (- ymax (/ height 2))) 0 '< (- xmax (/ width 8))))
    (setq left (filter (filter (filter sortony 1 '> (+ ymin (/ height 8))) 0 '< (+ xmin (/ width 2))) 1 '< (- ymax (/ height 8))))
    (setq right (filter (filter (filter sortony 1 '> (+ ymin (/ height 8))) 0 '> (- xmax (/ width 2))) 1 '< (- ymax (/ height 8))))

    ;; Zet de correcte layer aan
    (setq zcoord (unique (mapcar '(lambda (x) (roundm x 0.1)) zval)))
    (setq lyr (strcat "kolommen " (rtos (car zcoord) 2 2)))
    (command "._layer" "_M" lyr "")

    ;; BOTTOM - Plaatsen block aan de ZUID zijde
    (if (> (length bottom) 2)
      (progn
        (setq kword
          (strcat
            (rtos (car (nth (fix (/ (length bottom) 2)) bottom)) 2 5) ","
            (rtos (cadr (nth (fix (/ (length bottom) 2)) bottom)) 2 5) ","
            (rtos (caddr (nth (fix (/ (length bottom) 2)) bottom)) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
      (progn
        (setq kword
          (strcat
            (rtos (/ (+ xmax xmin) 2) 2 5) ","
            (rtos (- ymin (/ height 2)) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )

    ;; TOP - Plaatsen block aan de NOORD zijde
    (if (> (length top) 2)
      (progn
        (setq kword
          (strcat
            (rtos (car (nth (fix (/ (length top) 2)) top)) 2 5) ","
            (rtos (cadr (nth (fix (/ (length top) 2)) top)) 2 5) ","
            (rtos (caddr (nth (fix (/ (length top) 2)) top)) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
      (progn
        (setq kword
          (strcat
            (rtos (/ (+ xmax xmin) 2) 2 5) ","
            (rtos (+ ymax (/ height 2)) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )

    ;; LEFT - Plaatsen block aan de WEST zijde
    (if (> (length left) 2)
      (progn
        (setq kword
          (strcat
            (rtos (car (nth (fix (/ (length left) 2)) left)) 2 5) ","
            (rtos (cadr (nth (fix (/ (length left) 2)) left)) 2 5) ","
            (rtos (caddr (nth (fix (/ (length left) 2)) left)) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
      (progn
        (setq kword
          (strcat
            (rtos (- xmin (/ width 2)) 2 5) ","
            (rtos (/ (+ ymax ymin) 2) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )

    ;; RIGHT - Plaatsen block aan de OOST zijde
    (if (> (length right) 2)
      (progn
        (setq kword
          (strcat
            (rtos (car (nth (fix (/ (length right) 2)) right)) 2 5) ","
            (rtos (cadr (nth (fix (/ (length right) 2)) right)) 2 5) ","
            (rtos (caddr (nth (fix (/ (length right) 2)) right)) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
      (progn
        (setq kword
          (strcat
            (rtos (+ xmax (/ width 2)) 2 5) ","
            (rtos (/ (+ ymax ymin) 2) 2 5) ","
            (rtos (average zval) 2 1)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos (average zval) 2 0))
      )
    )

    (setq sortonx nil sortony nil)
    (setq xmin nil xmax nil ymin nil ymax nil)
    (setq width nil height nil)
    (setq bottom nil top nil left nil right nil)
    (setq kword nil)
    (princ)
  )

  ;;________________________________________________;;

  (princ "Selecteer de kolom")
  (setq selectie
    (ssget '(
      (-4 . "<OR")
        (-4 . "<AND")
          (0 . "INSERT")(2 . "PT")
        (-4 . "AND>")
        (-4 . "<AND")
          (0 . "*POLYLINE")(-4 . "&")(70 . 8)
        (-4 . "AND>")
      (-4 . "OR>")
    ))
  )

  (if selectie
    (progn
      (initget "Yes No")
      (if (eq (getkword "\nAlle bestaande 'PT' blocks zullen worden verwijderd. Wenst u door te gaan? [Yes/No] <No>: ") "Yes")
        (progn
          ;; Verwijder bestaande blocks
          (verwijderblocks selectie)

          ;; Bouw puntenlijst
          (setq puntenlijst (bouwpuntenlijst selectie))

          ;; Deel de polylijnen op in groepen
          (setq niv (vl-sort (mapcar 'round (mapcar 'caddr puntenlijst)) '<))

          (setvar 'attdia 0)
          (setvar 'attreq 1)

          (foreach n niv
            (setq ptn puntenlijst)
            (setq ptn (filter ptn 2 '>= (- n 0.5)))
            (setq ptn (filter ptn 2 '< (+ n 0.5)))

            (eenvleugjemaggie ptn)
          )

          (setvar 'attdia 1)

        )
      )
    )
  )

  (princ)
)
