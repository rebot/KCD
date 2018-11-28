;; Exporteer polylijnen

(Defun c:KCDBalken (/ selectie puntenlijst nivx nivy nx ny ptn)

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
              (setq lyr (strcat "balken " (rtos (car zcoord) 2 2)))
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

  (Defun eenvleugjemaggie ( lst name / subniv subn lng lsti xval sortony sortonz ymin ymax zmin zmax width height bottom kword zcoord lyr)

    (setq xval (mapcar 'car lst))

    (setq sortony (vl-sort lst '(lambda (a b) (< (cadr a) (cadr b)))))
    (setq sortonz (vl-sort lst '(lambda (a b) (< (caddr a) (caddr b)))))

    (setq ymin (apply 'min (mapcar 'cadr lst)))
    (setq ymax (apply 'max (mapcar 'cadr lst)))
    (setq zmin (apply 'min (mapcar 'caddr lst)))
    (setq zmax (apply 'max (mapcar 'caddr lst)))

    (princ (strcat "\nymin: " (rtos ymin 2 5) " ymax: " (rtos ymax 2 5)))
    (princ (strcat "\nzmin: " (rtos zmin 2 5) " zmax: " (rtos zmax 2 5)))

    ;; Definieer de breedte en hoogte van de balk
    (setq width (- ymax ymin))
    (setq height (- zmax zmin))

    (princ (strcat "\nwidth: " (rtos width 2 3) "height: " (rtos height 2 3)))

    ;; Maak een selectie van de zijkanten
    (setq bottom (filter (filter (filter sortony 0 '> (+ ymin (/ width 8))) 1 '< (+ zmin (/ height 2))) 0 '< (- ymax (/ width 8))))

    ;; Zet de correcte layer aan
    ;(setq zcoord (unique (mapcar '(lambda (x) (roundm x 0.1)) zval)))
    (setq lyr (strcat "balken " (rtos name 2 2)))
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
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos name 2 0))
      )
      (progn
        (setq kword
          (strcat
            (rtos (average xval) 2 1) ","
            (rtos (/ (+ ymax ymin) 2) 2 5) ","
            (rtos (- zmin (/ height 2)) 2 5)
          )
        )
        (command "_.-INSERT" "PT" kword "0.5" "0.5" "" (rtos name 2 0))
      )
    )


    (setq sortony nil sortonz nil)
    (setq ymin nil ymax nil zmin nil zmax nil)
    (setq width nil height nil)
    (setq bottom nil)
    (setq kword nil)
    (princ)
  )

  ;;________________________________________________;;

  (princ "Selecteer alle balken")
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
          (setq nivx (vl-sort (mapcar 'round (mapcar 'car puntenlijst)) '<))

          (setvar 'attdia 0)
          (setvar 'attreq 1)

          (foreach nx nivx

            ;; Deel de polylijnen op in groepen
            (setq nivy (vl-sort (mapcar 'round (mapcar 'cadr puntenlijst)) '<))

            (foreach ny nivy

              (setq ptn puntenlijst)

              (setq ptn (filter ptn 0 '>= (- nx 0.5)))
              (setq ptn (filter ptn 0 '< (+ nx 0.5)))
              (setq ptn (filter ptn 1 '>= (- ny 0.5)))
              (setq ptn (filter ptn 1 '< (+ ny 0.5)))

              (eenvleugjemaggie ptn ny)

            )
          )

          (setvar 'attdia 1)

        )
      )
    )
  )

  (princ)
)
