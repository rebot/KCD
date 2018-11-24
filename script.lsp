(Defun c:test ( )
  (SETVAR "CMDECHO" 0)
  (command "ZOOM" "E")
  (prompt "Alles wordt geselecteerd")
  (setq selectie (ssget "x"))
  (SETVAR "CMDECHO" 1)
  (princ)
)

(Defun c:cm ( )
  (setq ss1 (ssget))
  (command "COPY" ss1 "" "m")
)

(Defun c:maakselectie( )
  (setq selectie (ssget "_X" '((0 . "*POLYLINE")(-4 . "&")(70 . 8))))
  (sssetfirst nil selectie)
  (princ)
)

(defun C:LINE ( )
	(prompt "Shouldn’t you be using Polylines?")
	(command "PLINE"))

(defun S::STARTUP ( )
    (command "UCSICON" "N")
		(command "UNDEFINE" "LINE")
)

(defun C:PRINTDXF ( )
  (setq ent (entlast))               ; Set ent to last entity.
  (setq entl (entget ent))           ; Set entl to association list of last entity.
  (setq ct 0)                        ; Set ct (a counter) to 0.
  (textpage)                         ; Switch to the text screen.
  (princ "\nentget of last entity:")
  (repeat (length entl)              ; Repeat for number of members in list:
    (print (nth ct entl))            ; Output a newline, then each list member.
    (setq ct (1+ ct))                ; Increments the counter by one.
  )
 (princ)                             ; Exit quietly.
)

(Defun c:test123 (/ selectie parent child)
  (setq selectie (ssget "_X" '((0 . "*POLYLINE")(-4 . "&")(70 . 8))))
  (setq parent (cdr (assoc -1 (entget (ssname selectie 0)))))
  (if (member "AcDb3dPolyline" (mapcar 'cdr (entget (ssname selectie 0))))
      (setq child (cdr (assoc -2 (entget (entnext (ssname selectie 0))))))
  )
  (if (= parent child) (alert "feest"))
)
