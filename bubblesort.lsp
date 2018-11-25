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
  (cond
      ((= ineq -1) (vl-remove-if-not '(lambda (x) (< (nth elem x) param)) lst))
      ((= ineq 0) (vl-remove-if-not '(lambda (x) (= (nth elem x) param)) lst))
      ((= ineq 1) (vl-remove-if-not '(lambda (x) (> (nth elem x) param)) lst))
      (t nil)
  )
)

;;________________________________________________;;

(Defun filterp ( lst elem ineq param )
    (vl-remove-if-not '(lambda (x) (apply ineq (list (nth elem x) param))) lst)
)
