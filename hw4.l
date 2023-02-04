(defun MY-LENGTH (L)
  (cond
    ((null L)
      0
    )
    (T
      (+ 1 (MY-LENGTH (cdr L)))
    )
  )
)

(defun MY-MEMQ (A L)
  (cond
    ((null L)
      nil
    )
    ((equal A (car L))
      L
    )
    (T
      (MY-MEMQ A (cdr L))
    )
  )
)

(defun MY-APPEND (L1 L2)
  (cond
    ((null L1)
      L2
    )
    ((null L2)
      L1
    )
    (T
      (cons (car L1) (MY-APPEND (cdr L1) L2))
    )
  )
)

(defun MY-REVERSE (L)
  (cond
    ((equal 1 (MY-LENGTH L))
      L
    )
    (T
      ( MY-APPEND (MY-REVERSE (cdr L)) (cons (car L) nil) )
    )
  )
)

(defun MY-MAPCAR (F L)

  (cond
    ((equal 1 (MY-LENGTH L))
      ( cons (apply F L) nil)
    )
    (T
      ( MY-APPEND ( (apply F (cons (car L) nil)) (MY-MAPCAR F (cdr L)) ) )
    )

  )

)