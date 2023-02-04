(defun MY-LENGTH (L)
  2     (cond
  3         ((null L)
  4             0
  5         )
  6         (T
  7             (+ 1 (MY-LENGTH (cdr L)))
  8         )
  9     )
 10
 11 )
 12
 13 (defun MY-MEMQ (A L)
 14     (cond
 15         ((null L)
 16             nil
 17         )
 18         ((equal A (car L))
 19             L
 20         )
 21         (T
 22             (MY-MEMQ A (cdr L))
 23         )
 24     )
 25 )
 26
 27 (defun MY-APPEND (L1 L2)
 28     (cond
 29         ((null L1)
 30             L2
 31         )
 32         ((null L2)
 33             L1
 34         )
 35         (T
 36           (cons (car L1) (MY-APPEND (cdr L1) L2))
 37         )
 38     )
 39 )
 40
 41 (defun MY-REVERSE (L)
 42     (cond
 43         ((equal 1 (MY-LENGTH L))
 44             L
 45         )
 46         (T
 47             ( MY-APPEND (MY-REVERSE (cdr L)) (cons (car L) nil) )
 48         )
 49     )
 50 )
