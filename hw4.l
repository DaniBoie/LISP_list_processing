(defun MY-LENGTH (L) ;;Takes proper list : Returns number of atoms
  (cond ;; Base Case: Length == 0
    ((null L)
      0
    ) ;; Recursive Step : Add 1 for current element and recurse down list.
    (T
      (+ 1 (MY-LENGTH (cdr L)))
    )
  )
)

(defun MY-MEMQ (A L) ;; Takes Atom, Proper List : returns List starting from first occurance of Atom
  (cond ;; Base Cases: If List[0] == searching_atom return List
    ((null L)
      nil
    )
    ((equal A (car L))
      L
    )
    (T ;; Recursive Step: Search the list without the current element.
      (MY-MEMQ A (cdr L))
    )
  )
)

(defun MY-APPEND (L1 L2) ;; Takes Proper List 1/2 : Returns [ProperList1, ProperList2]
  (cond ;; Base Cases: If either list is empty, appened form is just the other list.
    ((null L1)
      L2
    )
    ((null L2)
      L1
    )
    (T ;; Recursive Step: return a new list made with first element of L1 + All elements after L1[0] appended to L2.  
      (cons (car L1) (MY-APPEND (cdr L1) L2))
    )
  )
)

(defun MY-REVERSE (L) ;; Takes proper List L : Returns Reverse of list L
  (cond ;; Base Case: if length 1, return element by itself
    ((equal 1 (MY-LENGTH L))
      L
    )
    (T ;; Recursive Step: return the reverse of the list 1 smaller than L APPENDED to L -> [smaller than L] + [L]
      (MY-APPEND 
        (MY-REVERSE (cdr L)) 
        (cons (car L) nil) 
      )
    )
  )
)

(defun MY-MAPCAR (F L) ;; Takes Function F & ProperList L : Returns list of results of F on elements of L -> [(F L[0]), (F L[1]), ... (F L(n))]

  (cond ;; Base Case: if length 1, return list with only that value mapped
    ((equal 1 (MY-LENGTH L))
      (cons (apply F L) nil)
    )
    (T ;; Recursive Step: return first atom (turned into list) mapped with F APPENDED to the rest of the list mapped with F
      (MY-APPEND 
        (cons (apply F (cons (car L) nil)) nil)  
        (MY-MAPCAR F (cdr L)) 
      )
    )
  )
)

(defun MY-COPY (L) ;; Takes ProperList L : Returns deep copy of L
  (cond
    ((equal 1 (MY-LENGTH L))
      (cons (car L) nil)
    )
    ((atom (cdr L))

    )
    ((listp L)
      (cons (car L) (MY-COPY (cdr L)))
    )
  )
)

(defun MY-NTH (L N) ;; Takes Proper list L, Positive Int N : Returns 
  (cond ;; Base Cases
    ((> N (MY-LENGTH L)) ;; If N > length of L return nil
      nil
    )
    ((equal 0 (MY-LENGTH L)) ;; If N == 0 return nil Atom + [List]
      (nil . L)
    )
    ((equal 1 (MY-LENGTH L)) ;; If N == 1 return L
      L
    )
    (T ;; Else
      (cond 
        ((equal N (car L)) ;; If Atom N == Car L return L
          L
        )
        (T ;; Else Recurse down list looking for N
          MY-NTH((cdr L) N)
        )
      )
    )
  )
)
