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

;; !! NOT PASSING TEST CASE ('A B . C)
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

(defun MY-NTH (L N) ;; Takes Proper list L, Positive Int N : Returns List including and after nth element
  (cond ;; Base Cases
    ((> N (MY-LENGTH L)) ;; If N > length of L return nil
      nil
    )
    ((equal 0 N) ;; If N == 0 return nil Atom + [List]
      (cons nil L)
    )
    ((equal 1 N) ;; If N == 1 return L
      L
    )
    (T  ;; Else Recurse down list until nth is reached
      (MY-NTH (cdr L) (- N 1))
    )
  )
)

(defun MY-REMOVE (X L) ;; Takes Atom to remove from list X and list L : Returns list L without occurances of X at top level.
  (cond
    ((equal 0 (MY-LENGTH L)) ;; Base Case: If null List, nil result
      nil
    )
    ((equal X (car L)) ;; if the current element == X return result my-remove on L[1->n]
      (MY-REMOVE X (cdr L))
    )
    (T
      (cons (car L) (MY-REMOVE X (cdr L))) ;; else return current element + result of my-remove on L[1->n]
    )
  )
)

;; NOT FULLY DEFINED
(defun MY-SUBST (X Y Z)
  (cond
    ((equal 0 (MY-LENGTH Z))
      nil
    ) 

  )
)

(defun MY-SUBSET (FN L) ;; Takes function FN and proper list L : Returns new list only containing elements of L that returned T when applied with FN
  (cond
    ((equal 0 (MY-LENGTH L)) ;; Base Case: If null List, nil result
      nil
    )
    ((apply FN (cons (car L) nil)) ;; If current element of list returns True, add it to list and recurse down rest of elements
      (cons (car L) (MY-SUBSET FN (cdr L)))
    )
    (T ;; If current element of list returns False, recurse down the list without this element.
      (MY-SUBSET FN (cdr L))
    )
  )
)

(defun MY-ADD (N1 N2)

 ;; YET TO BE DEFINED

)

(defun MY-MERGE (L1 L2) ;; Takes sorted proper lists L1 and L2 : Returns new sorted list containing elements of both lists.
  (cond
    ((equal 0 (MY-LENGTH L1))
      L2
    )
    ((equal 0 (MY-LENGTH L2)) ;; If either List is empty, return the other
      L1
    )
    ((< (car L1) (car L2)) 
      (cons (car L1) (MY-MERGE (cdr L1) L2))
    )
    ((> (car L1) (car L2)) ;; If current elements != return current smaller elem + [MY-MERGE everything else]
      (cons (car L2) (MY-MERGE L1 (cdr L2)))
    )
    (T ;; If values are equal, doesn't matter which case you return
      (cons (car L1) (MY-MERGE (cdr L1) L2))
    )
  )
)

(defun MY-SUBLIST (L1 L2)
  (cond
  
    ((> (MY-LENGTH L1) (MY-LENGTH L2))
      nil
    )
    ((equal 1 (MY-LENGTH L1))
      (cond
        ((eq (car L1) (car L2))
          T
        )
        (T
          nil
        )
      )
    )
    ((eq (car L1) (car L2))
      (MY-SUBLIST (cdr L1) (cdr L2))
    )
    (T
      (MY-SUBLIST L1 (cdr L2))
    )
  )
)
