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
  (cond ;; Base Case: if length 0, return nil
    ((equal 0 (MY-LENGTH L))
      nil
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

;; !! NOT PASSING TEST CASE ('A B . C) OR (EQ (SETQ L '(A (B) C)) (MY-COPY L))

(defun MY-COPY (L) ;; Takes ProperList L : Returns deep copy of L
  (cond
    ((equal 1 (MY-LENGTH L))
      (cons (car L) nil)
    )
    ((atom (cdr L)) ;; TEST
      (cons (cdr L) nil)
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
 ;; !! IN PROGRESS !!
(defun MY-SUBST (X Y Z)
  (cond
    ((equal 0 (MY-LENGTH Z)) ;; Base Case: If null List, nil result
      nil
    )
    ;;((listp (car Z))
    ;;  (MY-SUBST X Y (car Z))
    ;;)
    ((equal X (car Z))
      (cons Y (MY-SUBST X Y (cdr Z)))
    )
    (T
      (cons (car Z) (MY-SUBST X Y (cdr Z)))
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

 ;; !! YET TO BE DEFINED
 nil

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

;; !! NOT PASSING TEST CASE (MY-SUBLIST '(2 4) '(1 2 3 4 5)) & (MY-SUBLIST '(1 3 5) '(1 2 3 4 5))

(defun MY-SUBLIST (L1 L2) ;; Takes sublist L1 and search list L2 : Returns T if L1 is sublist of L2 and nil otherwise.
  (cond
  
    ((> (MY-LENGTH L1) (MY-LENGTH L2)) ;; Base Case for nil, length of sublist is greater than search list
      nil
    )
    ((equal 1 (MY-LENGTH L1)) ;; Base Case for T, If sublist is of size 1 and current element in L2 == sublist
      (cond
        ((eq (car L1) (car L2))
          T
        )
        (T ;; else keep searching for L1 in rest of L2
          (MY-SUBLIST L1 (cdr L2))
        )
      )
    )
    ((eq (car L1) (car L2)) ;; If found matching element in L2 for sublist L1, recurse down fiding rest of L1 sublist in rest of L2 search list.
      (MY-SUBLIST (cdr L1) (cdr L2))
    )
    (T ;; else recurse searching for match
      (MY-SUBLIST L1 (cdr L2))
    )
  )
)

(defun MY-ASSOC (A ALIST) ;; Takes Atom A and association list ALIST : Returns the association pair for A in ALIST. If there is no association, it returns nil
  (cond
    ((equal 0 (MY-LENGTH ALIST)) ;; Base Case : If null association list return nil
      nil
    )
    ((equal A (caar ALIST)) ;; If atom A == current ALIST.key return (ALIST.key, ALIST.value)
      (car ALIST)
    )
    (T ;; else recurse down searching for match
      (MY-ASSOC A (cdr ALIST))
    )
  )
)

(defun MY-REPLACE (ALIST L) ;; Takes association list ALIST and S-expression L : Returns L with each variable (key) in ALIST replaced with the corresponding value it is bound to in ALIST.
  (cond
    ((equal 0 (MY-LENGTH L)) ;; Base Case : Null List returns nil
      nil
    )
    ((equal nil (MY-ASSOC (car L) ALIST)) ;; If There is no association for the current element, return a list of the current element + MY-REPLACE of rest of list
      (cons (car L) (MY-REPLACE ALIST (cdr L)))
    )
    (T ;; Else return a list of the ALIST[current_element] + MY-REPLACE of rest of list
      (cons (cdr (MY-ASSOC (car L) ALIST))  (MY-REPLACE ALIST (cdr L)) )
    )
  )
)