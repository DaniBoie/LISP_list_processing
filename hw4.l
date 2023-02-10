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
    ((equal 0 (MY-LENGTH L))
      nil
    )
    ((listp (car L))
      (cons (MY-COPY (car L)) (MY-COPY (cdr L)))
    )
    ((atom L)
      (cons L nil)
    )
    (T
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

(defun MY-SUBST (X Y Z)
  (cond
    ((equal 0 (MY-LENGTH Z)) ;; Base Case: If null List, nil result
      nil
    )
    ((listp (car Z)) ;; if the current element is a List, return [MY-SUBST current List] + [MY-SUBST rest of Z]
      (cons (MY-SUBST X Y (car Z)) (MY-SUBST X Y (cdr Z)))
    )
    ((equal X (car Z)) ;; if current element == search element, return replace element + [MY-SUBST rest of Z]
      (cons Y (MY-SUBST X Y (cdr Z)))
    )
    (T ;; else return current element + [MY-SUBST rest of Z]
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
  
  (cond

    ((equal 0 (MY-LENGTH N1)) ;; Base Cases : If either list is null, return the other.
      N2
    )
    ((equal 0 (MY-LENGTH N2))
      N1
    )

    ((> (+ (car N1) (car N2)) 9) ;; if N1 + N2 > 9 return list (ones_digit, 1 + MY-ADD the rest)
      (cons 
        (- (+ (car N1) (car N2)) 10) ;; current digit carryout
        (MY-ADD '(1) (MY-ADD (cdr N1) (cdr N2))) ;; 1 + recurse    * TAUTOLOGY : SCENARIO A: CARRYOUT
      )
    )

    (T ;; else return list (N1 + N2, MY-ADD the rest)    * TAUTOLOGY : SCENARIO B: NO CARRYOUT
        (cons (+ (car N1) (car N2)) (MY-ADD (cdr N1) (cdr N2)))
    )

  )
  
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

(defun testallhw4 ()
        (PRINT "MY-LENGTH")
        (PRINT (MY-LENGTH NIL))
        (PRINT (MY-LENGTH '(B (A B C))))
        (PRINT (MY-LENGTH '(A (((B))) C)))
        (PRINT (MY-LENGTH '(A B C)))
    (TERPRI)
        (PRINT "MY-MEMQ")
        (PRINT (MY-MEMQ 'A NIL))
        (PRINT (MY-MEMQ 'B '(A B C)))
        (PRINT (MY-MEMQ 'D '(A B C D E F G)))
        (PRINT (MY-MEMQ 'D '(A B C D)))
        (PRINT (MY-MEMQ 'D '(A B C)))
    (TERPRI)
        (PRINT "MY-APPEND")
        (PRINT (MY-APPEND '(A B C) '(D E F)))
        (PRINT (MY-APPEND '((A) (B) (C)) '((D) (E) (F))))
        (PRINT (MY-APPEND NIL '(D E F)))
        (PRINT (MY-APPEND '(A B C) NIL))
    (TERPRI)
        (PRINT "MY-REVERSE")
        (PRINT (MY-REVERSE NIL))
        (PRINT (MY-REVERSE '(A)))
        (PRINT (MY-REVERSE '(1 2 3 4 5)))
        (PRINT (MY-REVERSE '((1 2 3) 4 ((5 6)))))
    (TERPRI)
        (PRINT "MY-MAPCAR")
        (PRINT (MY-MAPCAR 'CAR '((A B C) (X Y Z) (1 2 3))))
        (PRINT (MY-MAPCAR 'CDR '((A B C) (X Y Z) (1 2 3))))
        (PRINT (MY-MAPCAR '1+ '(1 3 5 7)))
        (PRINT (MY-MAPCAR 'ATOM '(A (B) C (D) E)))
    (TERPRI)
        (PRINT "MY-COPY")
        (PRINT (MY-COPY '(A B ((C 1)) 2 3)))
        (PRINT (MY-COPY '(1 2 3)))
        (PRINT (MY-COPY '(A B . C)))
        (PRINT (EQ (SETQ L '(A (B) C)) (MY-COPY L)))
    (TERPRI)
        (PRINT "MY-NTH")
        (PRINT (MY-NTH '(A B C D E) 1))
        (PRINT (MY-NTH '(A B C D E) 3))                
        (PRINT (MY-NTH '(A B C D E) 30))               
        (PRINT (MY-NTH '(A B C D E) 0))
    (TERPRI)
        (PRINT "MY-REMOVE")
        (PRINT (MY-REMOVE '(A B) '(A B (A B) A A B (A B))))
        (PRINT (MY-REMOVE 'A '(A B (A B) A B)))
    (TERPRI)
        (PRINT "MY-SUBST")
        (PRINT (MY-SUBST 'B 'A '(A B (A B) (C (A B)))))
    (TERPRI)
        (PRINT "MY-SUBSET")
        (PRINT (MY-SUBSET 'ATOM '(A (B) (C D) E F G)))
        (PRINT (MY-SUBSET 'LISTP '(A (B) (C D) E F G)))
    (TERPRI)
        (PRINT "MY-ADD")
        (PRINT (MY-ADD '(0) '(0)))
        (PRINT (MY-ADD '(1) '(1)))
        (PRINT (MY-ADD '(9) '(9)))
        (PRINT (MY-ADD '(1 1 1 1 1 1 1 1 1 1) '(9 9 9 9 9 9 9 9 9 9)))
        (PRINT (MY-ADD '(1) '(9 9 9 9 9 9 9 9 9 9)))
        (PRINT (MY-ADD '(9 9 9 9 9 9 9 9 9 9) '(1)))
    (TERPRI)
        (PRINT "MY-MERGE")
        (PRINT (MY-MERGE '(1 3 5 7 9) '(2 4 6 8 10)))
        (PRINT (MY-MERGE '(1 2 3 7 8 9) '(4 5 6 10)))
        (PRINT (MY-MERGE '(1 2 3) '(4 5 6 7 8 9 10)))
        (PRINT (MY-MERGE '(1 3 5 6 7 8 9 10) '(2 4)))
        (PRINT (MY-MERGE NIL '(1 2 3 4 5 6 7 8 9 10)))
    (TERPRI)
        (PRINT "MY-SUBLIST")
        (PRINT (MY-SUBLIST '(1 2 3) '(1 2 3 4 5)))
        (PRINT (MY-SUBLIST '(3 4 5) '(1 2 3 4 5)))
        (PRINT (MY-SUBLIST '(C D) '(A B C D E)))
        (PRINT (MY-SUBLIST '(3 4) '(1 2 3 5 6)))
        (PRINT (MY-SUBLIST '(1 2 3 4 5) '(3 4 5)))
        (PRINT (MY-SUBLIST '(2 4) '(1 2 3 4 5)))
        (PRINT (MY-SUBLIST '(1 3 5) '(1 2 3 4 5)))
    (TERPRI)
        (PRINT "MY-ASSOC")
        (PRINT (MY-ASSOC 'A NIL))
        (PRINT (MY-ASSOC 'A '((A . B)(C E F)(B))))
        (PRINT (MY-ASSOC 'C '((A . B)(C E F)(B))))
        (PRINT (MY-ASSOC 'B '((A . B)(C E F)(B))))
        (PRINT (MY-ASSOC 'F '((A . B)(C E F)(B))))
    (TERPRI)
        (PRINT "MY-REPLACE")
        (PRINT (MY-REPLACE '((G . C)(C . G)(T . A)(A . U)) '(G A T C)))
        (PRINT (MY-REPLACE '((G . C)(C . G)(T . A)(A . U)) '(G A T C C T C C A T A T A C A A C G G T)))
        (PRINT (MY-REPLACE '((G . C)(C . G)(T . A)(A . U)) '(G (A) T C C (T) C C A (T) A T A C A A (C) G G T)))
        (PRINT (MY-REPLACE '((G . C)(C . G)(T . A)(A . U)) '(G A X C C X C C A X A T A C A X C G X T)))
    (TERPRI)
        (PRINT (MY-REPLACE '((UCB . UCLA)(UCSD . UCI)(BASKETBALL . TENNIS)) '(UCSD IS PLAYING BASKETBALL AGAINST UCB)))
        (PRINT (MY-REPLACE '((UCB . UCLA)(UCSD . UCI)(BASKETBALL . TENNIS)) '(UCR IS PLAYING FOOTBALL AGAINST UCSC)))

    T
    )


(defun factorial (n)
  (cond
    ((equal 0 n)
      1
    )
    (T
      (* n (factorial (- n 1)))
    )
  )
)

(defun my_attach (x L)
  (cond
    ((null L)
      (cons x nil)
    )
    (T
      (cons (car L) (my_attach x (cdr L)))
    )
  )
)

(defun remove_all (L x) 
  (cond
    ((null L)
      nil
    )
    ((equal x (car L))
      (remove_all (cdr L) x)
    )
    (T
      (cons (car L) (remove_all (cdr L) x))
    )
  )
)

(defun rotate_left (L n)

  (cond
    ((null L)
      nil
    )
    ((equal 0 n)
      L
    )
    (T
      (rotate_left (my_attach (car L) (cdr L)) (- 1 n))
    )
  )
)

;;(defun rotate_helper (L)
;;  (cond
;;  ((null L))
;;    nil
;;  )
;;  (T
;;    
;;  )
;;
;;)