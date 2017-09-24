(defun solve (L)						;	Preparing and solving the whole expression
	(cond ((is_atom_all L T) (summ_up_all (list_all (make_plus (put_ones (fst_one L) '()) '()) '()) '()))
		(T (summ_up_all (cond ((and (listp (car (flatten_all (list_all (make_plus (put_ones (fst_one (pow_down L '())) '()) '()) '()) '()))) (null (cdr (flatten_all (list_all (make_plus (put_ones (fst_one (pow_down L '())) '()) '()) '()) '())))) (car (flatten_all (list_all (make_plus (put_ones (append (list 1) (pow_down L '())) '()) '()) '()) '())))
							((listp (car (flatten_all (list_all (make_plus (put_ones (fst_one (pow_down L '())) '()) '()) '()) '()))) (append (append (list (car (flatten_all (list_all (make_plus (put_ones (append (list 1) (pow_down L '())) '()) '()) '()) '()))) (list '+)) (flat (cddr (flatten_all (list_all (make_plus (put_ones (fst_one (pow_down L '())) '()) '()) '()) '())) '())))
							(T (flatten_all (list_all (make_plus (put_ones (fst_one (pow_down L '())) '()) '()) '()) '())))	 '()))
	)
)

(defun flat (L Lr)
	(cond ((null L) Lr)
		((listp (car L)) (cond ((is_atom_all (car L) T) (flat (cdr L) (append Lr (list (car L))))) (T (flat (cdr L) (append Lr (flat (car L) '()))))))
		(T (flat (cdr L) (append Lr (list (car L)))))
	)
)

(defun is_atom_all (L b) 				;	Checking if the list consists of atoms
	(cond ((null L) b)
		((atom (car L)) (is_atom_all (cdr L) (and b T)))
		(T NIL)
	)
)


(defun flatten_all (L Lr)				;	Flattening all the inside expressions
	(cond ((null L) Lr)
		((eq '+ (car L)) (flatten_all (cdr L) (append Lr (list '+))))
		((atom (car L)) (cond ((eq (cadr L) '^) (flatten_all (cdddr L) (solve_expr (append Lr (list (flatten (append (append (append '() (car L)) (cadr L)) (caddr L))) '())))))
						(T (flatten_all (cdr L) (solve_expr (append Lr (list (flatten (list (car L)) '()))))))))
		(T (flatten_all (cdr L) (cond ((no_plus Lr T) (solve_expr (append Lr (list (flatten (car L) '())))))
									(T (append Lr (list (flatten (car L) '()))))
								)
			))
	)
)

(defun find_atoms (L Lr)				;	Makes a list of atom elements
	(cond ((null L) Lr)
		((atom (car L)) (find_atoms (cdr L) (append Lr (list (car L)))))
		(T (find_atoms (cdr L) Lr))
	)
)

(defun del_atoms (L Lr)					;	Deletes all the atoms
	(cond ((null L) Lr)
		((atom (car L)) (del_atoms (cdr L) Lr))
		(T (del_atoms (cdr L) (append Lr (list (car L)))))
	)
)

(defun flatten (L Lr)					;	Flattening every expression
	(cond ((null L) Lr)
		((is_atom_all L T) (summ_up_all (list_all (make_plus (put_ones (append (list 1) (pow_down L '())) '()) '()) '()) '()))
		((no_plus L T) (append Lr (flatten_all (pow_down (append (list (find_atoms L '())) (del_atoms L '())) '()) '())))
		(T (append Lr (solve L)))
	)
)

(defun no_plus (L b)					;	Checking if there is no plus sign in an expression
	(cond ((null L) b)
		((eq '+ (car L)) 'NIL)
		(T (no_plus (cdr L) b))
	)
)

(defun summ_up_all (L Lr)				;	Summing all up
	(cond ((null L) Lr)
		((null (cdr L)) (append Lr L))
		((no_plus L T) (append Lr L))
		((eq '+ (car L)) (summ_up_all (cdr L) (append Lr (list '+))))
		(T (summ_up_all (delete_elements (cond ((atom (car L)) (list (car L))) (T (car L))) (cdr L) '()) (append Lr (list (summ_up (car L) (cdr L))))))
	)
)

(defun delete_elements (L Ln Lr)		;	Deleting repeting objects
	(cond ((null Ln) Lr)
		((eq '+ (car Ln)) (cond ((equality (cdr L) (cdr (cond ((atom (cadr Ln)) (list (cadr Ln))) (T (cadr Ln))))) (delete_elements L (cddr Ln) Lr))
								(T (delete_elements L (cddr Ln) (append (append Lr (list '+)) (list (cadr Ln)))))
							))
		(T (cond ((equality L (car Ln)) (delete_elements L (cdr Ln) Lr))
				(T (delete_elements L (cdr Ln) (append Lr (car Ln))))))
	)
)

(defun summ_up (L Lt)					;	Summing all letter-equal objects
	(cond ((null Lt) L)
		((eq '+ (car Lt)) (summ_up L (cdr Lt)))
		(T (summ_up (summarize L (car Lt)) (cdr Lt)))
	)
)

(defun fst_one (L)						;	Adding first "1" if needed
	(cond ((numberp (car L)) L)
		(T (append (list 1) L))
	)
)

(defun put_ones (L Lr)					;	Adding "1" if needed before every object
	(cond ((null L) Lr)
		((or (eq '- (car L)) (eq '+ (car L))) (put_ones (cdr L) (cond ((numberp (cadr L)) (append Lr (list (car L)))) (T (append (append Lr (list (car L))) (list 1))))))
		(T (put_ones (cdr L) (append Lr (list (car L)))))
	)
)

(defun make_plus (L Lr)					;	Changing all "-" into "+ -1"
	(cond ((null L) Lr)
		((eq '- (car L)) (make_plus (cddr L) (append (append Lr (list '+)) (list (- 0 (cadr L))))))
		(T (make_plus (cdr L) (append Lr (list (car L)))))
	)
)

(defun list_all (L Lr)					;	Making lists so it is easier to count
	(cond ((null L) Lr)
		(T (cond ((eq '+ (car (cdr (make_first_list L '())))) (list_all (cddr (make_first_list L '())) (append (append Lr (list (car (make_first_list L '())))) (list '+))))
				(T (list_all (cdr (make_first_list L '())) (append Lr (list (car (make_first_list L '()))))))))
	)
)

(defun make_first_list (L Lr)			;	Making the first list
	(cond ((null L) (list Lr))
		((eq '+ (car L)) (append (list Lr) L))
		(T (make_first_list (cdr L) (append Lr (list (car L)))))
	)
)

(defun summarize (L1 L2)				;	Summing up 2 objects
	(cond ((equality (cdr L1) (cdr L2)) (append (list (+ (car L1) (car L2))) (cdr L1)))
		(T L1)
	)
)

(defun isin (L1 L2)						;	Checking if this letter/letter with it's power is in L1
	(cond ((null L2) 'NIL)
		((null (cdr L1)) (cond ((eq (car L1) (car L2)) (cond ((eq (cadr L2) '^) 'NIL)
															(T T)))
								(T (isin L1 (cdr L2)))
		))
		(T (cond ((eq (car L1) (car L2)) (cond ((eq (cadr L2) '^) (eq (caddr L2) (caddr L1)))
															(T 'NIL)))
								(T (isin L1 (cdr L2)))))
	)
)

(defun include (L1 L2 b)				;	Checking if one expr is in another
	(cond ((null L2) b)
		((eq (cadr L2) '^) (include L1 (cdddr L2) (and b (isin (append (append (list (car L2)) (list (cadr L2))) (list (caddr L2))) L1))))
		(T (include L1 (cdr L2) (and b (isin (list (car L2)) L1))))
	)
)

(defun equality (L1 L2) 				;	Equality of the letter parts of 2 expressions
	(cond ((null L1) (cond ((null L2) T)
						(T NIL)))
		((null L2) (cond ((null L1) T)
						(T NIL)))
		(T (and (include L1 L2 T) (include L2 L1 T)))
	)
)

(defun pow_expr (L n Lr)				;	Making ()() .. () of () ^ n
	(cond ((eq n 0) Lr)
		(T (pow_expr L (- n 1) (append Lr (list L))))
	)
)

(defun pow_down (L Lr)					; 	Makes powers of lists unto multiplies of lists
	(cond ((null L) Lr)
		((listp (car L)) (cond ((eq (cadr L) '^) (pow_down (cdddr L) (append Lr (pow_expr (car L) (caddr L) '()))))
							(T (pow_down (cdr L) (append Lr (list (car L)))))
						))
		(T (pow_down (cdr L) (append Lr (list (car L)))))
	)
)

(defun solve_expr (L)					; 	Solving small expressions
	(cond ((null (cdr L)) (car L))
		((null (cddr L)) (list (multiply (car L) (cadr L) '() )))
		(T (append (list (multiply (car L) (cadr L) '() )) (cddr L)))
	)
)

(defun multiply (L1 L2 Lr)				; 	Multiplying 2 objects
	(cond ((null L1) Lr)
		((eq '+ (car L1)) (multiply (cdr L1) L2 (append Lr (list '+))))
		((is_atom_all L1 T) (append Lr (mult_left L1 L2 '())))
		(T (multiply (cdr L1) L2 (append Lr (mult_left (car L1) L2 '()))))
	)
)

(defun mult_left (L1 L2 Lr)				;	Helping function
	(cond ((null L2) Lr)
		((eq '+ (car L2)) (mult_left L1 (cdr L2) (append Lr (list '+))))
		((is_atom_all L2 T) (append Lr (list (simplify (append L1 L2)))))
		(T (mult_left L1 (cdr L2) (append Lr (list (simplify (append L1 (car L2)))))))
	)
)

(defun simplify (L)						; 	Simplifying
	(append (list (multiply_num (sort_num L '()) 1)) (pow_1_del (simplify_let L '()) '()))
)

(defun multiply_num (L n)				;	Multiplying all the numbers in the row
	(cond ((null L) n)
		(T (multiply_num (cdr L) (* n (car L))))
	)
)

(defun sort_num (L Lr)					;	Making a row of numbers to multiply all the numbers that are in the "chain"
	(cond ((null L) Lr)
		((eq (car L) '^) (sort_num (cddr L) Lr))
		((numberp (car L)) (sort_num (cdr L) (append Lr (list (car L)))))
		(T (sort_num (cdr L) (append Lr (list 1))))
	)
)

(defun count_let (x n L)				;	Helps us to count the power of the letter
	(cond ((null L) n)					;	when flattened (without sublists)
		((eq x (car L)) (count_let x (+ n (cond ((eq (cadr L) '^) (caddr L))
												(T 1)
											)) (cdr L)))
		(T (count_let x n (cdr L))) 
	)
)

(defun delete_let (x L Ln)				;	Helps us to delete all repeating letters of x
	(cond ((null L) Ln)
		((eq x (car L)) (delete_let x (cond ((eq (cadr L) '^) (cdddr L))
							(T (cdr L))) Ln))
		(T (delete_let x (cdr L) (append Ln (list (car L)))))
	)
)

(defun simplify_let (L Ln)				;	Simplifying when flattened
	(cond ((null L) Ln)
		((eq (car L) '^) (simplify_let (cdr L) (append (append Ln (list (car L))) (list (cadr L)))))
		((numberp (car L)) (simplify_let (cdr L) Ln))
		((atom (car L)) (simplify_let (delete_let (car L) L '()) (append (append (append Ln (list (car L))) (list '^)) (list (count_let (car L) 0 L)))))
		(T (simplify_let (cdr L) (append Ln (list (car L)))))
	)
)

(defun pow_1_del (L Lr)					;	Deleting all "^1"
	(cond ((null L) Lr)
		((eq '^ (car L)) (cond ((eq (cadr L) 1) (pow_1_del (cddr L) Lr))
								(T (pow_1_del (cddr L) (append (append Lr (list (car L))) (list (cadr L)))))
							))
		(T (pow_1_del (cdr L) (append Lr (list (car L)))))
	)
)

(defun main (L)							;	Main function
	(is_zero (signs (erase_nulls (fl (solve L) '()) '()) '()))
	;(signs (fl (solve L) '()) '())
)

(defun erase_nulls (L Lr)
	(cond ((null L) Lr)
		((eq '+ (car L)) (cond ((eq 0 (cadr L)) (erase_nulls (after_sign (cdr L)) Lr)) (T (erase_nulls (cdr L) (append Lr (list (car L)))))))
		((eq 0 (car L)) (erase_nulls (after_sign (cdr L)) Lr))
		(T (erase_nulls (cdr L) (append Lr (list (car L)))))
	)
)

(defun after_sign (L)
	(cond ((eq '+ (car L)) L)
		((null L) L)
		(T (after_sign (cdr L)))
	)
)

(defun is_zero (L)
	(cond ((null L) 0)
		(T L)
	)
)

(defun fl (L Lr)						;	Final flattening
	(cond ((null L) Lr)
		((is_atom_all L T) (cond ((and (eq 1 (car L)) (not (or (eq '+ (cadr L)) (null (cdr L))))) (cdr L))
								(T L)
							))
		((atom (car L)) (fl (cdr L) (append Lr (list (car L)))))
		(T (fl (cdr L) (append Lr (fl (car L) '()))))
	)
)

(defun signs (L Lr)						;	Changing signs back to normal
	(cond ((null L) Lr)
		((eq '+ (car L)) (cond ((and (numberp (cadr L)) (< (cadr L) 0)) (signs (cddr L) (append (append Lr (list '-)) (cond ((eq 1 (- 0 (cadr L))) '()) (T (list (- 0 (cadr L))))))))
								(T (signs (cddr L) (append (append Lr (list '+)) (list (cadr L)))))
							))
		(T (signs (cdr L) (append Lr (list (car L)))))
	)
)


;	Down below are the expressions that this program has been tested on

(defun test1 ()(main '(x ^ 2 - x ^ 3)))
(defun test2 ()(main '((y - z x ^ 2) ^ 3 - 4)))
(defun test3 ()(main '(z x ^ 2 (y + a x ^ 2) ^ 3 + x ^ 2 y ^ 3)))
(defun test4 ()(main '((y + z x ^ 2) ^ 3 )))
(defun test5 ()(main '(x ^ 2 + x ^ 3)))
(defun test6 ()(main '((x + y) 3)))
(defun test7 ()(main '((x + y) ^ 3)))
(defun test8 ()(main '((2 x) ^ 3)))
(defun test9 ()(main '(x)))
(defun test10 () (main '((x + 1) (x + 1))))
(defun test11 () (main '((x + 1) (x + 2 y) ^ 2 + (x ^ 2 + 1) (y + 1))))

(defun test_1 () (main '((2 x y x - x ^ 2 y - y x x) (a ^ 2 + b ^ 2))))
(defun test_2 () (main '((x ^ 2 - y ^ 2) (x ^ 2 + y ^ 2))))
(defun test_3 () (main '((a - b) (a ^ 2 + a b + b ^ 2))))
(defun test_4 () (main '((a + b) (a ^ 2 - a b + b ^ 2))))
(defun test_5 () (main '((n ^ 2 - 2 n + 2) (n ^ 2 + 2 n + 2))))
(defun test_6 () (main '((x - 2 (3 y - 2 y + 5 z - 4 z - z) + y (7 + 2 u - 6 - 2 u)) (x + y))))
(defun test_7 () (main '((x ^ 2 - y ^ 2) (x ^ 2) + (x ^ 2 - y ^ 2) (y ^ 2))))
(defun test_8 () (main '()))
