;;===============================================
;;==============Prueba===========================
;;===============================================

(setq arbolP
	'((1 -1 2 3 4)
	 (2 -1 5 6)
	 (3 -1 7 8)
	 (4 -1 9 10)
	 (5 -1 11 12)
	 (6 -1 13)
	 (7 -1 14 15)
	 (8 -1 16)
	 (9 -1 17)
	 (10 -1 18 19)
	 (11 -1 20 21)
	 (12 -1 22 23 24)
	 (13 -1 25)
	 (14 -1 26)
	 (15 -1 27 28)
	 (16 -1 29)
	 (17 -1 30)
	 (18 -1 31 32)
	 (19 -1 33)
	 (20 5)
	 (21 6)
	 (22 7)
	 (23 4)
	 (24 5)
	 (25 3)
	 (26 6)
	 (27 6)
	 (28 9)
	 (29 7)
	 (30 5)
	 (31 9)
	 (32 8)
	 (33 6)))

;(defun esTerminal (nodo)(null (cddr nodo)))
;;Prueba
;;(print (esTerminal '(21 6 3)))

;(defun generaMov (nodo i)	( ENCUENTRANODO (car (nthcdr (+ i 1) nodo)) arbolP))
;;Prueba
;(print (generaMov ( ENCUENTRANODO 10 arbolP) 3))

;(defun heuristica (nodo) (cadr nodo))
;Prueba
;(print (heuristica (ENCUENTRANODO 33 arbolP)))


;;===============================================
;;================MetodosConecta=================
;;===============================================

(setq tablero (make-array '(6 7)))

(defun imprimeTablero (tablero)
	(cond
		((null tablero))
		(T (print (car tablero)) (imprimeTablero (cdr tablero)))))
;(imprimeTablero tablero)

;Dada una columna (lst) inserta en el primer
;espacio vacio (0). Si no puede no modifica 
;la columna
(defun insertaEnPrimerVacio (lst ficha)
	(cond
		((null lst))
		((equal (car lst) 0) (setf (car lst) ficha))
		(T (insertaEnPrimerVacio (cdr lst) ficha))))

;(setq p '(r r r r r 0) )
;(print p)
;(insertaEnPrimerVacio p 'a)
;(print p)
(defun insertaFicha ( m col ficha i)
	(cond
		((> i 5) nil)
		((> col 6) nil)
		((null (aref m i col)) (setf (aref m i col) ficha))
		(T (insertaFicha m col ficha (+ i 1)))))
;(print tablero)
;(print (insertaFicha tablero 6 'r 0))
;(print tablero)

;;Inserta ficha en i-esima columna
;;Nodo es un tablero
(defun generaMov (nodo i ficha)
	(insertaFicha (caddr  nodo) i ficha  0)
	)

(defun indicePrimerFicha (tablero col j)
	(cond 
		((> j 5) nil)
		()))
(defun checaColT (tablero i)
	())

(setq sigMov nil)
(setq mInfinto -1)
(setq infinito 15)
(setq numMax 3)
(setq arbol2 nil)
(setq depth 5)

(DEFUN ENCUENTRANODO (IDNODO LISTA)
    (COND
    	((null idnodo) nil)
        ((NULL LISTA) '())
        ((= IDNODO (CAR (CAR LISTA))) (CAR LISTA))
        (T (ENCUENTRANODO IDNODO (CDR LISTA)))
    )
)



(defun alphaBeta (nodo prof a b esMax)
	(cond
		((or (= prof 0) (esTerminal nodo)) (heuristica nodo))
		(esMax
			(let ((v -1) (i 1) (act nil) (aux nil))
				(loop ;(print v)
					(setq act (generaMov nodo i))
					(when (or (> i numMax) (<= b a)) (return v))
					(cond
						((null act))
						(T (setq aux (alphaBeta act (- prof 1) a b nil)) 
						 (when (< v aux) (setq sigMov act))	(setq v (max v aux)) (setq a (max a v))
						 (push act arbol2)))
					(incf i))
				)
			)
		(T
			(let ((v 15) (i 1) (act nil) )
				(loop
					(setq act (generaMov nodo i))
					(when (or (> i numMax) (<= b a)) (return v))
					(cond
						((null act))
						(T (setq v (min v (alphaBeta act (- prof 1) a b T))) 
							(setq b (min b v)) (push act arbol2)))
					(incf i))))))

;(print (alphaBeta '(1 -1 2 3 4) depth -1 15 t ))
;(print arbol2)


