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
(setq tableroP (make-array '(6 7) :initial-contents 
	'((nil nil nil nil nil nil nil)
	  (nil nil r nil nil nil nil)
	  (nil nil r r nil nil nil)
	  (nil nil r r nil nil nil)
	  (nil nil r a nil nil nil)
	  (nil nil a r nil nil nil))))

(defun inserta (tablero col ficha)
	(insertaFicha tablero col ficha 0))

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

(defun renglon (tablero col)
	(renglonPrimerFicha tablero col 5))
(defun renglonPrimerFicha (tablero col j)
	(cond 
		((< j 0) nil)
		((null (aref tablero j col)) (renglonPrimerFicha tablero col (- j 1)))
		(T j)))
;(insertaFicha tablero 2 'r 0)
;(insertaFicha tablero 2 'r 0)
;(insertaFicha tablero 2 'r 0)
;(insertaFicha tablero 2 'r 0)
;(insertaFicha tablero 2 'r 0)
;(insertaFicha tablero 2 'r 0)
;(print tablero)
;(print (renglonPrimerFicha tablero 2 5))

;;FALTA PROBAR
(defun esTerminalC (tablero col ficha )
	(let ((ren (renglon tablero col)) (esTerminal nil) (j 0)
		(act nil) (descartado nil) (cont 0) (i 0) ) 
		(loop 
			(when esTerminal (return esTerminal))
			(when (< ren 3) (return nil))
			(setq j 0)
			(setq descartado nil)
			(setq cont 0)
			(loop
				(setq i (- ren j) )
				(setq act (aref tablero i col))
				;;(print act)
				(when (or esTerminal (< i 0)  descartado )
					(return))
				(cond
					((or (not (equal act ficha)) (null act) ) (setq descartado T))
					(T (incf cont)))
				(setq esTerminal (equal cont 4))
				(incf j))
			(decf ren))))

(print tableroP)
(print 'resTerminal)
(print (esTerminalC tableroP 2 'r))


(defun esTerminalColumna (tablero col ficha)
	(setq auxEsTermCol (renglon tablero col))
	()
	(esTerminalC tablero col ficha ))



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


