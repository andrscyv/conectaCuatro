;;VARIBLES GLOBALES
(setq sigMov nil)
(setq mInfinto -1000000)
(setq infinito  1000000)
(setq numMax 6)
(setq arbol2 nil)

(setq fichaYo 'r)
(setq fichaOp 'a)

;;===============================================
;;==============Prueba===========================
;;===============================================

; (setq arbolP
; 	'((1 -1 2 3 4)
; 	 (2 -1 5 6)
; 	 (3 -1 7 8)
; 	 (4 -1 9 10)
; 	 (5 -1 11 12)
; 	 (6 -1 13)
; 	 (7 -1 14 15)
; 	 (8 -1 16)
; 	 (9 -1 17)
; 	 (10 -1 18 19)
; 	 (11 -1 20 21)
; 	 (12 -1 22 23 24)
; 	 (13 -1 25)
; 	 (14 -1 26)
; 	 (15 -1 27 28)
; 	 (16 -1 29)
; 	 (17 -1 30)
; 	 (18 -1 31 32)
; 	 (19 -1 33)
; 	 (20 5)
; 	 (21 6)
; 	 (22 7)
; 	 (23 4)
; 	 (24 5)
; 	 (25 3)
; 	 (26 6)
; 	 (27 6)
; 	 (28 9)
; 	 (29 7)
; 	 (30 5)
; 	 (31 9)
; 	 (32 8)
; 	 (33 6)))

; (DEFUN ENCUENTRANODO (IDNODO LISTA)
;     (COND
;     	((null idnodo) nil)
;         ((NULL LISTA) '())
;         ((= IDNODO (CAR (CAR LISTA))) (CAR LISTA))
;         (T (ENCUENTRANODO IDNODO (CDR LISTA)))
;     )
; )


; (defun esTerminal (nodo)(null (cddr nodo)))
; ;Prueba
; ;(print (esTerminal '(21 6 3)))

; (defun generaMov (nodo i)	( ENCUENTRANODO (car (nthcdr (+ i 1) nodo)) arbolP))
; ;Prueba
; (print (generaMov ( ENCUENTRANODO 10 arbolP) 3))

; (defun heuristica (nodo) (cadr nodo))
; ;Prueba
; (print (heuristica (ENCUENTRANODO 33 arbolP)))


;;===============================================
;;================MetodosConecta=================
;;===============================================

(setq tablero (make-array '(6 7)))
(setq diag nil)
(setq tableroP (make-array '(6 7) :initial-contents 
	'((r   a   r   r   a   a   nil)
	  (nil r   a   r   r   a   nil)
	  (nil r   a   r   r   nil nil)
	  (nil r   r   a   r   nil nil)
	  (nil nil nil r   a   nil nil)
	  (nil nil nil nil r   nil nil))))

 (defun copy-array (array)
 (let ((dims (array-dimensions array)))
   (adjust-array
    (make-array dims :displaced-to array)
    dims)))

; (defun copy-array (arr)
; 	(let ((dims (array-dimensions arr)) (res nil) (col 0)
; 		(ren 0) (colF 0) (renF 0))
; 		(setq renF (car dims))
; 		(setq colF (cadr dims))
; 		(setq res (make-array dims))
; 		(loop 
; 			;(print ren)
; 			(when (<= renF ren) (return res))
; 			(setq col 0)
; 			(loop
; 				(when (<= colF col) (return))
; 				(setf (aref res ren col) (aref arr ren col))
; 				(incf col))
; 			(incf ren))))
;(print (copy-array tableroP))

(defun inserta (tablero col ficha)
	(insertaFicha tablero col ficha 0))

;Regresa nil si no es posible insertar ficha
(defun insertaFicha ( m col ficha i)
	(cond
		((> i 5) nil)
		((> col 6) nil)
		((null (aref m i col)) (setf (aref m i col) ficha))
		(T (insertaFicha m col ficha (+ i 1)))))
;(print tablero)
;(print (insertaFicha tablero 6 'r 0))
;(print tablero)








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
(defun esTerminalColumna (tablero col ficha )
	(let ((ren 5) (esTerminal nil) (j 0)
		(act nil) (descartado nil) (cont 0) (i 0) ) 
		(loop 
			(when esTerminal (return esTerminal))
			(when (< ren 3) (return nil))
			(setq j 0)
			(setq descartado nil)
			(setq cont 0)
			(loop
				(when (> j 3) (return))
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
;(print tableroP)
;(print 'resTerminal)
;(print (esTerminalColumna tableroP 3 'r))

;;Busca en una col el numero de veces que hay numFichas
;;en cuatro celdas adyacentes sin fichas del oponente entre
;ellas
(defun buscaNCol (tablero col ficha numFichas)
	(let ((ren 5)  (j 0)
		(act nil) (descartado nil) (cont 0) (i 0) (res 0) ) 
		(loop 
			(when (< ren 3) (return res))
			(setq j 0)
			(setq descartado nil)
			(setq cont 0)
			(loop
				(when (> j 3) (return))
				(setq i (- ren j) )
				(setq act (aref tablero i col))
				;(print act)
				;(print i)
				(when (or (> j 3) (< i 0)  descartado )
					(return))
				(cond
					((null act))
					(( not (equal act ficha) ) (setq descartado T))
					(T (incf cont)))
				(incf j))
			(when (and (= cont numFichas) (not descartado)) (incf res))
			(decf ren))))
; (print tableroP)
; (setq col 2)
; (print 'buscaNCol)
; (print col)
; (print (buscaNCol tableroP col 'r 4))

(defun buscaNRen (tablero ren ficha numFichas)
	(let ((col 0)  (j 0)
		(act nil) (descartado nil) (cont 0) (i 0) (res 0) ) 
		(loop 
			(when (> col 3) (return res))
			(setq j 0)
			(setq descartado nil)
			(setq cont 0)
			(loop
				(when (> j 3) (return))
				(setq i (+ col j) )
				(setq act (aref tablero ren i))
				;(print act)
				(when (or (> j 3) (> i 6)  descartado )
					(return))
				(cond
					((null act))
					(( not (equal act ficha) ) (setq descartado T))
					(T (incf cont)))
				(incf j))
			(when (and (= cont numFichas) (not descartado)) (incf res))
			(incf col))))

; (print tableroP)
; (setq ren 2)
; (print 'buscaNRen)
; (print ren)
; (print (buscaNRen tableroP ren 'r 4))

(defun buscaNDiagCres (tablero ren col ficha numFichas)
	(let (  (j 0)
		(act nil) (descartado nil) (cont 0) (i 0) (res 0)
		(termina nil) (colA 0) (renA 0) ) 
		(loop 
			(when termina (return res))
			;(when (> col 3) (return res))
			(setq j 0)
			(setq descartado nil)
			(setq cont 0)
			(setq colA col)
			(setq renA ren)
			(loop
				(when (> j 3) (return))
				(setq renA (+ ren j) )
				(setq colA (+ col j) )
				;(print `(,renA ,colA))
				(if (or (> colA 6) (> renA 5)) (setq termina T) (setq act (aref tablero renA colA)))
				;(print act)
				(when (or descartado termina )
					(return))
				(cond
					((null act))
					(( not (equal act ficha) ) (setq descartado T))
					(T (incf cont)))
				(incf j))
			(when (and (not descartado) (not termina) (= cont numFichas)) (incf res))
			(incf col)
			(incf ren))))
; (print tableroP)
; (setq ren 3)
; (setq col 0)
; (print 'buscaNDiagCres)
; (print `(,ren ,col))
; (print (buscaNDiagCres tableroP ren col 'r 4))

(defun buscaNDiagDec (tablero ren col ficha numFichas)
	(let (  (j 0)
		(act nil) (descartado nil) (cont 0) (i 0) (res 0)
		(termina nil) (colA 0) (renA 0) ) 
		(loop 
			(when termina (return res))
			;(when (> col 3) (return res))
			(setq j 0)
			(setq descartado nil)
			(setq cont 0)
			(setq colA col)
			(setq renA ren)
			(loop
				(when (> j 3) (return))
				(setq renA (- ren j) )
				(setq colA (+ col j) )
				;(print `(,renA ,colA))
				(if (or (> colA 6) (< renA 0)) (setq termina T) (setq act (aref tablero renA colA)))
				;(print act)
				(when (or descartado termina )
					(return))
				(cond
					((null act))
					(( not (equal act ficha) ) (setq descartado T))
					(T (incf cont)))
				(incf j))
			(when (and (not descartado) (not termina) (= cont numFichas)) (incf res))
			(incf col)
			(decf ren))))
; (print tableroP)
; (setq ren 5)
; (setq col 0)
; (print 'buscaNDiagDec)
; (print `(,ren ,col))
; (print (buscaNDiagDec tableroP ren col 'r 4))

;col es la columna donde se hizo la ultima 
; inserción
(defun esTerminalTablero (tablero col ficha)
	(let ((res 0) (ren (renglon tablero col)) (m -1)) 
		(setq res (+ res (buscaNCol tablero col ficha 4)))
		(setq res (+ res (buscaNRen tablero ren ficha 4)))
		(setq m (min col ren))
		(setq res (+ res (buscaNDiagCres tablero (- ren m) (- col m) ficha 4 )))
		(setq m (min col (- 5 ren)))
		(setq res (+ res (buscaNDiagDec tablero (+ ren m) (- col m) ficha 4)))
		(> res 0)))
; (print tableroP)
; (setq col 5)
; (print col)
; (print (esTerminalTablero tableroP col 'r))

(defun heuristicaCol (tablero ficha numFichas)
	(let ((i 0) (res 0) (aux 0))
		(loop
			(when (< 6 i) (return res))
			(setq aux (buscaNCol tablero i ficha numFichas))
			;(print aux)
			(setq res (+ res aux))
			(incf i))))
;(print tableroP)
; (setq numFichas 2)
; (print (heuristicaCol tableroP 'r numFichas))
;(print (buscaNCol tableroP 4 'r 3))

(defun heuristicaRen (tablero ficha numFichas)
	(let ((i 0) (res 0) (aux 0))
		(loop
			(when (< 5 i) (return res))
			(setq aux (buscaNRen tablero i ficha numFichas))
			;(print aux)
			(setq res (+ res aux))
			(incf i))))
; (setq numFichas 2)
; (print numFichas)
; (print (heuristicaRen tableroP 'r numFichas))
;(print (buscaNRen tableroP 4 'r 3))

(defun heuristicaDiagCres (tablero ficha numFichas)
	(let ((diagC '((2 0) (1 0) (0 0) (0 1) (0 2) (0 3))))
		(apply '+ (mapcar (lambda (parOrd) 
					(buscaNDiagCres tablero (car parOrd) (cadr parOrd) ficha numFichas)) diagC))))

;(print (heuristicaDiagCres tableroP 'r 3))

(defun heuristicaDiagDec (tablero ficha numFichas)
	(let ((diagD '((3 0) (4 0) (5 0) (5 1) (5 2) (5 3))))
		(apply '+ (mapcar (lambda (parOrd) 
			(buscaNDiagDec tablero (car parOrd) (cadr parOrd) ficha numFichas)) diagD))))
;(print (heuristicaDiagDec tableroP 'r 2))

(defun heuristica (nodo)
	(cond
		((car nodo)
			(cond
				((equal fichaYo (cadr nodo)) infinito)
				(T mInfinto)))
		(T (let ((res 0) (tablero (caddr nodo)))
			(setq res (+ res (heuristicaCol tablero fichaYo 3)))
			(setq res (+ res (heuristicaRen tablero fichaYo 3)))
			(setq res (+ res (heuristicaDiagCres tablero fichaYo 3)))
			(setq res (+ res (heuristicaDiagDec tablero fichaOp 3)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 3)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 3)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 3)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 3)))
			(setq res (+ res (heuristicaCol tablero fichaYo 2)))
			(setq res (+ res (heuristicaRen tablero fichaYo 2)))
			(setq res (+ res (heuristicaDiagCres tablero fichaYo 2)))
			(setq res (+ res (heuristicaDiagDec tablero fichaOp 2)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 2)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 2)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 2)))
			(setq res (- res (heuristicaDiagDec tablero fichaOp 2)))
			res))))
;(print (heuristica (list nil 'r tableroP)))

;;Inserta ficha en i-esima columna
;;Nodo es un tablero
(defun generaMov (nodo i ficha)
	(let ((nuevoTab (copy-array (caddr nodo)))) 
		(cond
			((null (insertaFicha nuevoTab i ficha  0)) nil)
			(T (list (esTerminalTablero nuevoTab i ficha) ficha nuevoTab))))
	)
;(print (generaMov (list nil 'r  tableroP) 0 'a))
(defun esTerminal (nodo)
	(car nodo))




(defun alphaBeta (nodo prof a b esMax)
	(cond
		((or (= prof 0) (esTerminal nodo)) 
			(let ((h (heuristica nodo))) 
				;(print (list 'esTerminal h nodo))
				 h))
		(esMax
			(let ((v mInfinto) (i 0) (act nil) (aux nil))
				(loop ;(print v)
					(when (or (> i numMax) (<= b a)) (return v))
					(setq act (generaMov nodo i fichaYo))
					;(print act)
					
					(cond
						((null act))
						(T  (setq aux (alphaBeta act (- prof 1) a b nil)) 
						 ;(when (and (< v aux) (equal prof depth)) (setq sigMov act))	
							(setq v (max v aux)) (setq a (max a v))
						 	(when (equal prof depth) (push (list aux i) sigMov))
							;(print (list (caddr nodo) aux i act))
							 ;(print '----------)
							 ;(print '----------)
							 ;(print '----------)
						 ))
					(incf i))
				)
			)
		(T
			(let ((v infinito) (i 0) (act nil)( aux nil) )
				(loop
					(when (or (> i numMax) (<= b a)) (return v))
					(setq act (generaMov nodo i fichaOp))
					;(print act)
					
					(cond
						((null act) )
						(T 
							(setq aux (alphaBeta act (- prof 1) a b T))

								(setq v (min v aux)) 
								(setq b (min b v))
								;(print 'b)
							    ;(print b)
								;(when (equal prof (- depth 1)) (push (list aux i act) sigMin)) 
								;(print  (list (caddr nodo) aux i act))
								 ;(print '----------)
								 ;(print '----------)
								 ;(print '----------)
								 ))
					(incf i))))))
;Prueba
; (print (alphaBeta '(1 -1 2 3 4) depth mInfinto infinito t ))
; (print arbol2)

(defun dameRes (maxim lst)
	 (cond
	 	((equal (car (car lst)) maxim) (cadar lst))
	 	(T (dameRes maxim (cdr lst)))))
;Conecta Cuatro
(setq sigMin nil)
(setq depth 4)
(setq tableroIni (make-array '(6 7) :initial-contents 
	'((r   r  r   a   a   nil nil)
	  (r   nil a   a   a   nil nil)
	  (r   nil a   r   r   nil nil)
	  (a   nil nil a   nil nil nil)
	  (r   nil nil nil nil nil nil)
	  (r   nil nil nil nil nil nil))))


; (setq tableroIni (make-array '(6 7) :initial-contents 
; 	'((nil nil nil nil nil nil nil)
; 	  (nil nil nil nil nil nil nil)
; 	  (nil nil nil nil nil nil nil)
; 	  (nil nil nil nil nil nil nil)
; 	  (nil nil nil nil nil nil nil)
; 	  (nil nil nil nil nil nil nil))))


;(print (heuristica (list nil 'a tableroIni)))
;(print (generaMov (list nil 'a tableroIni) 4 'r))
(print (dameRes (alphaBeta (list nil fichaYo tableroIni) depth mInfinto infinito t ) (reverse sigMov)))
(print sigMov)
 ;(print (alphaBeta (list nil fichaOp aux) (- depth 1) 0 infinito nil ))
 ; (print '----------)
 ; (print '----------)
 ; (print '----------)
 ;(print 'mov)
 ;(print sigMov)
 ;  (print '----------)
 ; (print '----------)
 ; (print '----------)
 ;(print (alphaBeta (generaMov (list nil fichaYo tableroIni) 5 fichaYo) (- depth 1) mInfinto infinito nil))
 ;(print 'mini)
 ;(print sigMin)

 ;(print arbol2)



