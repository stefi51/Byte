(defun praviTablu (velicinaTable pomvelicinaTable)
(if (= pomvelicinaTable 0) '() (cons (vratiRed velicinaTable pomvelicinaTable ) (praviTablu velicinaTable (1- pomvelicinaTable))) )

)


(defun vratiRed (velicinaTable pomvelicinaTable)

(cond  ((= velicinaTable 0) '())

	( (=(mod pomvelicinaTable 2) 0) (cons '(".") (vratiRed (1- velicinaTable) (1- pomvelicinaTable) ) ))  ;crno polje


        (t (cons '("B") (vratiRed (1- velicinaTable) (1- pomvelicinaTable) ) )) ;belo polje
)

)


(defun napuniTablu (praznaTabla )

(if (null praznaTabla) '() (cons (napuniRed (car praznaTabla) ) (napuniTablu (cdr praznaTabla) )) )

	)

(defun napuniRed (prazanRed )

(if (equal (car prazanRed) '("B")) (puniCrnim prazanRed ) (puniBelim prazanRed ))

	)

(defun puniCrnim (prazanRed )

(cond  ((null prazanRed) '())

	((equal (car prazanRed) '(".")) (cons '("X") (puniCrnim (cdr prazanRed) )))


        (t  (cons (car prazanRed) (puniCrnim (cdr prazanRed ))))
)
	)


(defun puniBelim (prazanRed )

(cond  ((null prazanRed) '())

	((equal (car prazanRed) '(".")) (cons '("O") (puniBelim (cdr prazanRed) )))


        (t  (cons (car prazanRed) (puniBelim (cdr prazanRed) )))
)
	)


(defun inicijalizuj (praznaTabla)

(let ((prviRed (car praznaTabla)) (zadnjiRed (car (reverse praznaTabla))))

	      (reverse (cons zadnjiRed (reverse (cons prviRed (napuniTablu (reverse (cdr (reverse (cdr praznaTabla)))))))))) ;sklanja prvi i poslednji red jer se tu ne stavljaju zetoni

)


(defun inicijalnoStanje (tabla velicinaTable)

		(list tabla "X" '(0 0) velicinaTable)

	)



(defun proveriCiljnoStanje (stanje velicinaTable);;ako je pobedio x vraca "X" isto i za "O" 

		
(if  (= velicinaTable 8) (if (= (caaddr stanje) 2) "X"  (if (= (car(cdaddr stanje)) 2) "O" '()    ))

	(if (= (caaddr stanje) 3) "X"  (if (= (car(cdaddr stanje)) 3) "O" '()    ))

	)

	

)

;(print (proveriCiljnoStanje (inicijalnoStanje (inicijalizuj (praviTablu 8 8)  ) 8) 8)) 


(defun zetoniNaTabli (tabla index);poziva se sa index=0   vraca polja na kojima su zetoni ((x y) (x y) ......)

	(if   (null tabla) '()     (append (proveriRed (car tabla) index 0 )  (zetoniNaTabli (cdr tabla) (1+ index))) )

)

(defun proveriRed (red indexReda indexKolone)

(cond  ((null red) '())

	( (or (equal (car red) '("B")) (equal (car red) '(".")  ))   (proveriRed (cdr red) indexReda (1+ indexKolone))           )


        (t  (cons  (cons indexReda (list indexKolone))   (proveriRed (cdr red) indexReda (1+ indexKolone))  ))
)	)



(defun rastojanje (x1 y1 x2 y2 )

(if  (> (abs(- x1 x2 )) (abs(- y1 y2 )))  (abs(- x1 x2 ))      (abs(- y1 y2 ))    )

)


(defun meniNajblizi ( potez pozicijeZetona ); potez je lista npr '(’(x1 y1) ’(x2 y2) ’0) ;;;; fja vraca listu koja sadrzi elemente: '( rastojanje do najblizeg elementa , X i Y najblizeg elementa) jer moze da ima vise najblizih

 (if (null pozicijeZetona) (list(list 100 -1 -1))
(let*   ((trX (caar pozicijeZetona ) ) (trY (cadar pozicijeZetona) ) (  trRastojanje (rastojanje (caar potez) (cadar potez) trX trY ) )   (pozicijaIRastojanjeIzRekurzije (meniNajblizi potez (cdr pozicijeZetona) ))

)
			(cond  ((< trRastojanje (caar pozicijaIRastojanjeIzRekurzije) )   (list(list trRastojanje trX trY))            ) ;mora list list da bi cons radio dole kako treba

	( (= trRastojanje (caar pozicijaIRastojanjeIzRekurzije) )  (cons (list trRastojanje trX trY) pozicijaIRastojanjeIzRekurzije )     )


        (t  pozicijaIRastojanjeIzRekurzije )
))	))



(defun izbaciPotez (pozicijeZetona potez );mora da se izbaci ta pozicija sa koje se prebacuju zetoni

;(if  (equal (car pozicijeZetona) (car potez)) (cdr pozicijeZetona ) (cons (car pozicijeZetona) (izbaciPotez (cdr pozicijeZetona) potez))  )



(cond  ((null pozicijeZetona) '())

	(  (equal (car pozicijeZetona) (car potez)) (cdr pozicijeZetona )         )


        (t  (cons (car pozicijeZetona) (izbaciPotez (cdr pozicijeZetona) potez)))
))



(setq potez '( (1 1) ( 2 0 ) 0 ) )




(defun daLiSePriblizavaNajblizem ( potez pozicijeZetona ); potez je lista npr '(’(x1 y1) ’(x2 y2) ’0)

    (let*   ((najblizePozicije (meniNajblizi potez pozicijeZetona ) ) ;(rastojanje2 (rastojanje (cadr najblizaPozicija) (caddr najblizaPozicija) (caadr potez) (cadadr potez) ) )

)
    (proveriPozicije najblizePozicije (list (caadr potez) (cadadr potez)) )


)


)

(defun proveriPozicije ( najbliziZetoni zeljeniPotez )


		(cond  ((null najbliziZetoni) '())

			( (<= (rastojanje (car zeljeniPotez) (cadr zeljeniPotez) (cadar najbliziZetoni) (caddar najbliziZetoni) ) (caar najbliziZetoni) )   t  )


        (t  (proveriPozicije (cdr najbliziZetoni) zeljeniPotez))
        )
)


;da se obradi slucaj kada se pomera na taj najblizi tipa pomera se sa 1 1 na 2 2 !!!!!!!!!!!!
 ;(print (daLiSePriblizavaNajblizem potez  (izbaciPotez (zetoniNaTabli (inicijalizuj (praviTablu 8 8) ) 0)  potez )))


(defun crtajElemente (polje brVrste brReda i)
( cond ( (< (length polje)  (- (* (- 4 brReda) 3) 2))    (list ". . .")     )
			 ( (< (length polje)  (- (* (- 4 brReda) 3) 1)) (list ". ." (nth (- (* (- 4 brReda) 3) 3) polje ))   )
			 ( (< (length polje)  (* (- 4 brReda) 3) ) (list "."  (nth (- (* (- 4 brReda) 3) 2) polje ) (nth (- (* (- 4 brReda) 3) 3)polje ))    )
			(  t (list   (nth (- (* (- 4 brReda) 3) 1) polje ) (nth (- (* (- 4 brReda) 3) 2)polje )    (nth (- (* (- 4 brReda) 3) 3)polje )      ) )

)

)


 (defun crtajVrstu (vrsta brVrste brReda i)
 ( cond ( (null vrsta)  nil)
				(  (/= (mod i 2)  (mod brVrste 2)) (append (list "    ")  (crtajVrstu (cdr vrsta) brVrste  brReda (1+ i)  )     )   )
				(  t  (append (crtajElemente (car vrsta) brVrste brReda (1+ i) )  (crtajVrstu (cdr vrsta) brVrste  brReda (1+ i)  )     ))

 )

)



(defun CrtajMatrica (matrica brVrste)
 ( cond ( (null matrica) '())
 				( t

						(format t "~{~a~^ ~}"	( crtajVrstu (car matrica) brVrste 1 1))
						(terpri)
						(format t "~{~a~^ ~}"		(crtajVrstu (car matrica) brVrste 2 1))
						(terpri)
						(format t "~{~a~^ ~}"			(crtajVrstu (car matrica) brVrste 3 1))
						(terpri)

						(CrtajMatrica (cdr matrica) (1+ brVrste))
					   )
)
)
(print (CrtajMatrica  (inicijalizuj (praviTablu 8 8)  )  1))
