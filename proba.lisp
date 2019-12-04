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


 
(defun inicijalnoStanje (tabla velicinaTable )
		
		(princ "Unesite ko prvi igra, 1-covek 0-kompjuter: ")
		(setq prviIgrac (read))

		
		(list tabla ( list "X" prviIgrac) '(0 0) velicinaTable)

	)
;(print (inicijalnoStanje (inicijalizuj (praviTablu 8 8)  ) 8 ))



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

	( (or (equal (car red) '("B")) (equal (car red) '(".")  )  (equal (car red) '()  ) )   (proveriRed (cdr red) indexReda (1+ indexKolone))           )

;dodato za null stefan
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



(setq listaSlova '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"))

(defun kreirajString ( n q pomeraj tacno)

  (cond ( (< q n) (if (equal tacno t) (concatenate 'string "~" (write-to-string pomeraj) "t~a" (kreirajString n (+ q 1) (+ pomeraj 5) (not tacno)))   

     (concatenate 'string "~" (write-to-string pomeraj) "t~a" (kreirajString n (+ q 1) (+ pomeraj 6 ) (not tacno))) 
    ))

  (t (concatenate 'string "~" (write-to-string (+ pomeraj )) "t~a"))
)
)


(defun crtajMatricaPom (matrica brVrste)
  
 ( cond ( (null matrica) (terpri))
 				( t

						(format t "~5T~{~a~^ ~}"	( crtajVrstu (car matrica) brVrste 1 1))
						(terpri)
						(format t "~1T~a~5t~{~a~^ ~}"	(nth (- brVrste 1) listaSlova)	(crtajVrstu (car matrica) brVrste 2 1))
						(terpri)
						(format t "~5T~{~a~^ ~}"			(crtajVrstu (car matrica) brVrste 3 1))
						(terpri)

						(crtajMatricaPom (cdr matrica) (1+ brVrste))
					   )
)
)


(defun crtajMatricu (stanje) 
 
  (progn
     (terpri)
    (format t (kreirajString (cadddr stanje) 1 7 t) 1 2 3 4 5 6 7 8 9 10)
    (terpri)
     (terpri)
     (CrtajMatricaPom (car stanje)  1)
    )
  )


(defun proveraCrnoPolje ( x y)
   (
   		cond ( (and (equalp(mod x 2) 0) (equalp (mod y 2) 0)) t )
   		     ( (and (equalp(mod x 2) 1 ) (equalp (mod y 2) 1)) t)
   		     ( t nil)
   )
)


(defun vratiPolje (x y stanje)

	 ( nth y (nth x (car stanje)))
)

;0-ubacuje
;1 nadovezuje

(defun ubaciNadoveziElementUVrstu (vrsta y diskovi rezim)

  (cond ((null vrsta) '())
      ((equalp y 0) ( if (equalp rezim 0) (cons diskovi (cdr vrsta)) (  cons (append (car vrsta) diskovi) (cdr vrsta) ) ))

      (t (cons (car vrsta) (ubaciNadoveziElementUVrstu (cdr vrsta) (- y 1) diskovi rezim)))
  )

)






  (defun ubaciUMatricu (matrica x y diskovi rezim)

     (cond ((null matrica) '())
         ((equalp x 0) ( if (equalp rezim 0) (cons (ubaciNadoveziElementUVrstu (car matrica) y diskovi 0) (cdr matrica))  (cons (ubaciNadoveziElementUVrstu (car matrica) y diskovi 1) (cdr matrica)) ))
         (t (cons (car matrica) (ubaciUMatricu (cdr matrica) (- x 1) y diskovi rezim )))


     )
  )



(defun izdvojiDiskoveZaPrebacivanje (listaZaPrebacivanje visina)
   
   ( cond ( (null listaZaPrebacivanje) '() )  
   	      ( (> visina 0) (izdvojiDiskoveZaPrebacivanje (cdr listaZaPrebacivanje) (- visina 1) ))
          ( (equal visina 0) ( cons (car listaZaPrebacivanje) (izdvojiDiskoveZaPrebacivanje (cdr listaZaPrebacivanje)  visina)))

   )

)




 (defun izdvojiDiskoveKojiOstaju (lista  visina)

 	(
 		cond ( (> visina 0) (cons (car lista) (izdvojiDiskoveKojiOstaju (cdr lista) (- visina 1))))
 		    (t nil)

 	)
 )
 
 
 
 (defun pomeriDiskove (x y x1 y1 visina stanje)

    (
    	let* ((diskoviOstatak (izdvojiDiskoveKojiOstaju (vratiPolje x y stanje) visina)) (diskoviPrebacivanje (izdvojiDiskoveZaPrebacivanje (vratiPolje x y stanje) visina))

              (matrica (ubaciUMatricu (car stanje) x y diskoviOstatak 0)  )  (sledeciNaPotezu (if (equalp (caadr stanje) "X")  (list "O" (mod (+ (cadadr stanje) 1 ) 2))  (list "X" (mod (+ (cadadr stanje) 1 ) 2)))) )
      

         (if (equalp (+(length (vratiPolje x1 y1 stanje)) (length diskoviPrebacivanje))  8)  

          ( 
            progn
           
             (let* ((matrica2 (ubaciUMatricu matrica x1 y1 '() 0) )  (brojOsvojenih ( if (equalp (car (reverse diskoviPrebacivanje)) "X") (list (+ (nth 0 (nth 2 stanje)) 1) (nth 1 (nth 2 stanje)) )
             ( list (nth 0 (nth 2 stanje))  (+ (nth 1 (nth 2 stanje)) 1) )  )) ) 

                 (list matrica2  sledeciNaPotezu brojOsvojenih (nth 3 stanje)) 

              )


          ) 

          
         (

        
            list (ubaciUMatricu matrica x1 y1 diskoviPrebacivanje 1)   sledeciNaPotezu (nth 2 stanje) (nth 3 stanje)

          )

          )
        
         
    )

)





(defun pomeriStekNaPrazno (x y x1 y1 visina stanje)

 (if (equalp (caadr stanje) (car (vratiPolje x y stanje))) 
 	( progn
   
      (let* ((matrica (ubaciUMatricu (ubaciUMatricu (car stanje) x y '() 0) x1 y1 (vratiPolje x y stanje) 0))

        (sledeciNaPotezu (if (equalp (caadr stanje) "X")  (list "O" (mod (+ (cadadr stanje) 1 ) 2))  (list "X" (mod (+ (cadadr stanje) 1 ) 2)) ) ))

       (list matrica  sledeciNaPotezu (nth 2 stanje) (nth 3 stanje)) 
   )
      )
   stanje
 )
)




(defun pomeriStekNaStek (x y x1 y1 visina stanje)

	(if (equalp (nth visina (vratiPolje x y stanje)) (caadr  stanje) )

		(
			cond ( (and (< visina (length (vratiPolje x1 y1 stanje))) ( <= (+ (- (length (vratiPolje x y stanje)) visina)  (length (vratiPolje x1 y1 stanje)) )  8)) (pomeriDiskove x y x1 y1 visina stanje))
			
           ( t stanje )
		
		)

		stanje
	)

)


(defun proveriValidnoVisinuPriblizavanje (x y x1 y1 visina stanje)
    
    ( let* ((potez1  (list  (list x y) (list x1 y1)  visina )))
    (if    (daLiSePriblizavaNajblizem potez1 (izbaciPotez (zetoniNaTabli (car stanje) 0)  potez1 ))
    	(  cond ( (and(>=(length (vratiPolje x y stanje)) 1) ( equalp (length (vratiPolje x1 y1 stanje)) 0 ) (equalp visina 0) )  (pomeriStekNaPrazno x y x1 y1 visina stanje)   ) 
    			( (and (>=(length (vratiPolje x y stanje)) 1) (>=(length (vratiPolje x1 y1 stanje)) 1)  ) (pomeriStekNaStek x y x1 y1 visina stanje) )
    	)

    stanje
    	
    ))


)

( defun validanPotez1 ( x y x1 y1 visina stanje) 
  
  (let* ((n (cadddr stanje)))
  ( if( proveraCrnoPolje x y )
   
   (
   cond (  ( and (equalp x1  (- x 1)) (equalp y1 (- y 1)) (> x1 0 ) (>= y1 0) ) (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje) ) 
         ( (and (equalp x1 (- x 1) )  (equalp y1 (+ y 1) ) (> x1 0) (< y1 n))  (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje) )
         ( (and (equalp x1 (+ x 1))  (equalp y1 (- y 1)) (< x1 ( - n 1)) (>= y1 0)  )  (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje))
         ( (and (equalp x1 (+ x 1)) (equalp y1 (+ y 1)) (< x1 (- n 1)) (< y1 n) )  (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje) )
         ( t stanje ) 
    
   )

   stanje
  ))

 )
 
 
 
 
 (defun validanPotez (potez stanje)
  (validanPotez1 (caar potez) (- (cadar potez) 1) (caadr potez) (-(cadadr potez)1) (caddr potez) stanje)
)


 (setq stanje (inicijalnoStanje (inicijalizuj (praviTablu 8 8)  ) 8) )
 (crtajMatricu stanje)



(defun generisiPoteze (stanje)

    (
        cons (nadjiDoleOkoloPrazno stanje) (nadjiDaSadrziOkoloPuno stanje)
    )

)

(defun nadjiDoleOkoloPrazno (stanje)

      (
         cond ((null (car stanje)) '())
              ( t ( const) )
      )
)

