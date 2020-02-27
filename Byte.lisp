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



;(setq potez '( (1 1) ( 2 0 ) 0 ) )




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
   nil
 )
)




(defun pomeriStekNaStek (x y x1 y1 visina stanje)

	(if (equalp (nth visina (vratiPolje x y stanje)) (caadr  stanje) )

		(
			cond ( (and (< visina (length (vratiPolje x1 y1 stanje))) ( <= (+ (- (length (vratiPolje x y stanje)) visina)  (length (vratiPolje x1 y1 stanje)) )  8)) (pomeriDiskove x y x1 y1 visina stanje))
			
           ( t nil )
		
		)

		nil
	)

)


(defun proveriValidnoVisinuPriblizavanje (x y x1 y1 visina stanje)
    
    ( let* ((potez1  (list  (list x y) (list x1 y1)  visina )))
    (if    (daLiSePriblizavaNajblizem potez1 (izbaciPotez (zetoniNaTabli (car stanje) 0)  potez1 ))
    	(  cond ( (and(>=(length (vratiPolje x y stanje)) 1) ( equalp (length (vratiPolje x1 y1 stanje)) 0 ) (equalp visina 0) )  (pomeriStekNaPrazno x y x1 y1 visina stanje)   ) 
    			( (and (>=(length (vratiPolje x y stanje)) 1) (>=(length (vratiPolje x1 y1 stanje)) 1)  ) (pomeriStekNaStek x y x1 y1 visina stanje) )
    	)

  nil
    	
    ))


)

( defun validanPotez1 ( x y x1 y1 visina stanje) 
  
  (let* ((n (cadddr stanje)))
  ( if( proveraCrnoPolje x y )
   
   (
   cond (  ( and (equalp x1  (- x 1)) (equalp y1 (- y 1)) (>= x1 0 ) (>= y1 0) ) (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje))
         ( (and (equalp x1 (- x 1) )  (equalp y1 (+ y 1) ) (>= x1 0) (< y1 n)) (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje) )
         ( (and (equalp x1 (+ x 1))  (equalp y1 (- y 1)) (< x1  n ) (>= y1 0)  ) (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje))
         ( (and (equalp x1 (+ x 1)) (equalp y1 (+ y 1)) (< x1  n) (< y1 n) )  (proveriValidnoVisinuPriblizavanje x y x1 y1 visina stanje) )
         ( t nil ) 
    
   )

   nil
  ))

 )
 
 
 
 (defun validanPotez (potez stanje)
  (validanPotez1 (caar potez)  (cadar potez)  (caadr potez) (cadadr potez) (caddr potez) stanje)
)


 
 ;(crtajMatricu stanje)



(defun generisiMogucaStanja (stanje)

 ( vratiMogucaStanja ( potencijalniMoguciPotezi stanje (car stanje) 0) stanje )

)

(defun vratiMogucaStanja (listaCvorova stanje)

(  
  cond ((null listaCvorova) '())
       (t ( append (generisiStanjaZaToPolje (car listaCvorova) stanje) (vratiMogucaStanja (cdr listaCvorova) stanje) ))
)

)




(defun generisiStanjaZaToPolje (polje stanje) 

      (let* (  (visinaPoljaSaKogSePomera  (length (vratiPolje (car polje) (cadr polje) stanje )) )  (x (car polje)) (y (cadr polje))
      		(n (cadddr stanje))

      	)
          (append 
          	(validnaPozicijaNaKojuMozeDaSePremesti  x y (- x 1) (- y 1) n visinaPoljaSaKogSePomera stanje )
          	(validnaPozicijaNaKojuMozeDaSePremesti  x y (- x 1) (+ y 1) n visinaPoljaSaKogSePomera stanje )
          	(validnaPozicijaNaKojuMozeDaSePremesti  x y (+ x 1) (- y 1)  n visinaPoljaSaKogSePomera stanje )
          	(validnaPozicijaNaKojuMozeDaSePremesti  x y (+ x 1) (+ y 1)   n visinaPoljaSaKogSePomera stanje )
          )


      )
)



;; pomocna fja
(defun validnaPozicijaNaKojuMozeDaSePremesti (x y x1 y1 n visinaPoljaSaKogSePomera stanje)

		(if (and (> x1 -1) (< x1 n)  (> y1 -1) (< y1 n) ) 
			(generisiStanja1 x y x1 y1 0 (length (vratiPolje x1 y1 stanje)) visinaPoljaSaKogSePomera stanje)
			'()

		)
	
)








(defun generisiStanja1 (x y x1 y1 trenutnaVisina visinaNaKojuSeIde visinaSaKojePomera  stanje)

  ( cond (  (or (and (>= trenutnaVisina visinaNaKojuSeIde ) (or (not(eq trenutnaVisina 0) ) (not (eq visinaNaKojuSeIde 0) )))  (>= trenutnaVisina visinaSaKojePomera) ) '() )
         (t 


      (let* (  (vracenoStanje  (validanPotez1 x y x1 y1 trenutnaVisina stanje) )  

      		
      	)
      	

      (if (equalp vracenoStanje nil) ( append  vracenoStanje (generisiStanja1 x y x1 y1 ( + trenutnaVisina 1) visinaNaKojuSeIde visinaSaKojePomera stanje) ) 

      	( cons  vracenoStanje (generisiStanja1 x y x1 y1 ( + trenutnaVisina 1) visinaNaKojuSeIde visinaSaKojePomera stanje) )
       )

          
      )


          )

  )
)


 (defun potencijalniMoguciPotezi (stanje tabla indexVrste)
 	;;vraca listu polja ((i j) (i j) ..) koja sadrze zetone koje je moguce odigrati ako prodju proveru za validan potez
 	;;indexVrste inicijalno je nula
  (cond ((null (car stanje)) '())
  	((append (obradiVrstu (caar stanje) stanje tabla indexVrste 0 ) (potencijalniMoguciPotezi (cons (cdar stanje) (cdr stanje)) tabla (1+ indexVrste))))

  	)

  	

)

 (defun obradiVrstu (vrsta stanje tabla indexVrste indexKolone)
 
  (cond ((null  vrsta) '())
  	( t (append (proveriPolje (car vrsta) stanje tabla indexVrste indexKolone) (obradiVrstu (cdr vrsta) stanje tabla indexVrste (1+ indexKolone)) ))

  	)

 )

(defun proveriPolje ( stek stanje tabla indexVrste indexKolone)

	  (cond ;( (not(equalp (car stek) (caadr stanje ))) '())
  	( (and (equalp (car stek) (caadr stanje )) (proveriOkoloDaLiJePrazno indexVrste indexKolone tabla))  (list(list indexVrste indexKolone)))
  	;;ako je skroz dole odgovrajuci i oko njega je prazno onda je okej taj
  	( (and (postojiOdgovarajuciZeton stek (caadr stanje )) (not(proveriOkoloDaLiJePrazno indexVrste indexKolone tabla)) )  (list (list indexVrste indexKolone)))
  	;;ima u steku zeton igraca koji je na potezu i oko njega je stek
  	(t '())
  	)


	)

(defun proveriOkoloDaLiJePrazno (indexVrste indexKolone tabla)

	 (cond ((and(equalp indexVrste 0) (equalp indexKolone 0)) (proveraOkolo (list (list 1 1)) tabla))
  	(  (and(equalp indexVrste (1-(length tabla))) (equalp indexKolone (1-(length tabla)))) (proveraOkolo (list (list  (- (length tabla ) 2) (- (length tabla ) 2))) tabla))
  		
  		((equalp indexVrste 0) (proveraOkolo (list (list  1  (- indexKolone 1))    (list  1  (+ indexKolone 1))  ) tabla)  )
  		
  		((equalp indexVrste (1-(length tabla)) )  (proveraOkolo (list (list  (-(length tabla) 2)  (- indexKolone 1))    (list  (-(length tabla) 2)   (+ indexKolone 1))  ) tabla)  )
  		
  		((equalp indexKolone 0) (proveraOkolo (list (list  (- indexVrste 1)  1)    (list  (+ indexVrste 1)  1)  ) tabla)  )

  		((equalp indexKolone (1-(length tabla)) ) (proveraOkolo (list (list  (- indexVrste 1) (-(length tabla) 2)  )    (list  (+ indexVrste 1)  (-(length tabla) 2) )  ) tabla)  )

  		(t (proveraOkolo (list (list  (- indexVrste 1) (- indexKolone 1)  )    (list  (- indexVrste 1)  (+ indexKolone 1) )  
  		(list  (+ indexVrste 1)  (- indexKolone 1) ) (list  (+ indexVrste 1)  (+ indexKolone 1) ) ) tabla)   )


  	)

	)

(defun proveraOkolo (listaSuseda tabla)
	(cond ((null listaSuseda) t  )
		( (equalp (length ( nth (cadar listaSuseda) ( nth (caar listaSuseda ) tabla)) ) 0) (proveraOkolo (cdr listaSuseda) tabla) )
		(t '()) 
	)

 )

(defun postojiOdgovarajuciZeton (stek iksIliOks)
	
	(cond ((null stek) '()  )
		( (equalp (car stek) iksIliOks) t )
		(t  (postojiOdgovarajuciZeton (cdr stek) iksIliOks)) 
	)
)


(defun promeniIgraca (stanje)
	(list (car stanje)  (if (equalp (caadr stanje) "X")  (list "O" (mod (+ (cadadr stanje) 1 ) 2))  (list "X" (mod (+ (cadadr stanje) 1 ) 2))) (caddr stanje) (cadddr stanje))
)

(defun unesiPotez (stanje)
	(progn (format t "~% Unesite potez :")


		(let* ((prvoSlovo (read-line) ))

			(if(equalp prvoSlovo "n" )   (promeniIgraca stanje)   
		(
			progn
		(read-char)
		(read-char ) 
		(read-char ) 


	      		(let* ((potez1x  (read) ) (read-char )  (potez1y  (read) )  )   
	      		(read-char )
	      		(read-char )
	      		(read-char )
	      		(read-char )

	      	(let*  (  (potez2x  (read) )   (read-char )  (potez2y  (read) )  )

	      				(read-char )



	       			(let* ((visina (read)))  
	       					(read-char )
	       					(read-line)
	       				;(print (list potez1x potez1y))
	       		;(print (list potez2x  potez2y ))
	       			;(print visina)
	       			;(print (proveriDaLiUnosKorektan  potez1x potez1y ))
	       			;(print (proveriDaLiUnosKorektan  potez2x potez2y ))

	       			(let* ((polje1 (proveriDaLiUnosKorektan potez1x potez1y)) (polje2 (proveriDaLiUnosKorektan potez2x potez2y))    )

	       					(if (or (equalp polje1 nil) (equalp polje2 nil)) 

	       						(progn
	       						 (format t "Uneli ste nevalidan potez" )
	       						 (unesiPotez stanje)
	       						 )

	       						(validanPotez (list polje1 polje2 visina) stanje)

	       					  )


	       				)

	       				)

	       		)
	       	 )
	      	)
		)

)
)
)



(defun proveriDaLiUnosKorektan (poljex poljey)
 	
 	(cond ( (string= poljex  "A") ( list 0  (-  poljey 1) ))
 		  ( (string= poljex "B") ( list 1  (-  poljey 1)  ))
 		  ( (string= poljex "C")  ( list 2  (-  poljey 1) ))
 		  ( (string= poljex "D") ( list 3 (-  poljey 1) ))
 		  ( (string= poljex "E") ( list 4 (-  poljey 1) ))
 		  ( (string= poljex "F") ( list 5 (-  poljey 1) ))
 		  ( (string= poljex "G") ( list 6 (-  poljey 1) ))
 		  ( (string= poljex "H") ( list 7 (-  poljey 1) ))
 		  ( (string= poljex "I") ( list 8 (-  poljey 1) ))
 		  ( (string= poljex "J") ( list 9 (-  poljey 1) ))
 		  (t nil)
 		  )
)

;(proveriDaLiUnosKorektan (list A 5))
;(setq stanje (inicijalnoStanje (inicijalizuj (praviTablu 8 8)  ) 8) )

;(unesiPotez stanje)

(defun unetiVelicinuTable () 
	(
		progn (format t "~% Unesite velicinaTable :")
		(let* ((velicinaTable (read))    )
         (inicijalnoStanje (inicijalizuj (praviTablu velicinaTable velicinaTable)  ) velicinaTable)

		)

	)

)




(defun main ()
(let* ((stanje (unetiVelicinuTable) )     )
	    ( crtajMatricu stanje)
		(igraj  stanje  (car (reverse stanje)) 1 0 )

)
)




(defun odigrajIProveriValidan (stanje)

	(let* ((novoStanje (unesiPotez stanje) ))

		(if (equalp novoStanje nil)

			( progn (format t "Uneli ste nevalidan potez" ) stanje )
			 novoStanje


		)
	)
)

(defun vratiPomDubinu (brojPoteza n brXIbrY) (
   cond ((and (= n 8) (< brojPoteza  4)) 2)
		((and (= n 8)(< brojPoteza 6) ) 3)

		( (and (= n 8) (= (+ (car brXIbrY) (cadr brXIbrY) ) 0) ) 3)
		((and (= n 8)(< brojPoteza 9) ) 4)

		((and (= n 8) (= (+ (car brXIbrY) (cadr brXIbrY) ) 1) ) 4)

		((and (= n 8)(< brojPoteza 12) ) 5)
		;-----------------------------------------------------------------------
		((and (= n 10) (< brojPoteza 12)) 2)
		((and (= n 10)(< brojPoteza 17) ) 3)
		((and (= n 10) (<= (+ (car brXIbrY) (cadr brXIbrY) ) 1) ) 3)
		

		((and (= n 10)(< brojPoteza 20) ) 4)
		((and (= n 10) (<= (+ (car brXIbrY) (cadr brXIbrY) ) 3) ) 4)
		((and (= n 10)(< brojPoteza 26) ) 5)
		(t 6)

) )





(defun igraj (stanje velicinaTable dubina brPoteza)

	( 		cond ( (not (equal (proveriCiljnoStanje stanje velicinaTable) nil )) (format t "Pobednik je: ~a" (proveriCiljnoStanje stanje velicinaTable) ) )
	
	( t 

	(if (equalp(cadadr stanje) 0)

		(
			progn 
			(format t "Igra racunar")
			(let* ((dubina (vratiPomDubinu brPoteza velicinaTable (caddr stanje)))   (vracenoIzMinMax (MaxPotez stanje -50 50   dubina dubina )) (stanjeNovo (car vracenoIzMinMax) ));;da se proveri sta treba za alfu i za betu inicijalno
				;(print "vracena heuristika iz minMax")
				;(print(cadr vracenoIzMinMax))
				;(print dubina)
				;(print brPoteza)
				(crtajMatricu stanjeNovo)
				(if (equalp (cadadr stanjeNovo) 0 )
				(igraj (promeniIgraca stanje) (cadddr stanje) dubina  brPoteza )
				(igraj stanjeNovo velicinaTable dubina (+ brPoteza 1))
				)
				)

		
			)

		 (

			progn 
			(format t "Igra covek")

             (let* ((stanjeNovo (odigrajIProveriValidan stanje)))

				(crtajMatricu stanjeNovo)
				(igraj stanjeNovo velicinaTable dubina brPoteza)

			)
		


		 )	
	)

     )

	)
)

 ;proveriCiljnoStanje (stanje velicinaTable)
;(trace main)
;(main )

(defun heuristikaStanja (stanje dubinapom) (
let* ((procenaStanja (donesiZakljucak stanje))  )
	;(
	;	progn
	;		(print procenaStanja)
	;		(print dubinapom)
	;		(print stanje)
   ( cond ((=(mod dubinapom 2) 0) procenaStanja )
         ((=(mod dubinapom 2) 1) (- 0 procenaStanja))
         (t 0)
     
	)
   )
	
)
;)

;(untrace heuristikaStanja)
(defun MaxPotez (stanje alfa beta dubina dubinapom)
  (let* ((novaStanja  (generisiMogucaStanja stanje)) (V -50));;-50 simulira -beskonacno

      (cond ( (or (zerop dubina) (null novaStanja)) (list stanje (heuristikaStanja stanje dubinapom)) )
          
          

          (t (MaxPomPetlja V novaStanja alfa beta dubina nil dubinapom )

      )
  ) )
 )

(defun MaxPomPetlja ( V novaStanja alfa beta dubina stanjeHeuristikaNajbolje dubinapom)
      (if (null novaStanja) stanjeHeuristikaNajbolje
      (let* ((V1 (cadr(MinPotez (car novaStanja) alfa beta (1- dubina ) dubinapom )))  )
      	


  ( cond ((and (>= V1 beta ) (> V1 V )) (list (car stanjeHeuristikaNajbolje) V1) )
         ((>= V1 beta ) (list (car stanjeHeuristikaNajbolje) V))
		 ((and (> V1 alfa )  (> V1 V)) (MaxPomPetlja V1 (cdr novaStanja) V1 beta dubina (list (car novaStanja) V1) dubinapom))
		 ((> V1 alfa ) (MaxPomPetlja V (cdr novaStanja) V1 beta dubina stanjeHeuristikaNajbolje dubinapom))
		 ((> V1 V ) (MaxPomPetlja V1 (cdr novaStanja) alfa beta dubina (list (car novaStanja) V1) dubinapom))
		 (t (MaxPomPetlja V (cdr novaStanja) alfa beta dubina stanjeHeuristikaNajbolje dubinapom))
     
	)

        
       ; (if (and (>= V1 beta ) (> V1 V )) (list (car stanjeHeuristikaNajbolje) V1);;odsecanje
        	;(if (>= V1 beta ) (list (car stanjeHeuristikaNajbolje) V)
       ;(if (and (> V1 alfa )  (> V1 V)) (MaxPomPetlja V1 (cdr novaStanja) V1 beta dubina (list (car novaStanja) V1) dubinapom) 
        	;(if (> V1 alfa )

        	;(MaxPomPetlja V (cdr novaStanja) V1 beta dubina stanjeHeuristikaNajbolje dubinapom)

        		;(if (> V1 V )
        			;(MaxPomPetlja V1 (cdr novaStanja) alfa beta dubina (list (car novaStanja) V1) dubinapom)
        		;(MaxPomPetlja V (cdr novaStanja) alfa beta dubina stanjeHeuristikaNajbolje dubinapom)
        	;)
        

       
       ; ))))
       )

  ))



(defun MinPotez (stanje alfa beta dubina dubinapom)
  (let* ((novaStanja  (generisiMogucaStanja stanje)) (V 50));;50 simulira beskonacno

      (cond ( (or (zerop dubina) (null novaStanja))  (list stanje (heuristikaStanja stanje dubinapom)) )
          
          

          (t (MinPomPetlja V novaStanja alfa beta dubina nil dubinapom )


      )
  ) )
 )

(defun MinPomPetlja ( V novaStanja alfa beta dubina stanjeHeuristikaNajbolje dubinapom)
       (if (null novaStanja) stanjeHeuristikaNajbolje
      (let* ((V1 (cadr(MaxPotez (car novaStanja) alfa beta (1- dubina ) dubinapom )))  )
      	
        
     ( cond ( (and (<= V1 alfa ) (< V1 V ) ) (list (car stanjeHeuristikaNajbolje) V1) )
         ((<= V1 alfa ) (list (car stanjeHeuristikaNajbolje) V))
		 ((and (< V1 V )  (< V1 beta)) (MinPomPetlja V1 (cdr novaStanja) alfa V1 dubina (list (car novaStanja) V1) dubinapom) )
		 ((< V1 beta )

        	(MinPomPetlja V (cdr novaStanja) alfa V1 dubina stanjeHeuristikaNajbolje dubinapom))
		 ((< V1 V )
        			(MinPomPetlja V1 (cdr novaStanja) alfa beta dubina (list (car novaStanja) V1) dubinapom))
		 
         (t (MinPomPetlja V (cdr novaStanja) alfa beta dubina stanjeHeuristikaNajbolje dubinapom))
     
	)


        ;(if (and (<= V1 alfa ) (< V1 V ) ) (list (car stanjeHeuristikaNajbolje) V1);;odsecanje
        	;(if (<= V1 alfa ) (list (car stanjeHeuristikaNajbolje) V)
       ;(if (and (< V1 V )  (< V1 beta)) (MinPomPetlja V1 (cdr novaStanja) alfa V1 dubina (list (car novaStanja) V1) dubinapom) 
        	;(if (< V1 beta )

        	;(MinPomPetlja V (cdr novaStanja) alfa V1 dubina stanjeHeuristikaNajbolje dubinapom)

        	;	(if (< V1 V )
        		;	(MinPomPetlja V1 (cdr novaStanja) alfa beta dubina (list (car novaStanja) V1) dubinapom)
        		;(MinPomPetlja V (cdr novaStanja) alfa beta dubina stanjeHeuristikaNajbolje dubinapom)
        	;)
        

       
        ;)))))
        )
))
;(trace MinPomPetlja)


(defun igraSeNaStek (stanje)
	(
		let  ((brojN (cadddr stanje) ))
		(list 'IgraSeNa (list 'quote  brojN))
	)
)

(defun trenutnoStekovaX (stanje)
	(
		let  ((brojX (caaddr stanje) ))
		 (list 'TrenutnostekovaX (list 'quote brojX))
	)
)

(defun trenutnoStekovaY (stanje)
	(
		let  ((brojY (cadr(nth 2 stanje)) ))
		 (list 'TrenutnostekovaY (list 'quote brojY))
	)
)

(defun StekXY (tabla i);;;tj (car stanje)  poziva se za i=0
			(if (null tabla ) '() 
			;;else
			(append (obradiRed (car tabla) i 0) (StekXY (cdr tabla) (1+ i)))


			)

)

(defun obradiRed (red i j)
			
		(cond ( (null red ) '() )
 		  ( (equalp (car red) '("B") ) (obradiRed (cdr red) i (1+ j)))
 		  ( (equalp (car red) '(".") ) (cons ( list 'Stek (list 'quote '()) i j )(obradiRed (cdr red) i (1+ j))))
 		  ( t (cons ( list 'Stek (list 'quote (car red)) i j )(obradiRed (cdr red) i (1+ j))))
 		  )
)

;(print (trenutnoStekovaY stanje))

;(print (igraSeNaStek stanje))
;(trace MaxPotez)
;(trace MinPotez)
;(trace MaxPomPetlja)
;(trace MinPomPetlja)
;(trace heuristikaStanja)


;alfa je najboja alternativa za max igraca
;beta je najboja alternativa za min igraca


(load ".\\Inference_engine.cl")
;
(defun !eq (a b)
  (equal a b))
(defun !noteq (a b)
  (not (equal a b)))
(defun !gt (a b)
  (> a b))
(defun !lt (a b)
  (< a b))

(defun !ne (a b)
  (not (equal a b)))
(defun !visina (a b)
(eq (length a) b)
  )
(defun !potez (a b c k)
(and (eq (abs(- a c)) 1) (eq (abs (- b k)) 1))
  )

(defun !nti (a b n)
	(cond ((null a) '())
		(t  (equalp b (nth (- (length a) n) a) )))
)

()
(defun !visinaVecaOd (a b)
(> (length a) b)
  )
(defun !postojiPodstekOsnoveLVisineN (a l n)
(equalp l (nth (- n 1) a))
  )
(defun !vrhOsnoveL (a l )
(equalp l (car a))
  )
(defun !vodi (a b )
(> a b)
  )
(defun !diffOne (a b )
(eq (- a 1) b)
  )
(defun !diffTwo (a b )
(eq (- a 2) b)
  )
(defun first(a b)
(equalp (car a) b
	))

(defparameter *T1-RULES* '(
	
    (if (and (STEK ?x ?y ?z) (!visina ?x 1)) then (PostojiZeton ?x))

    (if (and (PostojiZeton ?x) (!nti ?x ?y 1)) then (PostojiZetonBoje ?y))

    (if (and  (STEK ?x ?y ?z) (!nti ?x ?n 1)) then (PostojiZetonDole ?n))

      (if (and (Stek ?x ?y ?z) (!visina ?x '5) (Stek ?j ?m ?k) (!visina ?j '3) (!potez ?y ?z ?m ?k) (!vrhOsnoveL ?j ?b )) then (PostojiPetITriDirektno ?x ?j ?b))

    (if (and (Stek ?x ?y ?z) (!visina ?x '6) (Stek ?j ?m ?k) (!visina ?j '2) (!potez ?y ?z ?m ?k) (!vrhOsnoveL ?j ?b )) then (PostojiSestIDvaDirektno ?x ?j ?b))

    (if (and (Stek ?x ?y ?z) (!visina ?x '4) (Stek ?j ?m ?k) (!visina ?j '4) (!potez ?y ?z ?m ?k) (!vrhOsnoveL ?j ?b )) then (PostojiCetiriICetiriDirektno ?x ?j ?b))

    (if (and (Stek ?x ?y ?z) (!visina ?x '7) (Stek ?j ?m ?k) (!visina ?j '1) (!potez ?y ?z ?m ?k) (!vrhOsnoveL ?j ?b ) ) then (PostojiSedamIJedanDirektno ?x ?j ?b))

    (if (and (STEK ?x ?y ?z) (!visina ?x 5) (STEK ?j ?m ?k) (!visinaVecaOd ?j 3) (!potez ?y ?z ?m ?k) (!postojiPodstekOsnoveLVisineN ?j ?b 3 ) (!vrhOsnoveL ?j ?b ) ) then (PostojiPetITriDaSePrebaci ?x ?j ?b))

    (if (and (STEK ?x ?y ?z) (!visina ?x 6) (STEK ?j ?m ?k) (!visinaVecaOd ?j 2) (!potez ?y ?z ?m ?k) (!postojiPodstekOsnoveLVisineN ?j ?b 2 ) (!vrhOsnoveL ?j ?b )) then (PostojiSestIDvaDaSePrebaci ?x ?j ?b))

    (if (and (STEK ?x ?y ?z) (!visina ?x 4) (STEK ?j ?m ?k) (!visinaVecaOd ?j 4) (!potez ?y ?z ?m ?k) (!postojiPodstekOsnoveLVisineN ?j ?b 4 ) (!vrhOsnoveL ?j ?b )) then (PostojiCetiriICetiri ?x ?j ?b))

    (if (and (STEK ?x ?y ?z) (!visina ?x 7) (STEK ?j ?m ?k) (!visinaVecaOd ?j 1) (!potez ?y ?z ?m ?k) (!postojiPodstekOsnoveLVisineN ?j ?b 1 ) (!vrhOsnoveL ?j ?b )) then (PostojiSedamIJedan ?x ?j ?b))

    (if (and (PostojiPetITriDirektno  ?x ?j ?b) (!nti ?j ?b '1)) then (PostojiDirektno ?b))

    (if (and (PostojiSestIDvaDirektno ?x ?j ?b) (!nti ?j ?b '1)) then (PostojiDirektno ?b))

    (if (and (PostojiCetiriICetiriDirektno ?x ?j ?b) (!nti ?j ?b '1)) then (PostojiDirektno ?b))

    (if (and (PostojiSedamIJedanDirektno ?x ?j ?b) (!nti ?j ?b '1)) then (PostojiDirektno ?b))

    (if  (PostojiPetITriDaSePrebaci ?x ?j ?b)  then (PostojiIndirektno ?b))

    (if  (PostojiSestIDvaDaSePrebaci ?x ?j ?b)  then (PostojiIndirektno ?b))

    (if  (PostojiCetiriICetiri ?x ?j ?b)  then (PostojiIndirektno ?b))

    (if  (PostojiSedamIJedan ?x ?j ?b)  then (PostojiIndirektno ?b))

   (if (and  (TRENUTNOSTEKOVAX ?x) (TRENUTNOSTEKOVAY ?y) (!eq ?x ?y) ) then (Izjednaceno 1 ))

    (if (Izjednaceno ?x ) then (Oceni ?x "X"))

    (if (Izjednaceno ?x ) then (Oceni ?x "O"))

   
    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(!eq '3 ?y) )then (PobedaYD 10))

    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(!eq '3 ?y) )then (GubiXD -10))


    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAX ?y)(!eq '3 ?y) )then (PobedaXD 10))

    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAX ?y)(!eq '3 ?y) )then (GubiYD -10))


    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAY ?y)(!eq '2 ?y) )then (PobedaYO 10))

    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAY ?y)(!eq '2 ?y) )then (GubiXO -10))


    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAX ?y)(!eq '2 ?y) )then (PobedaXO 10))

    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAX ?y)(!eq '2 ?y) )then (GubiYO -10))


    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!gt ?x ?y) (!noteq ?x '2) )then (VodiXO 5))

    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!gt ?x ?y) (!noteq ?x '2) )then (GubiYO -5))


    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!gt ?y ?x) (!noteq ?y '2))then (VodiYO 5))

    (if (and (IGRASENA '8)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!gt ?y ?x) (!noteq ?y '2))then (GubiXO -5))



    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffOne ?y ?x) (!noteq ?y '3) )then (VodiYD 6))

    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffOne ?y ?x) (!noteq ?y '3) )then (GubiXD -6))



    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffOne ?x ?y) (!noteq ?x '3) )then (VodiXD 6))

     (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffOne ?x ?y) (!noteq ?x '3) )then (GubiYD -6))



    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffTwo ?y ?x) (!noteq ?y '3) )then (VodiYD 8))

    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffTwo ?y ?x) (!noteq ?y '3) )then (GubiXD -8))



    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffTwo ?x ?y) (!noteq ?x '3) )then (VodiXD 8))

    (if (and (IGRASENA '10)(TRENUTNOSTEKOVAY ?y)(TRENUTNOSTEKOVAX ?x)(!diffTwo ?x ?y) (!noteq ?x '3) )then (GubiYD -8))



    (if (PobedaXO ?x) then (Oceni ?x "X"))

    (if (GubiYO ?x) then (Oceni ?x "O"))

    (if (PobedaYO ?x) then (Oceni ?x "O"))

    (if (GubiXO ?x) then (Oceni ?x "X"))



    (if (VodiXO ?x) then (Oceni ?x "X"))

    (if (VodiYO ?x) then (Oceni ?x "O"))

    (if (VodiXD ?x) then (Oceni ?x "X"))

    (if (VodiYD ?x) then (Oceni ?x "O"))


    (if (PobedaXD ?x) then (Oceni ?x "X"))

    (if (GubiYD ?x) then (Oceni ?x "O"))

    (if (PobedaYD ?x) then (Oceni ?x "O"))

    (if (GubiXD ?x) then (Oceni ?x "X"))



))
	



                
(defun stanjeUCinjenice (stanje)
	
  (progn
 ( setq  *T1-FACTS*  (append  (list (igraSeNaStek stanje) (trenutnoStekovaX stanje) (trenutnoStekovaY stanje) ) (StekXY (car stanje) 0)  ) )
 	(prepare-knowledge *T1-RULES* *T1-FACTS* 10)
 
 )

)
(defun suprotniZeton(x)
  (cond ((equalp x "X") "O") (t "X"))
	)


(defun donesiZakljucak (stanje)
	(
		progn
		(stanjeUCinjenice stanje)
   
		(let* ( 
      (pom (caadr stanje))
			   ( potez (list 'Oceni '?x (caadr stanje)) )
			   (potez1 (list 'PostojiZetonBoje (caadr stanje)) )
			   (potez2 (list 'PostojiZetonDole (caadr stanje)) )
         (potez3 (list 'PostojiIndirektno (caadr stanje)) )
         (potez4 (list 'PostojiDirektno (caadr stanje)) )
         (potez5 (list 'PostojiIndirektno (suprotniZeton (caadr stanje))) )
         (potez6 (list 'PostojiDirektno (suprotniZeton(caadr stanje)) ))

			    (ocenaTrenutnihStekova (cadaar(infer potez)) )
				(postojiZetonBoje (count-results potez1))
				(postojiZetonDole (count-results potez2))
        (postojiIndirektno (count-results potez3))
          (postojiDirektno (count-results potez4))
          (postojiIndirektnoSuprotno (count-results potez5))
          (postojiDirektnoSuprotno (count-results potez6))

			
			
			)
		(- (+  (* ocenaTrenutnihStekova 0.1) (* postojiZetonDole 0.01) (* postojiZetonBoje 0.01) (* postojiDirektno 0.2) (* postojiIndirektno 0.2)) (* postojiIndirektnoSuprotno 0.4)(* postojiDirektnoSuprotno 0.4))
  

		)
		
	)

)



(main)
