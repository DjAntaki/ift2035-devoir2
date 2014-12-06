;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la premi�re section.
;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; �tre d�finie, et vous pouvez ajouter des d�finitions de fonction
;;; afin de bien d�composer le traitement � faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter re�oit en param�tre une liste de caract�res
;;; contenant la requ�te lue et le dictionnaire.  La fonction retourne
;;; une paire contenant la liste de caract�res qui sera imprim�e comme
;;; r�sultat de l'expression entr�e et le nouveau dictionnaire.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "go" qui se charge de cela.
;;;----------------------------------------------------------------------------
(define printligne
	(lambda (i x)
	(begin (display i) (display x)) (newline))
)

(define (exist x) (not (null? x)))

(define assert (lambda (expr . message)
                 (let ((color (if expr "\x1b[32m" "\x1b[31m")) (reset "\x1b[0m"))
                     (display color)
                     (display (if expr "pass" "fail"))
                     (display (if (not (null? message)) (string-append " " (car message)) ""))
                     (display "\n")
                     (display reset))))

(define foldl
  (lambda (f base lst)  
    (if (null? lst)
         base
		 (foldl f (f base (car lst)) (cdr lst)))))

(assert (equal? (foldl + 1 '(1 2 3)) 7) "fold-left an addition")
(assert (equal? (foldl string-append "" '("a" "b" "c")) "abc") "fold-left a string")
(assert (equal? (foldl string-append "" '()) "") "fold-left an empty list")
   
         
;; y'a clairement des endroits ou il va falloir utiliser ca. on la que trop vu souvent en cours    
(define foldr
  (lambda (f base lst)  
    (if (null? lst)
         base
         (f (car lst)
            (foldr f base (cdr lst))))))

(assert (equal? (foldr + 1 '(1 2 3)) 7) "foldr testing" )
(assert (equal? (foldr string-append "" '("a" "b" "c")) "cba") "foldr testing wierd")
(assert (equal? (foldr string-append "" '()) "") "foldr testing")

; Pour utilisation consquente dans le programme, appeler avec str1=cl� recherch�e et str2 noeud actuel
(define (compare str1 str2)
(let ((string1 (list->string str1)) (string2 (list->string str2)))
 (cond
  ((string<? string1 string2) 'left)
  ((string>? string1 string2) 'right)
  ((string=? string1 string2) 'youfoundme)
  (else -1))
)
)

(define node-key cadr)
(define node-definition caddr)
(define node-lchild car)
(define node-rchild cadddr)
(define (node-create key definition lchild rchild) (list lchild key definition rchild))
(define (node-reconstruct x lchild rchild) (list lchild (node-key x) (node-definition x) rchild))

(assert (equal? '() (node-lchild '(() "term" ("def1" "def2") ()))) "get the left child of a node")
(assert (equal? "term" (node-key '(() "term" ("def1" "def2") ()))) "get the term of a node")
(assert (equal? '("def1" "def2") (node-definition '(() "term" ("def1" "def2") ()))) "get the definitions of a node")
(assert (equal? '() (node-rchild '(() "term" ("def1" "def2") ()))) "get the right child of a node")

(define node-insert 
        (lambda (root node) 
                 (let ((cmp (if (null? root) #f (compare (node-key node) (node-key root)))))
                    (cond 
                        ((equal? cmp 'youfoundme) (node-reconstruct node (node-lchild root) (node-rchild root)))
                        ((equal? cmp 'right) (if (null? (node-rchild root))
                                                 (node-reconstruct root (node-lchild root) node) 
                                                 (node-reconstruct root (node-lchild root) (node-insert (node-rchild root) node))))
                        ((equal? cmp 'left)  (if (null? (node-lchild root))
                                                 (node-reconstruct root node (node-rchild root)) 
                                                 (node-reconstruct root (node-insert (node-lchild root) node) (node-rchild root))))
                        (else node)))))

;;;(display (node-insert '() '(() (a b) (d e f i n i t i o n) ())))
                    
(define node-remove
        (lambda (root key) 
                 (let ((cmp (if (null? root) #f (compare key (node-key root)))))
				 (display cmp)
                    (cond 
                        ((equal? cmp 'youfoundme) (if (null? (node-lchild root))
                                                      (node-rchild root)
                                                      (if (null? (node-rchild root))
														   (node-lchild root)
														    (node-insert (node-lchild root) (node-rchild root)))))
                        ((equal? cmp 'right) (if (null? (node-rchild root))
                                                 root 
                                                 (node-reconstruct root (node-lchild root) (node-remove (node-rchild root) key)))
												 )
                        ((equal? cmp 'left)  (if (null? (node-lchild root))
                                                 root
                                                 (node-reconstruct root (node-remove (node-lchild root) key) (node-rchild root))))
                        (else '())))
        )
)

(display (node-remove '(() (#\a) (#\d #\e #\f) ()) '(#\a)))

; �value la profondeur et
(define node-splay-tree
        (lambda (root to-splay)
                (let ((depth (node-depth root (node-key to-splay))))
                    (cond ((equal? depth 1) to-splay)
                          ((even? depth) (node-splay '() root (node-splay-tree (get-next-node root to-splay))))
                          ((odd? depth) (node-splay-repeat root to-splay (/ (- depth 1) 2)))
                          (else #f)))))

                    
                    
(define get-next-node 
        (lambda (actual final-key)
                (let ((cmp (if (null? root) #f (compare final-key (node-key actual)))))
  (cond 
    ((equal? cmp 'right) (node-rchild root))
    ((equal? cmp 'left) (node-lchild root)) 
    (else #f)))))
;fonction iterative pour les splays zig-zig et zig-zag. On suppose que le sous-arbre qui a g comme noeud a une profondeur de 2*repeat 
(define node-repeat-splay
        (lambda (g splay-key repeat)
                (let ((p (get-next-node g splay-key))
                      (x (get-next-node p splay-key)))
                    (if (equal? 1 repeat)
                        (node-splay g p x)
                        (node-splay g p (node-repeat-splay x splay-key (- 1 repeat)))))))
                        
; g (s'il existe) est le noeud parent de p.
; p est le noeud parent de x.
; p et x doivent exister sinon -> plante.
(define node-splay 
        (lambda (g p x)
                ;assign
                (let ((x-left (equal? (node-lchild p) x)))
                      ;(p-left (equal? (node-lchild g) p)))
                ;then
                    (if (null? g)
                        ; si true cas de zig zig
                        (if (x-left)
                            (zig-left p x)
                            (zig-right p x))
                        (let ((p-left (equal? (node-lchild g) p)))
                             (if (equal? p-left x-left)
                                 ; Si true on est dans un cas de zig-zig
                                 (if (x-left) 
                                     (zig-zig-left g p x)
                                     (zig-zig-right g p x))
                                 ; Si false on est dans un cas de zig-zag
                                 (if (x-left)
                                     (zig-zag-left g p x)
                                     (zig-zag-right g p x))))))))
                    
                
;
;Op�ration zig. p etait root -> c est root
; quand x est le left child de p et p est root
; (((A) X (B)) P (C)) -> ((A) X ((B) P (C)))
(define zig-left
    (lambda (p x) 
            (node-reconstruct x (node-lchild x) (node-reconstruct p (node-rchild x) (node-rchild p)))))
;Op�ration zig. p etait root -> c est root
; quand x est le right child de p et p est root
; ((C) P ((B) X (A))) -> (((C) P (B)) X (A))

(define zig-right
    (lambda (p x) 
            (node-reconstruct x (node-reconstruct p (node-lchild p) (node-lchild x))(node-rchild x) )))
; x is right child of p is right child of g
(define zig-zig-right
        (lambda (g p x)
            (node-reconstruct x (node-reconstruct p (node-reconstruct g (node-lchild g) 
                                                                        (node-lchild p)) 
                                                    (node-lchild x))
                                (node-rchild x))))   

; x is leftchild of p is leftchild of g
(define zig-zig-left
        (lambda (g p x)
            (node-reconstruct x (node-lchild x) 
                                (node-reconstruct p (node-rchild x)  
                                                    (node-reconstruc g (node-rchild p)
                                                                       (node-rchild g))))))
                                                                       
(define (node-depth root key)
        (if (null? root) 
             (0)
             (let ((cmp (compare key (node-key root))))
                  (cond 
                        ((equal? cmp 'youfoundme) 1)
                        ((equal? cmp 'right) (+ 1 (node-depth (node-rchild root) key))
                        ((equal? cmp 'left) (+ 1 (node-depth (node-lchild root) key)) 
                         (else #f)))))))                                                                       
                                                                       
#|                                
(define zig-zag-left
        (lambda (g p x)
                (node-reconstruct x (node-reconstruct p (node-lchild p) (node-lchild x)))))        

(define zig-zag-right
        (lambda (g p x)
                (node-reconstruct x ())))                
                
  |#



(define (compare2 list1 list2)
	(if (or (null? list1) (null? list2))
		'erreur
		(let ((comp (compare3 (symbol->string list1) (symbol->string list2))))
			(cond ((null? comp) 'youfoundme)
				((equal? comp 'left) 'left)
				((equal? comp 'right) 'right)
				(else 'right;;; compare ('(a b c) '(a b c d e f)) => right		  
				)
			)
		)
	)
)

(define (compare3 list1 list2)
	(foldl
		(lambda (lst symb)
			(cond ((null? lst) 'left);;; compare ('(a b c d e) '(a b)) => left	
				  ((equal? lst 'left) 'left)
				  ((equal? lst 'right) 'right)
				  (else (let ((str1 (car lst))(str2 symb))
							(cond ((string<? str1 str2) 'left)
								  ((string>? str1 str2) 'right)
								  ((string=? str1 str2) (cdr lst))
							
							)
						)
				  )
			)
		)
		list1
		list2
	)
)
#|
(assert (equal? (compare2 '(a b c) '()) 'erreur))
(assert (equal? (compare2 '() '(d e f)) 'erreur))
(assert (equal? (compare2 '() '()) 'erreur))
(assert (equal? (compare2 '(a b c) '(a b)) 'right))
(assert (equal? (compare2 '(c a d r e) '(c a d r e r)) 'left))
(assert (equal? (compare2 '(q w e r t y) '(q w e r t y)) 'youfoundme))
(assert (equal? (compare2 '(q w e r t = y) '(q w e r t = y)) 'youfoundme))
(assert (equal? (compare2 '(= + +) '(= + +)) 'youfoundme))
|#
 
(define (node-find root key)
	(let ((cmp (if (null? root) #f (compare key (node-key root)))))
		(cond 
			((equal? cmp 'youfoundme) root )
			((equal? cmp 'right) (node-find (node-rchild root) key))
			((equal? cmp 'left) (node-find (node-lchild root) key )) 
		(else #f) 
		)
	)
)

;;; Prend en input une liste de caract�re, retourne une liste de liste de caract�re s�par� au niveau du char demand�
;;; ex. (string-split '(a p p l e + p i e + a r e + f u c k i n g + d e l i c i o u s)) => ((a p p l e) (p i e) (a r e) (f u c k i n g) ( d e l i c i o u s))
;;;     (string-split '(a p p l e p i e)) => (a p p l e p i e)
(define string-split
 (lambda (str chr)
  (foldr  
    (lambda (x y)
            (if (equal? x chr) 
                (cons '() y)
                (cons (cons x (car y)) (cdr y))))
    '(()) str)))
	
;;;(assert (equal? (node-find '((() (a) (b r a v o) ()   ) (b) (b) ()) '(a)) '(() (a) (b r a v o) ())))

;;;prend une liste de chaines et retourne la concatenation de ces chaines en une seule liste
;;;ex: ((a b c) (d e) (f g h i)) => (a b c d e f g h i)
(define (construire-def lst)
    (display lst)
	(foldr
		(lambda(x y)
			(append x y)
		)
		'()
		lst
	)

)
;; prends en input une liste de terme a concatener
;; retourne une liste de definition concatener
(define (make-concatdefinition dict lst)
            (foldr 
                (lambda (x y)
                    (if (not (equal? y #f))
                        (let ((d (node-find dict x)))
                            (if (and d y) 
                                (cons (node-definition d) y)
                                #f))
                        #f))
                '() lst))

(assert (equal? (make-concatdefinition '(()(#\a #\b #\c)(#\p #\a #\t #\a #\t #\e)(() (#\d #\e #\f) (#\p #\o #\i #\l) ())) '((#\a #\b #\c) (#\d #\e #\f)) ) '((#\p #\a #\t #\a #\t #\e) (#\p #\o #\i #\l))) "test de concatenation de definition")		
(assert(equal? (make-concatdefinition '(() (#\a #\b) (#\z #\z #\z) ()) '((#\a #\b))) '((#\z #\z #\z))) "test de make-concatdefinition")
(assert (equal? (make-concatdefinition '(() (#\b) (#\b #\b #\b) (() (#\c) (#\c #\c #\c) ())) '((#\b) (#\c))) '((#\b #\b #\b) (#\c #\c #\c))) "test de make-concatdefinition")
(assert (equal? (string-split '(a p p l e + p i e + a r e + f u c k i n g + d e l i c i o u s) '+) '((a p p l e) (p i e) (a r e) (f u c k i n g) ( d e l i c i o u s))) "string-split")
(assert (equal? (string-split '(a p p l e p i e) +) '((a p p l e p i e))) "split without token in list");;;si dans la liste � splitter il n'y a aucune occurence du s�parateur, 
																		  ;;;on retourne la liste de la liste initiale (1 niveau d'encapsulation)
(assert (equal? (string-split '(1 2 3) 2) '((1) (3))) "regular split")
(assert (equal? (string-split '() 2) '()) "splitting an empty list") 
;;;(assert (equal? (eval-expr '(q w e 1 2 3 )) '(q w e 1 2 3)))

;; TEST�
;;;(eval-expr '(a b c =)) => (- (a b c))
;;;(eval-expr '(a b c)) => (a b c)
;;;(eval-expr '(a b c = d e f)) => (= (a b c) (d e f))
(define (eval-expr expr)
		;;;(printligne 1 expr)
		;;;(assert (member '= expr))
		;;;(assert (member = expr)) �a fail tout le temps
        ;;;(printligne 2 expr)
		(if (member #\= expr) 
			(let ((expr2 (string-split expr #\=)))
				;;;(display 3)
				;;;(display expr2)

				(if (null? (cadr expr2))
					(cons '- (list (car expr2)));;;retrait du mot expr
					(append (list '= (car expr2))
							(cdr expr2));;;ajouter le mot.  REMARQUE: S'il y a plusieurs '= dans expr, c'est bizarre 
				)
			)
			expr;;;recherche du mot expr    
		)
)

(assert (equal? (eval-expr '(a b c #\= d e + f g + h i)) '(= (a b c) (d e + f g + h i))))
(assert (equal? (string-split (caddr (eval-expr '(a b c #\= d e + f g + h i))) '+) '((d e) (f g) (h i))))

(define (affichage-dict dictio)
	(begin
	(display dictio)
	(newline)
	dictio
	
	)

)
;;;(display (node-find '(() (#\a) (#\d #\e #\f #\i #\n #\i #\t #\i #\o #\n) ()) '(#\a)))
(assert (equal? (node-find '(() (#\a) (#\d #\e #\f #\i #\n #\i #\t #\i #\o #\n) ()) '(#\a)) '(() (#\a) (#\d #\e #\f #\i #\n #\i #\t #\i #\o #\n) ())))
;;;(assert (equal? (eval-expr '(q w e 1 2 3 )) '(q w e 1 2 3)))
;;;(assert (equal? (eval-expr '(a b c = d e f)) '(= (a b c) (d e f))))  
;;;----------------------------------------------------------------------------
(define traiter
  (lambda (expr dict)
   ;;;(affichage-dict dict)
   ;;;evaluer l'expression
   ;;;(printligne 1 expr)
   ;;;(printligne 2 (member #\= expr))
   (if (null? expr) (cons (string->list "entree vide") dict) ;;;l'utilisateur a taper enter
   (let ((result (eval-expr expr)))
			;;;(display (eval-expr expr))
            (if (exist (cdr result))
                (cond((equal? (car result) '-);;;result est de la forme ('- key) et il faut remove le mot key
                      (cons (string->list "delete") (node-remove dict (cadr result))));;;appel � node-remove avec (cdr result)?
                    ((equal? (car result) '=);;;result est de la forme ('= key definition) et il faut ajouter le mot key
                        (if (member #\+ (caddr result))
                            ;;;(display (construire-def (gerer-concat result dict)));;;(printligne 1 result);;;concat�nation
                            (let ((d (make-concatdefinition dict (string-split (caddr result) #\+))))
                                (if d
                                    (cons (string->list "insertion-concatenation") (node-insert dict (node-create (cadr result) (construire-def d) '() '())))
                                    (cons (string->list "terme inconnu") dict)))
                            (cons (string->list "insertion") (node-insert dict (node-create (cadr result) (caddr result) '() '())))));;;ajout normal
                    (else ;;;result est de la forme (key) et il faut rechercher le mot key
                    ;;;(display "recherche")
                    ;;;(display (node-find dict result));;;appeler node-find
                         (cons (let ((n (node-find dict result)))
                                    (if n 
                                        (node-definition n)
                                        (string->list "terme inconnu")))
                                 dict)))
                (cons (string->list "entree non-valide") dict))))))

   ;;;appliquer le traitement appropri�
  #|
  ;;;sortir la r�ponse appropri�e
   (cons (append (string->list "*** le programme est ")
                  '(#\I #\N #\C #\O #\M #\P #\L #\E #\T #\! #\newline)
                  (string->list "*** la requete lue est: ")
                  expr
                  (string->list "\n*** nombre de caract�res: ")
                  (string->list (number->string (length expr)))
                  '(#\newline))
          dict)|#


;;;----------------------------------------------------------------------------
;;; Ne pas modifier cette section.

(define go
  (lambda (dict)
	(affichage-dict dict)
    (print "? ")
    (let ((ligne (read-line)))
      (if (string? ligne)
          (let ((r (traiter-ligne ligne dict)))
            (for-each write-char (car r))
            (go (cdr r)))))))

(define traiter-ligne
  (lambda (ligne dict)
    (traiter (string->list ligne) dict)))

(go '()) ;; dictionnaire initial est vide

;;;----------------------------------------------------------------------------
