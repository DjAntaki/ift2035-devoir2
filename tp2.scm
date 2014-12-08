;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la première section.
;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; être définie, et vous pouvez ajouter des définitions de fonction
;;; afin de bien décomposer le traitement à faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter reçoit en paramètre une liste de caractères
;;; contenant la requête lue et le dictionnaire.  La fonction retourne
;;; une paire contenant la liste de caractères qui sera imprimée comme
;;; résultat de l'expression entrée et le nouveau dictionnaire.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "go" qui se charge de cela.
;;;----------------------------------------------------------------------------

; retourne #f si x est '() ou #f
(define (exist x) (not (or (null? x) (not x))))

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

;Calcule à partir de la root la profondeur jusqu'au noeud avec la clée cherchée                                                                       
(define (node-depth root key)
        (if (null? root) 
             0
             (let ((cmp (compare key (node-key root))))
                  (cond 
                        ((equal? cmp 'youfoundme) 1)
                        ((equal? cmp 'right) (+ 1 (node-depth (node-rchild root) key)))
                        ((equal? cmp 'left) (+ 1 (node-depth (node-lchild root) key))) 
                        (else #f)))))                                                                       
                                                                       


; Pour utilisation consquente dans le programme, appeler avec str1=clé recherchée et str2 noeud actuel
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

;
;Opération zig. p etait root -> c est root
; quand x est le left child de p et p est root
; (((A) X (B)) P (C)) -> ((A) X ((B) P (C)))
(define zig
    (lambda (p x) 
            (node-reconstruct x (node-lchild x) (node-reconstruct p (node-rchild x) (node-rchild p)))))
;Opération zig. p etait root -> c est root
; quand x est le right child de p et p est root
; ((C) P ((B) X (A))) -> (((C) P (B)) X (A))



(define zag
    (lambda (p x) 
            (node-reconstruct x (node-reconstruct p (node-lchild p) (node-lchild x))(node-rchild x) )))
; x is right child of p is right child of g
(define zag-zag
        (lambda (g p x)
            (node-reconstruct x (node-reconstruct p (node-reconstruct g (node-lchild g) 
                                                                        (node-lchild p)) 
                                                    (node-lchild x))
                                (node-rchild x))))   

; x is leftchild of p is leftchild of g
(define zig-zig
        (lambda (g p x)
            (node-reconstruct x (node-lchild x) 
                                (node-reconstruct p (node-rchild x)  
                                                    (node-reconstruct g (node-rchild p)
                                                                       (node-rchild g))))))



                                                                       
(define zig-zag
        (lambda (g p x)
                (node-reconstruct x (node-reconstruct p (node-lchild p) (node-lchild x)) (node-reconstruct g (node-rchild x)(node-rchild g) ))))        

;pas encore fait                
(define zag-zig
        (lambda (g p x)
                (node-reconstruct x (node-reconstruct g (node-lchild g) (node-lchild x)) (node-reconstruct p (node-rchild x)(node-rchild p) ))))
                
(define get-next-node 
        (lambda (actual key)
                (let ((cmp (if (not (exist actual)) #f (compare key (node-key actual)))))
  (cond 
    ((equal? cmp 'right) (if (exist (node-rchild actual))
                                    (cons (node-rchild actual) 'right)
                                    #f))
    ((equal? cmp 'left) (if (exist (node-lchild actual))
                                   (cons (node-lchild actual) 'left)
                                   #f))
    ((equal? cmp 'youfoundme) (cons actual 'youfoundme))
    (else #f))))) 
; key est la clée recherchée
; g (s'il existe) est le noeud parent de p.
; p (s'il existe) est une paire avec le noeud apres g dans la recheche de la clée dans l'arbre
; x (s'il existe) est une paire avec le noeud apres p dans la recheche de la clée dans l'arbre
(define node-splay 
        (lambda (g key)
                ;assign
                (let ((p (get-next-node g key))) ; p est soit 
                     (cond 
                          ((not (exist p)) g) ; dans le cas où le noeud cherché n'est pas dans l'arbre
                          ((equal? (cdr p) 'youfoundme) g)
                          (else (let ((x (get-next-node (car p) key)))
                                     (cond
                                           ((not (exist x)) g);dans le cas où le noeud cherché n'est pas dans l'arbre
                                           
                                           ((and (equal? (cdr x) 'youfoundme) (equal? (cdr p) 'right))
                                             (zag g (car p))) 

                                           ((and (equal? (cdr x) 'youfoundme) (equal? (cdr p) 'left))
                                             (zig g (car p)))
                                             
                                           ((and (equal? (cdr x) 'right) (equal? (cdr p) 'right)) 
                                             (zag-zag g (car p) (node-splay (car x) key)))
                                             
                                           ((and (equal? (cdr x) 'left) (equal? (cdr p) 'right))
                                             (zag-zig g (car p) (node-splay (car x) key)))
                                             
                                           ((and (equal? (cdr x) 'right) (equal? (cdr p) 'left))
                                             (zig-zag g (car p) (node-splay (car x) key)))
                                             
                                           ((and (equal? (cdr x) 'left) (equal? (cdr p) 'left))
                                             (zig-zig g (car p) (node-splay (car x) key)))
                                             
                                           (else (display 'wtfomgerreur)))))))))
 
 
(display (node-splay '(((() (#\a) (#\a #\a) ()) (#\b) (#\b #\b) (() (#\c) (#\c #\c) ())) (#\d) (#\d #\d) ((() (#\e) (#\e #\e) ()) (#\f) (#\f #\f) (() (#\g) (#\g #\g) ()))) 
                             '(#\a)))
(newline)                     
(display (node-splay '(((() (#\a) (#\a #\a) ()) (#\b) (#\b #\b) (() (#\c) (#\c #\c) ())) (#\d) (#\d #\d) ((() (#\e) (#\e #\e) ()) (#\f) (#\f #\f) (() (#\g) (#\g #\g) ()))) 
                             '(#\b)))
(newline)
(display (node-splay '(((() (#\a) (#\a #\a) ()) (#\b) (#\b #\b) (() (#\c) (#\c #\c) ())) (#\d) (#\d #\d) ((() (#\e) (#\e #\e) ()) (#\f) (#\f #\f) (() (#\g) (#\g #\g) ()))) 
                             '(#\c)))
(newline)
(display (node-splay '(((() (#\a) (#\a #\a) ()) (#\b) (#\b #\b) (() (#\c) (#\c #\c) ())) (#\d) (#\d #\d) ((() (#\e) (#\e #\e) ()) (#\f) (#\f #\f) (() (#\g) (#\g #\g) ()))) 
                             '(#\d)))                             
                             
(newline)
(display (node-splay '(((() (#\a) (#\a #\a) ()) (#\b) (#\b #\b) (() (#\c) (#\c #\c) ())) (#\d) (#\d #\d) ((() (#\e) (#\e #\e) ()) (#\f) (#\f #\f) (() (#\g) (#\g #\g) ()))) 
                             '(#\e)))
(newline)
(display (node-splay '(((() (#\a) (#\a #\a) ()) (#\b) (#\b #\b) (() (#\c) (#\c #\c) ())) (#\d) (#\d #\d) ((() (#\e) (#\e #\e) ()) (#\f) (#\f #\f) (() (#\g) (#\g #\g) ()))) 
                             '(#\f)))
(newline)
(display (node-splay '(((() (#\a) (#\a #\a) ()) (#\b) (#\b #\b) (() (#\c) (#\c #\c) ())) (#\d) (#\d #\d) ((() (#\e) (#\e #\e) ()) (#\f) (#\f #\f) (() (#\g) (#\g #\g) ()))) 
                             '(#\g)))

                     
                #|
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (let ((cmp (if (null? g) #f (compare key (node-key g)))))
        (cond 
            ((equal? cmp 'right) (let ((p (node-rchild g)))
                                      (node- )
            
            node-rchild actual))
            ((equal? cmp 'left) (node-lchild actual)) 
            ((equal? cmp 'youfoundme) g))))) 
            
            (else #f))
                
                
                (let* ( (p (get-next-node g key))
                        (x-left (equal? (node-lchild p) x)))
                     ;(p-left (equal? (node-lchild g) p))
                ;then
                    (if (null? g)
                        ; si true cas de zig zig
                        (if x-left
                            (zig p x)
                            (zag p x))
                        (let ((p-left (equal? (node-lchild g) p)))
                             (if (equal? p-left x-left)
                                 ; Si true on est dans un cas de zig-zig
                                 (if x-left 
                                     (zig-zig g p x)
                                     (zag-zag g p x))
                                 ; Si false on est dans un cas de zig-zag
                                 (begin (display p)(if x-left
                                     (zig-zag g p x)
                                     (zig-zag g p x)))))))))    
;fonction iterative pour les splays zig-zig et zig-zag. On suppose que le sous-arbre qui a g comme noeud a une profondeur de 2*repeat 
(define node-repeat-splay
        (lambda (g splay-key repeat)
                (let* ((p (get-next-node g splay-key))
                      (x (get-next-node p splay-key)))
       (begin (display (list repeat g)) (newline) (if (equal? 1 repeat)
                        (node-splay g p x)
                        (node-splay g p (node-repeat-splay x splay-key (- 1 repeat)))))))
                )        
                        
; Évalue la profondeur et
(define node-splay-tree
        (lambda (root key)
                (let ((depth (node-depth root key)))
                    (cond ((equal? depth 1) root)
                          ((odd? depth) (node-splay '() root (node-splay-tree (get-next-node root key))))
                          ((even? depth) (node-repeat-splay root key (/ depth  2)))
                          (else #f)))))      
 |#
(define node-insert 
        (lambda (root node)
                (node-splay (_node-insert root node) (node-key node))))
 
(define _node-insert 
        (lambda (root node) 
                 (let ((cmp (if (null? root) #f (compare (node-key node) (node-key root)))))
                    (cond 
                        ((equal? cmp 'youfoundme) (node-reconstruct node (node-lchild root) (node-rchild root)))
                        ((equal? cmp 'right) (if (null? (node-rchild root))
                                                 (node-reconstruct root (node-lchild root) node) 
                                                 (node-reconstruct root (node-lchild root) (_node-insert (node-rchild root) node))))
                        ((equal? cmp 'left)  (if (null? (node-lchild root))
                                                 (node-reconstruct root node (node-rchild root)) 
                                                 (node-reconstruct root (_node-insert (node-lchild root) node) (node-rchild root))))
                        (else node))))) ; ce cas arrive quand la root est nulle

;;;(display (node-insert '() '(() (a b) (d e f i n i t i o n) ())))
                    
(define node-remove
        (lambda (root key) 
                 (let ((cmp (if (null? root) #f (compare key (node-key root)))))
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
 
 ;;Retourne  l'arbre splayé en fonction du noeud recherché. Retourne faux si l'élément n'est pas dans l'arbre 
(define (node-find root key)
    (let ((x (_node-find root key)))
        (if x 
           (node-splay root (node-key x))
           #f)))
 
;cherche et retourne le noeud avec une clé correspondante. retourne #f si le aucun noeud n'a une clé correspondante.
(define (_node-find root key)
	(let ((cmp (if (null? root) #f (compare key (node-key root)))))
		(cond 
			((equal? cmp 'youfoundme) root )
			((equal? cmp 'right) (_node-find (node-rchild root) key))
			((equal? cmp 'left) (_node-find (node-lchild root) key )) 
		(else #f) 
		)
	)
)

;;; Prend en input une liste de caractère, retourne une liste de liste de caractère séparé au niveau du char demandé
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
	
;;;(assert (equal? (_node-find '((() (a) (b r a v o) ()   ) (b) (b) ()) '(a)) '(() (a) (b r a v o) ())))

;;;prend une liste de chaines et retourne la concatenation de ces chaines en une seule liste
;;;ex: ((a b c) (d e) (f g h i)) => (a b c d e f g h i)
(define (construire-def lst)
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
                        (let ((d (_node-find dict x)))
                            (if (and d y) 
                                (cons (node-definition d) y)
                                #f))
                        #f))
                '() lst))

(assert (equal? (make-concatdefinition '(()(#\a #\b #\c)(#\p #\a #\t #\a #\t #\e)(() (#\d #\e #\f) (#\p #\o #\i #\l) ())) '((#\a #\b #\c) (#\d #\e #\f)) ) '((#\p #\a #\t #\a #\t #\e) (#\p #\o #\i #\l))) "test de concatenation de definition")		
(assert(equal? (make-concatdefinition '(() (#\a #\b) (#\z #\z #\z) ()) '((#\a #\b))) '((#\z #\z #\z))) "test de make-concatdefinition")
(assert (equal? (make-concatdefinition '(() (#\b) (#\b #\b #\b) (() (#\c) (#\c #\c #\c) ())) '((#\b) (#\c))) '((#\b #\b #\b) (#\c #\c #\c))) "test de make-concatdefinition")
(assert (equal? (string-split '(a p p l e + p i e + a r e + f u c k i n g + d e l i c i o u s) '+) '((a p p l e) (p i e) (a r e) (f u c k i n g) ( d e l i c i o u s))) "string-split")
(assert (equal? (string-split '(a p p l e p i e) +) '((a p p l e p i e))) "split without token in list");;;si dans la liste à splitter il n'y a aucune occurence du séparateur, 
																		  ;;;on retourne la liste de la liste initiale (1 niveau d'encapsulation)
(assert (equal? (string-split '(1 2 3) 2) '((1) (3))) "regular split")
(assert (equal? (string-split '() 2) '()) "splitting an empty list") 
;;;(assert (equal? (eval-expr '(q w e 1 2 3 )) '(q w e 1 2 3)))

;; TESTÉ
;;;(eval-expr '(a b c =)) => (- (a b c))
;;;(eval-expr '(a b c)) => (a b c)
;;;(eval-expr '(a b c = d e f)) => (= (a b c) (d e f))
(define (eval-expr expr)
		(if (member #\= expr) 
			(let ((expr2 (string-split expr #\=)))
				(if (null? (cadr expr2))
					(cons '- (list (car expr2)));;;retrait du mot expr
					(append (list '= (car expr2))
							(cdr expr2));;;ajouter le mot.  REMARQUE: S'il y a plusieurs '= dans expr, c'est bizarre 
				)
			)
			(cons '% expr);;;recherche du mot expr    
		)
)

(assert (equal? (eval-expr '(a b c #\= d e + f g + h i)) '(= (a b c) (d e + f g + h i))))
(assert (equal? (string-split (caddr (eval-expr '(a b c #\= d e + f g + h i))) '+) '((d e) (f g) (h i))))

(define (affichage-dict dictio)
	(begin
    (newline)
    (display    'dict:)
	(display dictio)
	(newline)
	dictio
	)
)
(assert (equal? (_node-find '(() (#\a) (#\d #\e #\f #\i #\n #\i #\t #\i #\o #\n) ()) '(#\a)) '(() (#\a) (#\d #\e #\f #\i #\n #\i #\t #\i #\o #\n) ())))
;;;(assert (equal? (eval-expr '(q w e 1 2 3 )) '(q w e 1 2 3)))
;;;(assert (equal? (eval-expr '(a b c = d e f)) '(= (a b c) (d e f))))  


;;; test d'assertion emprunté à guillaume

;(assert (equal?
;'((root-left root-term root-definitions node-left) node-term node-definitions node-right)
;(zag '(root-left root-term root-definitions (node-left node-term node-definitions node-right)))) "node-zag")

;(assert (equal?
;'(node-left node-term node-definitions (node-right root-term root-definitions root-right))
;(zig '((node-left node-term node-definitions node-right) root-term root-definitions root-right))) "node-zig")

;(assert (equal?
;'(((root-left root-term root-definitions node-left) node-term node-definitions child-left) child-term child-definitions child-right)
;(zag-zag '(root-left root-term root-definitions (node-left node-term node-definitions (child-left child-term child-definitions child-right))))) "node-zag-zag")

;(assert (equal?
;'(child-left child-term child-definitions (child-right node-term node-definitions (node-right root-term root-definitions root-right)))
;(zig-zig '(((child-left child-term child-definitions child-right) node-term node-definitions node-right) root-term root-definitions root-right))) "node-zig-zig")

;(assert (equal?
;'((root-left root-term root-definitions child-left) child-term child-definitions (child-right node-term node-definitions node-right))
;(zag-zig '(root-left root-term root-definitions ((child-left child-term child-definitions child-right) node-term node-definitions node-right)))) "zag-zig")

;(assert (equal?
;'((node-left node-term node-definitions child-left) child-term child-definitions (child-right root-term root-definitions root-right))
;(zig-zag '((node-left node-term node-definitions (child-left child-term child-definitions child-right)) root-term root-definitions root-right))) "zag-zig")

;;;----------------------------------------------------------------------------
(define traiter
  (lambda (expr dict)
   ;;;evaluer l'expression
   (if (null? expr) (cons (string->list "entree vide") dict) ;;;l'utilisateur a taper enter
   (let ((result (eval-expr expr)))
   
            (if (exist (cdr result))
            
                (cond ((equal? (car result) '-);;;result est de la forme ('- key) et il faut remove le mot key
                        (cons (string->list "delete") (node-remove dict (cadr result))));;;appel à node-remove avec (cdr result)?
                      
                      ((equal? (car result) '=);;;result est de la forme ('= key definition) et il faut ajouter le mot key
                        (if (member #\+ (caddr result))
                            (let ((d (make-concatdefinition dict (string-split (caddr result) #\+))))
                                  (if d
                                      (cons (string->list "insertion-concatenation") (node-insert dict (node-create (cadr result) (construire-def d) '() '())))
                                      (cons (string->list "terme inconnu") dict)))
                            (cons (string->list "insertion") (node-insert dict (node-create (cadr result) (caddr result) '() '())))));;;ajout normal
                            
                       ((equal? (car result) '%) ;;;result est de la forme ('% key) et il faut rechercher le mot key
                        (let ((n (node-find dict (cdr result))))
                              (display 'patate)
                              (display n)
                              (newline)
                              (if n 
                                 (cons (node-definition n) n) 
                                 (cons (string->list "terme inconnu") dict))))
                                        
                        (else (cons (string->list "???") dict)))
                    
                (cons (string->list "entree non-valide") dict))))))

   ;;;appliquer le traitement approprié
  #|
  ;;;sortir la réponse appropriée
   (cons (append (string->list "*** le programme est ")
                  '(#\I #\N #\C #\O #\M #\P #\L #\E #\T #\! #\newline)
                  (string->list "*** la requete lue est: ")
                  expr
                  (string->list "\n*** nombre de caractères: ")
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
