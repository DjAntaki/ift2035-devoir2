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

(define assert (lambda (expr . message)
                 (let ((color (if expr "\x1b[32m" "\x1b[31m")) (reset "\x1b[0m"))
                     (display color)
                     (display (if expr "pass" "fail"))
                     (display (if (not (null? message)) (string-append " " (car message)) ""))
                     (display "\n")
                     (display reset))))


(define node-key cadr)
(define node-definition caddr)
(define node-lchild car)
(define node-rchild cadddr)
(define (node-create key definition lchild rchild) (list lchild key definition rchild))
(define (node-reconstruct x lchild rchild) (list lchild (node-key x) (node-definition x) rchild))

;(define node-insert 
;        (lambda (root node) 
;                (..........)))

;(define node-remove ....)
                
;(define (node-delete root node)
;	 ((let (node node-find(key)))
;     ...........))

; Évalue la profondeur et
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
;Opération zig. p etait root -> c est root
; quand x est le left child de p et p est root
; (((A) X (B)) P (C)) -> ((A) X ((B) P (C)))
(define zig-left
    (lambda (p x) 
            (node-reconstruct x (node-lchild x) (node-reconstruct p (node-rchild x) (node-rchild p)))))
;Opération zig. p etait root -> c est root
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
(define (node-find root key) 
 (let ((cmp (if (null? root) #f (compare key (node-key root)))))
  (cond 
    ((equal? cmp 'youfoundme) root )
    ((equal? cmp 'right) (node-find (node-rchild root) key))
    ((equal? cmp 'left) (node-find (node-lchild root) key )) 
    (else #f))))
    
;; prends en input une liste de terme a concatener
;; retourne une liste de definition concatener
(define (make-concatdefinition dict lst)
        (foldr 
            (lambda (x y) 
                    (let ((d (node-find dict x)))
                     (if (and d y) 
                         (cons (node-definition d) y)
                          #f)))                         
            '() lst))
; Pour utilisation consquente dans le programme, appeler avec str1=clé recherchée et str2 noeud actuel
(define (compare str1 str2)
 (cond
  ((string< str1 str2) 'left)
  ((string> str1 str2) 'right)
  ((string= str1 str2) 'youfoundme)
  (else -1)))
    
;; y'a clairement des endroits ou il va falloir utiliser ca. on la que trop vu souvent en cours    
(define foldr
  (lambda (f base lst)  
    (if (null? lst)
         base
         (f (car lst)
            (foldr f base (cdr lst))))))

(define (gerer-concat str dict)
		(let ((x (string-split (caddr str) '+)))
			(display x);;;(make-concatdefinition x dict)
		)
)
;;;
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
  
;;; TESTÉ
;;;(eval-expr '(a b c =)) => (- (a b c))
;;;(eval-expr '(a b c)) => (a b c)
;;;(eval-expr '(a b c = d e f)) => (= (a b c) (d e f))
(define (eval-expr expr) 
        (if (member '= expr) 
			(let ((expr (string-split expr '=)))
				(if (null? (cadr expr))
					(cons '- (list (car expr)));;;retrait du mot expr
					(append (list '= (car expr))
							(cdr expr));;;ajouter le mot.  REMARQUE: S'il y a plusieurs '= dans expr, c'est bizarre 
				)
			)
		expr;;;recherche du mot expr    
		)
)    
;;;----------------------------------------------------------------------------
(define traiter
  (lambda (expr dict)
   ;;;evaluer l'expression
   (let ((result (eval-expr expr)))
			(cond((equal? (car result) '-);;;result est de la forme ('- key) et il faut delete le mot key
				  (display result);;;appel à node-delete avec (cdr result)?
				 )
				 ((equal? (car result) '=);;;result est de la forme ('= key definition) et il faut ajouter le mot key
					(if (member '+ (caddr result))
						(gerer-concat result dict);;;(printligneet1 result);;;concaténation
						result;;;ajout normal
					)
				 )
				 (else;;;result est de la forme (key) et il faut rechercher le mot key
				 (...);;;appeler node-find
				 )
			)
		)
   ;;;appliquer le traitement approprié
  
  ;;;sortir la réponse appropriée
   (cons (append (string->list "*** le programme est ")
                  '(#\I #\N #\C #\O #\M #\P #\L #\E #\T #\! #\newline)
                  (string->list "*** la requete lue est: ")
                  expr
                  (string->list "\n*** nombre de caractères: ")
                  (string->list (number->string (length expr)))
                  '(#\newline))
          dict)))

;;;----------------------------------------------------------------------------
;;; Ne pas modifier cette section.

(define go
  (lambda (dict)
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
