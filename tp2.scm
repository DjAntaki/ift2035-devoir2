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

(define node-key cadr)
(define node-definition caddr)
(define node-lchild node car)
(define node-rchild node cadddr)
(define (node-create key definition lchild rchild) (list lchild key definition rchild))
(define (node-reconstruct x lchild rchild) (list lchild (node-key x) (node-definition x) rchild))

(define node-insert 
        (lambda (root node) 
                (..........)))

(define node-remove ....)
(define node-replace ....)
                
;(define (node-delete root node)
;	 ((let (node node-find(key)))
;     ...........))

(define node-splay
        (lambda (root node)
                ()))
                
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
#|                                
(define zig-zag-left
        (lambda (g p x)
                (node-reconstruct x (node-reconstruct p (node-lchild p) (node-lchild x)))))        

(define zig-zag-right
        (lambda (g p x)
                (node-reconstruct x ())))                
                
  |#              
(define (node-find root key) 
 ((if (null? root) 
       (let cmp -1)
       (let cmp (compare key (node-key root))))
  (cond 
    ((equal? cmp 0) root )
    ((equal? cmp 1) (node-find (node-rchild root) key))
    ((equal? cmp 2) (node-find (node-lchild root) key )) 
    (else #f))))

;; prends en input une liste de terme a concatener
;; retourne une liste de definition concatener
(define (make-concatdefinition dict lst)
        (foldr 
            (lambda (x y) 
                    (let (d (node-find dict x))
                     (if (and d y) 
                         (cons (node-definition d) y)
                          #f)))                         
            '() lst))
    
(define (compare str1 str2)
 (cond
  ((string< str1 str2) 2)
  ((string> str1 str2) 1)
  ((string= str1 str2) 0)
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
  
;;; TEST�
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
				  (display result);;;appel � node-delete avec (cdr result)?
				 )
				 ((equal? (car result) '=);;;result est de la forme ('= key definition) et il faut ajouter le mot key
					(if (member '+ (caddr result))
						(gerer-concat result dict);;;(printligneet1 result);;;concat�nation
						result;;;ajout normal
					)
				 )
				 (else;;;result est de la forme (key) et il faut rechercher le mot key
				 (...);;;appeler node-find
				 )
			)
		)
   ;;;appliquer le traitement appropri�
  
  ;;;sortir la r�ponse appropri�e
   (cons (append (string->list "*** le programme est ")
                  '(#\I #\N #\C #\O #\M #\P #\L #\E #\T #\! #\newline)
                  (string->list "*** la requete lue est: ")
                  expr
                  (string->list "\n*** nombre de caract�res: ")
                  (string->list (number->string (length expr)))
                  '(#\newline))
          dict)))

;;;----------------------------------------------------------------------------
((let (x 1))
 ())
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
