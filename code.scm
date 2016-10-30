(use-modules (srfi srfi-1))

;; Get all leaves from current node
(define (get-leaves tree)
  (define (fold-fun nodes accumelated)
    (append (if (list? nodes)
                nodes
                (list nodes))
            accumelated))
  (cond
   ;((not (list? tree))
   ; (error "Bad Data"))
   ;((null? tree)
   ; '())
   ;; everything after the leaf symbol is the lead
   ((eqv? 'leaf (car tree))
    (cdr tree))
   ;; it's an error to have the first element in a list be a list and not the rest
   ((list? (car tree))
    (fold fold-fun '() (map get-leaves tree)))
   ;; No brances
   (else
    (get-leaves (cdr tree)))))


;; Returns the tree you get by following path
;; note that nothing from path is in it, so if
;; you are searching with "eng" you would get
;; "lish" for "english", also note that 'path
;; should be a char list and not a string, this
;; should probably be fixed
(define (get-subtree path tree)
  (cond
   ((null? path) tree)
   ((null? tree) #f)
   ((not (list? (car tree)))
    tree)
   (else
    (call/cc
     (lambda (return)
       (for-each
        (lambda (branch)
          (when (eqv? (car branch)
                      (car path))
                (return (get-subtree (cdr path)
                                     (cdr branch)))))
        tree)
       (return #f))))))

;; Takes a word tree and add a new word to it
;; can currently only have two top level letters
;; This fails if str is an empty string
;; also fails if you try to add a string already added
(define (add-leaf str tree)
  (define (inner path sub-tree)
    (cond
     ((null? tree)
      (append path `(leaf ,str)))
     ((not (list? (car sub-tree)))
      (if (eqv? (car sub-tree)
                (car path))
          (cons (car sub-tree)
                (inner (cdr path)
                      (cdr sub-tree)))
          ;; Possibly sort this in alphabetical order.
          ;; Since that would would allow lookups in O(log n)
          ;; instead of O(n). But it actually doesn't matter
          ;; since this list never ought to be longer than
          ;; 40 items
          (list sub-tree
                (append path `(leaf ,str)))))
     ((not (memv (car path) (map car sub-tree)))
      (cons (append path `(leaf ,str))
            sub-tree))
     (else
      (map (lambda (branch)
             (if (eqv? (car path)
                       (car branch))
                 (cons (car branch)
                       (inner (cdr path)
                              (cdr branch)))
                 branch))
           sub-tree))))
      (inner (string->list (string-downcase str)) tree));)
