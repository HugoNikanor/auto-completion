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

;; Takes a word tree and add a new word to it
;; can currently only have two top level letters
(define (add-leaf str tree)
  (define (inner path sub-tree)
    ;(format #t "~s\n" path)
    (if (not (list? (car sub-tree)))
        (if (eqv? (car sub-tree)
                  (car path))
            (cons (car sub-tree)
                  (inner (cdr path)
                         (cdr sub-tree)))
            ;; possibly sort this in alphabetical order
            (list sub-tree
                  (append path `(leaf ,str))))
        (map (lambda (branch)
               (if (eqv? (car path)
                         (car branch))
                   (cons (car branch)
                         (inner (cdr path)
                                (cdr branch)))
                   branch))
             sub-tree)))
  ;; The check for empty trees is only used when creating a new tree
  ;; It should possibly also be moved somewhere else, and utilize
  ;; early return features
  (if (null? tree)
      (append (string->list (string-downcase str)) `(leaf ,str))
      (inner (string->list (string-downcase str)) tree)))

(fold add-leaf
      '()
      '(;"10th"
        ;"1st"
        ;"2"
        ;"2nd"
        "Hi"
        "Hello"
        "Hejsan"
        "Orm"
        "Orelaterat"
        "Meme"
        ))

(pretty-print $26)

;; a better stream library might be srfi-43
(use-modules (ice-9 streams)
             (ice-9 rdelim))
(define english-tree
  (fold add-leaf
        '()
        (port->stream (open-input-file "/home/hugo/other/english-words.txt") read-line)))
(stream-fold add-leaf
             '()
             (port->stream (open-input-file "/home/hugo/other/english-words.txt") read-line))

