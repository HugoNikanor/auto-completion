;; a better stream library might be srfi-43
(use-modules (ice-9 streams)
             (ice-9 rdelim))

(load "code.scm")

(define english-tree
  (stream-fold
    add-leaf
    '()
    (port->stream
      (open-input-file "/home/hugo/other/english-words.txt")
      read-line)))
(define (displayln x)
  (display x)
  (newline))

(for-each displayln
          (get-leaves (get-subtree '(#\h) english-tree)))
