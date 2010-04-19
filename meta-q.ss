(module meta-q mzscheme
  (require (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "contract.ss")
           (lib "string.ss"))
  (provide/contract
   [meta-q (string? . -> . string?)]
   [meta-q-line-width parameter?])
  
  (define meta-q-line-width (make-parameter 72))
  
  (define ((string-division n replacement predicate?) s)
    (match
        (foldl
         (match-lambda*
           [(list next-char (list current-word acc results))
            (if (predicate? next-char)
                (if (equal? (sub1 n) (length acc))
                    (list empty 
                          empty
                          (if (empty? current-word)
                              results
                              (list* (list->string (reverse current-word)) results)))
                    (list current-word
                          (list* next-char acc)
                          results))
                (list (list* next-char 
                             (append (map (lambda _ replacement) acc)
                                     current-word))
                      empty results))])
         (list empty empty empty)
         (string->list s))
      [(list current-word acc results)
       (reverse (list* (list->string (reverse current-word)) results))]))
  (define words
    (string-division 1 " " char-whitespace?))
  (define paragraphs 
    (string-division 
     3 #\space
     (lambda (c)
       (or (equal? #\return c)
           (equal? #\newline c)))))
  
  (define (meta-q s)
    (apply string-append
           (reverse
            (first
             (foldl
              (match-lambda*
                [(list current-paragraph (list results))
                 (define inner
                   (apply string-append
                          (reverse
                           (first
                            (foldl
                             (match-lambda*
                               [(list current-word (list results line-length))
                                (define word-length (string-length current-word))
                                (define new-line-length (+ line-length word-length 1))
                                (if (<= new-line-length (meta-q-line-width))
                                    (list 
                                     (if (empty? results)
                                         (list* current-word results)
                                         (list* current-word " " results))
                                     new-line-length)
                                    (list 
                                     (list* current-word "\n" results)
                                     word-length))])
                             (list empty 0)
                             (words current-paragraph))))))
                 (list
                  (if (empty? results)
                      (list* inner results)
                      (list* inner "\n\n" results)))])
              (list empty)
              (paragraphs s))))))
  
  (define test
    "500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.500 words maximum, please.")  
    
  (define lorem
    "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Fusce augue purus, faucibus ac, blandit at, aliquet sed, libero. Cras hendrerit nisl non enim. Nam malesuada dictum mi. Pellentesque rhoncus. Nulla viverra, leo non lobortis ultricies, tellus orci tempus urna, ultricies aliquam dui lacus eget augue. Cras rhoncus. Curabitur in nulla eu sapien dictum tristique. Cras in metus. Pellentesque diam. Vivamus orci eros, nonummy at, tincidunt nec, nonummy vel, ante. Nulla aliquam lobortis dolor. Sed feugiat, leo et dignissim cursus, nisl neque lacinia dolor, ut feugiat tortor tellus tempor nibh. Aliquam eu turpis. Suspendisse in magna quis ante mattis pharetra. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Sed sed lectus in odio lacinia interdum. Proin metus. Vestibulum tempus bibendum dui.


Proin eget lacus quis nulla tempor semper. Duis sem. Fusce sapien. Mauris laoreet, mauris id placerat rhoncus, urna erat posuere risus, quis placerat ipsum enim ac dui. Ut fermentum neque ut enim. Donec blandit porta quam. Suspendisse arcu ligula, sagittis ut, tempor eget, adipiscing a, mauris. Nulla eu sem et magna suscipit tempus. Nunc nunc justo, commodo eget, rhoncus vel, pharetra eget, sapien. Sed varius, turpis at pharetra imperdiet, neque felis sodales sem, rhoncus accumsan quam mi eget risus. Donec adipiscing lorem eget dolor. "))