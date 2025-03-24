(define-module (conmanv5 utilities)
  #:use-module (ice-9 regex) ;;list-matches
 #:use-module (ice-9 receive)	     
 #:use-module (web response)
 #:use-module (web request)
 #:use-module (web uri)
 #:use-module (web client)
 #:use-module (ice-9 pretty-print)
 #:use-module (srfi srfi-1) 
 #:use-module (srfi srfi-19) ;;date time 
 #:use-module (conmanv5 env) 
  #:export (find-occurences-in-string
	    any-not-false?
	    to-regular-char
	    recurse-lst-add-index
	    first-or-last-auth?
	    recurse-remove-italicization
	    get-rand-file-name
	    ))

(define (get-rand-file-name pre suff)
  (string-append home-dir "/tmp/" pre "-" (substring (string-append (number->string  (time-second (current-time)))(number->string  (time-nanosecond (current-time)))) 0 12) "." suff))


(define (first-or-last-auth? auth pmid)
  ;;is the supplied author first or last in the pmid
 (let* ((summary-url (string-append "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id="  pmid))
	(the-body   (receive (response-status response-body)
			(http-request summary-url) response-body))

	  (the-body  (with-exception-handler
			    (lambda (ex)
			      (pretty-print "exception in first-or-last-auth:")
			      (pretty-print ex))
			    ;;  (search-fl-for-auth2 auth (cdr pmid-list)))
			  (lambda ()
			    (receive (response-status response-body)
				(http-request summary-url) response-body))
			  #:unwind? #t))


	(dummy (sleep 2))
	(b (map match:substring  (list-matches "<Item Name=\"Author\" Type=\"String\">[-A-Za-zÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć ]+</Item>" the-body )))
	(c (map (lambda (x) (substring x 34 (- (string-length x) 7))) b))
	(first-auth (car c))
	(last-auth (list-cdr-ref c (- (length c) 1)))
	(both-auths (cons first-auth last-auth))
	(contains (lset-intersection string=? (list auth) both-auths) ))
   (if (> (length contains) 0) #t #f)))

;; (first-or-last-auth? "Church GM" "32753383")
;; (first-or-last-auth? "Bhak Y" "32753383")
;; (first-or-last-auth? "Weber JA" "32753383")
;; (first-or-last-auth? "Bhak Y" "32381713")

(define (remove-italicization x)  ;;and other strange characters
  ;;x is a  DocSum
  (let* ((a (regexp-substitute/global #f "&lt;i&gt;"  x 'pre "" 'post))
	 (b (regexp-substitute/global #f "&lt;/i&gt;"  a 'pre "" 'post))
	 (c (regexp-substitute/global #f "&lt"  b 'pre "" 'post))	 
	 )
 (regexp-substitute/global #f "&gt;"  c 'pre "" 'post)))

(define (recurse-remove-italicization inlst outlst)
  ;;inlst is a list of extracted DocSums
  ;;outlst is the cleaned list
    (if (null? (cdr inlst))
      (begin
	(set! outlst (cons (remove-italicization (car inlst)) outlst))
	outlst)
      (begin
	(set! outlst (cons (remove-italicization (car inlst)) outlst))
	(recurse-remove-italicization (cdr inlst) outlst))))


(define (recurse-lst-add-index counter inlst outlist)
  ;;take an input list and turn it into an a-list where the index
  ;;is a number starting at counter and incremented by one
  (if (null? (cdr inlst))
      (begin
	(set! outlist (acons counter (car inlst) outlist))
	outlist)
      (begin
	(set! outlist (acons counter (car inlst)  outlist))
	(set! counter (+ counter 1))
	(recurse-lst-add-index counter (cdr inlst) outlist))))


(define (find-occurences-in-string query the-string)
  ;; (find-occurences-in-string "<DocSum>" "<DocSum>jsdhfjhsud<DocSum>sdfklskdf<DocSum>dd")) ==>
  ;; ((4 . 17) (22 . 34) (39 . 45))
  (let*((starts (map match:start (list-matches query the-string  )))
	(start-offset (map (lambda (x) (+ x 4)) starts))
	(end-offset-pre (map (lambda (x) (- x 1)) starts))
	(end-offset (append (cdr end-offset-pre) (list (string-length the-string))))
	(final '())
	(final  (map (lambda (x y) (append final (cons x y) )) start-offset end-offset))
	)
    final))

(define (any-not-false? x)
        (if (null? x) #f
	    (if (equal? (car x) #f) (any-not-false? (cdr x)) #t)))

;; to use to-regular-char:
;; (normal-chars (string->char-set "-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' "))
;; (normal-chars-only? (string-every  normal-chars author-name))
;; (define name "Marjanović Ž")
;; (pretty-print (string-map to-regular-char  name))

(define (to-regular-char x)
  (let* (
	 (A (string->char-set "ÀÁÂÃÄÅÆĀĂĄ"))
	 (a (string->char-set "àáâãäåæāă"))
	 (C (string->char-set "ÇĆĈĊČĆĈĊČ"))
	 (c (string->char-set "çćĉċčćĉċč"))
	 (D (string->char-set "ÐĎĐ"))
	 (d (string->char-set "ď"))
	 (E (string->char-set "ĒĔĖĘĚ"))
	 (e (string->char-set "èéêëēĕėęě"))
	 (G (string->char-set "ĜĞĠĢ"))
	 (g (string->char-set "ĝğġģ"))
	 (H (string->char-set "Ĥ"))
	 (h (string->char-set "ĥĦħ"))
	 (I (string->char-set "ĨĪĬĮİ"))
	 (i (string->char-set "ìíîïĩīĭįı"))
	 (J (string->char-set "Ĵ"))
	 (j (string->char-set "ĵ"))
	 (K (string->char-set "Ķ"))
	 (k (string->char-set "ķĸ"))
	 (L (string->char-set "ĹĻĽĿŁ"))
	 (l (string->char-set "ĺļľŀł"))
	 (N (string->char-set "ÑŃŅŇŊ"))
	 (n (string->char-set "ñńņňŉŋ"))
	 (O (string->char-set "ÒÓÔÕÖ×ØŌōŎŐ"))
	 (o (string->char-set "òóôõö÷øōŏő"))
	 (R (string->char-set "ŔŖŘ"))
	 (r (string->char-set "ŕŗ"))
	 (S (string->char-set "ŚŜŞŠ"))
	 (s (string->char-set "śŝşš"))
	 (T (string->char-set "ŢŤŦ"))
	 (t (string->char-set "ţťŧ"))
	 (U (string->char-set "ÙÚÛÜŨŪŬŮŰŲ"))
	 (u (string->char-set "ùúûüũūŭůűų"))
	 (W (string->char-set "Ŵ"))
	 (w (string->char-set "ŵ"))
	 (Y (string->char-set "ÝŶŸ"))
	 (y (string->char-set "ýŷ"))
	 (Z (string->char-set "ŹŻŽ"))
	 (z (string->char-set "źżž"))
	 )
    (cond
     [ (char-set-contains? A x) #\A]
     [ (char-set-contains? a x) #\a]
     [ (char-set-contains? C x) #\C]
     [ (char-set-contains? c x) #\c]
     [ (char-set-contains? D x) #\D]
     [ (char-set-contains? d x) #\d]
     [ (char-set-contains? E x) #\E]
     [ (char-set-contains? e x) #\e]
     [ (char-set-contains? G x) #\G]
     [ (char-set-contains? g x) #\g]
     [ (char-set-contains? H x) #\H]
     [ (char-set-contains? h x) #\h]
     [ (char-set-contains? I x) #\I]
     [ (char-set-contains? i x) #\i]
     [ (char-set-contains? J x) #\J]
     [ (char-set-contains? j x) #\j]
     [ (char-set-contains? K x) #\K]
     [ (char-set-contains? k x) #\k]
     [ (char-set-contains? L x) #\L]
     [ (char-set-contains? l x) #\l]
     [ (char-set-contains? N x) #\N]
     [ (char-set-contains? n x) #\n]
     [ (char-set-contains? O x) #\O]
     [ (char-set-contains? o x) #\o]
     [ (char-set-contains? R x) #\R]
     [ (char-set-contains? r x) #\r]
     [ (char-set-contains? S x) #\S]
     [ (char-set-contains? s x) #\s]
     [ (char-set-contains? T x) #\T]
     [ (char-set-contains? t x) #\t]
     [ (char-set-contains? U x) #\U]
     [ (char-set-contains? u x) #\u]
     [ (char-set-contains? W x) #\W]
     [ (char-set-contains? w x) #\w]
     [ (char-set-contains? Y x) #\Y]
     [ (char-set-contains? y x) #\y]
     [ (char-set-contains? Z x) #\Z]
     [ (char-set-contains? z x) #\z]
     [ else  x])
  ))
