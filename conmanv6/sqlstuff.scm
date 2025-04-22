(define-module (conmanv6 sqlstuff)
  #:use-module (ice-9 regex) ;;list-matches
 ;; (srfi srfi-19)   ;; date time
 #:use-module (web response)
 #:use-module (web request)
 #:use-module (web uri)
 #:use-module (web client)
 #:use-module (ice-9 receive)	     
 #:use-module (gnutls)	     
 #:use-module (conmanv6 recs)	     
 #:use-module (conmanv6 utilities)	     
 #:use-module (conmanv6 munger)
 #:use-module (conmanv6 env)
 #:use-module (conmanv6 cemail)
 #:use-module (ice-9 string-fun)  ;;string-replace-substring
 #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
 #:use-module  (srfi srfi-19)   ;; date time
 #:use-module (ice-9 pretty-print)
 #:use-module (dbi dbi)
 #:export (
	   make-ref-sql
	   ;;	   update-ref-table
	   send-sql
	   map-over-article
	   make-affils-sql
	   update-conman
	   update-conmanstats
	   mark-email-sent
	   ))


(define db-vendor "mysql")
(define connect-string "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306")

(define (make-ref-sql pmid journal title sql)
  ;;must pass in sql: INSERT INTO ref (pmid, journal, title) VALUES
    ;; full SQL statement will be created for insert into the ref table
  (if (null? (cdr pmid))
      (begin
	(set! sql (string-append  sql "('" (car pmid) "', '" (car journal) "', '" (car title)  "')"))
	sql)
      (begin
        (set! sql (string-append  sql  "('" (car pmid) "', '" (car journal) "', '" (car title)  "'),"))
	(make-ref-sql (cdr pmid) (cdr journal) (cdr title) sql))))


;; (define (update-ref-table sql)
;;   (let* ((db-obj (dbi-open db-vendor connect-string)))
;;     (begin 
;;       (dbi-query db-obj sql)
;;       (dbi-close db-obj)
;;       )))


;; (define (make-affils-sql affils-lst-in affils-lst-out sql db-obj)
;;   ;;must pass in sql: INSERT INTO affils (affil) VALUES
;;     ;; full SQL statement will be created for insert into the ref table
;;   (if (null? (cadr affils-lst-in))
;;       (let* ((sql2 (string-append  sql  "('" (cadar affils-lst-in) "')"))
;; 	     (_ (dbi-query db-obj sql))
;; 	     (response (dbi-query db-obj "SELECT LAST_INSERT_ID()"))
;; 	     (new-lst `(,(car affils-lst-in) ,response ,(caddr affils-lst-in)))
;; 	     )
;; 	affils-lst-out)
;;       (let* (;;(_ (pretty-print (cadar affils-lst-in)))
;; 	     (sql2 (string-append  sql  "('" (cadar affils-lst-in) "');SELECT LAST_INSERT_ID()"))
;; 	     (_ (pretty-print sql2))
;; ;;	     (_ (pretty-print db-obj))
;; 	     (_ (dbi-query db-obj sql2))
;; ;;	     (_ (pretty-print db-obj))
;; ;;	     (response (dbi-get_row db-obj))
;; 	     (_ (pretty-print (dbi-get_row db-obj)))
;; 	     (_ (pretty-print (caddar affils-lst-in)))	     
;; 	     (new-lst `(,(caar affils-lst-in) ,response ,(caddar affils-lst-in)))
;; 	     (_ (pretty-print new-lst))	     
;; 	     )
;; 	(make-affils-sql (cdr affils-lst-in) (cons new-lst affils-lst-out) sql db-obj))))



;; (define (map-over-article an-author)
;;   ;; for each article map over the authors and if email is null
;;   ;; initiate the find-email
;;   (let* ((author-name (cadr an-author))
;; 	 (old-email (cadddr (cddddr  (cdr an-author))))
;; 	 (email-null?   (string=? "null" old-email))
;; ;; 	 (deplorables '("China" "India" "Pfizer"))
;; ;; 	 (ok-affiliation? (not (any-not-false? (map string-contains-ci (circular-list (cadddr (cdddr  (cdr an-author)))) deplorables))))
;;  	 (new-email (if (and email-null?  ok-affiliation?) (find-email author-name) old-email))
;; ;; 	 (dummy (if ok-affiliation?
;; ;; 	   	    (let* (
;; ;; 	   		   (sql-statement "INSERT INTO conman ( batchid, pmid, qname, wholen, firstn, lastn, affil, email ) VALUES ")
;; ;; 	  		   (stmnt (make-insert-stmnt an-author new-email))
;; ;; 	   		   (sql-statement2 (string-append sql-statement  stmnt)))
;; ;; 	;;	      (pretty-print sql-statement2)
;; ;; ;;	 	      (dbi-query ciccio sql-statement2)
;; ;; 	 	      ) #f)))
;; 	    ;;                 (pretty-print sql-statement2)))  ;;a deplorable or null
;; 	)	#t))

;; ;; (map map-over-article all-articles)
;; ;; (map-over-summaries one-summary)


;;CREATE PROCEDURE update_conman( IN _batchid INTEGER, IN _pmid INTEGER, IN _qname CHARACTER(250), IN _wholen CHARACTER(250), IN _firstn CHARACTER(250), IN _lastn CHARACTER(250), IN _affil CHARACTER(250), IN _email CHARACTER(250))

(define (mark-email-sent batchid email)
  (let* ((sql-statement (format #f "UPDATE conman SET sent=current_timestamp WHERE batchid='~a' AND email='~a'" batchid email))	
	 )
    (send-sql sql-statement)))
  

(define (update-conman batchid pmid qname wholen firstn lastn  affil email)
  ;;check if the affiliation is already in the db; if yes return its id
  ;;if no insert and return the new id
  ;;check if the email is already in the db; if yes, is it marked unsubscribe?
  ;;if yes do not insert, if no insert because this is most likely a new pmid/affiliation
  (let* ((sql-statement (format #f "Call update_conman( '~a', '~a', '~a', '~a', '~a', '~a', '~a', '~a')" batchid pmid qname wholen firstn lastn affil email))
	;; (db-obj (dbi-open db-vendor connect-string))
	 )
    (send-sql sql-statement)))


(define (update-conmanstats lst)
  (let* ((sql-statement (format #f "INSERT INTO conmanstats (batchid, article, author, author_search, author_find, elapsed) VALUES ( '~a', ~a, ~a, ~a, ~a, ~a)"
				(cdr (assoc "batchid" lst))
				(cdr (assoc "article" lst))
				(cdr (assoc "author" lst))
				(cdr (assoc "author-find" lst))
				(cdr (assoc "author-search-success" lst))
				(cdr (assoc "elapsed-time" lst))
				)))
    (send-sql sql-statement)))


(define (send-sql sql-statement)
(let* ((db-obj (dbi-open db-vendor connect-string))
	 )
    (begin
      (dbi-query db-obj sql-statement)
      (dbi-close db-obj))))
  
