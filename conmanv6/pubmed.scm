(define-module (conmanv5 pubmed)
  #:use-module (ice-9 regex) ;;list-matches
 ;; (srfi srfi-19)   ;; date time
 #:use-module (web response)
 #:use-module (web request)
 #:use-module (web uri)
 #:use-module (web client)
 #:use-module (ice-9 receive)	     
 #:use-module (gnutls)	     
 #:use-module (conmanv5 recs)	     
 #:use-module (conmanv5 utilities)	     
 #:use-module (conmanv5 munger)
 #:use-module (conmanv5 env)
 #:use-module (conmanv5 cemail)
 #:use-module (ice-9 string-fun)  ;;string-replace-substring
 #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
 #:use-module  (srfi srfi-19)   ;; date time
 #:use-module (ice-9 pretty-print) 
 #:export (recurse-get-missing-email
	   retrieve-article
	   get-summaries
	   get-stats-list
	   ))

(define article-count 0)
(define author-count 0)
(define author-find-email-count 0)
(define batch-id (date->string  (current-date) "~Y~m~d~I~M"))

(define (process-vec-pmid lst results)
  ;;results passed in is '()
  (if (null? (cdr lst))
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
	    (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 4) (- (match:end (caar lst)) 5) )))))
	results)
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
            (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 4) (- (match:end (caar lst)) 5) )))))
	(process-vec-pmid (cdr lst) results))))

(define (process-vec-journal lst results)
  ;;results passed in is '()
  (if (null? (cdr lst))
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
	    (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 43) (- (match:end (caar lst)) 7) )))))
	results)
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
            (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 43) (- (match:end (caar lst)) 7) )))))
	(process-vec-journal (cdr lst) results))))

(define (process-vec-title lst results)
  ;;results passed in is '()
  (if (null? (cdr lst))
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
	    (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 33) (- (match:end (caar lst)) 7) )))))
	results)
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
            (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 33) (- (match:end (caar lst)) 7) )))))
	(process-vec-title (cdr lst) results))))



(define (get-pmid-jrn-title x)
  ;; in get-summaries this should come right after:
  ;; b (find-occurences-in-string "<DocSum>" all-summaries))
  ;; c (map (lambda (x) (substring all-summaries (car x) (cdr x))) b))
  ;; i.e. the summaries list is the input - pass in c
  ;; note that failed title or journal searches will insert "null"; failure is probably a missing character in the search term e.g. ®
  (let* ((search-term (string-append "<Id>[0-9]+</Id>"))
	 (a  (map list-matches  (circular-list search-term) x ))
	 (b (process-vec-pmid a '()))  ;;all PMIDs
	 (search-term (string-append "<Item Name=\"Title\" Type=\"String\">[" all-chars  "]+</Item>"))
	 (c  (map list-matches  (circular-list search-term) x ))
	 (d (process-vec-title c '()))  ;;Title
	 (search-term (string-append "<Item Name=\"FullJournalName\" Type=\"String\">[" all-chars  "]+</Item>"))
	 (e  (map list-matches  (circular-list search-term) x ))
	 (f (process-vec-journal e '()))  ;;Journals
	 )
    ;;    (pretty-print f)))
      (make-ref-records b f d )
    ))



(define (get-summaries reldate retmax)
  ;; this is the initializing method
  ;; calls get-id-authors which provides the return value -- the list (("37658855")  ("Jirangkul P"   "Lorsuwannarat N"   "Wanichjaroen N"))
  ;; calls get-pmid-jrn-title to make ref records as side effect
  (let*((db "pubmed")
;;	(query (string-append "96+multi+well+OR+high-throughput+screening+assay+(" (uri-encode two-weeks-ago) "[epdat])"))
	(query (string-append "(((96+well+plate)+OR+(high+throughput+screening))+OR+(multi+well+plate+assay))+AND+((\"" (uri-encode two-weeks-ago) "\"[Date+-+Entry]+%3A+\"" (uri-encode two-weeks-ago) "\"[Date+-+Entry]))"))
	
	
	(base "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/")
	(url (string-append base  "esearch.fcgi?db=" db "&term=" query "&retmax=" retmax))
	(the-body   (receive (response-status response-body)
			(http-request url) response-body))
	(dummy (sleep 1))
        (all-ids-pre   (map match:substring  (list-matches "<Id>[0-9]+</Id>" the-body ) ))
	(e (if (not (null? all-ids-pre))
	       (let* ((all-ids (map (lambda (x) (string-append (xsubstring x 4 12) ",")) all-ids-pre))
		      (all-ids-concat (string-concatenate all-ids))
		      (all-ids-concat (xsubstring all-ids-concat 0 (- (string-length all-ids-concat) 1)))
		      (summary-url (string-append base "esummary.fcgi?db=" db "&id=" all-ids-concat  ))
		      ;; (summary-url (string-append base "esummary.fcgi?db=" db "&id=" all-ids-concat "&version=2.0" ))
		      (all-summaries   (receive (response-status response-body)
					   (http-request summary-url) response-body))
		      (b (find-occurences-in-string "<DocSum>" all-summaries))
		      (c (map (lambda (x) (substring all-summaries (car x) (cdr x))) b))
		      (d (recurse-remove-italicization c '()))
		      ;; this is where I will insert the ref table processing
		      ;; this creates ref-records, an a-list of references
		      (dummy (get-pmid-jrn-title d)) ;;makes the ref-records as a side effect
		      ) 
		 (map get-id-authors d)
		 )		      
               '() ))  )
  ;;  (pretty-print e)))
   e))

;; (pretty-print (get-summaries "40" "3"))



(define (retrieve-article a-summary)
  ;;this does all the work; summary list repeately processed article by article
  ;;including send email
  (let* ((pmid (caar a-summary))
	 (auth-list (cadr a-summary))
	 (indexed-auth-lst (recurse-lst-add-index 1 auth-list '()))
	 (url (string-append "https://pubmed.ncbi.nlm.nih.gov/" pmid "/"))
	 (the-body (receive (response-status response-body)
		       (http-request url) response-body))
	 (dummy (set! article-count (+ article-count 1)))
	 (dummy2 (sleep 1))
	 ;; must test here for the text </a><sup class=\"equal-contrib-container OR </a><sup class=\"affiliation-links\"><spa
	 ;; if not present, no affiliations, move on
	 (author-records (if the-body (get-authors-records the-body) #f))
	 (affils-alist '())
	 (affils-alist (if (null? author-records) #f (get-affils-alist the-body )))
	 (author-records2 (if (null? affils-alist) #f (recurse-update-contact-records 1 pmid indexed-auth-lst author-records affils-alist '())))
;;	 (_ (pretty-print author-records2))
;;	 (author-records2 (if  affils-alist (recurse-update-contact-records 1 pmid indexed-auth-lst author-records affils-alist '()) #f ))
	 (author-records3 (if (null? author-records2) #f (recurse-get-missing-email author-records2 '())))
;;	 (author-records3 (if  author-records2 (recurse-get-missing-email author-records2 '()) #f))
	 (unique-emails (recurse-get-unique-emails author-records3 '()))
;;	  (_ (pretty-print  "unique-emails:"))
;;	 (_ (pretty-print  unique-emails))
	 (author-records4 (get-unique-email-contacts author-records3 unique-emails '()))
	 ;;comment next line for testing and pretty print
	 (dummy4 (if (null? author-records4) #f (recurse-send-email author-records4) ))
	 )     
       #f
    ))


;;(pretty-print (retrieve-article "33919699"))


;; provides a list of articles.  One article looks like:
;; (full_name first_name last_name id id affiliation [email])  e.g.:
;;
  ;; (("Peng Song"
 ;;  "Peng"
 ;;  "Song"
 ;;  "1"
 ;;  "1"
 ;;  "College of Water Resources and Civil Engineering, China Agricultural University, Beijing 100083, China."
 ;;  "null")
 ;; ("Yang Xiao"
 ;;  "Yang"
 ;;  "Xiao"
 ;;  "1"
 ;;  "1"
 ;;  "College of Water Resources and Civil Engineering, China Agricultural University, Beijing 100083, China."
 ;;  "null")
 ;; ("Zhiyong Jason Ren"
 ;;  "Zhiyong"
 ;;  "Ren" etc.....
;;
;; Must merge the names with the affiliations
;; Not all affiliations will contain an email address
;; provide url with &metadataPrefix=pmc_fm&tool=cntmgr&email=info@labsolns.com so I may be contacted prior to banning

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the ref table for custom emails



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-articles-for-auth auth)
  ;;this method supplies pmids for email search
  ;;search returns nothing if too many (20) pmids submitted
  ;;work with 10 for now
  ;;used in find-fl-aoi
  (let* ( (authmod (string-replace-substring auth " " "+"))
	 (url (string-append "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=" (uri-encode authmod) "[auth]&retmax=10"))
	 (summary-url  (uri-encode url )) 
	 (the-body   (receive (response-status response-body)
	 		 (http-request url) response-body))
	 (dummy (set! author-find-email-count (+ author-find-email-count 1)))
	 (dummy2 (sleep 1))
	 (a (map match:substring  (list-matches "<Id>[0-9]{8}</Id>" the-body )))
	 (b (map (lambda (x) (substring x 4 (- (string-length x) 5))) a))
	)
    b))

 ;; (pretty-print  (get-articles-for-auth "Marjanović Ž"))


(define (find-fl-aoi auth)
  ;; find first last author of interest (aoi)
  ;; return a list of pmids where the auth is the first or last author
  (let* ((a (get-articles-for-auth auth))
	 ;;next line returns nothing if too many pmids submitted
	 (b   (map first-or-last-auth? (circular-list auth) a))
	 (holder '())
	 (dummy (if b (map (lambda (x y) (if x (set! holder (append! holder (list y))) #f)) b a) #f))
	 )
 ;;   (pretty-print holder)))
    holder))

 ;; (pretty-print (find-fl-aoi "Church G"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find missing email methods
;; 
;; (find-email aoi)
;;     |
;;     |--> (find-fl-aoi aoi)                                                OK
;;     |         | find articles where author of interest
;;     |         | is the first or last author
;;     |         |
;;     |         |--> (get-articles-for-auth aoi)                            OK
;;     |         |        get N article for an author
;;     |         |
;;     |         |--> (map first-or-last-auth? aoi pmid)                     OK
;;     |                  determine whether the author is the
;;     |                  first or last author of article
;;     |
;;     |--> (map search-fl-for-auth aoi pmid)
;;               Pull down a single article where aoi
;;               is the first or last author and search
;;               html for email address


(define (find-email auth)
  ;;used in get-missing-email
  ;; fl-pmids are the pmids that have the author of interest as first or last author
  ;; note that more than 20 pmids may be triggering server to abort
  (let* (
	 (fl-pmids (find-fl-aoi auth))
	 (dummy (sleep 1))
	 (email (if fl-pmids (search-fl-for-auth  auth fl-pmids) #f))
	 )
      (if email email "null")))



(define (get-missing-email the-contact contacts-out)
  ;;input: contact records with all info but maybe email is missing
  ;;if email is missing find it
  ;;I will also count contacts in this method
  ;;this takes a lot of time
      (let* (
	     (email (contact-email the-contact))
	     (email-null?   (string=? "null" email))
	     (deplorables '( "Pfizer" "China"))
	     (affil (contact-affil the-contact))
	     (ok-affiliation? (not (any-not-false? (map string-contains-ci (circular-list affil) deplorables))))
	     (auth-name (contact-qname the-contact))
	     (new-email (if (and email-null?  ok-affiliation?) (find-email auth-name) email))
	     (dummy (set! author-count (+ author-count 1)))
	     (dummy (set-contact-email! the-contact new-email))
	     (dummy (set! contacts-out (cons the-contact contacts-out)))
	     )
	contacts-out))


(define (recurse-get-missing-email contacts contacts-out)
  ;;input: contact records with all info but maybe email is missing
  (if (null? (cdr contacts))     
	(get-missing-email (car contacts) contacts-out )   
	(recurse-get-missing-email (cdr contacts)
				   (get-missing-email (car contacts) contacts-out))))

(define (recurse-get-unique-emails contacts unique-emails)
  ;; input contacts records
  ;; output is a list of unique emails, but still contains nulls
  (if (null? (cdr contacts))
      (begin
	(set! unique-emails (cons (contact-email (car contacts)) unique-emails))
	(delete-duplicates! unique-emails))
      (begin
	(set! unique-emails (cons (contact-email (car contacts)) unique-emails ))
	(recurse-get-unique-emails (cdr contacts) unique-emails)
				 )))


 (define (scan-records-for-email contacts email)
   (if (null? (cdr contacts))
        (begin                  ;;the only one left
	 (if (string= (contact-email (car contacts)) email)
	     (car contacts)
	     #f)) 
       (begin
	 (if (string= (contact-email (car contacts)) email)
	     (car contacts)
	     (scan-records-for-email (cdr contacts) email)))))

(define (get-unique-email-contacts contacts unique-emails unique-contacts)
     (if (null? (cdr unique-emails))
	 (begin
	   (set! unique-contacts (cons (scan-records-for-email contacts (car unique-emails)) unique-contacts))
	   unique-contacts)
	 (begin
	   (set! unique-contacts (cons (scan-records-for-email contacts (car unique-emails)) unique-contacts))
	   (get-unique-email-contacts contacts (cdr unique-emails) unique-contacts ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; email end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-id-authors x)
  ;;used in get-summaries
  ;;provides the construct (("37658855")  ("Jirangkul P"   "Lorsuwannarat N"   "Wanichjaroen N"))
  (let* ((a (map match:substring  (list-matches "<Item Name=\"Author\" Type=\"String\">[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+</Item>" x )))
	 (b (map (lambda (x) (substring x 34 (- (string-length x) 7)) ) a))
	 (c (string-match "<Id>[0-9]{8}</Id>" x))
	 (d (xsubstring x (+ (match:start c) 4) (- (match:end c) 5))))
    (cons (list d) (list b))))


(define (search-fl-for-auth auth pmid-list)
  ;; search article where author of interest in either first or last
  ;; search for email id
  ;; articles is a list of pmids that have already been determined by find-fl-auth
  ;; to have the author of interest as first or last author
  ;; returns email or #f
  ;; process the list until you find an email
 (if (null? pmid-list) #f
      (let* ((url (string-append "https://pubmed.ncbi.nlm.nih.gov/" (car pmid-list) "/"))
	     (the-body  (with-exception-handler
			    (lambda (ex)
			      (pretty-print "exception in search-fl-for-auth:")
			      (pretty-print ex)
			      (search-fl-for-auth auth (cdr pmid-list)))
			  (lambda ()
			    (receive (response-status response-body)
				(http-request url) response-body))
			  #:unwind? #t))
	     (_ (sleep 2))
	     (coord-start (string-match "<div class=\"affiliations\">" the-body ))
	     (coord-end (string-match " <ul class=\"identifiers\" id=\"full-view-identifiers\">" the-body ))
	     (affil-chunk (if coord-start (xsubstring the-body (match:start coord-start) (match:start coord-end)) #f))
	     (first-space  (string-contains auth " "))
	     (lname (string-downcase (xsubstring auth 0  first-space )))	  
	     (a (if affil-chunk (regexp-exec generic-email-regexp affil-chunk) #f))
	     (email  (if a (xsubstring (match:string a) (match:start a) (match:end a)) #f)))
	(if email email (search-fl-for-auth auth (cdr pmid-list))))))


  
;; (search-fl-for-auth "Church G" "32753383")   this one has the email address
;; (search-fl-for-auth "Church G" "32381713")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authors end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (get-title x)
;;   (let* ((a (map match:substring  (list-matches "<Item Name=\"Author\" Type=\"String\">[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+</Item>" x )))
;; 	 (b (map (lambda (x) (substring x 34 (- (string-length x) 7)) ) a))
;; 	 (c (string-match "<Id>[0-9]{8}</Id>" x))
;; 	 (d (xsubstring x (+ (match:start c) 4) (- (match:end c) 5))))
;;     (cons (list d) (list b))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; title end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-stats-list elapsed-time)
  (list (cons "batchid" batch-id) (cons "article" (number->string article-count)) (cons "author" (number->string author-count)) (cons "author-find" (number->string author-find-email-count)) (cons "elapsed-time" (number->string elapsed-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reports end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



