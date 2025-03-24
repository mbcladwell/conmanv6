(define-module (conmanv5 recs)
  #:use-module (ice-9 regex) ;;list-matches
  #:use-module  (srfi srfi-9)  ;;records
  #:use-module (ice-9 pretty-print)
  #:export (make-ref-records
	    ref-records
	    get-ref-records
	    make-reference
	    reference-title
	    reference-journal
	    recurse-update-contact-records
	    make-contact
	    contact-firstn
	    contact-wholen
	    contact-pmid
	    contact-email
	    contact-affil
	    contact-qname
	    set-contact-email!

	    ))

(define ref-records '())  ;;this will hold pmid, title, journal as records; key is pmid

(define (get-ref-records) (display ref-records))

(define-record-type <reference>
  (make-reference pmid journal title)
  reference?
  (pmid    reference-pmid)
  (journal reference-journal)
  (title   reference-title ))

;;(set! results )(cons "923478234" (make-reference "948593485" "JMB" "A Title"))


(define (make-ref-records pmid journal title )
;; this will fill the global ref-records
  (if (null? (cdr pmid))
      (begin
	(set! ref-records (acons  (car pmid) (make-reference (car pmid) (car journal) (car title)) ref-records))
;;	(display ref-records)
	ref-records)
      (begin
;;	(pretty-print (string-append "in first level of make-ref-records: "))
	(set! ref-records (acons (car pmid) (make-reference (car pmid) (car journal) (car title)) ref-records))
	(make-ref-records (cdr pmid) (cdr journal) (cdr title) ))))


(define-record-type <contact>
  (make-contact pmid index qname wholen firstn lastn affil email)
  contact?
  (pmid    contact-pmid set-contact-pmid!)
  (index contact-index set-contact-index!)
  (qname contact-qname set-contact-qname!)
  (wholen contact-wholen)
  (firstn contact-firstn)
  (lastn contact-lastn)
  (affil contact-affil set-contact-affil!)
  (email contact-email set-contact-email!))



(define (update-contact-records counter pmid auth-list the-contact affils auth-out)
  ;;fill missing fields pmid, index, qname using the passed in, indexed auth-list
      (let* (
	     (affil-id (contact-affil the-contact)) ;;what I need
;;	     (affil-list (if affil-id (assoc affil-id affils) #f))
	     (affil-list (if (and affil-id affils) (assoc affil-id affils) #f))
	     (affil (if  affil-list (cadr affil-list) "null"))
	     (email (if  affil-list (caddr affil-list) "null"))
	     (dummy (if  affil (set-contact-affil! the-contact affil) #f))
	     (dummy (set-contact-email! the-contact email))
	     (dummy (set-contact-pmid! the-contact pmid))
	     (dummy (set-contact-index! the-contact counter))
	     (dummy (set-contact-qname! the-contact (cdr (assoc counter auth-list))))
	     (dummy (set! auth-out (cons the-contact auth-out)))
	     )
	 auth-out))

(define (recurse-update-contact-records counter pmid auth-list authors affils auth-out)
  ;;fill missing fields pmid, index, qname using the passed in, indexed auth-list
(if (null? (cdr authors)) 
    (update-contact-records counter  pmid auth-list (car authors) affils auth-out)     
    (let* ((a (update-contact-records counter  pmid auth-list (car authors) affils auth-out))
	   (counter (+ counter 1)))
      (recurse-update-contact-records counter pmid auth-list (cdr authors) affils a))))

