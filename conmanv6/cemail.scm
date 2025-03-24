(define-module (conmanv5 cemail)
  #:use-module (conmanv5 env)
  #:use-module (conmanv5 utilities)
  #:use-module (conmanv5 recs)
  #:use-module (ice-9 regex) ;;list-matches
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-19)   ;; date time
  #:use-module (dbi dbi)
  #:use-module (json)
  #:export (send-report
	    send-custom-email
	    recurse-send-email
	    emails-sent
	    emails-rejected
	    fname-from-email
	    main
 	    ))


(define emails-sent '())  ;;if an email is sent, cons it to this list
(define emails-rejected '()) ;;an unsubscribe I am reencountering

(define unsubscribes #f) ;;from MySQL all unscubscribes

;;cannot have this in /gnu/store
;; (let* ((p  (open-input-file unsubscribe-file))
;;        (data (json->scm p))
;;        (vec (assoc-ref data "emails"))
;;        )
;;   (set! unsubscribes (vector->list vec)))




(define (fname-from-email email)
  (let* ((at-loc (string-index email #\@))
	 (a (substring email 0 at-loc))
	 (b (string-index a #\.))
	 (c (if b (string-capitalize! (substring a 0 b))  #f)))
    c))

;;use the unsubscribes variable from env
(define (get-unsubscribes-from-json)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;; returns the vector portion converted to list
  (let* (
	 (p  (open-input-file unsubscribe-file))
	 (data (json->scm p))
	 (vec (assoc-ref data "emails"))
	 )
     (vector->list vec)))

;;(define a-contact (make-contact "28374827" "3" "qname" "Joe Blow" "Joe" "Blow" "GI" "jblow@acme.org"))
;; insert into unsubscribe (email) values ('sunguohui@bjut.edu');
;; insert into unsubscribe (email) values ('biswarup@iiti.ac.in');

(define (send-email a-contact)
  ;;the ref records have journal and title info, search with pmid
  ;;input to cemail is an alist:
  ;; (("email" . "Leen.Delang@kuleuven.be")
  ;;  ("journal" . "Microorganisms")
  ;;  ("title" . "Repurposing Drugs for Mayaro Virus: Identification.... Inhibitors.")
  ;;  ("firstn" . "Rana"))
  (let* (
	 (email (contact-email a-contact))
	 (firstn (contact-firstn a-contact))
	 (wholen (contact-wholen a-contact))
	 (pmid (contact-pmid a-contact))
	 (ref (assoc pmid  (@@ (conmanv5 recs) ref-records)))
;;	 (_ (pretty-print (@@ (conmanv5 recs) ref-records)))
	 (title (reference-title (cdr ref)))
	 (journal (reference-journal (cdr ref)))
	 (for-report (list (cons "wholen" wholen)(cons "email" email)))
         ;;need this when using /gnu/store
	 (unsubscribes (get-unsubscribes-from-json))
	 ;; (sql (format #f "SELECT * FROM unsubscribe WHERE email LIKE '~a';" email))      
         ;; (ciccio (dbi-open "mysql" "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306"))
         ;; (_ (dbi-query ciccio sql))      
;;         (email (if (dbi-get_row ciccio) "null" email));;if email is in unsubscribe list set to null
	 (email (if (member email unsubscribes)
;;	 (email (if (dbi-get_row ciccio)
		    (begin
		      (set! emails-rejected (cons for-report emails-rejected))
		      "null" )
		    email));;if email is in unsubscribe list set to null
;;	 (_ (pretty-print (string-append "email test- email: " email " test: " )))
;;	 (_ (pretty-print unsubscribes))
	 (the-list (list (cons "email" email) (cons "journal" journal)(cons "title" title)(cons "firstn" firstn)))
	 (dummy (if (string= email "null") #f
		    (begin
		      (send-custom-email the-list);;comment this out to send report only
		      (set! emails-sent (cons for-report emails-sent))))))
    #f))


(define (recurse-send-email lst)
  ;;lst is the list of contact records with unique emails
  ;;recurse over the contacts list and send an email if email is not null
  (if (null? (cdr lst))
      (send-email (car lst))
      (begin
	(send-email (car lst))
	(recurse-send-email (cdr lst)))))


(define (build-sent-list lst text)
  ;;lst is the list of emails sent (("firstn" . "a")("email" . "ea"))
  ;;recurse over the list and build the email contents
  (if (null? (cdr lst))
      (begin
	(set! text (string-append text (cdr (assoc  "wholen" (car lst))) " - " (cdr (assoc  "email" (car lst))) "\n"))
	text)
      (begin
	(set! text (string-append text (cdr (assoc  "wholen" (car lst))) " - " (cdr (assoc  "email" (car lst))) "\n"))	
	(build-sent-list (cdr lst) text))))



(define (send-custom-email item)
  ;; an item is a list with needed elements from query
  ;; (("email" . "Leen.Delang@kuleuven.be")
  ;;  ("journal" . "Microorganisms")
  ;;  ("title" . "Repurposing Drugs for Mayaro Virus: Identification.... Inhibitors.")
  ;;  ("firstn" . "Rana"))
  (let* (
	 (email (assoc-ref item "email"))  ;;comment this out for testing
;;	 (email "mbcladwell@labsolns.com")
;;	 (_ (pretty-print (string-append "the email: " email)))
	 (first-name (if (fname-from-email email) (fname-from-email email)(assoc-ref item "firstn")))
	 (txt-composite (format #f "Content-Transfer-Encoding: binary\nContent-Type: multipart/alternative; boundary=\"_----------=_1737893771238871\"\nMIME-Version: 1.0\nDate: ~a\nFrom: ~a\nTo: ~a\nBcc: ~a\nSubject: Multi-well plate management software\n\n--_----------=_1737893771238871\nContent-Disposition: inline\nContent-Transfer-Encoding: quoted-printable\nContent-Type: text/plain\n\nDear ~a,\n\nYour recent article entitled ~a in the journal ~a  suggests you might benefit from our product. Visit Laboratory Automation Solutions at www.labsolns.com and learn how LIMS*Nucleus can help you.\n\nLIMS*Nucleus can:\n\n-Reformat plates - four 96 well plates into a 384 well plate; four 384 well plates into a 1536 well plate\n-Associate assay data with plate sets\n-Identify hits scoring in assays using included algorithms - or write your own\n-Export annotated data\n-Generate worklists for liquid handling robots\n-Rearray hits into a smaller collection of plates\n-Track samples\n\nLIMS*Nucleus can serve as the core of a LIMS system.\nPrototype algorithms, dashboards, visualizations with R/Shiny.\nDownload a free copy or evaluate an online running instance by visiting www.labsolns.com/limsn/evaluate/\n\nFor more information contact mbcladwell@labsolns.com\n\nThank You!\n\nMortimer Cladwell MSc\nPrincipal\n\nTo unsubscribe, paste the following URL into a browser:\n\nhttps://www.labsolns.com/limsn/unsubscribe/insert.php?email=~a\n\n--_----------=_1737893771238871\nContent-Transfer-Encoding: binary\nContent-Type: multipart/related; boundary=\"_----------=_1737893771238870\"\n\nThis is a multi-part message in MIME format.\n\n" (date->string (current-date)) sender email bcc-recipient first-name (assoc-ref item "title")(assoc-ref item "journal") email))	
	 (html-composite (format #f "--_----------=_1737893771238870\nContent-Disposition: inline\nContent-Transfer-Encoding: 8bit\nContent-Type: text/html\n\n<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n<html><head><title></title></head><body style=\"font-family:Arial;font-size:14px\">\n<p>Dear ~a,<br><br>\nYour recent article entitled ~a in the journal <i>~a</i> suggests you might benefit from our product.<br>\nVisit <a href=\"http://www.labsolns.com\">Laboratory Automation Solutions</a> and learn how LIMS*Nucleus can help you.<br><br>\nLIMS*Nucleus can:<br><br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Reformat plates - four 96 well plates into a 384 well plate; four 384 well plates into a 1536 well plate<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Associate assay data with plate sets<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Identify hits scoring in assays using included algorithms - or write your own<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Export annotated data<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Generate worklists for liquid handling robots<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Rearray hits into a smaller collection of plates<br>\n
&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Track samples<br><br>\nLIMS*Nucleus can serve as the core of a LIMS system.<br>\nPrototype algorithms, dashboards, visualizations with R/Shiny.<br>\nDownload a free copy or evaluate an online running instance by visiting <a href=\"http://labsolns.com/limsn/evaluate/\">labsolns.com</a><br><br>\nThanks<br><br>\nMortimer Cladwell MSc<br>Principal<br><br>\n<a href=\"mailto:~a\">~a</a><br><br>\n<img src=\"data:image/png;base64,  ~a\" style=\"width: 175px; height: 62px;\"><br><br><a href=\"https://www.labsolns.com/limsn/unsubscribe/insert.php?email=~a\">Unsubscribe</a></body></html>\n--_----------=_1737893771238870--\n\n--_----------=_1737893771238871--\n" first-name (assoc-ref item "title")(assoc-ref item "journal") personal-email personal-email laspng email))
	 (_ (system (string-append "rm " home-dir "/tmp/rnd*.txt")))
	 (out-file-name  (get-rand-file-name "rnd" "txt"))
	 (p  (open-output-file out-file-name))
	 (_ (begin
	      (put-string p (string-append txt-composite html-composite ))
	      (force-output p)))
	 (msmtp-command (string-append "cat " out-file-name " | msmtp -t" ))
	 (_ (system msmtp-command))
	 )
	 #f))

(define (send-report lst)
  ;; lst is the stats
  ;; alist is emails that were sent, migt be null
  ;; blist is emails rejected because they were unsubscribed
  ;; (list (cons "wholen" wholen)(cons "email" email)) etc loop over to pull out names and emails
  (let* (
	 (str1 (string-append "From: " sender "\nTo: " personal-email "\nSubject: conman report\n\n"))
	 (str2 (string-append "Article count: " (cdr (assoc "article" lst)) "\n"))
	 (str3 (string-append "Author count: " (cdr (assoc "author" lst)) "\n"))
	 (str4 (string-append "Author find count: " (cdr (assoc "author-find" lst)) "\n"))
	 (str5 (string-append "Elapsed time: " (cdr (assoc  "elapsed-time" lst)) " minutes.\n\n"))
	 (str6 (if (null?  emails-sent) "null" (build-sent-list emails-sent "")))
	 (str7 "\n\n=====unsubscribes=====\n\n")
	 (str8 (if (null?  emails-rejected) "null" (build-sent-list emails-rejected "")))
	 (txt-composite (string-append str1 str2 str3 str4 str5 str6 str7 str8))
	 (txt-file-name (get-rand-file-name "rnd" "txt"))
	 (p2  (open-output-file txt-file-name))
	 (dummy (begin
		  (put-string p2 txt-composite )
		  (force-output p2)))
	 (msmtp-command (string-append "cat  " txt-file-name " | msmtp -t" ))
	 (dummy (system msmtp-command))
	 )
    #f))



