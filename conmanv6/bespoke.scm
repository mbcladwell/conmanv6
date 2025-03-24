(define-module (conmanv5 bespoke)
  #:use-module (conmanv5 env)
  #:use-module (conmanv5 utilities)
  #:use-module (conmanv5 recs)
  #:use-module (ice-9 regex) ;;list-matches
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-19)   ;; date time
   #:use-module (json)
  #:export (	    	    	 	 
	    main
 	    ))


(define unsubscribes #f) ;;from MySQL all unscubscribes

  (let* (
	 (p  (open-input-file unsubscribe-file))
	 (data (json->scm p))
	 (vec (assoc-ref data "emails"))
	 )
     (set! unsubscribes (vector->list vec)))

(define (fname-from-email email)
  (let* ((at-loc (string-index email #\@))
	 (a (substring email 0 at-loc))
	 (b (string-index a #\.))
	 (c (if b (string-capitalize! (substring a 0 b))  #f)))
    c))


(define (send-email firstnin emailin txt)
  ;;the ref records have journal and title info, search with pmid
  ;;input to cemail is an alist:
  ;; (("email" . "Leen.Delang@kuleuven.be")
  ;;  ("journal" . "Microorganisms")
  ;;  ("title" . "Repurposing Drugs for Mayaro Virus: Identification.... Inhibitors.")
  ;;  ("firstn" . "Rana"))
  (let* (
	 (email emailin)
	 (firstn firstnin)
	 (email (if (member email unsubscribes)
		    (begin
		      (send-email-rejected-message)
		      "null")
		    email));;if email is in unsubscribe list set to null
;;	 (_ (pretty-print (string-append "email test- email: " email " test: " )))
;;	 (_ (pretty-print unsubscribes))
	 (the-list (list (cons "email" email) (cons "journal" journal)(cons "title" title)(cons "firstn" firstn)))
	 (dummy (if (string= email "null") #f
		    (begin
		      (send-custom-email email firstn txt);;comment this out to send report only
		      ))))
    #f))


(define (send-rejected-email emailin)
  (let* (
	 (email emailin)
;;	 (_ (pretty-print (string-append "the email: " email)))
	 (txt-composite (format #f "Content-Transfer-Encoding: binary\nContent-Type: multipart/alternative; boundary=\"_----------=_1737893771238871\"\nMIME-Version: 1.0\nDate: ~a\nFrom: ~a\nTo: ~a\nBcc: ~a\nSubject: Multi-well plate management software\n\n--_----------=_1737893771238871\nContent-Disposition: inline\nContent-Transfer-Encoding: quoted-printable\nContent-Type: text/plain\n\nThe email ~a was found in the unsubscribe list.\n\nFor more information contact mbcladwell@labsolns.com\n\nThank You!\n\nMortimer Cladwell MSc\nPrincipal\n\n\n--_----------=_1737893771238871\nContent-Transfer-Encoding: binary\nContent-Type: multipart/related; boundary=\"_----------=_1737893771238870\"\n\nThis is a multi-part message in MIME format.\n\n" (date->string (current-date)) sender personal-email bcc-recipient email))	
	 (html-composite (format #f "--_----------=_1737893771238870\nContent-Disposition: inline\nContent-Transfer-Encoding: 8bit\nContent-Type: text/html\n\n<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n<html><head><title></title></head><body style=\"font-family:Arial;font-size:14px\">\n<p><br>The email ~a was found in the unsubscribe list.\n\nThanks<br><br>\nMortimer Cladwell MSc<br>Principal<br><br>\n<a href=\"mailto:~a\">~a</a><br><br>\n<img src=\"data:image/png;base64,  ~a\" style=\"width: 175px; height: 62px;\"><br></body></html>\n--_----------=_1737893771238870--\n\n--_----------=_1737893771238871--\n" email personal-email personal-email laspng ))
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



(define (send-custom-email firstnin emailin txt)
  (let* (
;;	 (email emailin)
	 (email "mbcladwell@labsolns.com")
;;	 (_ (pretty-print (string-append "the email: " email)))
	 (first-name firstnin)
	 (txt-composite (format #f "Content-Transfer-Encoding: binary\nContent-Type: multipart/alternative; boundary=\"_----------=_1737893771238871\"\nMIME-Version: 1.0\nDate: ~a\nFrom: ~a\nTo: ~a\nBcc: ~a\nSubject: Multi-well plate management software\n\n--_----------=_1737893771238871\nContent-Disposition: inline\nContent-Transfer-Encoding: quoted-printable\nContent-Type: text/plain\n\nDear ~a,\n\n~a\nVisit Laboratory Automation Solutions at www.labsolns.com and learn how LIMS*Nucleus can help you.\n\nLIMS*Nucleus can:\n\n-Reformat plates - four 96 well plates into a 384 well plate; four 384 well plates into a 1536 well plate\n-Associate assay data with plate sets\n-Identify hits scoring in assays using included algorithms - or write your own\n-Export annotated data\n-Generate worklists for liquid handling robots\n-Rearray hits into a smaller collection of plates\n-Track samples\n\nLIMS*Nucleus can serve as the core of a LIMS system.\nPrototype algorithms, dashboards, visualizations with R/Shiny.\nDownload a free copy or evaluate an online running instance by visiting www.labsolns.com/limsn/evaluate/\n\nFor more information contact mbcladwell@labsolns.com\n\nThank You!\n\nMortimer Cladwell MSc\nPrincipal\n\nTo unsubscribe, paste the following URL into a browser:\n\nhttps://www.labsolns.com/limsn/unsubscribe/insert.php?email=~a\n\n--_----------=_1737893771238871\nContent-Transfer-Encoding: binary\nContent-Type: multipart/related; boundary=\"_----------=_1737893771238870\"\n\nThis is a multi-part message in MIME format.\n\n" (date->string (current-date)) sender email bcc-recipient first-name txt email))	
	 (html-composite (format #f "--_----------=_1737893771238870\nContent-Disposition: inline\nContent-Transfer-Encoding: 8bit\nContent-Type: text/html\n\n<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n<html><head><title></title></head><body style=\"font-family:Arial;font-size:14px\">\n<p>Dear ~a,<br><br>\n~a<br>\nVisit <a href=\"http://www.labsolns.com\">Laboratory Automation Solutions</a> and learn how LIMS*Nucleus can help you.<br><br>\nLIMS*Nucleus can:<br><br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Reformat plates - four 96 well plates into a 384 well plate; four 384 well plates into a 1536 well plate<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Associate assay data with plate sets<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Identify hits scoring in assays using included algorithms - or write your own<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Export annotated data<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Generate worklists for liquid handling robots<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Rearray hits into a smaller collection of plates<br>\n
&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Track samples<br><br>\nLIMS*Nucleus can serve as the core of a LIMS system.<br>\nPrototype algorithms, dashboards, visualizations with R/Shiny.<br>\nDownload a free copy or evaluate an online running instance by visiting <a href=\"http://labsolns.com/limsn/evaluate/\">labsolns.com</a><br><br>\nThanks<br><br>\nMortimer Cladwell MSc<br>Principal<br><br>\n<a href=\"mailto:~a\">~a</a><br><br>\n<img src=\"data:image/png;base64,  ~a\" style=\"width: 175px; height: 62px;\"><br><br><a href=\"https://www.labsolns.com/limsn/unsubscribe/insert.php?email=~a\">Unsubscribe</a></body></html>\n--_----------=_1737893771238870--\n\n--_----------=_1737893771238871--\n" first-name txt personal-email personal-email laspng email))
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

;;first name    email    custom-text
(define (main args)
  (let* ((firstn (cadr args))
	 (email (caddr args))
	 (txt (cadddr args))
	 )
  (if (member email unsubscribes)	  
      (send-rejected-email email)
      (send-custom-email firstn email txt))))

;; guix shell --manifest=/home/mbc/projects/autostall/conmanv5/manifest.scm -- guile -L . -e '(conmanv5 bespoke)' -s conmanv5/bespoke.scm first-name email 'mytext in quotes'
  

;; from local ~/projects/conmanv5 after modifying env.scm to chang home directory to /home/mbc
;; guile -L . -e '(conmanv5 bespoke)' -s conmanv5/bespoke.scm Ben bkleinstiver@mgh.harvard.edu 'I saw your post-bac research opportunity solicitation on X. I can help with LIMS and/or assay development. Check out our software - LIMS*Nucleus.'
