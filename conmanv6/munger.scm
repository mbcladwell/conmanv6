(define-module (conmanv5 munger)
  #:use-module (conmanv5 env)
  #:use-module (conmanv5 recs)
  #:use-module (conmanv5 utilities)
  #:use-module (ice-9 regex) ;;list-matches
 #:use-module (ice-9 receive)	     

  #:export (generic-email-regexp
	    get-authors-records
	    get-affils-alist
	    extract-authors ;;remove later
	    ))

;;these functions perform the screen scraping of PubMed articles

(define generic-email-regexp (make-regexp "[A-Za-z0-9.-]*@[-A-Za-z0-9.]+(\\.com|\\.edu|\\.org|\\.net|\\.uk|\\.fr|\\.de|\\.it|\\.ru|\\.in|\\.au|\\.ca|\\.io|\\.py|\\.se|\\.dk|\\.sg|\\.be)" regexp/extended))

(define (get-coords lst)
  ;;expecting a 4 element list
  ;;used in extract-authors
  ;;pull out the full name and extract first and last and other as needed
  (let* ((a (if (car lst) (list (+ (match:start (car lst)) 1)(- (match:end (car lst)) 39)) #f)) ;;equal-contrib-container
	 (b (if (cadr lst) (list (+ (match:start (cadr lst)) 1)(- (match:end (cadr lst)) 39)) #f)) ;;affiliation-links
	 (c (if (caddr lst) (list (+ (match:start (caddr lst)) 1)(- (match:end (caddr lst)) 11)) #f))
	 (d (if (cadddr lst) (list (+ (match:start (cadddr lst)) 1)(- (match:end (cadddr lst)) 24)) #f))
	 )
    (if a a (if b b (if c c (if d d #f))))))

(define (extract-authors achunk)
  ;; If there are equal contributors, a different string search strategy is needed
  ;; the string extraction is such that either method extracts the same coordinates
  (let* (   
	 (coords  (get-coords
		   (list (string-match (string-append ">[" all-chars "]+</a><sup class=\"equal-contrib-container")  achunk)			  
		      (string-match (string-append ">[" all-chars "]+</a><sup class=\"affiliation-links\"><spa")  achunk)
		      (string-match (string-append ">[" all-chars "]+</a></span>")  achunk)
		      (string-match (string-append ">[" all-chars "]+</a><span class=\"comma\">") achunk )			
		      )))
	  (full-name (xsubstring achunk (car coords) (cadr coords)))
	   (name-num-sp (string-count full-name #\sp))
	   (first-sp (string-contains full-name " "))
	  (second-sp (if (> name-num-sp 1) (string-contains full-name " " (+ first-sp 1)) #f))
	  (third-sp (if (> name-num-sp 2) (string-contains full-name " " (+ second-sp 1)) #f))
	  (first (if (or (= name-num-sp  1) (= name-num-sp  2)) (xsubstring full-name 0  first-sp )  "null"  ))	 
	  (last (cond ((= name-num-sp 3) (xsubstring full-name (+ third-sp 1) (string-length full-name)))
	  	     ((= name-num-sp 2) (xsubstring full-name (+ second-sp 1) (string-length full-name)))
	   	     ((= name-num-sp 1) (xsubstring full-name (+ first-sp 1) (string-length full-name)))
	  	     ((= name-num-sp 0) full-name)))
	  (affiliation-pre (string-match ">\n *[0-9]+\n *<" achunk) )
	  (affiliation (if affiliation-pre (string-trim-both (xsubstring (match:string affiliation-pre) (+ (match:start affiliation-pre) 1)(- (match:end affiliation-pre) 1))) #f))
	 ;;(a-contact (make-contact "" "" "" full-name  first last affiliation ""))
	 )
  ;;  (pretty-print affiliation-pre)
   (make-contact "" "" "" full-name  first last affiliation "")
    ))


(define (extract-affiliations achunk )
  ;;used in get-affils-alist
  ;; affiliation2 is the affiliation without the email address
  (let* (
	 (affil-num-pre  (string-match ">[0-9]+</sup>"  achunk))		 
	 (affil-num (xsubstring (match:string affil-num-pre) (+ (match:start affil-num-pre) 1)(- (match:end affil-num-pre) 6)))
	 (affiliation-pre (string-match "</sup>[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\"\\/ ]+</li>" achunk))
	 (affiliation (string-trim-both (xsubstring (match:string affiliation-pre) (+ (match:start affiliation-pre) 6) (-(match:end affiliation-pre) 5 ) )))
	 (email-pre (regexp-exec generic-email-regexp affiliation))
	 (email-coords? (if email-pre (vector-ref email-pre 1) #f))
	 (email (if email-coords? (xsubstring affiliation (car email-coords?) (cdr email-coords?)) "null" ))
	 (email-add? (string-match "Electronic address:" affiliation))
	 (affiliation2 (if email-add? (xsubstring affiliation 0 (car (vector-ref email-add? 1))) affiliation))
	 )
  (list affil-num  affiliation2 email )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define authors-regexp
  ;; pulls out a single author
  (make-regexp "data-ga-label=[a-zA-Z0-9~_+=,.:;'()//&#@<>/\" -]+</a></sup><span" regexp/extended))


(define (get-authors-records the-body)
  (let*(
	(coord-start (string-match "<div class=\"authors-list\">" the-body ))
	(auth-v (if coord-start
		    (let* (
			   (coord-end (if (string-match "<div class=\"short-article-details\">" the-body )
					  (string-match "<div class=\"short-article-details\">" the-body )
					  (string-match "<div class=\"extended-article-details\" id" the-body )))		       
			   (auth-chunk (xsubstring the-body (match:start coord-start) (match:start coord-end)))
			   (auth-chunk (regexp-substitute/global #f "&#39;"  auth-chunk 'pre "" 'post))  ;; get rid of '; O'Hara
			   (auth-chunk (regexp-substitute/global #f "&amp;"  auth-chunk 'pre "" 'post))  ;; get rid of &
			   (b (find-occurences-in-string "data-ga-label=" auth-chunk))
			   (auth-lst (map (lambda (x) (substring auth-chunk (car x) (cdr x))) b))
			   (first-author (car auth-lst))
			   (proceed-flag (or (string-contains first-author "</a><sup class=\"equal-contrib-container")
					     (string-contains first-author "</a><sup class=\"affiliation-links\"><spa")
					      (string-contains first-author "</a></span>")
					      (string-contains first-author "</a><span class=\"comma\">")
					     )))
		      (if proceed-flag (map extract-authors auth-lst) #f))
		    #f )
		)
	)					     			      				 
    ;;			  (pretty-print proceed-flag)))
    auth-v)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Affiliations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define affiliations-regexp
  ;; pulls out a single author
  ;; used in get-affils-alist
  (make-regexp ">[0-9]+</sup>[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\"\\/ ]+</li>" regexp/extended))

(define (recurse-affil-lst inlst outlist)
  ;; used in get-affils-alist
  (if (null? (cdr inlst))
      (begin
	(set! outlist (acons (caar inlst) (cdar inlst) outlist))
	outlist)
      (begin
	(set! outlist (acons (caar inlst) (cdar inlst) outlist))
	(recurse-affil-lst (cdr inlst) outlist))))


(define (get-affils-alist the-body)
  (let*(
	(coord2-start (string-match "<div class=\"affiliations\">" the-body ))
	(the-alist (if coord2-start
	 	       (let* (
	 		    (coord2-end (string-match " <ul class=\"identifiers\" id=\"full-view-identifiers\">" the-body ))
	 		    (affil-chunk (xsubstring the-body (match:start coord2-start) (match:start coord2-end)))
	 		    (affil-v (map match:substring (list-matches affiliations-regexp affil-chunk)))
			    (lst-affils (map extract-affiliations affil-v ))
			    ;;here we must recurse and build alist
			    (affils-alist (recurse-affil-lst lst-affils '()))
	 		    ;;(lst-affils (map extract-affiliations affil-v ))
			    )
		       ;;lst-affils)
	 	        affils-alist)
	 	     #f )))
    the-alist)
  )






















