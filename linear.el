;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------
(defun keyword-name (keyword)
  "Gets the name of a keyword after the `:`"
  (when (not (keywordp keyword)) (error "Can only use keyword-name on keywords"))
  (subseq (symbol-name keyword) 1))

;; Test
;; (keyword-name :test)
;; (keyword-name 'test)
;; -----------------------------------------------------------------------------

(defun graphql-encode (q)
  "Encodes an sexp as a graphql query"
  (cond
   ((listp q)
    (cond
     ((keywordp (first q)) (format "( %s : \"%s\")" (keyword-name (first q)) (second q)))
     (t (format "{ %s }" (mapconcat 'graphql-encode q " ")))))
   ((symbolp q) (symbol-name q))
   (t q)))

;; Tests
;;  (graphql-encode "{this {is {a nested query}}}")
;;  (graphql-encode '(this (is (a nested query))))
;;  (graphql-encode '(issue (:id "BLA-123") (nodes (id title))))
;; -----------------------------------------------------------------------------
(defun jpath (pattern data)
  "jpath is to sexp what xpath is to xml. It is a query language for retrieving data from sexp."
  (if (null pattern) data
    (let* ((cmd (car pattern))
	   (rst (cdr pattern))
	   (data (cond
		  ((symbolp cmd) (cdr (assoc cmd data)))
		  ((arrayp cmd) (let ((idx (aref cmd 0)))
				  ((numberp idx) (aref data idx)))))))
      (if (null rst) data
	(if (arrayp data)
	    (mapcar (lambda (di) (mapcar (lambda (x) (jpath x di)) rst)) data)
	  (mapcan (lambda (x) (jpath x data)) rst))))))

;; Testing
;; (setq testdata '((data
;; 		  (issues
;; 		   (nodes .
;; 			  [((id . "abcdef")
;; 			    (title . "Learn the alphabet"))
;; 			   ((id . "123456")
;; 			    (title . "Number theory"))
;; 			   ((id . "zyxwvu")
;; 			    (title . "Learn the alphabet backwards"))
;; 			   ((id . "qwerty")
;; 			    (title . "Make a keyboard"))])))))
;; Tests
;; (setq data (linear-query `(issues (nodes (priority title labels (nodes (name)))))))
;; (jpath nil nil)
;; (jpath '(data (issues)) testdata)
;; (jpath '(data (issues (nodes (id)))) testdata)
;; (jpath '(data (issues (nodes))) testdata)
;; (jpath '(data (issues (nodes (id)))) testdata)
;; (jpath '(data (issues (nodes (title) (id)))) testdata)
;; (jpath '(data (issues (nodes (priority) (title)))) data)
;; (jpath '(data (issues (nodes (priority) (title) (labels (nodes (name)))))) data)

;; -----------------------------------------------------------------------------
;; Graphql query
;; -----------------------------------------------------------------------------
(defun linear-query (query)
  (let ((q (graphql-encode query))
	(result ""))
    (request
      "https://api.linear.app/graphql"
      :type "POST"
      :sync t
      :headers `(("Content-Type" . "application/json")
		 ("Authorization" . ,(getenv "LINEAR_APP_KEY")))
      :data (json-encode `(("query" . ,q)))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq result data)))
      :error (cl-function
	      (lambda (&key data &allow-other-keys)
		(let ((err_msg (cdr (assoc 'message (aref (cdr (assoc 'errors data)) 0)))))
		  (error err_msg)))))
    result))

;; Tests
;; (linear-query '(issues (nodes (id title))))
;; (linear-query '(nothing))  ;; throws a useful error

;; -----------------------------------------------------------------------------
;; Queries
;; -----------------------------------------------------------------------------
(defun linear-issues (&rest fields)
  (message "%s" `(issues (nodes ,@fields)))
  (jpath `(data (issues (nodes ,@(mapcar #'list fields))))
	 (linear-query `(issues (nodes ,fields)))))

;; Tests
;; (linear-issues 'title 'priority)



