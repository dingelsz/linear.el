;; POST https://api.linear.app/graphql
;; Authorization: lin_api_sjMhzwpbakT6asIZGHotggfOQ1myrWahupjlgq21
;; Content-Type: application/json
;; {
;; 	"query": "mutation IssueCreate { issueCreat( input: { title: \"New exception\" teamId: \"46696a01-d887-4389-81e0-2949416563fc\" } ) { success issue { id title } } }"
;; }

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
     ((keywordp (first q)) (format "( %s : %s)" (keyword-name (first q)) (graphql-encode (rest q))))
     (t (format "{ %s }" (mapconcat 'graphql-encode q " ")))))
   ((symbolp q) (symbol-name q))
   ((stringp q) (format "\"%s\"" q))
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

(defun linear-request (graphql)
  (let ((result ""))
    (request
      "https://api.linear.app/graphql"
      :type "POST"
      :sync t
      :headers `(("Content-Type" .  "application/json")
		 ("Authorization" . ,(getenv "LINEAR_APP_KEY")))
      :data (format "{ \"query\" : \"%s\" }" graphql)
      :parser 'json-read ;; reads input
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq result data)))
      :error (cl-function
	      (lambda (&key data response error-thrown &allow-other-keys)
		(let ((err_msg (cdr (assoc 'message (aref (cdr (assoc 'errors data)) 0)))))
		  (error err_msg)))))
    result))

(defun linear-query (query)
  (linear-request (graphql-encode query)))

;; Tests
(linear-query '(issues (nodes (id title))))
;;(linear-query '(teams (nodes (id name))))

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

;; -----------------------------------------------------------------------------
;; Teams
;; -----------------------------------------------------------------------------
(defun linear-teams ()
  (jpath `(data (teams (nodes (name) (id))))
	 (linear-query '(teams (nodes (name id))))))

(defun linear-teams-names ()
  (mapcar #'first  (linear-teams)))

(defun linear-teams-id-for (name)
  (second (assoc name (linear-teams))))
;; Test
;; (linear-teams)
;; (linear-teams-names)
;; (linear-teams-id-for (first (linear-teams-names)))

;; -----------------------------------------------------------------------------
;; Projects
;; -----------------------------------------------------------------------------
(defun linear-projects ()
  (jpath '(data (projects (nodes (name) (id))))
	    (linear-query '(projects (nodes (id name))))))

(defun linear-projects-names ()
  (mapcar #'first (linear-projects)))


(defun linear-projects-id-for (name)
  (second (assoc name (linear-projects))))

;; Test
;; (linear-projects)
;; (linear-projects-names)
;; (linear-projects-id-for (first (linear-projects-names)))

