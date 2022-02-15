(load (expand-file-name  "./linear.el"))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------
(setq linear-status-map
      '(("Backlog" . "B")
	("Todo" . "T")
	("In Progress" . "P")
	("Done" . "D")
	("Canceled" . "C")))

(defun format-bracket (items)
  (concat "[" (mapconcat #'car (remove-if (lambda (x) (null (car x))) items) "][") "]"))
;; Test
;; (format-bracket '(("Hello") (nil) ("World")))

(defun lr-format (lhs rhs width)
  (let ((num-spaces (- width (length lhs) 2)))
    (format (format "%%s %%%ds" num-spaces) lhs rhs)))

;; Test
;; (lr-format "hello" "world" (window-size))

;; -----------------------------------------------------------------------------
;; Issue Class
;; -----------------------------------------------------------------------------
(defclass ISSUE ()
  ((description :initarg :description :initform "Description of an issue")
   (id :initarg :id :initform nil)
   (priority :initarg :priority :initform 5)
   (project :initarg :project :initform nil)
   (status :initarg :status :initform "B")
   (tags :initarg :tags :initform ())
   (title :initarg :title :initform "Issue")))

(defun ISSUE-from-list (items)
  (cl-destructuring-bind (description priority project status tags title id) items
    (ISSUE :description description
	   :id id
	   :priority priority
	   :project project
	   :status (cdr (assoc status linear-status-map))
	   :tags tags
	   :title title)))

(cl-defmethod to-section ((issue ISSUE) width)
  (with-slots (title priority tags project description status) issue
    (let* ((lhs (format "%s %d - %s" status priority title))
	  (rhs-items (reverse (cons (list project) tags)))
	  (rhs (format "%s" (format-bracket rhs-items))))
      (lr-format lhs rhs width))))

(setq LINEAR_Q_CREATE "mutation IssueCreate { issueCreate( input: { title: \\\"%s\\\" description: \\\"%s\\\" projectId: \\\"%s\\\" teamId: \\\"%s\\\" } ) { success issue { id title } } }")

(cl-defmethod create ((issue ISSUE) teamID)
  (with-slots (title description project) issue
    (linear-request (format LINEAR_Q_CREATE title description project teamID))))

(setq LINEAR_Q_REMOVE "mutation IssueDelete { issueDelete( id : \\\"%s\\\" ) { success } }")

(cl-defmethod rm ((issue ISSUE))
  (with-slots (id) issue
    (linear-request (format LINEAR_Q_REMOVE id))))

;; Test
;; (setq test-issue (ISSUE :title 'Test :description "hello world" :project "Dev" :tags '(("Feature") ("Improvement")) :description "hello world"))
;; (to-section (ISSUE :title 'Test :tags '(("Feature") ("Improvement"))) (window-width))
;; (setq x (create test-issue "46696a01-d887-4389-81e0-2949416563fc"))
;; (rm test-issue)

;; -----------------------------------------------------------------------------
;; API
;; -----------------------------------------------------------------------------
(defun issues ()
  (let* ((jpath-query '(data (issues (nodes (description) (priority) (project (name)) (state (name)) (labels (nodes (name))) (title) (id)))))
	(graphql-query `(issues (nodes (project (name) priority id title description state (name) labels (nodes (name))))))
	(data (jpath jpath-query (linear-query graphql-query)))
	(data (mapcar #'ISSUE-from-list data))
	(data (sort data (lambda (x y) (< (oref x :priority) (oref y :priority))))))
    data))

;; Tests
;; (issues)

(defun open-issues ()
  (let* ((jpath-query '(data (issues (nodes (description) (priority) (project (name)) (state (name)) (labels (nodes (name))) (title)))))
	(graphql-query `(issues (nodes (project (name) priority title description state (name) labels (nodes (name))))))
	(data (jpath jpath-query (linear-query graphql-query)))
	(data (mapcar #'ISSUE-from-list data))
	(data (sort data (lambda (x y) (< (oref x :priority) (oref y :priority))))))
    data))
