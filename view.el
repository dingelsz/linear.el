(load (expand-file-name  "./linear.el"))

;; -----------------------------------------------------------------------------
;; Controller
;; -----------------------------------------------------------------------------
(defun issues ()
  (sort (jpath '(data (issues (nodes (priority) (title) (labels (nodes (name))))))
	       (linear-query `(issues (nodes (priority title labels (nodes (name)))))))
	(lambda (x y) (< (car x) (car y)))))

(defun format-issue (issue)
  (let* ((priority (nth 0 issue))
	(title (nth 1 issue))
	(labels (concat "[" (mapconcat #'car (nth 2 issue) "][") "]"))
	(left-msg (format "%d - %s" priority title))
	(right-align (- (window-width) (length left-msg) 2))
	(fmt-template (format "%%s %%%ds\n" right-align)))
    (format fmt-template left-msg labels)))

;; -----------------------------------------------------------------------------
;; View: Sections
;; -----------------------------------------------------------------------------
(defmacro with-demo-buffer (&rest body)
  (declare (indent 0))
  `(let ((buffer (get-buffer-create "*Linear*")))
     (with-current-buffer buffer
       (switch-to-buffer-other-window buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (magit-section-mode)
         (magit-insert-section (demo-buffer)
           ,@body)))))

(defun linear-view ()
  (with-demo-buffer
    (magit-insert-section (issues)
      (magit-insert-heading "P - Title [tags]")
      (dolist (issue (issues))
	(magit-insert-section (issues)
	  (insert (format-issue issue)))))))



