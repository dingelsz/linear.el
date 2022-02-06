(load (expand-file-name  "./issue.el"))
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
           ,@body))
       (beginning-of-buffer)
       (magit-section-forward)
       )))

(defun linear-view ()
  (with-demo-buffer
    (magit-insert-section (issues)
      (magit-insert-heading (lr-format "S P - Title" "[tags+][project]" (window-width)))
      (dolist (issue (issues))
	(magit-insert-section (issues)
	  (insert (format "%s\n" (to-section issue (window-width)))))))))

