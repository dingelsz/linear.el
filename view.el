(load (expand-file-name  "./issue.el"))
(require 'transient)
(require 'magit-section)

;; -----------------------------------------------------------------------------
;; Transient
;; -----------------------------------------------------------------------------
(defun test-function ()
  (interactive)
  (message "Test function"))

(define-transient-command linear-transient ()
  "Test Transient Title"
  ["Actions"
   ("a" "Action a" test-function)
   ("b" "Action a" test-function)])


;; -----------------------------------------------------------------------------
;; Major Mode
;; -----------------------------------------------------------------------------
(defvar linear-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") 'linear-transient)
    (define-key map (kbd "q") (lambda () (interactive) (delete-window)))
    map))

(define-derived-mode linear-mode
  magit-section-mode "Linear")

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
         (linear-mode)
         (magit-insert-section (demo-buffer)
           ,@body))
       (beginning-of-buffer)
       (magit-section-forward)
       )))

(defun linear ()
  (with-demo-buffer
    (magit-insert-section (issues)
      (magit-insert-heading (lr-format "S P - Title" "[tags+][project]" (window-width)))
      (dolist (issue (issues))
	(magit-insert-section
	 (issues)
	 (insert (format "%s\n" (to-section issue (window-width)))))))))
