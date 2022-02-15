(load (expand-file-name  "./issue.el"))
(require 'transient)
(require 'magit-section)

;; -----------------------------------------------------------------------------
;; Transient Actions
;; -----------------------------------------------------------------------------
(defun test-function ()
  (interactive)
  (message "Task created!"))

(defun linear-create-task ()
  (interactive)
  (let* ((title (read-from-minibuffer "Task's title: "))
	(description (read-from-minibuffer "Task's description: "))
	(team-name (ido-completing-read "Task's team: " (linear-teams) :REQUIRE-MATCH t))
	(teamId (linear-teams-id-for team-name))
	(project-name (ido-completing-read "Task's project: " (linear-projects-names) :REQUIRE-MATCH t))
	(projectId (linear-projects-id-for project-name))
	(issue (ISSUE :title title :description description :project projectId)))
    (create issue teamId)))

;; -----------------------------------------------------------------------------
;; Transient
;; -----------------------------------------------------------------------------
(define-transient-command linear-transient ()
  ["Tasks"
   ("c" "Create a new task" linear-create-task)
   ("d" "Delete the current task" linear-delete-task)]
  ["Display"
   ("s" "Sort by column" test-function)
   ("f" "Filter by column" test-function)])


;; -----------------------------------------------------------------------------
;; Major Mode
;; -----------------------------------------------------------------------------
(defvar linear-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") 'linear-transient)
    (define-key map (kbd "q") (lambda () (interactive) (if (= 1 (length (window-list))) (kill-buffer (current-buffer))
							   (delete-window))))
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
  (interactive)
  (with-demo-buffer
    (magit-insert-section (issues)
      (magit-insert-heading (lr-format "S P - Title" "[tags+][project]" (window-width)))
      (dolist (issue (issues))
	(magit-insert-section
	 (issues)
	 (insert (format "%s\n" (to-section issue (window-width)))))))))
