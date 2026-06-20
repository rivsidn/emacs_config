;;; task-manager.el --- Config for task manager org file -*- lexical-binding: t; -*-

(defun my/new-workspace-item ()
  "Create a dated workspace item beside the current task manager file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((title (read-string "工作标题: "))
         (date (format-time-string "%Y.%m.%d"))
         (name (concat date "-" title))
         (dir (expand-file-name name (file-name-directory buffer-file-name)))
         (entry (expand-file-name (concat title ".md") dir)))
    (make-directory dir t)
    (save-excursion
      (goto-char (point-max))
      ;; Avoid a literal Local Variables marker so Emacs does not parse this file as one.
      (when (re-search-backward (concat "^# Local " "Variables:") nil t)
        (beginning-of-line))
      (insert (format "\n** TODO %s\n\n- 目录：[[file:%s/]]\n- 文档：[[file:%s/%s.md]]\n\n\n"
                      name name name title)))
    (save-buffer)
    (find-file entry)))

(provide 'task-manager)
;;; task-manager.el ends here
