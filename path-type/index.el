;;; index.el --- Path-specific config loader -*- lexical-binding: t; -*-

(defvar my/path-type-configs
  '(("/home/yuchao/Documents/workspace/0000.00.00-task-manager.org" . "task-manager.el"))
  "Mapping from absolute file paths to path-specific config files.")

(defun my/path-type-config-for-file (file)
  "Return the path-specific config for FILE, or nil when none matches."
  (let ((configs my/path-type-configs)
        matched)
    (while (and configs (not matched))
      (let ((config (car configs)))
        (when (string= file (car config))
          (setq matched (cdr config))))
      (setq configs (cdr configs)))
    matched))

(defun my/load-path-type-config ()
  "Load path-specific config for the current buffer."
  (when buffer-file-name
    (let ((config (my/path-type-config-for-file buffer-file-name)))
      (when config
        (load (expand-file-name config
                                (expand-file-name "path-type" user-emacs-directory))
              nil t)))))

(add-hook 'find-file-hook #'my/load-path-type-config)
(my/load-path-type-config)

(provide 'path-type-index)
;;; index.el ends here
