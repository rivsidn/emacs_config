;;; format-elisp.el --- Format Emacs Lisp files -*- lexical-binding: t; -*-

(require 'seq)

(defun format-elisp--target-files ()
  "Return Emacs Lisp files to format."
  (let ((args (remove "--" command-line-args-left)))
    (if args
        args
      (seq-remove
       (lambda (file)
         (string-prefix-p (expand-file-name ".git" default-directory)
                          (expand-file-name file default-directory)))
       (directory-files-recursively default-directory "\\.el\\'")))))

(dolist (file (format-elisp--target-files))
  (find-file file)
  (emacs-lisp-mode)
  (indent-region (point-min) (point-max))
  (save-buffer)
  (kill-buffer))

;;; format-elisp.el ends here
