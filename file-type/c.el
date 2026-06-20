;; 编程和代码阅读相关设置

;; 编译与搜索结果自动滚动
(setq compilation-scroll-output t)

;; 设置 Linux C 代码风格
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")
            (setq indent-tabs-mode t)
            (setq tab-width 8)
            (hs-minor-mode 1)
            (imenu-add-menubar-index)))

(defun my/find-project-file-upward (filename)
  "Find FILENAME by searching upward from `default-directory'."
  (locate-dominating-file default-directory filename))

(defun my/visit-project-tags ()
  "Visit the nearest TAGS file from the current directory."
  (interactive)
  (let ((root (my/find-project-file-upward "TAGS")))
    (if root
        (visit-tags-table (expand-file-name "TAGS" root))
      (user-error "No TAGS file found in parent directories"))))

(defun my/cscope-root ()
  "Return the nearest directory containing cscope.out."
  (my/find-project-file-upward "cscope.out"))

(defun my/cscope-read-symbol (prompt)
  "Read a symbol for cscope with PROMPT, defaulting to symbol at point."
  (let ((symbol (thing-at-point 'symbol t)))
    (read-string
     (if symbol
         (format "%s (default %s): " prompt symbol)
       (concat prompt ": "))
     nil nil symbol)))

(defun my/cscope-query (search-type query)
  "Run cscope SEARCH-TYPE for QUERY and display results through xref."
  (let ((root (my/cscope-root)))
    (unless root
      (user-error "No cscope.out found in parent directories"))
    (let* ((default-directory root)
           (lines (process-lines "cscope" "-d" "-L" (format "-%d" search-type) query))
           (xrefs
            (mapcar
             (lambda (line)
               (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\(.*\\)$" line)
                   (let ((file (match-string 1 line))
                         (context (match-string 4 line))
                         (line-number (string-to-number (match-string 3 line))))
                     (xref-make
                      context
                      (xref-make-file-location
                       (expand-file-name file root) line-number 0)))
                 (xref-make line (xref-make-bogus-location line))))
             lines)))
      (if xrefs
          (xref--show-xrefs xrefs nil)
        (message "No cscope results for %s" query)))))

(defun my/cscope-find-symbol (symbol)
  "Find all references to SYMBOL with cscope."
  (interactive (list (my/cscope-read-symbol "Find symbol")))
  (my/cscope-query 0 symbol))

(defun my/cscope-find-definition (symbol)
  "Find global definition of SYMBOL with cscope."
  (interactive (list (my/cscope-read-symbol "Find global definition")))
  (my/cscope-query 1 symbol))

(defun my/cscope-find-callers (symbol)
  "Find callers of SYMBOL with cscope."
  (interactive (list (my/cscope-read-symbol "Find callers of function")))
  (my/cscope-query 3 symbol))

(defun my/cscope-find-text (text)
  "Find TEXT in files indexed by cscope."
  (interactive "sFind text string: ")
  (my/cscope-query 4 text))

(defun my/linux-kernel-read-mode ()
  "Convenient key bindings for reading large C codebases."
  (local-set-key (kbd "M-.") #'xref-find-definitions)
  (local-set-key (kbd "M-,") #'xref-go-back)
  (local-set-key (kbd "M-?") #'xref-find-references)
  (local-set-key (kbd "C-c t") #'my/visit-project-tags)
  (local-set-key (kbd "C-c s s") #'my/cscope-find-symbol)
  (local-set-key (kbd "C-c s d") #'my/cscope-find-definition)
  (local-set-key (kbd "C-c s c") #'my/cscope-find-callers)
  (local-set-key (kbd "C-c s t") #'my/cscope-find-text))

(add-hook 'c-mode-common-hook #'my/linux-kernel-read-mode)
