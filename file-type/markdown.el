;; Markdown 编辑相关设置
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'markdown-mode))

(unless (package-installed-p 'markdown-preview-mode)
  (package-refresh-contents)
  (package-install 'markdown-preview-mode))

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files." t)
(autoload 'markdown-preview-mode "markdown-preview-mode"
  "Realtime preview Markdown in the browser." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

(setq markdown-command
      (cond
       ((executable-find "pandoc")
        '("pandoc" "--from=gfm" "--to=html5"))
       ((executable-find "markdown_py")
        '("markdown_py" "-x" "extra" "-x" "sane_lists"))
       (t markdown-command))
      markdown-command-needs-filename nil)

(with-eval-after-load 'markdown-mode
  (setq markdown-asymmetric-header t)
  (define-key markdown-mode-command-map (kbd "p") #'markdown-preview-mode))

;; 安装并配置 imenu-list 侧边栏
(unless (package-installed-p 'imenu-list)
  (package-refresh-contents)
  (package-install 'imenu-list))

(autoload 'imenu-list-smart-toggle "imenu-list" "Toggle the imenu-list sidebar." t)
(setq imenu-list-focus-after-activation t
      imenu-list-auto-resize nil
      imenu-list-position 'left
      imenu-list-size 20)

(defun my/imenu-list-open-and-focus ()
  "Open imenu-list and focus it."
  (interactive)
  (if (get-buffer-window "*Ilist*" t)
      (select-window (get-buffer-window "*Ilist*" t))
    (imenu-list-minor-mode 1)))

(defun my/markdown-open-imenu-list ()
  "Automatically open imenu-list for Markdown buffers without stealing focus."
  (when (derived-mode-p 'markdown-mode)
    (let ((imenu-list-focus-after-activation nil))
      (imenu-list-minor-mode 1))))

;; 关闭自动打开imenu-list 功能
;; (add-hook 'markdown-mode-hook #'my/markdown-open-imenu-list)
;; (global-set-key (kbd "<f8>") #'my/imenu-list-open-and-focus)
