;; 初始化包管理并启用常用软件源
(require 'package)
(require 'xref)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; 关闭菜单栏
(menu-bar-mode -1)

;; dired 默认隐藏详细信息，只显示文件名
(setq dired-listing-switches "-alh --group-directories-first")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; 显示行号
(global-display-line-numbers-mode t)

;; 复制到系统粘贴板
(setq select-enable-clipboard t)

(defun my/clipboard-copy (text)
  "Copy TEXT to the system clipboard in terminal Emacs."
  (let ((process-connection-type nil))
    (cond
     ((executable-find "wl-copy")
      (let ((proc (start-process "wl-copy" nil "wl-copy" "-f" "-n")))
        (process-send-string proc text)
        (process-send-eof proc)))
     ((executable-find "xclip")
      (let ((proc (start-process "xclip" nil "xclip" "-selection" "clipboard")))
        (process-send-string proc text)
        (process-send-eof proc)))
     ((executable-find "pbcopy")
      (let ((proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))))

(defun my/clipboard-paste ()
  "Read text from the system clipboard in terminal Emacs."
  (cond
   ((executable-find "wl-paste")
    (string-trim-right (shell-command-to-string "wl-paste -n")))
   ((executable-find "xclip")
    (string-trim-right
     (shell-command-to-string "xclip -selection clipboard -o 2>/dev/null")))
   ((executable-find "pbpaste")
    (string-trim-right (shell-command-to-string "pbpaste")))))

(unless (display-graphic-p)
  (setq interprogram-cut-function #'my/clipboard-copy
        interprogram-paste-function #'my/clipboard-paste))

;; 启用自动不补全
(electric-pair-mode 1)

;; 退格按实际字符删除，不将 tab 展开为空格
(setq backward-delete-char-untabify-method nil)
(global-set-key [remap backward-delete-char-untabify] #'delete-backward-char)
(global-set-key (kbd "DEL") #'delete-backward-char)
(global-set-key (kbd "<backspace>") #'delete-backward-char)

;; 设置背景颜色
(load-theme 'tango-dark t)

;; 将备份和自动保存文件集中到单独目录
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory)))
  (make-directory backup-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,backup-dir t))
        auto-save-list-file-prefix (expand-file-name ".saves-" backup-dir)))

(defun my/open-dired-on-right ()
  "Open dired for the current directory in a right side window."
  (interactive)
  (let* ((dir (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-directory))
         (buf (dired-noselect dir)))
    (display-buffer-in-side-window
     buf
     '((side . right)
       (slot . 0)
       (window-width . 0.25)))
    (select-window (get-buffer-window buf))))

(global-set-key (kbd "<f9>") #'my/open-dired-on-right)

(let ((config-dir (file-name-directory (or load-file-name user-init-file))))
  (load (expand-file-name "file-type/markdown.el" config-dir))
  (load (expand-file-name "file-type/c.el" config-dir))
  (load (expand-file-name "path-type/index.el" config-dir)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(imenu-list markdown-preview-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
