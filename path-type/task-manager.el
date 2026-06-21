;;; task-manager.el --- Config for task manager org file -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'subr-x)

(defgroup my/task-manager nil
  "Task manager helpers for the path-specific Org entry file."
  :group 'org)

(defcustom my/task-manager-product-line-file
  (expand-file-name "local/相关产线.txt" user-emacs-directory)
  "Plain text file that contains product line candidates."
  :type '(choice (const :tag "Not configured" nil) file)
  :group 'my/task-manager)

(defcustom my/task-manager-module-file
  (expand-file-name "local/功能模块.txt" user-emacs-directory)
  "Plain text file that contains function module candidates."
  :type '(choice (const :tag "Not configured" nil) file)
  :group 'my/task-manager)

(defcustom my/task-manager-product-model-file nil
  "Plain text file that contains product model candidates."
  :type '(choice (const :tag "Not configured" nil) file)
  :group 'my/task-manager)

(defconst my/task-manager--root-heading "task manager")
(defconst my/task-manager--task-section "工作任务")
(defconst my/task-manager--idea-section "工作想法")
(defconst my/task-manager--task-summary "工作总结：")
(defconst my/task-manager--idea-summary "最终总结：")
(defconst my/task-manager--todo-keywords
  '((sequence "TODO(t)" "DOING(g)" "PAUSED(p)" "|"
              "DONE(d)" "CANCELLED(c)")))
(defconst my/task-manager--quick-choice-letters "abcdefghijklmnopqrstuvwxyz")

(defvar-local my/task-manager--entry-buffer nil
  "Non-nil means the current buffer is the task manager entry file.")

(defvar my/task-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t c") #'my/task-manager-create-task)
    (define-key map (kbd "C-c t i") #'my/task-manager-create-idea)
    (define-key map (kbd "C-c t s") #'my/task-manager-start)
    (define-key map (kbd "C-c t p") #'my/task-manager-pause)
    (define-key map (kbd "C-c t d") #'my/task-manager-finish)
    (define-key map (kbd "C-c t o") #'my/task-manager-open-detail)
    (define-key map (kbd "C-c t r") #'my/task-manager-report)
    map)
  "Local keymap for the task manager entry file.")

(define-minor-mode my/task-manager-mode
  "Minor mode for the task manager entry file."
  :lighter " TaskMgr"
  :keymap my/task-manager-mode-map)

(defun my/task-manager--assert-entry-buffer ()
  "Signal an error unless the current buffer is the task manager entry file."
  (unless (bound-and-true-p my/task-manager--entry-buffer)
    (user-error "当前缓冲区不是 task-manager 入口文件")))

(defun my/task-manager--apply-org-settings ()
  "Apply task-manager Org settings to the current buffer."
  (setq-local org-todo-keywords my/task-manager--todo-keywords)
  (setq-local org-priority-highest ?A)
  (setq-local org-priority-lowest ?C)
  (setq-local org-default-priority ?B)
  (setq-local org-clock-into-drawer "LOGBOOK")
  (when (derived-mode-p 'org-mode)
    (let ((default-todo-keywords (default-value 'org-todo-keywords)))
      ;; `org-set-regexps-and-options' derives todo regexps from the default
      ;; value when the file does not declare #+TODO keywords.
      (unwind-protect
          (progn
            (setq-default org-todo-keywords my/task-manager--todo-keywords)
            (org-set-regexps-and-options))
        (setq-default org-todo-keywords default-todo-keywords)))))

(defun my/task-manager--read-non-empty (prompt)
  "Read a non-empty string with PROMPT."
  (let ((value ""))
    (while (string-empty-p (string-trim value))
      (setq value (read-string prompt))
      (when (string-empty-p (string-trim value))
        (message "内容不能为空")))
    (string-trim value)))

(defun my/task-manager--choice-letter (index)
  "Return the quick letter for zero-based INDEX."
  (when (< index (length my/task-manager--quick-choice-letters))
    (aref my/task-manager--quick-choice-letters index)))

(defun my/task-manager--choice-explicit-key (candidate key-alist)
  "Return explicit quick key for CANDIDATE from KEY-ALIST."
  (let ((key (car (cl-find candidate key-alist :key #'cdr :test #'string=))))
    (cond
     ((characterp key) key)
     ((and (stringp key) (> (length key) 0))
      (aref key 0)))))

(defun my/task-manager--choice-key (candidate index key-alist)
  "Return quick key for CANDIDATE at zero-based INDEX."
  (or (my/task-manager--choice-explicit-key candidate key-alist)
      (when (< index 9)
        (+ ?1 index))
      (my/task-manager--choice-letter (- index 9))))

(defun my/task-manager--choice-items (candidates key-alist)
  "Return `read-multiple-choice' items for CANDIDATES and KEY-ALIST."
  (cl-loop for candidate in candidates
           for index from 0
           collect
           (list (my/task-manager--choice-key candidate index key-alist)
                 candidate)))

(defun my/task-manager--choice-help (prompt choices)
  "Return a help string for PROMPT and CHOICES without duplicated keys."
  (concat prompt "\n\n"
          (mapconcat
           (lambda (choice)
             (format "%s: %s"
                     (key-description (char-to-string (car choice)))
                     (cadr choice)))
           choices
           "\n")))

(defun my/task-manager--read-choice
    (prompt candidates &optional default key-alist)
  "Read a value from CANDIDATES with PROMPT and DEFAULT.
Use native `read-multiple-choice' for short lists and
`completing-read' for long lists."
  (let ((default (or default (car candidates))))
    (if (<= (length candidates)
            (+ 9 (length my/task-manager--quick-choice-letters)))
        (let ((choices (my/task-manager--choice-items candidates key-alist)))
          (cadr (read-multiple-choice
                 prompt choices
                 (my/task-manager--choice-help prompt choices))))
      (completing-read (format "%s: " prompt)
                       candidates nil t nil nil default))))

(defun my/task-manager--read-priority ()
  "Read an Org priority and return it as A, B or C."
  (let ((value (my/task-manager--read-choice
                "优先级" '("A" "B" "C") "B"
                '(("a" . "A") ("b" . "B") ("c" . "C")))))
    (if (string-empty-p value)
        "B"
      (upcase (substring value 0 1)))))

(defun my/task-manager--read-candidates-file (file)
  "Return candidates from FILE, ignoring empty lines and comments."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let (candidates)
        (dolist (line (split-string (buffer-string) "\n"))
          (let ((candidate (string-trim line)))
            (unless (or (string-empty-p candidate)
                        (string-prefix-p "#" candidate)
                        (member candidate candidates))
              (push candidate candidates))))
        (nreverse candidates)))))

(defun my/task-manager--read-external-value (prompt file)
  "Read a value for PROMPT from candidates in FILE, falling back to input."
  (let ((candidates (my/task-manager--read-candidates-file file)))
    (if candidates
        (my/task-manager--read-choice prompt candidates)
      (read-string (format "%s（列表未配置，手动输入）: " prompt)))))

(defun my/task-manager--heading-title ()
  "Return current Org heading title without todo, priority or tags."
  (string-trim (org-get-heading t t t t)))

(defun my/task-manager--find-heading (level title &optional bound)
  "Move to Org heading LEVEL with TITLE before BOUND.
Return non-nil when a matching heading is found."
  (let ((regexp (format "^\\*\\{%d\\}[ \t]+%s[ \t]*\\(?:[ \t]+:.*:\\)?[ \t]*$"
                        level (regexp-quote title))))
    (when (re-search-forward regexp bound t)
      (beginning-of-line)
      t)))

(defun my/task-manager--find-root ()
  "Move to the task-manager root heading.
Return non-nil when the root heading is found."
  (goto-char (point-min))
  (my/task-manager--find-heading 1 my/task-manager--root-heading))

(defun my/task-manager--subtree-end-position ()
  "Return the end position of the current Org subtree."
  (save-excursion
    (org-end-of-subtree t t)
    (point)))

(defun my/task-manager--local-variables-position ()
  "Return the position of a file local variables block, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^# Local Variables:" nil t)
      (line-beginning-position))))

(defun my/task-manager--content-end-position ()
  "Return the insertion boundary before file local variables."
  (or (my/task-manager--local-variables-position) (point-max)))

(defun my/task-manager--find-section (section)
  "Move to task-manager SECTION.
Return non-nil when SECTION exists under the task-manager root."
  (when (my/task-manager--find-root)
    (let ((end (my/task-manager--subtree-end-position)))
      (forward-line 1)
      (my/task-manager--find-heading 2 section end))))

(defun my/task-manager--append-section (section)
  "Append SECTION to the task-manager root."
  (unless (my/task-manager--find-root)
    (user-error "缺少 task manager 根节点"))
  (let ((end (min (my/task-manager--subtree-end-position)
                  (my/task-manager--content-end-position))))
    (goto-char end)
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert (format "\n** %s\n" section))))

(defun my/task-manager--ensure-structure ()
  "Ensure the task manager root and section headings exist."
  (my/task-manager--assert-entry-buffer)
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (save-excursion
    (unless (my/task-manager--find-root)
      (goto-char (my/task-manager--content-end-position))
      (unless (or (bobp) (eq (char-before) ?\n))
        (insert "\n"))
      (insert (format "\n* %s\n\n** %s\n\n** %s\n"
                      my/task-manager--root-heading
                      my/task-manager--task-section
                      my/task-manager--idea-section)))
    (unless (save-excursion (my/task-manager--find-section
                             my/task-manager--task-section))
      (if (save-excursion (my/task-manager--find-section
                           my/task-manager--idea-section))
          (progn
            (my/task-manager--find-section my/task-manager--idea-section)
            (insert (format "** %s\n\n" my/task-manager--task-section)))
        (my/task-manager--append-section my/task-manager--task-section)))
    (unless (save-excursion (my/task-manager--find-section
                             my/task-manager--idea-section))
      (my/task-manager--append-section my/task-manager--idea-section))))

(defun my/task-manager--section-end-position (section)
  "Return insertion position at the end of SECTION."
  (unless (my/task-manager--find-section section)
    (user-error "缺少 %s 节点" section))
  (my/task-manager--subtree-end-position))

(defun my/task-manager--property-drawer-bounds ()
  "Return bounds of current entry property drawer as (START . END)."
  (save-excursion
    (org-back-to-heading t)
    (let ((entry-end (save-excursion
                       (outline-next-heading)
                       (point)))
          start)
      (forward-line 1)
      (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" entry-end t)
        (setq start (line-beginning-position))
        (when (re-search-forward "^[ \t]*:END:[ \t]*$" entry-end t)
          (cons start (line-end-position)))))))

(defun my/task-manager--entry-property (name)
  "Return current entry property NAME, or nil when it does not exist."
  (save-excursion
    (let ((bounds (my/task-manager--property-drawer-bounds)))
      (when bounds
        (goto-char (car bounds))
        (when (re-search-forward
               (format "^[ \t]*:%s:[ \t]*\\(.*\\)$" (regexp-quote name))
               (cdr bounds) t)
          (string-trim (match-string-no-properties 1)))))))

(defun my/task-manager--set-entry-property (name value)
  "Set current entry property NAME to VALUE."
  (let* ((value (or value ""))
         (line (if (string-empty-p value)
                   (format ":%s:" name)
                 (format ":%s: %s" name value))))
    (save-excursion
      (org-back-to-heading t)
      (let ((bounds (my/task-manager--property-drawer-bounds)))
        (if bounds
            (let ((drawer-end (copy-marker (cdr bounds))))
              (goto-char (car bounds))
              (if (re-search-forward
                   (format "^[ \t]*:%s:[ \t]*.*$" (regexp-quote name))
                   drawer-end t)
                  (replace-match line t t)
                (goto-char drawer-end)
                (beginning-of-line)
                (insert line "\n"))
              (set-marker drawer-end nil))
          (forward-line 1)
          (insert ":PROPERTIES:\n" line "\n:END:\n"))))))

(defun my/task-manager--section-title-for-entry ()
  "Return the level-2 section title for the current entry."
  (save-excursion
    (org-back-to-heading t)
    (while (and (> (org-outline-level) 2)
                (org-up-heading-safe)))
    (when (= (org-outline-level) 2)
      (my/task-manager--heading-title))))

(defun my/task-manager--root-title-for-entry ()
  "Return the level-1 root title for the current entry."
  (save-excursion
    (org-back-to-heading t)
    (while (and (> (org-outline-level) 1)
                (org-up-heading-safe)))
    (when (= (org-outline-level) 1)
      (my/task-manager--heading-title))))

(defun my/task-manager--kind-at-heading ()
  "Return `task' or `idea' for the current item heading."
  (when (and (= (org-outline-level) 3)
             (string= (my/task-manager--root-title-for-entry)
                      my/task-manager--root-heading))
    (let ((section (my/task-manager--section-title-for-entry)))
      (cond
       ((string= section my/task-manager--task-section) 'task)
       ((string= section my/task-manager--idea-section) 'idea)))))

(defun my/task-manager--goto-item-heading ()
  "Move to the current task or idea heading and return its kind."
  (my/task-manager--assert-entry-buffer)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前缓冲区不是 Org 模式"))
  (condition-case nil
      (org-back-to-heading t)
    (error (user-error "请把光标放在任务或想法节点内")))
  (while (and (> (org-outline-level) 3)
              (org-up-heading-safe)))
  (let ((kind (my/task-manager--kind-at-heading)))
    (unless kind
      (user-error "请把光标放在工作任务或工作想法条目内"))
    kind))

(defun my/task-manager--insert-entry (section text jump-label)
  "Insert TEXT into SECTION, then jump to JUMP-LABEL."
  (my/task-manager--ensure-structure)
  (let (start)
    (goto-char (my/task-manager--section-end-position section))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert "\n")
    (setq start (point))
    (insert text)
    (unless (string-suffix-p "\n\n" text)
      (insert "\n"))
    (goto-char start)
    (when (search-forward jump-label nil t)
      (end-of-line))))

(defun my/task-manager--created-time ()
  "Return a formatted creation timestamp."
  (format-time-string "%Y-%m-%d %H:%M"))

(defun my/task-manager-create-task (title priority work-type product-line module)
  "Create a work task with TITLE, PRIORITY, WORK-TYPE, PRODUCT-LINE and MODULE."
  (interactive
   (list (my/task-manager--read-non-empty "任务标题: ")
         (my/task-manager--read-priority)
         (my/task-manager--read-choice
          "工作类型" '("研发任务" "支持任务") "研发任务"
          '(("r" . "研发任务") ("s" . "支持任务")))
         (my/task-manager--read-external-value
          "相关产线" my/task-manager-product-line-file)
         (my/task-manager--read-external-value
          "功能模块" my/task-manager-module-file)))
  (my/task-manager--assert-entry-buffer)
  (let ((text (format
               "*** TODO [#%s] %s\n:PROPERTIES:\n:工作类型: %s\n:相关产线: %s\n:功能模块: %s\n:创建时间: %s\n:详情目录:\n:详情文件:\n:END:\n:LOGBOOK:\n:END:\n\n- 任务描述：\n- 工作总结：\n"
               priority title work-type product-line module
               (my/task-manager--created-time))))
    (my/task-manager--insert-entry my/task-manager--task-section
                                   text "任务描述："))
  (save-buffer))

(defun my/task-manager-create-idea (title priority)
  "Create a work idea with TITLE and PRIORITY."
  (interactive
   (list (my/task-manager--read-non-empty "想法标题: ")
         (my/task-manager--read-priority)))
  (my/task-manager--assert-entry-buffer)
  (let ((text (format
               "*** TODO [#%s] %s\n:PROPERTIES:\n:创建时间: %s\n:详情目录:\n:详情文件:\n:END:\n:LOGBOOK:\n:END:\n\n- 想法描述：\n- 最终总结：\n"
               priority title (my/task-manager--created-time))))
    (my/task-manager--insert-entry my/task-manager--idea-section
                                   text "想法描述："))
  (save-buffer))

(defun my/task-manager--sanitize-file-name (name)
  "Return NAME with unsafe file name characters replaced."
  (let ((safe (string-trim name)))
    (setq safe (replace-regexp-in-string "[/\\\\:*?\"<>|]+" "_" safe))
    (setq safe (replace-regexp-in-string "[[:cntrl:]]+" "" safe))
    (setq safe (replace-regexp-in-string "[ \t\n\r]+" " " safe))
    (setq safe (string-trim safe))
    (if (string-empty-p safe)
        "untitled"
      safe)))

(defun my/task-manager--directory-date ()
  "Return date part for a detail directory."
  (let ((created (my/task-manager--entry-property "创建时间")))
    (if (and created
             (string-match
              "\\([0-9]\\{4\\}\\)[-.]\\([0-9]\\{2\\}\\)[-.]\\([0-9]\\{2\\}\\)"
              created))
        (format "%s.%s.%s"
                (match-string 1 created)
                (match-string 2 created)
                (match-string 3 created))
      (format-time-string "%Y.%m.%d"))))

(defun my/task-manager--unique-directory (parent basename)
  "Return a unique directory under PARENT based on BASENAME."
  (let ((index 1)
        (candidate (expand-file-name basename parent)))
    (while (file-exists-p candidate)
      (setq index (1+ index))
      (setq candidate
            (expand-file-name (format "%s-%d" basename index) parent)))
    candidate))

(defun my/task-manager--expand-manager-path (path)
  "Expand PATH relative to the task-manager entry file."
  (expand-file-name path (file-name-directory buffer-file-name)))

(defun my/task-manager--relative-manager-path (path)
  "Return PATH relative to the task-manager entry file."
  (file-relative-name path (file-name-directory buffer-file-name)))

(defun my/task-manager--detail-template (kind title work-type)
  "Return detail template for KIND, TITLE and WORK-TYPE."
  (cond
   ((eq kind 'idea)
    (format "* %s\n\n** 想法描述\n\n" title))
   ((string= work-type "支持任务")
    (format
     "* %s\n:PROPERTIES:\n:客户名称:\n:软件版本:\n:内核分支:\n:内核版本:\n:产品型号:\n:供应商:\n:END:\n\n** 问题现象\n\n"
     title))
   (t
    (format "* %s\n\n** 背景信息\n\n" title))))

(defun my/task-manager--detail-paths ()
  "Return detail path data for the current task or idea."
  (let* ((kind (my/task-manager--goto-item-heading))
         (title (my/task-manager--heading-title))
         (safe-title (my/task-manager--sanitize-file-name title))
         (manager-dir (file-name-directory buffer-file-name))
         (dir-prop (my/task-manager--entry-property "详情目录"))
         (file-prop (my/task-manager--entry-property "详情文件"))
         (dir (cond
               ((and dir-prop (not (string-empty-p dir-prop)))
                (my/task-manager--expand-manager-path dir-prop))
               ((and file-prop (not (string-empty-p file-prop)))
                (file-name-directory
                 (my/task-manager--expand-manager-path file-prop)))
               (t
                (my/task-manager--unique-directory
                 manager-dir
                 (format "%s-%s" (my/task-manager--directory-date)
                         safe-title)))))
         (file (if (and file-prop (not (string-empty-p file-prop)))
                   (my/task-manager--expand-manager-path file-prop)
                 (expand-file-name (concat safe-title ".org") dir))))
    (list :kind kind :title title :dir dir :file file
          :work-type (my/task-manager--entry-property "工作类型"))))

(defun my/task-manager-open-detail ()
  "Open or create the detail file for the current task or idea."
  (interactive)
  (my/task-manager--assert-entry-buffer)
  (let* ((paths (my/task-manager--detail-paths))
         (kind (plist-get paths :kind))
         (title (plist-get paths :title))
         (dir (plist-get paths :dir))
         (file (plist-get paths :file))
         (work-type (plist-get paths :work-type))
         (created-file (not (file-exists-p file))))
    (make-directory dir t)
    (when created-file
      (with-temp-file file
        (insert (my/task-manager--detail-template kind title work-type))))
    (my/task-manager--set-entry-property
     "详情目录" (my/task-manager--relative-manager-path dir))
    (my/task-manager--set-entry-property
     "详情文件" (my/task-manager--relative-manager-path file))
    (save-buffer)
    (find-file file)))

(defun my/task-manager--set-state (state)
  "Set current Org heading todo STATE."
  (my/task-manager--apply-org-settings)
  (org-todo state))

(defun my/task-manager--clocking-current-entry-p ()
  "Return non-nil when the current entry owns the active Org clock."
  (and (org-clocking-p)
       (markerp org-clock-marker)
       (eq (marker-buffer org-clock-marker)
           (or (buffer-base-buffer) (current-buffer)))
       (let ((clock-position (marker-position org-clock-marker))
             (begin (save-excursion
                      (org-back-to-heading t)
                      (point)))
             (end (save-excursion
                    (org-back-to-heading t)
                    (org-end-of-subtree t t)
                    (point))))
         (and (>= clock-position begin)
              (< clock-position end)))))

(defun my/task-manager--clear-stale-clock ()
  "Clear stale Org clock state."
  (when (bound-and-true-p org-clock-mode-line-timer)
    (cancel-timer org-clock-mode-line-timer)
    (setq org-clock-mode-line-timer nil))
  (when (bound-and-true-p org-clock-idle-timer)
    (cancel-timer org-clock-idle-timer)
    (setq org-clock-idle-timer nil))
  (when (markerp org-clock-marker)
    (move-marker org-clock-marker nil))
  (when (markerp org-clock-hd-marker)
    (move-marker org-clock-hd-marker nil))
  (setq org-clock-current-task nil)
  (setq global-mode-string
        (delq 'org-mode-line-string global-mode-string))
  (org-clock-restore-frame-title-format)
  (force-mode-line-update)
  (message "已清理失效的 Org 计时状态"))

(defun my/task-manager--clock-out-active ()
  "Stop the active Org clock, tolerating stale Org clock markers."
  (when (org-clocking-p)
    (condition-case err
        (org-clock-out)
      (error
       (if (string= (error-message-string err) "Clock start time is gone")
           (my/task-manager--clear-stale-clock)
         (signal (car err) (cdr err)))))))

(defun my/task-manager--clock-in-current ()
  "Start an Org clock for the current heading, tolerating stale old clocks."
  (condition-case err
      (let ((org-clock-into-drawer "LOGBOOK"))
        (org-clock-in))
    (error
     (if (string= (error-message-string err) "Clock start time is gone")
         (progn
           (my/task-manager--clear-stale-clock)
           (let ((org-clock-into-drawer "LOGBOOK"))
             (org-clock-in)))
       (signal (car err) (cdr err))))))

(defun my/task-manager-start ()
  "Start the current task or idea and open its detail file."
  (interactive)
  (my/task-manager--assert-entry-buffer)
  (let (heading)
    (save-excursion
      (my/task-manager--goto-item-heading)
      (unless (member (org-get-todo-state) '("TODO" "DOING" "PAUSED"))
        (user-error "只有 TODO、DOING 或 PAUSED 状态可以开始"))
      (setq heading (point-marker))
      (goto-char heading)
      (my/task-manager--set-state "DOING")
      (my/task-manager--clock-in-current)
      (save-buffer))
    (set-marker heading nil))
  (my/task-manager-open-detail))

(defun my/task-manager-pause ()
  "Pause the current DOING task or idea."
  (interactive)
  (my/task-manager--assert-entry-buffer)
  (let (heading)
    (my/task-manager--goto-item-heading)
    (unless (string= (org-get-todo-state) "DOING")
      (user-error "只有 DOING 状态可以暂停"))
    (setq heading (point-marker))
    (when (my/task-manager--clocking-current-entry-p)
      (my/task-manager--clock-out-active))
    (goto-char heading)
    (my/task-manager--set-state "PAUSED")
    (save-buffer)
    (set-marker heading nil)))

(defun my/task-manager--goto-summary (kind)
  "Move to the summary line for KIND."
  (let ((label (if (eq kind 'idea)
                   my/task-manager--idea-summary
                 my/task-manager--task-summary))
        (end (save-excursion
               (org-end-of-subtree t t)
               (point))))
    (if (re-search-forward (regexp-quote label) end t)
        (goto-char (match-end 0))
      (goto-char end)
      (unless (bolp)
        (insert "\n"))
      (insert (format "- %s\n" label))
      (search-backward label)
      (goto-char (match-end 0)))))

(defun my/task-manager-finish ()
  "Finish the current task or idea and jump to its summary area."
  (interactive)
  (my/task-manager--assert-entry-buffer)
  (let (heading kind)
    (setq kind (my/task-manager--goto-item-heading))
    (unless (member (org-get-todo-state) '("TODO" "DOING" "PAUSED"))
      (user-error "只有 TODO、DOING 或 PAUSED 状态可以结束"))
    (setq heading (point-marker))
    (when (my/task-manager--clocking-current-entry-p)
      (my/task-manager--clock-out-active))
    (goto-char heading)
    (my/task-manager--set-state "DONE")
    (my/task-manager--goto-summary kind)
    (save-buffer)
    (set-marker heading nil)))

(defun my/task-manager--parse-duration (duration start end)
  "Return minutes from DURATION or timestamps START and END."
  (cond
   ((and duration
         (string-match "\\([0-9]+\\):\\([0-9][0-9]\\)"
                       (string-trim duration)))
    (+ (* 60 (string-to-number (match-string 1 duration)))
       (string-to-number (match-string 2 duration))))
   ((and start end)
    (floor (/ (float-time (time-subtract end start)) 60)))
   (t 0)))

(defun my/task-manager--parse-clocks (begin end)
  "Parse Org clock lines between BEGIN and END."
  (let (clocks)
    (save-excursion
      (goto-char begin)
      (while (re-search-forward
              "^[ \t]*CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\]\\(?:[ \t]*=>[ \t]*\\([0-9]+:[0-9][0-9]\\)\\)?"
              end t)
        (let* ((start-text (match-string-no-properties 1))
               (end-text (match-string-no-properties 2))
               (duration (match-string-no-properties 3))
               (start-time (org-time-string-to-time
                            (concat "[" start-text "]")))
               (end-time (org-time-string-to-time
                          (concat "[" end-text "]")))
               (minutes (my/task-manager--parse-duration
                         duration start-time end-time)))
          (push (list :day (substring start-text 0 10)
                      :week (format-time-string "%Y-W%W" start-time)
                      :minutes minutes)
                clocks))))
    (nreverse clocks)))

(defun my/task-manager--priority-char ()
  "Return current heading priority character."
  (let ((priority (nth 3 (org-heading-components))))
    (cond
     ((integerp priority) priority)
     ((characterp priority) priority)
     (t org-default-priority))))

(defun my/task-manager--collect-items ()
  "Collect task-manager items from the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (when (= (org-outline-level) 3)
          (let ((kind (my/task-manager--kind-at-heading)))
            (when kind
              (let* ((begin (point))
                     (end (my/task-manager--subtree-end-position))
                     (clocks (my/task-manager--parse-clocks begin end))
                     (minutes (cl-loop for clock in clocks
                                       sum (plist-get clock :minutes))))
                (push (list :kind kind
                            :title (my/task-manager--heading-title)
                            :todo (org-get-todo-state)
                            :priority (my/task-manager--priority-char)
                            :created (or (my/task-manager--entry-property
                                          "创建时间")
                                         "")
                            :work-type (or (my/task-manager--entry-property
                                            "工作类型")
                                           "")
                            :product-line (or (my/task-manager--entry-property
                                               "相关产线")
                                              "")
                            :module (or (my/task-manager--entry-property
                                         "功能模块")
                                        "")
                            :clocks clocks
                            :minutes minutes)
                      items)))))
        (forward-line 1)))
    (nreverse items)))

(defun my/task-manager--minutes-string (minutes)
  "Format MINUTES as H:MM."
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))

(defun my/task-manager--hash-add (table key minutes)
  "Add MINUTES to KEY in hash TABLE."
  (when (> minutes 0)
    (puthash (if (string-empty-p key) "未填写" key)
             (+ minutes (gethash (if (string-empty-p key) "未填写" key)
                                 table 0))
             table)))

(defun my/task-manager--hash-rows (table &optional by-minutes)
  "Return rows from TABLE, optionally sorted BY-MINUTES descending."
  (let (rows)
    (maphash (lambda (key minutes)
               (push (cons key minutes) rows))
             table)
    (sort rows
          (if by-minutes
              (lambda (a b)
                (if (= (cdr a) (cdr b))
                    (string< (car a) (car b))
                  (> (cdr a) (cdr b))))
            (lambda (a b)
              (string< (car a) (car b)))))))

(defun my/task-manager--insert-report-table (title key-label rows)
  "Insert report table TITLE with KEY-LABEL and ROWS."
  (insert (format "* %s\n\n" title))
  (insert (format "| %s | 耗时 |\n|---+---|\n" key-label))
  (if rows
      (dolist (row rows)
        (insert (format "| %s | %s |\n"
                        (car row)
                        (my/task-manager--minutes-string (cdr row)))))
    (insert "| 无数据 | 0:00 |\n"))
  (insert "\n"))

(defun my/task-manager--kind-label (kind)
  "Return display label for item KIND."
  (if (eq kind 'idea) "工作想法" "工作任务"))

(defun my/task-manager--insert-priority-table (items)
  "Insert priority sorted active ITEMS."
  (insert "* 优先级排序\n\n")
  (insert "| 类型 | 优先级 | 创建时间 | 状态 | 标题 | 累计耗时 |\n")
  (insert "|---+---+---+---+---+---|\n")
  (let ((active
         (sort
          (cl-remove-if
           (lambda (item)
             (member (plist-get item :todo) '("DONE" "CANCELLED")))
           (copy-sequence items))
          (lambda (a b)
            (let ((pa (plist-get a :priority))
                  (pb (plist-get b :priority)))
              (if (= pa pb)
                  (string< (plist-get a :created)
                           (plist-get b :created))
                (< pa pb)))))))
    (if active
        (dolist (item active)
          (insert (format "| %s | [#%c] | %s | %s | %s | %s |\n"
                          (my/task-manager--kind-label
                           (plist-get item :kind))
                          (plist-get item :priority)
                          (plist-get item :created)
                          (or (plist-get item :todo) "")
                          (plist-get item :title)
                          (my/task-manager--minutes-string
                           (plist-get item :minutes)))))
      (insert "| 无数据 |  |  |  |  | 0:00 |\n"))))

(defun my/task-manager--build-report (items)
  "Build task-manager report for ITEMS in the current buffer."
  (let ((by-day (make-hash-table :test 'equal))
        (by-week (make-hash-table :test 'equal))
        (by-work-type (make-hash-table :test 'equal))
        (by-product-line (make-hash-table :test 'equal))
        (by-module (make-hash-table :test 'equal))
        (by-kind (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((kind (plist-get item :kind)))
        (dolist (clock (plist-get item :clocks))
          (let ((minutes (plist-get clock :minutes)))
            (my/task-manager--hash-add
             by-day (plist-get clock :day) minutes)
            (my/task-manager--hash-add
             by-week (plist-get clock :week) minutes)
            (my/task-manager--hash-add
             by-kind (my/task-manager--kind-label kind) minutes)
            (when (eq kind 'task)
              (my/task-manager--hash-add
               by-work-type (plist-get item :work-type) minutes)
              (my/task-manager--hash-add
               by-product-line (plist-get item :product-line) minutes)
              (my/task-manager--hash-add
               by-module (plist-get item :module) minutes))))))
    (insert (format "#+title: task-manager 统计报表\n#+date: %s\n\n"
                    (format-time-string "%Y-%m-%d %H:%M")))
    (my/task-manager--insert-report-table
     "按天统计" "日期" (my/task-manager--hash-rows by-day))
    (my/task-manager--insert-report-table
     "按周统计" "周" (my/task-manager--hash-rows by-week))
    (my/task-manager--insert-report-table
     "按工作类型统计" "工作类型"
     (my/task-manager--hash-rows by-work-type t))
    (my/task-manager--insert-report-table
     "按相关产线统计" "相关产线"
     (my/task-manager--hash-rows by-product-line t))
    (my/task-manager--insert-report-table
     "按功能模块统计" "功能模块"
     (my/task-manager--hash-rows by-module t))
    (my/task-manager--insert-report-table
     "按工作任务和工作想法统计" "类型"
     (my/task-manager--hash-rows by-kind t))
    (my/task-manager--insert-priority-table items)))

(defun my/task-manager-report ()
  "Show task-manager clock and priority report."
  (interactive)
  (my/task-manager--assert-entry-buffer)
  (let ((items (my/task-manager--collect-items))
        (buffer (get-buffer-create "*task-manager-report*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (my/task-manager--build-report items)
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buffer)))

(defun my/new-workspace-item ()
  "Compatibility wrapper for creating a task-manager work task."
  (interactive)
  (call-interactively #'my/task-manager-create-task))

(defun my/task-manager--setup-buffer ()
  "Enable task-manager behavior in the current buffer."
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (setq-local my/task-manager--entry-buffer t)
  (my/task-manager--apply-org-settings)
  (my/task-manager-mode 1))

(my/task-manager--setup-buffer)

(provide 'task-manager)
;;; task-manager.el ends here
