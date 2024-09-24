;;; custom_functions.el --- Description -*- lexical-binding: t; -*-

(defun my/copy-whole-buffer ()
  "Copy the entire buffer to clipboard."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (region-beginning) (region-end))))

(defun my/copy-and-delete-whole-buffer ()
  "Copy the entire buffer to clipboard and then delete its content."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (region-beginning) (region-end))
    (delete-region (region-beginning) (region-end))))

(defun my/search-will-in-project ()
  "Search for the pattern 'will' with something before or after it in the current project."
  (interactive)
  (require 'projectile)
  (let ((root (projectile-project-root)))
    (if root
        (progn
          ;; Run the grep
          (rgrep "[[:alnum:][:punct:]]will\\|will[[:alnum:][:punct:]]" "*" root)
          ;; Wait briefly for rgrep to initiate and then switch to the next window
          (run-with-timer 1 nil (lambda () (other-window 1))))
      (message "Not in a project"))))

(defun my/search-wil-in-project ()
  "Search for the pattern 'wil' with something before or after it in the current project."
  (interactive)
  (require 'projectile)
  (let ((root (projectile-project-root)))
    (if root
        (progn
          ;; Run the grep
          (rgrep "[[:alnum:][:punct:]]wil0\\|wil0[[:alnum:][:punct:]]" "*" root)
          ;; Wait briefly for rgrep to initiate and then switch to the next window
          (run-with-timer 1 nil (lambda () (other-window 1))))
      (message "Not in a project"))))

(defun my/search-quote-in-project ()
  "Search for the pattern 'quote' with something before or after it in the current project."
  (interactive)
  (require 'projectile)
  (let ((root (projectile-project-root)))
    (if root
        (progn
          ;; Run the grep
          (rgrep "[[:alnum:][:punct:]]quote\\|quote[[:alnum:][:punct:]]" "*" root)
          ;; Wait briefly for rgrep to initiate and then switch to the next window
          (run-with-timer 1 nil (lambda () (other-window 1))))
      (message "Not in a project"))))


(defun my/paste-at-end-of-line ()
  "Pastes the clipboard content at the end of the current line with a space."
  (interactive)
  (end-of-line)   ; Move to the end of the line
  (insert " ")    ; Insert a space
  (clipboard-yank)  ; Paste the clipboard content
  (when (bound-and-true-p evil-mode)
    (evil-normal-state)))  ; Switch to normal mode if using Evil

(defun my/insert-newline-and-return-to-insert()
  "Inserts a newline or continues a list in Org mode and returns to insert mode."
  (interactive)
  (end-of-line)   ; Move to the end of the line
  (if (eq major-mode 'org-mode)
      (org-meta-return)  ; In Org mode, continue the list
    (newline-and-indent))  ; In other modes, just insert a newline
  (when (bound-and-true-p evil-mode)
    (evil-insert-state)))  ; Switch back to normal mode if using Evil

(defun my/markdown-preview-fullscreen ()
  "Enable markdown-live-preview-mode and then maximize the window."
  (interactive)
  (markdown-live-preview-mode)
  ;; Wait a brief moment to allow markdown-live-preview-mode to activate
  (run-with-timer 0.5 nil #'doom/window-maximize-buffer))

(defun my/replace-buffer-with-clipboard ()
  "Replace the entire buffer content with the clipboard content."
  (interactive)
  (delete-region (point-min) (point-max))  ; Delete the entire buffer content
  (clipboard-yank)                        ; Insert clipboard content
  (goto-char (point-min)))                ; Optionally move the cursor to the beginning


(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name (without path) to the clipboard."
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (progn
        (kill-new (file-name-nondirectory filename))
        (message "Copied file name '%s' to the clipboard" (file-name-nondirectory filename)))
    (message "No file is currently open")))

(defun my/wrap-with-anki-cloze ()
  "Toggle wrapping the selected text or the current word with {{c1::<text>}}."
  (interactive)
  (let (start end)
    (if (use-region-p)
        (setq start (region-beginning) 
              end (region-end))
      (save-excursion
        (backward-word)
        (setq start (point))
        (forward-word)
        (setq end (point))))
    (let ((region-text (buffer-substring start end)))
      (if (string-match "^{{c1::\\(.*?\\)}}$" region-text)
          (let ((unwrapped-text (match-string 1 region-text)))
            (delete-region start end)
            (insert unwrapped-text))
        (delete-region start end)
        (insert (concat "{{c1::" region-text "}}"))))))

(defun my/pyvenv-activate ()
  "Activate a Python virtual environment by selecting from a list with completion."
  (interactive)
  (let ((venv-dir (file-name-as-directory (expand-file-name "virtual_environments" "~")))
        (venvs (directory-files (expand-file-name "virtual_environments" "~") nil "\\w+")))
    (pyvenv-activate (expand-file-name (completing-read "Select venv: " venvs) venv-dir))))

(defun my/pyvenv-create (venv-name)
  "Create and activate a Python virtual environment in ~/virtual_environments.
VENV-NAME is the name of the virtual environment."
  (interactive (list (read-string "Enter venv name: ")))
  (let ((venv-dir (expand-file-name venv-name "~/virtual_environments")))
    (if (file-directory-p venv-dir)
        (message "A virtual environment named '%s' already exists." venv-name)
      (progn
        (shell-command (format "python3 -m venv %s" (shell-quote-argument venv-dir)))
        (message "Virtual environment '%s' created." venv-name)))))

(defun my/eval-last-sexp-on-line ()
  "Evaluates the last Emacs Lisp expression on the current line."
  (interactive)
  (end-of-line)
  (eval-last-sexp nil))

(defun my/copy-lines-containing-phrase (phrase)
  "Copy all lines containing the specified PHRASE to the clipboard."
  (interactive "sEnter phrase to search: ")
  (let ((buffer-contents (buffer-string))
        lines)
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (while (search-forward-regexp (regexp-quote phrase) nil t)
        (setq lines (append lines (list (thing-at-point 'line t)))))
      (when lines
        (kill-new (string-join lines "")))
      (message "Copied lines containing '%s' to clipboard" phrase))))

                                        ;
(defun my/copy-filename-and-contents ()
  "Copy the current buffer's file name and its contents to the clipboard in a formatted way."
  (interactive)
  (let ((filename (buffer-file-name))
        (contents (buffer-string)))
    (if filename
        (progn
          (kill-new (concat "File Name: " (file-name-nondirectory filename) "\n"
                            "File Contents:\n"
                            contents))
          (message "Successfully copied file name and contents."))
      (message "Buffer does not have a file name."))))

(defun my-toggle-greader ()
  "Toggle GReader mode and reading. If GReader is actively reading, stop it and turn off the mode. Otherwise, activate the mode and start GReader."
  (interactive)
  (if my-greader-reading-active
      (progn
        (greader-stop) 
        (greader-mode -1)
        (setq my-greader-reading-active nil) 
        (message "GReader stopped and mode turned off."))
    (progn
      (greader-mode 1) 
      (greader-read) 
      (setq my-greader-reading-active t)
      (message "GReader mode activated and started."))))

(defun my-docker-bash ()
  "Simulates pressing 'R', then 'R' in uppercase, typing '/bin/bash', and pressing Enter."
  (interactive)
  (execute-kbd-macro "R")
  (execute-kbd-macro "R")
  (execute-kbd-macro "/bin/bash") (execute-kbd-macro (kbd "RET")))

(defun my-docker-kali ()
  "Runs the 'williseed2000/kali:latest' Docker image interactively in vterm and removes it after exit."
  (interactive)
  (vterm)
  (vterm-send-string "docker run --gpus all -it --rm williseed2000/kali:latest /bin/bash\n")
  (delete-other-windows))

(defun my/close-buffer-and-window ()
  (interactive)
  (kill-buffer)
  (evil-window-delete))

(defun my/chmod-current-file (mode)
  "Change the mode of the current buffer's file to MODE.
  If MODE is numeric (e.g., '644'), it's applied directly.
  If MODE includes non-numeric characters (e.g., 'g+w'), it's assumed to be symbolic and converted accordingly."
  (interactive
   (list (read-string "Mode (numeric or symbolic): ")))
  (unless (buffer-file-name)
    (error "Buffer is not associated with a file"))
  (let ((file (buffer-file-name))
        (num-mode (if (string-match-p "^[0-9]+$" mode)
                      (string-to-number mode 8)  ; Direct conversion for numeric mode
                    (file-modes-symbolic-to-number mode (file-modes (buffer-file-name))))))  ; Convert symbolic to numeric
    (set-file-modes file num-mode)
    (message "Changed mode of %s to %s" (buffer-file-name) mode)))

(defun my/git-config-add-github-aws-origins (repo-path)
  "Update the .git/config for a given repository located at REPO-PATH."
  (interactive "DSelect repository directory: ")
  (let* ((git-config-path (expand-file-name ".git/config" repo-path))
         (config-contents (with-temp-buffer
                            (insert-file-contents git-config-path)
                            (buffer-string)))
         (repo-name (if (string-match "url = git@bitbucket.org:[^/]+/\\([^/]+\\)\\.git" config-contents)
                        (match-string 1 config-contents)
                      nil)))
    (when repo-name
      (with-temp-file git-config-path
        (insert config-contents)
        ;; Add AWS CodeCommit as a new remote, if not already present
        (goto-char (point-min)) 
        (unless (re-search-forward "url = ssh://git-codecommit.us-east-1.amazonaws.com/v1/repos/.+\\.git" nil t)
          (goto-char (point-max))
          (insert (format "\n[remote \"origin\"]\nurl = ssh://git-codecommit.us-east-1.amazonaws.com/v1/repos/%s.git\nfetch = +refs/heads/*:refs/remotes/aws-codecommit/*\n" repo-name))
          (message "Updated .git/config for AWS CodeCommit with repository: %s, preserving Bitbucket origin." repo-name))
        ;; Add GitHub as a new remote, if not already present
        (goto-char (point-min))
        (unless (re-search-forward "url = git@github.com:.+\\.git" nil t)
          (goto-char (point-max))
          ;;this uses the github user name
          (insert (format "[remote \"origin\"]\nurl = git@github.com:%s/%s.git\nfetch = +refs/heads/*:refs/remotes/github/*\n" "will-abb" repo-name))
          (message "Updated .git/config for GitHub with repository: %s." repo-name))
        )))
  ;;call git script to update several giconfig settings
  (shell-command (format "cd %s && bash ~/repositories/bitbucket/williseed1/configs/repos-git/config.sh" repo-path))
  )

(defun my/copy-current-directory-path ()
  "Copy the current directory path to the clipboard."
  (interactive)
  (let ((dir-path (expand-file-name default-directory)))
    (kill-new dir-path)
    (message "Copied directory path: %s" dir-path)))

(defun my/ensure-frame-maximized ()
  "Maximize the current frame if it isn't maximized already."
  (unless (frame-parameter nil 'fullscreen)
    (toggle-frame-maximized)))

(defun my-disable-ws-butler ()
  " This disables Butler Mode to prevent Last Space from being auto deleted."
  (ws-butler-mode -1))

(defun my/goto-first-complete-quote ()
  "Move the cursor to the beginning of the first set of complete quotes on the line."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (and (string-match "\\(\"[^\"]*\"\\|'[^']*'\\)" line)
             (= (match-beginning 0) (match-end 0)))
        (message "No complete quotes found on this line.")
      (goto-char (+ (line-beginning-position) (match-beginning 0) 1))
      (evil-insert 1))))

(defun toggle-highlight-and-mark-line ()
  "Toggle highlighting on the current line using overlays and set an Evil marker."
  (interactive)
  (let ((overlays (overlays-at (point)))
        (found nil))
    (dolist (ov overlays)
      (when (eq (overlay-get ov 'type) 'highlight-line)
        (delete-overlay ov)
        (setq found t)))
    (if found
        (message "Highlight and marker removed.")
      (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
        (overlay-put ov 'face 'highlight)
        (overlay-put ov 'type 'highlight-line)
        (evil-set-marker ?')
        (message "Line highlighted and marked with 'm'.")))))

(map! :map vterm-mode-map
      :desc "Scroll up one line in VTerm"
      "C-c C-k" #'vterm--self-insert)

(defun vterm-scroll-up-one-line ()
  "Scroll up one line in VTerm."
  (interactive)
  (vterm-send-string "\e[1A" t))  ;; Sending the escape sequence for moving up one line

(map! :map vterm-mode-map
      :desc "Scroll up one line in VTerm"
      "C-c C-k" #'vterm-scroll-up-one-line)


(defun wrap-region-with-src-block (start end lang)
  "Wrap the selected region with an Org mode source block.
   START and END are positions defining the region. LANG is the language for the source block."
  (interactive "r\nsLanguage for source block: ")
  (save-excursion
    (goto-char end) (insert "\n#+END_SRC")
    (goto-char start) (insert "#+BEGIN_SRC " lang "\n")))


(defun delete-all-empty-lines ()
  "Delete all completely empty lines in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\s*\n" nil t)
      (let ((beg (match-beginning 0)))
        (replace-match "")
        (goto-char beg)))))

(defun delete-consecutive-empty-lines ()
  "Delete occurrences of two consecutive empty lines, leave single empty lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\s*\n\s*\n\s*" nil t)
      (replace-match "\n"))))

;;fix copy paste replacement issues
(defun my/paste-and-delete-region ()
  "Delete the region, if active, without copying it, then paste from clipboard."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (yank))
(define-key evil-visual-state-map (kbd "p") 'my/paste-and-delete-region)


(defun my/kill-region-advice (orig-fun &rest args)
  "Kill text between BEG and END without affecting the system clipboard."
  (let ((interprogram-cut-function nil))
    (apply orig-fun args)))

(defun my/projectile-switch-project-to-dir ()
  "Switch to a project, open the project root in Dired mode, and create a new workspace."
  (interactive)
  (let ((original-action projectile-switch-project-action)
        (project-name (projectile-project-name)))
    (unwind-protect
        (progn
          (setq projectile-switch-project-action
                (lambda ()
                  (dired (projectile-project-root))
                  (when (featurep 'persp-mode)
                    (if (persp-get-by-name project-name)
                        (persp-switch project-name)
                      (progn
                        (persp-switch project-name)
                        (persp-rename project-name))))))
          (projectile-switch-project))
      (setq projectile-switch-project-action original-action))))

(defun my/extract-cloudwatch-log-json-messages ()
  "Extracts and displays the 'message' field from each object in the 'events' array of a JSON file in a fullscreen buffer."
  (interactive)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json-buffer (current-buffer))
         (json-text (with-current-buffer json-buffer
                      (buffer-string)))
         (json-data (json-read-from-string json-text))
         (events (gethash "events" json-data))
         (messages (mapcar (lambda (event) (gethash "message" event)) events))
         (output-buffer (get-buffer-create "*Extracted Messages*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (dolist (message messages)
        (insert (concat message "\n")))
      (let ((window (display-buffer output-buffer)))
        (delete-other-windows window)
        (set-window-dedicated-p window t)
        (text-mode)))))

(defun my/delete-specified-lines (scope)
  "Delete lines containing specific key-value pairs based on the user's choice of SCOPE."
  (interactive
   (list (completing-read "Scope (file/directory): " '("file" "directory") nil t nil nil "file")))
  (let ((patterns '("environment = \"dev\""
                    "environment = \"uat\""
                    "environment = \"prod\""
                    "owner       = \"devops\""
                    "managed     = \"terraform\""
                    "repo        = \"https://bitbucket.org/SelectQuote/devops-terraform\""
                    "repo        = \"https://bitbucket.org/SelectQuote/devops-aws-terraform\""
                    )))
    (cond ((string= scope "file")
           (delete-matching-lines patterns (point-min) (point-max)))
          ((string= scope "directory")
           (dolist (file (directory-files "." t "\\.tf$"))
             (when (file-regular-p file)
               (with-temp-file file
                 (insert-file-contents file)
                 (delete-matching-lines patterns (point-min) (point-max)))))))))

(defun my/delete-matching-lines (patterns start end)
  "Delete lines matching any of PATTERNS between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward (regexp-opt patterns) end t)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point))))))

(provide 'delete-specified-lines)


(defun my/insert-asterisks-and-space ()
  "Insert '** ' based on the content of the current line.
   If the line is empty (contains only whitespace), insert at the beginning of the line.
   If the line contains other characters, move to the next line, create a new line, insert '** ', and enter insert mode."
  (interactive)
  (let ((line-content (string-trim (thing-at-point 'line t))))
    (if (string-empty-p line-content)
        (progn
          (beginning-of-line)
          (insert "** ")
          (evil-insert 1))
      (progn
        (end-of-line)
        (newline-and-indent)
        (insert "** ")
        ;; (evil-insert 1)
        ))))


(defun my-vterm-or-kill-buffer ()
  "If the current buffer is a vterm buffer, put it in insert mode, send `C-c` twice with a pause, and then send EOF. Otherwise, kill the buffer."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (progn
        (evil-insert-state)
        (vterm-send-C-c)
        (sleep-for 0.05)
        (vterm-send-C-c)
        (sleep-for 0.05) 
        (vterm-send-C-d)) 
    (kill-buffer (current-buffer))))

(defun open-downloads-directory ()
  "Open the Downloads directory in dired mode."
  (interactive)
  (dired "~/Downloads"))
(map! :leader
      :desc "Open Downloads directory"  ; Optional description
      "D" #'open-downloads-directory)

(defun get-window-app-info ()
  "Run xprop command to get information about a window by clicking on it."
  (interactive)
  (async-shell-command "xprop"))

(map! :leader
      :desc "Dired Jump" 
      "d j" #'dired-jump)



(provide 'custom_functions.el)
;;; custom_functions.el ends here
