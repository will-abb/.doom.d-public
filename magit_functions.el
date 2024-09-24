;;; magit_functions.el -*- lexical-binding: t; -*-

;; if you want to remove the domain from say github.com and use github as the directory, set the following:
(setq my/magit-clone-default-directory-remove-domain t)

(defun my/magit-clone-default-directory (git-clone-url)
  (let* ((parsed-git-clone-url (my/magit-clone-parse git-clone-url)))
    (cond ((string-prefix-p "git@" parsed-git-clone-url)
           (my/git-url-parse (cadr (split-string parsed-git-clone-url "git@"))))
          ((string-prefix-p "https://" parsed-git-clone-url)
           (my/git-https-url-parse parsed-git-clone-url)))))

(defun my/git-https-url-parse (my/git-https-url-parse)
  (let* ((url (url-generic-parse-url my/git-https-url-parse))
         (host (url-host url))
         (user-or-workspace-and-repo (substring (url-filename url) 1)))
    (my/git-url-parse (format "%s:%s" host user-or-workspace-and-repo))))

(defun my/git-url-parse (git-clone-url)
  (let* ((git-url-path-and-repo-split (string-split git-clone-url ":"))
         (uri (car git-url-path-and-repo-split))
         (uri-without-domain (car (split-string uri "\\.")))
         (user-or-workspace-and-repo-split (split-string (cadr git-url-path-and-repo-split) "/"))
         (user-or-workspace (car user-or-workspace-and-repo-split))
         (repository_name (car(split-string (cadr user-or-workspace-and-repo-split) ".git"))))
    (if (bound-and-true-p my/magit-clone-default-directory-remove-domain)
        (format "~/repositories/%s/%s" uri-without-domain user-or-workspace)
      (format "~/repositories/%s/%s" uri user-or-workspace))))

(defun my/list-ssh-keys ()
  "List all SSH keys in the ~/.ssh directory."
  (let ((ssh-dir (expand-file-name "~/.ssh/")))
    (mapcar (lambda (file) (file-name-nondirectory file))
            (directory-files ssh-dir t ".*\\.pub$"))))

(defun my/magit-clone-repository ()
  "Clone a repository using a specified SSH key."
  (interactive)
  (let* ((repo (magit-read-string "Clone repository"))
         (ssh-keys (my/list-ssh-keys))
         (selected-key (completing-read "Select SSH key: " ssh-keys nil t))
         (ssh-key (if (string-empty-p selected-key)
                      "~/.ssh/id_rsa"  ; Default SSH key
                    (format "~/.ssh/%s" (string-remove-suffix ".pub" selected-key)))) ; Custom SSH key
         (git-ssh-command (if (string-empty-p selected-key)
                              "ssh"  ; Use default SSH command
                            (format "ssh -i %s -o IdentitiesOnly=yes" ssh-key))) ; Use custom SSH key
         (repo-url (my/magit-clone-parse repo))
         (clone-directory (my/magit-clone-default-directory repo-url))
         (repo-name (my/magit-clone-url-to-name repo-url))
         (clone-directory-with-repo-name (format "%s/%s" clone-directory repo-name)))
    ;; Set GIT_SSH_COMMAND for the current command
    (let ((process-environment (cons (concat "GIT_SSH_COMMAND=" git-ssh-command) process-environment)))
      (if (file-directory-p clone-directory-with-repo-name)
          (magit-status clone-directory-with-repo-name)
        (magit-clone-internal repo-url clone-directory-with-repo-name nil))
      (message "SSH key used: %s\nrepo: %s\nrepo-name: %s\nclone-directory: %s\nrepo-url: %s\nclone-directory-with-repo-name: %s"
               ssh-key repo repo-name clone-directory repo-url clone-directory-with-repo-name))))

(defun my/magit-clone-url-to-name (url)
  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
       (match-string 1 url)))

(defun my/magit-clone-parse (git-clone-url)
  (if (or (string-prefix-p "git@" git-clone-url) (string-prefix-p "https://" git-clone-url))
      git-clone-url
    (cadr (split-string git-clone-url "git clone "))))

(defun my/magit-automatic-version-control ()
  "Stage all files, commit with a standard message, and push."
  (interactive)
  (magit-status) 
  (magit-stage-modified t)
  (sit-for 0.25)
  (execute-kbd-macro (kbd "y"))
  (magit-refresh-buffer)
  (magit-call-git "commit" "-m" "Commit automatically generated.")
  (magit-refresh-buffer)
  (magit-push-current-to-pushremote nil)
  (magit-refresh-buffer)
  ) 
