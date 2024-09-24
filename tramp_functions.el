;;; tramp_functions.el -*- lexical-binding: t; -*-


(defun my/tramp-docker-init ()
  "Pre-fill the minibuffer with the Docker TRAMP prefix, clearing any existing text."
  (interactive)
  (let ((prefix "/docker:"))
    (minibuffer-with-setup-hook
        (lambda ()
          (delete-minibuffer-contents)
          (insert prefix))
      (call-interactively 'find-file))))

(defun my/tramp-sudo-init ()
  "Pre-fill the minibuffer with the sudo TRAMP prefix, clearing any existing text."
  (interactive)
  (let ((prefix "/sudo::/"))
    (minibuffer-with-setup-hook
        (lambda ()
          (delete-minibuffer-contents)
          (insert prefix))
      (call-interactively 'find-file))))

(defun my/tramp-ssh-init ()
  "Pre-fill the minibuffer with the SSH TRAMP prefix, clearing any existing text."
  (interactive)
  (let ((prefix "/ssh:"))
    (minibuffer-with-setup-hook
        (lambda ()
          (delete-minibuffer-contents)
          (insert prefix))
      (call-interactively 'find-file))))
