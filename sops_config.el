;;; sops_config.el -*- lexical-binding: t; -*-


(defun sops-setup-env ()
  "Extract the AWS account ID from a SOPS file and set the AWS_PROFILE accordingly."
  (save-excursion
    (goto-char (point-min))
    ;; Search for the sequence of 'sops' > 'kms' > 'arn' with any non-alphanumeric characters in between
    (if (re-search-forward "arn:aws:kms.*:\\([[:digit:]]+\\):" nil t)
        (let* ((account-id (match-string 1))
               (aws-profile-name
                (cond ((string= account-id "442893166637") "ir")
                      ((string= account-id "463931586000") "uat")
                      ((string= account-id "690994262422") "dev")
                      ((string= account-id "375777745621") "util")
                      ((string= account-id "144487101178") "market")
                      ((string= account-id "790685595415") "play")
                      ((string= account-id "586672212047") "prod")
                      ((string= account-id "923261199481") "datsci")
                      ((string= account-id "526083761887") "health")
                      ((string= account-id "473742870145") "sox")
                      ((string= account-id "208648672950") "devops-play")
                      ((string= account-id "309059759820") "log-athena")
                      (t (message "No matching AWS profile for account ID: %s" account-id)
                         nil))))
          (when aws-profile-name
            (setenv "AWS_PROFILE" aws-profile-name)
            (message "AWS_PROFILE set to %s" aws-profile-name)))
      (message "No AWS KMS ARN found in the SOPS file structure specified."))))

;; Assuming use-package is configured properly in your Emacs
(use-package! sops
  :config
  (setq sops-before-encrypt-decrypt-hook 'sops-setup-env)
  (global-sops-mode 1))

