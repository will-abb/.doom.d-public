;;; aws_helper.el -*- lexical-binding: t; -*-

(defvar my/ssm-ssh-users
  '("root" "ubuntu" "ec2-user" "admin" "centos" "bitnami" "fedora" "debian" "cloud-user" "redhat" "transfer")
  "List of default SSH users for EC2 instances.")

(defvar my/aws-profile-list '(
                              "marketing-read" "marketing" "devops-read" "devops" "cloud-trail" "aws-management-read"
                              "aws-management" "inside-response-read" "inside-response" "uat-read" "uat" "sox-read"
                              "sox" "healthcareservices-read" "healthcareservices" "prod-read" "prod" "dev-read"
                              "dev" "dev-appdev" "playground-read" "playground" "shared-services-read" "shared-services"
                              "data-science-read" "data-science" "marketing-read-east" "marketing-east" "devops-read-east"
                              "devops-east" "cloud-trail-east" "aws-management-read-east" "aws-management-east"
                              "inside-response-read-east" "inside-response-east" "uat-read-east" "uat-east" "sox-read-east"
                              "sox-east" "healthcareservices-read-east" "healthcareservices-east" "prod-read-east" "prod-east"
                              "dev-read-east" "dev-east" "dev-appdev-east" "playground-read-east" "playground-east"
                              "shared-services-read-east" "shared-services-east" "data-science-read-east" "data-science-east"
                              "selectquote-finance" "log-archive" "audit" "selectquote-finance-east" "log-archive-east" "audit-east"
                              )
  "List of AWS profiles.")

(defun my/set-last-aws-profile-used ()
  "Set the `my/last-used-aws-profile` variable by selecting from a list of predefined AWS profiles."
  (interactive)
  (let ((profile (completing-read "Select AWS profile: " my/aws-profile-list nil t)))
    (if (not (string-empty-p profile))
        (progn
          (setenv "AWS_PROFILE" profile) ;; Optionally set an environment variable if needed.
          (setq my/last-used-aws-profile profile)
          (message "AWS profile set to: %s" profile))
      (message "No profile selected!"))))


(defun my/set-and-export-aws-profile ()
  "Set the `my/last-used-aws-profile` variable and export it as an environment variable."
  (interactive)
  (let ((profile (completing-read "Select AWS profile: " my/aws-profile-list nil t)))
    (if (not (string-empty-p profile))
        (progn
          (setenv "AWS_PROFILE" profile)
          (setq my/last-used-aws-profile profile)
          (message "AWS profile set and exported to: %s" profile))
      (message "No profile selected!"))))


(defun my/set-ssm-ssh-default-user ()
  "Set the `my/ssm-ssh-default-user` variable by selecting from a list of common EC2 SSH users."
  (interactive)
  (let ((user (completing-read "Select default SSH user: " my/ssm-ssh-users nil t)))
    (if (not (string-empty-p user))
        (progn
          (setq my/ssm-ssh-default-user user)  ;; Update the Emacs variable.
          ;;(setenv "SSH_DEFAULT_USER" user)     ;; Optionally set an environment variable.
          (message "Default SSH user set to: %s" user))
      (message "No SSH user selected!"))))

(defun my/set-and-export-aws-default-region ()
  "Set and export the `AWS_DEFAULT_REGION` environment variable."
  (interactive)
  (let ((region (completing-read "Select AWS region: " '("us-east-1" "us-west-2") nil t)))
    (if (not (string-empty-p region))
        (progn
          (setenv "AWS_DEFAULT_REGION" region)
          (message "AWS default region set and exported to: %s" region))
      (message "No region selected!"))))
