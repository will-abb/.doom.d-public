;;; tramp_ssm_ssh.el -*- lexical-binding: t; -*-

;; Connecting to an AWS Instance
;; `my/tramp-ssm-to-aws-instance`: Initiates a TRAMP session to an AWS instance. If the AWS profile is not given, it uses the last profile or defaults to "DEV". Adds your ssh key to server.

;; Listing Docker Containers
;; `my/list-docker-containers-ssm`: Lists Docker containers running on the specified EC2 instance using AWS SSM. 

;; Connecting to a Docker Container on an AWS Instance
;; `my/tramp-ssm-to-container`: Establishes a TRAMP session to a Docker container within an AWS instance. It requires an instance ID, a container ID, and an AWS profile. Adds your ssh key to server.

;; Ending a TRAMP SSM Session
;; `my/tramp-end-ssm-session-and-remove-key`: Cleans up all TRAMP buffers and connections and calls the function to remove the SSH key from the AWS instance.
;; 
;;you must set up ssh config, see useful.org or initial setup ssh file

;; ls -al /home/ec2-user/.ssh/ && cat /home/ec2-user/.ssh/authorized_keys
;; ls -al /root/.ssh/ && cat /root/.ssh/authorized_keys
(defvar my/last-used-aws-instance-id nil
  "Store the last used AWS instance ID for re-use in AWS-related functions.")
(defvar my/last-used-aws-profile "dev"
  "Store the last used AWS profile for re-use in AWS-related functions.")
(defvar my/ssm-ssh-default-user "root"
  "Default SSH user for SSM TRAMP connections.")

(defun my/tramp-ssm-to-aws-instance (&optional instance-id aws-profile)
  "Connect to an AWS instance using TRAMP with the given INSTANCE-ID and AWS-PROFILE.
   If INSTANCE-ID or AWS-PROFILE is not provided, prompts for them interactively.
   Uses `my/ssm-ssh-default-user` as the default SSH user."
  (interactive
   (list (read-string "Enter AWS Instance ID: " my/last-used-aws-instance-id)
         (let ((input (read-string (format "Enter AWS Profile (default %s): " my/last-used-aws-profile) nil nil my/last-used-aws-profile)))
           (if (string-empty-p input) my/last-used-aws-profile input))))
  (unless instance-id
    (setq instance-id (read-string "Enter AWS Instance ID: " my/last-used-aws-instance-id)))
  (unless aws-profile
    (setq aws-profile (let ((input (read-string (format "Enter AWS Profile (default %s): " my/last-used-aws-profile) nil nil my/last-used-aws-profile)))
                        (if (string-empty-p input) my/last-used-aws-profile input))))

  (let ((ssh-user (or (getenv "SSM_SSH_DEFAULT_USER") my/ssm-ssh-default-user))
        (aws-profile-env aws-profile))
    (my/add-ssh-key-to-instance instance-id aws-profile-env)
    (setenv "AWS_PROFILE" aws-profile-env)
    (let ((default-directory (format "/ssh:%s@%s:~" ssh-user instance-id)))
      (dired default-directory))
    (setq my/last-used-aws-instance-id instance-id)
    (setq my/last-used-aws-profile aws-profile)
    (setenv "AWS_PROFILE" nil)
    (message "Connected to instance %s as user %s using profile %s and added SSH key." instance-id ssh-user aws-profile)))

(defun my/tramp-ssm-to-container (instance-id aws-profile)
  "Connect to a Docker container on an AWS instance using TRAMP."
  (interactive
   (list (read-string "Enter AWS Instance ID: " my/last-used-aws-instance-id)
         (read-string "Enter AWS Profile (default %s): " my/last-used-aws-profile nil nil my/last-used-aws-profile)))
  
  ;; Update global variables.
  (setq my/last-used-aws-instance-id instance-id)
  (setq my/last-used-aws-profile aws-profile)

  ;; Get list of containers.
  (let ((containers (my/list-docker-containers-ssm-non-interactive instance-id aws-profile)))
    ;; Select container from list.
    (when containers
      (let ((container-id (completing-read "Select Docker Container: " containers nil t)))
        (when container-id
          (let ((ssh-user my/ssm-ssh-default-user))
            (my/add-ssh-key-to-instance instance-id aws-profile)
            (setenv "AWS_PROFILE" aws-profile)
            (let ((tramp-path (format "/ssh:%s@%s|docker:%s:/" ssh-user instance-id container-id)))
              (dired tramp-path))
            (setenv "AWS_PROFILE" nil)
            (message "Connected to instance %s, container %s as user %s using profile %s."
                     instance-id container-id ssh-user aws-profile)))))))

(defun my/add-ssh-key-to-instance (instance-id aws-profile)
  "Add a specific SSH key to an EC2 instance's authorized_keys using AWS SSM."
  (interactive
   (list (read-string "Enter AWS Instance ID: " my/last-used-aws-instance-id)
         (read-string (format "Enter AWS Profile (default %s): " my/last-used-aws-profile) nil nil my/last-used-aws-profile)))
  (let* ((ssh-user my/ssm-ssh-default-user)
         (public-key-path (expand-file-name "~/.ssh/id_rsa.pub"))
         (ssh-options "no-port-forwarding,no-agent-forwarding,no-X11-forwarding")
         (public-key (with-temp-buffer
                       (insert-file-contents-literally public-key-path)
                       (goto-char (point-max))
                       (delete-char -1)
                       (concat ssh-options " " (buffer-string))))
         (authorized-keys-path (if (string-equal ssh-user "root")
                                   "/root/.ssh/authorized_keys"
                                 (format "/home/%s/.ssh/authorized_keys" ssh-user)))
         (command-to-execute (if (string-equal ssh-user "root")
                                 (format "if ! grep -qF -- '%s' %s; then echo '%s' >> %s; chmod 600 %s; echo 'Key added to authorized_keys'; else echo 'Key already exists in authorized_keys'; fi" 
                                         public-key authorized-keys-path public-key authorized-keys-path authorized-keys-path)
                               (format "if ! grep -qF -- '%s' %s; then echo '%s' >> %s; chmod 600 %s; chown %s:%s %s; echo 'Key added to authorized_keys'; else echo 'Key already exists in authorized_keys'; fi"
                                       public-key authorized-keys-path public-key authorized-keys-path authorized-keys-path ssh-user ssh-user authorized-keys-path)))
         (encoded-command (base64-encode-string command-to-execute t))
         (aws-command (format "aws ssm send-command --document-name \"AWS-RunShellScript\" --targets \"Key=instanceids,Values=%s\" --parameters '{\"commands\":[\"echo %s | base64 --decode | bash\"]}' --comment \"Adding SSH key to authorized_keys\" --profile %s" instance-id encoded-command aws-profile))
         (output (shell-command-to-string aws-command)))
    (if (string-match "\"CommandId\": \"\\([^\"]*\\)\"" output)
        (message "Command to ADD ssh key for user '%s' SUCCEEDED. Command ID: %s" ssh-user (match-string 1 output))
      (message "Command to ADD ssh key for user '%s' FAILED. Did you check SSO status?" ssh-user)
      (sleep-for 1.5))

    (setq my/last-used-aws-instance-id instance-id)
    (setq my/last-used-aws-profile aws-profile)
    (setenv "AWS_PROFILE" nil)))

(defun my/remove-ssh-key-from-instance (&optional instance-id aws-profile)
  "Remove a specific SSH key from an EC2 instance's authorized_keys using AWS SSM.
If INSTANCE-ID or AWS-PROFILE are nil, use the last used values."
  (interactive)
  (unless instance-id
    (setq instance-id (or my/last-used-aws-instance-id
                          (read-string "Enter EC2 Instance ID: "))))
  (unless aws-profile
    (setq aws-profile (or my/last-used-aws-profile
                          (read-string (format "Enter AWS Profile (default %s): " (or my/last-used-aws-profile "default"))))))
  (let* ((ssh-user my/ssm-ssh-default-user)
         (public-key-path (expand-file-name "~/.ssh/id_rsa.pub"))
         (ssh-options "no-port-forwarding,no-agent-forwarding,no-X11-forwarding")
         (public-key (with-temp-buffer
                       (insert-file-contents-literally public-key-path)
                       (goto-char (point-max))
                       (delete-char -1)
                       (concat ssh-options " " (buffer-string))))
         (authorized-keys-path (if (string-equal ssh-user "root")
                                   "/root/.ssh/authorized_keys"
                                 (format "/home/%s/.ssh/authorized_keys" ssh-user)))
         (command-to-execute (format "grep -vF '%s' %s > /tmp/temp_keys && mv /tmp/temp_keys %s && chmod 600 %s%s" 
                                     public-key authorized-keys-path authorized-keys-path authorized-keys-path
                                     (if (string-equal ssh-user "root") ""
                                       (format " && chown %s:%s %s" ssh-user ssh-user authorized-keys-path))))
         (encoded-command (base64-encode-string command-to-execute t))
         (aws-command (format "aws ssm send-command --document-name \"AWS-RunShellScript\" --targets \"Key=instanceids,Values=%s\" --parameters '{\"commands\":[\"echo %s | base64 --decode | bash\"]}' --comment \"Removing SSH key from authorized_keys\" --profile %s"
                              instance-id encoded-command aws-profile))
         (output (shell-command-to-string aws-command)))
    (message "Using AWS Profile: %s and Instance ID: %s" aws-profile instance-id)
    (if (string-match "\"CommandId\": \"\\([^\"]*\\)\"" output)
        (message "Command to REMOVE ssh key for user '%s' SUCCEEDED. Command ID: %s" ssh-user (match-string 1 output))
      (message "Command to REMOVE ssh key for user '%s' FAILED. Did you check SSO status?" ssh-user))
    (sleep-for 1.5)
    (setq my/last-used-aws-instance-id instance-id)
    (setq my/last-used-aws-profile aws-profile)
    (setenv "AWS_PROFILE" nil)))

(defun my/tramp-end-ssm-session-and-remove-key ()
  "Ends the Tramp SSM session by cleaning up all buffers and connections,
   then calls the function to remove the SSH key from the server using last used parameters."
  (interactive)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections)
  (my/remove-ssh-key-from-instance my/last-used-aws-instance-id my/last-used-aws-profile)
  (message "Tramp SSM session cleaned up and ended using AWS Profile: %s and Instance ID: %s."
           my/last-used-aws-profile my/last-used-aws-instance-id))


;;;; get docker container list
(defun my/list-docker-containers-ssm-non-interactive (instance-id aws-profile)
  "Non-Interactive version: List Docker containers on a specified EC2 instance using AWS SSM."
  (let* ((list-containers-cmd "docker ps --format '{{.Names}}'")
         (encoded-command (base64-encode-string list-containers-cmd t))
         (aws-command (format "aws ssm send-command --document-name \"AWS-RunShellScript\" --targets \"Key=instanceids,Values=%s\" --parameters '{\"commands\":[\"echo %s | base64 --decode | bash\"]}' --profile %s --output json"
                              instance-id encoded-command aws-profile))
         (command-output (shell-command-to-string aws-command))
         (command-id (my/parse-command-id command-output))
         (containers-list '()))
    (when command-id
      ;; Fetch command invocation output and remove the first line if necessary.
      (let* ((output (my/get-ssm-command-invocation-output command-id instance-id aws-profile))
             (output-lines (and output (split-string output "\n" t)))
             (filtered-output-lines (cdr output-lines)))  ; Remove the first line.
        (setq containers-list filtered-output-lines)))
    containers-list))
