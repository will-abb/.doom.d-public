;;; ssm_run_command.el -*- lexical-binding: t; -*-

(defun my/get-ssm-command-invocation-output (command-id instance-id aws-profile)
  "Retrieve the output of an SSM command invocation."
  (let* ((get-command-invocation-cmd (format "aws ssm get-command-invocation --command-id %s --instance-id %s --profile %s --output text --query 'StandardOutputContent'"
                                             command-id instance-id aws-profile))
         (invocation-output (shell-command-to-string get-command-invocation-cmd)))
    (message "Command output:\n%s" invocation-output)))

(defun my/parse-command-id (json-output)
  "Parse the CommandId from the JSON output."
  (when (string-match "\"CommandId\": \"\\([^\"]+\\)\"" json-output)
    (match-string 1 json-output)))

(defun my/send-ssm-command-and-fetch-output (commands)
  "Send an SSM command to an AWS EC2 instance and fetch the output.
COMMANDS can be passed as an argument or entered interactively."
  (interactive (list (read-string "Enter command for SSM: ")))
  (let* ((instance-id (read-string "Enter EC2 Instance ID: " my/last-used-aws-instance-id))
         (aws-profile (read-string "Enter AWS Profile (default %s): " my/last-used-aws-profile nil nil my/last-used-aws-profile))
         (document-name "AWS-RunShellScript")
         (target (format "Key=instanceids,Values=%s" instance-id))
         (params (format "{\"commands\":[\"%s\"]}" commands))
         (send-cmd-format "aws ssm send-command --document-name \"%s\" --targets \"%s\" --parameters '%s' --profile %s --output json")
         (send-cmd (format send-cmd-format document-name target params aws-profile))
         (send-cmd-output (shell-command-to-string send-cmd))
         (command-id (when send-cmd-output
                       (and (string-match "\"CommandId\": \"\\([^\"]*\\)\"" send-cmd-output)
                            (match-string 1 send-cmd-output)))))
    ;; Update global variables if new values are provided
    (when instance-id
      (setq my/last-used-aws-instance-id instance-id))
    (when aws-profile
      (setq my/last-used-aws-profile aws-profile))

    (if command-id
        (progn
          (message "SSM Command sent. Command ID: %s" command-id)
          (sleep-for 1)  ; Wait for a second before proceeding
          (let* ((get-cmd-format "aws ssm get-command-invocation --command-id %s --instance-id %s --profile %s --output text --query 'StandardOutputContent'")
                 (get-cmd (format get-cmd-format command-id instance-id aws-profile))
                 (result (shell-command-to-string get-cmd)))
            (if (not (string-empty-p result))
                (progn
                  (message "SSM Command Output: %s" result)
                  (kill-new result))  ; Copies the result to the clipboard
              (message "Failed to fetch output or output is empty."))))
      (message "Failed to execute SSM command or parse Command ID."))))


(defun my/ssm-get-instance-info ()
  "Fetch instance identity document from an EC2 instance."
  (interactive)
  (my/send-ssm-command-and-fetch-output "curl http://169.254.169.254/latest/dynamic/instance-identity/document"))
