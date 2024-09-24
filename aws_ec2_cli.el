;;; aws_ec2_cli.el -*- lexical-binding: t; -*-

(defun my/list-ec2-by-name-substring (substring)
  "List EC2 instances with names containing SUBSTRING, using the specified AWS profile."
  (interactive (list (read-string (format "Enter substring for EC2 instance names (using profile %s): " my/last-used-aws-profile))))
  (let* ((filter-value (concat "*" substring "*")) ;; Build the filter with wildcard characters
         (command (format "ec2 describe-instances --filters \"Name=tag:Name,Values=%s\" --query \"Reservations[*].Instances[*].{InstanceID:InstanceId, Name:Tags[?Key=='Name'].Value | [0]}\"" filter-value))
         (result (my/aws-json-command command)))
    (if result
        (let ((buffer (get-buffer-create "*EC2 Instances JSON*")))
          (with-current-buffer buffer
            (erase-buffer)
            (insert (json-encode result))
            (json-mode)
            (json-pretty-print-buffer)
            (goto-char (point-min))
            (if (search-forward "\"InstanceID\": " nil t)  ;; Find the location of "InstanceID":
                (skip-chars-forward " \t\n\"")  ;; Skip spaces, tabs, newlines, and the quote, placing cursor right at the start of the value
              (goto-char (point-min))))  ;; If not found, revert to the start of the buffer
          (switch-to-buffer buffer)
          (delete-other-windows))
      (message "No results found or there was an error."))))
