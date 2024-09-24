(defun my/aws-display-description (description-buffer-name json-data)
  "Display the JSON data by replacing the current buffer's content."
  (let ((buffer (get-buffer-create description-buffer-name)))
    (switch-to-buffer buffer)
    (read-only-mode -1)
    (erase-buffer)
    (insert (json-encode json-data))
    (json-mode)
    (json-pretty-print-buffer)
    (goto-char (point-min))json-data))

(defun my/aws-json-command (command)
  "Run an AWS CLI command provided in COMMAND and return parsed JSON, handling errors."
  (let* ((command-string (concat "aws " command " --profile " my/last-used-aws-profile))
         (output (shell-command-to-string command-string))
         (json-object-type 'alist))
    (condition-case nil
        (json-parse-string output :object-type json-object-type)
      (json-parse-error
       (error "Failed to parse JSON response: %s" output)))))

(defun my/aws-parse-resource-names (arns-alist)
  "Parse a list of ARNs from an alist by finding any key that ends with 'Arns', and return just the last part, the resource names."
  (let* ((arns-pair (cl-assoc-if (lambda (key) (string-suffix-p "Arns" key))
                                 arns-alist
                                 :key #'symbol-name))
         (arns (cdr arns-pair)))  ; Get the value part of the found pair.
    (mapcar #'my/extract-name-from-arn arns)))

(defun my/extract-name-from-arn (arn)
  "Extract the name from an ARN by returning the substring after the last slash."
  (if (and arn (string-match "/\\([^/]+\\)$" arn))
      (match-string 1 arn)
    "Invalid ARN"))

(defun my/select-from-list (prompt choices)
  "Prompt the user to select an item from CHOICES.
PROMPT is the string to display as the prompt.
CHOICES is a list of strings from which the user can choose.
Includes the currently used AWS profile in the prompt."
  ;; Assume `my/last-used-aws-profile` holds the current AWS profile
  (let ((enhanced-prompt (format "%s (profile %s): " (upcase prompt) my/last-used-aws-profile)))
    (completing-read enhanced-prompt choices nil t)))

(defun my/aws-ec2-instance-menu (instance-id)
  "Present a menu with actions for an EC2 instance."
  (interactive)
  (let ((action (completing-read (format "Select action for instance %s: " instance-id)
                                 '("Connect: Instance" "Connect: Container" "Describe: Instance"))))
    (setq my/last-used-aws-instance-id instance-id)
    (cond ((string= action "Connect: Instance")
           (my/tramp-ssm-to-aws-instance instance-id  my/last-used-aws-profile))
          ((string= action "Connect: Container")
           (my/tramp-ssm-to-container my/last-used-aws-instance-id my/last-used-aws-profile))
          ((string= action "Describe: Instance")
           (my/aws-describe-ec2-instance instance-id )))))

(defun my/aws-describe-cluster-and-select ()
  "Describe an ECS cluster and allow the user to select one."
  (interactive)
  (let* ((command "ecs list-clusters")
         (clusters-json (my/aws-json-command command))
         (cluster-names (my/aws-parse-resource-names clusters-json)))
    (if cluster-names
        (let ((selected-cluster (my/select-from-list "Select a cluster" cluster-names)))
          (when selected-cluster
            (my/aws-display-description "*AWS Cluster Description*"
                                        (my/aws-json-command (format "ecs describe-clusters --clusters %s" selected-cluster)))
            (kill-new selected-cluster))  ; Copy selected cluster to clipboard
          selected-cluster)
      (progn
        (message "No clusters found.")
        nil))))

(defun my/aws-describe-service-and-select (cluster-name)
  "List all services in the specified ECS cluster and allow the user to select one."
  (interactive)
  (let* ((command (format "ecs list-services --cluster %s" cluster-name))
         (services-json (my/aws-json-command command))
         (service-names (my/aws-parse-resource-names services-json)))
    (if service-names
        (let ((selected-service (my/select-from-list "Select a service" service-names)))
          (when selected-service
            (my/aws-display-description "*AWS Service Description*"
                                        (my/aws-json-command (format "ecs describe-services --cluster %s --services %s" cluster-name selected-service)))
            (kill-new selected-service))  ; Copy selected service to clipboard
          selected-service)
      (progn
        (message "No services found in cluster %s" cluster-name)
        nil))))

(defun my/aws-describe-task-and-select (cluster-name service-name)
  "List all tasks for a given ECS service and allow the user to select one to describe."
  (interactive)
  (let* ((command (format "ecs list-tasks --cluster %s --service-name %s" cluster-name service-name))
         (tasks-json (my/aws-json-command command))
         (task-arns (cdr (assoc 'taskArns tasks-json))))
    (if task-arns
        (let ((selected-task (my/select-from-list "Select a task: " (mapcar #'my/extract-name-from-arn task-arns))))
          (when selected-task
            (my/aws-display-description "*AWS Task Description*"
                                        (my/aws-json-command (format "ecs describe-tasks --cluster %s --tasks %s" cluster-name selected-task)))
            (kill-new selected-task))  ; Copy selected task to clipboard
          selected-task)
      (progn
        (message "No tasks found for service %s in cluster %s" service-name cluster-name)
        nil))))

(defun my/aws-describe-container-instance-and-select (cluster-name task-id)
  "List all container instances for a given task and allow the user to select one."
  (interactive)
  (let* ((command (format "ecs describe-tasks --cluster %s --tasks %s" cluster-name task-id))
         (tasks-json (my/aws-json-command command))
         (container-instance-arn (cdr (assoc 'containerInstanceArn (aref (cdr (assoc 'tasks tasks-json)) 0)))))
    (if container-instance-arn
        (let ((selected-container-instance (my/select-from-list "Select a container instance" (list (my/extract-name-from-arn container-instance-arn)))))
          (when selected-container-instance
            (my/aws-display-description "*AWS Container Instance Description*"
                                        (my/aws-json-command (format "ecs describe-container-instances --cluster %s --container-instances %s" cluster-name selected-container-instance)))
            (kill-new selected-container-instance))  ; Copy selected container instance to clipboard
          selected-container-instance)
      (progn
        (message "No container instances found for task %s in cluster %s" task-id cluster-name)
        nil))))

(defun my/aws-get-ec2-instance-id-from-container-and-select (cluster-name container-instance-id)
  "Retrieve and pass the EC2 Instance ID for the selected container instance to the action menu."
  (interactive)
  (let* ((command (format "ecs describe-container-instances --cluster %s --container-instances %s" cluster-name container-instance-id))
         (container-instances-json (my/aws-json-command command))
         (ec2-instance-id (cdr (assoc 'ec2InstanceId (aref (cdr (assoc 'containerInstances container-instances-json)) 0)))))
    (if ec2-instance-id
        (progn
          (my/aws-ec2-instance-menu ec2-instance-id)
          (kill-new ec2-instance-id))  ; Copy EC2 instance ID to clipboard
      (progn
        (message "EC2 Instance ID not found for container instance %s in cluster %s" container-instance-id cluster-name)
        nil))))

(defun my/aws-describe-ec2-instance (instance-id)
  "Fetch and display the description of an EC2 instance."
  (interactive)
  (if instance-id
      (my/aws-display-description "*AWS EC2 Instance Description*"
                                  (my/aws-json-command (format "ec2 describe-instances --instance-ids %s" instance-id)))
    (message "EC2 Instance ID not found for instance %s" instance-id)
    nil))

(defun my/aws-ecs-workflow ()
  "Workflow to select and describe a cluster, service, task, container instance, and EC2 instance ID."
  (interactive)
  (let* ((cluster-name (my/aws-describe-cluster-and-select))
         (service-name (when cluster-name
                         (my/aws-describe-service-and-select cluster-name)))
         (task-id (when service-name
                    (my/aws-describe-task-and-select cluster-name service-name)))
         (container-instance-id (when task-id
                                  (my/aws-describe-container-instance-and-select cluster-name task-id)))
         (ec2-instance-id (when container-instance-id
                            (my/aws-get-ec2-instance-id-from-container-and-select cluster-name container-instance-id))))
    (when ec2-instance-id
      (message "Workflow completed for EC2 instance ID: %s" ec2-instance-id))))
