;;; grafana.el -*- lexical-binding: t; -*-
(defun grafana-replace-description (file-path)
  "Replace specific words in lines starting with 'description':"
  (let ((description-replacement-alist
         ;; ecs service name
         '(("select-care" . "select-care"))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      ;; Process each line starting with 'description':
      (while (re-search-forward "^\\s-*\"description\": \".*\"" nil t)
        (let ((description-line (match-string 0)))
          (dolist (pair description-replacement-alist)
            (let ((old (car pair))
                  (new (cdr pair)))
              (setq description-line (replace-regexp-in-string old new description-line))))
          (replace-match description-line t t)))
      (write-file file-path))))


;; helper grafana function to replace urls
(defun grafana-replace-url (file-path)
  "Replace specific words in lines starting with 'url': (OldValue . NewValue)"
  (let ((url-replacement-alist
         ;;1. ecs cluster name
         '(("SelectCareSenior" . "SelectCareSenior")
           ;;2. ecs service name
           ("select-care" . "select-care"))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      ;; Process each line starting with 'url':
      (while (re-search-forward "^\\s-*\"url\": \".*\"" nil t)
        (let ((url-line (match-string 0)))
          (dolist (pair url-replacement-alist)
            (let ((old (car pair))
                  (new (cdr pair)))
              (setq url-line (replace-regexp-in-string old new url-line))))
          (replace-match url-line t t)))
      (write-file file-path))))


;; grafana create new ecs service dashboard replace function (calls helper functions)
;; CLOSE BUFFER BEFORE RUNNING!!!
(defun my/grafana-replace-configs ()
  "grafana config replace. First value is replaced with secon value. Update 12 values."
  (interactive)
  ;;3. DON'T FORGET TO UPDATE FILE PATH!!!                                      ;
  (let ((file-path "~/repositories/bitbucket/SelectQuote/grafana-dashboards/dashboards_json_configs/select-care-senior-uat-service.json")
        (old-new-alist
         (list
          ;;4. Regular expression for ARN of loadbalancer
          (cons "arn:aws:elasticloadbalancing:.*:\\d+:loadbalancer/app/.*"
                "arn:aws:elasticloadbalancing:us-west-2:463931586000:loadbalancer/app/UATGeneral/740100a4366eea9c")
          ;;6. Regular expression for ARN of targetgroup
          (cons "arn:aws:elasticloadbalancing:.*:\\d+:targetgroup/.*"
                "arn:aws:elasticloadbalancing:us-west-2:463931586000:targetgroup/select-care-senior/db501ecb23aad026")
          ;;7. Regular expression to match "LoadBalancer": "any-value",
          (cons "\"LoadBalancer\": \"[^\"]*\"" "\"LoadBalancer\": \"app/UATGeneral/740100a4366eea9c\"")
          ;;8. Regular expression to match "TargetGroup": "any-value",
          (cons "\"TargetGroup\": \"[^\"]*\"" "\"TargetGroup\": \"targetgroup/select-care-senior/db501ecb23aad026\"")
          ;;9. Regular expression to match "ClusterName": "any-value",
          (cons "\"ClusterName\": \"[^\"]*\"" "\"ClusterName\": \"SelectCareSenior\"")
          ;;10. Regular expression to match "ServiceName": "any-value"
          (cons "\"ServiceName\": \"[^\"]*\"" "\"ServiceName\": \"select-care\"")
          ;;11. aws account number
          (cons "586672212047" "463931586000")
          ;;12. grafana datasource
          (cons "ADzHeO67z" "000000008"))))
    ;; Outer loop - run the whole process 2 times
    (dotimes (outer 2)
      (dolist (pair old-new-alist)
        (let ((old (car pair))
              (new (cdr pair)))
          ;; Inner loop for each replacement, run 2 times
          (dotimes (inner 3)
            (with-temp-buffer
              (insert-file-contents file-path)
              (goto-char (point-min))
              ;; Perform the replacement using regular expression
              (while (re-search-forward old nil t)
                (replace-match new t t)) ; 'fixedcase' and 'literal' arguments set to t
              (write-file file-path)
              (message "Outer loop %d, Inner loop %d: Replaced %s with %s" (1+ outer) (1+ inner) old new))))))
    ;; Call the new function to handle 'url': lines
    (grafana-replace-url file-path)
    (grafana-replace-description file-path)
    (message "Update complete in file %s" file-path)))
