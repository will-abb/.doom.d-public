;;; org-jira.el -*- lexical-binding: t; -*-

;; jira settings
;; use ~/.authinfo.gpg and make sure auth-sources points to it if it doesn't by default. add line:
;; machine selectquote.atlassian.net login williams.bosch-bello@selectquote.com password [jira token] port 443
(setq auth-sources '("~/.authinfo.gpg"))
(setq jiralib-url "https://selectquote.atlassian.net")
(setq jiralib-user "williams.bosch-bello@selectquote.com")
(setq jiralib-update-issue-fields-exclude-list '(reporter))

;; Function to handle custom description field
(defun my-org-jira-get-description-field (fields)
  "Retrieve the correct description field from the JIRA issue FIELDS."
  (or (cdr (assoc 'customfield_10504 fields)) ; Replace 'customfield_10504' with your custom field ID
      (cdr (assoc 'description fields))
      ""))

(defun my-org-jira-set-description-field (description fields)
  "Set the correct description field for the JIRA issue."
  (let ((custom-description-field 'customfield_10504)) ; Replace 'customfield_10504' with your custom field ID
    (if (assoc custom-description-field fields)
        (cons custom-description-field description)
      (cons 'description description))))

;; Update org-jira-sdk-from-data to use the custom get-description function
(defun org-jira-sdk-from-data (data)
  "Convert DATA into an org-jira issue."
  (let* ((fields (alist-get 'fields data)))
    `(:key ,(alist-get 'key data)
      :summary ,(alist-get 'summary fields)
      :description ,(my-org-jira-get-description-field fields)
      ;; Add other fields as needed
      )))

;; Update org-jira-update-issue to use the custom set-description function
(defun org-jira-update-issue (issue-id)
  "Update an existing JIRA issue."
  (let* ((fields (alist-get 'fields (org-jira-get-issue issue-id)))
         (description (my-org-jira-set-description-field org-issue-description fields)))
    (jiralib-call "editIssue" issue-id `((fields . ,(cons description (remove description fields)))))))

;; Jira Helper function
(defun my-org-jira-get-issue (issue-number)
  "Get a JIRA issue with a partial ID."
  (interactive "sEnter Issue Number: ")
  (let ((full-issue-id (concat "DEVO-" issue-number)))
    (org-jira-get-issue full-issue-id)))

;; Keybinds
(require 'org-jira)
(map! :leader
      (
       :prefix ("j" . "org-jira")
       :desc "Assign Issue"                     "a" #'org-jira-assign-issue
       :desc "Create Issue"                     "c" #'org-jira-create-issue
       :desc "Browse Issue"                     "b" #'org-jira-browse-issue
       :desc "Get Issues by Board"              "B" #'org-jira-get-issues-by-board
       :desc "Download Attachment"              "d" #'org-jira-download-attachment
       :desc "Get Issue by Number"              "g" #'my-org-jira-get-issue
       :desc "Get Issue"                        "G" #'org-jira-get-issue
       :desc "Get Issues by Filter"             "f" #'org-jira-get-issues-from-filter
       :desc "Head: Get Issues by Filter"       "H" #'org-jira-get-issues-from-filter-headonly
       :desc "Head: Get All Issues"             "h" #'org-jira-get-issues-headonly
       :desc "Copy Current Issue Key"           "k" #'org-jira-copy-current-issue-key
       :desc "Get All Issues"                   "l" #'org-jira-get-issues
       :desc "Add Comment"                      "m" #'org-jira-add-comment
       :desc "Update Comment"                   "M" #'org-jira-update-comment
       :desc "Get Issues by JQL"                "q" #'org-jira-get-issues-from-custom-jql
       :desc "Progress Issue"                   "p" #'org-jira-progress-issue
       :desc "Progress Issue Next"              "n" #'org-jira-progress-issue-next
       :desc "Refresh Issue"                    "r" #'org-jira-refresh-issue
       :desc "Refresh Issues in Buffer"         "R" #'org-jira-refresh-issues-in-buffer
       :desc "Get Subtasks"                     "s" #'org-jira-get-subtasks
       :desc "Create Subtask"                   "S" #'org-jira-create-subtask
       :desc "Todo to Jira"                     "T" #'org-jira-todo-to-jira
       :desc "Update Issue"                     "u" #'org-jira-update-issue
       ;; :desc "Update Worklogs Clocks"           "w" #'org-jira-update-worklogs-from-org-clocks ;; don't clock in anymore
       ))

;; (setq org-jira-custom-jqls
;;       '((:jql "assignee = currentUser() AND status = Done AND resolved >= \"2023-05-17\" AND resolved <= \"2024-05-17\" ORDER BY updated ASC"
;;          :limit 5
;;          :filename "this-years-work-2023-2024")))

;; Commented-out function for reference
;; (defun org-jira-update-issue (issue-id)
;;   "Update an existing JIRA issue."
;;   (let* ((fields (alist-get 'fields (org-jira-get-issue issue-id)))
;;          (description (my-org-jira-set-description-field org-issue-description fields)))
;;     (jiralib-call "editIssue" issue-id `((fields . ,(cons description (remove description fields)))))))
