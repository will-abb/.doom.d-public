;;; temporary-functions.el -*- lexical-binding: t; -*-

(defun temp/reformat-org-roles ()
  "Reformat Org mode roles to level 3 and clean up policies."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^Role: \\(.+\\)$" nil t)
      (let ((role-name (match-string 1)))
        ;; Replace Role: with *** followed by the role name
        (replace-match (concat "*** " role-name))
        ;; Move to the next line
        (forward-line 1)
        ;; Remove "Policies with Access:" line
        (when (looking-at "Policies with Access:")
          (kill-line 1))
        ;; Adjust the list of policies
        (while (looking-at " - Managed Policy:\\| - Inline Policy:")
          (replace-match "- Managed Policy:")
          (forward-line 1))))))
