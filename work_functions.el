;;; work_functions.el -*- lexical-binding: t; -*-

;; Transfer keywords Check with file extension check
(defun transfer-keywords-check ()
  "Search for 'profile', 'secret', 'gpg', 'require' 'var/tmp' or 'root' in the current buffer using +default/search-buffer.
   Also checks if the file name contains 'core', 'commission', or ends with '.sh'."
  (interactive)
  (let ((filename (buffer-file-name)))
    ;; Check if the file name contains 'core', 'commission', or ends with '.sh'
    (if (and filename (string-match-p "\\(core\\|commission\\|\\.sh\\)" filename))
        (message "Error: Restricted file type or name.")
      ;; Search for keywords in the buffer content
      (let ((keywords '("profile" "secret" "root" "/var/tmp" "gpg" "require"))
            (start (when (region-active-p) (region-beginning)))
            (end (when (region-active-p) (region-end))))
        (dolist (keyword keywords)
          (if (and start end)
              (progn
                (goto-char start)
                (+default/search-buffer))
            (progn
              (goto-char (point-min))
              (when (search-forward keyword nil t)
                (+default/search-buffer)))))))))

(defun transfer-check-crontab ()
  (interactive)
  (beginning-of-line)
  (if (looking-at-p "#")
      (let* ((match nil))
        (re-search-forward "/opt/scripts/transfers-automation/scripts/.*.pl " nil t)
        (setq match (file-name-nondirectory (string-trim (match-string 0))))
        (message "found file '%s'" match)
        (switch-to-buffer (find-file-noselect (format "scripts/%s" match) nil nil nil))
        (transfer-keywords-check))))
