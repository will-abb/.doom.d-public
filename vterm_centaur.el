;;; vterm_centaur.el -*- lexical-binding: t; -*-

(defvar my/vterm-buffer-list nil
  "List of vterm buffers for cycling.")

(defvar my/vterm-buffer-index 0
  "Current index in the vterm buffer list.")

(defun my/vterm-buffer-name-to-index (name)
  "Convert vterm buffer NAME to an index, defaulting to 0 if no number is present."
  (if (string-match "\\*vterm\\*\\(?:<\\([0-9]+\\)>\\)?" name)
      (let ((num (match-string 1 name)))
        (if num (string-to-number num) 0))
    -1))

(defun my/update-vterm-buffer-list ()
  "Update the list of vterm buffers."
  (setq my/vterm-buffer-list
        (sort (delq nil
                    (mapcar (lambda (buf)
                              (when (string-match "\\*vterm\\*\\(?:<\\([0-9]+\\)>\\)?" (buffer-name buf))
                                buf))
                            (buffer-list)))
              (lambda (b1 b2)
                (< (my/vterm-buffer-name-to-index (buffer-name b1))
                   (my/vterm-buffer-name-to-index (buffer-name b2))))))
  (unless my/vterm-buffer-list
    (my/open-vterm-in-current-directory)
    (my/update-vterm-buffer-list)))

(defun my/cycle-vterm-buffers ()
  "Cycle through vterm buffers, or open a new one if none exist."
  (interactive)
  (my/update-vterm-buffer-list)
  (when my/vterm-buffer-list
    (setq my/vterm-buffer-index (mod (1+ my/vterm-buffer-index) (length my/vterm-buffer-list)))
    (switch-to-buffer (nth my/vterm-buffer-index my/vterm-buffer-list))))

(defun my/open-vterm-in-current-directory ()
  "Open vterm in the current buffer's directory."
  (interactive)
  (let ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    (+vterm/here 1)))

(defun my/kill-vterm-without-confirmation ()
  "Check before killing a buffer if it's a vterm to kill without confirmation."
  ;; Check if it's a vterm buffer and if it has a process.
  (if (and (eq major-mode 'vterm-mode)
           (string-match-p "vterm" (buffer-name))
           (get-buffer-process (current-buffer)))
      (progn
        ;; It's a vterm buffer with a process, set the process's query-on-exit flag to nil.
        (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
        t)  ;; Return t to allow the buffer to be killed.
    t))  ;; Return t in all other cases to not interfere with other buffers being killed.

;; Add to kill-buffer-query-functions

;;;;;;;;;;;;;;;;;;;;;;Centaur Tabs;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my/centaur-tabs-buffer-groups ()
;;   "Function that groups buffers by their project root directory."
;;   (list
;;    (if (and (buffer-file-name) (project-current))
;;        (concat "Project: " (expand-file-name (project-root (project-current))))
;;      "Others")))

;; (defvar my/ignored-buffers '("*doom*" "*scratch*" "*Messages*" "*Async-native-compile-log*" "*Native-compile-Log*" "*Echo Area 0*" "*which-key*" "*eldoc*" "*string-pixel-width*" "*jka-compr-wr-temp*" "*code-conversion*" "org-src-fontification:sh-mode" "code-converting-work" "company-sps")
;;   "List of buffer names to ignore in Centaur Tabs.")

;; (defun my/centaur-tabs-buffer-list ()
;;   "Custom function to list buffers in the current workspace, ignoring specific buffers."
;;   (delq nil
;;         (mapcar (lambda (buf)
;;                   (let ((buf-name (buffer-name buf)))
;;                     (if (and (not (member buf-name my/ignored-buffers))
;;                              (buffer-file-name buf)) ; make sure it's a file buffer or meets other criteria
;;                         buf))) ; Correctly return the buffer if it doesn't match ignored list
;;                 (buffer-list))))

;; (defun my/disable-centaur-tabs-in-vterm ()
;;   "Disable Centaur Tabs when entering vterm."
;;   (centaur-tabs-local-mode 1))

;; (defun my/enable-centaur-tabs-after-vterm ()
;;   "Enable Centaur Tabs when exiting vterm."
;;   (unless (centaur-tabs-mode)
;;     (centaur-tabs-mode 1)
;;     (centaur-tabs-local-mode -1)))
