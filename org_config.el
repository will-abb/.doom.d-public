;;; org_config.el -*- lexical-binding: t; -*-

(after! org
  (setq org-M-RET-may-split-line t)
  (setq org-directory "~/personal-repos/org/")
  (setq org-agenda-files '("~/personal-repos/org/"))
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 3)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN PROGRESS(i)" "BLOCKED(b)" "WAITING(w)" "IMPORTANT(p)" "LATER(l)" "TOMORROW(T)" "SCHEDULED(s)" "|" "DONE(d)" "CANCELLED(c)" "NOT NEEDED(x)")))
  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "green" :weight bold))
          ("NEXT" . (:foreground "blue" :weight bold))
          ("IN PROGRESS" . (:foreground "orange" :weight bold))
          ("BLOCKED" . (:foreground "#800000" :weight bold))
          ("WAITING" . (:foreground "#800000" :weight bold))
          ("IN-ALERT!" . (:foreground "red" :weight bold))
          ("PROJECT" . (:foreground "purple" :weight bold))
          ("TOMORROW" . (:foreground "#505050" :weight bold))
          ("LATER" . (:foreground "#3c2f2f" :weight bold))
          ("DONE" . (:foreground "black" :weight bold))
          ("CANCELLED" . (:foreground "black" :weight bold))
          ("NOT NEEDED" . (:foreground "black" :weight bold))
          ("IMPORTANT" . (:foreground "red" :weight bold))
          ("SCHEDULED" . (:foreground "pink" :weight bold))
          ))
  (setq org-tag-alist '((:startgroup)
                        ("work" . ?w)
                        ("home" . ?h)
                        ("study" . ?S)
                        ("@health" . ?l)
                        (:endgroup)
                        (:startgroup)
                        ("StartedMentioned" . ?s)
                        ("FinishedMentioned" . ?f)
                        ("WasNotMention" . ?N)
                        ("DoNotMention" . ?n)
                        (:endgroup)
                        (:startgroup)
                        ("@APPOINTMENT" . ?a)
                        ("@IMPORTANT" . ?i)
                        (:endgroup)
                        (:startgroup)
                        ("Errands" . ?e)
                        ("Buy" . ?b)
                        (:endgroup)
                        ("project" . ?p)
                        ("note" . ?t)
                        ("idea" . ?i)))
  )
(setq org-tag-faces
      '(("work" . (:foreground "sky blue" :weight bold))
        ("home" . (:foreground "gray" :weight bold))
        ("study" . (:foreground "yellow" :weight bold))
        ("@health" . (:foreground "green" :weight bold))
        ("@APPOINTMENT" . (:foreground "red" :weight bold))
        ("@IMPORTANT" . (:foreground "red" :weight bold))
        ("project" . (:foreground "purple" :weight bold))
        ("note" . (:foreground "magenta" :weight bold))
        ("StartedMentioned" . (:foreground "green" :weight bold))
        ("Errands" . (:foreground "magenta" :weight bold))
        ("Buy" . (:foreground "green" :weight bold))
        ("FinishedMentioned" . (:foreground "red" :weight bold))
        ("WasNotMention" . (:foreground "yellow" :weight bold))
        ("DoNotMention" . (:foreground "yellow" :weight bold))
        ("idea" . (:foreground "brown" :weight bold))))

;; Org Agenda Settings
(after! org-agenda
  (setq org-agenda-custom-commands
        '(("w" "Agenda with TOMORROW tasks"
           todo "TOMORROW"
           ((org-agenda-overriding-header "Tasks with TOMORROW"))))))

(setq org-priority-faces '((?A . (:foreground "red" :weight bold))
                           (?B . (:foreground "#8B0000"))
                           (?C . (:foreground "#330000"))))
                                        ; Capture templates
(defun my/org-capture-under-current-main-heading ()
  "Capture a TODO under the current main heading (level 1) in the Org buffer."
  (if (eq major-mode 'org-mode)
      `(file+function ,(buffer-file-name) my/org-find-main-heading)
    `(file+headline "~/path/to/your/default/orgfile.org" "Tasks"))
  (shell-command "bash ~/repositories/bitbucket/williseed1/transcription/run_whisper.sh"))

(defun my/org-find-main-heading ()
  "Find the current main heading (level 1) to insert the captured item."
  (while (and (not (bobp)) (not (org-at-heading-p)) (org-up-heading-safe)))
  (org-end-of-subtree t))

;; Org Capture Templates
(after! org
  (setq org-capture-templates
        '(("T" "Task with Low Priority" entry
           (function my/org-capture-under-current-main-heading)
           "** TODO [#C] %?\n")
          ("g" "Goal (SMART)" entry
           (function my/org-capture-under-current-main-heading)
           "* %^{Goal Title}\n  Added on %U - Last reviewed on %U\n  :SMART:\n  :Specific:   %^{What exactly do you want to achieve?}\n  :Measurable: %^{How will you measure progress and know if you've achieved the goal?}\n  :Achievable: %^{Is this goal realistic with effort and commitment? Do you have the resources to achieve it?}\n  :Relevant:   %^{Why is this goal important to you or your organization? How does it align with other objectives?}\n  :Time-bound: %^{When do you want to achieve this goal? Set a specific date or timeframe.}\n  :END:\n"))))

(defun my/org-archive-then-delete-subtree ()
  "Archive the current subtree and then delete it."
  (interactive)
  (org-archive-subtree-default)
  (org-cut-subtree))

(defun org-focus-personal()
  "Set Agenda focus on personal tasks"
  (interactive)
  (setq org-agenda-files '("~/personal-repos/org/today.org"
                           "~/personal-repos/org/despues.org")))
(defun org-focus-work()
  "Set Agenda focus on work tasks"
  (interactive)
  (setq org-agenda-files '("~/personal-repos/org/work.org")))

(defun org-focus-despues()
  "Set Agenda focus on despues tasks"
  (interactive)
  (setq org-agenda-files '("~/personal-repos/org/despues.org")))

(defun org-focus-today()
  "Set Agenda focus on today tasks"
  (interactive)
  (setq org-agenda-files '("~/personal-repos/org/today.org")))

(defun org-focus-study()
  "Set Agenda focus on study tasks"
  (interactive)
  (setq org-agenda-files '("~/personal-repos/org/study.org")))

(defun org-focus-all()
  "Set Agenda focus on all tasks"
  (interactive)
  (setq org-agenda-files '("~/personal-repos/org/")))

(defun my/org-archive-done-tasks ()
  "Archive all DONE or CANCELLED tasks in the current Org file."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE|CANCELLED" 'file))

(defun my/org-schedule-out-selected-tasks (days)
  "Reschedule selected tasks by a specified number of days."
  (interactive "nNumber of days to reschedule: ")
  (org-map-entries
   (lambda ()
     (let ((scheduled (org-entry-get nil "SCHEDULED")))
       (when (and scheduled (not (org-entry-is-done-p)))
         (org-schedule nil (format-time-string "%Y-%m-%d" (time-add (org-read-date nil t scheduled) (days-to-time days)))))))
   nil 'region))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("►" "▷" "■" "□" "●" "◉" "○" "◌" "★" "☆")))

;; Function to customize org headline faces
(defun my/org-mode-visual-fill ()
  (set-face-attribute 'org-level-1 nil :weight 'normal :height 1.15 :foreground "#98C379")
  (set-face-attribute 'org-level-2 nil :weight 'normal :height 1.12 :foreground "#61AFEF")
  (set-face-attribute 'org-level-3 nil :weight 'normal :height 1.09 :foreground "#E06C75")
  (set-face-attribute 'org-level-4 nil :weight 'normal :height 1.06 :foreground "#56B6C2")
  (set-face-attribute 'org-level-5 nil :weight 'normal :height 1.03 :foreground "#E5C07B")
  (set-face-attribute 'org-level-6 nil :weight 'normal :height 1.0 :foreground "#61AFEF")
  (set-face-attribute 'org-level-7 nil :weight 'normal :height 0.97 :foreground "#ABB2BF")
  (set-face-attribute 'org-level-8 nil :weight 'normal :height 0.94 :foreground "#FF6C6B")
  (set-face-attribute 'org-level-9 nil :weight 'normal :height 0.91 :foreground "#FF6C6B")
  (set-face-attribute 'org-level-10 nil :weight 'normal :height 0.88 :foreground "#FF6C6B"))

(add-hook 'org-mode-hook 'my/org-mode-visual-fill)



(defun org-download-clipboard-and-fold ()
  "Downloads an image from the clipboard into the current Org buffer and folds it."
  (interactive)
  (org-download-clipboard)  ; Download the image
  (org-toggle-inline-images))


(defun my/org-diagram-view ()
  "Run the org_to_dot.py script on the current buffer's file."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (error "Current buffer is not visiting a file"))
    (let ((default-directory (file-name-directory file)))
      (shell-command (format "python3 ~/.doom.d/python-scripts/org-diagram/org-diagram.py %s" (shell-quote-argument (expand-file-name file))))
      (let ((image-file (expand-file-name "output.png" default-directory)))
        (if (file-exists-p image-file)
            (progn
              (find-file-other-window image-file)
              (delete-other-windows)
              (message "Generated image saved as output.png and opened in a new buffer fullscreen"))
          (message "Failed to generate image (Check capitalization like 'DONE' not 'Done')"))))))


;; Define the custom paste function
(defun my/custom-paste-with-format ()
  "Paste the yanked text with the same number of asterisks as the current line at the beginning of a new line and return to normal mode."
  (interactive)
  (let* ((text (current-kill 0))
         (text (string-trim-right text))
         (heading-prefix (save-excursion
                           (beginning-of-line)
                           (when (looking-at "\\(\\*+\\)")
                             (match-string 1)))))
    (move-end-of-line nil)
    (insert "\n" heading-prefix " " text)
    (evil-normal-state)))

;; Babel Source Blocks
(defun my/org-insert-python-block ()
  "Insert a Python source code block in org-mode."
  (interactive)
  (insert "#+begin_src python\n\n#+end_src")
  (forward-line -1)
  (org-edit-src-code))

(defun my/org-insert-bash-block ()
  "Insert a Bash source code block in org-mode."
  (interactive)
  (insert "#+begin_src bash\n\n#+end_src")
  (forward-line -1)
  (org-edit-src-code))

(defun my/org-insert-emacs-lisp-block ()
  "Insert an Emacs Lisp source code block in org-mode."
  (interactive)
  (insert "#+begin_src emacs-lisp\n\n#+end_src")
  (forward-line -1)
  (org-edit-src-code))

;; Keybinding for Org mode to insert source blocks
(map! :map org-mode-map
      :localleader
      :desc "Insert source block" "I" nil
      (:prefix ("I" . "Insert Source Block")
       :desc "Insert Python block" "p" #'my/org-insert-python-block
       :desc "Insert Bash block" "s" #'my/org-insert-bash-block
       :desc "Insert Emacs Lisp block" "l" #'my/org-insert-emacs-lisp-block))
