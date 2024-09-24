;;; key_bindings.el --- Description -*- lexical-binding: t; -*-

;; 1. Leader Keybindings for custom functions/functionality
(map! :leader
      :desc "Anki Cloze Wrap"
      "k w" #'my/wrap-with-anki-cloze)

(map! :leader
      :desc "Org Archive Done"
      "m R" #'my/org-archive-done-tasks)

(map! :leader
      :desc "Copy Dir Path"
      "y d" #'my/copy-current-directory-path)

(map! :leader
      :desc "Chmod Current File"
      "f C" #'my/chmod-current-file)

(map! :leader
      :desc "Copy and delete whole buffer"
      "d d" #'my/copy-and-delete-whole-buffer)

(map! :leader
      :desc "Docker Open Bash"
      "d z" #'my/docker-bash)

(map! :leader
      :desc "Docker Kali"
      "k d" #'my/docker-kali)

(map! :leader
      :desc "Copy file name"
      "y f" #'my/copy-file-name-to-clipboard)

(map! :leader
      :desc "Copy file with name"
      "y F" #'my/copy-filename-and-contents)

(map! :leader
      :desc "Copy lines with phrase"
      "y l" #'my/copy-lines-containing-phrase)

(map! :leader
      :desc "Copy whole buffer"
      "y y" #'my/copy-whole-buffer)


(map! :leader
      :desc "Close Buffer & Window"
      "w C" #'my/close-buffer-and-window)

(map! :leader
      :desc "Grafana Replace"
      "r g" #'my/grafana-replace-configs)

(map! :leader
      :desc "Inserts a newline and returns to insert mode."
      "i n" #'my/insert-newline-and-return-to-insert)

(map! :leader
      :desc "Custom repository clone function."
      "m c" #'my/magit-clone-repository)

(map! :leader
      :desc "Open vterm in current directory"
      "t t" #'my/open-vterm-in-current-directory)

(map! :leader
      :desc "Pastes the clipboard content at the end of the current line with a space."
      "p z" #'my/paste-at-end-of-line)

(map! :leader
      :desc "Replace the entire buffer content with the clipboard content."
      "y r" #'my/replace-buffer-with-clipboard)

(map! :leader
      :desc "Toggle Markdown Preview Full Screen"
      "m f" #'my/markdown-preview-fullscreen)

(map! :leader
      :desc "Git Auto Commit-Push"
      "g A" #'my/magit-automatic-version-control)

(map! :leader
      (:prefix ("v" . "v-environment")
       :desc "Activate Virtual Env"             "a" #'my/pyvenv-activate
       :desc "Deactivate Virtual Env"           "d" #'pyvenv-deactivate
       :desc "Create Virtual Env"               "c" #'my/pyvenv-create
       :desc "Last AWS Profile"                 "l" #'my/set-last-aws-profile-used
       :desc "Export AWS Profile"               "P" #'my/set-and-export-aws-profile
       :desc "Default SSH User"                 "h" #'my/set-ssm-ssh-default-user
       :desc "AWS Default Region"                 "r" #'my/set-and-export-aws-default-region
       ))



(map! :leader
      :desc "Go to First Complete Quote"
      "i q" #'my/goto-first-complete-quote)

(map! :leader
      :desc "Insert **"
      "i a" #'my/insert-asterisks-and-space)


;;;;;;;;;2. Remapping default functions;;;;;;;;;

(map! :leader
      :desc "Next Tab"
      "c n" #'centaur-tabs-forward)

(map! :leader
      :desc "Copy This File"
      "f c" #'doom/copy-this-file)

(map! :leader
      :desc "Open Docker"
      "d o" #'docker)

(map! :leader
      :desc "Agenda with TOMORROW tasks" "o a w" #'(lambda () (interactive) (org-agenda nil "w")))

(map! :leader
      :desc "Archive and delete subtree" "m z" #'my/org-archive-then-delete-subtree)

(map! :leader
      :desc "Comment Region"
      "c c" #'comment-region)

(map! :leader
      (:prefix "c"
       :desc "Change Workspace" "W" #'+workspace/switch-to))

(map! :leader
      (:prefix "m"
       :desc "Org Capture" "j" #'org-capture))

(map! :leader
      (:prefix ("l" . "Lisp ")
       :desc "Evaluate Elisp" "l" #'eval-last-sexp-on-line))

(map! :leader
      (:prefix "c"
       :desc "Next Workspace" "w" #'+workspace/switch-right))

(map! :leader
      (:prefix "n"
       :desc "Evil Next Buffer" "b" #'evil-next-buffer))

(map! :leader
      :desc "Open vterm"
      "t T" #'my/cycle-vterm-buffers)

(map! :leader
      :desc "Regex Replace"
      "r r" #'replace-regexp)

(map! :leader
      :desc "Replace String"
      "r s" #'replace-string)

(map! :leader
      :desc "Treemacs"
      "t m" #'treemacs)

(map! :leader
      :desc "Treemacs Remove Project"
      "t R" #'treemacs-remove-project-from-workspace)

(map! :leader
      :desc "Uncomment Region"
      "c u" #'uncomment-region)

(map! :leader
      :desc "Yank History"
      "y h" #'yank-from-kill-ring)

(map! :leader
      :desc "Fill Column Indicator"
      "t C" #'global-display-fill-column-indicator-mode)

global-display-fill-column-indicator-mode
(map! :leader
      :desc "Whisper Run"
      "d w" #'my-whisper-run)
(map! :leader
      :desc "Speak Greader"
      "d t" #'my-toggle-greader)

(map! :leader
      :prefix ("z" . "org-ai")
      :desc "Org Ai On Region"                "r" #'org-ai-on-region
      :desc "Org Ai Refactor Code"            "c" #'org-ai-refactor-code
      :desc "Org Ai Open Request Buffer"      "R" #'org-ai-open-request-buffer
      :desc "Org Ai Open Account Usage Page"  "u" #'org-ai-open-account-usage-page
      :desc "Org Ai On Project"               "p" #'org-ai-on-project
      :desc "Org Ai Prompt"                   "s" #'org-ai-prompt
      :desc "Org Ai Summarize"                "S" #'org-ai-summarize
      :desc "Org Ai Switch Chat Model"        "m" #'org-ai-switch-chat-model
      :desc "Org Ai Mark Last Region"         "l" #'org-ai-mark-last-region
      :desc "Org Ai Explain Code"             "e" #'org-ai-explain-code
      :desc "My Chat Gpt Integrated"          "g" #'my-chat-gpt-integrated
      :desc "Org Ai Prompt In New Buffer"     "n" #'org-ai-prompt-in-new-buffer
      :desc "My Chat Gpt New Conversation Block" "b" #'my-chat-gpt-new-conversation-block
      :desc "My Chatgpt No Audio"             "A" #'my-chatgpt-no-audio
      )


;; don't clock in anymore
;; (map! :leader
;;       :prefix ("c" . "Org Clock")
;;       :desc "Cancel Clock"      "C" #'org-clock-cancel             
;;       :desc "Display Clock"     "d" #'org-clock-display
;;       :desc "Clock In"          "i" #'org-clock-in
;;       :desc "Clock Out"         "o" #'org-clock-out
;;       :desc "Clock Go To"       "g" #'org-clock-goto
;;       :desc "Clock In Last"     "l" #'org-clock-in-last
;;       :desc "Clock Report"      "r" #'org-clock-report
;;       :desc "Clock Update"      "U" #'org-clock-update-time-maybe
;;       )

;;Encryption
;;When in list/epa key mode
;;e=encrypt, u=unmark key, m=mark key, v=verify sign, s=sign
;;d=delete keys, i=import keys, o=export keys, q=quit
;;when in dired press ';' to get to epa menu
(map! :leader
      :prefix ("e" . "Encryption")
      :desc "Encrypt Sops"              "s" #'sops-save-file
      :desc "Cancel Sops "              "c" #'sops-cancel
      :desc "Encrypt File"              "f" #'epa-encrypt-file
      :desc "List Keys"                 "l" #'epa-list-keys
      :desc "Import Keys"               "i" #'epa-list-keys
      :desc "Sign File"                 "S" #'epa-sign-file
      :desc "Encrypt Region"            "r" #'epa-encrypt-region
      )


(map! :leader
      :prefix ("d" . "Decryption")
      :desc "Sops Decrypt"              "s" #'sops-edit-file
      :desc "Verify File"               "v" #'epa-verify-file
      :desc "Decrypt File"              "f" #'epa-decrypt-file
      :desc "Decrypt Region"            "r" #'epa-decrypt-region
      )

;;tramp
(map! :leader
      :desc "Tramp SSM to Instance"
      "t i" #'my/tramp-ssm-to-aws-instance)
(map! :leader
      :desc "Tramp SSM Disconnect"
      "t d" #'my/tramp-end-ssm-session-and-remove-key)
(map! :leader
      :desc "Tramp SSM to Container"
      "t c" #'my/tramp-ssm-to-container)
(map! :leader
      :desc "Tramp Local Container"
      "t k" #'my/tramp-docker-init)
(map! :leader
      :desc "Tramp Sudo"
      "t s" #'my/tramp-sudo-init)

(map! :leader
      :desc "Tramp SSH"
      "t h" #'my/tramp-ssh-init)

(map! :leader
      :desc "SSM List Containers"
      "d l" #'my/list-docker-containers-ssm)

(map! :leader
      :desc "SSM Send Command"
      "s c" #'my/send-ssm-command-and-fetch-output)

(map! :leader
      :desc "Get Instance Info"
      "i i" #'my/ssm-get-instance-info)

(map! :leader
      :desc "AWS ECS"
      "c z" #'my/aws-ecs-workflow)

;;the keybind below replaced this(evil-ex "R!echo ")
(map! :leader
      :desc "Org Clipboard Paste"
      "i p" #'org-download-clipboard-and-fold)

(map! :leader
      :desc "Org Screenshot-Paste"
      "i s" #'org-download-screenshot)

(map! :leader
      :desc "Org Image Delete"
      "i d" #'org-download-delete)

(map! :leader
      :desc ""
      "i S" #'consult-yasnippet)


(map! :leader
      :desc "Edebug Function"
      "d f" #'edebug-defun)

(map! :leader
      :desc "Edebug Function"
      "d F" #'edebug-remove-instrumentation)

(map! :leader
      :desc "Toggle highlight and mark line"
      "m h" #'toggle-highlight-and-mark-line)

(map! :leader
      :desc "Delete all empty lines"
      "d e" #'delete-all-empty-lines
      :desc "Delete consecutive empty lines"
      "d c" #'delete-consecutive-empty-lines)


(map! :leader
      :desc "Switch Project: Directory"
      "p d" #'my/projectile-switch-project-to-dir
      :desc "Remove Known Project"
      "p x" #'projectile-remove-known-project
      )



(map! :leader
      :desc "AWS Mode"
      "c a" #'aws
      :desc "AWS Login"
      "c l" #'aws-login)

(map! :map org-mode-map
      :localleader
      "S" #'wrap-region-with-src-block)

;; Unbind the existing universal argument key
(map! :leader
      "u" nil)

;; Bind universal argument to SPC U
(map! :leader
      :desc "Universal argument"
      "U" #'universal-argument)

;; Bind winner undo and redo to new keys and update description
(map! :leader
      :desc "Undo"
      "u u" #'winner-undo
      :desc "Redo"
      "u r" #'winner-redo)
;; (map! :leader
;;       :desc ""
;;       " " #')
;;
;; (map! :leader
;;       (:prefix ("c" . "compute")
;;        :desc "AWS Menu" "a" (:prefix ("a" . "AWS")
;;                              :desc "Instance ID" "i" #'my/list-ec2-by-name-substring)))
;;
;;
(map! :map org-mode-map
      :localleader
      "v" #'my/org-diagram-view)

(map! :leader
      :desc "Undo"
      "u u" #'winner-undo
      :desc "Redo"
      "u r" #'winner-redo)

(map! :leader
      :desc "My Kill Buffer"
      "k k" #'my-vterm-or-kill-buffer)
;; Bind the custom paste function to the capital P key in org mode
(after! org
  (map! :map org-mode-map
        :n "P" #'my/custom-paste-with-format))


;; Setting descriptions for key prefixes using which-key
(after! which-key
  (which-key-add-key-based-replacements
    "SPC y" "copy commands"
    "SPC r" "replace string"
    "SPC c" "Compute & Comment"
    "SPC m" "random?"
    "SPC u" "undo"

    ;; Add more descriptions for other prefixes if needed
    ))
(provide 'key_bindings)
;;; key_bindings.el ends here
