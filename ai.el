;;; ai2.el -*- lexical-binding: t; -*-

(defun setup-ai-file ()
  "Ensure the AI file exists and open it for editing."
  (let ((file-path (expand-file-name "~/Dropbox/org/ai/ai.org")))
    (make-directory (file-name-directory file-path) t)
    (find-file file-path)
    (goto-char (point-max))))

(defun activate-org-ai-mode ()
  "Activate org-ai-mode if available."
  (when (fboundp 'org-ai-mode) (org-ai-mode 1)))

(defun start-transcription ()
  "Start voice transcription process."
  (message "Recording. Press butterfly key to stop and transcribe.")
  (when (fboundp 'my-whisper-run)
    (insert " ")
    (end-of-line)
    (my-whisper-run)
    (message "Transcription complete. Process with 'C-c C-c'.")))

(defun my-chat-gpt-initial()
  "Sets up Org AI environment in a specified file for the first transcription session."
  (interactive)
  (setup-ai-file)
  (insert "* first black thick tiger\n#+begin_ai\n\n#+end_ai\n")
  (goto-char (point-min))
  (forward-line 2)
  (insert "[ME]: ")
  (activate-org-ai-mode)
  (start-transcription)
  (save-buffer))

(defun my-chat-gpt-continue ()
  "Continues voice transcription in an existing Org AI environment within the specified file."
  (interactive)
  (setup-ai-file)
  (search-backward "[ME]:")
  (end-of-line)
  (activate-org-ai-mode)
  (start-transcription)
  (save-buffer))

(defun my-chat-gpt-integrated ()
  "Determines whether to start a new transcription session or continue an existing one in the specified Org file."
  (interactive)
  (setup-ai-file)
  (if (> (buffer-size) 0)
      (my-chat-gpt-continue)
    (my-chat-gpt-initial)))

(defun my-chatgpt-no-audio ()
  "Switches to the '*AI-Org-Mode*' buffer, placing the cursor after the last '[ME]: ' marker before the '#+end_ai'.
   Activates org-ai-mode if it is not already active."
  (interactive)
  (setup-ai-file)
  (activate-org-ai-mode)
  (search-backward "[ME]:")
  (end-of-line)
  (save-buffer))

(defun my-chat-gpt-new-conversation-block ()
  "Creates a new conversation block in the specified Org file with a predetermined heading."
  (interactive)
  (setup-ai-file) ; Setup and open the AI file
  (let* ((ordinals '("first" "second" "third" "fourth" "fifth"))
         (colors '("black" "green" "red" "blue" "yellow"))
         (adjectives '("thick" "small" "large" "slim" "tall"))
         (animals '("tiger" "dog" "cat" "bear" "fox"))
         (block-count (with-temp-buffer
                        (insert-file-contents "~/Dropbox/org/ai/ai.org")
                        (count-matches "#\\+begin_ai")))
         (index (mod block-count (length ordinals)))
         (ordinal (nth index ordinals))
         (color (nth index colors))
         (adjective (nth index adjectives))
         (animal (nth index animals)))
    (insert (format "\n* %s %s %s %s\n#+begin_ai\n\n#+end_ai\n" ordinal color adjective animal))
    (forward-line -2)
    (insert "[ME]: ")
    (activate-org-ai-mode)
    (start-transcription)
    (save-buffer)))

(defun my-whisper-run ()
  "Calls the Python script for Whisper AI using an external shell script."
  (interactive)
  (evil-insert-state)
  (shell-command "bash ~/personal-repos/transcription/run_whisper.sh"))
