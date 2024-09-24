;;; quote-string.el -*- lexical-binding: t; -*-

(defun my/surround-word-or-region (symbol)
  "Surround the current word or region with the given SYMBOL."
  (interactive "cEnter symbol to surround with: ")
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert symbol)
        (goto-char beg)
        (insert symbol))
    (let (beg end)
      (save-excursion
        ;; Move backward to the start of the word (including special characters)
        (skip-chars-backward "^ \t\n\r")
        (setq beg (point))
        ;; Move forward to the end of the word (including special characters)
        (skip-chars-forward "^ \t\n\r")
        (setq end (point)))
      (when (and beg end)
        (goto-char end)
        (insert symbol)
        (goto-char beg)
        (insert symbol)))))

(defun my/surround-with-pair (open close)
  "Surround the current word or region with the given OPEN and CLOSE symbols."
  (interactive "cEnter opening symbol: \ncEnter closing symbol: ")
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open))
    (let (beg end)
      (save-excursion
        ;; Move backward to the start of the word (including special characters)
        (skip-chars-backward "^ \t\n\r")
        (setq beg (point))
        ;; Move forward to the end of the word (including special characters)
        (skip-chars-forward "^ \t\n\r")
        (setq end (point)))
      (when (and beg end)
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open)))))

(defun my/surround-with-parentheses ()
  "Surround with parentheses."
  (interactive)
  (my/surround-with-pair "(" ")"))

(defun my/surround-with-brackets ()
  "Surround with brackets."
  (interactive)
  (my/surround-with-pair "[" "]"))

(defun my/surround-with-braces ()
  "Surround with braces."
  (interactive)
  (my/surround-with-pair "{" "}"))

(defun my/surround-with-single-quotes ()
  "Surround with single quotes."
  (interactive)
  (my/surround-word-or-region "'"))

(defun my/surround-with-double-quotes ()
  "Surround with double quotes."
  (interactive)
  (my/surround-word-or-region "\""))

(defun my/surround-with-asterisks ()
  "Surround with asterisks."
  (interactive)
  (my/surround-word-or-region "*"))

(defun my/surround-with-backticks ()
  "Surround with backticks."
  (interactive)
  (my/surround-word-or-region "`"))

(defun my/surround-with-dashes ()
  "Surround with dashes."
  (interactive)
  (my/surround-word-or-region "-"))

(defun my/surround-with-dollar-signs ()
  "Surround with dollar signs."
  (interactive)
  (my/surround-word-or-region "$"))

(defun my/surround-with-caret ()
  "Surround with caret."
  (interactive)
  (my/surround-word-or-region "^"))

(defun my/surround-with-hash ()
  "Surround with hash."
  (interactive)
  (my/surround-word-or-region "#"))

(defun my/surround-with-angle-brackets ()
  "Surround with angle brackets."
  (interactive)
  (my/surround-with-pair "<" ">"))

(defun my/surround-with-colon ()
  "Surround with angle brackets."
  (interactive)
  (my/surround-with-pair ":" ":"))

(map! :leader
      :desc "Surround with backticks"
      "q `" #'my/surround-with-backticks
      :desc "Surround with parentheses"
      "q (" #'my/surround-with-parentheses
      :desc "Surround with parentheses"
      "q )" #'my/surround-with-parentheses
      :desc "Surround with brackets"
      "q [" #'my/surround-with-brackets
      :desc "Surround with brackets"
      "q ]" #'my/surround-with-brackets
      :desc "Surround with braces"
      "q {" #'my/surround-with-braces
      :desc "Surround with braces"
      "q }" #'my/surround-with-braces
      :desc "Surround with single quotes"
      "q '" #'my/surround-with-single-quotes
      :desc "Surround with double quotes"
      "q \"" #'my/surround-with-double-quotes
      :desc "Surround with asterisks"
      "q *" #'my/surround-with-asterisks
      :desc "Surround with dashes"
      "q -" #'my/surround-with-dashes
      :desc "Surround with dollar signs"
      "q $" #'my/surround-with-dollar-signs
      :desc "Surround with caret"
      "q ^" #'my/surround-with-caret
      :desc "Surround with hash"
      "q #" #'my/surround-with-hash
      :desc "Surround with angle brackets"
      "q <" #'my/surround-with-angle-brackets
      :desc "Surround with angle brackets"
      "q >" #'my/surround-with-angle-brackets
      "q :" #'my/surround-with-colon)
