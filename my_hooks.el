;;; my_hooks.el -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook 'real-auto-save-mode)

(add-hook 'text-mode-hook 'real-auto-save-mode)

(add-hook 'after-change-major-mode-hook 'my-disable-ws-butler)

(add-hook 'org-mode-hook 'my/org-mode-visual-fill)

(advice-add 'kill-region :around #'my/kill-region-advice)

(add-hook 'dired-mode-hook 'org-download-enable)

(add-hook 'kill-buffer-query-functions #'my/kill-vterm-without-confirmation)
