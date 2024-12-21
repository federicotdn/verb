;; init.el --- Manual testing setup for Verb  -*- lexical-binding: t; -*-

;; Build Verb autoloads and load them
(require 'package)
(package-generate-autoloads "verb" ".")
(load "verb-autoloads.el")

;; Set up misc. Emacs config
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-face-attribute 'default nil :height (string-to-number (getenv "FONT_SIZE")))
(setq initial-scratch-message nil)
(load-theme 'wombat)
(setq url-debug t)
(toggle-debug-on-error)

;; Set up Org and Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((verb . t)))
(setq org-confirm-babel-evaluate nil)

;; Set up Verb
(setq verb-auto-kill-response-buffers 3)

;; Keybindings
(with-eval-after-load 'org (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
(global-set-key (kbd "M-l") 'switch-to-buffer)
(global-set-key (kbd "M-o") 'other-window)

;; Create a useful initial window/buffer setup
(with-current-buffer (get-buffer "*scratch*")
  (org-mode)
  (verb-mode)
  (insert "* Test :verb:")
  (newline)
  (insert "get http://localhost:8000/endpoints"))
(let ((inhibit-message t))
  (dired "examples"))
(delete-other-windows)
