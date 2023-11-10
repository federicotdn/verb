;; init-check.el --- Init for Verb linting/checks  -*- lexical-binding: t; -*-

(require 'package)

(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg '(cl-lib let-alist compat package-lint xr relint))
  (unless (package-installed-p pkg)
    (package-install pkg)))
