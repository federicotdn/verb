;;; verb-util.el --- Utilities for Verb  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>
;; Homepage: https://github.com/federicotdn/verb
;; Keywords: tools
;; Package-Version: 2.16.0
;; Package-Requires: ((emacs "26.3"))

;; This file is NOT part of GNU Emacs.

;; verb is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; verb is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verb.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; General utility variables and functions for Verb.

;;; Code:

(defconst verb-util--log-buffer-name "*Verb Log*"
  "Default name for log buffer.")

(defconst verb-util--log-levels '(D I W E)
  "Log levels for the log buffer.
D = Debug.
I = Information.
W = Warning.
E = Error.")

(defface verb-util--log-debug '((t :inherit font-lock-constant-face))
  "Face for highlighting D entries in the log buffer."
  :group 'verb)

(defface verb-util--log-info '((t :inherit homoglyph))
  "Face for highlighting I entries in the log buffer."
  :group 'verb)

(defface verb-util--log-warning '((t :inherit warning))
  "Face for highlighting W entries in the log buffer."
  :group 'verb)

(defface verb-util--log-error '((t :inherit error))
  "Face for highlighting E entries in the log buffer."
  :group 'verb)

(defconst verb-util--http-header-parse-regexp
  "^\\s-*\\([[:alnum:]_-]+\\)\\s-*:\\(.*\\)$"
  "Regexp for parsing HTTP headers.")

(define-derived-mode verb-util-log-mode special-mode "Verb[Log]"
  "Major mode for displaying Verb logs.

Each line contains a short message representing an event that has been
logged:

  The first part of each line is a number that represents what
  request has generated this event (the number identifies the request).
  If an event does not correspond to any request, \"-\" is used instead.
  If the first part of the line is empty, then the event corresponds to
  the same request from the previous line.

  The second part of each line represents the level at which this event
  has been logged, I for information, W for warnings and E for errors.

  The third part of each line is the message itself.

If this buffer is killed, it will be created again when the next
message is logged.  To turn off logging, set `verb-enable-log' to nil."
  (font-lock-add-keywords
   nil '(;; Request number, e.g. 10.
         ("^[[:digit:]-]+\\s-"
          (0 'bold))
         ;; Log level D after request number.
         ("^[[:digit:]-]*\\s-+\\(D\\)"
          (1 'verb-util--log-debug))
         ;; Log level I after request number.
         ("^[[:digit:]-]*\\s-+\\(I\\)"
          (1 'verb-util--log-info))
         ;; Log level W after request number.
         ("^[[:digit:]-]*\\s-+\\(W\\)"
          (1 'verb-util--log-warning))
         ;; Log level E after request number.
         ("^[[:digit:]-]*\\s-+\\(E\\)"
          (1 'verb-util--log-error)))))

(defun verb-util--log (request level &rest args)
  "Log a message in the *Verb Log* buffer.
REQUEST must be a number corresponding to an HTTP request made.  LEVEL
must be a value in `verb-util--log-levels'.  Use the remaining ARGS to call
`format', and then log the result in the log buffer.

If `verb-enable-log' is nil, do not log anything."
  (setq request (if request (number-to-string request) "-"))
  (unless (member level verb-util--log-levels)
    (user-error "Invalid log level: \"%s\"" level))
  (when (bound-and-true-p verb-enable-log) ; Var is defined in verb.el.
    (with-current-buffer (get-buffer-create verb-util--log-buffer-name)
      (unless (derived-mode-p 'verb-util-log-mode)
        (verb-util-log-mode))
      (let ((inhibit-read-only t)
            (last "")
            (msg (apply #'format args)))
        (unless (= (buffer-size) 0)
          (insert "\n"))
        ;; Get last logged request number.
        (when (re-search-backward "^\\(-\\|[[:digit:]]+\\)\\s-"
                                  nil t)
          (setq last (match-string 1)))
        (goto-char (point-max))
        ;; Log new message.
        (insert (format "%-5s %s  "
                        (if (string= last request)
                            (make-string (length request) ? )
                          request)
                        level)
                msg)
        ;; If logged messaged contained newlines, add a blank line
        ;; to make things more readable.
        (when (string-match-p "\n" msg)
          (newline)))
      (dolist (w (get-buffer-window-list (current-buffer) nil t))
        (set-window-point w (point-max))))))

(defun verb-util-show-log ()
  "Switch to the *Verb Log* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create verb-util--log-buffer-name)))

(defun verb-util--nonempty-string (s)
  "Return S. If S is the empty string, return nil."
  (if (string= s "")
      nil
    s))

(defun verb-util--string= (s1 s2)
  "Return non-nil if strings S1 and S2 are equal, ignoring case."
  (string= (downcase s1) (downcase s2)))

(provide 'verb-util)
;;; verb-util.el ends here
