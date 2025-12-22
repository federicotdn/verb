;;; verb-restclient.el --- Compat for restclient.el  -*- lexical-binding: t -*-

;; Copyright (C) 2025  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>
;; Homepage: https://github.com/federicotdn/verb
;; Keywords: tools
;; Package-Version: 3.1.0
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

;; Utilities for Verb <-> restclient.el compatibility, such as importing
;; restclient.el files to Verb.
;; See: https://github.com/pashky/restclient.el

;;; Code:

(require 'cl-lib)

(cl-defstruct verb-restclient--request
  comments
  body)

(defun verb-restclient--build-request (comments body)
  "Build a request structure from COMMENTS and BODY strings.
Unsupported restclient.el features are prefixed with comments."
  (setq body (replace-regexp-in-string "^\\(:.+?:=.+\\)"
                                       "# [Unsupported] \\1"
                                       (string-trim body)))
  (setq body (replace-regexp-in-string "^\\(->.+\\)"
                                       "# [Unsupported] \\1"
                                       body))
  (make-verb-restclient--request :comments (string-trim comments) :body body))

(defun verb-restclient--parse (buf)
  "Parse restclient.el requests from BUF.
Return a list of `verb-restclient--request' structures."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "#.+mode:\\s-*restclient" (line-end-position) t)
        (end-of-line)
        (forward-char))

      (let (requests comments-start comments-end comments body done)
        (catch 'end
          (while t
            (setq comments-start (point)
                  done nil)
            ;; Go over comments
            (while (re-search-forward "^#.*$" (line-end-position) t)
              (end-of-line)
              (if (eobp) (throw 'end nil) (forward-char)))
            (setq comments-end (point))
            ;; Go over the actual request
            (while (and (not done)
                        (re-search-forward "^\\(?:[^#].*\\)?$"
                                           (line-end-position) t))
              (end-of-line)
              (if (or (eobp)
                        (string-empty-p (string-trim
                                         (buffer-substring (point)
                                                           (point-max)))))
                  (setq done t)
                (forward-char)))

            (setq comments (buffer-substring comments-start comments-end)
                  body (buffer-substring comments-end (point)))

            (push (verb-restclient--build-request comments body) requests)
            (when done (throw 'end nil))))
        (nreverse requests)))))

(defun verb-restclient--import (validator)
  "Import restclient.el requests from current buffer to Verb format.
VALIDATOR is a function that validates each request body.
Return a buffer with the imported results."
  (let ((name (buffer-name))
        (requests (verb-restclient--parse (current-buffer)))
        (i 1)
        comments body valid)
    (with-current-buffer (get-buffer-create
                          (generate-new-buffer-name "*restclient.el Import*"))
      (insert (format "* Imported from %s (restclient.el)  :verb:\n" name))
      (insert
       "# NOTE: Some imported requests may not be correctly formatted.\n"
       "# It's recommended to go over all of them carefully, specially the\n"
       "# ones marked with the [Unsupported] label.\n\n")
      (dolist (req requests)
        (setq comments (verb-restclient--request-comments req)
              body (verb-restclient--request-body req))
        (setq valid
              (ignore-errors
                (funcall validator (concat comments "\n" body)) t))
        (insert (format "** Request #%s\n" i))
        (unless valid
          (insert
           "# [Unsupported] This request could not be parsed correctly.\n"))
        (insert comments "\n")
        (unless (string-empty-p body)
          (insert body "\n"))
        (newline)
        (setq i (1+ i)))
      (org-mode)
      (goto-char (point-min))
      (current-buffer))))

(provide 'verb-restclient)
;;; verb-restclient.el ends here
