;;; post.el --- A new HTTP client for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2019  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>

;; post is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; post is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with post.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Main module for post.

;;; Code:
(require 'eieio)

(defun post--back-to-heading ()
  "Move to the previous heading.
Or, move to beggining of this line if it's a heading.  If there are no
headings, move to the beggining of buffer.  Return t if a heading was
found."
  (if (ignore-errors (outline-back-to-heading t))
      t
    (goto-char (point-min))
    nil))

(defun post--next-heading ()
  "Move to the next heading."
  (outline-next-heading))

(defun post--section-end ()
  "Skip forward to before the next heading.
If there is no next heading, skip to the end of the buffer."
  (outline-next-preface))

(defun post--outline-level ()
  "Return the outline level.
Level zero indicates that no headings exist."
  (save-match-data
    (save-excursion
      (if (post--back-to-heading)
	  (funcall outline-level)
	0))))

(defun post--heading-has-children-p ()
  "Return non-nil if the heading has any children (sub-headings)."
  (save-match-data
    (save-excursion
      (let ((level (post--outline-level)))
	(post--back-to-heading)
	(when (post--next-heading)
	  (> (post--outline-level) level))))))

(defun post--heading-has-content-p ()
  "Return non-nil if the heading is followed by text contents."
  (save-match-data
    (save-excursion
      (if (post--back-to-heading)
	  ;; A heading was found
	  (let ((line (line-number-at-pos)))
	    (post--section-end)
	    (> (line-number-at-pos) line))
	;; Buffer has no headings
	(< 0 (buffer-size))))))

(defun post--heading-contents ()
  "Return the heading's text contents.
Return nil if `post--heading-has-content-p' returns nil."
  (when (post--heading-has-content-p)
    (let ((start (save-excursion
		   (when (post--back-to-heading)
		     (end-of-line)
		     (forward-char))
		   (point)))
	  (end (save-excursion (post--section-end) (point))))
      (buffer-substring-no-properties start end))))

(defconst post--http-methods '("GET" "POST")
  "List of valid HTTP methods.")

(defun post--http-method-p (m)
  "Return non-nil if M is a valid HTTP method."
  (member m post--http-methods))

(defclass post--request-spec ()
  ((method :initarg :method
	   :initform "GET"
	   :type post--http-method
	   :documentation "HTTP method.")
   (url :initarg :url
	:type string
	:documentation "Request URL.")
   (headers :initarg :headers
	    :initform ()
	    :type (or null cons)
	    :documentation "HTTP headers.")
   (body :initarg :body
	 :initform ""
	 :type string
	 :documentation "Request body."))
  "Represents an HTTP request to be made.")

(define-derived-mode post-mode outline-mode "Post"
  "Enable or disable post mode."
  (setq-local outline-regexp "[-\^L]+"))

(provide 'post)
;;; post.el ends here
