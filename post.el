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

(defconst post--comment-character "#"
  "Character to use to mark commented lines.")

(defconst post--outline-character "-"
  "Character to use to create headings.")

(defconst post--http-methods '("GET" "POST")
  "List of valid HTTP methods.")

(defconst post--template-keyword "TEMPLATE"
  "Keyword to use when defining request templates without defined HTTP
methods.")

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

(define-derived-mode post-mode outline-mode "Post"
  "Enable or disable post mode."
  (setq-local outline-regexp (concat "[" post--outline-character "\^L]+")))

(defun post--http-method-p (m)
  "Return non-nil if M is a valid HTTP method."
  (member m post--http-methods))

(defclass post--request-spec ()
  ((method :initarg :method
	   :initform nil
	   :type (or null post--http-method)
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

(defun post--http-methods-regexp ()
  "Return a regexp that matches a HTTP method.
HTTP methods are defined in `post--http-methods'.
Additionally, allow matching `post--template-keyword'."
  (mapconcat #'identity
	     (append post--http-methods
		     (list post--template-keyword)
		     (mapcar #'downcase post--http-methods)
		     (list (downcase post--template-keyword)))
	     "\\|"))

(defun post--request-spec-from-text (text)
  "Create a `post--request-spec' from a text specification.

The text format for defining requests is:

METHOD [URL]
[HEADER-NAME1: HEADER-VALUE1]
[HEADER-NAME2: HEADER-VALUE2]...

[BODY]

METHOD must be a method matched by `post--http-methods-regexp'.
URL can be an empty string.
Headers and BODY can be separated by a blank line, which will be
ignored."
  (let (method url headers body)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Skip initial blank lines and commments.
      (while (and (re-search-forward (concat "^\\(\\s-*"
					     post--comment-character
					     ".*\\)?$")
				     (line-end-position) t)
		  (not (eobp)))
	(forward-char))
      ;; Read HTTP method and URL
      (when (re-search-forward (concat "^\\s-*\\("
				       (post--http-methods-regexp)
				       "\\)\\s-+\\(.*\\)$")
			       (line-end-position) t)
	(setq method (upcase (match-string 1))
	      url (match-string 2)))
      (if method
	  (when (string= method post--template-keyword)
	    (setq method nil))
	(user-error (concat "Could not read a valid HTTP method\n"
			    "Valid HTTP methods are: %s\n"
			    "Additionally, you can also specify %s\n"
			    "(Matching is case-insensitive)")
		    post--http-methods post--template-keyword))
      ;; Skip newline after URL line
      (when (not (eobp)) (forward-char))
      ;; Search for HTTP headers
      ;; Stop as soon as we find a blank line or a non-matching line
      (while (re-search-forward "^\\s-*\\(\\w+\\)\\s-*:\\s-?\\(.*\\)$"
				(line-end-position) t)
	(push (cons (match-string 1) (match-string 2)) headers)
	(when (not (eobp)) (forward-char)))
      (setq headers (reverse headers))
      ;; Allow a blank like to separate headers and body (not required)
      (when (re-search-forward "^$" (line-end-position) t)
	(when (not (eobp)) (forward-char)))
      ;; The rest of the buffer is the request body
      (setq body (buffer-substring (point) (point-max)))
      ;; Return a `post--request-spec'
      (post--request-spec :method method
			  :url url
			  :headers headers
			  :body body))))

(provide 'post)
;;; post.el ends here
