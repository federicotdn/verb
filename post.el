;;; post.el --- A new HTTP client for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2019  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>
;; Homepage: https://github.com/federicotdn/post
;; Keywords: http
;; Package-Requires: ((emacs "26"))

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
(require 'subr-x)
(require 'url)

(defconst post--comment-character "#"
  "Character to use to mark commented lines.")

(defconst post--outline-character "-"
  "Character to use to create headings.")

(defconst post--http-methods '("GET" "POST" "DELETE")
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

(defun post--request-spec-from-heading ()
  "Return a `post--request-spec' generated from the heading's text contents."
  (let ((text (post--heading-contents)))
    (when (or (null text)
	      (= (length (string-trim text)) 0))
      (user-error "%s" "Current heading is empty (no text contents)"))
    (post--request-spec-from-text text)))

(defun post-execute-request-on-point ()
  "Send the request specified by the selected heading's text contents."
  (interactive)
  (post--request-spec-execute (post--request-spec-from-heading)))

(defvar post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'post-execute-request-on-point)
    map)
  "Keymap for post mode.")

(defun post--setup-font-lock-keywords ()
  "Configure font lock keywords for `post-mode'."
  (font-lock-add-keywords
   nil
   `(;; GET www.example.com
     (,(concat "^\\(" (post--http-methods-regexp) "\\) *\\(.*\\)$")
      (1 'font-lock-constant-face)
      (2 'font-lock-function-name-face))
     ;; Content-type: application/json
     ("^[[:alpha:]-]+: .+$"
      (0 'font-lock-doc-face))
     ;; # This is a comment
     (,(concat "^\\s-*" post--comment-character ".*$")
      (0 'font-lock-comment-face))))
  (setq font-lock-keywords-case-fold-search t))

(define-derived-mode post-mode outline-mode "Post"
  "Enable or disable post mode."
  (setq-local outline-regexp (concat "[" post--outline-character "\^L]+"))
  (post--setup-font-lock-keywords))

(defun post--http-method-p (m)
  "Return non-nil if M is a valid HTTP method."
  (member m post--http-methods))

(defun post--http-headers-p (h)
  "Return non-nil if H is an alist of (HEADER . VALUE) elements.
HEADER and VALUE must be nonempty strings."
  (when (consp h)
    (catch 'end
      (dolist (elem h)
	(unless (and (consp elem)
		     (stringp (car elem))
		     (stringp (cdr elem))
		     (< 0 (length (car elem)))
		     (< 0 (length (cdr elem))))
	  (throw 'end nil)))
      t)))

(defclass post--request-spec ()
  ((method :initarg :method
	   :type (or null post--http-method)
	   :documentation "HTTP method.")
   (url :initarg :url
	:type (or null url)
	:documentation "Request URL.")
   (headers :initarg :headers
	    :initform ()
	    :type (or null post--http-headers)
	    :documentation "HTTP headers.")
   (body :initarg :body
	 :initform nil
	 :type (or null string)
	 :documentation "Request body."))
  "Represents an HTTP request to be made.")

(cl-defmethod post--request-spec-url-string ((rs post--request-spec))
  "Return RS's url member as a string if it is non-nil."
  (let ((url (oref rs :url)))
    (when url
      (url-recreate-url url))))

(defun post--request-spec-callback (status rs)
  "Callback for `post--request-spec-execute' for request RS.
More response information can be read from STATUS."
  (switch-to-buffer-other-window (current-buffer)))

(cl-defmethod post--request-spec-execute ((rs post--request-spec))
  "Execute the HTTP request described by RS."
  (unless (oref rs :method)
    (user-error "%s" (concat "No HTTP method specified\n"
			     "Make sure you specify a concrete HTTP "
			     "method (i.e. not " post--template-keyword
			     ") in the heading hierarchy")))
  (unless (oref rs :url)
    (user-error "%s" (concat "No URL specified\nMake sure you specify "
			     "a nonempty URL in the heading hierarchy")))
  (let ((url (oref rs :url)))
    (unless (url-host url)
      (user-error "%s" (concat "URL has no host defined\n"
			       "Make sure you specify a host "
			       "(e.g. \"github.com\") in the heading "
			       "hierarchy")))
    (url-retrieve url #'post--request-spec-callback (list rs) t)
    (message "%s request sent to %s"
	     (oref rs :method)
	     (post--request-spec-url-string rs))))

(defun post--override-url-paths (original other)
  "Override URL path (and query string) ORIGINAL with OTHER.
ORIGINAL and OTHER have the form (PATH . QUERY).  Work using the rules
described in `post--request-spec-override'."
  (let ((original-path (car original))
	(original-query (cdr original))
	(other-path (car other))
	(other-query (cdr other)))
    (concat original-path other-path)))

(defun post--url-port (url)
  "Return port used by URL, or nil if it can be inferred from its schema."
  (let ((port (url-port url))
	(schema (url-type url)))
    (if (and (numberp port)
	     (or (and (= port 80) (string= schema "http"))
		 (and (= port 443) (string= schema "https"))))
	nil
      port)))

(defun post--override-url (original other)
  "Override URL struct ORIGINAL with OTHER.
Do this using the rules described in `post--request-spec-override'."
  ;; If either url is nil, return the other one
  (if (not (and original other))
      (or original other)
    ;; Override ORIGINAL with OTHER
    (let ((schema (or (url-type other) (url-type original)))
	  (user (or (url-user other) (url-user original)))
	  (password (or (url-password other) (url-password original)))
	  (host (or (url-host other) (url-host original)))
	  (port (or (post--url-port other) (post--url-port original)))
	  (path (post--override-url-paths (url-path-and-query original)
					  (url-path-and-query other)))
	  (fragment (or (url-target other) (url-target original)))
	  (attributes (or (url-attributes other) (url-attributes original)))
	  (fullness (or (url-fullness other) (url-fullness original))))
      (url-parse-make-urlobj schema user password host
			     port path fragment
			     attributes fullness))))

(cl-defmethod post--request-spec-override ((rs post--request-spec) other)
  "Override request specification RS with OTHER, return the result.

Each member of request RS is overridden with the one from OTHER in the
following way, to form a new request specification:

method

  Use OTHER's HTTP method if it is non-nil, otherwise use RS's.

url

  A new URL is constructed using a combination of both URLs.  The new
  URL's path is a concatenation of RS's and OTHER's paths.  The new
  URL's query string is a union of both RS's and OTHER's query
  strings, using OTHER's value when both contain the same key.  All
  other components (host, port, user, etc.) of the new URL are taken
  from OTHER if they are non-nil, or from RS otherwise.  If either
  OTHER's or RS's URL is nil, use the other one's without
  modifications.

headers

  Create a union of RS's and OTHER's headers, using OTHER's value when
  both contain the same header.

body

  Use OTHER's body if it is non-nil, otherwise use RS's.

Neither request specification is modified, a new one is returned.
"
  (unless (object-of-class-p other post--request-spec)
    (error "%s" "Argument OTHER must be a `post--request-spec'.")))

(defun post--http-methods-regexp ()
  "Return a regexp that matches a HTTP method.
HTTP methods are defined in `post--http-methods'.
Additionally, allow matching `post--template-keyword'."
  (mapconcat #'identity
	     (append post--http-methods
		     (list post--template-keyword))
	     "\\|"))

(defun post--clean-url (url)
  "Return a correctly encoded URL struct to be used with `url-retrieve'.

Additionally, given a URL like \"http://foo.com?a=b\", return
\"http://foo.com/?a=b\". This is what curl does when the path is empty
and there are query string arguments present.

If a schema is not present, interpret the URL as a path, query string and
fragment component of a URL with no host or schema defined."
  (let* ((url-obj (url-generic-parse-url (url-encode-url url)))
	 (path (url-filename url-obj))
	 (schema (url-type url-obj)))
    (if (not schema)
	;; If no schema defined, interpret everything as path + query
	;; string + fragment
	(progn
	  (setf (url-filename url-obj)
		(concat (url-host url-obj)
			(url-filename url-obj)))
	  (setf (url-host url-obj) nil))
      ;; Schema is present:
      (unless (member schema '("http" "https"))
	(user-error (concat "The URL must specify \"http://\" or "
			    "\"https://\" (got: \"%s\")")
		    schema))
      ;; If path is "" but there are query string arguments, set path
      ;; to "/" (taken from curl)
      (when (string-prefix-p "?" path)
	(setf (url-filename url-obj) (concat "/" path))))
    url-obj))

(defun post--request-spec-from-text (text)
  "Create a `post--request-spec' from a text specification.

The text format for defining requests is:

METHOD [URL | PARTIAL-URL]
[HEADER-NAME1: HEADER-VALUE1]
[HEADER-NAME2: HEADER-VALUE2]...

[BODY]

METHOD must be a method matched by `post--http-methods-regexp'.
URL can be an empty string, or a URL with a \"http\" or \"https\"
schema.
PARTIAL-URL can be an empty string, or the path + query string +
fragment part of a URL.
Headers and BODY can be separated by a blank line, which will be
ignored."
  (let (method url headers body)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Skip initial blank lines and commments
      (while (and (re-search-forward (concat "^\\(\\s-*"
					     post--comment-character
					     ".*\\)?$")
				     (line-end-position) t)
		  (not (eobp)))
	(forward-char))
      ;; Read HTTP method and URL
      (let ((case-fold-search t))
	(when (re-search-forward (concat "^\\s-*\\("
					 (post--http-methods-regexp)
					 "\\)\\s-*\\(.*\\)$")
				 (line-end-position) t)
	  (setq method (upcase (match-string 1))
		url (match-string 2))))
      (if method
	  (when (string= method post--template-keyword)
	    (setq method nil))
	(user-error (concat "Could not read a valid HTTP method, "
			    "valid HTTP methods are: %s\n"
			    "Additionally, you can also specify %s "
			    "(matching is case insensitive)")
		    (mapconcat #'identity post--http-methods ", ")
		    post--template-keyword))
      ;; Skip newline after URL line
      (when (not (eobp)) (forward-char))
      ;; Search for HTTP headers
      ;; Stop as soon as we find a blank line or a non-matching line
      (while (re-search-forward "^\\s-*\\([[:alpha:]-]+\\)\\s-*:\\s-?\\(.*\\)$"
				(line-end-position) t)
	(push (cons (match-string 1) (match-string 2)) headers)
	(when (not (eobp)) (forward-char)))
      (setq headers (reverse headers))
      ;; Allow a blank like to separate headers and body (not required)
      (when (re-search-forward "^$" (line-end-position) t)
	(when (not (eobp)) (forward-char)))
      ;; The rest of the buffer is the request body
      (let ((rest (buffer-substring (point) (point-max))))
	(unless (= 0 (length (string-trim rest)))
	  ;; Only read body if it isn't comprised entirely of whitespace
	  (setq body rest)))
      ;; Return a `post--request-spec'
      (post--request-spec :method method
			  :url (when (< 0 (length url))
				 (post--clean-url url))
			  :headers headers
			  :body body))))

(provide 'post)
;;; post.el ends here
