;;; verb.el --- A new HTTP client for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2019  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>
;; Homepage: https://github.com/federicotdn/verb
;; Keywords: http
;; Package-Requires: ((emacs "26"))

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

;; Main module for verb.

;;; Code:
(require 'eieio)
(require 'subr-x)
(require 'url)

(defgroup verb nil
  "A new HTTP client for Emacs."
  :prefix "verb-"
  :group 'tools)

(defcustom verb-default-response-coding-system 'utf-8
  "Default coding system to use when reading HTP responses."
  :type 'coding-system)

(defcustom verb-content-type-modes-alist
  '(("text/html" . html-mode)
    ("application/json" . js-mode))
  "Major modes to use for different values of the \"Content-Type\"
header."
  :type '(alist :key-type string :value-type function))

(defcustom verb-inhibit-cookies nil
  "If non-nil, do not send or receive cookies when sending requests."
  :type 'boolean)

(defface verb-http-keyword '((t :inherit font-lock-constant-face
				:weight bold))
  "Face for highlighting HTTP methods.")

(defface verb-header '((t :inherit font-lock-constant-face))
  "Face for highlighting HTTP headers.")

(defface verb-comment '((t :inherit font-lock-comment-face))
  "Face for highlighting comments.")

(defconst verb--comment-character "#"
  "Character to use to mark commented lines.")

(defconst verb--outline-character "-"
  "Character to use to create headings.")

(defconst verb--http-methods '("GET" "POST" "DELETE" "PUT"
			       "OPTIONS" "HEAD" "PATCH"
			       "TRACE" "CONNECT")
  "List of valid HTTP methods.")

(defconst verb--template-keyword "TEMPLATE"
  "Keyword to use when defining request templates without defined HTTP
methods.")

(defvar verb--default-request-charset "utf-8"
  "Charset to add to \"Content-Type\" headers in HTTP requests.
This variable is only used when the charset isn't specified in the
header value (\"charset=utf-8\").")

(defvar verb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'verb-execute-request-on-point-other-window)
    map)
  "Keymap for verb mode.")

(defun verb--setup-font-lock-keywords ()
  "Configure font lock keywords for `verb-mode'."
  (font-lock-add-keywords
   nil
   `(;; GET
     (,(concat "^\\(" (verb--http-methods-regexp) "\\)$")
      (1 'verb-http-keyword))
     ;; GET www.example.com
     (,(concat "^\\(" (verb--http-methods-regexp) "\\)\\s-+.+$")
      (1 'verb-http-keyword))
     ;; Content-type: application/json
     ("^\\([[:alpha:]-]+:\\)\\s-.+$"
      (1 'verb-header))
     ;; # This is a comment
     (,(concat "^\\s-*" verb--comment-character ".*$")
      (0 'verb-comment))))
  (setq font-lock-keywords-case-fold-search t)
  (font-lock-ensure)
  ;; `outline-4' is just `font-lock-comment-face', avoid using that
  ;; one in heading fonts.
  (setq-local outline-font-lock-faces
	      [outline-1 outline-2 outline-3 outline-5
			 outline-6 outline-7 outline-8]))

;;;###autoload
(define-derived-mode verb-mode outline-mode "Verb"
  "Major mode for making HTTP requests from Emacs."
  (setq-local outline-regexp (concat "[" verb--outline-character "\^L]+"))
  (setq-local comment-start verb--comment-character)
  (setq-local fill-prefix (concat verb--comment-character " "))
  (verb--setup-font-lock-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verb\\'" . verb-mode))

(define-minor-mode verb-response-body-mode
  "Minor mode to allow for some extra functionality in response
buffers showing the resulting HTTP body."
  :lighter " Verb[Body]"
  :group 'verb)

(define-minor-mode verb-response-headers-mode
  "Minor mode to allow for some extra functionality in response
buffers showing the resulting HTTP headers."
  :lighter " Verb[Headers]"
  :group 'verb)

(defun verb--back-to-heading ()
  "Move to the previous heading.
Or, move to beggining of this line if it's a heading.  If there are no
headings, move to the beggining of buffer.  Return t if a heading was
found."
  (if (ignore-errors (outline-back-to-heading t))
      t
    (goto-char (point-min))
    nil))

(defun verb--section-end ()
  "Skip forward to before the next heading.
If there is no next heading, skip to the end of the buffer."
  (outline-next-preface))

(defun verb--up-heading ()
  "Move to the parent heading, if there is one.
Return t if there was a heading to move towards to and nil otherwise."
  (ignore-errors
    (outline-up-heading 1 t)
    t))

(defun verb--outline-level ()
  "Return the outline level.
Level zero indicates that no headings exist."
  (save-match-data
    (save-excursion
      (if (verb--back-to-heading)
	  (funcall outline-level)
	0))))

(defun verb--heading-has-content-p ()
  "Return non-nil if the heading is followed by text contents."
  (save-match-data
    (save-excursion
      (if (verb--back-to-heading)
	  ;; A heading was found
	  (let ((line (line-number-at-pos)))
	    (verb--section-end)
	    (> (line-number-at-pos) line))
	;; Buffer has no headings
	(< 0 (buffer-size))))))

(defun verb--heading-contents ()
  "Return the heading's text contents.
Return nil if `verb--heading-has-content-p' returns nil."
  (when (verb--heading-has-content-p)
    (let ((start (save-excursion
		   (when (verb--back-to-heading)
		     (end-of-line)
		     (forward-char))
		   (point)))
	  (end (save-excursion (verb--section-end) (point))))
      (buffer-substring-no-properties start end))))

(defun verb--request-spec-from-heading ()
  "Return a `verb--request-spec' generated from the heading's text contents.
Return nil of the heading has no text contents."
  (let ((text (verb--heading-contents)))
    (unless (or (null text)
		(string-empty-p (string-trim text)))
      (catch 'empty
	(verb--request-spec-from-text text)))))

(defun verb-execute-request-on-point-other-window ()
  "Send the request specified by the selected heading's text contents.
Show the results on another window (use
`verb-execute-request-on-point')."
  (interactive)
  (verb-execute-request-on-point 'other-window))

(defun verb-execute-request-on-point (&optional where)
  "Send the request specified by the selected heading's text contents.
The contents of all parent headings are used as well; see
`verb--request-spec-override' to see how this is done.

If WHERE is `other-window', show the results of the request on another
window.  If WHERE has any other value, show the results of the request
in the current window."
  (interactive)
  (let (specs done final-spec)
    (save-excursion
      ;; Go up through the Outline tree taking a request specification
      ;; from each level
      (while (not done)
	(let ((spec (verb--request-spec-from-heading)))
	  (when spec (push spec specs)))
	(setq done (not (verb--up-heading)))))
    (if specs
	(progn
	  (setq final-spec (car specs))
	  (when (< 1 (length specs))
	    (dolist (spec (cdr specs))
	      ;; Override spec 1 with spec 2, and the result with spec
	      ;; 3, then with 4, etc.
	      (setq final-spec (verb--request-spec-override final-spec
							    spec))))
	  (verb--request-spec-execute final-spec where))
      (user-error "%s" (concat "No request specification found\nTry "
			       "writing: get https://<hostname>/<path>")))))

(defun verb--http-method-p (m)
  "Return non-nil if M is a valid HTTP method."
  (member m verb--http-methods))

(defun verb--http-headers-p (h)
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

(defclass verb--request-spec ()
  ((method :initarg :method
	   :type (or null verb--http-method)
	   :documentation "HTTP method.")
   (url :initarg :url
	:type (or null url)
	:documentation "Request URL.")
   (headers :initarg :headers
	    :initform ()
	    :type (or null verb--http-headers)
	    :documentation "HTTP headers.")
   (body :initarg :body
	 :initform nil
	 :type (or null string)
	 :documentation "Request body."))
  "Represents an HTTP request to be made.")

(cl-defmethod verb--request-spec-url-string ((rs verb--request-spec))
  "Return RS's url member as a string if it is non-nil."
  (let ((url (oref rs :url)))
    (when url
      (url-recreate-url url))))

(defun verb--response-header-line-string (status-line elapsed header-count body-size)
  "Return a short description of a response's results.
STATUS-LINE should contain the response's first text line.
ELAPSED should contain the number of seconds the request took, in seconds.
HEADER-COUNT should contain the number of HTTP headers received.
BODY-SIZE should contain the HTTP body size."
  (concat
   status-line
   " | "
   (format "%.4gs" elapsed)
   (unless (zerop header-count)
     (format " | %s header%s" header-count (if (= header-count 1) "" "s")))
   (unless (zerop body-size)
     (format " | body size: %s" body-size))))

(defun verb--major-mode-for-content (content-type)
  "Return the appropiate major mode for handling content of type CONTENT-TYPE."
  (or (cdr (assoc content-type verb-content-type-modes-alist))
      'fundamental-mode))

(defun verb--headers-content-type (headers)
  "Return (TYPE . CHARSET) parsed from the \"Content-Type\" header in HEADERS.
If the charset is not present, return (TYPE . nil).
If the header itself is not present, return (nil . nil)."
  (if-let* ((value (cdr (assoc-string "Content-Type" headers t)))
	    (type-subtype (string-trim (car (split-string value ";")))))
      (cons type-subtype
	    (when (string-match "charset=\\([[:alnum:]-.]+\\)" value)
	      (match-string 1 value)))
    (cons nil nil)))

(defun verb--request-spec-callback (status rs start where)
  "Callback for `verb--request-spec-execute' for request RS.
More response information can be read from STATUS.
START should contain a floating point number indicating the timestamp
at which the request was sent.
WHERE describes where the results should be shown in (see
`verb-execute-request-on-point').

This function sets up the current buffer so that it can be used to
view the HTTP response in a user-friendly way."
  (let ((elapsed (- (time-to-seconds) start))
	status-line headers content-type)
    (widen)
    (goto-char (point-min))
    ;; Skip HTTP/1.X status line
    (setq status-line (buffer-substring-no-properties (point)
						      (line-end-position)))
    (forward-line)
    ;; Skip all HTTP headers
    (while (re-search-forward "^\\s-*\\([[:alpha:]-]+\\)\\s-*:\\s-*\\(.*\\)$"
			      (line-end-position) t)
      (let ((key (match-string 1))
	    (value (match-string 2)))
	;; Save header to alist
	(push (cons key value) headers)
	(when (not (eobp)) (forward-char))))

    ;; Remove headers and blank line from buffer
    ;; All left should be the content
    (beginning-of-line)
    (forward-line)
    (delete-region (point-min) (point))

    ;; Decode body content if possible
    (setq content-type (verb--headers-content-type headers))
    (when enable-multibyte-characters
      (error "Expected a unibyte buffer for HTTP response"))
    (set-buffer-multibyte 'to)

    (set-buffer-file-coding-system 'binary)

    (if-let ((coding-system (mm-charset-to-coding-system
			     (cdr content-type))))
	(progn
	  (decode-coding-region (point-min) (point-max) coding-system)
	  (set-buffer-file-coding-system coding-system))
      (message "Unknown charset: '%s'" (or (cdr content-type)
					   "<none>")))

    ;; Prepare buffer for editing by user
    (buffer-enable-undo)
    (goto-char (point-min))
    (funcall (verb--major-mode-for-content (car content-type)))
    (font-lock-ensure)

    ;; TODO: Move to minor modes
    (setq-local verb--response-headers (nreverse headers))
    (setq-local header-line-format
    		(verb--response-header-line-string status-line
    						   elapsed
    						   (length verb--response-headers)
    						   (buffer-size)))

    (rename-buffer "*HTTP response*" t)

    (if (eq where 'other-window)
	(switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))))

(defun verb--prepare-http-headers (headers)
  "Prepare alist HEADERS of HTTP headers to use them on a request.
Add/modify the following headers if they are not already
present/incomplete:

Content-Type:
  Add \"charset=\" to it if not already present.
Accept-Charset:
  Set it if not already present.

Uses `verb--to-ascii' to ensure all added text is unibyte.
Returns a new alist, does not modify HEADERS."
  (let ((content-type (assoc-string "Content-Type" headers t))
	(accept-charset (assoc-string "Accept-Charset" headers t)))
    (when (and content-type
	       (not (string-match-p "charset=" (cdr content-type))))
      (setcdr content-type (concat (cdr content-type)
				   "; charset="
				   verb--default-request-charset)))
    (unless accept-charset
      (push (cons "Accept-Charset" (url-mime-charset-string)) headers))
    ;; Encode all text to `us-ascii'
    (mapcar (lambda (e)
	      (cons (verb--to-ascii (car e))
		    (verb--to-ascii (cdr e))))
	    headers)))


(defun verb--encode-http-body (body charset)
  "Encode content BODY using CHARSET.
If CHARSET is nil, use `verb--default-request-charset'."
  (when body
    (if-let ((coding-system (mm-charset-to-coding-system
			     (or charset verb--default-request-charset))))
	(encode-coding-string body coding-system)
      (user-error (concat "No coding system found for charset \"%s\"\n"
			  "Make sure you set the \"Content-Type\" header"
			  " correctly (e.g. \"application/json;"
			  " charset=utf-8\")")
		  charset))))

(defun verb--to-ascii (s)
  "Encode string S to `us-ascii'."
  (if (multibyte-string-p s)
      (encode-coding-string s 'us-ascii)
    s))

(cl-defmethod verb--request-spec-execute ((rs verb--request-spec) where)
  "Execute the HTTP request described by RS.
Show the results according to parameter WHERE (see
`verb-execute-request-on-point')."
  (unless (oref rs :method)
    (user-error "%s" (concat "No HTTP method specified\n"
			     "Make sure you specify a concrete HTTP "
			     "method (i.e. not " verb--template-keyword
			     ") in the heading hierarchy")))
  (unless (oref rs :url)
    (user-error "%s" (concat "No URL specified\nMake sure you specify "
			     "a nonempty URL in the heading hierarchy")))

  (let* ((url (oref rs :url))
	 (url-request-method (verb--to-ascii (oref rs :method)))
	 (url-request-extra-headers (verb--prepare-http-headers
				     (oref rs :headers)))
	 (content-type (verb--headers-content-type
			url-request-extra-headers))
	 (url-request-data (verb--encode-http-body (oref rs :body)
						   (cdr content-type))))
    (unless (url-host url)
      (user-error "%s" (concat "URL has no host defined\n"
			       "Make sure you specify a host "
			       "(e.g. \"github.com\") in the heading "
			       "hierarchy")))
    ;; Send the request!
    (url-retrieve url
		  #'verb--request-spec-callback
		  (list rs (time-to-seconds) where)
		  t verb-inhibit-cookies))
  ;; Show user some information
  (message "%s request sent to %s"
	   (oref rs :method)
	   (verb--request-spec-url-string rs)))

(defun verb--override-alist (original other)
  "Override alist ORIGINAL with OTHER.
That is; overwrite (KEY . VALUE) pairs present in ORIGINAL with ones
present in OTHER if KEYs are equal.  Return the results in a new
alist."
  (let ((result (nreverse (copy-alist original)))
	(processed))
    (dolist (key-value other)
      (let ((key (car key-value))
	    (value (cdr key-value)))
	(when (and (assoc key result)
		   (not (member key processed)))
	  ;; key in OTHER is in ORIGINAL, delete all entries using
	  ;; this key in ORIGINAL
	  (setq result (assoc-delete-all key result)))
	(push key-value result)
	;; Remember we deleted this key from ORIGINAL so that we don't
	;; do it again accidentally (this can happen if OTHER contains
	;; multiple values mapped to the same key)
	(push key processed)))
    (nreverse result)))

(defalias 'verb--override-url-queries #'verb--override-alist
  "Override query string alist ORIGINAL with OTHER.
Return the results in a new alist.  Work using the rules described in
`verb--request-spec-override'.")

(defalias 'verb--override-headers #'verb--override-alist
  "Override headers alist ORIGINAL with OTHER.
Return the results in a new alist.  Work using the rules described in
`verb--request-spec-override'.")

(defun verb--url-query-string-to-alist (query)
  "Return an alist of (KEY . VALUE) from query string QUERY.

For example, return:
  \"foo=bar&quux\"
as:
  ((\"foo\" . \"bar\") (\"quux\" . nil))
"
  (when query
    (let ((parts (split-string query "&"))
	  result)
      (dolist (p parts)
	(let* ((key-value (split-string p "="))
	       (key (car key-value))
	       (value (cdr key-value)))
	  (unless (string-empty-p key)
	    (push (cons key
			(when value
			  (mapconcat #'identity value "=")))
		  result))))
      (nreverse result))))

(defun verb--url-query-alist-to-string (query)
  "Return alist query string QUERY as a string."
  (when query
    (mapconcat (lambda (kv)
		 (if (cdr kv)
		     (concat (car kv) "=" (cdr kv))
		   (car kv)))
	       query
	       "&")))

(defun verb--override-url-paths (original other)
  "Override URL path (and query string) ORIGINAL with OTHER.
ORIGINAL and OTHER have the form (PATH . QUERY).  Work using the rules
described in `verb--request-spec-override'."
  (let* ((original-path (car original))
	 (original-query (cdr original))
	 (other-path (car other))
	 (other-query (cdr other))
	 (paths (concat original-path other-path))
	 (queries (verb--url-query-alist-to-string
		   (verb--override-url-queries
		    (verb--url-query-string-to-alist original-query)
		    (verb--url-query-string-to-alist other-query)))))
    ;; If after joining two both paths the result path starts with //,
    ;; remove one of the slashes (this may happen often because we
    ;; sometimes add slashes in `verb--clean-url'.)
    (concat (if (string-prefix-p "//" paths)
		(substring paths 1 nil)
	      paths)
	    (unless (string-empty-p (or queries ""))
	      ;; If query string is present and path is empty,
	      ;; set / as the path (see `verb--clean-url')
	      (concat (when (string-empty-p paths) "/")
		      "?"
		      queries)))))

(defun verb--url-port (url)
  "Return port used by an HTTP URL.
Return nil if the port can be inferred from the URL's schema."
  (let ((port (url-port url))
	(schema (url-type url)))
    (if (and (numberp port)
	     (or (and (= port 80) (string= schema "http"))
		 (and (= port 443) (string= schema "https"))))
	nil
      port)))

(defun verb--override-url (original other)
  "Override URL struct ORIGINAL with OTHER.
Do this using the rules described in `verb--request-spec-override'."
  ;; If either url is nil, return the other one
  (if (not (and original other))
      (or original other)
    ;; Override ORIGINAL with OTHER
    (let ((schema (or (url-type other) (url-type original)))
	  (user (or (url-user other) (url-user original)))
	  (password (or (url-password other) (url-password original)))
	  (host (or (url-host other) (url-host original)))
	  (port (or (verb--url-port other) (verb--url-port original)))
	  (path (verb--override-url-paths (url-path-and-query original)
					  (url-path-and-query other)))
	  (fragment (or (url-target other) (url-target original)))
	  (attributes (or (url-attributes other) (url-attributes original)))
	  (fullness (or (url-fullness other) (url-fullness original))))
      (url-parse-make-urlobj schema user password host
			     port path fragment
			     attributes fullness))))

(cl-defmethod verb--request-spec-override ((original verb--request-spec) other)
  "Override request specification ORIGINAL with OTHER, return the result.

Each member of request ORIGINAL is overridden with the one from OTHER
in the following way, to form a new request specification:

method

  Use OTHER's HTTP method if it is non-nil, otherwise use ORIGINAL's.

url

  A new URL is constructed using a combination of both URLs.  The new
  URL's path is a concatenation of ORIGINAL's and OTHER's paths.  The
  new URL's query string is a union of both ORIGINAL's and OTHER's
  query strings, using OTHER's value when both contain the same key.
  All other components (host, port, user, etc.) of the new URL are
  taken from OTHER if they are non-nil, or from ORIGINAL otherwise.
  If either OTHER's or ORIGINAL's URL is nil, use the other one's
  without modifications.

headers

  Create a union of ORIGINAL's and OTHER's headers, using OTHER's
  value when both contain the same header.

body

  Use OTHER's body if it is non-nil, otherwise use ORIGINAL's.

Neither request specification is modified, a new one is returned.
"
  (unless (object-of-class-p other verb--request-spec)
    (error "%s" "Argument OTHER must be a `verb--request-spec'."))
  (verb--request-spec :method (or (oref other :method)
				  (oref original :method))
		      :url (verb--override-url (oref original :url)
					       (oref other :url))
		      :headers (verb--override-headers (oref original :headers)
						       (oref other :headers))
		      :body (or (oref other :body) (oref original :body))))

(defun verb--http-methods-regexp ()
  "Return a regexp that matches a HTTP method.
HTTP methods are defined in `verb--http-methods'.
Additionally, allow matching `verb--template-keyword'."
  (mapconcat #'identity
	     (append verb--http-methods
		     (list verb--template-keyword))
	     "\\|"))

(defun verb--clean-url (url)
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
      ;; Note that `path' here contains path and query string
      (when (string-prefix-p "?" path)
	(setf (url-filename url-obj) (concat "/" path))))
    url-obj))

(defun verb--request-spec-from-text (text)
  "Create a `verb--request-spec' from a text specification.

The text format for defining requests is:

[COMMENTS]
[METHOD] [URL | PARTIAL-URL]
[HEADERS]

[BODY]

COMMENTS must be lines starting with `verb--comment-character'.
METHOD must be a method matched by `verb--http-methods-regexp'.
URL can be an empty string, or a URL with a \"http\" or \"https\"
schema.
PARTIAL-URL can be an empty string, or the path + query string +
fragment part of a URL.
HEADERS and BODY can be separated by a blank line, which will be
ignored.  Each line of HEADERS must be in the form of KEY: VALUE.
If the text specification consists exclusively of comments or is the
empty string, `throw' symbol `empty' with nil as associated value."
  (let (method url headers body)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Skip initial blank lines and commments
      (while (and (re-search-forward (concat "^\\(\\s-*"
					     verb--comment-character
					     ".*\\)?$")
				     (line-end-position) t)
		  (not (eobp)))
	(forward-char))
      ;; Check if the entire specification was just comments or empty
      (when (string-empty-p (string-trim (buffer-substring (point)
							   (point-max))))
	;; Throw `empty' if so
	(throw 'empty nil))
      ;; Read HTTP method and URL
      (let ((case-fold-search t))
	(when (re-search-forward (concat "^\\s-*\\("
					 (verb--http-methods-regexp)
					 "\\)\\s-*\\(.*\\)$")
				 (line-end-position) t)
	  (setq method (upcase (match-string 1))
		url (match-string 2))))
      (if method
	  (when (string= method verb--template-keyword)
	    (setq method nil))
	(user-error (concat "Could not read a valid HTTP method, "
			    "valid HTTP methods are: %s\n"
			    "Additionally, you can also specify %s "
			    "(matching is case insensitive)")
		    (mapconcat #'identity verb--http-methods ", ")
		    verb--template-keyword))
      ;; Skip newline after URL line
      (when (not (eobp)) (forward-char))
      ;; Search for HTTP headers
      ;; Stop as soon as we find a blank line or a non-matching line
      (while (re-search-forward "^\\s-*\\([[:alpha:]-]+\\)\\s-*:\\s-?\\(.*\\)$"
				(line-end-position) t)
	(push (cons (match-string 1) (match-string 2)) headers)
	(when (not (eobp)) (forward-char)))
      (setq headers (nreverse headers))
      ;; Allow a blank like to separate headers and body (not required)
      (when (re-search-forward "^$" (line-end-position) t)
	(when (not (eobp)) (forward-char)))
      ;; The rest of the buffer is the request body
      (let ((rest (buffer-substring (point) (point-max))))
	(unless (string-empty-p (string-trim rest))
	  ;; Only read body if it isn't comprised entirely of whitespace
	  (setq body rest)))
      ;; Return a `verb--request-spec'
      (verb--request-spec :method method
			  :url (unless (string-empty-p url)
				 (verb--clean-url url))
			  :headers headers
			  :body body))))

(provide 'verb)
;;; verb.el ends here
