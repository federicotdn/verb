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
(require 'outline)
(require 'eieio)
(require 'subr-x)
(require 'url)
(require 'url-queue)
(require 'mm-util)
(require 'json)
(require 'js)

(defgroup verb nil
  "A new HTTP client for Emacs."
  :prefix "verb-"
  :group 'tools)

(defcustom verb-default-response-charset "utf-8"
  "Default charset to use when reading HTP responses.
This variable is only used when the charset isn't specified in the
\"Content-Type\" header value (\"charset=utf-8\")."
  :type 'string)

(defcustom verb-default-request-charset "utf-8"
  "Charset to add to \"Content-Type\" headers in HTTP requests.
This variable is only used when the charset isn't specified in the
header value (\"charset=utf-8\")."
  :type 'string)

(defcustom verb-text-content-type-handlers
  '(("text/html" . html-mode)
    ("application/xml" . xml-mode)
    ("application/xhtml+xml" . xml-mode)
    ("application/json" . verb--handler-json)
    ("application/javascript" . js-mode)
    ("application/css" . css-mode)
    ("text/plain" . text-mode))
  "List of text content type handlers.
Handlers are functions to be called without any arguments.  Text
handlers, specifically, are called after the text contents of the
response have been decoded into a multibyte buffer (with that buffer
as the current buffer).
Note: if a content type is listed in
`verb-binary-content-type-handlers', then its binary handler will be
used instead of any handler specified here.  This behaviour can't be
disabled."
  :type '(alist :key-type string :value-type function))

(defcustom verb-binary-content-type-handlers
  '(("application/pdf" . doc-view-mode)
    ("image/png" . image-mode)
    ("image/svg+xml" . image-mode)
    ("image/x-windows-bmp" . image-mode)
    ("image/gif" . image-mode)
    ("image/jpeg" . image-mode))
  "List of binary content type handlers.
Handlers are functions to be called without any arguments.  Binary
handlers, specifically, are called after the binary contents of the
response have been inserted into a unibyte buffer (with that buffer as
the current buffer).
See also: `verb-text-content-type-handlers'."
  :type '(alist :key-type string :value-type function))

(defcustom verb-inhibit-cookies nil
  "If non-nil, do not send or receive cookies when sending requests."
  :type 'boolean)

(defcustom verb-using-proxy nil
  "Either nil or the fully qualified proxy URL in use.
See also: `url-using-proxy'."
  :type '(choice (string :tag "Proxy URL")
		 (const :tag "No proxy" nil)))

(defcustom verb-max-redirections url-max-redirections
  "Max number of redirection requests to honor in an HTTP connection.
See also: `url-max-redirections'."
  :type 'integer)

(defcustom verb-show-headers-buffer nil
  "Automatically show headers buffer after receiving an HTTP response.

Value nil means never show the headers buffer.
Value `when-empty' means automatically show the headers buffer only
when the response's body size is 0.
Any other value means always show the headers buffer."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "When empty" when-empty)
		 (const :tag "Always" t)))

(defcustom verb-show-timeout-warning 10.0
  "Reasonable max duration (s) for an HTTP request.
Indicates the nunmber of seconds to wait after an HTTP request is sent
to warn the user about a possible network timeout.  When set to nil,
don't show any warnings."
  :type '(choice (float :tag "Time in seconds")
		 (const :tag "Off" nil)))

(defcustom verb-code-tag-delimiters '("{{" . "}}")
  "Lisp code tag delimeters for HTTP request specifications.
The delimiters (left and right) are used to specify, evaluate and then
substitute Lisp code tags inside HTTP request specifications.
If different parts of your HTTP request specifications need to include
literal values identical to one or both of the delimiters, it is
recommended you change them to something else through this setting."
  :type '(cons string string))

(defcustom verb-url-retrieve-function #'url-retrieve
  "Function to use in order to send HTTP requests.
For more information on `url-retrieve' and `url-queue-retrieve', see
info node `(url)Retrieving URLs'."
  :type '(choice (function-item
		  :tag "url-retrieve from url.el"
		  url-retrieve)
		 (function-item
		  :tag "url-queue-retrieve from url-queue.el"
		  url-queue-retrieve)))

(defcustom verb-json-max-pretty-print-size (* 1 1024 1024)
  "Max JSON file size (bytes) to automatically prettify when received.
If nil, never prettify JSON files automatically."
  :type '(choice (integer :tag "Max bytes")
		 (const :tag "Off" nil)))

(defface verb-http-keyword '((t :inherit font-lock-constant-face
				:weight bold))
  "Face for highlighting HTTP methods.")

(defface verb-header '((t :inherit font-lock-constant-face))
  "Face for highlighting HTTP headers.")

(defface verb-comment '((t :inherit font-lock-comment-face))
  "Face for highlighting comments.")

(defface verb-code-tag '((t :inherit italic))
  "Face for highlighting Lisp code tags.")

(defconst verb--comment-character "#"
  "Character to use to mark commented lines.")

(defconst verb--outline-character "-"
  "Character to use to create headings.")

(defconst verb--http-methods '("GET" "POST" "DELETE" "PUT"
			       "OPTIONS" "HEAD" "PATCH"
			       "TRACE" "CONNECT")
  "List of valid HTTP methods.")

(defconst verb--template-keyword "TEMPLATE"
  "Keyword to use when defining request templates.
Request templates are defined without HTTP methods, paths or hosts.")

(defvar-local verb--http-response nil
  "HTTP response for this response buffer (`verb--response' object).
The body contents of the response are in the buffer itself.")
(put 'verb--http-response 'permanent-local t)

(defvar-local verb--response-headers-buffer nil
  "Buffer currently showing the HTTP response's headers.")

(defvar verb--debug-enable nil
  "If non-nil, enable logging debug messages with `verb--debug'.")

(defvar verb-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-r") #'verb-execute-request-on-point-other-window)
    (define-key map (kbd "C-f") #'verb-execute-request-on-point)
    (define-key map (kbd "C-p") #'verb-preview-request-on-point)
    map)
  "Prefix map for `verb-mode'.")

(defvar verb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") verb-mode-prefix-map)
    (define-key map (kbd "TAB") #'verb-cycle)
    map)
  "Keymap for `verb-mode'.")

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
     ("^\\([[:alnum:]-]+:\\)\\s-.+$"
      (1 'verb-header))
     ;; # This is a comment
     (,(concat "^\\s-*" verb--comment-character ".*$")
      (0 'verb-comment))
     ;; Lisp code tags
     (,(concat "^.*?\\("
	       (car verb-code-tag-delimiters)
	       ".*?"
	       (cdr verb-code-tag-delimiters)
	       "\\).*$")
      (1 'verb-code-tag))))
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
  (setq imenu-generic-expression
	(list (list nil (concat "^" outline-regexp "\\s-+\\(.+\\)$") 1)))
  (verb--setup-font-lock-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verb\\'" . verb-mode))

(defvar verb-response-headers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'verb-kill-buffer-and-window)
    map)
  "Keymap for `verb-response-headers-mode'.")

(define-derived-mode verb-response-headers-mode special-mode "Verb[Headers]"
  "Major mode for displaying an HTTP response's headers."
  (font-lock-add-keywords
   nil '(("^\\([[:alnum:]-]+:\\)\\s-.+$" (1 'verb-header)))))

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

(defclass verb--response ()
  ((request :initarg :request
	    :type verb--request-spec
	    :documentation "Corresponding request.")
   (headers :initarg :headers
	    :type (or null verb--http-headers)
	    :documentation "Response headers.")
   (status :initarg :status
	   :type (or null string)
	   :documentation "Response's first line.")
   (duration :initarg :duration
	     :type float
	     :documentation
	     "Time taken for response to be received, in seconds.")
   (body-bytes :initarg :body-bytes
	       :initform 0
	       :type integer
	       :documentation
	       "Number of bytes in response buffer, without headers."))
  "Represents an HTTP response to a request.")

(define-minor-mode verb-response-body-mode
  "Minor mode for displaying an HTTP response's body."
  :lighter " Verb[Body]"
  :group 'verb
  :keymap `((,(kbd "C-c C-r C-r") . verb-toggle-show-headers)
	    (,(kbd "C-c C-r C-k") . verb-kill-response-buffer-and-window))
  (if verb-response-body-mode
      (progn
	(setq header-line-format
	      (verb--response-header-line-string verb--http-response))
	(when verb-show-headers-buffer
	  (if (eq verb-show-headers-buffer 'when-empty)
	      (when (zerop (oref verb--http-response body-bytes))
		(verb-toggle-show-headers))
	    (verb-toggle-show-headers))))
    (setq header-line-format nil)))

(defun verb--nonempty-string (s)
  "Return S. If S is the empty string, return nil."
  (if (string-empty-p s)
      nil
    s))

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
  (let ((p (point)))
    (ignore-errors
      (outline-up-heading 1 t)
      (not (= p (point))))))

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
  "Return a request spec generated from the heading's text contents.
Return nil of the heading has no text contents."
  (let ((text (verb--heading-contents)))
    (unless (or (null text)
		(string-empty-p (string-trim text)))
      (condition-case nil
	  (verb--request-spec-from-text text)
	(verb--empty-spec nil)))))

(defun verb--request-spec-from-hierarchy ()
  "Return a request spec generated from the headings hierarchy.
To do this, use `verb--request-spec-from-heading' for the current
heading, for that heading's parent, and so on until the root of the
hierarchy is reached.  Once all the request specs have been collected,
override them in inverse order according to the rules described in
`verb--request-spec-override'."
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
	  final-spec)
      (user-error "%s" (concat "No request specification found\nTry "
			       "writing: get https://<hostname>/<path>")))))

(defun verb--split-window ()
  "Split selected window by its longest side."
  (split-window nil nil (if (< (window-pixel-height)
			       (window-pixel-width))
			    'right
			  'below)))

(defun verb-kill-response-buffer-and-window ()
  "Delete response window and kill its buffer.
If the response buffer has a corresponding headers buffer, kill it and
delete any window displaying it."
  (interactive)
  (when verb--response-headers-buffer
    (when-let ((w (get-buffer-window verb--response-headers-buffer)))
      (ignore-errors
	(delete-window w)))
    (when (buffer-live-p verb--response-headers-buffer)
      (kill-buffer verb--response-headers-buffer)))
  (kill-current-buffer)
  (ignore-errors
    (delete-window)))

(defun verb-kill-buffer-and-window ()
  "Delete selected window and kill its current buffer.
Delete the window only if it isn't the only window in the frame."
  (interactive)
  (kill-current-buffer)
  (ignore-errors
    (delete-window)))

(defun verb--heading-invisible-p ()
  "Return non-nil if the contents of the current heading are invisible."
  (save-excursion
    (end-of-line)
    (outline-invisible-p)))

(defun verb-cycle ()
  "Cycle the current heading's visibility, like in Org mode.
If point is not on a heading, emulate a TAB key press."
  (interactive)
  (if (outline-on-heading-p)
      (let ((level (save-excursion
		     (beginning-of-line)
		     (outline-level)))
	    (next-invisible (save-excursion
			      (and (outline-next-heading)
				   (verb--heading-invisible-p))))
	    (next-level (save-excursion
			  (and (outline-next-heading)
			       (outline-level)))))
	(cond
	 ((verb--heading-invisible-p)
	  (outline-toggle-children))
	 ((and next-level (> next-level level) next-invisible)
	  (outline-show-subtree))
	 (t
	  (outline-hide-subtree))))
    (call-interactively (global-key-binding "\t"))))

(defun verb--insert-header-contents (headers)
  "Insert the contents of HTTP HEADERS into the current buffer."
  (let ((inhibit-read-only t))
    (dolist (key-value headers)
      (let ((key (car key-value))
	    (value (cdr key-value)))
	(insert key ": " value "\n")))
    (unless (zerop (buffer-size))
      (backward-delete-char 1))))

(defun verb-toggle-show-headers ()
  "Show or hide the HTTP response's headers on a separate buffer."
  (interactive)
  (unless verb-response-body-mode
    (user-error "%s" "This buffer is not showing an HTTP response"))

  (when (and verb--response-headers-buffer
	     (not (get-buffer-window verb--response-headers-buffer)))
    (when (buffer-live-p verb--response-headers-buffer)
      (kill-buffer verb--response-headers-buffer))
    (setq verb--response-headers-buffer nil))

  (if verb--response-headers-buffer
      (progn
	(ignore-errors
	  (delete-window (get-buffer-window verb--response-headers-buffer)))
	(kill-buffer verb--response-headers-buffer)
	(setq verb--response-headers-buffer nil))
    (setq verb--response-headers-buffer
	  (generate-new-buffer "*HTTP Headers*"))
    (let ((headers (oref verb--http-response headers)))
      (with-selected-window (verb--split-window)
	(switch-to-buffer verb--response-headers-buffer)
	(verb-response-headers-mode)
	(setq header-line-format (format "HTTP Response Headers | count: %s"
					 (length headers)))
	(verb--insert-header-contents headers)
	(fit-window-to-buffer)))))

(defun verb-execute-request-on-point-other-window ()
  "Send the request specified by the selected heading's text contents.
Show the results on another window (use
`verb-execute-request-on-point')."
  (interactive)
  (verb-execute-request-on-point 'other-window))

(defun verb-execute-request-on-point (&optional where)
  "Send the request specified by the selected heading's text contents.
The contents of all parent headings are used as well; see
`verb--request-spec-from-hierarchy' to see how this is done.

If WHERE is `other-window', show the results of the request on another
window.  If WHERE is `preview', only show information about the
request, but don't actually send it.  If WHERE has any other value,
show the results of the request in the current window."
  (interactive)
  (verb--request-spec-execute (verb--request-spec-from-hierarchy)
			      where))

(defun verb-preview-request-on-point ()
  "Preview the request that would be sent using `verb-execute-request-on-point'."
  (interactive)
  (verb-execute-request-on-point 'preview))

(cl-defmethod verb--response-header-line-string ((response verb--response))
  "Return a short description of an HTTP RESPONSE's properties."
  (let ((status-line (oref response status))
	(elapsed (oref response duration))
	(headers (oref response headers))
	(bytes (oref response body-bytes)))
    (concat
     (or status-line "No Response")
     " | "
     (format "%.4gs" elapsed)
     (let ((content-type (or (car (verb--headers-content-type headers))
			     "?")))
       (format " | %s" content-type))
     (let* ((content-length (cdr (assoc-string "Content-Length"
					       headers t)))
	    (value (if content-length
		       (string-to-number content-length)
		     bytes)))
       (format " | %s byte%s"
	       value
	       (if (= value 1) "" "s"))))))

(cl-defmethod verb--request-spec-url-string ((rs verb--request-spec))
  "Return RS's url member as a string if it is non-nil."
  (let ((url (oref rs url)))
    (when url
      (url-recreate-url url))))

(defun verb--handler-json ()
  "Handler for \"application/json\" content type."
  (js-mode)
  (when (< (oref verb--http-response body-bytes)
	   (or verb-json-max-pretty-print-size 0))
    (unwind-protect
	(let ((json-pretty-print-max-secs 0))
	  (buffer-disable-undo)
	  (json-pretty-print-buffer)
	  ;; "Use" `json-pretty-print-max-secs' here to avoid byte-compiler warning in
	  ;; Emacs 26
	  json-pretty-print-max-secs)
      (buffer-enable-undo))
    (goto-char (point-min))))

(defun verb--headers-content-type (headers)
  "Return the value of the \"Content-Type\" header in HEADERS.
The value returned has the form (TYPE . CHARSET).  If the charset is
not present, return (TYPE . nil).  If the header itself is not
present, return (nil . nil)."
  (if-let* ((value (cdr (assoc-string "Content-Type" headers t)))
	    (type-subtype (string-trim (car (split-string value ";")))))
      (cons type-subtype
	    (when (string-match "charset=\\([[:alnum:]-.]+\\)" value)
	      (match-string 1 value)))
    (cons nil nil)))

(defun verb--debug (&rest args)
  "Log ARGS in the debugging buffer using `format'."
  (when verb--debug-enable
    (with-current-buffer (get-buffer-create "*verb-debug*")
      (insert (apply #'format args) "\n"))))

(defun verb--get-handler (content-type handlers-list)
  "Get a handler from HANDLERS-LIST for a specific CONTENT-TYPE.
CONTENT-TYPE must be the value returned by `verb--headers-content-type'."
  (cdr (assoc-string (car content-type) handlers-list t)))

(defun verb--request-spec-callback (status rs response-buf start timeout-timer where)
  "Callback for `verb--request-spec-execute' for request RS.
More response information can be read from STATUS.
RESPONSE-BUF should point to a buffer where the response should be
copied to, which the user can then use or edit freely.
START should contain a floating point number indicating the timestamp
at which the request was sent.
TIMEOUT-TIMER should contain a timer set to call `verb--timeout-warn',
or nil.
WHERE describes where the results should be shown in (see
`verb-execute-request-on-point').

This function sets up the current buffer so that it can be used to
view the HTTP response in a user-friendly way."
  (when timeout-timer
    (cancel-timer timeout-timer))

  ;; Handle errors first
  (when-let ((http-error (plist-get status :error))
	     (error-info (cdr http-error))
	     (url (oref rs url)))
    ;; If there's an HTTP error code (404, 405, etc.) in the error
    ;; information, continue as normal
    (unless (numberp (and (eq (car error-info) 'http)
			  (cadr error-info)))
      (kill-current-buffer)
      (kill-buffer response-buf)
      (verb--debug "Connection error (from status plist): %s" http-error)
      (user-error "Failed to connect to host %s (port: %s)"
		  (url-host url) (url-port url))))

  ;; No errors, continue to read response
  (let ((elapsed (- (time-to-seconds) start))
	(original-buffer (current-buffer))
	status-line headers content-type charset coding-system bytes
	binary-handler text-handler)

    (widen)
    (goto-char (point-min))
    ;; Skip HTTP/1.X status line
    (setq status-line (verb--nonempty-string
		       (buffer-substring-no-properties (point)
						       (line-end-position))))

    (forward-line)
    ;; Skip all HTTP headers
    (while (re-search-forward "^\\s-*\\([[:alnum:]-]+\\)\\s-*:\\s-*\\(.*\\)$"
			      (line-end-position) t)
      (let ((key (match-string 1))
	    (value (match-string 2)))
	;; Save header to alist
	(push (cons key value) headers)
	(when (not (eobp)) (forward-char))))

    ;; Read Content-Type and charset
    (setq content-type (verb--headers-content-type headers))
    (verb--debug "Content-Type: %s" content-type)

    ;; Try to get a buffer handler function for this content type
    ;; Binary handlers have priority over text handlers
    (setq binary-handler (verb--get-handler content-type
					    verb-binary-content-type-handlers))

    (unless binary-handler
      (setq text-handler (or (verb--get-handler content-type
						verb-text-content-type-handlers)
			     #'fundamental-mode))
      (setq charset (or (cdr content-type) verb-default-response-charset)))

    ;; Remove headers and blank line from buffer
    ;; All left should be the content
    (beginning-of-line)
    (forward-line)
    (delete-region (point-min) (point))

    ;; Record body size in bytes
    (setq bytes (buffer-size))

    ;; Current buffer should be unibyte
    (when enable-multibyte-characters
      (error "Expected a unibyte buffer for HTTP response"))

    ;; Store details of request and response
    ;; `verb--http-response' is a permanent buffer local variable
    (with-current-buffer response-buf
      (setq verb--http-response
	    (verb--response :headers (nreverse headers)
			    :request rs
			    :status status-line
			    :duration elapsed
			    :body-bytes bytes)))

    (if binary-handler
	;; Response content is a binary format:

	(with-current-buffer response-buf
	  (fundamental-mode)
	  (set-buffer-multibyte nil)
	  (buffer-disable-undo)
	  ;; Copy bytes into RESPONSE-BUF
	  (insert-buffer-substring original-buffer)
	  (goto-char (point-min))
	  (funcall binary-handler))

      ;; Response content is text:

      ;; Convert buffer to multibyte, contents are still raw bytes from
      ;; the response
      (set-buffer-multibyte 'to)
      (set-buffer-file-coding-system 'binary)

      ;; Choose corresponding coding system for charset
      (setq coding-system (or (mm-charset-to-coding-system charset)
			      'utf-8))

      ;; Decode contents into RESPONSE-BUF
      (decode-coding-region (point-min) (point-max)
			    coding-system
			    response-buf))

    ;; Kill original response buffer
    (kill-buffer original-buffer)

    (with-current-buffer response-buf
      (when text-handler
	(set-buffer-file-coding-system coding-system)

	;; Prepare buffer for editing by user
	(goto-char (point-min))
	(funcall text-handler))

      (if (eq where 'other-window)
	  (switch-to-buffer-other-window (current-buffer))
	(switch-to-buffer (current-buffer)))

      (verb-response-body-mode))))

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
				   verb-default-request-charset)))
    (unless accept-charset
      (push (cons "Accept-Charset" (url-mime-charset-string)) headers))
    ;; Encode all text to `us-ascii'
    (mapcar (lambda (e)
	      (cons (verb--to-ascii (car e))
		    (verb--to-ascii (cdr e))))
	    headers)))


(defun verb--encode-http-body (body charset)
  "Encode content BODY using CHARSET.
If CHARSET is nil, use `verb-default-request-charset'."
  (when body
    (if-let ((coding-system (mm-charset-to-coding-system
			     (or charset verb-default-request-charset))))
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
`verb-execute-request-on-point'). Return the buffer the response will
be loaded into."
  (unless (oref rs method)
    (user-error "%s" (concat "No HTTP method specified\n"
			     "Make sure you specify a concrete HTTP "
			     "method (i.e. not " verb--template-keyword
			     ") in the heading hierarchy")))
  (unless (oref rs url)
    (user-error "%s" (concat "No URL specified\nMake sure you specify "
			     "a nonempty URL in the heading hierarchy")))

  (let* ((url (oref rs url))
	 (url-request-method (verb--to-ascii (oref rs method)))
	 (url-request-extra-headers (verb--prepare-http-headers
				     (oref rs headers)))
	 (url-using-proxy verb-using-proxy)
	 (url-max-redirections verb-max-redirections)
	 (content-type (verb--headers-content-type
			url-request-extra-headers))
	 (url-request-data (verb--encode-http-body (oref rs body)
						   (cdr content-type)))
	 (not-preview (not (eq where 'preview)))
	 (response-buf (and not-preview
			    (generate-new-buffer "*HTTP Response*")))
	 timeout-timer)
    (unless (url-host url)
      (user-error "%s" (concat "URL has no host defined\n"
			       "Make sure you specify a host "
			       "(e.g. \"github.com\") in the heading "
			       "hierarchy")))

    (when not-preview
      ;; Start the timeout warning timer
      (when verb-show-timeout-warning
	(setq timeout-timer (run-with-timer verb-show-timeout-warning nil
					    #'verb--timeout-warn
					    response-buf rs)))

      ;; Send the request!
      (funcall verb-url-retrieve-function
	       url
	       #'verb--request-spec-callback
	       (list rs
		     response-buf
		     (time-to-seconds)
		     timeout-timer
		     where)
	       t verb-inhibit-cookies))

    ;; Show user some information
    (message "%s request %ssent to %s%s"
	     (oref rs method)
	     (if not-preview "" "would be ")
	     (verb--request-spec-url-string rs)
	     (if not-preview ""
	       (format (concat "\nUser headers count: %s"
			       "\nBody present: %s")
		       (length (oref rs headers))
		       (if (oref rs body) "yes" "no"))))

    ;;Return the response buffer
    response-buf))

(defun verb--timeout-warn (buffer rs)
  "Warn the user about a possible network timeout for request RS.
This function should be run `verb-show-timeout-warning' seconds after
an HTTP request has been sent.  Show the warning only when response
buffer BUFFER is live."
  (when (buffer-live-p buffer)
    (message "Request to %s is taking longer than expected"
	     (verb--request-spec-url-string rs))))

(defun verb--override-alist (original other)
  "Override alist ORIGINAL with OTHER.
That is; overwrite (KEY . VALUE) pairs present in ORIGINAL with ones
present in OTHER if KEYs are equal.  Return the results in a new
alist."
  (let ((result (nreverse (copy-alist original)))
	(processed))
    (dolist (key-value other)
      (let ((key (car key-value)))
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
  ((\"foo\" . \"bar\") (\"quux\" . nil))"
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
  "Override request spec ORIGINAL with OTHER, return the result.

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

Neither request specification is modified, a new one is returned."
  (unless (object-of-class-p other 'verb--request-spec)
    (error "%s" "Argument OTHER must be a `verb--request-spec'."))
  (verb--request-spec :method (or (oref other method)
				  (oref original method))
		      :url (verb--override-url (oref original url)
					       (oref other url))
		      :headers (verb--override-headers (oref original headers)
						       (oref other headers))
		      :body (or (oref other body) (oref original body))))

(defun verb--http-methods-regexp ()
  "Return a regexp to match an HTTP method.
HTTP methods are defined in `verb--http-methods'.
Additionally, allow matching `verb--template-keyword'."
  (mapconcat #'identity
	     (append verb--http-methods
		     (list verb--template-keyword))
	     "\\|"))

(defun verb--eval-string (s)
  "Eval S as Lisp code, return the result as a string.
As a special case, if S is the empty string, return the empty string."
  (if (string-empty-p s)
      ""
    (save-mark-and-excursion
      (save-match-data
	(let ((result
	       (eval (car (read-from-string (format "(progn %s)" s))) t)))
	  (format "%s" result))))))

(defun verb--eval-lisp-code-in (s)
  "Evalue and replace Lisp code within code tags in S.
Code tags are delimited with `verb-code-tag-delimiters'."
  (when s
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (re-search-forward (concat (car verb-code-tag-delimiters)
					"\\(.*?\\)"
					(cdr verb-code-tag-delimiters))
				nil t)
	(replace-match (verb--eval-string (match-string 1))))
      (buffer-string))))

(defun verb--clean-url (url)
  "Return a correctly encoded URL struct to use with `url-retrieve'.

Additionally, given a URL like \"http://foo.com?a=b\", return
\"http://foo.com/?a=b\". This is what curl does when the path is empty
and there are query string arguments present.

If a schema is not present, interpret the URL as a path, query string
and fragment component of a URL with no host or schema defined."
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

(define-error 'verb--empty-spec
  "Request specification has no contents.")

(defun verb--request-spec-from-text (text)
  "Create a request spec from a text specification, TEXT.

The text format for defining requests is:

[COMMENTS]
METHOD [URL | PARTIAL-URL]
[HEADERS]

[BODY]

COMMENTS must be lines starting with `verb--comment-character'.
Adding comments is optional.
METHOD must be a method matched by `verb--http-methods-regexp' (that
is, an HTTP method or the value of `verb--template-keyword').
URL can be the empty string, or a URL with a \"http\" or \"https\"
schema.
PARTIAL-URL can be the empty string, or the path + query string +
fragment part of a URL.
HEADERS and BODY can be separated by a blank line, which will be
ignored.  Each line of HEADERS must be in the form of KEY: VALUE.

As a special case, if the text specification consists exclusively of
comments or is the empty string, signal `verb--empty-spec'."
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
	;; Signal `verb--empty-spec' if so
	(signal 'verb--empty-spec nil))
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
      (while (re-search-forward "^\\s-*\\([[:alnum:]-]+\\)\\s-*:\\s-?\\(.*\\)$"
				(line-end-position) t)
	(push (cons (verb--eval-lisp-code-in (match-string 1))
		    (verb--eval-lisp-code-in (match-string 2)))
	      headers)
	(when (not (eobp)) (forward-char)))
      (setq headers (nreverse headers))
      ;; Allow a blank like to separate headers and body (not
      ;; required)
      (when (re-search-forward "^$" (line-end-position) t)
	(when (not (eobp)) (forward-char)))
      ;; The rest of the buffer is the request body
      (let ((rest (buffer-substring (point) (point-max))))
	(unless (string-empty-p (string-trim rest))
	  ;; Only read body if it isn't comprised entirely of
	  ;; whitespace, but if it's not and has leading/trailing
	  ;; whitespace, include it
	  (setq body rest)))
      ;; Return a `verb--request-spec'
      (verb--request-spec :method method
			  :url (unless (string-empty-p url)
				 (verb--clean-url
				  (verb--eval-lisp-code-in url)))
			  :headers headers
			  :body (verb--eval-lisp-code-in body)))))

(provide 'verb)
;;; verb.el ends here
