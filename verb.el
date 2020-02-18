;;; verb.el --- Organize and send HTTP requests  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>
;; Homepage: https://github.com/federicotdn/verb
;; Keywords: tools
;; Package-Version: 2.7.2
;; Package-Requires: ((emacs "26"))

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

;; Verb is a package that allows you to organize and send HTTP
;; requests from Emacs.  See the project's README.md file for more
;; details.

;;; Code:
(require 'org)
(require 'ob)
(require 'eieio)
(require 'subr-x)
(require 'url)
(require 'url-queue)
(require 'mm-util)
(require 'json)
(require 'js)
(require 'seq)

(defgroup verb nil
  "An HTTP client for Emacs that extends Org mode."
  :prefix "verb-"
  :group 'tools)

(defcustom verb-default-response-charset "utf-8"
  "Default charset to use when reading HTTP responses.
This variable is only used when the charset isn't specified in the
\"Content-Type\" header value (\"charset=utf-8\")."
  :type 'string)

(defcustom verb-default-request-charset "utf-8"
  "Charset to add to \"Content-Type\" headers in HTTP requests.
This variable is only used when the charset isn't specified in the
header value (\"charset=utf-8\")."
  :type 'string)

(defcustom verb-content-type-handlers
  '(;; Text handlers
    ("text/html" html-mode)
    ("\\(application\\|text\\)/xml" xml-mode)
    ("application/xhtml+xml" xml-mode)
    ("application/json" verb--handler-json)
    ("application/javascript" js-mode)
    ("application/css" css-mode)
    ("text/plain" text-mode)
    ;; Binary handlers
    ("application/pdf" doc-view-mode t)
    ("image/png" image-mode t)
    ("image/svg+xml" image-mode t)
    ("image/x-windows-bmp" image-mode t)
    ("image/gif" image-mode t)
    ("image/jpe?g" image-mode t))
  "List of content type handlers.
Handlers are functions to be called without any arguments.  There are
two types of handlers: text and binary.

Text handlers are called after the text contents of the response have
been decoded into a multibyte buffer (with that buffer as the current
buffer).

Binary handlers, on the other hand, are called after the binary
contents of the response have been inserted into a unibyte buffer
\(with that buffer as the current buffer).

Both handler types should prepare the contents of the response buffer,
so that the user can then access or modify the information received in
a convenient way.

Entries of the alist must have the form (CONTENT-TYPE HANDLER BIN?).
CONTENT-TYPE must be a regexp which can match any number of valid
content types, or a string containing a content type.  HANDLER must be
a function that takes no arguments.  BIN?, if present, must be t, in
order to indicate that this handler is binary instead of text.

To choose a handler, Verb will try to match the received content type
with each CONTENT-TYPE in the alist (sequentially) using
`string-match-p'.  The handler for the first CONTENT-TYPE to match
will be used."
  :type '(repeat (list regexp function
		       (choice (const :tag "Binary" t)
			       (const :tag "Text" nil)))))

(defcustom verb-export-functions
  '(("human" . verb--export-to-human)
    ("verb" . verb--export-to-verb)
    ("curl" . verb--export-to-curl))
  "Alist of request specification export functions.
Each element should have the form (NAME . FN), where NAME should be a
user-friendly name for this function, and FN should be the function
itself.  FN should take a `verb-request-spec' object as its only
argument."
  :type '(alist :key-type string :value-type function))

(defcustom verb-auto-kill-response-buffers nil
  "If non-nil, kill all response buffers before sending a request.
Set this variable to t if you wish to have old response buffers (named
*HTTP Response*) automatically killed when sending a new HTTP
request."
  :type 'boolean)

(defcustom verb-inhibit-cookies nil
  "If non-nil, do not send or receive cookies when sending requests."
  :type 'boolean)

(defcustom verb-advice-url t
  "Whether to advice url.el functions or not.
If non-nil, the following url.el functions will be adviced in order to
make Verb more flexible and user-friendly:
- `url-http-user-agent-string': Adviced to allow the user to set their
  own \"User-Agent\" headers.
- `url-http-handle-authentication': Adviced to disable annoying user
  prompt on 401 responses.
Note that the functions will be adviced only during the duration of
the HTTP requests made."
  :type 'boolean)

(defcustom verb-auto-show-headers-buffer nil
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
Indicates the number of seconds to wait after an HTTP request is sent
to warn the user about a possible network timeout.  When set to nil,
don't show any warnings."
  :type '(choice (float :tag "Time in seconds")
		 (const :tag "Off" nil)))

(defcustom verb-babel-timeout 10.0
  "Timeout (s) for HTTP requests made from a Babel source blocks.
Note that Emacs will be blocked while the response hasn't been
receieved."
  :type 'float)

(defcustom verb-code-tag-delimiters '("{{" . "}}")
  "Lisp code tag delimiters for HTTP request specifications.
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

(defcustom verb-enable-log t
  "When non-nil, log different events in the *Verb Log* buffer."
  :type 'boolean)

(defcustom verb-post-response-hook nil
  "Hook run after receiving an HTTP response.
The hook is run with the response body buffer as the current buffer.
The appropiate major mode will have already been activated, and
`verb-response-body-mode' as well.  The buffer will contain the
response's decoded contents.  The buffer-local `verb-http-response'
variable will be set to the corresponding class `verb-response'
object."
  :type 'hook)

(defcustom verb-tag "verb"
  "Tag used to mark headings that contain HTTP request specs.
Headings that do not contain this tag will be ignored when building
requests from heading hierarchies.

If set to t, consider all headings to contain HTTP request specs.

You can set this variable file-locally to use different tags on
different files, like so:

# -*- verb-tag: \"foo\" -*-

Note that if a heading has a tag, then all its subheadings inherit
that tag as well.  This can be changed via the
`org-use-tag-inheritance' variable."
  :type '(choice (string :tag "verb")
		 (const :tag "All" t)))

(defcustom verb-trim-body-end nil
  "When set to a regexp, use it to trim request body endings.
If set to nil, read request bodies as they appear on the buffer.  In
that case, if any whitespace is present between the body end and the
next heading (or buffer end), it will be included in the body."
  :type '(choice (regexp :tag "Other regexp")
		 (const :tag "All whitespace" "[ \t\n\r]+")
		 (const :tag "Disable" nil)))

(defcustom verb-base-headers nil
  "Set of HTTP headers used as base when reading request specs.
These headers will be included by default in requests, but still may
be overriden by re-specifying them somwhere in the document
hierarchy."
  :type '(alist :key-type string :value-type string))

(defface verb-http-keyword '((t :inherit font-lock-constant-face
				:weight bold))
  "Face for highlighting HTTP methods.")

(defface verb-header '((t :inherit font-lock-constant-face))
  "Face for highlighting HTTP headers.")

(defface verb-code-tag '((t :inherit italic))
  "Face for highlighting Lisp code tags.")

(defface verb-json-key '((t :inherit font-lock-doc-face))
  "Face for highlighting JSON keys.")

(defface verb-log-info '((t :inherit homoglyph))
  "Face for highlighting I entries in the log buffer.")

(defface verb-log-warning '((t :inherit warning))
  "Face for highlighting W entries in the log buffer.")

(defface verb-log-error '((t :inherit error))
  "Face for highlighting E entries in the log buffer.")

(defconst verb--http-methods '("GET" "POST" "DELETE" "PUT"
			       "OPTIONS" "HEAD" "PATCH"
			       "TRACE" "CONNECT")
  "List of valid HTTP methods.")

(defconst verb--bodyless-http-methods '("GET" "HEAD" "DELETE" "TRACE"
					"OPTIONS" "CONNECT")
  "List of HTTP methods which usually don't include bodies.")

(defconst verb--log-buffer-name "*Verb Log*"
  "Default name for log buffer.")

(defconst verb--url-pre-defined-headers '("MIME-Version"
					  "Connection"
					  "Host"
					  "Accept-Encoding"
					  "Extension"
					  "Content-Length")
  "List of HTTP headers which are automatically added by url.el.
The values of these headers can't be easily modified by Verb, so a
warning will be shown to the user if they set any of them (as they
will appear duplicated in the request).")

(defconst verb--template-keyword "TEMPLATE"
  "Keyword to use when defining request templates.
Request templates are defined without HTTP methods, paths or hosts.")

(defconst verb--log-levels '(I W E)
  "Log levels for the log buffer.
I = Information.
W = Warning.
E = Error.")

(defconst verb--http-header-regexp "^\\([[:alnum:]-]+:\\).*$"
  "Regexp for font locking HTTP headers.")

(defconst verb--metadata-prefix "verb-"
  "Prefix for Verb metadata keys in heading properties.
Matching is case insensitive.")

(defvar-local verb-http-response nil
  "HTTP response for this response buffer (`verb-response' object).
The decoded body contents of the response are included in the buffer
itself.

In a response buffer, before the response has been received, this
variable will be set to t.")
(put 'verb-http-response 'permanent-local t)

(defvar-local verb--response-headers-buffer nil
  "Buffer currently showing the HTTP response's headers.
This variable is only set on buffers showing HTTP response bodies.")

(defvar-local verb-kill-this-buffer nil
  "If non-nil, kill this buffer after readings its contents.
When Verb evaluates Lisp code tags, a tag may produce a buffer as a
result. If the buffer-local value of this variable is non-nil for that
buffer, Verb will kill it after it has finished reading its contents.")

(defvar verb-last nil
  "Stores the last received HTTP response (`verb-response' object).
This variable is shared across any buffers using Verb mode.  Consider
using this variable inside code tags if you wish to use results from
previous requests on new requests.")

(defvar verb--stored-responses nil
  "Alist of stored HTTP responses.
Responses are stored only when the corresponding HTTP request contains
a nonempty \"Verb-Store\" metadata field.  The response will be stored
here under its value.")

(defvar-local verb--vars nil
  "List of values set with `verb-var', with their corresponding names.")

(defvar verb--requests-count 0
  "Number of HTTP requests sent in the past.")

;;;###autoload
(defvar verb-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") #'verb-send-request-on-point-other-window)
    (define-key map (kbd "C-r") #'verb-send-request-on-point-other-window-stay)
    (define-key map (kbd "C-f") #'verb-send-request-on-point)
    (define-key map (kbd "C-k") #'verb-kill-all-response-buffers)
    (define-key map (kbd "C-e") #'verb-export-request-on-point)
    (define-key map (kbd "C-u") #'verb-export-request-on-point-curl)
    (define-key map (kbd "C-b") #'verb-export-request-on-point-verb)
    (define-key map (kbd "C-n") #'verb-export-request-on-point-human)
    (define-key map (kbd "C-v") #'verb-set-var)
    map)
  "Keymap for `verb-mode' commands.
Bind this to an easy-to-reach key in Org mode in order to use Verb
comfortably.  All commands listed in this keymap automatically enable
`verb-mode' in the current buffer when used.")

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
     (,verb--http-header-regexp
      (1 'verb-header))
     ;; "something": 123
     ("\\s-\\(\"[[:graph:]]+?\"\\)\\s-*:."
      (1 'verb-json-key))
     ;; {{(format "%s" "Lisp code tag")}}
     (,(concat (car verb-code-tag-delimiters)
	       ".*?"
	       (cdr verb-code-tag-delimiters))
      (0 'verb-code-tag))))
  (font-lock-flush))

(defvar verb-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define verb-mode-menu map
      "Menu for Verb mode"
      '("Verb"
	["Send request on selected window" verb-send-request-on-point]
	["Send request on other window & switch" verb-send-request-on-point-other-window]
	["Send request on other window" verb-send-request-on-point-other-window-stay]
	"--"
	["Kill response buffers" verb-kill-all-response-buffers]
	"--"
	["Set variable value" verb-set-var]
	"--"
	["Export request to curl" verb-export-request-on-point-curl]
	["Export request to human-readable" verb-export-request-on-point-human]
	["Export request to Verb" verb-export-request-on-point-verb]
	"--"
	["Customize Verb" verb-customize-group]
	["View log" verb-view-log]))
    map)
  "Keymap for `verb-mode'.")

;;;###autoload
(define-minor-mode verb-mode
  "Minor mode for organizing and making HTTP requests from Emacs.
This mode acts as an extension to Org mode.  Make sure you enable it
on buffers using Org as their major mode.

See the documentation in URL `https://github.com/federicotdn/verb' for
more details on how to use it."
  :lighter " Verb"
  :group 'verb
  (when verb-mode
    (verb--setup-font-lock-keywords)
    (when (buffer-file-name)
      (verb--log nil 'I
		 "Verb mode enabled in buffer: %s"
		 (buffer-name))
      (verb--log nil 'I "Org version: %s, GNU Emacs version: %s"
		 (org-version)
		 emacs-version))))

(defvar verb-response-headers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'verb-kill-buffer-and-window)
    map)
  "Keymap for `verb-response-headers-mode'.")

(define-derived-mode verb-response-headers-mode special-mode "Verb[Headers]"
  "Major mode for displaying an HTTP response's headers."
  (font-lock-add-keywords
   nil `(;; Key: Value
	 (,verb--http-header-regexp
	  (1 'verb-header)))))

(define-derived-mode verb-log-mode special-mode "Verb[Log]"
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
   nil '(;; (request number e.g. 10)
	 ("^[[:digit:]-]+\\s-"
	  (0 'bold))
	 ;; Log level I after request number
	 ("^[[:digit:]-]*\\s-+\\(I\\)"
	  (1 'verb-log-info))
	 ;; Log level W after request number
	 ("^[[:digit:]-]*\\s-+\\(W\\)"
	  (1 'verb-log-warning))
	 ;; Log level E after request number
	 ("^[[:digit:]-]*\\s-+\\(E\\)"
	  (1 'verb-log-error)))))

(defun verb--http-method-p (m)
  "Return non-nil if M is a valid HTTP method."
  (member m verb--http-methods))

(defun verb--alist-p (l)
  "Return on-nil if L is an alist."
  (when (consp l)
    (catch 'end
      (dolist (elem l)
	(unless (consp elem)
	  (throw 'end nil)))
      t)))

(defun verb--http-headers-p (h)
  "Return non-nil if H is an alist of (KEY . VALUE) elements.
KEY and VALUE must be strings.  KEY must not be the empty string."
  (when (consp h)
    (catch 'end
      (dolist (elem h)
	(unless (and (consp elem)
		     (stringp (car elem))
		     (stringp (cdr elem))
		     (not (string-empty-p (car elem))))
	  (throw 'end nil)))
      t)))

(defclass verb-request-spec ()
  ((method :initarg :method
	   :initform nil
	   :type (or null verb--http-method)
	   :documentation "HTTP method.")
   (url :initarg :url
	:initform nil
	:type (or null url)
	:documentation "Request URL.")
   (headers :initarg :headers
	    :initform ()
	    :type (or null verb--http-headers)
	    :documentation "HTTP headers.")
   (body :initarg :body
	 :initform nil
	 :type (or null string)
	 :documentation "Request body.")
   (metadata :initarg :metadata
	     :initform nil
	     :type (or null verb--alist)
	     :documentation "User-defined request metadata."))
  "Represents an HTTP request to be made.")

(defclass verb-response ()
  ((request :initarg :request
	    :type verb-request-spec
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
   (body :initarg :body
	 :initform nil
	 :type (or null string)
	 :documentation "Response body.")
   (body-bytes :initarg :body-bytes
	       :initform 0
	       :type integer
	       :documentation
	       "Number of bytes in response body."))
  "Represents an HTTP response to a request.")

(defvar verb-response-body-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r C-r") #'verb-toggle-show-headers)
    (define-key map (kbd "C-c C-r C-k") #'verb-kill-response-buffer-and-window)
    (define-key map (kbd "C-c C-r C-f") #'verb-re-send-request)
    (easy-menu-define verb-response-body-mode-menu map
      "Menu for Verb response body mode"
      '("Verb[Body]"
	["Toggle show response headers" verb-toggle-show-headers]
	["Kill buffer and window" verb-kill-response-buffer-and-window]
	["Re-send request" verb-re-send-request]))
    map)
  "Keymap for `verb-response-body-mode'.")

(define-minor-mode verb-response-body-mode
  "Minor mode for displaying an HTTP response's body."
  :lighter " Verb[Body]"
  :group 'verb
  (if verb-response-body-mode
      (progn
	(setq header-line-format
	      (verb--response-header-line-string verb-http-response))
	(when verb-auto-show-headers-buffer
	  (if (eq verb-auto-show-headers-buffer 'when-empty)
	      (when (zerop (oref verb-http-response body-bytes))
		(verb-toggle-show-headers))
	    (verb-toggle-show-headers))))
    (setq header-line-format nil)))

(defun verb-view-log ()
  "Switch to the *Verb Log* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create verb--log-buffer-name)))

(defun verb-customize-group ()
  "Show the Customize menu buffer for the Verb package group."
  (interactive)
  (customize-group "verb"))

(defun verb--log (request level &rest args)
  "Log a message in the *Verb Log* buffer.
REQUEST must be a number corresponding to an HTTP request made.  LEVEL
must be a value in `verb--log-levels'.  The remaining ARGS are used to
call `format', and then the result is logged in the log buffer.

If `verb-enable-log' is nil, do not log anything."
  (setq request (if request (number-to-string request) "-"))
  (unless (member level verb--log-levels)
    (user-error "Invalid log level: \"%s\"" level))
  (when verb-enable-log
    (with-current-buffer (get-buffer-create verb--log-buffer-name)
      (unless (derived-mode-p 'verb-log-mode)
	(verb-log-mode))
      (let ((inhibit-read-only t)
	    (last "")
	    (line (line-number-at-pos)))
	;; Get last logged request number
	(when (re-search-backward "^\\(-\\|[[:digit:]]+\\)\\s-"
				  nil t)
	  (setq last (match-string 1)))
	(goto-char (point-max))
	;; Log new message
	(insert (format "%s  %s  "
			(if (string= last request)
			    (make-string (length request) ? )
			  request)
			level)
		(apply #'format args)
		"\n")
	;; If logged messaged contained newlines, add a blank line
	;; to make things more readable
	(when (> (line-number-at-pos) (1+ line))
	  (newline))))))

(defun verb--ensure-verb-mode ()
  "Ensure `verb-mode' is enabled in the current buffer."
  (unless verb-mode
    (verb-mode)))

(defun verb--nonempty-string (s)
  "Return S. If S is the empty string, return nil."
  (if (string-empty-p s)
      nil
    s))

(defun verb-headers-get (headers name)
  "Return value for HTTP header under NAME in HEADERS.
HEADERS must be an alist of (KEY . VALUE) elements.  NAME and KEY will
be compared ignoring case.  If no value is present under NAME, signal
an error."
  (if-let ((val (assoc-string name headers t)))
      (cdr val)
    (user-error "HTTP header has no value for \"%s\"" name)))

(defun verb-json-get (text &rest path)
  "Interpret TEXT as a JSON object and return value under PATH.
The outermost JSON element in TEXT must be an object.
PATH must be a list of strings, symbols (which will be converted to
strings), or integers.  The PATH list will be traversed from beginning
to end, using each item to access a sub-value in the current JSON
element (and setting the current JSON element to that new value).
This is supposed to work in a similar way JSONPath does, more info at
URL `https://goessner.net/articles/JsonPath/'.

For example, for the following TEXT:
{
  \"test\": {
    \"foo\": [\"apples\", \"oranges\"]
  }
}

Using PATH (\"test\" \"foo\" 1) will yield \"oranges\"."
  (unless path
    (user-error "%s" "No path specified for JSON value"))
  (let ((obj (json-read-from-string text)))
    (dolist (key path)
      (setq
       obj
       (cond
	(;; Path element is a string (or symbol)
	 (or (stringp key) (symbolp key))
	 (when (symbolp key)
	   (setq key (symbol-name key)))
	 ;; Obj may be an alist, plist or hash-table
	 (pcase json-object-type
	   ('alist
	    (cdr (assoc-string key obj)))
	   ('plist
	    (plist-get obj (intern (concat ":" key))))
	   ('hash-table
	    (gethash key obj))
	   (_
	    (user-error "%s" "Unknown value for `json-object-type'"))))
	(;; Path element is an integer
	 (integerp key)
	 ;; Obj may be a list or a vector
	 (pcase json-array-type
	   ('list
	    (nth key obj))
	   ('vector
	    (aref obj key))
	   (_
	    (user-error "%s" "Unknown value for `json-array-type'"))))
	(;; Invalid key type
	 t
	 (user-error "Invalid key: %s" key)))))
    obj))

(defun verb--buffer-string-no-properties ()
  "Return the contents of the current buffer as a string.
Do not include text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun verb--back-to-heading ()
  "Move to the previous heading.
Or, move to beggining of this line if it's a heading.  If there are no
headings, move to the beggining of buffer.  Return t if a heading was
found."
  (if (ignore-errors (org-back-to-heading t))
      t
    (goto-char (point-min))
    nil))

(defun verb--up-heading ()
  "Move to the parent heading, if there is one.
Return t if there was a heading to move towards to and nil otherwise."
  (let ((p (point)))
    (ignore-errors
      (org-up-heading-all 1)
      (not (= p (point))))))

(defun verb--heading-tags ()
  "Return all (inherited) tags from current heading."
  (verb--back-to-heading)
  (when-let ((tags (org-entry-get (point) "ALLTAGS" t)))
    (split-string (string-trim tags ":" ":") ":")))

(defun verb--heading-properties (prefix)
  "Return alist of current heading properties starting with PREFIX.
Does not use property inheritance.  Matching is case-insensitive."
  (verb--back-to-heading)
  (seq-filter (lambda (e)
		(string-prefix-p prefix (car e) t))
	      (org-entry-properties (point))))

(defun verb--heading-contents ()
  "Return the current heading's text contents.
If not on a heading, signal an error."
  (unless (org-at-heading-p)
    (user-error "%s" "Can't get heading text contents: not at a heading"))
  (let ((start (save-excursion
		 (end-of-line)
		 (unless (eobp) (forward-char))
		 (point)))
	(end (save-excursion
	       (goto-char (org-entry-end-position))
	       (when (and (org-at-heading-p)
			  (not (eobp)))
		 (backward-char))
	       (point))))
    (if (<= start end)
	(buffer-substring-no-properties start end)
      "")))

(defun verb--request-spec-from-heading ()
  "Return a request spec from the current heading's text contents.
If a heading is found, get its contents using
`verb--heading-contents'.  After getting the heading's text content,
run it through `verb--maybe-extract-babel-src-block'.  From that
result, try to parse a request specification.  Return nil if the
heading has no text contents, if contains only comments, or if the
heading does not have the tag `verb-tag'.
If not on a heading, signal an error."
  (unless (org-at-heading-p)
    (user-error "%s" "Can't read request spec: not at a heading"))
  (when (or (member verb-tag (verb--heading-tags))
	    (eq verb-tag t))
    (let ((text (verb--maybe-extract-babel-src-block
		 (verb--heading-contents)))
	  (metadata (verb--heading-properties verb--metadata-prefix)))
      (unless (string-empty-p text)
	(condition-case nil
	    (verb-request-spec-from-string text metadata)
	  (verb-empty-spec nil))))))

(defun verb--maybe-extract-babel-src-block (text)
  "Return contents of the first Verb Babel source block in TEXT.
If no Babel source blocks are found, return TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search t)
	  start result)
      (when (search-forward "#+begin_src" nil t)
	(unless (looking-at " +verb")
	  (user-error "%s" (concat "Found a non-verb Babel source block\n"
				   "Make sure all source blocks in the "
				   "hierarchy use \"verb\" as language")))
	;; Found the start
	(end-of-line)
	(forward-char)
	(setq start (point))
	(when (search-forward "#+end_src" nil t)
	  ;; Found the end
	  (beginning-of-line)
	  (backward-char)
	  (setq result
		(if (<= start (point))
		    (buffer-substring-no-properties start (point))
		  ""))))
      (or result text))))

(defun verb--request-spec-from-babel-src-block (pos body)
  "Return a request spec generated from a Babel source block.
BODY should contain the body of the source block.  POS should be a
position of the buffer that lies inside the source block.

Note that the entire buffer will be considered when generating the
request spec, not only the section contained by the source block."
  (save-excursion
    (goto-char pos)
    (let* ((metadata (verb--heading-properties verb--metadata-prefix))
	   (rs (verb-request-spec-from-string body metadata)))
      ;; Go up one level first, if possible. Do this to avoid
      ;; re-reading the request in the current level (contained in the
      ;; source block). If no more levels exist, skip the call to
      ;; `verb--request-spec-from-hierarchy'.
      (when (verb--up-heading)
	;; Continue reading requests from the headings
	;; hierarchy. Pre-include the one we read from the source block
	;; at the end of the list.
	(setq rs (verb--request-spec-from-hierarchy rs)))
      (verb--request-spec-post-process rs))))

(defun verb--request-spec-post-process (rs)
  "Validate and prepare request spec RS to be used.
- Check if `verb-base-headers' needs to be applied.
- Run validations with `verb-request-spec-validate'.
Return another request spec corresponding to RS."
  ;; Use `verb-base-headers' if necessary
  (when verb-base-headers
    (setq rs (verb-request-spec-override
	      (verb-request-spec :headers verb-base-headers)
	      rs)))
  ;; Validate and return
  (verb-request-spec-validate rs))

(defun verb--request-spec-from-hierarchy (&rest specs)
  "Return a request spec generated from the headings hierarchy.
To do this, use `verb--request-spec-from-heading' for the current
heading, for that heading's parent, and so on until the root of the
hierarchy is reached.
Once all the request specs have been collected, override them in
inverse order according to the rules described in
`verb-request-spec-override'.  After that, override that result with
all the request specs in SPECS, in the order they were passed in."
  (let (done final-spec)
    (save-excursion
      ;; First, go back to the current heading, if possible. If no
      ;; heading is found, then don't attempt to read anything.
      (setq done (not (verb--back-to-heading)))
      ;; If there's at least one heading above us, go up through the
      ;; headings tree taking a request specification from each level.
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
	      (setq final-spec (verb-request-spec-override final-spec
							   spec))))
	  ;; Process and return
	  (verb--request-spec-post-process final-spec))
      (user-error (concat "No request specifications found\n"
			  "Remember to tag your headlines with :%s:")
		  verb-tag))))

(defun verb--split-window ()
  "Split selected window by its longest side."
  (split-window nil nil (if (< (window-pixel-height)
			       (window-pixel-width))
			    'right
			  'below)))

(defun verb-kill-response-buffer-and-window (&optional keep-window)
  "Delete response window and kill its buffer.
If KEEP-WINDOW is non-nil, kill the buffer but do not delete the
window.
If the response buffer has a corresponding headers buffer, kill it and
delete any window displaying it."
  (interactive)
  (let ((response-buf (current-buffer)))
    (when verb--response-headers-buffer
      (when-let ((w (get-buffer-window verb--response-headers-buffer)))
	(ignore-errors
	  (delete-window w)))
      (when (buffer-live-p verb--response-headers-buffer)
	(kill-buffer verb--response-headers-buffer)))
    (unless keep-window
      (when-let ((w (get-buffer-window response-buf)))
	(ignore-errors
	  (delete-window))))
    (kill-buffer response-buf)))

(defun verb-re-send-request ()
  "Re-send request for the response shown on current buffer.
If the user chose to show the current response buffer on another
window, show the new one on another window as well.  Return the buffer
where the response will be loaded in.

If you use this command frequently, consider setting
`verb-auto-kill-response-buffers' to t.  This will help avoiding
having many response buffers open."
  (interactive)
  (verb--request-spec-send (oref verb-http-response request)
			   'this-window))

(defun verb-kill-buffer-and-window ()
  "Delete selected window and kill its current buffer.
Delete the window only if it isn't the only window in the frame."
  (interactive)
  (kill-buffer (current-buffer))
  (ignore-errors
    (delete-window)))

(defmacro verb-var (var &optional default)
  "Return value of Verb variable VAR.
If VAR is has no value yet, use `read-string' to set its value first,
unless DEFAULT is non-nil, in which case that value is used instead."
  (let ((val (assoc-string var verb--vars)))
    (unless val
      (setq val (cons var
		      (or default
			  (read-string (format "(verb-var) Set value for %s: "
					       var)))))
      (push val verb--vars))
    (cdr val)))

(defun verb-set-var (&optional var value)
  "Set new value for variable VAR previously set with `verb-var'.
When called interactively, prompt the user for a variable that has
been set once with `verb-var', and then prompt for VALUE."
  (interactive)
  (verb--ensure-verb-mode)
  (unless verb--vars
    (user-error "%s" (concat "No variables have been initialized yet\n"
			     "Run a {{(verb-var my-var)}} code tag first")))
  (let* ((v (or var
	       (completing-read "Variable: " (mapcar (lambda (e)
						       (symbol-name (car e)))
						     verb--vars)
				nil t)))
	 (val (or value (read-string (format "Set value for %s: " v))))
	 (elem (assoc-string v verb--vars)))
    (if elem
	(setcdr elem val)
      (user-error "Variable does not exist: %s" v))))

(defun verb-read-file (file &optional coding-system)
  "Return a buffer with the contents of FILE.
If CODING-SYSTEM system is a valid coding system, use it when reading
the file contents (see `coding-system-for-read' for more information).
Set the buffer's `verb-kill-this-buffer' variable to t."
  (with-current-buffer (generate-new-buffer " *verb-temp*")
    (buffer-disable-undo)
    (let ((coding-system-for-read coding-system))
      (insert-file-contents file))
    (setq verb-kill-this-buffer t)
    (current-buffer)))

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
    (let ((headers (oref verb-http-response headers)))
      (with-selected-window (verb--split-window)
	(switch-to-buffer verb--response-headers-buffer)
	(verb-response-headers-mode)
	(setq header-line-format (format "HTTP Response Headers | count: %s"
					 (length headers)))
	(verb--insert-header-contents headers)
	(fit-window-to-buffer)))))

;;;###autoload
(defun verb-send-request-on-point-other-window ()
  "Send the request specified by the selected heading's text contents.
Show the results on another window and switch to it (use
`verb-send-request-on-point')."
  (interactive)
  (verb-send-request-on-point 'other-window))

;;;###autoload
(defun verb-send-request-on-point-other-window-stay ()
  "Send the request specified by the selected heading's text contents.
Show the results on another window, but don't switch to it (use
`verb-send-request-on-point')."
  (interactive)
  (verb-send-request-on-point 'stay-window))

;;;###autoload
(defun verb-send-request-on-point (where)
  "Send the request specified by the selected heading's text contents.
After the request has been sent, return the response buffer (the buffer
where the response will be loaded into).

Note that the contents of all parent headings are considered as well;
see `verb--request-spec-from-hierarchy' to see how this is done.

If WHERE is `other-window', show the results of the request (the
response buffer) on another window and select it.  If WHERE is
`show-window', show the results of the request on another window, but
keep the current one selected.  If WHERE is `this-window', show the
results of the request in the current window.  If WHERE has any other
value, send the request but do not show the results anywhere.

The `verb-post-response-hook' hook is called after a response has been
received."
  (interactive (list 'this-window))
  (verb--ensure-verb-mode)
  (verb--request-spec-send (verb--request-spec-from-hierarchy)
			   where))

;;;###autoload
(defun verb-kill-all-response-buffers (&optional keep-windows)
  "Kill all response buffers, and delete their windows.
If KEEP-WINDOWS is non-nil, do not delete their respective windows."
  (interactive)
  (verb--ensure-verb-mode)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when verb-http-response
	(verb-kill-response-buffer-and-window keep-windows)))))

;;;###autoload
(defun verb-export-request-on-point (&optional name)
  "Export the request specification on point.
Interactively, prompt the user for an export function, and call that
function with the request specification object.  See the
`verb-export-functions' variable for more details.  If called from
Lisp, use the export function under NAME.  If NAME is nil, prompt the
user anyways.

No HTTP request will be sent, unless the export function does this
explicitly.  Lisp code tags will be evaluated before exporting."
  (interactive)
  (verb--ensure-verb-mode)
  (let ((rs (verb--request-spec-from-hierarchy))
	(exporter (or name
		      (completing-read "Export function: "
				       verb-export-functions
				       nil t))))
    (when-let ((fn (cdr (assoc exporter verb-export-functions))))
      (funcall fn rs)
      (verb--log nil 'I "Exported request to %s format." exporter))))

;;;###autoload
(defun verb-export-request-on-point-verb ()
  "Export request on point to verb format.
See `verb--export-to-verb' for more information."
  (interactive)
  (verb-export-request-on-point "verb"))

;;;###autoload
(defun verb-export-request-on-point-human ()
  "Export request on point to a human-readable format.
See `verb--export-to-human' for more information."
  (interactive)
  (verb-export-request-on-point "human"))

;;;###autoload
(defun verb-export-request-on-point-curl ()
  "Export request on point to curl format.
See `verb--export-to-curl' for more information."
  (interactive)
  (verb-export-request-on-point "curl"))

(defun verb--export-to-human (rs)
  "Export a request spec RS to a human-readable format.
Return a new buffer with the export results inserted into it."
  (with-current-buffer (generate-new-buffer "*HTTP Request Spec*")
    (text-mode)
    (insert (propertize "HTTP Method: " 'font-lock-face 'bold)
	    (oref rs method) "\n"
	    (propertize "URL: " 'font-lock-face 'bold)
	    (url-recreate-url (oref rs url)) "\n"
	    (propertize "Headers:\n" 'font-lock-face 'bold))
    (let ((headers (oref rs headers)))
      (if headers
	  (dolist (key-value headers)
	    (insert "    " (car key-value) ": " (cdr key-value) "\n"))
	(insert "    No headers defined.\n")))
    (insert "\n")
    (let ((body (oref rs body)))
      (if body
	  (insert (propertize "Body:" 'font-lock-face 'bold) "\n"
		  body "\n")
	(insert "No body defined.")))
    (switch-to-buffer-other-window (current-buffer))
    (current-buffer)))

(defun verb--export-to-verb (rs)
  "Export a request spec RS to Verb format.
Return a new buffer with the export results inserted into it."
  (with-current-buffer (generate-new-buffer "*HTTP Request Spec*")
    (verb-mode)
    (insert (verb-request-spec-to-string rs))
    (switch-to-buffer-other-window (current-buffer))
    (current-buffer)))

(defun verb--export-to-curl (rs &optional no-message no-kill)
  "Export a request spec RS to curl format.
Add the generated command to the kill ring and return it.  For more
information about curl see URL `https://curl.haxx.se/'.  If NO-MESSAGE
is non-nil, do not display a message on the minibuffer.  If NO-KILL is
non-nil, do not add the command to the kill ring."
  (with-temp-buffer
    (insert "curl '" (verb-request-spec-url-to-string rs) "'")
    (dolist (key-value (oref rs headers))
      (insert " \\\n")
      (insert "-H '" (car key-value) ": " (cdr key-value) "'"))
    (unless (string= (oref rs method) "GET")
      (insert (if (oref rs headers) " \\\n" " ")))
    (pcase (oref rs method)
      ;; GET: no argument needed
      ((or "PATCH" "PUT" "POST")
       (insert "-X "
	       (oref rs method)
	       " \\\n--data-raw '"
	       (or (oref rs body) "")
	       "'"))
      ("DELETE"
       (insert "-X DELETE"))
      ("OPTIONS"
       (insert "-X OPTIONS -i"))
      ("HEAD"
       (insert "-I"))
      ("TRACE"
       (insert "-X TRACE"))
      ("CONNECT"
       (user-error "%s" "CONNECT method not supported in curl format")))
    (let ((result (verb--buffer-string-no-properties)))
      (unless no-kill
	(kill-new result))
      (unless no-message
	(message "Curl command copied to the kill ring"))
      ;; Return the generated command
      result)))

(cl-defmethod verb--response-header-line-string ((response verb-response))
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
			     "-")))
       (format " | %s" content-type))
     (let* ((content-length (cdr (assoc-string "Content-Length"
					       headers t)))
	    (value (if content-length
		       (string-to-number content-length)
		     bytes)))
       (format " | %s byte%s"
	       (file-size-human-readable value)
	       (if (= value 1) "" "s"))))))

(cl-defmethod verb-request-spec-url-to-string ((rs verb-request-spec))
  "Return RS's url member as a string if it is non-nil."
  (let ((url (oref rs url)))
    (when url
      (url-recreate-url url))))

(defun verb--handler-json ()
  "Handler for \"application/json\" content type."
  (js-mode)
  (when (< (oref verb-http-response body-bytes)
	   (or verb-json-max-pretty-print-size 0))
    (unwind-protect
	(unless (zerop (buffer-size))
	  (let ((json-pretty-print-max-secs 0))
	    (buffer-disable-undo)
	    (json-pretty-print-buffer)
	    ;; "Use" `json-pretty-print-max-secs' here to avoid byte-compiler warning in
	    ;; Emacs 26
	    json-pretty-print-max-secs))
      (buffer-enable-undo))
    (goto-char (point-min))))

(defun verb--headers-content-type (headers)
  "Return the value of the \"Content-Type\" header in HEADERS.
The value returned has the form (TYPE . CHARSET).  If the charset is
not present, return (TYPE . nil).  If the header itself is not
present, return (nil . nil)."
  (let* ((value (cdr (assoc-string "Content-Type" headers t)))
	 (type-subtype (and value (string-trim (car (split-string
						     value ";"))))))
    (if (and value type-subtype)
	(cons type-subtype
	      (when (string-match "charset=\\([[:alnum:]-]+\\)" value)
		(match-string 1 value)))
      (cons nil nil))))

(defun verb--get-handler (content-type)
  "Return a handler from `verb-content-type-handlers' for a CONTENT-TYPE.
CONTENT-TYPE must be the value returned by `verb--headers-content-type'."
  (when (car content-type)
    (catch 'end
      (dolist (key-value verb-content-type-handlers)
	(let ((case-fold-search t)
	      (regexp (car key-value))
	      (handler (cdr key-value)))
	  (when (string-match-p regexp (car content-type))
	    (throw 'end handler)))))))

(defun verb--maybe-store-response (response)
  "Store RESPONSE depending on its request metadata.
Check `verb--stored-responses' for more details."
  (when-let ((req (oref response request))
	     (metadata (oref req metadata))
	     (val (verb--nonempty-string (cdr (assoc-string
					       "verb-store"
					       metadata t)))))
    (setq verb--stored-responses (cl-delete val verb--stored-responses
					    :key #'car
					    :test #'equal))
    (push (cons val response) verb--stored-responses)))

(defun verb-stored-response (key)
  "Return stored HTTP response under KEY.
To automatically store HTTP responses, set the request heading's
\"Verb-Store\" property to a nonempty value.  The response will then
be stored under that value. For example:

* Example          :verb:
:properties:
:Verb-Store: storage-key-here
:end:
get https://gnu.org/test"
  (if-let ((resp (assoc-string key verb--stored-responses)))
      (cdr resp)
    (user-error (concat "No response stored under key \"%s\"\n"
			"Make sure you've set the \"Verb-Store\" heading property"
			" and sent the request at least once")
		key)))

(defun verb--request-spec-callback (status rs response-buf start timeout-timer where num)
  "Callback for `verb--request-spec-send' for request RS.
More response information can be read from STATUS.
RESPONSE-BUF points to a buffer where the response should be copied
to, which the user can then use or edit freely.
START contains a floating point number indicating the timestamp at
which the request was sent.
TIMEOUT-TIMER contains a timer set to call `verb--timeout-warn', or
nil.
WHERE describes where the results should be shown in (see
`verb-send-request-on-point').
NUM is this request's identification number.

This function sets up the current buffer so that it can be used to
view the HTTP response in a user-friendly way."
  (when timeout-timer
    (cancel-timer timeout-timer))

  ;; Remove url.el advice
  (verb--unadvice-url)

  ;; Handle errors first
  (when-let ((http-error (plist-get status :error))
	     (error-info (cdr http-error))
	     (url (oref rs url)))
    ;; If there's an HTTP error code (404, 405, etc.) in the error
    ;; information, continue as normal
    (unless (numberp (and (eq (car error-info) 'http)
			  (cadr error-info)))
      (kill-buffer (current-buffer))
      (kill-buffer response-buf)
      (let ((msg (format "Request error: could not connect to %s:%s"
			 (url-host url) (url-port url))))
	(verb--log num 'E msg)
	(verb--log num 'E "Error details: %s" http-error)
	(user-error "%s" msg))))

  ;; No errors, continue to read response
  (let ((elapsed (- (time-to-seconds) start))
	(original-buffer (current-buffer))
	status-line headers content-type charset coding-system body-bytes
	binary-handler text-handler)

    (widen)
    (goto-char (point-min))
    ;; Skip HTTP/1.X status line
    (setq status-line (verb--nonempty-string
		       (buffer-substring-no-properties (point)
						       (line-end-position))))

    (verb--log num 'I "%s" status-line)

    (forward-line)
    ;; Skip all HTTP headers
    (while (re-search-forward "^\\s-*\\([[:alnum:]-]+\\)\\s-*:\\s-*\\(.*\\)$"
			      (line-end-position) t)
      (let ((key (match-string 1))
	    (value (match-string 2)))
	;; Save header to alist
	(push (cons key value) headers)
	(unless (eobp) (forward-char))))

    ;; Read Content-Type and charset
    (setq content-type (verb--headers-content-type headers))
    (setq charset (or (cdr content-type) verb-default-response-charset))

    ;; Try to get a buffer handler function for this content type
    (let ((handler (verb--get-handler content-type)))
      (unless handler
	;; Default handler is fundamental mode (text)
	(setq handler #'fundamental-mode))

      (if (= (length handler) 1)
	  ;; Text handler
	  (setq text-handler (nth 0 handler))
	;; Binary handler (maybe)
	(unless (and (consp handler)
		     (functionp (nth 0 handler))
		     (eq (nth 1 handler) t)
		     (= (length handler) 2))
	  (user-error "Invalid content handler: %s" handler))
	(setq binary-handler (nth 0 handler))))

    ;; Remove headers and blank line from buffer
    ;; All left should be the content
    (beginning-of-line)
    (forward-line)
    (delete-region (point-min) (point))

    ;; Record body size in bytes
    (setq body-bytes (buffer-size))

    ;; Current buffer should be unibyte
    (when enable-multibyte-characters
      (error "%s" "Expected a unibyte buffer for HTTP response"))

    ;; Store details of request and response
    ;; `verb-http-response' is a permanent buffer local variable
    (with-current-buffer response-buf
      (setq verb-http-response
	    (verb-response :headers (nreverse headers)
			   :request rs
			   :status status-line
			   :duration elapsed
			   :body-bytes body-bytes))

      ;; Update global last response variable
      (setq verb-last verb-http-response)

      ;; Store the response separately as well depending on user
      ;; metadata
      (verb--maybe-store-response verb-http-response))

    (if binary-handler
	;; Response content is a binary format:

	(progn
	  (verb--log num 'I "Using binary handler: %s" binary-handler)
	  (with-current-buffer response-buf
	    (fundamental-mode)
	    (set-buffer-multibyte nil)
	    (set-buffer-file-coding-system 'binary)

	    (buffer-disable-undo)
	    ;; Copy bytes into RESPONSE-BUF
	    (insert-buffer-substring original-buffer)
	    (goto-char (point-min))
	    (funcall binary-handler)))

      ;; Response content is text:

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
      ;; Now that the response content has been processed, update
      ;; `verb-http-response's body slot
      (oset verb-http-response
	    body
	    (unless (zerop (oref verb-http-response body-bytes))
	      (verb--buffer-string-no-properties)))

      (when text-handler
	(verb--log num 'I "Using text handler: %s" text-handler)

	(set-buffer-file-coding-system coding-system)

	;; Prepare buffer for editing by user
	(goto-char (point-min))
	(funcall text-handler))

      (pcase where
	('other-window (switch-to-buffer-other-window (current-buffer)))
	('stay-window (save-selected-window
			(switch-to-buffer-other-window (current-buffer))))
	('this-window (switch-to-buffer (current-buffer)))))

    (with-current-buffer response-buf
      (verb-response-body-mode)

      ;; Run post response hook
      (run-hooks 'verb-post-response-hook))))

(defun verb--prepare-http-headers (headers)
  "Prepare alist HEADERS of HTTP headers to use them on a request.
Add/modify/remove the following headers if they are not already
present/incomplete:

Accept:
  Remove header from list (will be set via `url-mime-accept-string').

Uses `verb--to-ascii' to ensure all added text is unibyte.
Returns a new alist, does not modify HEADERS."
  (let* ((headers (copy-alist headers))
	 (accept (assoc-string "Accept" headers t)))
    ;; ;; Accept
    (when accept
      (setq headers (cl-delete "Accept" headers
    			       :key #'car
			       :test #'verb--string=)))
    ;; Encode all text to `us-ascii'
    (mapcar (lambda (e)
	      (cons (verb--to-ascii (car e))
		    (verb--to-ascii (cdr e))))
	    headers)))


(defun verb--encode-http-body (body charset)
  "Encode content BODY using CHARSET.
If CHARSET is nil, use `verb-default-request-charset'."
  (when body
    (if-let ((inhibit-message t)
	     (coding-system (mm-charset-to-coding-system
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

(defun verb--http-handle-authentication (_proxy)
  "Replacement function for `url-http-handle-authentication'."
  t)

(defun verb--http-user-agent-string ()
  "Replacement function for `url-http-user-agent-string'."
  nil)

(defun verb--advice-url ()
  "Advice some url.el functions.
For more information, see `verb-advice-url'."
  (when verb-advice-url
    (advice-add 'url-http-user-agent-string :override
		#'verb--http-user-agent-string)
    (advice-add 'url-http-handle-authentication :override
		#'verb--http-handle-authentication)))

(defun verb--unadvice-url ()
  "Undo advice from `verb--advice-url'."
  (when verb-advice-url
    (advice-remove 'url-http-user-agent-string
		   #'verb--http-user-agent-string)
    (advice-remove 'url-http-handle-authentication
		   #'verb--http-handle-authentication)))

(cl-defmethod verb-request-spec-validate ((rs verb-request-spec))
  "Run validations on request spec RS and return it.
If a validation does not pass, signal with `user-error'."
  (unless (oref rs method)
    (user-error "%s" (concat "No HTTP method specified\n"
			     "Make sure you specify a concrete HTTP "
			     "method (not " verb--template-keyword
			     ") in the heading hierarchy")))
  (let ((url (oref rs url)))
    (unless url
      (user-error "%s" (concat "No URL specified\nMake sure you specify "
			       "a nonempty URL in the heading hierarchy")))
    (unless (url-host url)
      (user-error "%s" (concat "URL has no schema or host defined\n"
			       "Make sure you specify a schema and host "
			       "(e.g. \"https://github.com\") in the "
			       "heading hierarchy"))))
  rs)

(defun verb--generate-response-buffer ()
  "Return a new buffer ready to be used as response buffer."
  (with-current-buffer (generate-new-buffer "*HTTP Response*")
    ;; Set `verb-http-response's value to something other than nil
    ;; so that `verb-kill-all-response-buffers' can find it even if
    ;; no response was ever received.
    (setq verb-http-response t)
    (current-buffer)))

(cl-defmethod verb--request-spec-send ((rs verb-request-spec) where)
  "Send the HTTP request described by RS.
Show the results according to parameter WHERE (see
`verb-send-request-on-point').  Return the buffer the response will
be loaded into."
  ;; If auto kill buffers is enabled, kill all previous response
  ;; buffers now
  (when verb-auto-kill-response-buffers
    (verb-kill-all-response-buffers t))

  (let* ((url (oref rs url))
	 (accept-header (cdr (assoc-string "Accept"
					   (oref rs headers) t)))
	 (url-request-method (verb--to-ascii (oref rs method)))
	 (url-request-extra-headers (verb--prepare-http-headers
				     (oref rs headers)))
	 (content-type (verb--headers-content-type
			url-request-extra-headers))
	 (url-request-data (verb--encode-http-body (oref rs body)
						   (cdr content-type)))
	 (url-mime-accept-string (or accept-header "*/*"))
	 (response-buf (verb--generate-response-buffer))
	 (num (setq verb--requests-count (1+ verb--requests-count)))
	 timeout-timer)
    ;; Start the timeout warning timer
    (when verb-show-timeout-warning
      (setq timeout-timer (run-with-timer verb-show-timeout-warning nil
					  #'verb--timeout-warn
					  response-buf rs num)))

    ;; Advice url.el functions
    (verb--advice-url)

    ;; Look for headers that might get duplicated by url.el
    (dolist (h verb--url-pre-defined-headers)
      (when (assoc-string h url-request-extra-headers t)
	(verb--log num 'W (concat "Header \"%s\" will appear duplicated "
				  "in the request, as url.el adds its "
				  "own version of it")
		   h)))

    ;; Maybe log a warning if body is present but method usually
    ;; doesn't take one
    (when (and (member url-request-method verb--bodyless-http-methods)
	       url-request-data)
      (verb--log num 'W "Body is present but request method is %s"
		 url-request-method))

    ;; Send the request!
    (condition-case err
	(funcall verb-url-retrieve-function
		 url
		 #'verb--request-spec-callback
		 (list rs
		       response-buf
		       (time-to-seconds)
		       timeout-timer
		       where
		       num)
		 t verb-inhibit-cookies)
      (error (progn
	       ;; Cancel timer
	       (when timeout-timer
		 (cancel-timer timeout-timer)
		 (setq timeout-timer nil))
	       ;; Kill response buffer
	       (kill-buffer response-buf)
	       ;; Undo advice
	       (verb--unadvice-url)

	       (let ((msg (format "Error sending request: %s" (cadr err))))
		 ;; Log the error
		 (verb--log num 'E msg)
		 ;; Signal it
		 (user-error "%s" msg)))))

    ;; Show user some quick information
    (message "%s request sent to %s"
	     (oref rs method)
	     (verb-request-spec-url-to-string rs))

    ;; Log the request
    (verb--log num 'I "%s %s"
	       (oref rs method)
	       (verb-request-spec-url-to-string rs))

    ;; Return the response buffer
    response-buf))

(cl-defmethod verb-request-spec-to-string ((rs verb-request-spec))
  "Return request spec RS as a string.
This string should be able to be used with
`verb-request-spec-from-string', yielding the same request spec again."
  (with-temp-buffer
    (insert (oref rs method) " "
	    (url-recreate-url (oref rs url)) "\n")
    (dolist (key-value (oref rs headers))
      (insert (car key-value) ": " (cdr key-value) "\n"))
    (when-let ((body (oref rs body)))
      (insert "\n" body))
    (verb--buffer-string-no-properties)))

(cl-defmethod verb-response-to-string ((resp verb-response) buf)
  "Return HTTP response RESP as a string.
BUF is the response buffer corresponding to this response object.  The
buffer must contain the response's processed body."
  (with-temp-buffer
    (insert (oref resp status) "\n")
    (verb--insert-header-contents (oref resp headers))
    (insert "\n")
    (when (oref resp body)
      (insert "\n")
      (insert-buffer-substring buf))
    (verb--buffer-string-no-properties)))

(defun verb--timeout-warn (buffer rs num)
  "Warn the user about a possible network timeout for request RS.
This function should be run `verb-show-timeout-warning' seconds after
an HTTP request has been sent.  Show the warning only when response
buffer BUFFER is live.  NUM is the request's identification number."
  (verb--log num 'W "%s" "Request is taking longer than expected.")
  (when (buffer-live-p buffer)
    (message "Request to %s is taking longer than expected"
	     (verb-request-spec-url-to-string rs))))

(defun verb--string= (s1 s2)
  "Return non-nil if strings S1 and S2 are equal, ignoring case."
  (string= (downcase s1) (downcase s2)))

(defun verb--override-alist (original other &optional case-fold)
  "Override alist ORIGINAL with OTHER.
That is; overwrite (KEY . VALUE) pairs present in ORIGINAL with ones
present in OTHER if KEYs are equal.  Return the results in a new
alist.  If CASE-FOLD is non-nil, ignore case when comparing KEYs."
  (let ((result (nreverse (copy-alist original)))
	(processed))
    (dolist (key-value other)
      (let ((key (car key-value)))
	(when (and (assoc-string key result case-fold)
		   (not (funcall (if case-fold
				     #'member-ignore-case
				   #'member)
				 key processed)))
	  ;; key in OTHER is in ORIGINAL, delete all entries using
	  ;; this key in ORIGINAL (may be more than one)
	  (setq result (cl-delete key result
				  :key #'car
				  :test (if case-fold
					    #'verb--string=
					  #'string=))))
	(push key-value result)
	;; Remember we deleted this key from ORIGINAL so that we don't
	;; do it again accidentally (this can happen if OTHER contains
	;; multiple values mapped to the same key)
	(push key processed)))
    (nreverse result)))

(defalias 'verb--override-url-queries #'verb--override-alist
  "Override query string alist ORIGINAL with OTHER.
Return the results in a new alist.  Work using the rules described in
`verb-request-spec-override'.")

(defun verb--override-headers (original other)
  "Override headers alist ORIGINAL with OTHER.
Return the results in a new alist.  Work using the rules described in
`verb-request-spec-override'.  Note that case is ignored when comparing
header keys (names)."
  (verb--override-alist original other t))

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
described in `verb-request-spec-override'."
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
Do this using the rules described in `verb-request-spec-override'."
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

(cl-defmethod verb-request-spec-override ((original verb-request-spec) other)
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

metadata

  Always use OTHER's metadata.

Neither request specification is modified, a new one is returned."
  (unless (object-of-class-p other 'verb-request-spec)
    (user-error "%s" "Argument OTHER must be a `verb-request-spec'"))
  (verb-request-spec :method (or (oref other method)
				 (oref original method))
		     :url (verb--override-url (oref original url)
					      (oref other url))
		     :headers (verb--override-headers (oref original headers)
						      (oref other headers))
		     :body (or (oref other body) (oref original body))
		     :metadata (oref other metadata)))

(defun verb--http-methods-regexp ()
  "Return a regexp to match an HTTP method.
HTTP methods are defined in `verb--http-methods'.
Additionally, allow matching `verb--template-keyword'."
  (let ((terms (append verb--http-methods
		       (mapcar #'downcase verb--http-methods)
		       (list verb--template-keyword
			     (downcase verb--template-keyword)))))
    (mapconcat #'identity terms "\\|")))

(defun verb--eval-string (s &optional context)
  "Eval S as Lisp code and return the result.
When evaluating the code, use buffer CONTEXT as the current buffer.
If CONTEXT is nil, use the already current buffer as context.  As a
special case, if S is the empty string, return the empty string."
  (if (string-empty-p s)
      ""
    (save-mark-and-excursion
      (save-match-data
	(with-current-buffer (or context (current-buffer))
	  (eval (car (read-from-string (format "(progn %s)" s))) t))))))

(defun verb--eval-code-tags-in-buffer (buf context)
  "Evalue code tags within buffer BUF.
When evaluating the code, use buffer CONTEXT as the current buffer.
Replace the code tags with the results of their own evaluations.  Code
tags are delimited with `verb-code-tag-delimiters'."
  (with-current-buffer buf
    (while (re-search-forward (concat (car verb-code-tag-delimiters)
				      "\\(.*?\\)"
				      (cdr verb-code-tag-delimiters))
			      nil t)
      (let ((result (verb--eval-string (match-string 1) context)))
	(cond
	 ((stringp result)
	  (replace-match result))
	 ((bufferp result)
	  (goto-char (match-beginning 0))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert-buffer-substring result)
	  (when (buffer-local-value 'verb-kill-this-buffer result)
	    (kill-buffer result)))
	 (t
	  (replace-match (format "%s" result))))))))

(defun verb--eval-code-tags-in-string (s &optional context)
  "Like `verb--eval-code-tags-in-buffer', but in a string S.
Use buffer CONTEXT as the current buffer, or a temporal one if CONTEXT
is nil.  Return a new string with the code tags expanded."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (verb--eval-code-tags-in-buffer (current-buffer)
				    (or context (current-buffer)))
    (verb--buffer-string-no-properties)))

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

(define-error 'verb-empty-spec
  "Request specification has no contents.")

(defun verb-request-spec-from-string (text &optional metadata)
  "Create and return a request specification from string TEXT.

The text format for request specifications is the following:

[COMMENT]...
METHOD [URL]
[HEADER]...

[BODY]

Each COMMENT must start with \"#\" or \":\" (see Org mode comments and
headline property syntax).  All comments will be discarded after being
read (they are not part of the returned value).  COMMENT may also be a
blank line.

METHOD must be a method matched by `verb--http-methods-regexp' (that
is, an HTTP method or the value of `verb--template-keyword').
Matching is case-insensitive.

URL must be a full URL, or a part of it.  If present, the schema must
be \"http\" or \"https\".  If the schema is not present, the URL will
be interpreted as a path, plus (if present) query string and fragment.
Therefore, using just \"example.org\" (note no schema present) as URL
will result in a URL with its path set to \"example.org\", not as its
host.

Each HEADER must be in the form of KEY: VALUE.  KEY must be a nonempty
string, VALUE can be the empty string.  HEADER may also start with
\"#\", in which case it will be ignored.

BODY can contain arbitrary data.  Note that there must be a blank
line between the HEADER list and BODY.

As a special case, if the text specification consists exclusively of
comments and/or whitespace, or is the empty string, signal
`verb-empty-spec'.

If TEXT does not conform to the request specification text format,
signal an error.

Before returning the request specification, set its metadata to
METADATA."
  (let ((context (current-buffer))
	method url headers body)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))

      ;; Skip initial blank lines, comments and properties
      (while (and (re-search-forward "^\\s-*\\(\\(:\\|#\\).*\\)?$"
				     (line-end-position) t)
		  (not (eobp)))
	(forward-char))
      ;; Check if the entire specification was just comments or empty
      (when (string-empty-p (string-trim (buffer-substring (point)
							   (point-max))))
	;; Signal `verb-empty-spec' if so
	(signal 'verb-empty-spec nil))

      ;; Read HTTP method and URL line
      ;; First, expand any code tags on it (if any)
      (let ((case-fold-search t)
	    (line (verb--eval-code-tags-in-string
		   (buffer-substring-no-properties (point)
						   (line-end-position))
		   context)))
	(if (string-match (concat "^\\s-*\\("
				  (verb--http-methods-regexp)
				  "\\)\\s-+\\(.+\\)$")
			  line)
	    ;; Matched method + URL, store them
	    (setq method (upcase (match-string 1 line))
		  url (match-string 2 line))
	  (when (string-match (concat "^\\s-*\\("
				      (verb--http-methods-regexp)
				      "\\)\\s-*$")
			      line)
	    ;; Matched method only, store it
	    (setq method (upcase (match-string 1 line))))))

      ;; We've processed the URL line, move to the end of it
      (end-of-line)

      (if method
	  (when (string= method verb--template-keyword)
	    (setq method nil))
	(user-error (concat "Could not read a valid HTTP method (%s)\n"
			    "Additionally, you can also specify %s "
			    "(matching is case insensitive)")
		    (mapconcat #'identity verb--http-methods ", ")
		    verb--template-keyword))

      ;; Skip newline after URL line
      (unless (eobp) (forward-char))

      ;; Search for HTTP headers, stop as soon as we find a blank line
      (while (re-search-forward "^\\(.+\\)$" (line-end-position) t)
	(let ((line (match-string 1)))
	  ;; Process line if it doesn't start with '#'
	  (unless (string-prefix-p "#" (string-trim-left line))
	    ;; Check if line matches KEY: VALUE after evaluating any
	    ;; present code tags
	    (setq line (verb--eval-code-tags-in-string line context))
	    (if (string-match "^\\s-*\\([[:alnum:]-]+\\)\\s-*:\\(.*\\)$"
			      line)
		;; Line matches, trim KEY and VALUE and store them
		(push (cons (string-trim (match-string 1 line))
			    (string-trim (match-string 2 line)))
		      headers)
	      (user-error (concat "Invalid HTTP header: \"%s\"\n"
				  "Make sure there's a blank line between"
				  " the headers and the request body")
			  line))))
	(unless (eobp) (forward-char)))
      (setq headers (nreverse headers))

      ;; Expand code tags in the rest of the buffer (if any)
      (save-excursion
	(verb--eval-code-tags-in-buffer (current-buffer) context))

      ;; Skip blank line after headers
      (unless (eobp) (forward-char))

      ;; The rest of the buffer is the request body
      (let ((rest (buffer-substring (point) (point-max))))
	;; Only read body if it isn't comprised entirely of
	;; whitespace, but if it's not and has leading/trailing
	;; whitespace, include itP
	(unless (string-empty-p (string-trim rest))
	  ;; Now we know body isn't comprised entirely of whitespace,
	  ;; check if the user wants to delete any trailing characters
	  (setq body (if verb-trim-body-end
			 (string-trim-right rest verb-trim-body-end)
		       rest))))
      ;; Return a `verb-request-spec'
      (verb-request-spec :method method
			 :url (unless (string-empty-p (or url ""))
				(verb--clean-url url))
			 :headers headers
			 :body body
			 :metadata metadata))))

(provide 'verb)
;;; verb.el ends here
