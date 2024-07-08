;;; verb.el --- Organize and send HTTP requests  -*- lexical-binding: t -*-

;; Copyright (C) 2023  Federico Tedin

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

;; Verb is a package that allows you to organize and send HTTP
;; requests from Emacs.  See the project's README.md file for more
;; details.

;;; Code:
(require 'org)
(require 'org-element)
(require 'ob)
(require 'eieio)
(require 'subr-x)
(require 'url)
(require 'mm-util)
(require 'json)
(require 'js)
(require 'seq)
(require 'verb-util)

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
    ("application/xhtml\\+xml" xml-mode)
    ("application/json" verb-handler-json)
    ("application/javascript" js-mode)
    ("application/css" css-mode)
    ("text/plain" text-mode)
    ;; Binary handlers
    ("application/pdf" doc-view-mode t)
    ("image/png" image-mode t)
    ("image/svg\\+xml" image-mode t)
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

(defcustom verb-default-content-type-handler '(fundamental-mode)
  "Default content type handler.
This handler is used when no appropriate handler was found in
`verb-content-type-handlers'."
  :type '(list function (choice (const :tag "Binary" t)
                                (const :tag "Text" nil))))

(defcustom verb-export-functions
  '(("verb" ?v verb--export-to-verb)
    ("curl" ?c verb--export-to-curl)
    ("eww" ?e verb--export-to-eww)
    ("websocat" ?w verb--export-to-websocat))
  "Alist of request specification export functions.
Each element should have the form (NAME . FN), where NAME should be a
user-friendly name for this function, and FN should be the function
itself.  FN should take a `verb-request-spec' object as its only
argument."
  :type '(repeat (list string character function)))

(defcustom verb-auto-kill-response-buffers nil
  "Whether to kill existing response buffers before sending a request.
Set this variable to t if you wish to have all old response buffers (named
*HTTP Response*) automatically killed when sending a new HTTP
request.
Set this variable to an integer number if you wish to have all old response
buffers killed, except the N most recent ones.
Set this variable to nil if you do not wish to have any old response buffers
killed before sending a request."
  :type '(choice (const :tag "Never" nil)
                 (integer :tag "Kill all but keep N most recent")
                 (const :tag "Kill all" t)))

(defcustom verb-inhibit-cookies nil
  "If non-nil, do not send or receive cookies when sending requests."
  :type 'boolean)

(defcustom verb-advice-url t
  "Whether to advice url.el functions or not.
If non-nil, the following url.el functions will be advised in order to
make Verb more flexible and user-friendly:
- `url-http-user-agent-string': Advised to allow the user to set their
  own \"User-Agent\" headers.
- `url-http-handle-authentication': Advised to disable annoying user
  prompt on 401 responses.
Note that the functions will be advised only during the duration of
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
received."
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
(make-obsolete-variable 'verb-url-retrieve-function
                        "this feature is no longer supported."
                        "2024-04-02")

(defcustom verb-json-max-pretty-print-size (* 1 1024 1024)
  "Max JSON file size (bytes) to automatically prettify when received.
If nil, never prettify JSON files automatically.  This variable only applies
if `verb-handler-json' is being used to handle JSON responses."
  :type '(choice (integer :tag "Max bytes")
                 (const :tag "Off" nil)))

(defcustom verb-json-use-mode #'js-mode
  "Mode to enable in response buffers containing JSON data.
This variable only applies if `verb-handler-json' is being used to
handle JSON responses."
  :type 'function)

(defcustom verb-post-response-hook nil
  "Hook run after receiving an HTTP response.
The hook is run with the response body buffer as the current buffer.
The appropriate major mode will have already been activated, and
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
  :type '(choice (regexp :tag "Custom regexp")
                 (const :tag "All whitespace" "[ \t\n\r]+")
                 (const :tag "Disable" nil)))

(defcustom verb-base-headers nil
  "Set of HTTP headers used as base when reading request specs.
These headers will be included by default in requests, but still may
be overridden by re-specifying them somewhere in the headings
hierarchy."
  :type '(alist :key-type string :value-type string))

(defcustom verb-enable-elisp-completion t
  "When set to a non-nil value, enable Lisp completion in code tags.
Completion is handled by the `verb-elisp-completion-at-point'
function.

Note the the point must be between the two code tag delimiters
\(e.g.  \"{{\" and \"}}\") for the completion function to work."
  :type 'boolean)

(defcustom verb-enable-var-preview t
  "When set to a non-nil value, enable preview of Verb variables.
A preview of the value of a Verb variable will be shown in the
minibuffer, when the point is moved over a code tag containing only
a call to `verb-var'."
  :type 'boolean)

(defcustom verb-enable-log t
  "When non-nil, log different events in the *Verb Log* buffer."
  :group :verb
  :type 'boolean)

(defcustom verb-suppress-load-unsecure-prelude-warning nil
  "When set to a non-nil, suppress warning about loading Emacs Lisp Preludes.
Loading Emacs Lisp (.el) configuration files as a prelude is
potentially unsafe, so if this setting is nil a warning prompt is
shown asking user to allow it to be loaded and evaluated.  If non-nil,
no warning will be shown when loading Emacs Lisp external files."
  :type 'boolean)

(defface verb-http-keyword '((t :inherit font-lock-constant-face
                                :weight bold))
  "Face for highlighting HTTP methods.")

(defface verb-header '((t :inherit font-lock-constant-face))
  "Face for highlighting HTTP headers.")

(defface verb-code-tag '((t :inherit italic))
  "Face for highlighting Lisp code tags.")

(defface verb-json-key '((t :inherit font-lock-doc-face))
  "Face for highlighting JSON keys.")

(defconst verb--http-methods '("GET" "POST" "DELETE" "PUT"
                               "OPTIONS" "HEAD" "PATCH"
                               "TRACE" "CONNECT")
  "List of valid HTTP methods.")

(defconst verb--bodyless-http-methods '("GET" "HEAD" "DELETE" "TRACE"
                                        "OPTIONS" "CONNECT")
  "List of HTTP methods which usually don't include bodies.")

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

(defconst verb--http-header-regexp "^\\s-*\\([[:alnum:]_-]+:\\).*$"
  "Regexp for font locking HTTP headers.")

(defconst verb--metadata-prefix "verb-"
  "Prefix for Verb metadata keys in heading properties.
Matching is case insensitive.")

(defconst verb-version "2.16.0"
  "Verb package version.")

(defconst verb--multipart-boundary-alphabet
  (concat "abcdefghijklmnopqrstuvwxyz"
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          "0123456789")
  "Valid characters for multipart form boundaries.")

(defconst verb--multipart-boundary-length 64
  "Number of characters per multipart form boundary.")

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
result.  If the buffer-local value of this variable is non-nil for
that buffer, Verb will kill it after it has finished reading its
contents.")

(defvar-local verb--multipart-boundary nil
  "Current multipart form boundary available for use in specs.")

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

(defvar verb--set-var-hist nil
  "Input history for `verb-set-var'.")

(defvar verb--requests-count 0
  "Number of HTTP requests sent in the past.")

(defvar-local verb--response-number nil
  "The number of this particular HTTP response buffer.")
(put 'verb--response-number 'permanent-local t)

(defvar verb--inhibit-code-tags-evaluation nil
  "When non-nil, do not evaluate code tags in requests specs.
This variable is used mostly to parse and then copy request specs to
other buffers without actually expanding the embedded code tags.")

(defvar verb--elisp-completion-buffer nil
  "Auxiliary buffer for performing completion for Lisp code.")

;;;###autoload
(defvar verb-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") #'verb-send-request-on-point-other-window)
    (define-key map (kbd "C-r") #'verb-send-request-on-point-other-window-stay)
    (define-key map (kbd "C-<return>") #'verb-send-request-on-point-no-window)
    (define-key map (kbd "C-f") #'verb-send-request-on-point)
    (define-key map (kbd "C-k") #'verb-kill-all-response-buffers)
    (define-key map (kbd "C-e") #'verb-export-request-on-point)
    (define-key map (kbd "C-v") #'verb-set-var)
    (define-key map (kbd "C-x") #'verb-show-vars)
    map)
  "Keymap for `verb-mode' commands.
Bind this to an easy-to-reach key in Org mode in order to use Verb
comfortably.  All commands listed in this keymap automatically enable
`verb-mode' in the current buffer when used.")

(defun verb--setup-font-lock-keywords (&optional remove)
  "Configure font lock keywords for `verb-mode'.
If REMOVE is nil, add the necessary keywords to
`font-lock-keywords'.  Otherwise, remove them."
  (funcall
   (if remove #'font-lock-remove-keywords #'font-lock-add-keywords)
   nil
   `(;; GET
     (,(concat "^\\s-*\\(" (verb--http-methods-regexp) "\\)$")
      (1 'verb-http-keyword))
     ;; GET www.example.com
     (,(concat "^\\s-*\\(" (verb--http-methods-regexp) "\\)\\s-+.+$")
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
        ["Send request on other window & switch"
         verb-send-request-on-point-other-window]
        ["Send request on other window"
         verb-send-request-on-point-other-window-stay]
        ["Send request without showing response"
         verb-send-request-on-point-no-window]
        "--"
        ["Kill response buffers" verb-kill-all-response-buffers]
        "--"
        ["Set variable value" verb-set-var]
        ["Show all variable values" verb-show-vars]
        ["Unset all variables" verb-unset-vars]
        "--"
        ["Export request to curl" verb-export-request-on-point-curl]
        ["Export request to Verb" verb-export-request-on-point-verb]
        ["Export request to EWW" verb-export-request-on-point-eww]
        ["Export request to websocat" verb-export-request-on-point-websocat]
        "--"
        ["Customize Verb" verb-customize-group]
        ["Show log" verb-util-show-log]))
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
  (if verb-mode
      ;; Enable verb-mode.
      (progn
        (verb--setup-font-lock-keywords)
        (when verb-enable-elisp-completion
          (add-hook 'completion-at-point-functions
                    #'verb-elisp-completion-at-point
                    nil 'local))
        (add-hook 'post-command-hook #'verb--var-preview nil t)
        (when (buffer-file-name)
          (verb-util--log nil 'I
                          "Verb mode enabled in buffer: %s"
                          (buffer-name))
          (verb-util--log nil 'I "Verb version: %s" verb-version)
          (verb-util--log nil 'I "Org version: %s, GNU Emacs version: %s"
                          (org-version)
                          emacs-version)))
    ;; Disable verb-mode.
    (verb--setup-font-lock-keywords t)
    (remove-hook 'completion-at-point-functions
                 #'verb-elisp-completion-at-point
                 'local)))

(defvar verb-response-headers-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define verb-mode-menu map
      "Menu for Verb response headers mode"
      '("Verb"
        ["Hide response headers" verb-kill-buffer-and-window]))
    (define-key map (kbd "q") 'verb-kill-buffer-and-window)
    map)
  "Keymap for `verb-response-headers-mode'.")

(define-derived-mode verb-response-headers-mode special-mode "Verb[Headers]"
  "Major mode for displaying an HTTP response's headers."
  (font-lock-add-keywords
   nil `(;; Key: Value
         (,verb--http-header-regexp
          (1 'verb-header)))))

(defun verb--http-method-p (m)
  "Return non-nil if M is a valid HTTP method."
  (member m verb--http-methods))

(defun verb--alist-p (l)
  "Return non-nil if L is an alist."
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

(cl-deftype verb--http-method-type ()
  '(or null (satisfies verb--http-method-p)))

(cl-deftype verb--alist-type ()
  '(or null (satisfies verb--alist-p)))

(cl-deftype verb--http-headers-type ()
  '(or null (satisfies verb--http-headers-p)))

(defclass verb-request-spec ()
  ((method :initarg :method
           :initform nil
           :type verb--http-method-type
           :documentation "HTTP method.")
   (url :initarg :url
        :initform nil
        :type (or null url)
        :documentation "Request URL.")
   (headers :initarg :headers
            :initform ()
            :type verb--http-headers-type
            :documentation "HTTP headers.")
   (body :initarg :body
         :initform nil
         :type (or null string)
         :documentation "Request body.")
   (metadata :initarg :metadata
             :initform nil
             :type verb--alist-type
             :documentation "User-defined request metadata."))
  "Represents an HTTP request to be made.")

(defclass verb-response ()
  ((request :initarg :request
            :type verb-request-spec
            :documentation "Corresponding request.")
   (headers :initarg :headers
            :type verb--http-headers-type
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
    (define-key map (kbd "C-c C-r C-w") #'verb-re-send-request-eww)
    (define-key map (kbd "C-c C-r C-s") #'verb-show-request)
    (easy-menu-define verb-response-body-mode-menu map
      "Menu for Verb response body mode"
      '("Verb[Body]"
        ["Toggle show response headers" verb-toggle-show-headers]
        ["Kill buffer and window" verb-kill-response-buffer-and-window]
        "--"
        ["Re-send request" verb-re-send-request]
        ["Re-send request with EWW" verb-re-send-request-eww]
        ["Show corresponding request" verb-show-request]))
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

(defun verb-customize-group ()
  "Show the Customize menu buffer for the Verb package group."
  (interactive)
  (customize-group "verb"))

(defun verb--var-preview ()
  "Preview the value of a Verb variable in the minibuffer."
  (when-let* (((not (current-message)))
              (verb-enable-var-preview)
              ((not (string= (buffer-substring (line-beginning-position)
                                               (1+ (line-beginning-position)))
                             "#")))
              ;; Get the contents inside the code tag: {{<content>}}.
              (beg (save-excursion
                     (when (search-backward (car verb-code-tag-delimiters)
                                            (line-beginning-position) t)
                       (match-end 0))))
              (end (save-excursion
                     (when (search-forward (cdr verb-code-tag-delimiters)
                                           (line-end-position) t)
                       (match-beginning 0))))
              (code (buffer-substring-no-properties beg end))
              (form (car (ignore-errors (read-from-string code))))
              ((eq (car form) 'verb-var))
              (var (cadr form))
              (val (assq var verb--vars)))
    (let ((message-log-max nil))
      (message "Current value for %s: %s" (car val) (cdr val)))))

(defun verb-elisp-completion-at-point ()
  "Completion at point function for Lisp code tags."
  (when-let (;; Get the contents inside the code tag: {{<content>}}.
             (beg (save-excursion
                    (when (search-backward (car verb-code-tag-delimiters)
                                           (line-beginning-position) t)
                      (match-end 0))))
             (end (save-excursion
                    (when (search-forward (cdr verb-code-tag-delimiters)
                                          (line-end-position) t)
                      (match-beginning 0)))))
    ;; Set up the buffer where we'll run `elisp-completion-at-point'.
    (unless verb--elisp-completion-buffer
      (setq verb--elisp-completion-buffer
            (get-buffer-create " *verb-elisp-completion*")))
    ;; Copy the contents of the code tag to the empty buffer, run
    ;; completion there.
    (let* ((code (buffer-substring-no-properties beg end))
           (point-offset (1+ (- (point) beg)))
           (completions (with-current-buffer verb--elisp-completion-buffer
                          (erase-buffer)
                          (insert code)
                          (goto-char point-offset)
                          (elisp-completion-at-point))))
      ;; The beginning/end positions will belong to the other buffer, add
      ;; `beg' so that they make sense on the original one.
      (when completions
        (append (list (+ (nth 0 completions) beg -1)
                      (+ (nth 1 completions) beg -1))
                (cddr completions))))))

(defun verb--ensure-org-mode ()
  "Ensure `org-mode' is enabled in the current buffer."
  (unless (derived-mode-p 'org-mode)
    (org-mode)))

(defun verb--ensure-verb-mode ()
  "Ensure `verb-mode' is enabled in the current buffer."
  (unless verb-mode
    (verb-mode)))

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
        (;; Path element is a string (or symbol).
         (or (stringp key) (symbolp key))
         (when (symbolp key)
           (setq key (symbol-name key)))
         ;; Obj may be an alist, plist or hash-table.
         (pcase json-object-type
           ('alist
            (cdr (assoc-string key obj)))
           ('plist
            (plist-get obj (intern (concat ":" key))))
           ('hash-table
            (gethash key obj))
           (_
            (user-error "%s" "Unknown value for `json-object-type'"))))
        (;; Path element is an integer.
         (integerp key)
         ;; Handle negative indexes by adding the negative index to the size of
         ;; the sequence, and using that as the new index.
         (when (and (< key 0) (seqp obj))
           (setq key (+ key (length obj))))
         ;; Obj may be a list or a vector.
         (pcase json-array-type
           ('list
            (nth key obj))
           ('vector
            (aref obj key))
           (_
            (user-error "%s" "Unknown value for `json-array-type'"))))
        (;; Invalid key type.
         t
         (user-error "Invalid key: %s" key)))))
    obj))

(defun verb--buffer-string-no-properties ()
  "Return the contents of the current buffer as a string.
Do not include text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun verb--back-to-heading ()
  "Move to the previous heading.
Or, move to beginning of this line if it's a heading.  If there are no
headings, move to the beginning of buffer.  Return t if a heading was
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
  (when-let ((tags (org-entry-get (point) "ALLTAGS")))
    (seq-filter (lambda (s) (or org-use-tag-inheritance
                                (not (get-text-property 0 'inherited s))))
                (split-string tags ":" t))))

(defun verb--heading-properties (prefix)
  "Return alist of current heading properties starting with PREFIX.
Respects `org-use-property-inheritance'.  Matching is case-insensitive."
  (verb--back-to-heading)
  (thread-last
    ;; 1) Get all doc properties and filter them by prefix. This will push the
    ;; property already `upcase''d, but only if there is no `string=' the
    ;; `upcase''d value.
    (seq-reduce (lambda (properties property)
                  (if (string-prefix-p prefix property t)
                      (cl-pushnew (upcase property) properties :test #'string=)
                    properties))
                (org-buffer-property-keys)
                '())
    ;; 2) Get the value for each of those properties and return an alist
    ;; Note: this will respect the value of `org-use-property-inheritance'.
    (mapcar (lambda (key) (cons key (org-entry-get (point) key 'selective))))
    ;; 3) Discard all (key . nil) elements in the list.
    (seq-filter #'cdr)))

(defun verb--heading-contents (&optional point)
  "Return text under the current heading, with some conditions.
If one or more Babel source blocks are present in the text, and the
point is located inside one of them, return the content of that source
block.  Otherwise, simply return the all the text content under the
current heading.
Additionally, assume point was at position POINT before it was moved
to the heading.
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
    (when (< end start) (setq end start))
    (verb--maybe-extract-babel-src-block point start end)))

(defun verb--maybe-extract-babel-src-block (point start end)
  "Return the text between START and END, with some exceptions.
If there are one or more Babel source blocks within the text, and the
position POINT lies within one of these blocks, return that block's
text contents.
If POINT is nil, set it to START.  Also, clamp POINT between START and
END."
  (unless point (setq point start))
  (when (< point start) (setq point start))
  (when (< end point) (setq point end))
  (save-excursion
    (save-match-data
      (goto-char point)
      (let ((case-fold-search t)
            block-start)
        (when (re-search-backward "#\\+begin_src\\s-+verb" start t)
          ;; Found the start.
          (end-of-line)
          (forward-char)
          (setq block-start (point))
          (goto-char point)
          (when (re-search-forward "#\\+end_src" end t)
            ;; Found the end.
            (beginning-of-line)
            (backward-char)
            (setq start block-start)
            (setq end (point))))
        (buffer-substring-no-properties start end)))))

(defun verb--request-spec-from-heading (point)
  "Return a request spec from the current heading's text contents.
If a heading is found, get its contents using
`verb--heading-contents'.  From that result, try to parse a request
specification.  Return nil if the heading has no text contents, if
contains only comments, or if the heading does not have the tag
`verb-tag'.
Additionally, assume point was at position POINT before it was moved
to the heading.
If not on a heading, signal an error."
  (unless (org-at-heading-p)
    (user-error "%s" "Can't read request spec: not at a heading"))
  (when (or (member verb-tag (verb--heading-tags))
            (eq verb-tag t))
    (let ((text (verb--heading-contents point))
          (metadata (verb--heading-properties verb--metadata-prefix)))
      (unless (string-empty-p text)
        (condition-case nil
            (verb-request-spec-from-string text metadata)
          (verb-empty-spec nil))))))

(defun verb--request-spec-from-babel-src-block (pos body vars)
  "Return a request spec generated from a Babel source block.
BODY should contain the body of the source block.  POS should be a
position of the buffer that lies inside the source block.  VARS should
be an alist of argument names and values that should be temporarily
added to the values available through `verb-var'.

Note that the entire buffer is considered when generating the request
spec, not only the section contained by the source block.

This function is called from ob-verb.el (`org-babel-execute:verb')."
  (verb-load-prelude-files-from-hierarchy)
  (save-excursion
    (goto-char pos)
    (let* ((verb--vars (append vars verb--vars))
           (metadata (verb--heading-properties verb--metadata-prefix))
           (rs (verb-request-spec-from-string body metadata)))
      ;; Go up one level first, if possible. Do this to avoid
      ;; re-reading the request in the current level (contained in the
      ;; source block). If no more levels exist, skip the call to
      ;; `verb--request-spec-from-hierarchy'.
      (if (verb--up-heading)
          ;; Continue reading requests from the headings
          ;; hierarchy. Pre-include the one we read from the source block
          ;; at the end of the list.
          (verb--request-spec-from-hierarchy rs)
        (verb--request-spec-post-process rs)))))

(defun verb--object-of-class-p (obj class)
  "Return non-nil if OBJ is an instance of CLASS.
CLASS must be an EIEIO class."
  (ignore-errors
    (object-of-class-p obj class)))

(defun verb--try-read-fn-form (form)
  "Try `read'ing FORM and throw error if failed."
  (condition-case _err (read form)
    (end-of-file (user-error "`%s' is a malformed expression" form))))

(defun verb--request-spec-metadata-get (rs key)
  "Get the metadata value under KEY for request spec RS.
If no value is found under KEY, or if the value associated is the
empty string, return nil.  KEY must NOT have the prefix
`verb--metadata-prefix' included."
  (thread-first
    (concat verb--metadata-prefix key)
    (assoc-string (oref rs metadata) t)
    cdr
    verb-util--nonempty-string))

(defun verb--request-spec-post-process (rs)
  "Validate and prepare request spec RS to be used.

The following checks/preparations are run:
1) Check if `verb-base-headers' needs to be applied.
2) Apply request mapping function, if one was specified.
3) Run validations with `verb-request-spec-validate'.

After that, return RS."
  ;; Use `verb-base-headers' if necessary.
  (when verb-base-headers
    (setq rs (verb-request-spec-override
              (verb-request-spec :headers verb-base-headers
                                 :url (oref rs url))
              rs)))
  ;; Apply the request mapping function, if present.
  (when-let ((form (verb--request-spec-metadata-get rs "map-request"))
             (fn (verb--try-read-fn-form form)))
    (if (functionp fn)
        (setq rs (funcall fn rs))
      (user-error "`%s' is not a valid function" fn))
    (unless (verb--object-of-class-p rs 'verb-request-spec)
      (user-error (concat "Request mapping function `%s' must return a "
                          "`verb-request-spec' value")
                  fn)))
  ;; Validate and return.
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
  ;; Load all prelude verb-var's before rest of the spec to be complete, unless
  ;; specs already exists which means called from ob-verb block and loaded.
  (unless specs
    (verb-load-prelude-files-from-hierarchy))
  (let ((p (point))
        done final-spec)
    (save-restriction
      (widen)
      (save-excursion
        ;; First, go back to the current heading, if possible. If no
        ;; heading is found, then don't attempt to read anything.
        (setq done (not (verb--back-to-heading)))
        ;; If there's at least one heading above us, go up through the
        ;; headings tree taking a request specification from each level.
        (while (not done)
          (let ((spec (verb--request-spec-from-heading p)))
            (when spec (push spec specs)))
          (setq done (not (verb--up-heading))))))
    (if specs
        (progn
          (setq final-spec (car specs))
          (when (< 1 (length specs))
            (dolist (spec (cdr specs))
              ;; Override spec 1 with spec 2, and the result with spec
              ;; 3, then with 4, etc.
              (setq final-spec (verb-request-spec-override final-spec
                                                           spec))))
          ;; Process and return.
          (verb--request-spec-post-process final-spec))
      (user-error (concat "No request specifications found\n"
                          "Remember to tag your headlines with :%s:")
                  verb-tag))))

(defun verb-load-prelude-files-from-hierarchy ()
  "Load all Verb-Prelude's of current heading and up, including buffer level.
Children with same named verb-vars as parents, will override the parent
settings."
  (save-restriction
    (widen)
    (save-excursion
      (let (preludes)
        (while
            (progn
              (let* ((spec (verb-request-spec
                            :metadata (verb--heading-properties
                                       verb--metadata-prefix)))
                     (prelude (verb--request-spec-metadata-get spec
                                                               "prelude")))
                (when prelude
                  (push prelude preludes)))
              (verb--up-heading)))
        (let* ((prelude (car (org-element-map (org-element-parse-buffer)
                                 'keyword
                               (lambda (keyword)
                                 (when (string= (upcase (concat
                                                         verb--metadata-prefix
                                                         "prelude"))
                                                (org-element-property
                                                 :key keyword))
                                   (org-element-property :value keyword)))))))
          (when prelude
            (push prelude preludes)))
        ;; Lower-level prelude files override same settings in hierarchy
        (dolist (file preludes)
          (verb-load-prelude-file file))))))

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

(defun verb--check-response-buffer ()
  "Ensure that the current buffer is a response buffer."
  (unless (verb--object-of-class-p verb-http-response 'verb-response)
    (user-error "%s" (concat "Can't execute command as current buffer is not "
                             "a response buffer"))))

(defun verb-re-send-request ()
  "Re-send request for the response shown on current buffer.
If the user chose to show the current response buffer on another
window, show the new one on another window as well.  Return the buffer
where the response will be loaded in.

If you use this command frequently, consider setting
`verb-auto-kill-response-buffers' to t.  This will help avoiding
having many response buffers open."
  (interactive)
  (verb--check-response-buffer)
  (verb--request-spec-send (oref verb-http-response request)
                           'this-window))

(defun verb-re-send-request-eww ()
  "Re-send request for the response shown on current buffer with EWW.
The result will be displayed on a separate buffer managed by EWW."
  (interactive)
  (verb--check-response-buffer)
  (let ((req (oref verb-http-response request)))
    (unless (string= (oref (oref verb-http-response request) method) "GET")
      (user-error "%s" "Can only perform GET requests using EWW"))
    (verb--request-spec-send-eww req)))

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
  `(progn
     (when (stringp ',var)
       (user-error "%s (got: \"%s\")"
                   "[verb-var] Variable name must be a symbol, not a string"
                   ',var))
     (let ((val (assq ',var verb--vars)))
       (unless val
         (setq val (cons ',var
                         (or ,default
                             (read-string
                              (format "[verb-var] Set value for %s: "
                                      ',var)))))
         (push val verb--vars))
       (cdr val))))

(defun verb-set-var (&optional var value)
  "Set new value for variable VAR previously set with `verb-var'.
When called interactively, prompt the user for a variable that has
been set once with `verb-var', and then prompt for VALUE.  Otherwise,
use string VAR and value VALUE.

When called with a non-nil prefix argument, copy the current variable
value to the kill ring instead of setting it, and ignore VALUE."
  (interactive)
  (verb--ensure-verb-mode)
  (let* ((name (or (and (stringp var) var)
                   (and (symbolp var) var (symbol-name var))
                   (completing-read "Variable: "
                                    (mapcar (lambda (e)
                                              (symbol-name (car e)))
                                            verb--vars)
                                    nil nil nil
                                    'verb--set-var-hist)))
         (key (intern name))
         (val (or current-prefix-arg
                  value
                  (read-string (format "Set value for %s: " name))))
         (elem (assq key verb--vars)))
    (when (string-empty-p name)
      (user-error "%s" "Variable name can't be empty"))
    (if elem
        (if current-prefix-arg
            (progn
              (kill-new (format "%s" (cdr elem)))
              (message "Variable value copied to the kill ring"))
          (setcdr elem val))
      (if current-prefix-arg
          (user-error "%s" "Variable has no value set")
        (push (cons key val) verb--vars)))))

(defun verb-unset-vars ()
  "Unset all variables set with `verb-var' or `verb-set-var'.
This affects only the current buffer."
  (interactive)
  (verb--ensure-verb-mode)
  (when (or (not (called-interactively-p 'any))
            (yes-or-no-p "Unset all Verb variables for current buffer? "))
    (setq verb--vars nil)))

(defun verb-load-prelude-file (filename)
  "Load Emacs Lisp or JSON configuration file FILENAME into Verb variables."
  (interactive)
  (save-excursion
    (let ((file-extension (file-name-extension filename)))
      (when (member file-extension '("gpg" "gz" "z" "7z"))
        (setq file-extension (file-name-extension (file-name-base filename))))
      (cond
       ((string= "el" file-extension) ; file is Emacs Lisp
        (when (or verb-suppress-load-unsecure-prelude-warning
                  (yes-or-no-p
                   (concat (format "File %s may contain code " filename)
                           "that may not be safe\nLoad it anyways? ")))
          (load-file filename)))
       ((string-match-p "^json.*" file-extension) ; file is JSON(C)
        (let* ((file-contents
                (with-temp-buffer
                  (insert-file-contents filename)
                  (set-auto-mode)
                  (goto-char (point-min))
                  ;; If a modern JSON / JavaScript package not
                  ;; installed, then comments cannot be removed or
                  ;; supported. Also, not likely to have JSON comments
                  ;; if this is the case.
                  (when comment-start
                    (comment-kill (count-lines (point-min) (point-max))))
                  (verb--buffer-string-no-properties)))
               (json-object-type 'plist)
               (data (json-read-from-string file-contents)))
          ;; Search for values on the topmost container, and one level down.
          (cl-loop for (k v) on data by #'cddr
                   do (verb-set-var (substring (symbol-name k) 1) v)
                   if (and (listp v) (cl-evenp (length v)))
                   do (cl-loop for (subk subv) on v by #'cddr
                               do (verb-set-var
                                   (substring (symbol-name subk) 1) subv)))))
       (t (user-error "Unable to determine file type for %s" filename))))))

(defun verb-show-vars ()
  "Show values of variables set with `verb-var' or `verb-set-var'.
Values correspond to variables set in the current buffer.  Return the
buffer used to show the values."
  (interactive)
  (unless verb--vars
    (user-error "%s" "No variables are currently defined"))
  (let ((buf (current-buffer))
        (inhibit-read-only t))
    (with-current-buffer-window
        (get-buffer-create "*Verb Variables*")
        (cons 'display-buffer-below-selected
              '((window-height . fit-window-to-buffer)))
        nil
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (dolist (elem (buffer-local-value 'verb--vars buf))
        (insert (propertize (format "%s: " (car elem)) 'face 'verb-header)
                (format "%s" (cdr elem)))
        (newline))
      (unless (zerop (buffer-size))
        (delete-char -1))
      (current-buffer))))

(defun verb-read-file (file &optional coding-system)
  "Return a buffer with the contents of FILE.
If CODING-SYSTEM system is a valid coding system, use it when reading
the file contents (see `coding-system-for-read' for more information).
Set the buffer's `verb-kill-this-buffer' variable locally to t.
Additionally, add the `verb-lf-keep' property to all of the resulting
buffer's text, to prevent function `verb-body-lf-to-crlf' from
potentially modifying it."
  (with-current-buffer (generate-new-buffer " *verb-temp*")
    (buffer-disable-undo)
    (let ((coding-system-for-read coding-system))
      (insert-file-contents file))
    (add-text-properties (point-min) (point-max) '(verb-lf-keep t))
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
      (delete-char -1))))

(defun verb-show-request ()
  "Show the corresponding HTTP request for a received response.
The request will be displayed in a format very similar in which it was
written originally, on a separate buffer.  Note that code tags will be
shown with their values already evaluated.  Some of the headers sent
by url.el may also differ from the headers shown with this function,
see the project's README.md file for more information.  Return the
buffer used to show the request."
  (interactive)
  (unless verb-response-body-mode
    (user-error "%s" "This buffer is not showing an HTTP response"))

  (let ((rs (oref verb-http-response request))
        (name (buffer-name)))
    (with-current-buffer (get-buffer-create "*Show HTTP Request*")
      ;; Set up buffer.
      (verb--ensure-org-mode)
      (verb--ensure-verb-mode)
      (erase-buffer)

      ;; Insert the request contents and show the buffer.
      (insert "* Corresponding HTTP request for response in "
              name)
      (newline)
      (insert (verb-request-spec-to-string rs))
      (switch-to-buffer-other-window (current-buffer)))))

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
      (with-selected-window (display-buffer
                             verb--response-headers-buffer
                             '(display-buffer-below-selected))
        (verb-response-headers-mode)
        (setq header-line-format (format "HTTP Response Headers | count: %s"
                                         (length headers)))
        (verb--insert-header-contents headers)
        (fit-window-to-buffer)))))

;;;###autoload
(defun verb-send-request-on-point-other-window (&optional arg)
  "Send the request specified by the selected heading's text contents.
Show the results on another window and switch to it, using
`verb-send-request-on-point'.  See that function's documentation for a
description of prefix argument ARG."
  (interactive "P")
  (verb-send-request-on-point 'other-window arg))

;;;###autoload
(defun verb-send-request-on-point-other-window-stay (&optional arg)
  "Send the request specified by the selected heading's text contents.
Show the results on another window but don't switch to it, using
`verb-send-request-on-point'.  See that function's documentation for a
description of prefix argument ARG."
  (interactive "P")
  (verb-send-request-on-point 'stay-window arg))

;;;###autoload
(defun verb-send-request-on-point-no-window (&optional arg)
  "Send the request specified by the selected heading's text contents.
Do this using `verb-send-request-on-point', but do not show the
results on any window.  See that function's documentation for a
description of prefix argument ARG.

This command is useful for cases where the request is only being sent
for its side effects."
  (interactive "P")
  (verb-send-request-on-point 'minibuffer arg))

;;;###autoload
(defun verb-send-request-on-point (where &optional arg)
  "Send the request specified by the selected heading's text contents.
After the request has been sent, return the response buffer (the
buffer where the response will be loaded into).

Note that the contents of all parent headings are considered as well;
see `verb--request-spec-from-hierarchy' to see how this is done.

The buffer containing the response is shown (or not shown) in
different ways, depending on the value of WHERE:

- `other-window': Show the response buffer on another window and
  select it.
- `stay-window': Show the response buffer on another window, but
  keep the current one selected.
- `this-window': Show the response buffer in the current window.
- `minibuffer': Show the response status on the minibuffer, but don't
  show the response buffer anywhere.
- nil: Send the request but do not show the response buffer nor the
  response status anywhere.

The response buffer won't have any contents until the HTTP response
has been received.  For all valid values of WHERE except nil, the
response status will be shown on the minibuffer when the response is
received.

If prefix argument ARG is non-nil, allow the user to quickly edit the
request before it is sent.  The changes made will not affect the
contents of the current buffer and will be discarded after the request
is sent.

The `verb-post-response-hook' hook is called after a response has been
received."
  (interactive (list 'this-window current-prefix-arg))
  (verb--ensure-verb-mode)
  (when (and where
             (not (member where
                          '(other-window stay-window this-window minibuffer))))
    (user-error "Invalid value for WHERE: %s" where))
  (let* ((verb--inhibit-code-tags-evaluation arg)
         (rs (verb--request-spec-from-hierarchy)))
    (if arg
        ;; If ARG is non-nil, setup a buffer to edit the request.
        (verb--setup-temp-request-buffer rs
                                         (selected-window)
                                         verb--vars
                                         where)
      ;; If ARG is nil, just send the request.
      (verb--request-spec-send rs where))))

(defun verb--setup-temp-request-buffer (rs source-window verb-variables where)
  "Setup and show a temporary buffer for editing a request spec.
Argument RS indicates the request specification to edit.
SOURCE-WINDOW indicates which window should be selected when the
request is sent.  VERB-VARIABLES should contain the Verb user-defined
variables set in the buffer displayed by SOURCE-WINDOW.  WHERE
describes where the response should be shown in (see
`verb-send-request-on-point' for a complete description).

After the user has finished modifying the buffer, they can press
\\<org-mode-map>\\[org-ctrl-c-ctrl-c] to send the request."
  (switch-to-buffer-other-window (get-buffer-create "*Edit HTTP Request*"))
  ;; "Reset" the buffer in case it wasn't killed correctly.
  (erase-buffer)
  (verb--ensure-org-mode)
  (verb--ensure-verb-mode)

  ;; Don't require tagging for this temp buffer.
  (set (make-local-variable 'verb-tag) t)

  ;; Copy over Verb variables.
  (setq verb--vars verb-variables)

  ;; Insert the request spec.
  (insert "* Press 'C-c C-c' to send the request.\n")

  (when-let ((metadata (oref rs metadata)))
    (insert ":properties:\n")
    (dolist (element metadata)
      (insert (format ":%s: %s\n" (car element) (cdr element))))
    (insert ":end:\n"))

  (insert "# You can also press 'C-c C-k' to cancel.\n"
          "# Note that any changes made here won't be saved.\n"
          (verb-request-spec-to-string rs))

  ;; Use a copy of Org mode's keymap as the local keymap, so
  ;; that we can rebind C-c C-c freely.
  (use-local-map (copy-keymap org-mode-map))

  ;; Unbind keys for verb-send-request-on-point-* commands.
  (dolist (cmd '(verb-send-request-on-point
                 verb-send-request-on-point-other-window
                 verb-send-request-on-point-other-window-stay
                 verb-send-request-on-point-no-window))
    (when-let ((key (where-is-internal cmd nil t)))
      (local-unset-key key)))

  ;; Rebind C-c C-c to send the request.
  (local-set-key (kbd "C-c C-c")
                 (lambda ()
                   "Send the request specified in the current buffer."
                   (interactive)
                   (verb--send-temp-request-on-point source-window
                                                     where)))
  (local-set-key (kbd "C-c C-k") #'verb-kill-buffer-and-window))

(defun verb--send-temp-request-on-point (source-window where)
  "Send the request specified in the current temporary buffer.
SOURCE-WINDOW specifies which window must be selected when the request
is actually sent.  WHERE specifies where the result should be shown
in."
  (let ((new-rs (verb--request-spec-from-hierarchy)))
    (verb-kill-buffer-and-window)
    (select-window source-window)
    (verb--request-spec-send new-rs where)))

;;;###autoload
(defun verb-kill-all-response-buffers (&optional keep-windows)
  "Kill all response buffers, and delete their windows.
If KEEP-WINDOWS is non-nil, do not delete their respective windows.
If the value of `verb-auto-kill-response-buffers' is an integer,
kill all response buffers but keep the N most recent ones."
  (interactive)
  (verb--ensure-verb-mode)
  (let ((keep (if (integerp verb-auto-kill-response-buffers)
                  (max 0 verb-auto-kill-response-buffers)
                0)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and verb-http-response
                   (> (1+ (- verb--requests-count verb--response-number))
                      keep))
          (verb-kill-response-buffer-and-window keep-windows))))))

;;;###autoload
(defun verb-export-request-on-point (&optional name)
  "Export the request specification on point.
Interactively, prompt the user for an export function, and call that
function with the request specification object.  See the
`verb-export-functions' variable for more details.  If called from
Lisp, use the export function under NAME.  If NAME is nil, prompt the
user anyways.

No HTTP request is sent, unless the export function does this
explicitly.  Lisp code tags are evaluated when exporting."
  (interactive)
  (verb--ensure-verb-mode)
  (let ((rs (verb--request-spec-from-hierarchy))
        (prompt (string-join
                 (append
                  (mapcar (lambda (x)
                            (format "[%c]: Export to %s" (nth 1 x) (nth 0 x)))
                          verb-export-functions)
                  '("Choice: "))
                 "\n"))
        (choice))
    (unless name
      (setq choice (read-char-choice prompt
                                     (mapcar (lambda (x)
                                               (nth 1 x))
                                             verb-export-functions))))
    (mapc (lambda (x)
            (when (or (string= name (nth 0 x))
                      (equal choice (nth 1 x)))
              (funcall (nth 2 x) rs)
              (verb-util--log nil 'I "Exported request to %s format"
                              (nth 0 x))))
          verb-export-functions)))

;;;###autoload
(defun verb-export-request-on-point-verb ()
  "Export request on point to verb format.
See `verb--export-to-verb' for more information."
  (interactive)
  (verb-export-request-on-point "verb"))

;;;###autoload
(defun verb-export-request-on-point-curl ()
  "Export request on point to curl format.
See `verb--export-to-curl' for more information."
  (interactive)
  (verb-export-request-on-point "curl"))

;;;###autoload
(defun verb-export-request-on-point-eww ()
  "Export request on point to EWW.
See `verb--export-to-eww' for more information."
  (interactive)
  (verb-export-request-on-point "eww"))

;;;###autoload
(defun verb-export-request-on-point-websocat ()
  "Export request on point to websocat format.
See `verb--export-to-websocat' for more information."
  (interactive)
  (verb-export-request-on-point "websocat"))

(defun verb--export-to-verb (rs &optional omit-heading)
  "Export a request spec RS to Verb format.
Return a new buffer with the export results inserted into it.
If OMIT-HEADING is non-nil, do not insert an Org heading on the top."
  (with-current-buffer (generate-new-buffer "*Exported HTTP Request*")
    (verb--ensure-org-mode)
    (verb--ensure-verb-mode)
    (unless omit-heading
      (insert (format "* Exported HTTP Request  :%s:\n" verb-tag)))
    (insert (verb-request-spec-to-string rs))
    (switch-to-buffer-other-window (current-buffer))
    (current-buffer)))

(defun verb--export-to-eww (rs)
  "Export and perform GET request RS using EWW.
Return a buffer created by the `eww' function where the results will
be displayed."
  (unless (string= (oref rs method) "GET")
    (user-error "%s" "Can only perform GET requests using EWW"))
  (verb--request-spec-send-eww rs))

(defun verb--export-to-websocat (rs &optional no-message no-kill)
  "Export a request spec RS to websocat format.
Add the generated command to the kill ring and return it.  For more
information about websocat see URL `https://github.com/vi/websocat'.
If NO-MESSAGE is non-nil, do not display a message on the minibuffer.
If NO-KILL is non-nil, do not add the command to the kill ring."
  (with-temp-buffer
    (unless (string= (oref rs method) "GET")
      (user-error "%s" "Can only export GET requests to websocat"))
    (let ((url (concat "ws"
                       (string-remove-prefix
                        "http" (verb-request-spec-url-to-string rs)))))
      (insert "websocat '" url "'"))
    (dolist (key-value (oref rs headers))
      (insert " \\\n")
      (insert "-H '" (car key-value) ": " (cdr key-value) "'"))
    (let ((result (verb--buffer-string-no-properties)))
      (unless no-kill
        (kill-new result))
      (unless no-message
        (message "Websocat command copied to the kill ring"))
      ;; Return the generated command.
      result)))

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
      ;; GET: no argument needed.
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
      ;; Return the generated command.
      result)))

(cl-defmethod verb--response-header-line-string ((response verb-response))
  "Return a short description of an HTTP RESPONSE's properties."
  (let ((status-line (oref response status))
        (elapsed (oref response duration))
        (headers (oref response headers))
        (bytes (oref response body-bytes))
        (path (car (url-path-and-query (oref (oref response request) url)))))
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
               (if (= value 1) "" "s")))
     (when (not (string-empty-p path))
       (format " | %s" path)))))

(cl-defmethod verb-request-spec-url-to-string ((rs verb-request-spec))
  "Return RS's url member as a string if it is non-nil."
  (let ((url (oref rs url)))
    (when url
      (url-recreate-url url))))

(defun verb-handler-json ()
  "Standard handler for the \"application/json\" text content type."
  (when verb-json-use-mode
    (funcall verb-json-use-mode))
  (when (< (oref verb-http-response body-bytes)
           (or verb-json-max-pretty-print-size 0))
    (unwind-protect
        (unless (zerop (buffer-size))
          (let ((json-pretty-print-max-secs 0)
                (json-key-type 'string))
            (buffer-disable-undo)
            (json-pretty-print-buffer)
            ;; "Use" `json-pretty-print-max-secs' here to avoid
            ;; byte-compiler warning in Emacs 26.
            json-pretty-print-max-secs))
      (buffer-enable-undo))
    (goto-char (point-min))))

(defun verb--headers-content-type (headers)
  "Return the value of the \"Content-Type\" header in HEADERS.
The value returned has the form (TYPE . CHARSET).  If the charset is
not present, return (TYPE).  If the header itself is not present,
return (nil)."
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
See `verb--stored-responses' for more details."
  (when-let ((req (oref response request))
             (val (verb--request-spec-metadata-get req "store")))
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
                        "Make sure you've set the \"Verb-Store\""
                        " heading property and sent the request at least once")
                key)))

(defun verb--request-spec-callback (status rs response-buf start timeout-timer
                                           where num)
  "Callback for `verb--request-spec-send' for request RS.
Set up the current buffer so that it can be used to view the HTTP
response in a user-friendly way.

More response information can be read from STATUS.
RESPONSE-BUF points to a buffer where the response should be copied
to, which the user can then use or edit freely.
START contains a floating point number indicating the timestamp at
which the request was sent.
TIMEOUT-TIMER contains a timer set to call `verb--timeout-warn', or
nil.
WHERE describes where the results should be shown in (see
`verb-send-request-on-point').
NUM is this request's identification number."
  (when timeout-timer
    (cancel-timer timeout-timer))

  ;; Remove url.el advice.
  (verb--unadvice-url)
  ;; Undo proxy setup.
  (verb--undo-setup-proxy rs)

  ;; Handle errors first.
  (when-let ((http-error (plist-get status :error))
             (error-info (cdr http-error))
             (url (oref rs url)))
    ;; If there's an HTTP error code (404, 405, etc.) in the error
    ;; information, continue as normal.
    (unless (numberp (and (eq (car error-info) 'http)
                          (cadr error-info)))
      (kill-buffer (current-buffer))
      (kill-buffer response-buf)
      (let ((msg (format "Request error: could not connect to %s:%s"
                         (url-host url) (url-port url))))
        (verb-util--log num 'E msg)
        (verb-util--log num 'E "Error details: %s" http-error)
        (user-error "%s" msg))))

  ;; No errors, continue to read response.
  (let ((elapsed (- (time-to-seconds) start))
        (original-buffer (current-buffer))
        status-line headers content-type charset coding-system body-bytes
        binary-handler text-handler)

    (widen)
    (goto-char (point-min))
    ;; Skip HTTP/1.X status line.
    (setq status-line (verb-util--nonempty-string
                       (buffer-substring-no-properties (point)
                                                       (line-end-position))))

    (verb-util--log num 'I "%s" status-line)

    (forward-line)
    ;; Skip all HTTP headers.
    (while (re-search-forward verb-util--http-header-parse-regexp
                              (line-end-position) t)
      (let ((key (string-trim (match-string 1)))
            (value (string-trim (match-string 2))))
        ;; Save header to alist.
        (push (cons key value) headers)
        (unless (eobp) (forward-char))))

    ;; Read Content-Type and charset.
    (setq content-type (verb--headers-content-type headers))
    (setq charset (or (cdr content-type) verb-default-response-charset))

    ;; Try to get a buffer handler function for this content type.
    (let ((handler (verb--get-handler content-type)))
      (unless handler
        ;; Use default content type handler instead.
        (setq handler verb-default-content-type-handler))

      (if (= (length handler) 1)
          ;; Text handler.
          (setq text-handler (nth 0 handler))
        ;; Binary handler (maybe).
        (unless (and (consp handler)
                     (functionp (nth 0 handler))
                     (eq (nth 1 handler) t)
                     (= (length handler) 2))
          (user-error "Invalid content handler: %s" handler))
        (setq binary-handler (nth 0 handler))))

    ;; Remove headers and blank line from buffer.
    ;; All left should be the content.
    (beginning-of-line)
    (forward-line)
    (delete-region (point-min) (point))

    ;; Record body size in bytes.
    (setq body-bytes (buffer-size))

    ;; Current buffer should be unibyte.
    (when enable-multibyte-characters
      (error "%s" "Expected a unibyte buffer for HTTP response"))

    ;; Store details of request and response
    ;; `verb-http-response' is a permanent buffer local variable.
    (with-current-buffer response-buf
      (setq verb-http-response
            (verb-response :headers (nreverse headers)
                           :request rs
                           :status status-line
                           :duration elapsed
                           :body-bytes body-bytes))

      ;; Update global last response variable.
      (setq verb-last verb-http-response)

      ;; Store the response separately as well depending on user
      ;; metadata.
      (verb--maybe-store-response verb-http-response))

    ;; Make RESPONSE-BUF the current buffer, as we'll need to change
    ;; its major mode, coding system, etc.
    (set-buffer response-buf)

    ;; Copy bytes from `original-buffer' into it.
    (insert-buffer-substring original-buffer)

    (if binary-handler
        (progn
          ;; Response content is a binary format:
          (verb-util--log num 'I "Using binary handler: %s" binary-handler)

          (set-buffer-multibyte nil)
          (set-buffer-file-coding-system 'binary)
          (buffer-disable-undo)
          (funcall binary-handler))

      ;; else: Response content is text:
      (verb-util--log num 'I "Using text handler: %s" text-handler)

      ;; Choose corresponding coding system for charset.
      (setq coding-system (or (mm-charset-to-coding-system charset)
                              'utf-8))

      ;; Decode contents using coding system.
      (decode-coding-region (point-min) (point-max) coding-system)

      (set-buffer-file-coding-system coding-system)

      ;; Prepare buffer for editing by user.
      (funcall text-handler))

    (goto-char (point-min))

    ;; Kill original response buffer.
    (kill-buffer original-buffer)

    ;; Now that the response content has been processed, update
    ;; `verb-http-response's body slot.
    (oset verb-http-response
          body
          (unless (zerop (oref verb-http-response body-bytes))
            (verb--buffer-string-no-properties)))

    (verb-response-body-mode)

    (when where
      (message "%s | %s %s"
               (oref verb-http-response status)
               (oref rs method)
               (verb-request-spec-url-to-string rs)))

    ;; Run post response hook.
    (run-hooks 'verb-post-response-hook)))

(defun verb--prepare-http-headers (headers)
  "Prepare alist HEADERS of HTTP headers to be used on a request.
Add/modify/remove the following headers if they are not already
present or are incomplete:

Accept:
  Remove header from list (will be set via `url-mime-accept-string').

Use `verb--to-ascii' to ensure all added text is unibyte.
Return a new alist, does not modify HEADERS."
  (let* ((headers (copy-alist headers))
         (accept (assoc-string "Accept" headers t)))
    ;; Accept header:
    (when accept
      (setq headers (cl-delete "Accept" headers
                               :key #'car
                               :test #'verb-util--string=)))
    ;; Encode all text to `us-ascii'.
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

(defun verb--zlib-decompress-region (args)
  "Advice ARGS for function `zlib-decompress-region'.
The function ensures that the ALLOW-PARTIAL argument is always set to
nil when the original function is called.  Setting it to a non-nil
value will make `zlib-decompress-region' partially decompress
contents, which means it can potentially delete a response body if the
`Content-Encoding' header was incorrectly set to `gzip' when the
response body was actually not compressed."
  (list (nth 0 args) (nth 1 args)))

(defun verb--advice-url ()
  "Advice some url.el functions.
For more information, see `verb-advice-url'."
  (when verb-advice-url
    (advice-add 'url-http-user-agent-string :override
                #'verb--http-user-agent-string)
    (advice-add 'url-http-handle-authentication :override
                #'verb--http-handle-authentication)
    (when (and (fboundp 'zlib-available-p)
               (zlib-available-p))
      (advice-add 'zlib-decompress-region :filter-args
                  #'verb--zlib-decompress-region))))

(defun verb--unadvice-url ()
  "Undo advice from `verb--advice-url'."
  (when verb-advice-url
    (advice-remove 'url-http-user-agent-string
                   #'verb--http-user-agent-string)
    (advice-remove 'url-http-handle-authentication
                   #'verb--http-handle-authentication)
    (when (and (fboundp 'zlib-available-p)
	           (zlib-available-p))
      (advice-remove 'zlib-decompress-region
                     #'verb--zlib-decompress-region))))

(defun verb--setup-proxy (rs)
  "Set up any HTTP proxy configuration specified by RS."
  (when-let ((proxy (verb--request-spec-metadata-get rs "proxy")))
    (push (cons "http" proxy) url-proxy-services)))

(defun verb--undo-setup-proxy (rs)
  "Undo any HTTP proxy configuration specified by RS."
  (when-let ((proxy (verb--request-spec-metadata-get rs "proxy")))
    (setq url-proxy-services (cdr url-proxy-services))))

(defun verb--get-accept-header (headers)
  "Retrieve the value of the \"Accept\" header from alist HEADERS.
If the header is not present, return \"*/*\" as default."
  (verb--to-ascii (or (cdr (assoc-string "Accept" headers t))
                      "*/*")))

(cl-defmethod verb-request-spec-validate ((rs verb-request-spec))
  "Run validations on request spec RS and return it.
If a validation does not pass, signal `user-error'."
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
      (user-error "%s" (concat "URL has no scheme or host defined\n"
                               "Make sure you specify a scheme and host "
                               "(e.g. \"https://github.com\") in the "
                               "heading hierarchy"))))
  rs)

(defun verb--generate-response-buffer (num)
  "Return a new buffer ready to be used as a response buffer.
NUM is the request's identification number."
  (with-current-buffer (generate-new-buffer
                        (format "*HTTP Response%s*"
                                (if (eq verb-auto-kill-response-buffers t)
                                    ""
                                  (format " %s" num))))
    ;; Set `verb-http-response's value to something other than nil
    ;; so that `verb-kill-all-response-buffers' can find it even if
    ;; no response was ever received.
    (setq verb-http-response t)
    (setq verb--response-number num)
    (setq header-line-format "Waiting for HTTP response...")
    (current-buffer)))

(cl-defmethod verb--request-spec-send ((rs verb-request-spec) where)
  "Send the HTTP request described by RS.
Show the results according to argument WHERE (see
`verb-send-request-on-point').  Return the buffer the response will be
loaded into."
  ;; If auto kill buffers is enabled, kill all previous response
  ;; buffers now.
  (when verb-auto-kill-response-buffers
    (verb-kill-all-response-buffers t))

  (let* ((url (oref rs url))
         (url-request-method (verb--to-ascii (oref rs method)))
         (url-mime-accept-string (verb--get-accept-header (oref rs headers)))
         (url-request-extra-headers (verb--prepare-http-headers
                                     (oref rs headers)))
         (content-type (verb--headers-content-type
                        url-request-extra-headers))
         (url-request-data (verb--encode-http-body (oref rs body)
                                                   (cdr content-type)))
         (num (setq verb--requests-count (1+ verb--requests-count)))
         (start-time (time-to-seconds))
         (response-buf (verb--generate-response-buffer num))
         timeout-timer)
    ;; Start the timeout warning timer.
    (when verb-show-timeout-warning
      (setq timeout-timer (run-with-timer verb-show-timeout-warning nil
                                          #'verb--timeout-warn
                                          response-buf rs num)))

    ;; Advice url.el functions.
    (verb--advice-url)

    ;; Configure proxy if needed.
    (verb--setup-proxy rs)

    ;; Look for headers that might get duplicated by url.el.
    (dolist (h verb--url-pre-defined-headers)
      (when (assoc-string h url-request-extra-headers t)
        (verb-util--log num 'W (concat "Header \"%s\" will appear duplicated "
                                       "in the request, as url.el adds its "
                                       "own version of it")
                        h)))

    ;; Maybe log a warning if body is present but method usually
    ;; doesn't take one (like GET).
    (when (and (member url-request-method verb--bodyless-http-methods)
               url-request-data)
      (verb-util--log num 'W "Body is present but request method is %s"
                      url-request-method))

    ;; Send the request!
    (condition-case err
        (url-retrieve url
                      #'verb--request-spec-callback
                      (list rs
                            response-buf
                            start-time
                            timeout-timer
                            where
                            num)
                      t verb-inhibit-cookies)
      (error (progn
               ;; Cancel timer.
               (when timeout-timer
                 (cancel-timer timeout-timer)
                 (setq timeout-timer nil))
               ;; Kill response buffer.
               (kill-buffer response-buf)
               ;; Undo advice.
               (verb--unadvice-url)
               ;; Undo proxy setup.
               (verb--undo-setup-proxy rs)

               (let ((msg (format "Error sending request: %s" (cadr err))))
                 ;; Log the error.
                 (verb-util--log num 'E msg)
                 ;; Signal it.
                 (user-error "%s" msg)))))

    ;; Show user some quick information.
    (message "%s request sent to %s"
             (oref rs method)
             (verb-request-spec-url-to-string rs))

    ;; Log the request.
    (verb-util--log num 'I "%s %s"
                    (oref rs method)
                    (verb-request-spec-url-to-string rs))

    (pcase where
      ('other-window (switch-to-buffer-other-window response-buf))
      ('stay-window (save-selected-window
                      (switch-to-buffer-other-window response-buf)))
      ('this-window (switch-to-buffer response-buf)))

    ;; Return the response buffer.
    response-buf))

(cl-defmethod verb--request-spec-send-eww ((rs verb-request-spec))
  "Send request spec RS using EWW (Emacs Web Wowser).
Return the buffer created by EWW.

Note: this function is unrelated to `verb--request-spec-send'."
  ;; Require eww to load the `eww-accept-content-types' variable.
  (require 'eww)
  (let ((eww-accept-content-types (verb--get-accept-header (oref rs headers)))
        (url-request-extra-headers (verb--prepare-http-headers
                                    (oref rs headers))))
    (verb--advice-url)
    (verb--setup-proxy rs)
    (unwind-protect
        (prog1
            (eww (verb-request-spec-url-to-string rs))
          ;; "Use" the variable to avoid compiler warning.
          ;; This variable is not available in some Emacs versions.
          eww-accept-content-types)
      (verb--unadvice-url)
      (verb--undo-setup-proxy rs))))

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
  (verb-util--log num 'W "%s" "Request is taking longer than expected.")
  (when (buffer-live-p buffer)
    (message "Request to %s is taking longer than expected"
             (verb-request-spec-url-to-string rs))))

(defun verb--override-alist (original other &optional case-fold)
  "Override alist ORIGINAL with OTHER.
That is, overwrite (KEY . VALUE) pairs present in ORIGINAL with ones
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
          ;; this key in ORIGINAL (may be more than one).
          (setq result (cl-delete key result
                                  :key #'car
                                  :test (if case-fold
                                            #'verb-util--string=
                                          #'string=))))
        (push key-value result)
        ;; Remember we deleted this key from ORIGINAL so that we don't
        ;; do it again accidentally (this can happen if OTHER contains
        ;; multiple values mapped to the same key).
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
    ;; sometimes add slashes in `verb--clean-url').
    (concat (if (string-prefix-p "//" paths)
                (substring paths 1 nil)
              paths)
            (unless (string-empty-p (or queries ""))
              ;; If query string is present and path is empty,
              ;; set / as the path (see `verb--clean-url').
              (concat (when (string-empty-p paths) "/")
                      "?"
                      queries)))))

(defun verb--url-port (url)
  "Return port used by an HTTP URL.
Return nil if the port can be inferred from the URL's scheme."
  (let ((port (url-port url))
        (scheme (url-type url)))
    (if (and (numberp port)
             (or (and (= port 80) (string= scheme "http"))
                 (and (= port 443) (string= scheme "https"))))
        nil
      port)))

(defun verb--override-url (original other)
  "Override URL struct ORIGINAL with OTHER.
Do this using the rules described in `verb-request-spec-override'."
  ;; If either url is nil, return the other one.
  (if (not (and original other))
      (or original other)
    ;; Override ORIGINAL with OTHER.
    (let ((scheme (or (url-type other) (url-type original)))
          (user (or (url-user other) (url-user original)))
          (password (or (url-password other) (url-password original)))
          (host (or (url-host other) (url-host original)))
          (port (or (verb--url-port other) (verb--url-port original)))
          (path (verb--override-url-paths (url-path-and-query original)
                                          (url-path-and-query other)))
          (fragment (or (url-target other) (url-target original)))
          (attributes (or (url-attributes other) (url-attributes original)))
          (fullness (or (url-fullness other) (url-fullness original))))
      (url-parse-make-urlobj scheme user password host
                             port path fragment
                             attributes fullness))))

(cl-defmethod verb--request-spec-url-origin ((rs verb-request-spec))
  "Return the origin of RS's URL.
The URL origin consists of the scheme, user, password host and port
properties.
Return nil instead if RS has no URL, or if all of the properties
making up for the origin are themselves nil."
  (when-let ((rs)
             (url (oref rs url)))
    (let ((type (url-type url))
          (user (url-user url))
          (password (url-password url))
          (host (url-host url))
          (port (url-port url)))
      (when (or type user password host port)
        (url-parse-make-urlobj type user password
                               host port)))))

(cl-defmethod verb-request-spec-override ((original verb-request-spec) other)
  "Override request spec ORIGINAL with OTHER, return the result.
Override each member of request ORIGINAL with one from OTHER in the
following way, to form a new request specification:

method

  Use OTHER's HTTP method if it is non-nil, otherwise use ORIGINAL's.

url

  Construct a new URL using a combination of both URLs.  The new URL's
  path is a concatenation of ORIGINAL's and OTHER's paths.  The new
  URL's query string is a union of both ORIGINAL's and OTHER's query
  strings, preferring OTHER's values when both contain the same key.
  Use all other components (host, port, user, etc.) from OTHER if they
  are non-nil, or from ORIGINAL otherwise.  If either OTHER's or
  ORIGINAL's URL is nil, use the other one's without modifications.

headers

  Create a union of ORIGINAL's and OTHER's headers, using OTHER's
  values when both contain the same header.

body

  Use OTHER's body if it is non-nil, otherwise use ORIGINAL's.

metadata

  Always use OTHER's metadata.

Modify neither request specification, return a new one."
  (unless (object-of-class-p other 'verb-request-spec)
    (user-error "%s" "Argument OTHER must be a `verb-request-spec'"))
  (if (let ((original-origin (verb--request-spec-url-origin original))
            (other-origin (verb--request-spec-url-origin other)))
        (and other-origin
             (not (equal original-origin other-origin))))
      other
    (verb-request-spec :method (or (oref other method)
                                   (oref original method))
                       :url (verb--override-url (oref original url)
                                                (oref other url))
                       :headers (verb--override-headers (oref original headers)
                                                        (oref other headers))
                       :body (or (oref other body) (oref original body))
                       :metadata (oref other metadata))))

(defun verb--http-methods-regexp ()
  "Return a regexp to match an HTTP method.
HTTP methods are defined in `verb--http-methods'.
Additionally, allow matching `verb--template-keyword'."
  (let ((terms (append verb--http-methods
                       (mapcar #'downcase verb--http-methods)
                       (list verb--template-keyword
                             (downcase verb--template-keyword)))))
    (mapconcat #'identity terms "\\|")))

(defun verb--generate-multipart-boundary ()
  "Generate a new random multipart form boundary."
  (let (chars i)
    (dotimes (_ verb--multipart-boundary-length)
      (setq i (% (abs (random)) (length verb--multipart-boundary-alphabet)))
      (push (substring verb--multipart-boundary-alphabet i (1+ i)) chars))
    (mapconcat #'identity chars "")))

(defun verb-boundary (&optional boundary)
  "Set the multipart form boundary for the current buffer.
Use the value of BOUNDARY if it is non-nil.  Otherwise, generate a new
random boundary using `verb--generate-multipart-boundary'.
Once the boundary has been set for the current buffer (containing
request specifications), it can be inserted into a request body using
`verb-part'."
  (setq verb--multipart-boundary (or boundary
                                     (verb--generate-multipart-boundary))))

(defun verb-part (&optional name filename)
  "Start a new multipart form part.
Use NAME as the \"name\" parameter, and FILENAME as the \"filename\"
parameter in the Content-Disposition header.
If neither NAME nor FILENAME are specified, instead of starting a new
part, insert the final boundary delimiter."
  (unless verb--multipart-boundary
    (user-error "%s" (concat "No multipart boundary defined\n"
                             "Please ensure you have called "
                             "(verb-boundary) first within this "
                             "requests tree")))
  (if name
      (concat "--" verb--multipart-boundary "\n"
              "Content-Disposition: form-data; name=\"" name "\""
              (when filename (concat "; filename=\"" filename "\"")))
    (let (boundary)
      (setq boundary verb--multipart-boundary
            verb--multipart-boundary nil)
      (concat "--" boundary "--"))))

(defun verb-body-lf-to-crlf (rs)
  "Prepend a carriage-return before all line-feeds in RS's body.
Do this only for intervals of the string not having the `verb-lf-keep'
property."
  (let* ((body (oref rs body))
         (intervals (verb-util--object-intervals body))
         parts)
    (dolist (elem intervals)
      (let* ((start (nth 0 elem))
             (end (nth 1 elem))
             (props (nth 2 elem))
             (s (substring body start end)))
        (push (if (plist-member props 'verb-lf-keep)
                  s
                (replace-regexp-in-string "\n" "\r\n" s))
              parts)))
    (oset rs body (string-join (nreverse parts)))
    rs))

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
  "Eval code tags within buffer BUF.
When evaluating the code, use buffer CONTEXT as the current buffer.
Replace the code tags with the results of their own evaluations.  Code
tags are delimited with `verb-code-tag-delimiters'."
  (unless verb--inhibit-code-tags-evaluation
    (with-current-buffer buf
      (while (re-search-forward (concat (car verb-code-tag-delimiters)
                                        "\\(.*?\\)"
                                        (cdr verb-code-tag-delimiters))
                                nil t)
        (let ((result (verb--eval-string (match-string 1) context)))
          (cond
           ((stringp result)
            (replace-match result nil t))
           ((bufferp result)
            (goto-char (match-beginning 0))
            (delete-region (match-beginning 0) (match-end 0))
            (insert-buffer-substring result)
            (when (buffer-local-value 'verb-kill-this-buffer result)
              (kill-buffer result)))
           (t
            (replace-match (format "%s" result) nil t))))))))

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

If a scheme is not present, interpret the URL as a path, query string
and fragment component of a URL with no host or scheme defined."
  ;; If we're not expanding code tags, do not attempt to encode '{',
  ;; '}', etc., so that we keep the original URL text.
  (let* ((encoded-url (if verb--inhibit-code-tags-evaluation
                          url
                        (url-encode-url url)))
         (url-obj (url-generic-parse-url encoded-url))
         (path (url-filename url-obj))
         (scheme (url-type url-obj)))
    (if (not scheme)
        ;; If no scheme defined, interpret everything as path + query
        ;; string + fragment.
        (progn
          (setf (url-filename url-obj)
                (concat (url-host url-obj)
                        (url-filename url-obj)))
          (setf (url-host url-obj) nil))
      ;; Scheme is present:
      (unless (member scheme '("http" "https"))
        (user-error (concat "The URL must specify \"http://\" or "
                            "\"https://\" (got: \"%s\")")
                    scheme))
      ;; If path is "" but there are query string arguments, set path
      ;; to "/" (taken from curl).
      ;; Note that `path' here contains path and query string.
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

URL must be a full URL, or a part of it.  If present, the scheme must
be \"http\" or \"https\".  If the scheme is not present, the URL will
be interpreted as a path, plus (if present) query string and fragment.
Therefore, using just \"example.org\" (note no scheme present) as URL
will result in a URL with its path set to \"example.org\", not as its
host.  URL may end in a backslash, in which case the following line
will be appended to it (ignoring its leading whitespace).  The process
is repeated as long as the current line ends with a backslash.

Each HEADER must be in the form of KEY: VALUE.  KEY must be a nonempty
string, VALUE can be the empty string.  HEADER may also start with
\"#\", in which case it will be ignored.

BODY can contain arbitrary data.  Note that there must be a blank
line between the HEADER list and BODY.  If BODY contains a properly
formatted Babel source block, the block beginning and end lines will
be removed.

As a special case, if the text specification consists exclusively of
comments and/or whitespace, or is the empty string, signal
`verb-empty-spec'.

If TEXT does not conform to the request specification text format,
signal an error.

Before returning the request specification, set its metadata to
METADATA."
  (let ((context (current-buffer))
        method url headers headers-start body)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))

      ;;; COMMENTS + PROPERTIES

      ;; Skip initial blank lines, comments and properties.
      (while (and (re-search-forward "^\\s-*\\(\\(:\\|#\\).*\\)?$"
                                     (line-end-position) t)
                  (not (eobp)))
        (forward-char))
      ;; Check if the entire specification was just comments or empty.
      (when (string-empty-p (string-trim (buffer-substring (point)
                                                           (point-max))))
        ;; Signal `verb-empty-spec' if so
        (signal 'verb-empty-spec nil))

      ;;; METHOD + URL

      ;; Read HTTP method and URL line.
      ;; First, expand any code tags on it (if any):
      (let* ((case-fold-search t)
             (get-line-fn (lambda ()
                            (verb--eval-code-tags-in-string
                             (buffer-substring-no-properties
                              (point) (line-end-position))
                             context)))
             (line (funcall get-line-fn)))
        ;; Try to match:
        ;; A) METHOD URL
        ;; B) METHOD
        (if (string-match (concat "^\\s-*\\("
                                  (verb--http-methods-regexp)
                                  "\\)\\s-+\\(.+\\)$")
                          line)
            ;; A) Matched method + URL, store them.
            (progn
              (setq method (upcase (match-string 1 line))
                    url (string-remove-suffix "\\" (match-string 2 line)))

              ;; Subcase:
              ;; If URL ends with '\', append following lines to it
              ;; until one of them does not end with '\' (ignoring
              ;; leading whitespace, for alignment).
              (while (string-suffix-p "\\" line)
                (end-of-line)
                (if (eobp)
                    (user-error
                     "Backslash in URL not followed by additional line")
                  (forward-char))
                (back-to-indentation)
                (setq line (funcall get-line-fn))
                (when (string-empty-p line)
                  (user-error
                   "Backslash in URL not followed by additional content"))

                (setq url (concat url (string-remove-suffix "\\" line)))))

          (when (string-match (concat "^\\s-*\\("
                                      (verb--http-methods-regexp)
                                      "\\)\\s-*$")
                              line)
            ;; B) Matched method only, store it.
            (setq method (upcase (match-string 1 line))))))

      ;; We've processed the URL line, move to the end of it.
      (end-of-line)

      (if method
          (when (string= method verb--template-keyword)
            (setq method nil))
        (user-error (concat "Could not read a valid HTTP method (%s)\n"
                            "Additionally, you can also specify %s "
                            "(matching is case insensitive)")
                    (mapconcat #'identity verb--http-methods ", ")
                    verb--template-keyword))

      ;; Skip newline after URL line.
      (unless (eobp) (forward-char))
      (setq headers-start (point))

      ;;; HEADERS

      ;; Scan forward until we find a blank line (headers end).  In
      ;; the region covered, delete all lines starting with '#', as
      ;; these headers have been commented out.  Finally, go back to
      ;; where we started.
      (save-excursion
        (while (re-search-forward "^\\(.+\\)$" (line-end-position) t)
          (unless (eobp) (forward-char)))
        (delete-matching-lines "^[[:blank:]]*#" headers-start (point)))

      ;; Expand code tags in the rest of the buffer (if any).
      (save-excursion
        (verb--eval-code-tags-in-buffer (current-buffer) context))

      ;; Parse HTTP headers, stop as soon as we find a blank line.
      (while (re-search-forward "^\\(.+\\)$" (line-end-position) t)
        (let ((line (match-string 1)))
          ;; Check if line matches KEY: VALUE.
          (if (string-match verb-util--http-header-parse-regexp line)
              ;; Line matches, trim KEY and VALUE and store them.
              (push (cons (string-trim (match-string 1 line))
                          (string-trim (match-string 2 line)))
                    headers)
            (user-error (concat "Invalid HTTP header: \"%s\"\n"
                                "Make sure there's a blank line between"
                                " the headers and the request body")
                        line)))
        (unless (eobp) (forward-char)))
      (setq headers (nreverse headers))

      ;;; BODY

      ;; Allow users to include Babel source blocks in their request
      ;; bodies. This allows them to have font locking and other
      ;; features that depend on specific content types (e.g JSON,
      ;; XML, etc.). Here we delete the source block delimiters so
      ;; that they are not included in the actual request.
      (delete-matching-lines "^[[:blank:]]*#\\+\\(begin\\|end\\)_src")

      ;; Skip blank line after headers.
      (unless (eobp) (forward-char))

      ;; The rest of the buffer is the request body.
      (let ((rest (buffer-substring (point) (point-max))))
        ;; Only read body if it isn't comprised entirely of
        ;; whitespace, but if it's not and has leading/trailing
        ;; whitespace, include it.
        (unless (string-empty-p (string-trim rest))
          ;; Now we know body isn't comprised entirely of whitespace,
          ;; check if the user wants to delete any trailing characters.
          (setq body (if verb-trim-body-end
                         (replace-regexp-in-string
                          (concat verb-trim-body-end "$") "" rest)
                       rest))))
      (when (buffer-local-value 'verb--multipart-boundary context)
        (verb-util--log nil 'W "Detected an unfinished multipart form"))
      ;; Return a `verb-request-spec'.
      (verb-request-spec :method method
                         :url (unless (string-empty-p (or url ""))
                                (verb--clean-url url))
                         :headers headers
                         :body body
                         :metadata metadata))))

(provide 'verb)
;;; verb.el ends here
