# Verb API

All hooks, variables, functions and classes starting with `verb-` but not starting with `verb--` are part of the package's public API. They are listed below. **Make sure to check each symbol's documentation for more information.**

- Hook: **verb-mode-hook** \
  Run when Verb mode is activated (e.g. when executing M-x verb-mode).
- Hook: **verb-response-body-mode-hook** \
  Run after deciding what major mode to use on a response buffer.
- Hook: **verb-response-headers-mode-hook** \
  Run after opening the response headers buffer.
- Hook: **verb-post-response-hook** \
  Hook run after receiving an HTTP response, processing its contents, and setting up the response buffer. Use this hook to add custom behaviour after receiving a response.
- Minor Mode: **verb-mode**
- Minor Mode: **verb-response-body-mode**
- Major Mode: **verb-response-headers-mode**
- Major Mode: **verb-log-mode**
- Major Mode: **ob-verb-response-mode**
- Face: **verb-http-keyword**
- Face: **verb-header**
- Face: **verb-code-tag**
- Face: **verb-json-key**
- Face: **verb-log-info**
- Face: **verb-log-warning**
- Face: **verb-log-error**
- Variable: **verb-last** \
  Stores the last received HTTP response.
- Variable: **verb-http-response** \
  Response object for this response buffer. Use this variable from the verb-post-response-hook to access request and response information and contents.
- Variable: **verb-kill-this-buffer**
- Variable: **verb-mode-map**
- Variable: **verb-response-body-mode-map**
- Variable: **verb-command-map** \
  Keymap for Verb mode commands.
- Variable: **verb-response-headers-mode-map**
- Function: **org-babel-execute:verb** *body params*
- Function: **verb-read-file** *file &optional coding-system*
- Function: **verb-stored-response** *key* \
  Retrieve a previously stored HTTP response.
- Function: **verb-json-get** *text &rest path*
- Function: **verb-headers-get** *headers name*
- Function: **verb-response-to-string** *resp buf*
- Function: **verb-request-spec-validate** *rs*
- Function: **verb-request-spec-url-to-string** *rs*
- Function: **verb-request-spec-to-string** *rs*
- Function: **verb-request-spec-from-string** *text*
- Function: **verb-request-spec-override** *original other*
- Error: **verb-empty-spec**
- Command: **verb-send-request-on-point** *where &optional arg*
- Command: **verb-send-request-on-point-other-window** *&optional arg*
- Command: **verb-send-request-on-point-other-window-stay** *&optional arg*
- Command: **verb-send-request-on-point-no-window** *&optional arg*
- Command: **verb-re-send-request**
- Command: **verb-kill-all-response-buffers** *&optional keep-windows*
- Command: **verb-export-request-on-point** *name*
- Command: **verb-export-request-on-point-verb**
- Command: **verb-export-request-on-point-curl**
- Command: **verb-kill-response-buffer-and-window** *&optional keep-window*
- Command: **verb-kill-buffer-and-window**
- Command: **verb-toggle-show-headers**
- Command: **verb-set-var** *&optional var*
- Command: **verb-view-log**
- Command: **verb-customize-group**
- Macro: **verb-var** *var &optional default*
- Class: **verb-request-spec** \
  Represents an HTTP request specification.
  - Slot: **method** \
    HTTP method to use (string).
  - Slot: **url** \
    URL where to request (url struct)
  - Slot: **headers** \
    Request headers (alist).
  - Slot: **body** \
    Request body (string).
  - Slot: **metadata** \
    User-defined request metadata (alist). Won't be included in the sent request.
- Class: **verb-response** \
  Represents an HTTP response.
  - Slot: **request** \
    Points back to the verb-request-spec instance that requested this response (verb-request-spec).
  - Slot: **headers** \
    Response headers (alist).
  - Slot: **status** \
    First line of response content, includes status code (string).
  - Slot: **duration** \
    The time it took in seconds to receive the response (float).
  - Slot: **body** \
    Response body. If response was handled using a binary handler, the string will be unibyte (string).
  - Slot: **body-bytes** \
    Number of bytes in the response body (integer).
- User Option: **verb-default-response-charset**
- User Option: **verb-default-request-charset**
- User Option: **verb-content-type-handlers**
- User Option: **verb-export-functions**
- User Option: **verb-auto-kill-response-buffers**
- User Option: **verb-auto-show-headers-buffer**
- User Option: **verb-inhibit-cookies**
- User Option: **verb-advice-url**
- User Option: **verb-enable-log**
- User Option: **verb-tag**
- User Option: **verb-show-timeout-warning**
- User Option: **verb-code-tag-delimiters**
- User Option: **verb-url-retrieve-function**
- User Option: **verb-json-max-pretty-print-size**
- User Option: **verb-babel-timeout**
- User Option: **verb-base-headers**
- User Option: **verb-trim-body-end**
