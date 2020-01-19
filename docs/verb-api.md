# Verb API

All hooks, variables, functions and classes starting with `verb-` but not starting with `verb--` are part of the package's public API. They are listed below. Make sure to check each symbol's documentation for more information.

- Hook: **verb-mode-hook** \
  Run when Verb mode is activated (e.g. when opening a `.verb` file).
- Hook: **verb-response-body-mode-hook** \
  Run after deciding what major mode to use on a response buffer.
- Hook: **verb-response-headers-mode-hook** \
  Run after opening the response headers buffer.
- Hook: **verb-post-response-hook** \
  Hook run after receiving an HTTP response, processing its contents, and setting up the response buffer. Use this hook to add custom behaviour after receiving a response.
- Major Mode: **verb-mode**
- Major Mode: **verb-response-headers-mode**
- Major Mode: **verb-log-mode**
- Minor Mode: **verb-response-body-mode**
- Face: **verb-http-keyword**
- Face: **verb-header**
- Face: **verb-comment**
- Face: **verb-code-tag**
- Face: **verb-json-key**
- Face: **verb-log-info**
- Face: **verb-log-warning**
- Face: **verb-log-error**
- Variable: **verb-last**
- Variable: **verb-http-response** \
  Response object for this response buffer. Use this variable from the verb-post-response-hook to access request and response information and contents.
- Variable: **verb-kill-this-buffer**
- Variable: **verb-mode-prefix-map**
- Variable: **verb-response-headers-mode-map**
- Function: **verb-read-file** *file*
- Function: **verb-request-spec-validate** *rs*
- Function: **verb-request-spec-url-to-string** *rs*
- Function: **verb-request-spec-to-string** *rs*
- Function: **verb-request-spec-from-string** *text*
- Function: **verb-request-spec-override** *original* *other*
- Error: **verb-empty-spec**
- Command: **verb-send-request-on-point** *where*
- Command: **verb-send-request-on-point-other-window**
- Command: **verb-send-request-on-point-other-window-stay**
- Command: **verb-re-send-request**
- Command: **verb-kill-all-response-buffers** *keep-windows*
- Command: **verb-export-request-on-point**
- Command: **verb-export-request-on-point-verb**
- Command: **verb-export-request-on-point-human**
- Command: **verb-export-request-on-point-curl**
- Command: **verb-kill-response-buffer-and-window** *keep-window*
- Command: **verb-kill-buffer-and-window**
- Command: **verb-toggle-show-headers**
- Command: **verb-set-var** *var*
- Macro: **verb-var** *var*
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
- User Option: **verb-text-content-type-handlers**
- User Option: **verb-binary-content-type-handlers**
- User Option: **verb-export-functions**
- User Option: **verb-auto-kill-response-buffers**
- User Option: **verb-auto-show-headers-buffer**
- User Option: **verb-inhibit-cookies**
- User Option: **verb-using-proxy**
- User Option: **verb-advice-url**
- User Option: **verb-enable-log**
- User Option: **verb-max-redirections**
- User Option: **verb-show-timeout-warning**
- User Option: **verb-code-tag-delimiters**
- User Option: **verb-url-retrieve-function**
- User Option: **verb-json-max-pretty-print-size**
