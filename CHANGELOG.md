# Verb Changelog
## **2.11.0** - 2020-05-27 (MELPA & MELPA Stable)
- If a (non-Verb) Babel source block is found inside a request body, its corresponding `#+begin_src` and `#+end_src` delimiters are erased before sending the request.
- Enabled code completion for code tags by default.
- Added support for the `Verb-Map-Request` heading property. This allows specifying a function per request, that can modify the request before it's sent.
- Properly clean up font locking and completion at point functions when `verb-mode` is disabled.
- Added `verb-send-request-on-point-no-window` to `verb-mode` mouse menu.

## **2.10.0** - 2020-05-01
- Added the `verb-json-use-mode` customizable variable. It allows users to change what mode is enabled on JSON responses.
- Renamed `verb--handler-json` to `verb-handler-json`.
- Added code completion for code tags via `completion-at-point-functions`.

## **2.9.0** - 2020-03-27
- Added support for Emacs 25.
- Removed human-readable export format as it was too similar to Verb format.
- Search for `:verb:` tag now respects the `org-use-tag-inheritance` variable.

## **2.8.2** - 2020-03-16
- Fixed error signaled when receiving a response with an unknown content type.

## **2.8.1** - 2020-03-12
- Use standard `switch-to-buffer`/`display-buffer` functions to show side buffers and pop-up buffers.
- Added `verb-send-request-on-point-no-window` command. By default, it is bound to <kbd>C-c C-r C-m</kbd>.
- Fixed error signaled when specifying an `Accept` header and using a non-ascii body at the same time.

## **2.8.0** - 2020-02-23
- All the `verb-send-request-on-point-*` commands now accept a prefix argument (<kbd>C-u</kbd>). When it is specified, the user can modify the request that was extracted from the headings hierarchy before it is sent, in a temporary buffer.
- The `verb-set-var` command can now set variable values for variables that haven't been used before.
- Changed type of `verb-content-type-handlers` elements (to lists).

## **2.7.2** - 2020-02-17
- Fixed code tags not being fontified when having more than one of them on a single line.
- Log a warning when sending a request with a body present on GET, HEAD, DELETE, etc.

## **2.7.1** - 2020-02-12
- Use `file-size-human-readable` to format response body sizes in header lines.
- Fixed error on responses with no content type.

## **2.7.0** - 2020-02-09
- Bound `verb-export-request-on-point-verb` to `C-b` in the Verb command map.
- Bound `verb-export-request-on-point-human` to `C-n` in the Verb command map.
- Variables created with `verb-var` are now buffer-local, and are not defined as global values.
- Code tags are now evaluated with the current `.org` buffer as the current buffer.
- Added `verb-headers-get` function.
- Regular expressions can now be used as keys in `verb-content-type-handlers`, to easily assign one handler to one or more content types.

## **2.6.0** - 2020-02-03
- Removed `verb-max-redirections` variable, as it wasn't working as intended. Use `url-max-redirections` instead.
- Added `default` argument to `verb-var`.
- Removed automatic addition of `charset=` value in `Content-Type` request headers.
- Added optional `coding-system` argument to `verb-read-file`.

## **2.5.0** - 2020-01-30
- Added `verb-trim-body-end` variable.
- Extended `:op send` so that it now accepts an optional, additional `get-headers` or `get-body` argument.
- Added "Customize Verb" button to the `verb-mode` mouse menu.
- Removed `verb-using-proxy` variable, as it wasn't working as intended.
- Combined `verb-binary-content-type-handlers` and `verb-text-content-type-handlers` variables into a unified one, `verb-content-type-handlers`.

## **2.4.0** - 2020-01-29
- Heading properties starting with `Verb-` will now be added to request and response objects automatically as metadata. Metadata is not included in outgoing HTTP requests.
- Added mechanism for automatically storing responses by key.
- Added the `verb-stored-response` function.
- Added the `verb-json-get` function.

## **2.3.0** - 2020-01-26
- Do not automatically add `Accept-Charset` header to requests anymore.
- Fixed `Accept` header being sent duplicated if included in a request specification.
- Log a warning to `*Verb Log*` buffer when headers might get duplicated by url.el.
- Overriding HTTP headers now ignores case (e.g. it is now possible to override `content-type` using `Content-Type`).
- Added customizable option `verb-base-headers`.

## **2.2.0** - 2020-01-24
- Renamed `verb-mode-prefix-map` to `verb-command-map` (existing `init.el` configs must be updated).
- Signal an error when using `verb-set-var` if no variables have been previously set with `verb-var`.
- Fixed `verb-re-send-request` not showing response buffer.
- Added mouse menus for `verb-mode` and `verb-response-body-mode`.

## **2.1.0** - 2020-01-24
- Verb can now be used from Babel source blocks.
- Added customizable option `verb-babel-timeout`.
- Added new major mode: `ob-verb-response-mode`.
- Verb now requires at least one heading to be defined for reading request specifications.

## **2.0.0** - 2020-01-21
- Verb is now based on Org mode instead of Outline mode.
- Verb is now a minor mode (to be enabled with Org) instead of a major mode.
- Because Verb now works on Org mode buffers, the preferred file extension is `.org` (the old `.verb` extension is no longer used).
- Added package autoloads for most user-facing commands (e.g. `verb-send-request-on-point`).
- Verb now only processes headings which have been tagged as `verb`. All other headings are ignored.
- Fixed small detail in HTTP method parsing - leaving a space between the method and the URL was optional before but now it's required.
- Fixed error when receiving an empty body marked as content type JSON.
- HTTP headers in request specifications now accept empty values.
- Fixed error when receiving HTTP headers with empty values.

## **1.4.1** - 2020-01-19
- Added new logging system, logs will be shown in the `*Verb Log*` buffer.
- Added a `verb-enable-log` customizable option.

## **1.4.0** - 2020-01-18
- Fixed Lisp code tags being evaluated in comments.
- Rebound `verb-send-request-on-point-other-window` to <kbd>C-c C-r C-s</kbd> in Verb mode.
- Added new command `verb-send-request-on-point-other-window-stay`, bound to <kbd>C-c C-r C-r</kbd> in Verb mode.
- Added new command `verb-kill-all-response-buffers`, bound to <kbd>C-c C-r C-k</kbd> in Verb mode.
- Added new command `verb-re-send-request`, bound to <kbd>C-c C-r C-f</kbd> in Verb response body mode.
- Removed `verb-headers-to-string` function.
- A blank line must now be present between request headers and body.
- Added user error messages for invalid HTTP headers.
- Allow using code tags in HTTP header names.

## **1.3.0** - 2020-01-17
- Request headers can now be commented out using `#`.
- Surrounding whitespace is now automatically removed from header names and values.
- Changed `curl` export format slightly:
  - URL is now the first argument.
  - Arguments are now separated by (escaped) newlines.
- Added `verb-export-request-on-point-curl`, `verb-export-request-on-point-verb` and `verb-export-request-on-point-human` commands.
- Bound <kbd>C-c C-r C-u</kbd> to `verb-export-request-on-point-curl` in Verb mode.
- Added `verb-set-var` command, bound to <kbd>C-c C-r C-v</kbd> in Verb mode.

## **1.2.0** - 2020-01-16
- Request specifications can now be exported to `curl`.

## **1.1.0** - 2020-01-15
- Made Lisp code tags more useful: the HTTP method for a request spec can now be determined by the result of a code tag.
- Renamed customizable variable `verb-show-headers-buffer` to `verb-auto-show-headers-buffer`.
- Added `verb-last` variable.
- Added `verb-headers-to-string` function.
- Added `verb-post-response-hook` hook.

## **1.0.0** - 2020-01-13
- Initial release.
