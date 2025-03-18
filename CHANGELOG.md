# Verb Changelog
## **3.1.0** - 2025-03-18 (MELPA & MELPA Stable)
- Added support for `Verb-Map-Response` heading property. This allows for calling functions automatically with the HTTP response data, when it is received.
- Added the `verb-util-form-url-encode` helper function for use with `application/x-www-form-urlencoded`.
- Fixed error occuring when maxium number of redirections set by `url-max-redirections` was reached.
- Added support for `Verb-Max-Redirections` heading property.
- The `verb-headers-get` function now accepts an instance of `verb-response` as first argument.
- Added Verb-specific handler for Org's <kbd>C-c C-c</kbd> contextual key.

## **3.0.0** - 2024-12-23
Breaking changes:
- Updated behaviour for `verb-send-request-on-point-*` functions when dealing with one or more Babel source blocks under a heading. Now, the actual source block on point will be used to build the request, instead of simply the first one. See [issue #53](https://github.com/federicotdn/verb/issues/53).
- Defining a URL origin (scheme + host + port) in a request specification will now make Verb ignore all parent headings when computing what request to send. In other words, defining a new URL origin effectively creates a new tree of request specifications.
- Removed support for the `url-queue` backend.
- Removed all Emacs 25-specific code.
- Escape sequences ('\\') contained in strings resulting from evaluating code tags will not be interepreted, i.e. all contents will be taken as literal.
- Removed `verb-view-log` alias.
- Renamed `verb-show-log` to `verb-util-show-log`.
- All `verb-log-*` variables containing faces are no longer part of the package's public API.

New features / improvements:
- Added support for `Verb-Proxy` heading property.
- Improved `*Verb Log*` buffer formatting.
- Headers can now contain code tags that expand to multiple lines.
- URLs can now span multiple lines, place '\\' at the end of the URL line to continue it in the next one. Leading whitespace in the additional lines will be skipped.
- Added new function `verb-body-lf-to-crlf` designed for use with requests sending multipart data.
- Added new `Verb-Prelude` heading property, which can be used to specify Emacs Lisp or JSON contents to load variables from, before performing requests. Contents can come optionally from files, e.g. `.el` or `.json`.
- Added `verb-shell`, `verb-url` and `verb-unix-epoch` utility functions.
- Allow using Org [hyperlinks](https://orgmode.org/guide/Hyperlinks.html) in URLs, for example: `get [[http://example.com][my example]]`.

## **2.16.0** - 2024-03-02
- Fixed LF being used instead of CRLF in multipart boundaries.
- When sending a request, include the HTTP method and path in the message displayed on the minibuffer.
- The `verb-json-get` function now accepts negative integer arguments for accessing list elements.
- Changed default binding of `verb-send-request-on-point-no-window` to <kbd>C-c C-r C-<return></kbd>.
- Allow using single- or multi-line lambda expressions for `Verb-Map-Request`.
- The `verb-auto-kill-response-buffers` customizable variable can now be set to an integer. This will cause all response buffers to be killed when a request is sent, except the N most recent ones.
- Calling `verb-set-var` interactively with a prefix argument (<kbd>C-u</kbd>) will copy the variable value to the kill ring.
- The current value of a Verb variable will be shown in the minibuffer when the point is moved over a code tag containing only `(verb-var xyz)`.
- `verb-set-var` now has its own input history.
- Dropped support for Emacs 25. Emacs 26.3 is now the minimum supported version. Verb may still work partially or completely on Emacs 25, but this may change without prior warning.
- Added default binding for `verb-show-vars`: <kbd>C-c C-r C-x</kbd>.
- Added new export format `websocat`.
- Added new function `verb-export-request-on-point-websocat`.
- Removed default keybindings for all `verb-export-request-on-point-*` functions, and made export function querying faster in `verb-export-request-on-point`.
- Added additional advice to url.el to prevent it from dropping the response body when the `Content-Encoding` is set to `gzip` but the contents themselves are not actually compressed.
- Added support for using `org-babel-expand-src-block` on `verb` Babel source blocks.

## **2.15.0** - 2021-11-03
- Fixed font locking on indented Babel source blocks.
- Added `verb-part` and `verb-boundary` functions, to facilitate building requests using `multipart/form-data`.

## **2.14.0** - 2021-04-29
- Fixed error when receiving JSON responses that include `"t"` as a key.
- Allow underscores (`_`) in HTTP header names.
- Fixed <kbd>C-u C-c C-r C-r</kbd> and <kbd>C-u C-c C-r C-s</kbd> not displaying the response buffer when a response is received.
- Fixed Verb not reading parent headings when buffer has been narrowed (e.g. with `org-narrow-to-subtree`).
- When using Babel `verb` source blocks, arguments specified via the `:var` keyword can now be read using `verb-var`.
- Added new export function `verb-export-request-on-point-eww`, which uses EWW to display `GET` requests.
- Added new function `verb-re-send-request-eww` (see above).

## **2.13.1** - 2021-01-12
- Heading properties (e.g. `Verb-Store`, `Verb-Map-Request`) can now be inherited from parent headings by setting `org-use-property-inheritance` to `t`.

## **2.13.0** - 2020-11-16
- Added the `verb-default-content-type-handler` customizable variable.
- Verb will now display response buffer immediately after the request is sent to avoid window/buffer configuration being changed after the response is received, which can happen after an indeterminate amount of time. Note that this only applies to commands that display the response buffer. This change makes the behaviour of Verb more predictable.
- Made the minibuffer response status message more descriptive.

## **2.12.0** - 2020-08-02
- Added the `verb-show-vars` command. It allows users to see a listing of all currently defined variables.
- Added the `verb-unset-vars` command. It allows users to unset all currently defined variables. This implies that if the DEFAULT argument was specified for `verb-var`, that value will be used the next time the variable's value is retrieved.
- Added the `verb-show-request` command. It allows users to quickly see the corresponding request that was sent for an already received HTTP response.
- Renamed `verb-view-log` to `verb-show-log`.
- Added `verb-version` constant.

## **2.11.0** - 2020-05-27
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
