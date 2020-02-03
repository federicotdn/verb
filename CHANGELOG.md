# Verb Changelog
## master
- Bind `verb-export-request-on-point-verb` to `C-b` in the Verb command map.
- Bind `verb-export-request-on-point-human` to `C-n` in the Verb command map.

## **2.6.0** - 2020-02-03
- Removed `verb-max-redirections` variable, as it wasn't working as intended. Use `url-max-redirections` instead.
- Add `default` argument to `verb-var`.
- Removed automatic addition of `charset=` value in `Content-Type` request headers.
- Add optional `coding-system` argument to `verb-read-file`.

## **2.5.0** - 2020-01-30
- Added `verb-trim-body-end` variable.
- Extended `:op send` so that it now accepts an optional, additional `get-headers` or `get-body` argument.
- Added "Customize Verb" button.
- Removed `verb-using-proxy` variable, as it wasn't working as intended.
- Combined `verb-binary-content-type-handlers` and `verb-text-content-type-handlers` variables into a unified one, `verb-content-type-handlers`.

## **2.4.0** - 2020-01-29
- Heading properties starting with `Verb-` will now be added to requests (and responses) automatically as metadata.
- Added mechanism for automatically storing responses by key.
- Added the `verb-stored-response` function.
- Added the `verb-json-get` function.

## **2.3.0** - 2020-01-26
- Do not automatically add `Accept-Charset` header to requests anymore.
- Fix `Accept` header being sent duplicated if included in a request specification.
- Log a warning to `*Verb Log*` buffer when headers might get duplicated by url.el.
- Overriding HTTP headers now ignores case (e.g. it is now possible to override `content-type` using `Content-Type`).
- Added customizable option `verb-base-headers`.

## **2.2.0** - 2020-01-24
- Renamed `verb-mode-prefix-map` to `verb-command-map` (existing `init.el` configs must be updated).
- Signal an error when using `verb-set-var` if no variables have been previously set with `verb-var`.
- Fix `verb-re-send-request` not showing response buffer.
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
- Verb now only processes headlines which have been tagged as `verb`. All other headlines are ignored.
- Updated installation instructions.
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
- Remove `verb-headers-to-string` function.
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
