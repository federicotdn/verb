# Verb Changelog
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
