# Verb Changelog
## **1.3.0** - 2020-01-17
- Request headers can now be commented out using `#`.
- Surrounding whitespace is now automatically removed from header names and values.
- Changed `curl` export format slightly:
  - URL is now the first argument.
  - Arguments are now separated by (escaped) newlines.
- Added `verb-export-request-on-point-curl`, `verb-export-request-on-point-verb` and `verb-export-request-on-point-human` commands.
- Bind <kbd>C-c C-r C-u</kbd> to `verb-export-request-on-point-curl` in Verb mode.
- Added `verb-set-var` command.

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
