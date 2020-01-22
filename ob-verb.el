;;; ob-verb.el --- Babel integration for Verb  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>
;; Homepage: https://github.com/federicotdn/verb
;; Keywords: tools
;; Package-Version: 2.0.0
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

;; This file contains the necessary functions to integrate Verb with
;; Org mode's Babel.

;;; Code:
(require 'verb)

(defun org-babel-execute:verb (body _params)
  "Send the request specified by the selected Babel source block.
BODY should contain the contents of the source block, and PARAMS
should contain any parameters passed to it.  Note that Emacs will be
blocked while waiting for a response.  The timeout for this can be
configured via the `verb-babel-timeout' variable.

This function is called by `org-babel-execute-src-block'."
  (let* ((start (time-to-seconds))
	 (rs (verb--request-spec-from-babel-src-block (point) body))
	 (buf (verb--request-spec-send rs nil)))
    (while (and (eq (buffer-local-value 'verb-http-response buf) t)
		(< (- (time-to-seconds) start) verb-babel-timeout))
      (sleep-for 0.1))
    (with-current-buffer buf
      (if (eq verb-http-response t)
	  (format "(Request timed out after %.4g seconds)" (- (time-to-seconds)
							      start))
	(verb-response-to-string verb-http-response)))))

(provide 'ob-verb)
;;; ob-verb.el ends here
