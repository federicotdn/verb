;;; verb-ws.el --- Websocket support for Verb  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Federico Tedin

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

;; This file adds support for WebSocket connections to Verb, using
;; functions from the url.el library when possible.
;; More info at: https://datatracker.ietf.org/doc/html/rfc6455

;;; Code:

(require 'url)
(require 'verb-util)

(defconst verb-ws--key-alphabet
  (concat "abcdefghijklmnopqrstuvwxyz"
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          "0123456789")
  "TODO: Docs.")

(defconst verb-ws--key-length 16
  "TODO: Docs.")

(defconst verb-ws--version "13"
  "TODO: Docs.")

(defconst verb-ws--states '(handshake)
  "TODO: Docs.")

(defun verb-ws--state-p (s)
  "TODO: Docs S."
  (memq s verb-ws--states))

(cl-deftype verb-ws--state-type ()
  '(satisfies verb-ws--state-p))

(defclass verb-ws--conn ()
  ((url :initarg :url
        :type url
        :documentation "WebSocket URL.")
   (state :initarg :state
          :initform 'handshake
          :type verb-ws--state-type
          :documentation "Current state..")
   (connection :initarg :connection
               :type process
               :documentation "Connection to remote host.")
   (buffer :initarg :buffer
           :initform nil
           :type (or null buffer)
           :documentation "Buffer where WebSocket data is loaded into.")
   (callback :initarg :callback
             :type function
             :documentation "User-provided callback for events.")
   (cbargs :initarg :cbargs
           :type list
           :documentation "Arguments for user-provided callback."))
  "TODO: Docs.")

(defun verb-ws--get-headers (url key)
  "TODO: Docs URL KEY."
  (append url-request-extra-headers
          (list (cons "Host" (url-host url))
                (cons "Upgrade" "websocket")
                (cons "Connection" "Upgrade")
                (cons "Sec-WebSocket-Key" key)
                (cons "Sec-WebSocket-Version" verb-ws--version))))

(defun verb-ws--generate-key ()
  "TODO: Docs."
  (let (chars i)
    (dotimes (_ verb-ws--key-length)
      (setq i (% (abs (random t)) (length verb-ws--key-alphabet)))
      (push (substring verb-ws--key-alphabet i (1+ i)) chars))
    (base64-encode-string (mapconcat #'identity chars ""))))

(defun verb-ws--retrieve (url callback &optional cbargs)
  "TODO: Docs URL CALLBACK CBARGS."
  (let* ((url (if (stringp url) (url-generic-parse-url url) url))
         (connection (verb-ws--open-stream url))
         (ws (verb-ws--conn :url url
                       :connection connection
                       :callback callback
                       :cbargs cbargs))
         (recv-fn (verb-ws--recv-fn ws)))
	(set-process-filter connection recv-fn)
    (verb-ws--initialize ws)))

(defun verb-ws--recv-fn (ws)
  "TODO: Docs WS."
  (lambda (_ data)
    (verb-ws--recv-internal ws data)))

(defun verb-ws--send-fn (ws)
  "TODO: Docs WS."
  (lambda (s)
    (verb-ws--send-internal ws s)))

(cl-defmethod verb-ws--initialize ((ws verb-ws--conn))
  "TODO: Docs WS."
  (let* ((url (oref ws url))
         (path (or (verb--nonempty-string (car (url-path-and-query url)))
                   "/"))
         (key (verb-ws--generate-key)))
    (verb-ws--send-internal ws (format "GET %s HTTP/1.1\r\n" path))
    (dolist (h (verb-ws--get-headers url key))
      (verb-ws--send-internal ws (format "%s: %s\r\n" (car h) (cdr h))))
    (verb-ws--send-internal ws "\r\n")))

(cl-defmethod verb-ws--recv-internal ((ws verb-ws--conn) data)
  "TODO: Docs WS DATA."
  (let ((state (oref ws state))
        (buf (oref ws buffer)))
    (verb--log nil 'D "ws-recv-internal; state=%s" state)
    (pcase state
      ('handshake ; Receiving handshake response from server
       ;; Create buffer lazily
       (unless buf
         (setq buf (oset ws buffer (generate-new-buffer " *verb-ws*"))))

       (with-current-buffer buf
         (insert data)
         (when (re-search-backward "\r\n\r\n" nil t)
           (let (status-code headers)
             (goto-char (point-min))
             ;; Read status line
             (if (re-search-forward verb--http-status-parse-regexp
                                    (line-end-position) t)
                 (setq status-code (match-string 1))
               (error "TODO status-code not found"))

             (verb--log nil 'D "ws-recv-internal; status-code=%s" status-code)

             (unless (string= status-code "101")
               (error "TODO non-101 response"))

             (unless (eobp) (forward-char))

             ;; Read all headers
             (while (re-search-forward verb--http-header-parse-regexp
                                       (line-end-position) t)
               (let ((key (string-trim (match-string 1)))
                     (value (string-trim (match-string 2))))
                 ;; Save header to alist
                 (push (cons key value) headers)
                 (unless (eobp) (forward-char))))

             (verb--log nil 'D "ws-recv-internal; headers=%s" headers)))))
      (_
       (error "Unknown state: %s" state)))))

(cl-defmethod verb-ws--send-internal ((ws verb-ws--conn) data)
  "TODO: Docs WS DATA."
  (process-send-string (oref ws connection) data))

(defun verb-ws--open-stream (url)
  "TODO: Docs URL."
  ;; Heavily based on code from url-http.el (url-http-find-free-connection).
  (let ((url-current-object url)
        (host (url-host url))
        (port (url-port url))
        (url-using-proxy (if (url-host url)
			                 (url-find-proxy-for-url url (url-host url))))
        (buffer (generate-new-buffer " *verb-ws-temp*")))
	(unwind-protect
        (let ((proc (url-open-stream host buffer
                                     (if url-using-proxy
                                         (url-host url-using-proxy)
                                       host)
                                     (if url-using-proxy
                                         (url-port url-using-proxy)
                                       port))))
	      (when (processp proc)
		    (set-process-buffer proc nil))
          proc)
	  (when (get-buffer-process buffer)
	    (set-process-query-on-exit-flag (get-buffer-process buffer) nil))
	  (kill-buffer buffer))))

;; (defun verb-ws-ws ()
;;   (interactive)
;;   (eval-buffer)
;;   (setq cb
;;         (lambda (event send-fn)
;;           (funcall send-fn "test data")
;;           (funcall send-fn "test data")))

;;   (verb-ws--retrieve "ws://localhost:8000/ws/echo" cb))

(provide 'verb-ws)
;;; verb-ws.el ends here
