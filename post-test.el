;;; post-test.el --- Tests for post  -*- lexical-binding: t -*-

;; Copyright (C) 2019  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>

;; post is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; post is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with post.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; General tests for post.

;;; Code:

(require 'post)

(defun text-as-spec (&rest args)
  (post--request-spec-from-text (mapconcat #'identity args "")))

(ert-deftest test-back-to-heading-no-headings ()
  ;; Empty buffer
  (with-temp-buffer
    (setq aux (post--back-to-heading))
    (should (null aux))
    (should (= (point) 1)))
  ;; With contents
  (with-temp-buffer
    (insert "foobar\nfoobar")
    (setq aux (post--back-to-heading))
    (should (null aux))
    (should (= (point) 1))))

(ert-deftest test-request-spec-from-text-error ()
  (should-error (text-as-spec "foobar example.com"))
  (should-error (text-as-spec "")))

(ert-deftest test-request-spec-from-text-template ()
  (setq aux (text-as-spec "template example.com"))
  (null (oref aux :method)))

(ert-deftest test-request-spec-from-text-case ()
  (setq aux (text-as-spec "post example.com"))
  (should (string= (oref aux :method) "POST"))

  (setq aux (text-as-spec "Post example.com"))
  (should (string= (oref aux :method) "POST"))

  (setq aux (text-as-spec "PoST example.com"))
  (should (string= (oref aux :method) "POST"))

  (setq aux (text-as-spec "POST example.com"))
  (should (string= (oref aux :method) "POST")))

(ert-deftest test-request-spec-from-text-simple ()
  (setq aux (text-as-spec "GET example.com"))
  (should (string= (oref aux :url) "https://example.com"))
  (should (string= (oref aux :method) "GET"))

  (setq aux (text-as-spec "GET example.com\n"))
  (should (string= (oref aux :url) "https://example.com"))

  (setq aux (text-as-spec "# Comment\n"
			  "\n"
			  "GET example.com"))
  (should (string= (oref aux :url) "https://example.com"))
  (should (string= (oref aux :method) "GET"))

  (setq aux (text-as-spec "\n"
			  "  # hello\n"
			  "\n"
			  "GET example.com"))
  (should (string= (oref aux :url) "https://example.com"))
  (should (string= (oref aux :method) "GET")))

(ert-deftest test-request-spec-from-text-headers ()
  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text"))))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text"))))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "Referer: host.com\n"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text")
		       (cons "Referer" "host.com")))))

(ert-deftest test-request-spec-from-text-body ()
  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"))
  (should (null (oref aux :body)))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "\n"))
  (should (null (oref aux :body)))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "\n\n"))
  (should (null (oref aux :body)))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "\n\n\n\n  \n\n"))
  (should (null (oref aux :body)))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "\n"
			  "\n"
			  "hello\n"))
  (should (string= (oref aux :body) "\nhello\n"))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "\n" ;; This line is ignored
			  "hello world"))
  (should (string= (oref aux :body) "hello world"))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "hello world"))
  (should (string= (oref aux :body) "hello world")))

(ert-deftest test-request-spec-from-text-complete ()
  (setq aux (text-as-spec "# Comment\n"
			  "  #\n"
			  "  #   \n"
			  "  #  test \n"
			  "\n"
			  "#\n"
			  "\n"
			  " Post    example.com/foobar\n"
			  "Accept : text\n"
			  "Foo:bar\n"
			  "Quux: Quuz\n"
			  " Referer   :host\n"
			  "\n"
			  "Content\n"))
  (should (string= (oref aux :url) "https://example.com/foobar"))
  (should (string= (oref aux :method) "POST"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text")
		       (cons "Foo" "bar")
		       (cons "Quux" "Quuz")
		       (cons "Referer" "host"))))
  (should (string= (oref aux :body) "Content\n")))

(ert-deftest test-http-headers-p ()
  (should (post--http-headers-p (list (cons "Foo" "Bar"))))

  (should (post--http-headers-p (list (cons "Foo" "Bar")
				      (cons "Quux" "Quuz"))))

  (should-not (post--http-headers-p 1))
  (should-not (post--http-headers-p nil))
  (should-not (post--http-headers-p (list nil)))
  (should-not (post--http-headers-p (list (cons 1 2))))
  (should-not (post--http-headers-p (list (cons nil nil))))
  (should-not (post--http-headers-p (list (cons "" ""))))
  (should-not (post--http-headers-p (list (cons "Hello" ""))))
  (should-not (post--http-headers-p (list (cons "" "Hello")))))

(ert-deftest test-clean-url ()
  (should-error (post--clean-url "foo://hello.com"))

  (should (string= (post--clean-url "http://foo.com")
		   "http://foo.com"))

  (should (string= (post--clean-url "http://foo.com/a/path")
		   "http://foo.com/a/path"))

  (should (string= (post--clean-url "http://foo.com/a/path?a=b&b=c")
		   "http://foo.com/a/path?a=b&b=c"))

  ;; URL encoding
  (should (string= (post--clean-url "http://foo.com/test?q=hello world")
		   "http://foo.com/test?q=hello%20world"))

  (should (string= (post--clean-url "https://foo.com")
		   "https://foo.com"))

  (should (string= (post--clean-url "foo.com")
		   "https://foo.com"))

  ;; Empty path + query string
  (should (string= (post--clean-url "http://foo.com?test")
		   "http://foo.com/?test"))

  ;; Empty path + query string, no schema, URL encoding
  (should (string= (post--clean-url "foo.com?test=hello world")
		   "https://foo.com/?test=hello%20world")))

(provide 'post-test)
;;; post.el ends here
