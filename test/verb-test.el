;;; verb-test.el --- Tests for verb  -*- lexical-binding: t -*-

;; Copyright (C) 2019  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>

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

;; General tests for verb.

;;; Code:

(require 'verb)

(setq verb-show-headers-buffer nil)

(defun text-as-spec (&rest args)
  (verb--request-spec-from-text (mapconcat #'identity args "")))

(defun override-specs (s1 s2 &optional url method headers body)
  (should (equal (verb--request-spec-override
		  (verb--request-spec-from-text (mapconcat #'identity s1 ""))
		  (verb--request-spec-from-text (mapconcat #'identity s2 "")))
		 (verb--request-spec :url (verb--clean-url url)
				     :method method
				     :headers headers
				     :body body))))

(ert-deftest test-back-to-heading-no-headings ()
  ;; Empty buffer
  (with-temp-buffer
    (setq aux (verb--back-to-heading))
    (should (null aux))
    (should (= (point) 1)))
  ;; With contents
  (with-temp-buffer
    (insert "foobar\nfoobar")
    (setq aux (verb--back-to-heading))
    (should (null aux))
    (should (= (point) 1))))

(ert-deftest test-request-spec-from-text-comments-only ()
  (should-error (text-as-spec "# Hello\n" "# world")
		:type 'verb--empty-spec)

  (should-error (text-as-spec "# Hello\n" "")
		:type 'verb--empty-spec)

  (should-error (text-as-spec "")
		:type 'verb--empty-spec))

(ert-deftest test-response-header-line-string ()
  (should (string= (verb--response-header-line-string
		    "test" 1.123 '(("Content-Type" . "hello")
				   ("Content-Length" . "1"))
		    999)
		   "test | 1.123s | hello | 1 byte"))

  (should (string= (verb--response-header-line-string
		    "test" 1.123 '(("Content-Type" . "hello")
				   ("Content-Length" . "33"))
		    999)
		   "test | 1.123s | hello | 33 bytes"))

  (should (string= (verb--response-header-line-string
		    "test" 1.123 '(("Content-Length" . "33")) 0)
		   "test | 1.123s | ? | 33 bytes"))

  (should (string= (verb--response-header-line-string
		    "test" 1.123 '(("Content-Type" . "hello")) 55)
		   "test | 1.123s | hello | 55 bytes")))

(ert-deftest test-request-spec-from-text-error ()
  (should-error (text-as-spec "foobar example.com")))

(ert-deftest test-request-spec-from-text-template ()
  (setq aux (text-as-spec "template example.com"))
  (null (oref aux :method)))

(ert-deftest test-request-spec-from-text-no-url ()
  (setq aux (text-as-spec "GET"))
  (null (oref aux :method))

  (setq aux (text-as-spec "GET "))
  (null (oref aux :method)))

(ert-deftest test-request-spec-from-text-case ()
  (setq aux (text-as-spec "post example.com"))
  (should (string= (oref aux :method) "POST"))

  (setq aux (text-as-spec "Post example.com"))
  (should (string= (oref aux :method) "POST"))

  (setq aux (text-as-spec "PosT example.com"))
  (should (string= (oref aux :method) "POST"))

  (setq aux (text-as-spec "POST example.com"))
  (should (string= (oref aux :method) "POST")))

(ert-deftest test-request-spec-from-text-simple ()
  (setq aux (text-as-spec "GET https://example.com"))
  (should (string= (verb--request-spec-url-string aux)
		   "https://example.com"))
  (should (string= (oref aux :method) "GET"))

  (setq aux (text-as-spec "GET https://example.com\n"))
  (should (string= (verb--request-spec-url-string aux)
		   "https://example.com"))

  (setq aux (text-as-spec "GET /some/path"))
  (should (string= (verb--request-spec-url-string aux)
		   "/some/path"))

  (setq aux (text-as-spec "# Comment\n"
			  "\n"
			  "GET https://example.com"))
  (should (string= (verb--request-spec-url-string aux)
		   "https://example.com"))
  (should (string= (oref aux :method) "GET"))

  (setq aux (text-as-spec "\n"
			  "  # hello\n"
			  "\n"
			  "GET https://example.com"))
  (should (string= (verb--request-spec-url-string aux)
		   "https://example.com"))
  (should (string= (oref aux :method) "GET")))

(ert-deftest test-request-spec-from-text-headers ()
  (should-error (text-as-spec "GET example.com\nTest:\n"))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text"))))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text"))))

  (setq aux (text-as-spec "GET example.com\n"
			  "Foo-Bar: text\n"
			  "Referer: host.com\n"))
  (should (equal (oref aux :headers)
		 (list (cons "Foo-Bar" "text")
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

(ert-deftest test-request-spec-from-text-code-tags ()
  (setq aux (text-as-spec "GET http://example.com/users/{{(+ 1 1)}}\n"))
  (should (string= (verb--request-spec-url-string aux) "http://example.com/users/2"))

  (setq aux (text-as-spec "GET http://example.com\n"
			  "Accept: {{(* 3 2)}}\n"
			  "\n"
			  "test body {{(+ 10 20)}}"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "6"))))
  (should (string= (oref aux :body) "test body 30"))

  (setq aux (text-as-spec "GET http://example.com\n"
			  "Content-Type: text/markdown"
			  "\n"
			  "# A markdown list.\n"
			  "{{}}- Hello\n"
			  "{{}}- World"))
  (should (string= (oref aux :body) "# A markdown list.\n- Hello\n- World")))

(ert-deftest test-request-spec-from-text-complete ()
  (setq aux (text-as-spec "# Comment\n"
			  "  #\n"
			  "  #   \n"
			  "  #  test \n"
			  "\n"
			  "#\n"
			  "\n"
			  " Post   http://example.com/foobar\n"
			  "Accept : text\n"
			  "Foo:bar\n"
			  "Quux: Quuz\n"
			  " Referer   :host\n"
			  "\n"
			  "Content\n"))
  (should (string= (verb--request-spec-url-string aux)
		   "http://example.com/foobar"))
  (should (string= (oref aux :method) "POST"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text")
		       (cons "Foo" "bar")
		       (cons "Quux" "Quuz")
		       (cons "Referer" "host"))))
  (should (string= (oref aux :body) "Content\n")))

(ert-deftest test-request-spec-override ()
  (setq aux (verb--request-spec :url nil :method nil))
  (should-error (verb--request-spec-override aux "test")))

(ert-deftest test-request-spec-url-string ()
  (setq aux (verb--request-spec-from-text
	     "GET http://hello.com/test"))
  (should (string= (verb--request-spec-url-string aux)
		   "http://hello.com/test"))

  (setq aux (verb--request-spec-from-text
	     "GET hello/world"))
  (should (string= (verb--request-spec-url-string aux)
		   "hello/world")))

(ert-deftest test-override-url ()
  (should (equal (verb--override-url nil nil) nil))

  (setq url1 (url-generic-parse-url "http://test.com"))
  (setq url2 nil)
  (should (equal (verb--override-url url1 url2) url1))

  (setq url1 nil)
  (setq url2 (url-generic-parse-url "http://test.com"))
  (should (equal (verb--override-url url1 url2) url2)))

(ert-deftest test-http-headers-p ()
  (should (verb--http-headers-p (list (cons "Foo" "Bar"))))

  (should (verb--http-headers-p (list (cons "Foo" "Bar")
				      (cons "Quux" "Quuz"))))

  (should-not (verb--http-headers-p 1))
  (should-not (verb--http-headers-p nil))
  (should-not (verb--http-headers-p (list nil)))
  (should-not (verb--http-headers-p (list (cons 1 2))))
  (should-not (verb--http-headers-p (list (cons nil nil))))
  (should-not (verb--http-headers-p (list (cons "" ""))))
  (should-not (verb--http-headers-p (list (cons "Hello" ""))))
  (should-not (verb--http-headers-p (list (cons "" "Hello")))))

(ert-deftest test-insert-header-contents ()
  (should (string= (with-temp-buffer
		     (verb--insert-header-contents nil)
		     (buffer-string))
		   ""))

  (should (string= (with-temp-buffer
		     (verb--insert-header-contents '(("A" . "B")
						     ("C" . "D")))
		     (buffer-string))
		   "A: B\nC: D")))

(ert-deftest test-eval-string ()
  (should (string= (verb--eval-string "1")
		   "1"))

  (setq hello 1)
  (should (string= (verb--eval-string "(+ 1 hello)")
		   "2"))

  (should (string= (verb--eval-string "\"test\"")
		   "test"))

  (should (string= (verb--eval-string "'test")
		   "test"))

  (should (string= (verb--eval-string "nil")
		   "nil"))

  (should (string= (verb--eval-string "t")
		   "t"))

  (should (string= (verb--eval-string "")
		   "")))

(ert-deftest test-eval-lisp-code-in ()
  (should (string= (verb--eval-lisp-code-in "1 {{1}}")
		   "1 1"))

  (should (string= (verb--eval-lisp-code-in "{{}}--")
		   "--"))

  (should (string= (verb--eval-lisp-code-in "1 {{(+ 1 1)}}")
		   "1 2"))

  (setq hello 99)
  (should (string= (verb--eval-lisp-code-in "1 {{(+ 1 hello)}}")
		   "1 100"))

  (should (string= (verb--eval-lisp-code-in "{{\"{{\"}}")
  		   "{{"))

  (should (string= (verb--eval-lisp-code-in "{{\"}\"}}{{\"}\"}}")
  		   "}}"))

  (should-error (verb--eval-lisp-code-in "Hello {{asdfasdf}}")))

(ert-deftest test-url-port ()
  (should (null (verb--url-port (verb--clean-url "http://hello.com"))))
  (should (null (verb--url-port (verb--clean-url "https://hello.com"))))
  (should (null (verb--url-port (verb--clean-url "http://hello.com:80"))))
  (should (null (verb--url-port (verb--clean-url "https://hello.com:443"))))
  (should (= (verb--url-port (verb--clean-url "http://hello.com:8080"))
	     8080))
  (should (= (verb--url-port (verb--clean-url "https://hello.com:8080"))
	     8080))
  (should (= (verb--url-port (url-generic-parse-url "test://hello.com:80"))
	     80)))

(ert-deftest test-clean-url ()
  (should-error (verb--clean-url "foo://hello.com"))

  (should (string= (url-recreate-url (verb--clean-url "http://foo.com"))
		   "http://foo.com"))

  (should (string= (url-recreate-url (verb--clean-url "http://foo.com/"))
		   "http://foo.com/"))

  (should (string= (url-recreate-url (verb--clean-url "http://foo.com/a/path"))
		   "http://foo.com/a/path"))

  (should (string= (url-recreate-url (verb--clean-url "http://foo.com/a/path?a=b&b=c"))
		   "http://foo.com/a/path?a=b&b=c"))

  ;; URL encoding
  (should (string= (url-recreate-url (verb--clean-url "http://foo.com/test?q=hello world"))
		   "http://foo.com/test?q=hello%20world"))

  ;; Empty path + query string
  (should (string= (url-recreate-url (verb--clean-url "http://foo.com?test"))
		   "http://foo.com/?test"))

  ;; Empty path + query string, URL encoding
  (should (string= (url-recreate-url (verb--clean-url "https://foo.com?test=hello world"))
		   "https://foo.com/?test=hello%20world"))

  ;; No schema
  (should (string= (url-recreate-url (verb--clean-url "foo/bar"))
		   "foo/bar"))

  (should (string= (url-recreate-url (verb--clean-url "/"))
		   "/"))

  (should (string= (url-recreate-url (verb--clean-url "/foo/bar"))
		   "/foo/bar"))

  (should (string= (url-recreate-url (verb--clean-url "/foo/bar?a"))
		   "/foo/bar?a"))

  (should (string= (url-recreate-url (verb--clean-url "/foo/bar?a#b"))
		   "/foo/bar?a#b")))

(ert-deftest test-override-specs ()
  (override-specs '("GET http://test.com")
		  '("TEMPLATE")
		  "http://test.com"
		  "GET")

  (override-specs '("GET")
		  '("TEMPLATE http://hello.com")
		  "http://hello.com"
		  "GET")

  (override-specs '("GET ?test=1")
		  '("TEMPLATE http://hello.com")
		  "http://hello.com/?test=1"
		  "GET")

  (override-specs '("TEMPLATE http://test.com")
		  '("TEMPLATE")
		  "http://test.com")

  (override-specs '("TEMPLATE http://test.com")
		  '("GET")
		  "http://test.com"
		  "GET")

  (override-specs '("TEMPLATE http://test.com")
		  '("GET /users")
		  "http://test.com/users"
		  "GET")

  (override-specs '("TEMPLATE http://test.com?token=hello")
		  '("GET /users")
		  "http://test.com/users?token=hello"
		  "GET")

  (override-specs '("GET http://test.com")
		  '("POST /users")
		  "http://test.com/users"
		  "POST")

  (override-specs '("TEMPLATE http://test.com\n"
		    "Auth: Bearer hello")
		  '("POST /users")
		  "http://test.com/users"
		  "POST"
		  (list (cons "Auth" "Bearer hello")))

  (override-specs '("TEMPLATE http://test.com?a=b\n"
		    "Auth: Bearer hello")
		  '("POST /users?a=c#hello\n"
		    "Auth: foobar")
		  "http://test.com/users?a=c#hello"
		  "POST"
		  (list (cons "Auth" "foobar")))

  (override-specs '("TEMPLATE http://test.com\n"
		    "\n"
		    "Random body")
		  '("POST /users/1")
		  "http://test.com/users/1"
		  "POST"
		  nil
		  "Random body")

  (override-specs '("TEMPLATE http://test.com\n")
		  '("POST /users/1\n"
		    "\n"
		    "Random body")
		  "http://test.com/users/1"
		  "POST"
		  nil
		  "Random body")

  (override-specs '("TEMPLATE http://bye.com/x?a=1\n"
		    "Test: 1")
		  '("POST https://hello.com/users/1\n"
		    "Hello: 2\n"
		    "\n"
		    "Test body")
		  "https://hello.com/x/users/1?a=1"
		  "POST"
		  (list (cons "Test" "1")
			(cons "Hello" "2"))
		  "Test body")
  )

(ert-deftest test-override-headers ()
  (should (equal (verb--override-headers (list) (list))
		 (list)))

  (should (equal (verb--override-headers (list (cons "a" "b"))
					 (list (cons "a" "c")))
		 (list (cons "a" "c"))))

  (should (equal (verb--override-headers (list (cons "a" "d"))
					 (list (cons "c" "c")))
		 (list (cons "a" "d")
		       (cons "c" "c")))))

(ert-deftest test-override-url-queries ()
  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "")
		  (verb--url-query-string-to-alist ""))
		 nil))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "a=b")
		  (verb--url-query-string-to-alist ""))
		 (list (cons "a" "b"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "a=b")
		  (verb--url-query-string-to-alist "dd=bb"))
		 (list (cons "a" "b")
		       (cons "dd" "bb"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "a=b")
		  (verb--url-query-string-to-alist "a=c"))
		 (list (cons "a" "c"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "a=b&t=y")
		  (verb--url-query-string-to-alist "a=c"))
		 (list (cons "t" "y")
		       (cons "a" "c"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "")
		  (verb--url-query-string-to-alist "e=r&a=c"))
		 (list (cons "e" "r")
		       (cons "a" "c"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "")
		  (verb--url-query-string-to-alist "g=1&g=2"))
		 (list (cons "g" "1")
		       (cons "g" "2"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "a=1&a=2&n=i")
		  (verb--url-query-string-to-alist "a=n"))
		 (list (cons "n" "i")
		       (cons "a" "n"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "a=1&a=2&n=i")
		  (verb--url-query-string-to-alist "a=n&a=h"))
		 (list (cons "n" "i")
		       (cons "a" "n")
		       (cons "a" "h"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "r=t")
		  (verb--url-query-string-to-alist "a=n&a=h&a=l"))
		 (list (cons "r" "t")
		       (cons "a" "n")
		       (cons "a" "h")
		       (cons "a" "l"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "r")
		  (verb--url-query-string-to-alist "r=1"))
		 (list (cons "r" "1"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "r&r")
		  (verb--url-query-string-to-alist "r=1"))
		 (list (cons "r" "1"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "r&r")
		  (verb--url-query-string-to-alist "r=1&r=3"))
		 (list (cons "r" "1")
		       (cons "r" "3"))))

  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "a=1&a=2&foo&c=3")
		  (verb--url-query-string-to-alist "a=n&a=h&foo&bar&g=1"))
		 (list (cons "c" "3")
		       (cons "a" "n")
		       (cons "a" "h")
		       (cons "foo" nil)
		       (cons "bar" nil)
		       (cons "g" "1")))))

(ert-deftest test-url-query-alist-to-string ()
  (should (equal (verb--url-query-alist-to-string nil)
		 nil))

  (should (equal (verb--url-query-alist-to-string
		  (list (cons "a" "b")))
		 "a=b"))

  (should (equal (verb--url-query-alist-to-string
		  nil)
		 nil))

  (should (equal (verb--url-query-alist-to-string
		  (list (cons "a" nil)))
		 "a"))

  (should (equal (verb--url-query-alist-to-string
		  (list (cons "a" "b")
			(cons "c" "d")))
		 "a=b&c=d")))

(ert-deftest test-url-query-string-to-alist ()
  (should (equal (verb--url-query-string-to-alist nil)
		 ()))

  (should (equal (verb--url-query-string-to-alist "")
		 ()))

  (should (equal (verb--url-query-string-to-alist "a=b")
		 (list (cons "a" "b"))))

  (should (equal (verb--url-query-string-to-alist "=&=&=")
		 ()))

  (should (equal (verb--url-query-string-to-alist "a=b&&&")
		 (list (cons "a" "b"))))

  (should (equal (verb--url-query-string-to-alist "foo")
		 (list (cons "foo" nil))))

  (should (equal (verb--url-query-string-to-alist "foo&bar")
		 (list (cons "foo" nil)
		       (cons "bar" nil))))

  (should (equal (verb--url-query-string-to-alist "foo&foo=1")
		 (list (cons "foo" nil)
		       (cons "foo" "1"))))

  (should (equal (verb--url-query-string-to-alist "a=b&c=d")
		 (list (cons "a" "b")
		       (cons "c" "d"))))

  (should (equal (verb--url-query-string-to-alist "a=b&c=d&")
		 (list (cons "a" "b")
		       (cons "c" "d"))))

  (should (equal (verb--url-query-string-to-alist "&a=b&c=d&")
		 (list (cons "a" "b")
		       (cons "c" "d"))))

  (should (equal (verb--url-query-string-to-alist "a=b=x&c=d")
		 (list (cons "a" "b=x")
		       (cons "c" "d"))))

  (should (equal (verb--url-query-string-to-alist "foo=1&foo=2")
		 (list (cons "foo" "1")
		       (cons "foo" "2"))))

  (should (equal (verb--url-query-string-to-alist "foo=1&foo=2&foo=3")
		 (list (cons "foo" "1")
		       (cons "foo" "2")
		       (cons "foo" "3"))))

  (should (equal (verb--url-query-string-to-alist "foo[x]=1&foo[y]=2")
		 (list (cons "foo[x]" "1")
		       (cons "foo[y]" "2")))))

(defun assert-url-override (original other expected)
  (should (equal (verb--override-url (verb--clean-url original)
				     (verb--clean-url other))
		 (verb--clean-url expected))))

(ert-deftest test-override-url ()
  ;; Schema
  (assert-url-override "https://hello.com"
		       "http://hello.com"
		       "http://hello.com")

  ;; Port
  (assert-url-override "http://hello.com"
		       "http://hello.com:8080"
		       "http://hello.com:8080")

  (assert-url-override "http://hello.com:8080"
		       "http://hello.com"
		       "http://hello.com:8080")

  ;; Host
  (assert-url-override "http://hello.com"
		       "http://foo.com"
		       "http://foo.com")

  (assert-url-override "http://hello.com/test"
		       "http://foo.com"
		       "http://foo.com/test")

  ;; Path
  (assert-url-override "http://hello.com"
		       "/test"
		       "http://hello.com/test")

  (assert-url-override "http://hello.com"
		       "/test/foo"
		       "http://hello.com/test/foo")

  (assert-url-override "http://hello.com"
		       "/"
		       "http://hello.com/")

  (assert-url-override "http://hello.com/"
		       ""
		       "http://hello.com/")

  ;; Path + query string
  (assert-url-override "http://hello.com?a=b"
  		       "/hello"
  		       "http://hello.com/hello?a=b")

  (assert-url-override "http://hello.com?a=b"
  		       "http://hello.com?c=d"
  		       "http://hello.com/?a=b&c=d")

  (assert-url-override "http://hello.com?a=b"
  		       "/hello/bye?a=c"
  		       "http://hello.com/hello/bye?a=c")

  (assert-url-override "http://hello.com/?a=b&a=g"
  		       "/hello/bye?a=c"
  		       "http://hello.com/hello/bye?a=c")

  (assert-url-override "http://hello.com/foo?a=b&a=g&hello"
  		       "/hello/bye?a=c"
  		       "http://hello.com/foo/hello/bye?hello&a=c")

  ;; Fragment
  (assert-url-override "http://hello.com#a"
		       "http://hello.com#b"
		       "http://hello.com#b")

  (assert-url-override "http://hello.com#a"
		       "http://hello.com"
		       "http://hello.com#a")

  ;; Various things
  (assert-url-override "http://hello.com/user?a=1&a=2&foo#foobar"
		       "http://hello.com/test?a=2&a=3&quux#baz"
		       "http://hello.com/user/test?foo&a=2&a=3&quux#baz")
  )

(ert-deftest test-headers-content-type ()
  (should (equal (verb--headers-content-type
		  '(("A" . "a")
		    ("Content-Type" . "application/json; charset=hello")))
		 (cons "application/json" "hello")))

  (should (equal (verb--headers-content-type
		  '(("A" . "a")
		    ("Content-Type" . "application/json; charset=utf-8; foo")))
		 (cons "application/json" "utf-8")))

  (should (equal (verb--headers-content-type
		  '(("A" . "a")
		    ("Content-Type" . "application/json")))
		 (cons "application/json" nil)))

  (should (equal (verb--headers-content-type
		  '(("A" . "a")
		    ("B" . "b")))
		 (cons nil nil)))

  (should (equal (verb--headers-content-type nil)
		 (cons nil nil))))

(ert-deftest test-prepare-http-headers ()
  (should (equal (verb--prepare-http-headers '(("A" . "a")
					       ("B" . "v")))
		 `(("Accept-Charset" . ,(url-mime-charset-string))
		   ("A" . "a")
		   ("B" . "v"))))

  (should (equal (verb--prepare-http-headers '(("A" . "test")
					       ("B" . "test")
					       ("Content-Type" . "text")))
		 `(("Accept-Charset" . ,(url-mime-charset-string))
		   ("A" . "test")
		   ("B" . "test")
		   ("Content-Type" . "text; charset=utf-8"))))

  (should (equal (verb--prepare-http-headers '(("A" . "test")
					       ("B" . "test")
					       ("Content-Type" . "text; charset=hello")))
		 `(("Accept-Charset" . ,(url-mime-charset-string))
		   ("A" . "test")
		   ("B" . "test")
		   ("Content-Type" . "text; charset=hello")))))

(ert-deftest test-to-ascii ()
  (should-not (multibyte-string-p (verb--to-ascii "ññáé"))))

(ert-deftest test-http-method-p ()
  (should (verb--http-method-p "GET"))
  (should (verb--http-method-p "POST"))
  (should-not (verb--http-method-p verb--template-keyword))
  (should-not (verb--http-method-p "test")))

;; Tests using the test server (server.py)

(defmacro server-test (test-name &rest body)
  (declare (indent 1))
  `(progn
     (find-file "test/test.verb")
     (goto-char (point-min))
     (re-search-forward (concat "^-+ " ,test-name "$"))
     (let ((inhibit-message t))
       (with-current-buffer (verb-execute-request-on-point)
	 (sleep-for 0.3)
	 ,@body))
     (kill-buffer)))

(ert-deftest test-server-basic ()
  (server-test "basic"
    (should (string= (buffer-string) "Hello, World!"))
    (should (eq major-mode 'text-mode))))

(ert-deftest test-server-basic-json ()
  (server-test "basic-json"
    (should (string= (buffer-string)
		     "{\"foo\":true,\"hello\":\"world\"}"))
    (should (eq major-mode 'js-mode))))

(ert-deftest test-server-error-400 ()
  (server-test "error-400"
    (should (string-match "400" header-line-format))))

(ert-deftest test-server-error-404 ()
  (server-test "error-404"
    (should (string-match "404" header-line-format))))

(ert-deftest test-server-error-405 ()
  (server-test "error-405"
    (should (string-match "405" header-line-format))))

(ert-deftest test-server-root-with-args ()
  (server-test "root-with-args"
    (should (string= (buffer-string) "OK"))))

(ert-deftest test-server-response-latin-1 ()
  (server-test "response-latin-1"
    (should (coding-system-equal buffer-file-coding-system 'iso-latin-1-unix))
    (should (string-match "ñáéíóúß" (buffer-string)))))

(ert-deftest test-server-request-latin-1 ()
  (server-test "request-latin-1"
    (should (string= (buffer-string) "OK"))))

(ert-deftest test-server-request-utf-8-default ()
  (server-test "request-utf-8-default"
    (should (string= (buffer-string) "OK"))))

(ert-deftest test-server-request-utf-8-default ()
  (server-test "request-utf-8-default-2"
    (should (string= (buffer-string) "OK"))))

(ert-deftest test-server-response-big5 ()
  (server-test "response-big5"
    (should (coding-system-equal buffer-file-coding-system 'chinese-big5-unix))
    (should (string-match "常用字" (buffer-string)))))

(ert-deftest test-server-response-utf-8-default ()
  (server-test "response-utf-8-default"
    (should (string= (cdr (assoc-string "Content-Type" verb--response-headers))
		     "text/plain"))
    (should (string= verb-default-response-charset "utf-8"))
    (should (coding-system-equal buffer-file-coding-system 'utf-8-unix))
    (should (string-match "ñáéíóúß" (buffer-string)))))

(ert-deftest test-redirect-302 ()
  (server-test "redirect-302"
    (should (string= (buffer-string) "Hello, World!"))))

(ert-deftest test-redirect-301 ()
  (server-test "redirect-301"
    (should (string= (buffer-string) "Hello, World!"))))

(ert-deftest test-redirect-308 ()
  (server-test "redirect-308"
    (should (string= (buffer-string) "Redirect successful"))))

(ert-deftest test-buffers-created ()
  (setq num-buffers (length (buffer-list)))
  (server-test "basic")
  (should (= (1+ num-buffers) (length (buffer-list)))))

(ert-deftest test-connection-error ()
  (setq num-buffers (length (buffer-list)))
  (ignore-errors
    (server-test "connection-fail-test"))
  (kill-buffer "test.verb")
  (should (= num-buffers (length (buffer-list)))))

(provide 'verb-test)
;;; verb-test.el ends here
