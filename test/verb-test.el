;;; verb-test.el --- Tests for verb  -*- lexical-binding: t -*-

;; Copyright (C) 2021  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>

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

;; General tests for verb.

;;; Code:

(require 'ert-x)
(require 'verb)
(require 'ob-verb)
(require 'cl-lib)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((verb . t)))

(setq org-confirm-babel-evaluate nil)

(defun join-lines (&rest args)
  (mapconcat #'identity args "\n"))

(defun text-as-spec (&rest args)
  (verb-request-spec-from-string (mapconcat #'identity args "")))

(defun text-as-spec-nl (&rest args)
  (verb-request-spec-from-string (mapconcat #'identity args "\n")))

(defun override-specs (s1 s2 &optional url method headers body)
  (should (equal (verb-request-spec-override
		  (verb-request-spec-from-string (mapconcat #'identity s1 ""))
		  (verb-request-spec-from-string (mapconcat #'identity s2 "")))
		 (verb-request-spec :url (verb--clean-url url)
				    :method method
				    :headers headers
				    :body body))))

;; Create log buffer now
(verb--log nil 'I "")

(ert-deftest test-up-heading ()
  (setq outline-test
	;; Test up-heading without level 1 heading
	(join-lines "** Level 2 heading"
		    "*** Level 3 heading"
		    "get http://test.com"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (verb--up-heading)
    (should (= (point) 1))
    (verb--up-heading)
    (should (= (point) 1)))

  (setq outline-test
	;; Test up-heading without level 1 heading, with empty space
	(join-lines "test"
		    ""
		    "** Level 2 heading"
		    "*** Level 3 heading"
		    "get http://test.com"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (verb--up-heading)
    (should (= (point) 7))
    (verb--up-heading)
    (should (= (point) 1))))

(ert-deftest test-heading-contents ()
  (setq outline-test
	(join-lines "* Heading1"
		    "get http://test.com"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should (string= (verb--heading-contents)
		     "get http://test.com")))

  (setq outline-test
	(join-lines "* Heading"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should (string= (verb--heading-contents) "")))

  (setq outline-test
	(join-lines "* Heading"
		    "* H2"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should (string= (verb--heading-contents) "")))

  (setq outline-test
	(join-lines "** Heading level 2"
		    "get http://test.com"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should (string= (verb--heading-contents)
		     "get http://test.com")))

  (setq outline-test
	(join-lines "** Heading level 2"
		    "\nget http://test.com\n\n"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should (string= (verb--heading-contents)
		     "\nget http://test.com\n\n")))

  (setq outline-test
	(join-lines "** Heading level 2"
		    "hello world"
		    "* level 1"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should (string= (verb--heading-contents)
		     "hello world")))

  (setq outline-test
	(join-lines "** Heading level 2"
		    "hello world"
		    ""
		    "* level 1"
		    "something"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should (string= (verb--heading-contents)
		     "hello world\n")))

  (setq outline-test
	;; no headings
	(join-lines "get http://test.com"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (goto-char (point-min))
    (should-error (verb--heading-contents))))

(ert-deftest test-request-spec-from-hierarchy-babel-blocks-above ()
  (setq tgt-spec (verb-request-spec :method "GET"
				    :url (verb--clean-url
					  "http://hello.com")))

  (setq outline-test
	(join-lines "* Test :verb:"
		    "#+begin_src verb"
		    "template http://hello.com"
		    "#+end_src"
		    "** Test2"
		    "get"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should (equal (verb--request-spec-from-hierarchy) tgt-spec)))

  (setq outline-test
	(join-lines "* Test :verb:"
		    "#+begin_src            verb"
		    "template http://hello.com"
		    "#+end_src"
		    "** Test2"
		    "get"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should (equal (verb--request-spec-from-hierarchy) tgt-spec)))

  (setq outline-test
	(join-lines "* Test :verb:"
		    "#+begin_src python"
		    "print('hellooooo')"
		    "#+end_src"
		    "** Test2"
		    "get"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should-error (verb--request-spec-from-hierarchy))))

(ert-deftest test-request-spec-from-hierarchy-ignore-nontagged ()
  (setq test-rs (verb-request-spec :method "GET"
				   :url (verb--clean-url "http://hello.com")))
  (setq outline-test
	(join-lines "* Header"
		    "this is not a valid spec"
		    "- [ ] a todo list"
		    ""
		    "** Test :verb:"
		    ":PROPERTIES:"
		    ":Author: me"
		    ":END:"
		    "template http://hello.com"
		    "*** Test2"
		    "get"))

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should (equal (verb--request-spec-from-hierarchy) test-rs))))

(ert-deftest test-request-spec-from-hierarchy-no-headings ()
  (setq outline-test
	(join-lines "delete http://test.com"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should-error (verb--request-spec-from-hierarchy))))

(ert-deftest test-request-spec-from-hierarchy-base ()
  (let ((verb-base-headers '(("Foo" . "Bar")
			     ("Quuz" . "Quux"))))
    (setq outline-test
	  (join-lines "* Test :verb:"
		      "get http://hello.com"
		      "Quuz: X"))
    (with-temp-buffer
      (org-mode)
      (verb-mode)
      (insert outline-test)
      (setq req-spec (verb--request-spec-from-hierarchy))
      (should (equal (oref req-spec headers)
		     '(("Foo" . "Bar")
		       ("Quuz" . "X")))))))

(ert-deftest test-request-spec-from-hierarchy ()
  (setq outline-test
	(join-lines "* Test :verb:"
		    "template http://hello.com"
		    "** Test2"
		    "get"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should (equal (verb--request-spec-from-hierarchy)
		   (verb-request-spec :method "GET"
				      :url (verb--clean-url
					    "http://hello.com")))))
  (setq outline-test
	(join-lines "foo bar"
		    "* Test :verb:"
		    "template http://hello.com"
		    "** Test2"
		    "post ?a=b"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should (equal (verb--request-spec-from-hierarchy)
		   (verb-request-spec :method "POST"
				      :url (verb--clean-url
					    "http://hello.com?a=b")))))

  (setq outline-test
	(join-lines "* Test :verb:"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should-error (verb--request-spec-from-hierarchy)))

  (setq outline-test
	(join-lines "* Test :verb:"
		    "template http://hello.com"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should-error (verb--request-spec-from-hierarchy)))

    (setq outline-test
	(join-lines "* Test :verb:"
		    "get"))
    (with-temp-buffer
      (org-mode)
      (verb-mode)
      (insert outline-test)
      (should-error (verb--request-spec-from-hierarchy)))

  (setq outline-test
	(join-lines "* Test :verb:"
		    "get /some/path"))
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert outline-test)
    (should-error (verb--request-spec-from-hierarchy))))

(ert-deftest test-request-spec-from-hierarchy-metadata ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* test :verb:"
			":properties:"
			":Verb-Foo: xyz"
			":Verb-Name: X"
			":end:"
			"** Test"
			":properties:"
			":Verb-Name: JOHN"
			":end:"
			"get http://foobar.com"))
    (setq req-spec (verb--request-spec-from-hierarchy))
    (should (equal (oref req-spec metadata)
		   '(("VERB-NAME" . "JOHN"))))))

(ert-deftest test-request-spec-from-hierarchy-metadata-multiline ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* test :verb:"
            ":properties:"
            ":Verb-Foo: xyz"
            ":Verb-Name: X"
            ":end:"
            "** Test"
            ":properties:"
            ":Verb-Name: JOHN"
            ":verb-NAME+: DOE"
            ":verb-name+: Smith"
            ":end:"
            "get http://foobar.com"))

    (should (equal (oref (verb--request-spec-from-hierarchy) metadata)
		           '(("VERB-NAME" . "JOHN DOE Smith"))))))

(ert-deftest test-request-spec-from-hierarchy-map-request ()
  (defun map-req-1 (rs)
    (oset rs body "foobarfoobar")
    rs)

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert
     (join-lines
      "* Test :verb:"
      ":properties:"
      ":Verb-Map-Request: map-req-1"
      ":end:"
      "post http://localhost"))

    (should (string= (oref (verb--request-spec-from-hierarchy) body)
                     "foobarfoobar")))

  (defun map-req-2 (rs)
    (oset rs headers (append (oref rs headers) '(("X-Foo" . "Test"))))
    rs)

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert
     (join-lines
      "* Test :verb:"
      ":properties:"
      ":VERB-MAP-REQUEST: map-req-2"
      ":end:"
      "post http://localhost"
      "Content-Type: application/json"))

    (should (equal (oref (verb--request-spec-from-hierarchy) headers)
                   '(("Content-Type" . "application/json")
                     ("X-Foo" . "Test"))))))

(ert-deftest test-request-spec-from-hierarchy-map-request-no-fn ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert
     (join-lines
      "* Test :verb:"
      ":properties:"
      ":Verb-Map-Request: helloworld"
      ":end:"
      "post http://localhost"))

    (should-error (verb--request-spec-from-hierarchy))))

(ert-deftest test-request-spec-from-hierarchy-map-request-bad-fn ()
  (defun map-req-3 (rs)
    (oset rs body "foobarfoobar")
    ;; return an int
    42)

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert
     (join-lines
      "* Test :verb:"
      ":properties:"
      ":Verb-Map-Request: map-req-3"
      ":end:"
      "post http://localhost"))

    (should-error (verb--request-spec-from-hierarchy))))

(ert-deftest test-request-spec-from-hierachy-narrowed ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert
     (join-lines
      "* test :verb:"
      "template http://localhost/foo"
      "** test2"
      "get /bar"))
    (re-search-backward "test2")
    (org-narrow-to-subtree)
    (let ((rs (verb--request-spec-from-hierarchy)))
      (should (string= (verb-request-spec-url-to-string rs)
                       "http://localhost/foo/bar")))))

(ert-deftest test-nonempty-string ()
  (should (string= (verb--nonempty-string "hello") "hello"))
  (should-not (verb--nonempty-string "")))

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

(ert-deftest test-verb-heading-tags ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* H1  :a:b:"
			"something"
			"** H2 :c:"))
    (should (equal (verb--heading-tags) '("a" "b" "c"))))

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* Heading with no tags"
                        "content"))
    (should-not (verb--heading-tags)))

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "no headings"))
    (should-not (verb--heading-tags))))

(ert-deftest test-verb-heading-tags-inheritance ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* H1  :a:b:"
			"something"
			"** H2 :c:"))
    (let ((org-use-tag-inheritance t))
      (should (equal (verb--heading-tags) '("a" "b" "c")))))

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* H1  :a:b:"
			"something"
			"** H2 :c:"))
    (let ((org-use-tag-inheritance nil))
      (should (equal (verb--heading-tags) '("c")))))

    (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* H1"
			"something"
			"** H2 :c:"))
    (let ((org-use-tag-inheritance nil))
      (should (equal (verb--heading-tags) '("c"))))))

(ert-deftest test-verb-heading-properties-no-inheritance ()
  (with-temp-buffer
    (org-mode)
    (insert (join-lines "* H1"
			":properties:"
			":Verb-X: something"
			":end:"
			"** H2"
			":properties:"
			":Verb-Y: foo"
			":end:"))
    (should (equal (verb--heading-properties "verb-")
		     '(("VERB-Y" . "foo"))))))

(ert-deftest test-verb-heading-properties-with-inheritance ()
  (with-temp-buffer
    (org-mode)
    (insert (join-lines "* H1"
			            ":properties:"
			            ":Verb-X: something"
			            ":Verb-Y: foobar"
			            ":end:"
			            "** H2"
			            ":properties:"
			            ":Verb-Y: hello"
			            ":end:"))
    (let ((org-use-property-inheritance t))
      (should (equal (verb--heading-properties "verb-")
		             '(("VERB-X" . "something")
                       ("VERB-Y" . "hello")))))))

(ert-deftest test-request-spec-from-text-comments-only ()
  (should-error (text-as-spec "# Hello\n" "# world")
		:type 'verb-empty-spec)

  (should-error (text-as-spec "# Hello\n" "")
		:type 'verb-empty-spec)

  (should-error (text-as-spec "")
		:type 'verb-empty-spec))

(ert-deftest test-response-header-line-string ()
  (should (string= (verb--response-header-line-string
		    (verb-response
		     :status "test"
		     :duration 1.123
		     :headers '(("Content-Type" . "hello")
				("Content-Length" . "1"))
		     :body-bytes 999))
		   "test | 1.123s | hello | 1 byte"))

  (should (string= (verb--response-header-line-string
		    (verb-response
		     :status "test"
		     :duration 1.123
		     :headers '(("Content-Length" . "2024"))
		     :body-bytes 999))
		   (if (< emacs-major-version 27)
		       "test | 1.123s | - | 2.0k bytes"
		     "test | 1.123s | - | 2k bytes")))

  (should (string= (verb--response-header-line-string
		    (verb-response
		     :status "test"
		     :duration 1.123
		     :headers '(("Content-Type" . "hello"))
		     :body-bytes 999))
		   "test | 1.123s | hello | 999 bytes"))

  (should (string= (verb--response-header-line-string
		    (verb-response
		     :status nil
		     :duration 1.123
		     :headers nil))
		   "No Response | 1.123s | - | 0 bytes")))

(ert-deftest test-request-spec-from-text-error ()
  (should-error (text-as-spec "foobar example.com")))

(ert-deftest test-request-spec-from-text-template ()
  (setq aux (text-as-spec "template example.com"))
  (should-not (oref aux :method)))

(ert-deftest test-request-spec-from-text-no-url ()
  (setq aux (text-as-spec "GET"))
  (should-not (oref aux url))

  (setq aux (text-as-spec "GET "))
  (should-not (oref aux url))

  (setq aux (text-as-spec "POST\n\n\n"))
  (should-not (oref aux url))
  (should (string= (oref aux method) "POST"))

  (setq aux (text-as-spec "{{(concat \"PO\" \"ST\")}}\n"))
  (should-not (oref aux url))
  (should (string= (oref aux method) "POST")))

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
  (should (string= (verb-request-spec-url-to-string aux)
		   "https://example.com"))
  (should (string= (oref aux :method) "GET"))

  (setq aux (text-as-spec "GET https://example.com\n"))
  (should (string= (verb-request-spec-url-to-string aux)
		   "https://example.com"))

  (setq aux (text-as-spec "GET /some/path"))
  (should (string= (verb-request-spec-url-to-string aux)
		   "/some/path"))

  (setq aux (text-as-spec "# Comment\n"
			  "\n"
			  "GET https://example.com"))
  (should (string= (verb-request-spec-url-to-string aux)
		   "https://example.com"))
  (should (string= (oref aux :method) "GET"))

  (setq aux (text-as-spec "\n"
			  "  # hello\n"
			  "\n"
			  "GET https://example.com"))
  (should (string= (verb-request-spec-url-to-string aux)
		   "https://example.com"))
  (should (string= (oref aux :method) "GET")))

(ert-deftest test-request-spec-from-text-headers ()
  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text"))))

  (setq aux (text-as-spec "GET example.com\n"
			  "A:\n"
			  "B:"))
  (should (equal (oref aux :headers)
		 (list (cons "A" "")
		       (cons "B" ""))))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text"))))

  (should-error (text-as-spec "optionssss"))

  (setq aux (text-as-spec "GET example.com\n"
			  "Foo-Bar: text\n"
			  "Referer: host.com\n"))
  (should (equal (oref aux :headers)
		 (list (cons "Foo-Bar" "text")
		       (cons "Referer" "host.com")))))

(ert-deftest test-request-spec-from-text-body-trailing-chars ()
  (setq aux (text-as-spec-nl "GET example.com"
			     "Accept: text"
			     ""
			     "hello"
			     ""
			     " "))
  (should (string= (oref aux :body) "hello\n\n "))

  (let ((verb-trim-body-end "[ \t\n\r]+"))
    (setq aux (text-as-spec-nl "GET example.com"
			       "Accept: text"
			       ""
			       "  hello   "
			       ""
			       " "
			       ""))
    (should (string= (oref aux :body) "  hello")))

  (let ((verb-trim-body-end "-+"))
    (setq aux (text-as-spec-nl "GET example.com"
			       "Accept: text"
			       ""
                               "---text---"))
    (should (string= (oref aux :body) "---text"))))

(ert-deftest test-request-spec-remove-src-blocks ()
  (setq aux (text-as-spec-nl "GET example.com"
			     "Accept: text"
			     ""
			     "#+begin_src xml"
                             "<something/>"
                             "#+end_src"))
  (should (string= (oref aux :body) "<something/>\n"))

  (setq aux (text-as-spec-nl "GET example.com"
			     "Accept: text"
			     ""
			     "#+begin_src xml"
                             "<something/>"
                             "#+end_src"
                             "hello"))
  (should (string= (oref aux :body) "<something/>\nhello"))

  ;; Removes src block markers even when indented
  (setq aux (text-as-spec-nl "  GET example.com"
        		     "  Accept: text"
        		     ""
        		     "  #+begin_src xml"
                             "  <something/>"
                             "  #+end_src"))
  (should (string= (oref aux :body) "  <something/>\n"))

  (setq aux (text-as-spec-nl "GET example.com"
			     "Accept: text"
			     ""
			     "#+begin_src"
                             "foo"
                             "bar"
                             "#+end_src"))
  (should (string= (oref aux :body) "foo\nbar\n")))

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
  (should (string= (oref aux :body) "hello world")))

(ert-deftest test-request-spec-from-text-code-tags ()
  (setq aux (text-as-spec "GET http://example.com/users/{{(+ 1 1)}}\n"))
  (should (string= (verb-request-spec-url-to-string aux) "http://example.com/users/2"))

  (setq aux (text-as-spec "GET http://example.com\n"
			  "Accept: {{(* 3 2)}}\n"
			  "\n"
			  "test body {{(+ 10 20)}}"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "6"))))
  (should (string= (oref aux :body) "test body 30"))

  (setq aux (text-as-spec "GET http://example.com\n"
			  "Content-Type: text/markdown\n"
			  "\n"
			  "# A markdown list.\n"
			  "{{}}- Hello\n"
			  "{{}}- World"))
  (should (string= (oref aux :body) "# A markdown list.\n- Hello\n- World"))
  (should (equal (oref aux :headers)
		 (list (cons "Content-Type" "text/markdown"))))

  (setq test-header "Content-Type: text/markdown")
  (setq aux (text-as-spec "{{(concat \"g\" \"et\")}} http://example.com\n"
			  "{{test-header}}\n"
			  "\n"
			  "# A markdown list.\n"
			  "{{}}- Hello\n"
			  "{{}}- World"))
  (should (string= (oref aux :body) "# A markdown list.\n- Hello\n- World"))
  (should (equal (oref aux :headers)
		 (list (cons "Content-Type" "text/markdown")))))

(ert-deftest test-blank-line-between-headers-and-body ()
  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "\n"
			  "hello\n"))
  (should (string= (oref aux :body) "hello\n"))

  (should-error
   (setq aux (text-as-spec "GET example.com\n"
			   "Accept: text\n"
			   "hello\n")))

  (setq aux (text-as-spec "GET example.com\n"
			  "Accept: text\n"
			  "\n"))
  (should-not (oref aux :body))

  (setq aux (text-as-spec "GET example.com\n"
			  "\n"))
  (should-not (oref aux :body)))

(ert-deftest test-dont-evaluate-code-tags-in-comments ()
  (setq counter 0)
  (setq inc-counter (lambda () (setq counter (1+ counter))))

  (text-as-spec "GET http://hello.com/{{(funcall inc-counter)}}")
  (should (= counter 1))

  (text-as-spec "GET http://hello.com/{{(funcall inc-counter)}}\n"
		"Header: {{(funcall inc-counter)}}")
  (should (= counter 3))

  (text-as-spec "GET http://hello.com/api\n"
		"Something: 123\n"
		"# Header: {{(funcall inc-counter)}}")
  (should (= counter 3))

  (setq aux (text-as-spec "# Comment\n"
			  "\n"
			  "# Commented out {{(funcall inc-counter)}}\n"
			  "# {{asdfsadfsadf}}\n"
			  "GET http://hello.com/api\n"
			  "Something: 123\n"
			  "# Header: {{(funcall inc-counter)}}\n"
			  "# Hello: World\n"
			  "Uno: Dos\n"))
  (should (= counter 3))
  (should (equal (oref aux headers)
		 '(("Something" . "123")
		   ("Uno" . "Dos")))))

(ert-deftest test-request-spec-from-text-headline-properties ()
  (setq aux (text-as-spec ":PROPERTIES:\n"
			  ":Something: hello\n"
			  ":END:\n"
			  "get http://example.com"))
  (should (string= (oref aux :method) "GET")))

(ert-deftest test-request-spec-from-text-commented-headers ()
  (setq aux (text-as-spec "get http://example.com/foobar\n"
			  "Accept: text\n"
			  "#Foo: bar\n"
			  "\n"
			  "Content\n"))
  (should (equal (oref aux :headers)
		 '(("Accept" . "text"))))
  (should (string= (oref aux :body) "Content\n"))

  (setq aux (text-as-spec "get http://example.com/foobar\n"
			  "#Foo: bar\n"
			  "#Foo: bar2\n"
			  "#kasdflkjasdlfjasdf\n"
			  "\n"
			  "Content\n"))
  (should-not (oref aux :headers))
  (should (string= (oref aux :body) "Content\n"))

  (should-error
   (setq aux (text-as-spec "get http://example.com/foobar\n"
			   "#Foo: bar\n"
			   "#Foo: bar2\n"
			   "Content\n")))

  (setq aux (text-as-spec "get http://example.com/foobar\n"
			  "Accept: text\n"
			  "#Foo: bar\n"
			  "Abc: xyz\n"
			  "\n"
			  "Content\n"))
  (should (equal (oref aux :headers)
		 '(("Accept" . "text")
		   ("Abc" . "xyz"))))
  (should (string= (oref aux :body) "Content\n"))

  (setq aux (text-as-spec "post http://example.com/foobar\n"
			  "# Content-Type: application/json\n"
			  "\n"
			  "Content\n"))
  (should-not (oref aux :headers))
  (should (string= (oref aux :body) "Content\n"))

  (setq aux (text-as-spec "get http://example.com/foobar\n"
			  "Accept: text\n"
			  "#Foo: bar\n"
			  "#Content: content\n"))
  (should (equal (oref aux :headers)
		 '(("Accept" . "text"))))
  (should-not (oref aux :body))

  (setq aux (text-as-spec "get http://example.com/foobar\n"
			  "Accept: text\n"
			  "#Foo: bar\n"
			  "\n"
			  "# Contents\n"))
  (should (equal (oref aux :headers)
		 '(("Accept" . "text"))))
  (should (string= (oref aux :body) "# Contents\n")))

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
			  "Quux:    Quuz\n"
              "Example:      HeaderValue   \n"
			  " Referer   :host\n"
			  "\n"
			  "Content\n"))
  (should (string= (verb-request-spec-url-to-string aux)
		   "http://example.com/foobar"))
  (should (string= (oref aux :method) "POST"))
  (should (equal (oref aux :headers)
		 (list (cons "Accept" "text")
		       (cons "Foo" "bar")
		       (cons "Quux" "Quuz")
               (cons "Example" "HeaderValue")
		       (cons "Referer" "host"))))
  (should (string= (oref aux :body) "Content\n")))

(ert-deftest test-request-spec-headers-underscore ()
  (setq aux (text-as-spec "get http://example.com/foobar\n"
			              "Accept: text\n"
			              "Foo_Bar: xyz\n"
                          "Baz-Bar: quux\n"
			              "\n"
			              "Content\n"))
  (should (equal (oref aux :headers)
		         '(("Accept" . "text")
		           ("Foo_Bar" . "xyz")
                   ("Baz-Bar" . "quux")))))

(ert-deftest test-request-spec-override ()
  (setq aux (verb-request-spec :url nil :method nil))
  (should-error (verb-request-spec-override aux "test")))

(ert-deftest test-request-spec-url-string ()
  (setq aux (verb-request-spec-from-string
	     "GET http://hello.com/test"))
  (should (string= (verb-request-spec-url-to-string aux)
		   "http://hello.com/test"))

  (setq aux (verb-request-spec-from-string
	     "GET hello/world"))
  (should (string= (verb-request-spec-url-to-string aux)
		   "hello/world")))

(ert-deftest test-override-url-nil ()
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
  (should (= (verb--eval-string "1") 1))

  (setq hello 1)
  (should (= (verb--eval-string "(+ 1 hello)") 2))

  (should (string= (verb--eval-string "\"test\"") "test"))

  (should (eq (verb--eval-string "'test") 'test))

  (should (null (verb--eval-string "nil")))

  (should (eq (verb--eval-string "t") t))

  (should (string= (verb--eval-string "") "")))

(ert-deftest test-verb-var ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _)
                                                "Foobar.")))
      (should-not verb--vars)
      (should (string= (verb-var v1) "Foobar."))
      (should (equal verb--vars '((v1 . "Foobar."))))

      (should (string= (verb-var v2 "123") "123"))
      (should (equal verb--vars '((v2 . "123")
                                  (v1 . "Foobar.")))))))

(ert-deftest test-verb-set-var ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (should (string= (verb--eval-string "(verb-var test-var-1 \"hello\")"
					(current-buffer))
		     "hello"))

    (verb-set-var "test-var-2" "bye")
    (should (string= (verb--eval-string "(verb-var test-var-2)"
					(current-buffer))
                     "bye"))

    ;; Set same var again, should replace old entry
    (verb-set-var "test-var-2" "byeee")

    ;; Set var to a non-string value (e.g. integer)
    (should (= (verb--eval-string "(verb-var test-var-3 8080)"
                                  (current-buffer))
               8080))

    (should (= (length verb--vars) 3)))
  (with-temp-buffer
    (should-not verb--vars)))

(ert-deftest test-verb-set-var-empty-name ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (should-error (verb-set-var "" "foo"))))

(ert-deftest test-verb-show-vars-no-vars ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (should-error (verb-show-vars))))

(ert-deftest test-verb-unset-vars ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (verb-set-var "test-var-1" "foo")
    (verb-set-var "test-var-2" "bar")

    (should (= (length verb--vars) 2))

    (verb-unset-vars)

    (should (zerop (length verb--vars)))))

(ert-deftest test-show-vars ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (verb-set-var "test-var-1" 100)
    (verb-set-var "test-var-2" "bar")
    (verb-set-var "test-var-3" "quuz")

    (with-current-buffer (verb-show-vars)
      (should (string= (buffer-string)
                       (join-lines
                        "test-var-3: quuz"
                        "test-var-2: bar"
                        "test-var-1: 100"))))))

(ert-deftest test-verb-set-var-previously-unset ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (setq verb--vars nil)
    (verb-set-var "test" "hello")
    (should (equal verb--vars '((test . "hello"))))))

(ert-deftest test-eval-code-tags-context ()
  (with-temp-buffer
    (should (string= (buffer-name)
		     (verb--eval-code-tags-in-string "{{(buffer-name)}}"
						     (current-buffer))))))

(ert-deftest test-eval-code-tags-context-2 ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (setq fill-column 9000) ; buffer local
    (insert (join-lines "* H1 :verb:"
			"get http://example.com/{{fill-column}}"))
    (should (equal (verb--request-spec-from-hierarchy)
		   (verb-request-spec :method "GET"
				      :url (verb--clean-url
					    "http://example.com/9000")))))
  (with-temp-buffer
    ;; yep!
    (should (= fill-column 70))))

(ert-deftest test-verb-eval-lisp-code-in ()
  (should (string= (verb--eval-code-tags-in-string "1 {{1}}")
		   "1 1"))

  (should (string= (verb--eval-code-tags-in-string "{{}}--")
		   "--"))

  (should (string= (verb--eval-code-tags-in-string "1 {{(+ 1 1)}}")
		   "1 2"))

  (setq hello 99)
  (should (string= (verb--eval-code-tags-in-string "1 {{(+ 1 hello)}}")
		   "1 100"))

  (should (string= (verb--eval-code-tags-in-string "{{\"{{\"}}")
  		   "{{"))

  (setq num-buffers (length (buffer-list)))
  (should (string= (verb--eval-code-tags-in-string "{{(verb-read-file \"test/test.txt\")}}")
  		   "Example text!\n"))
  (should (= (length (buffer-list)) num-buffers))

  (setq testbuf (generate-new-buffer "testbuffer"))
  (should-not (buffer-local-value 'verb-kill-this-buffer testbuf))
  (with-current-buffer testbuf
    (insert "TEST"))

  (should (string= (verb--eval-code-tags-in-string "this is a {{(get-buffer \"testbuffer\")}}")
  		   "this is a TEST"))

  (should (buffer-live-p testbuf))
  (kill-buffer testbuf)

  (should (string= (verb--eval-code-tags-in-string "{{\"}\"}}{{\"}\"}}")
  		   "}}"))

  (should-error (verb--eval-code-tags-in-string "Hello {{asdfasdf}}")))

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

(ert-deftest test-clean-url-url-encoding ()
  ;; Non-ascii example
  (should (equal (url-path-and-query (verb--clean-url "http://example.com?a=ñ"))
		 (cons "/" "a=%C3%B1")))
  (should (equal (url-path-and-query (verb--clean-url "http://example.com?a=hello world"))
		 (cons "/" "a=hello%20world"))))

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

  (should (equal (verb--override-headers (list (cons "A" "b"))
					 (list (cons "a" "c")))
		 (list (cons "a" "c"))))

  (should (equal (verb--override-headers (list (cons "a" "d"))
					 (list (cons "c" "c")))
		 (list (cons "a" "d")
		       (cons "c" "c")))))

(ert-deftest test-override-headers-casing ()
  (should (equal (verb--override-headers '(("Content-Type" . "foo"))
					 '(("content-type" . "bar")))
		 '(("content-type" . "bar"))))

  (should (equal (verb--override-headers '(("Content-Type" . "foo")
					   ("Content-TYPE" . "X"))
					 '(("content-type" . "bar")))
		 '(("content-type" . "bar"))))

  (should (equal (verb--override-headers '(("Content-Type" . "foo")
					   ("Content-TYPE" . "X"))
					 '(("content-type" . "bar")
					   ("Content-TyPE" . "Y")))
		 '(("content-type" . "bar")
		   ("Content-TyPE" . "Y")))))

(ert-deftest test-override-url-queries-casing ()
  (should (equal (verb--override-url-queries
		  (verb--url-query-string-to-alist "foo_bar=X")
		  (verb--url-query-string-to-alist "FOO_BAR=Y"))
		 '(("foo_bar" . "X")
		   ("FOO_BAR" . "Y")))))

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
		       "http://hello.com/user/test?foo&a=2&a=3&quux#baz"))

(ert-deftest test-get-handler ()
  (should (equal (verb--get-handler (cons "image/png" nil))
		 '(image-mode t)))

  (should (equal (verb--get-handler (cons "application/pdf" nil))
		 '(doc-view-mode t)))

  (should (equal (verb--get-handler (cons "application/xml" nil))
		 '(xml-mode))))

(ert-deftest test-get-handler-nil ()
  (should-not (verb--get-handler (cons "application/foobar" nil)))
  (should-not (verb--get-handler (cons nil nil)))
  (should-not (verb--get-handler nil)))

(ert-deftest test-get-handler-regexp ()
  (should (equal (verb--get-handler (cons "image/jpg" nil))
		 '(image-mode t)))

  (should (equal (verb--get-handler (cons "image/jpeg" nil))
		 '(image-mode t)))

  (should (equal (verb--get-handler (cons "text/xml" nil))
		 '(xml-mode)))

  (should (equal (verb--get-handler (cons "application/xml" nil))
		 '(xml-mode))))

(ert-deftest test-encode-http-body ()
  (should (string= (verb--encode-http-body "helló" "utf-8") "hell\303\263"))
  (should (string= (verb--encode-http-body "helló" nil) "hell\303\263"))
  (should (string= (verb--encode-http-body "hello" nil) "hello"))
  (should-error (verb--encode-http-body "helló" "foobar")))

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
		 '(("A" . "a")
		   ("B" . "v"))))

  (should (equal (verb--prepare-http-headers '(("A" . "test")
					       ("B" . "test")
					       ("Content-Type" . "text; charset=hello")))
		 '(("A" . "test")
		   ("B" . "test")
		   ("Content-Type" . "text; charset=hello")))))

(ert-deftest test-to-ascii ()
  (should-not (multibyte-string-p (verb--to-ascii "ññáé"))))

(ert-deftest test-verb-alist ()
  (should-not (verb--alist-p nil))
  (should (verb--alist-p '((1 . 1)
			   (2 . 2))))
  (should-not (verb--alist-p "asdf")))

(ert-deftest test-string= ()
  (should (verb--string= "hello" "hello"))
  (should (verb--string= "HELLO" "hello"))
  (should (verb--string= "HELLO_" "hello_"))
  (should-not (verb--string= "foo" "hello")))

(ert-deftest test-http-method-p ()
  (should (verb--http-method-p "GET"))
  (should (verb--http-method-p "POST"))
  (should-not (verb--http-method-p verb--template-keyword))
  (should-not (verb--http-method-p "test")))

(ert-deftest test-disable-verb-mode-font-lock ()
  (with-temp-buffer
    (should-not font-lock-keywords)
    (verb-mode)
    (should font-lock-keywords)
    (verb-mode -1)
    (should (equal font-lock-keywords '(t nil)))))

(ert-deftest test-disable-verb-mode-completion-at-point ()
  (with-temp-buffer
    (should-not (member #'verb-elisp-completion-at-point completion-at-point-functions))
    (verb-mode)
    (should (member #'verb-elisp-completion-at-point completion-at-point-functions))
    (verb-mode -1)
    (should-not (member #'verb-elisp-completion-at-point completion-at-point-functions))))

(setq elisp-code "system-")

(ert-deftest test-elisp-completion ()
  (with-temp-buffer
    (insert elisp-code)
    ;; Sanity checks
    (let ((comp (elisp-completion-at-point)))
      (should (= (nth 0 comp) 1))
      (should (= (nth 1 comp) 8))))

  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert "{{" elisp-code "}}")
    (backward-char 2)

    (let ((comp (verb-elisp-completion-at-point)))
      (should (= (nth 0 comp) 3))
      (should (= (nth 1 comp) 10)))))

(ert-deftest test-elisp-completion-fn-installed ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (should (member #'verb-elisp-completion-at-point
                    completion-at-point-functions))))

(ert-deftest test-elisp-completion-outside-tag ()
    (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert elisp-code)
    (should-not (verb-elisp-completion-at-point))))

(ert-deftest test-elisp-completion-partial-tag ()
    (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert "{{" elisp-code)
    (should-not (verb-elisp-completion-at-point))))

;; Tests using the test server (server.py)

(setq test-file-name (expand-file-name "test/test.org"))
(setq test-buf (find-file test-file-name))
(with-current-buffer test-buf (verb-mode))
(setq req-sleep-time 0.01)

(defmacro server-test (test-name &rest body)
  (declare (indent 1))
  `(progn
     (set-buffer test-buf)
     (goto-char (point-min))
     (re-search-forward (concat "^\\*+ " ,test-name "$"))
     (let ((inhibit-message t))
       (with-current-buffer (verb-send-request-on-point nil)
         (while (eq verb-http-response t)
           (sleep-for req-sleep-time)
           (when (eq verb-url-retrieve-function #'url-queue-retrieve)
             (ert-run-idle-timers)))
         ,@body))))

(defun clear-log ()
  (with-current-buffer verb--log-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun should-log-contain (s)
  (with-current-buffer verb--log-buffer-name
    (save-excursion
      (goto-char (point-min))
      (should (search-forward s)))))

(defun test-log ()
  (clear-log)

  (verb--log nil 'I "Hello, World!")
  (should-log-contain "- I Hello, World!")

  (verb--log 1 'I "Hello, World!")
  (should-log-contain "1 I Hello, World!")

  (should-error (verb--log 1 'X "foo")))

(ert-deftest test-response-buffer-name ()
  (let ((verb--requests-count 41)
        (verb-auto-kill-response-buffers nil))
    (server-test "basic"
      (should (string-match-p "\\*HTTP Response 42\\*"
			      (buffer-name)))))

  (let ((verb-auto-kill-response-buffers t))
    (server-test "basic"
      (should (string= "*HTTP Response*" (buffer-name))))))

(ert-deftest test-request-body-warning ()
  (clear-log)
  (server-test "get-with-body"
    (should-log-contain "Body is present but request method is GET")))

(defun get-response-buffers ()
  (seq-filter (lambda (b) (buffer-local-value 'verb-http-response b))
	      (buffer-list)))

(ert-deftest test-kill-all-response-buffers ()
  (let ((n (length (get-response-buffers)))
        (verb-auto-kill-response-buffers nil))
    (server-test "basic")
    (server-test "basic-json")
    (should (= (length (get-response-buffers)) (+ n 2)))
    (with-current-buffer test-buf
      (verb-kill-all-response-buffers))
    (should (zerop (length (get-response-buffers))))))

(ert-deftest test-re-send-request ()
  (server-test "basic"
    (erase-buffer)
    (should (string= (buffer-string) ""))
    (with-current-buffer (verb-re-send-request)
      (while (eq verb-http-response t)
        (sleep-for req-sleep-time))
      (should (string= (buffer-string) "Hello, World!")))))

(ert-deftest test-send-no-window ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* test :verb:"
                        "get http://localhost:8000/basic"))
    (let ((w (selected-window))
          (b (current-buffer))
          (resp (verb-send-request-on-point-no-window))
          (inhibit-message t))
      (while (eq (buffer-local-value 'verb-http-response resp) t)
        (sleep-for req-sleep-time))
      (should (equal w (selected-window)))
      (should (equal b (current-buffer))))))

(ert-deftest test-c-u-temp-buffer-contents ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* test :verb:"
                        "get http://example.org/{{(+ 40 2)}}/"))
    ;; C-u M-x verb-send-request-on-point
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'verb--split-window) (lambda () (selected-window))))
        (call-interactively 'verb-send-request-on-point)))
    (should (string= (buffer-name) "*Edit HTTP Request*"))
    (goto-char (point-min))

    ;; Code tags should not have been evaluated
    (should (search-forward "{{(+ 40 2)}}"))))

(ert-deftest test-c-u-verb-var ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (verb-var testvar1 "World!")
    (insert (join-lines "* test :verb:"
                        "post http://localhost:8000/echo"
                        ""
                        "Hello, {{(verb-var testvar1)}}"))
    ;; C-u M-x verb-send-request-on-point
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'verb--split-window) (lambda () (selected-window))))
        (call-interactively 'verb-send-request-on-point)))
    (should (string= (buffer-name) "*Edit HTTP Request*"))

    ;; C-c C-c
    (with-current-buffer (call-interactively (local-key-binding (kbd "C-c C-c")))
      (while (eq verb-http-response t)
        (sleep-for req-sleep-time))
      (should (string= (buffer-string) "Hello, World!")))))

(ert-deftest test-c-u-send-request ()
  (setq verb--stored-responses nil)
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* test :verb:"
                        ":properties:"
                        ":verb-store: test-c-u"
                        ":end:"
                        "# comment"
                        "post http://localhost:8000/echo"
                        "Accept: text/plain"
                        ""
                        "Foobar"))

    ;; C-u M-x verb-send-request-on-point
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'verb--split-window) (lambda () (selected-window))))
        (call-interactively 'verb-send-request-on-point)))
    (should (string= (buffer-name) "*Edit HTTP Request*"))
    (goto-char (point-max))
    (insert "!!??")

    ;; C-c C-c
    (with-current-buffer (call-interactively (local-key-binding (kbd "C-c C-c")))
      (while (eq verb-http-response t)
        (sleep-for req-sleep-time))
      (should (string= (buffer-string) "Foobar!!??"))
      (should (string= (oref (verb-stored-response "test-c-u") body) "Foobar!!??")))))

(ert-deftest test-repeated-args ()
  (server-test "repeated-args"
    (should (string= (buffer-string)
		     (join-lines "a=1"
				 "a=2"
				 "a=3"
				 "b=42")))))

(ert-deftest test-object-of-class-p ()
  (should (verb--object-of-class-p (verb-request-spec) 'verb-request-spec))
  (should (verb--object-of-class-p (verb-response) 'verb-response))
  (should-not (verb--object-of-class-p t 'verb-response))
  (should-not (verb--object-of-class-p nil 'verb-response))
  (should-not (verb--object-of-class-p 42 'verb-response)))

(ert-deftest test-server-basic-queue ()
  (let ((verb-url-retrieve-function #'url-queue-retrieve))
    (server-test "basic"
      (should (string= (buffer-string) "Hello, World!")))))

(ert-deftest test-server-basic ()
  (server-test "basic"
    (should (string= (buffer-string) "Hello, World!"))
    (should (eq major-mode 'text-mode))
    (should verb-response-body-mode)
    (should (verb--object-of-class-p verb-http-response 'verb-response))
    (should (oref verb-http-response request))
    (let ((req-url (verb-request-spec-url-to-string (oref verb-http-response request))))
      (should (string= req-url (if (< emacs-major-version 26)
                                   "http://127.0.0.1:8000/basic"
                                 "http://localhost:8000/basic"))))))

(ert-deftest test-server-basic-json ()
  (let ((verb-json-max-pretty-print-size nil))
    (server-test "basic-json"
		 (should (string= (buffer-string)
				  "{\"foo\":true,\"hello\":\"world\"}"))
		 (should (eq major-mode 'js-mode)))))

(ert-deftest test-body-bytes ()
  (server-test "basic"
    (should (= 13 (oref verb-http-response body-bytes)))
    (should (= 13 (length (oref verb-http-response body))))
    (should (= 13 (buffer-size)))))

(define-derived-mode fake-handler-mode fundamental-mode "fake"
  "docs")

(ert-deftest test-binary-image ()
  ;; Can't really display images during testing, mock the handler
  (let ((verb-content-type-handlers '(("image/png" fake-handler-mode t))))
    (should (equal (verb--get-handler (cons "image/png" nil))
		   '(fake-handler-mode t)))
    (server-test "image"
      (should (equal major-mode 'fake-handler-mode))
      (should (= (oref verb-http-response body-bytes) 4959))
      (should-not enable-multibyte-characters)
      (should (coding-system-equal buffer-file-coding-system 'binary)))))

(ert-deftest test-unknown-content-type ()
  (let ((verb-content-type-handlers nil))
    (server-test "basic-json"
      (should (equal major-mode 'fundamental-mode))
      (should (string= (buffer-string) "{\"foo\":true,\"hello\":\"world\"}")))))

(ert-deftest test-repeated-header ()
  (clear-log)
  (server-test "repeated-sorted-headers"
    (should-log-contain "Header \"MIME-Version\" will appear duplicated")))

(ert-deftest test-server-basic-json-pretty ()
  (let ((verb-json-max-pretty-print-size 99999))
    (server-test "basic-json"
		 (should (string= (buffer-string)
				  "{\n  \"foo\": true,\n  \"hello\": \"world\"\n}"))
		 (should (eq major-mode 'js-mode)))))

(ert-deftest test-server-keywords-json-pretty ()
  (let ((verb-json-max-pretty-print-size 99999))
    (server-test "keywords-json"
		 (should (string= (buffer-string)
				  "{\n  \"t\": true\n}"))
		 (should (eq major-mode 'js-mode)))))

(ert-deftest test-server-basic-json-mode ()
  (let ((verb-json-use-mode #'html-mode))
    (server-test "basic-json"
      (should (eq major-mode 'html-mode)))))

(ert-deftest test-server-error-400 ()
  (server-test "error-400"
    (should (string-match "400" header-line-format))))

(ert-deftest test-server-error-401 ()
  ;; url.el should not handle 401 in any way
  (server-test "error-401"
    (should (string-match "401" header-line-format))))

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

(ert-deftest test-server-request-utf-8-default-2 ()
  (server-test "request-utf-8-default-2"
    (should (string= (buffer-string) "OK"))))

(ert-deftest test-server-request-utf-8-with-accept ()
  (server-test "utf-8-request-with-accept"
    (should (string= (buffer-string) "語"))))

(ert-deftest test-server-response-big5 ()
  (server-test "response-big5"
    (should (coding-system-equal buffer-file-coding-system 'chinese-big5-unix))
    (should (string-match "常用字" (buffer-string)))))

(ert-deftest test-server-response-big5-size ()
  (server-test "response-big5"
    ;; Six bytes, but three "characters"
    (should (= 6 (oref verb-http-response body-bytes)))
    (should (= 3 (buffer-size)))
    (should (= 3 (length (oref verb-http-response body))))))

(ert-deftest test-server-response-utf-8-default ()
  (server-test "response-utf-8-default"
	       (should (string= (cdr (assoc-string "Content-Type"
						   (oref verb-http-response headers)))
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

(ert-deftest test-no-user-agent ()
  ;; default user agent from url.el should not be included
  (server-test "no-user-agent"
    (should (string= (buffer-string) "OK"))))

(ert-deftest test-image-upload-md5 ()
  (server-test "upload-image"
    (should (string= (buffer-string)
                     "935ef9d8ab56be5b6265becf6135e1d9"))))

(ert-deftest test-bin-upload-md5 ()
  (server-test "upload-binary"
    (should (string= (buffer-string)
                     "aa0f16d7831947b778dac603c29871fd"))))

(ert-deftest test-content-length-request ()
  (server-test "content-length-1"
    (should (string= (buffer-string) "OK")))
  (server-test "content-length-2"
    (should (string= (buffer-string) "OK"))))

(ert-deftest test-buffers-created ()
  (let ((verb-auto-kill-response-buffers nil))
    (setq num-buffers (length (buffer-list)))
    (server-test "basic")
    (should (= (1+ num-buffers) (length (buffer-list))))))

(ert-deftest test-kill-buffer-and-window ()
  (setq num-buffers (length (buffer-list)))
  (let ((verb--requests-count 0))
    (server-test "basic"))
  (switch-to-buffer "*HTTP Response 1*")
  (verb-kill-buffer-and-window)
  (should (= num-buffers (length (buffer-list)))))

(ert-deftest test-show-request ()
  (let ((verb-auto-kill-response-buffers t))
    (server-test "request-latin-1"
      (with-current-buffer (verb-show-request)
        (should (string=
                 (string-trim (buffer-string))
                 (join-lines
                  "* Corresponding HTTP request for response in *HTTP Response*"
                  (format "POST %s/request-latin-1" (if (< emacs-major-version 26)
                                                        "http://127.0.0.1:8000"
                                                      "http://localhost:8000"))
                  "Content-Type: text/plain; charset=latin1"
                  ""
                  "áéíóúñü")))))))

(ert-deftest test-auto-kill-buffers ()
  (let ((num-buffers (length (buffer-list)))
        (verb-auto-kill-response-buffers t))
    (server-test "basic")
    (server-test "basic-json")
    (server-test "no-user-agent")
    (garbage-collect)
    (should (= (1+ num-buffers) (length (buffer-list))))))

(ert-deftest test-headers ()
  (server-test "headers"
    (should (string= (buffer-string) "HeadersTest"))
    (should (string= (cdr (assoc "x-test-1"
				 (oref verb-http-response headers))) "foo"))
    (should (string= (cdr (assoc "OTHER-TEST"
				 (oref verb-http-response headers))) "bar"))))

(ert-deftest test-zero-bytes-json ()
  (server-test "zero-bytes-json"
    (should (zerop (buffer-size)))
    (should (string-match "200" header-line-format))))

(ert-deftest test-default-headers-accept ()
  (server-test "accept-sorted-headers"
    (goto-char (point-min))
    (should (search-forward "accept: hello-world"))
    (goto-char (point-min))
    (should (= (count-matches "accept:") 1))))

(ert-deftest test-default-headers ()
  (server-test "sorted-headers"
    (let ((headers '("mime-version"
		     "connection"
		     "content-length"
		     "host"
		     "accept"
		     "accept-encoding"
		     "extension")))
      (dolist (h headers)
	(goto-char (point-min))
	(should (search-forward (concat h ": "))))

      (goto-char (point-min))
      (delete-matching-lines "cookie") ; ignore cookie header

      (should (= (count-lines (point-min) (point-max))
		 (length headers))))))

(setq test-json (join-lines "{"
			    "  \"foo\": {"
			    "    \"test\": \"Hello, World!\","
			    "    \"nested\": {\"x\": 99}"
			    "  },"
			    "  \"bar\": [42, 100, true, false, null],"
			    "  \"empty-array\":  [],"
			    "  \"empty-object\": {}"
			    "}"))

(ert-deftest test-headers-get ()
  (setq test-headers '(("Content-Type" . "application/json")
		       ("Accept" . "")))
  (should (string= (verb-headers-get test-headers "Content-Type")
		   "application/json"))
  (should (string= (verb-headers-get test-headers "Content-type")
		   "application/json"))
  (should (string= (verb-headers-get test-headers "content-Type")
		   "application/json"))
  (should (string= (verb-headers-get test-headers "accept")
		   ""))
  (should-error (verb-headers-get "foo")))

(ert-deftest test-json-get ()
  (should-error (verb-json-get test-json))
  (should-error (verb-json-get test-json 1.0))
  (should-error (verb-json-get test-json '(1 2 3)))

  (should (string= (verb-json-get test-json "foo" "test")
		   "Hello, World!"))

  (should (= (verb-json-get test-json "foo" "nested" "x")
	     99))

  (should (= (verb-json-get test-json "foo" 'nested 'x)
	     99))

  (should-not (verb-json-get test-json "foo" "nested" "abc"))

  (should (= (verb-json-get test-json "bar" 0) 42))
  (should (= (verb-json-get test-json "bar" 1) 100))

  (should (verb-json-get test-json "bar" 2))
  (should-error (verb-json-get test-json "bar" 999))
  (should (eq (verb-json-get test-json "bar" 3) :json-false))
  (should-not (verb-json-get test-json "bar" 4))

  (should-not (verb-json-get test-json "asfdasdfsaf"))

  (should (equal (verb-json-get test-json "empty-array") []))
  (should-not (verb-json-get test-json "empty-object")))

(ert-deftest test-verb-last ()
  (server-test "basic")
  (should (string= (oref verb-last body) "Hello, World!"))

  (server-test "error-400")
  (should (null (oref verb-last body))))

(ert-deftest test-server-stored ()
  (server-test "stored"
    (setq stored-resp (verb-stored-response "foobar"))
    (should (string= (oref stored-resp body)
		     "Hello, World!"))))

(ert-deftest test-cookies ()
  (setq verb-inhibit-cookies nil)
  (setq url-cookie-storage nil)
  (setq url-cookie-secure-storage nil)

  (server-test "get-cookies"
    (should (= (buffer-size) 0)))

  (server-test "set-cookies"
    (should (string= (buffer-string) "OK")))

  (server-test "get-cookies"
    (should (string= (buffer-string) "foo=bar\nabc=123\n")))

  (server-test "delete-cookies"
    (should (string= (buffer-string) "OK")))

  (server-test "get-cookies"
    (should (or (= (buffer-size) 0)
                (string= (buffer-string)
                         "foo=\nabc=\n")))))

(ert-deftest test-cookies-disabled ()
  (setq verb-inhibit-cookies t)

  (server-test "get-cookies"
    (should (= (buffer-size) 0)))

  (server-test "set-cookies"
    (should (string= (buffer-string) "OK")))

  (server-test "get-cookies"
    (should (= (buffer-size) 0))))

(ert-deftest test-no-stored-response ()
  (should-error (verb-stored-response "adfsadfsadf")))

(ert-deftest test-connection-error-port ()
  (clear-log)
  (setq num-buffers (length (buffer-list)))
  (should-error (server-test "connection-fail-port"))
  (should (= num-buffers (length (buffer-list))))
  (should-log-contain "Request error")
  (should-log-contain "Error details"))

(ert-deftest test-connection-error-host ()
  (skip-unless (eq system-type 'darwin))
  (clear-log)
  (setq num-buffers (length (buffer-list)))
  (should-error (server-test "connection-fail-host"))
  (should (= num-buffers (length (buffer-list))))
  (should-log-contain "Error sending request"))

(ert-deftest test-request-spec-send-eww ()
  (skip-unless (>= emacs-major-version 27))
  (setq test-rs (text-as-spec-nl "GET http://localhost:8000/sorted-headers"
                                 "Accept: text/xhtml"
                                 "Foo: Bar123"))
  (verb--request-spec-send-eww test-rs)
  (with-current-buffer (get-buffer "*eww*")
    (sleep-for 0.5)
    (goto-char (point-min))
    (should (search-forward "Accept: text/xhtml"))
    (should (search-forward "Foo: Bar123"))))

(defun should-curl (rs-text &rest lines)
  (should (string= (verb--export-to-curl
		    (verb-request-spec-from-string rs-text) t)
		   (apply #'join-lines lines))))

(ert-deftest test-curl-export ()
  (should-curl (join-lines "GET http://example.com")
	       "curl 'http://example.com'")

  (should-curl (join-lines
		"GET http://example.com"
		"Header1: Val1")
	       "curl 'http://example.com' \\\n-H 'Header1: Val1'")

  (should-curl (join-lines
		"POST http://example.com"
		"Header1: Val1")
	       "curl 'http://example.com' \\\n-H 'Header1: Val1' \\\n-X POST \\\n--data-raw ''")

  (should-curl (join-lines "DELETE http://example.com")
	       "curl 'http://example.com' -X DELETE")

  (should-curl (join-lines "TRACE http://example.com")
	       "curl 'http://example.com' -X TRACE")

  (should-curl (join-lines "POST http://example.com")
	       "curl 'http://example.com' -X POST \\\n--data-raw ''")

  (should-curl (join-lines
		"POST http://example.com"
		""
		"Some content")
	       "curl 'http://example.com' -X POST \\\n--data-raw 'Some content'")

  (should-curl (join-lines
		"PUT http://example.com"
		""
		"Some content"
		"Multiple lines")
	       "curl 'http://example.com' -X PUT \\\n--data-raw 'Some content\nMultiple lines'")

  (should-curl (join-lines
		"PUT http://example.com"
		"A: B"
		""
		"Some content"
		"Multiple lines")
	       "curl 'http://example.com' \\\n-H 'A: B' \\\n-X PUT \\\n--data-raw 'Some content\nMultiple lines'")

  (should-curl (join-lines
		"PATCH http://example.com"
		""
		"ñáéíóúß")
	       "curl 'http://example.com' -X PATCH \\\n--data-raw 'ñáéíóúß'")

  (should-curl (join-lines "OPTIONS http://example.com")
	       "curl 'http://example.com' -X OPTIONS -i")

  (should-curl (join-lines "HEAD http://example.com")
	       "curl 'http://example.com' -I")

  (should-error (verb--export-to-curl
		 (verb-request-spec-from-string
		  "CONNECT http://abc.com"))))

(ert-deftest test-verb-export ()
  (setq test-spec (text-as-spec-nl "PUT https://hello.com"
				   "Test: Foobar"
				   "Test2: Foo"
				   ""
				   "Body contents"))
  (with-current-buffer (verb--export-to-verb test-spec)
    (should (string= (verb--buffer-string-no-properties)
		     (verb-request-spec-to-string test-spec)))
    ;; We should be able to parse the string back to a request-spec and
    ;; get the same thing
    (should (equal test-spec (verb-request-spec-from-string
			      (verb--buffer-string-no-properties))))))

(ert-deftest test-verb-export-eww ()
  (setq test-spec (text-as-spec "PUT https://hello.com"))
  (should-error (verb--export-to-eww test-spec)))

(defun replace-all (from to)
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t)))

(ert-deftest test-babel-single-block ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "#+begin_src verb"
			"template http://localhost:8000/basic"
			"#+end_src"))
    (re-search-backward "template")
    (should-error (org-ctrl-c-ctrl-c))))

(ert-deftest test-babel-request-name ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "* test"
			":properties:"
			":Verb-name: JOHN"
			":end:"
			""
			"#+begin_src verb"
			"get http://localhost:8000/basic"
			"#+end_src"))
    (re-search-backward "get")
    (org-ctrl-c-ctrl-c)
    (should (equal (oref (oref verb-last request) metadata)
		   '(("VERB-NAME" . "JOHN"))))))

(ert-deftest test-babel-invalid-op ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "#+begin_src verb :op foobar"
			"get http://localhost:8000/basic"
			"#+end_src"))
    (re-search-backward "get")
    (should-error (org-ctrl-c-ctrl-c))))

(ert-deftest test-babel-invalid-export ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "#+begin_src verb :op export foo"
			"get http://localhost:8000/basic"
			"#+end_src"))
    (re-search-backward "get")
    (should-error (org-ctrl-c-ctrl-c))))

(ert-deftest test-babel-invalid-send-arg ()
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert (join-lines "#+begin_src verb :op send foo"
			"get http://localhost:8000/basic"
			"#+end_src"))
    (re-search-backward "get")
    (should-error (org-ctrl-c-ctrl-c))))

(defun babel-test (input output)
  (with-temp-buffer
    (org-mode)
    (verb-mode)
    (insert input)
    (search-backward "#+begin_src")
    (let ((inhibit-message t)
          start
          result)
      (setq result (org-ctrl-c-ctrl-c))
      (when (< emacs-major-version 26)
        ;; In Emacs 25, `org-ctrl-c-ctrl-c' does not return the result
        (search-forward "#+results:")
        (forward-char)
        (setq start (point))

        ;; Two cases to handle
        (if (looking-at-p ": ")
            ;; Results have been inserted in lines with ": "
            (while (re-search-forward "^: " nil t)
              (replace-match ""))
          ;; Results have been inserted inside begin/end_src blocks
          (delete-matching-lines "#\\+\\(begin\\|end\\)_\\(src\\|example\\)"))

        ;; Replace 127.0.0.1 - > localhost
        (goto-char (point-min))
        (while (re-search-forward "127\\.0\\.0\\.1" nil t)
          (replace-match "localhost"))

        ;; Set the result
        (setq result (buffer-substring start (point-max))))

      (should (string= (string-trim-right result)
                       (string-trim-right output))))))

(ert-deftest test-babel-curl ()
  (babel-test (join-lines "#+begin_src verb :op export curl"
			  "post http://example.org?test=foo"
			  "Content-Type: application/json"
			  ""
			  "{}"
			  "#+end_src")
	      (join-lines "curl 'http://example.org/?test=foo' \\"
			  "-H 'Content-Type: application/json' \\"
			  "-X POST \\"
			  "--data-raw '{}'")))

(ert-deftest test-babel-verb-format ()
  (babel-test (join-lines "#+begin_src verb :op export verb"
			  "delete http://example.org"
                          "Some: Header"
			  "#+end_src")
	      (join-lines "DELETE http://example.org"
			  "Some: Header\n")))

(ert-deftest test-babel-send ()
  (babel-test (join-lines "* Heading 1    :verb:"
                          "template http://localhost:8000"
                          "** heading 2"
                          "get"
                          "Accept: application/json"
                          "*** Heading 3"
                          "#+begin_src verb :wrap src ob-verb-response"
                          "get /basic-json"
                          "#+end_src")
              (join-lines "HTTP/1.1 200 OK"
                          "Content-Length: 28"
                          "Content-Type: application/json"
                          "Connection: keep-alive"
                          "Keep-Alive: 5"
                          ""
                          "{"
                          "  \"foo\": true,"
                          "  \"hello\": \"world\""
                          "}")))

(ert-deftest test-babel-send-get-body ()
  (babel-test (join-lines "* Heading 1"
			"#+begin_src verb :op send get-body"
			"get http://localhost:8000/basic"
			"#+end_src")
	      (join-lines "Hello, World!")))

(ert-deftest test-babel-send-get-body-tagged ()
  (babel-test (join-lines "* Heading 1            :verb:"
			"#+begin_src verb :op send get-body"
			"get http://localhost:8000/basic"
			"#+end_src")
	      (join-lines "Hello, World!")))

(ert-deftest test-babel-send-get-headers ()
  (babel-test (join-lines "* Heading 1"
                          "#+begin_src verb :op send get-headers"
                          "get http://localhost:8000/basic-json"
                          "#+end_src")
              (join-lines "Content-Length: 28"
                          "Content-Type: application/json"
                          "Connection: keep-alive"
                          "Keep-Alive: 5")))

(ert-deftest test-babel-base-headers ()
  (let ((verb-base-headers '(("Foo" . "Bar")
                             ("Quux" . "Quuz"))))
    (babel-test (join-lines "#+begin_src verb"
                            "get http://localhost:8000/sorted-headers?dropcookies=1"
                            "Foo: XYZ"
                            "#+end_src")
                (join-lines "HTTP/1.1 200 OK"
                            "Content-Type: text/plain"
                            "Content-Length: 155"
                            "Connection: keep-alive"
                            "Keep-Alive: 5"
                            ""
                            "accept-encoding: gzip"
                            "accept: */*"
                            "connection: keep-alive"
                            "extension: Security/Digest Security/SSL"
                            "foo: XYZ"
                            "host: localhost:8000"
                            "mime-version: 1.0"
                            "quux: Quuz"))))

(ert-deftest test-babel-org-variables ()
  ;; Use :var for each variable
  (babel-test (join-lines "* Heading 1"
                          "#+begin_src verb :op send get-body :var x=1 :var y=\"foo\""
                          "get http://localhost:8000/echo-args?a={{(verb-var x)}}&b={{(verb-var y)}}"
                          "#+end_src")
              (join-lines "a=1"
                          "b=foo"))

  ;; Use :var only once
  (babel-test (join-lines "* Heading 1"
                          "#+begin_src verb :op send get-body :var x=2 y=\"bar\""
                          "get http://localhost:8000/echo-args?a={{(verb-var x)}}&b={{(verb-var y)}}"
                          "#+end_src")
              (join-lines "a=2"
                          "b=bar")))

(defun babel-src-test (input output)
  (should (string= (verb--maybe-extract-babel-src-block input)
		   output)))

(ert-deftest test-maybe-extract-babel-source-block ()
  (babel-src-test (join-lines "hello")
		  (join-lines "hello"))

  (babel-src-test (join-lines "")
		  (join-lines ""))

  (babel-src-test (join-lines "" "")
		  (join-lines "" ""))

  (babel-src-test (join-lines "test"
			      "#+begin_src verb"
			      "hello world"
			      "#+end_src")
		  (join-lines "hello world"))

  (babel-src-test (join-lines "test"
			      "#+begin_src verb"
			      "#+end_src")
		  (join-lines ""))

  (babel-src-test (join-lines "test"
			      "#+begin_src verb"
			      ""
			      "#+end_src")
		  (join-lines ""))

  (babel-src-test (join-lines "test"
			      "#+begin_src verb"
			      "something"
			      "something else"
			      "#+end_src"
			      "test"
			      "#+begin_src verb"
			      "101010101"
			      "#+end_src"
			      )
		  (join-lines "something"
			      "something else"))

  (babel-src-test (join-lines "test"
			      "#+begin_src verb"
			      "hello world"
			      "#+end_src"
			      "trailing stuff")
		  (join-lines "hello world"))

  (babel-src-test (join-lines "test"
			      "#+begin_src verb"
			      "hello world"
			      "foobar"
			      "#+end_src"
			      "trailing stuff")
		  (join-lines "hello world"
			      "foobar"))

  (babel-src-test (join-lines "test"
			      "#+begin_SRC verb"
			      "hello world"
			      "#+eNd_src")
		  (join-lines "hello world"))

  (babel-src-test (join-lines "test"
			      "#+begin_SRC verb abcdef"
			      "hello world"
			      "#+eNd_src")
		  (join-lines "hello world")))


(ert-deftest test-verb-check-response-buffer ()
  (with-current-buffer (get-buffer-create "*new buffer*")
    (should-error (verb--check-response-buffer))
    (setq verb-http-response (verb-response))
    (verb--check-response-buffer)))


(ert-deftest test-verb-get-accept-header ()
  (should (equal (verb--get-accept-header '(("Foo" . "Bar") ("Quu" . "Quz")))
                 "*/*"))

  (should (equal (verb--get-accept-header '(("Foo" . "Bar") ("Accept" . "abc")))
                 "abc"))

  (should (equal (verb--get-accept-header '(("Foo" . "Bar") ("accept" . "xyz")))
                 "xyz")))

(ert-deftest test-generate-multipart-boundary ()
  (let ((boundary (verb--generate-multipart-boundary))
        (boundary2 (verb--generate-multipart-boundary)))
    (should (= (length boundary) 64))
    (should-not (string= boundary boundary2))))

(ert-deftest test-multipart-boundary ()
  (setq aux (text-as-spec-nl "get https://hello.com"
				             "Content-Type: multipart/form-data; boundary={{(verb-boundary \"abc\")}}"
				             ""
				             "{{(verb-part \"foobar-1\")}}"
                             "Content-Type: text/plain"
                             ""
                             "file-contents-here"
				             "{{(verb-part \"foobar-2\")}}"
                             "Content-Type: text/plain"
                             ""
                             "other-file-contents-here"
                             "{{(verb-part)}}"))
  (should (string= (oref aux body)
                   (join-lines "--abc\r"
                               "Content-Disposition: form-data; name=\"foobar-1\""
                               "Content-Type: text/plain"
                               ""
                               "file-contents-here"
                               "--abc\r"
                               "Content-Disposition: form-data; name=\"foobar-2\""
                               "Content-Type: text/plain"
                               ""
                               "other-file-contents-here"
                               "--abc--"))))

(ert-deftest test-multipart-boundary-error-no-boundary ()
  (should-error
   (text-as-spec-nl "get https://hello.com"
                    ""
                    "{{(verb-part \"foobar\")}}")))

(ert-deftest test-multipart-boundary-warning-on-no-final-delimiter ()
  (clear-log)
  (text-as-spec-nl "get https://hello.com"
				   "Content-Type: multipart/form-data; boundary={{(verb-boundary \"abc\")}}"
				   ""
				   "{{(verb-part \"foobar-1\")}}"
                   "Content-Type: text/plain"
                   ""
                   "file-contents-here") ;; No final boundary
  (should-log-contain "Detected an unfinished multipart form"))

(provide 'verb-test)
;;; verb-test.el ends here
