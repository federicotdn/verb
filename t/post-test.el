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

(provide 'post-test)
;;; post.el ends here
