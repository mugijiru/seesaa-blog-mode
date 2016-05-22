;;; seesaa-blog-mode-test.el --- test for seesaa-blog-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Mugijiru

;; Author: Mugijiru <mugijiru.dev@gmail.com>
;; Keywords: seesaa blog

(require 'ert)
(require 'seesaa-blog-mode)


(ert-deftest seesaa-blog-mode-test/build-html ()
  (let* ((org "#+TITLE: Seesaa Blog Build HTML Test

* test

foobar
")
         (html (seesaa-blog-build-html-from-org-string org))

         (expect "
<div id=\"outline-container-sec-1\" class=\"outline-4\">
<h4 id=\"sec-1\">test</h4>
<div class=\"outline-text-4\" id=\"text-1\">
<p>\nfoobar
</p>
</div>
</div>
"))
    (should (equal html expect)))
)
