;;; seesaa-blog-mode.el --- Create org file for seesaa blog and Post HTML

;; Author: Mugijiru <mugijiru.dev@gmail.com>
;; URL: https://github.com/mugijiru/seesaa-blog-mode
;; Version: 0.0.1
;; Keywords: seesaa blog org-mode

;; Copyright (c) 2016 Mugijiru
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; seesaa-blog-mode.el is a tool for seesaa blog.
;;
;; This script only support Mac OS X
;;

;;; Code:

(require 'xml-rpc)
(require 'f)
(require 's)
(require 'org)
(defvar seesaa-blog-id nil)
(defvar seesaa-blog-email nil)
(defvar seesaa-blog-password nil)
(defvar seesaa-blog-title nil)
;;; これが投げるデータだから下で生成するやつ
;;; (defvar seesaa-blog-content nil)

(defvar seesaa-blog-entry-dir "~/seesaa_entry")
(defvar seesaa-blog-filename-prefix-format "%Y-%m-%d-%H%M%S-")
(defvar seesaa-blog-tmp-file-path nil)

(defvar seesaa-blog-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'seesaa-blog-build-html)
    (define-key map (kbd "C-c C-p") 'seesaa-blog-publish)
    map)
  "keymap for `seesaa-blog-mode'.")

(defun seesaa-blog-disable-org-function ()
  (make-local-variable 'org-export-html-postamble)
  (make-local-variable 'org-export-with-toc)
  (make-local-variable 'org-export-with-section-numbers)
  (setq org-html-toplevel-hlevel 4)
  (setq org-export-html-postamble nil)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil))


(defun seesaa-blog-build-html ()
  "Build HTML for seesaa blog from org file and preview on emacs-w3m.
"
  (interactive)
  (let* ((file (file-name-nondirectory
                (file-name-sans-extension (buffer-file-name))))
         (path (concat "/tmp/" file ".html")))
    (setq seesaa-blog-tmp-file-path path)
    (save-excursion
      (beginning-of-buffer))
    (org-export-to-file 'html path nil nil t)
    ;; window を分割
    (split-window-right)
    ;; 分割先に移動
    (other-window 1)
    (w3m-find-file path)
    (other-window 1)))

(defun seesaa-blog-build-html-from-org-string (str)
  (with-temp-buffer
    (seesaa-blog-disable-org-function)
    (insert str)
    (org-export-as 'html nil t t)))

(defun seesaa-blog-html ()
  "生成されたHTML を preview"
  (interactive)
  (let* ((file (file-name-nondirectory
                (file-name-sans-extension (buffer-file-name))))
         (path (concat "/tmp/" file ".html")))
    (setq seesaa-blog-tmp-file-path path)
    (save-excursion
      (beginning-of-buffer)
      ())
    (org-export-to-file 'html path nil nil t t)
    (f-read path 'utf-8)))

(defun seesaa-blog-content-title ()
  "org ファイルからtitle部分を取得する。"
  (save-excursion
    (beginning-of-buffer)
    (end-of-line)
    (buffer-substring-no-properties (point-min) (point))))

(defun seesaa-blog-get-title ()
  (let ((title-org (seesaa-blog-content-title)))
    (s-replace "#+TITLE: " "" title-org)))

(defun seesaa-blog-content-body ()
  "org ファイルからtitle以外の部分を取得する。"
  (save-excursion
    (beginning-of-buffer)
    (forward-line)
    (buffer-substring-no-properties (point) (point-max))))

(defun seesaa-blog-contents ()
  "org ファイルからHTMLを取得する"
  (let ((content-org (seesaa-blog-content-body)))
    (with-temp-buffer
      (insert content-org)
      (org-html-export-as-html t t t t)
      (set-buffer (get-buffer "*Org HTML Export*"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun seesaa-blog-publish ()
  "Build HTML from org file and publish to seesaa blog use seesaa blog metaweblog api."
  (interactive)
  (xml-rpc-method-call "http://blog.seesaa.jp/rpc"
                       "metaWeblog.newPost"
                       seesaa-blog-id
                       seesaa-blog-email
                       seesaa-blog-password
                       `(("title" . ,(seesaa-blog-get-title))
                         ("description" . ,(seesaa-blog-html)))
                       nil))


(defun seesaa-blog-new-entry (subject)
  "Create new org file for blog entry and enable seesaa-blog-mode."
  (interactive "sタイトル: ")
  (let* ((filename-prefix (format-time-string seesaa-blog-filename-prefix-format (current-time)))
         (filename-suffix ".org")
         (filename (concat filename-prefix (s-replace " " "_" subject) filename-suffix))
         (filepath (concat (expand-file-name seesaa-blog-entry-dir) "/" filename)))
    (setq seesaa-blog-title subject)
    (make-directory seesaa-blog-entry-dir t)
    (find-file-other-window filepath)
    (seesaa-blog-mode 1)
    (insert (concat "#+TITLE: " subject "\n\n"))
    (end-of-buffer)))

(define-minor-mode seesaa-blog-mode
  "Post to Seesaa Blog"
  :keymap seesaa-blog-map
  :lighter " SeB"
  :global nil
  (seesaa-blog-disable-org-function))

(provide 'seesaa-blog-mode)

;;; seesaa-blog-mode.el ends here
