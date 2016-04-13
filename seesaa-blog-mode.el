;;; org-mode をメジャーモードにして
;;; こちらをminor modeにして
;;; C-M-o とかでこちらを起動して
;;; org-mode のファイルを作って
;;; ... って、C-M-o で起動するってのを用意しないといけないか。
;;; それって minor-mode で出来たっけ。できるよな。
;;; こっち側で用意しなければいいのか。

(require 'xml-rpc)

(defvar seesaa-blog-id nil)
(defvar seesaa-blog-email nil)
(defvar seesaa-blog-password nil)
(defvar seesaa-blog-title nil)
;;; これが投げるデータだから下で生成するやつ
;;; (defvar seesaa-blog-content nil)

(defvar seesaa-blog-entry-dir "~/seesaa_entry")
(defvar seesaa-blog-filename-prefix-format "%Y-%m-%d-%H%M%S-")
(defvar seesaa-blog-tmp-file-path nil)

(defun seesaa-blog-define-key ()
  "キーバインド設定"
  (use-local-map seesaa-blog-map)
  (with-current-buffer (get-buffer seesaa-blog-buffer-name);;; これ違う気がする
    (save-excursion
      (define-key seesaa-blog-map (kbd "C-c C-c") 'seesaa-blog-build-html)
      (define-key seesaa-blog-map (kbd "C-c C-p") 'seesaa-blog-publish))))

(defun seesaa-blog-disable-footer ()
  (make-local-variable 'org-export-html-postamble)
  (make-local-variable 'org-export-with-toc)
  (make-local-variable 'org-export-with-section-numbers)
  (setq org-export-html-postamble nil)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil))

(defun seesaa-blog-build-html ()
  "HTMLを生成する。
ファイル名から適当に/tmp/[filename].html を生成する
かつ preview するといいなあ

関数名に対して色々やりすぎているので後で直そう。
"
  (interactive)
  (let* ((file (file-name-nondirectory
                (file-name-sans-extension (buffer-file-name))))
         (path (concat "/tmp/" file ".html")))
    (setq seesaa-blog-tmp-file-path path)
    (save-excursion
      (beginning-of-buffer)
      ())
    (org-export-to-file 'html path nil nil t t)
    ;; window を分割
    (split-window-right)
    ;; 分割先に移動
    (other-window 1)
    (w3m-find-file path)
    (other-window 1)))

(defun seesaa-blog-preview ()
  "生成されたHTML を preview")

(defun seesaa-blog-content-body ()
  "org ファイルからtitle以外の部分を取得する。"
  (save-excursion
    (beginning-of-buffer)
    ;(forward-line)
    (buffer-substring-no-properties (point) (point-max))))

(defun seesaa-blog-contents ()
  "org ファイルからHTMLを取得する"
  (let ((content-org (seesaa-blog-content-body)))
    (with-temp-buffer
      (insert content-org)
      (org-html-export-as-html t t t t)
      (get-buffer "*Org HTML Export*")
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun seesaa-blog-publish ()
  "preview を隣に表示している状態で publish する
publish ってどうするの?
http叩く必要がるかな

XMLRPC を叩く必要がある

叩き先 は blog.seesaa.jp/rpc か
method は metaWeblog.newPost

さて。中身はどう取得しようか
"
  (interactive)
  (xml-rpc-method-call "http://bloc.seesaa.jp/rpc"
                       'metaWeblob.newPost
                       seesaa-blog-id
                       seesaa-blog-email
                       seesaa-blog-password
                       '(("title" . seesaa-blog-title)
                         ("description" . (seesaa-blog-contents)))
                       false))

(defun seesaa-blog-new-entry (subject)
  "記事タイトルをつける"
  (interactive "sタイトル: ")
  (let* ((filename-prefix (format-time-string seesaa-blog-filename-prefix-format (current-time)))
         (filename-suffix ".org")
         (filename (concat filename-prefix subject filename-suffix))
         (filepath (concat (expand-file-name seesaa-blog-entry-dir) "/" filename)))
    (setq seesaa-blog-title subject)
    (make-directory seesaa-blog-entry-dir t)
    (find-file-other-window filepath)
    (seesaa-blog-mode 1)
    (insert (concat "#+TITLE: " subject "\n\n"))
    (end-of-buffer)))

(define-minor-mode seesaa-blog-mode
  "Post to Seesaa Blog"
  :lighter " SeB"
  :global nil
  )


(provide 'seesaa-blog-mode)
;;; seesaa-blog-mode.el ends here
