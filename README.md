# seesaa-blog-mode

## これは何?

org-mode を使って Emacs から seesaa blog に投稿するためのもの。

## インストール

### GitHub から clone する場合 ###

1. このリポジトリを clone: git clone https://github.com/mugijiru/seesaa-blog-mode.git
2. 'seesaa-blog-mode' を load-path に追加し、require する

```lisp
(add-to-list 'load-path (expand-file-name "~/projects/dev/seesaa-blog-mode/"))
(require 'seesaa-blog-mode)
```

### el-get を使う場合 ###

seesaa-blog-mode.rcp を作る

```lisp:seesaa-blog-mode.rcp
(:name seesaa-blog-mode
       :description "create seesaa blog entry and publish with org-mode"
       :type github
       :features "seesaa-blog-mode"
       :depends (s f xml-rpc)
       :pkgname "mugijiru/seesaa-blog-mode")
```

そして

`M-x el-get-install RET seesaa-blog-mode RET`

## 設定

ログインに必要な情報を適宜設定

```lisp
(setq seesaa-blog-id "YOUR-BLOG-ID")
(setq seesaa-blog-email "YOUR-EMAIL")
(setq seesaa-blog-password "YOUR-PASSWORD")
```

## 使い方


`M-x seesaa-blog-new-entry`

で新記事データを作成。

タイトルや本文を記入後

`C-c C-c`

で記事をHTML化 & ブラウザでプレビュー。

`C-c C-p` で記事を公開。

## ライセンス

MIT License
