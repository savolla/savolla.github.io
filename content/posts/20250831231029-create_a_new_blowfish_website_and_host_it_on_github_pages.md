+++
title = "Create a Hugo Blog with Emacs Org-Mode and Publish it on GitHub Pages"
author = ["Kuzey Koç"]
date = 2025-08-31T00:00:00+03:00
tags = ["-F", "hugo", "github", "emacs"]
draft = false
+++

-   installed hugo
-   create a new hugo page
    ```shell
    hugo new site savolla.github.io
    cd savolla.github.io
    git submodule add -b main https://github.com/nunocoracao/blowfish.git themes/blowfish
    cp -r themes/blowfish/config .
    ```
-   uncomment theme setting in `config/_default/hugo.toml`
-   start hugo server
    ```shell
    hugo server
    ```
-   edit website
    -   add your posts on home page

-   build website inside `/docs`
    ```shell
    hugo -d docs
    ```
-   commit changes
-   push to github
-   go to settings of your github pages repo
-   change site directory to `main` and `docs`
-   save changes
-   add syntax highlighting to your posts by putting the following into your `config/_default/markup.toml`
    ```toml { hl_lines=["1","2"] }
    [highlight]
      codeFences = true
      noClasses = true
      style = "gruvbox"
    ```


## setup hugo with org-mode {#setup-hugo-with-org-mode}

to create a page for hugo you'll need to setup the following settings on each file. you need to specify the root directory of your hugo site and hugo section.

```org
#+HUGO_BASE_DIR: ~/project/publishing/savolla.github.io
#+HUGO_SECTION: posts
```


## Create an On-Save Hook {#create-an-on-save-hook}

You can create an on-save hook to make emacs automatically export your page hugo page to the base directory like this:

define your function somewhere in your `config.el`

```elisp
(defun my/org-mode-hugo-auto-export ()
  "Automatically export to Hugo-compatible Markdown on save if the Org file is for ox-hugo."
  (when (and (eq major-mode 'org-mode)
             (string-match-p "HUGO_BASE_DIR" (buffer-string))) ; only if it's a Hugo post
    (org-hugo-export-wim-to-md)))
```

then use the following hook under your hooks section in `config.el`

```elisp
(add-hook 'after-save-hook #'my/org-mode-hugo-auto-export)
```


## References {#references}

-   [ox-hugo - Org to Hugo exporter](https://ox-hugo.scripter.co/)
