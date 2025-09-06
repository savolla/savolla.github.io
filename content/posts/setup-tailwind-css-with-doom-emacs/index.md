+++
title = "Setup Tailwind CSS with Doom Emacs"
author = ["Kuzey Koç"]
date = 2025-09-06T00:00:00+03:00
tags = ["doomemacs", "tailwind", "css", "-F"]
draft = false
+++

Project repo [merrickluo/lsp-tailwindcss](https://github.com/merrickluo/lsp-tailwindcss)

Tailwind does not play well with Doom Emacs. I spent a significant amount of time to set it up.. So here is how you do it

```emacs-lisp
;; tailwind
(use-package! lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-server-version "0.14.8"
        lsp-tailwindcss-skip-config-check t))
```

-   As you see you need to tell tailwind lsp to load as an add-on to prevent replacing the major lsp such as `rjsx` or other javascript lsps.
-   Only the `0.14.8` version of the tailwind server works correctly (speaking for 2025)
-   And the last option is from a GitHub issue that I found back in the day. I don't remember which one..
