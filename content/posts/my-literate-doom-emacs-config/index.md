+++
title = "savolla's Literate Doom Emacs Configuration"
author = ["Kuzey Koç"]
date = 2025-09-01T00:00:00+03:00
tags = ["emacs", "-F"]
draft = false
+++

## Basic Customizations {#basic-customizations}


### Header {#header}

```emacs-lisp
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
```


### Dashboard {#dashboard}

Nothing is better than a clean and customized dashboard. Here is my "suckless" stiled username and clean as possible dashboard. Btw I couldn't find a way to disable that github icon but anyway.

```emacs-lisp
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "")))
(defun my-weebery-is-always-greater ()
  (let* ((banner '(
                   "                        ██  ██        "
                   "                        ██  ██        "
                   "  ████████████  ██████████  ██  ██████"
                   "  ██  ██▄▄████  ████  ████  ██  ██▄▄██"
                   "████  ██  ██  ██  ████████████████  ██"
                   ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property

     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))
(setq +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)
```


### Persoal Information {#persoal-information}

```emacs-lisp
;; personal info
(setq user-full-name "Kuzey Koç"
      user-mail-address "savolla@protonmail.com")
```


### Fonts {#fonts}

I personally like narrow mono fonts. Iosevka is already narrow but IosevkaTerm is better as a terminal font and programming in general. More characters fit in a single line

```emacs-lisp
;; font configuration
(setq doom-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 24))
```


### Theme {#theme}

I'm a big fan of Gruvbox color scheme.

```emacs-lisp
;; theme
(setq doom-theme 'doom-gruvbox)
```

But I don't like the default low contrast background of it so I made the background more darker. This setting is also better to use if you use transparency in emacs

```emacs-lisp
;; make darker background in gruvbox
(after! doom-themes
  (custom-set-faces!
    '(default :background "#000000")))
```

Make emacs transparent. Right now it only works with `emacs-gtk` in NixOS

```emacs-lisp
(add-to-list 'default-frame-alist '(alpha-background . 70))
```


### Line Numbers {#line-numbers}

```emacs-lisp
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
```


### Window Separator {#window-separator}

Doom's line separator is not visible at all. so I change it's width and color for better visibility

```emacs-lisp
;;; thicken window separator
(setq window-divider-default-right-width 2
      window-divider-default-bottom-width 2)

;;; change color of separator
(custom-set-faces! '(vertical-border :foreground "#877b75"))
```


### Line Wrapping {#line-wrapping}

this gives me a little bit of headache because line wrapping also affects my **vterm** shell. I need to find a way (a hook maybe?) to disable this in vterm

```emacs-lisp
(global-visual-line-mode t) ; enable line wrapping globally
```


### Workspaces {#workspaces}

I changed some default workspace keybindings

```emacs-lisp
;; workspaces
(map! :leader :desc "switch to previous workspace" "TAB h"   #'+workspace:switch-previous )
(map! :leader :desc "switch to next workspace"     "TAB l"   #'+workspace:switch-next )
(map! :leader :desc "last workspace"               "TAB TAB" #'+workspace/other )
(map! :leader :desc "display workspaces"           "TAB SPC" #'+workspace/display )
```


### Buffers {#buffers}

I made some improvements for easier buffer navigation like switching to last buffer by hitting `space b b` and selecting buffers interactively with `space b l`

```emacs-lisp
;; buffers
(map! :leader :desc "switch buffer"
      "b l" #'consult-buffer)
(map! :leader :desc "last buffer"
      "b b" #'evil-switch-to-windows-last-buffer)
```


### Tabs {#tabs}

Navigate tabs with `t` and `T`

```emacs-lisp
(map! :n "t" #'centaur-tabs-forward )
(map! :n "T" #'centaur-tabs-backward )
```

Some cosmetics for better looking

```emacs-lisp
(setq
 tab-bar-close-button-show nil
 tab-bar-new-button-show nil
 tab-bar-separator "|"
 )
```

Tabs configuration continues in the [Centaur Tabs](#centaur-tabs) section


### Windows {#windows}

```emacs-lisp
;; window
(setq evil-vsplit-window-right t) ;; automatic focus when splitted
(setq evil-split-window-below t) ;; automatic focus when splitted
(setq split-width-threshold 240)
```


### Some QoL Improvements {#some-qol-improvements}

I really don't like the "whole line highlighting" in my editor. it may improve visibility of the current position but I prefer brighter cursor colors like `#00FF00` instead

```emacs-lisp
(setq global-hl-line-modes nil) ; disable current line hightlight
```

Doom comes with line highlighting by default. I set my cursor's color to more vibrant color. So I don't need this

```emacs-lisp
(setq global-hl-line-modes nil) ; disable current line hightlight
```

I changed my cursor color based on mode

```emacs-lisp
;; change cursor colors by mode
(setq evil-insert-state-cursor '(box "#00FF00")
      evil-visual-state-cursor '(box "orange")
      evil-normal-state-cursor '(box "#E2E8EF"))
```


## Package Based Customizations {#package-based-customizations}


### Evil {#evil}

I mostly use `evil-mode` to scrach my **vim** itch so I modified this mode to my liking

In vim, very long lines often wrap across multiple screen lines for visibility. This causes the cursor to jump several lines when using j or k. I fix this issue with the following configuration:

```emacs-lisp
;; better j k experience skip pressing "g j" or "g k"
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
```

I find the `Escape` key too far from the home row, so I brought it closer.

```emacs-lisp
(setq evil-escape-key-sequence "jk") ;; escape from evil mode using j+k keys
```

In Emacs when you hit `A` key while the `evil-mode` is active, your cursor jumps to the end of line and enters into **insert** mode. but when you hit `Escape` key it moves the cursor one character back. I fixed this annoying behavior with the following

```emacs-lisp
(setq evil-move-cursor-back nil) ;; don't move cursor back when exiting insert mode
(setq evil-move-beyond-eol t) ;; can place cursor at the end of line. huge qol improvement
```

Since `L` and `H` keys don't have a meaningful purpose in `evil-mode` I decided to use them for jumping between **begining of line** and **end of line**

```emacs-lisp
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line) ;; shift + l go to end of line
(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank) ;; shift + h go to start of line
```

History option to track history better in doom emacs

```emacs-lisp
(setq evil-want-fine-undo t) ;; track history better
```

When you hit `s` key in **vim** it deletes the character under the cursor and enters into the insert mode. I need this behavior but doom emacs replaces this functionality with a mode called `evil-snipe`. I mostly use `ivy` for this purpose. So I got my `s` key back by disabling `evil-snipe`.

```emacs-lisp
;; save 's' from evil-snipe
(after! evil-snipe
  ;; Disable evil-snipe overriding 's' and 'S' in normal mode
  (evil-snipe-mode -1)
  (evil-snipe-override-mode -1))
```

And re-enabled the `s` behavior like this

```emacs-lisp
(define-key evil-normal-state-map (kbd "s") 'evil-substitute) ;; delete current char and enter in insert mode
```


### Avy {#avy}

I use `avy-mode` for a faster navigation inside my buffer. I hit `F` to activate `avy` then press the character that I want to jump to. After I finish my work on that area I hit `Ctrl-o` to jump back

I was using this functionality with lower case `f` before but I couldn't use `f` key while composing vim macros. So I made it `F` instead.

```emacs-lisp
;; avy
(map! :n "F" #'avy-goto-char :v "F" #'avy-goto-char) ;; easymotion like in lazyvim
```


### Completion Frameworks {#completion-frameworks}


#### Company {#company}

I use `company` for completion because it's mature and stable. But I'm waiting for `corfu` to become more stable and feature rich at the same time.

I like to use `Tab` key for comletions and I also want to get the completion buffer as fast as possible. Here is my small configuration for `company`

```emacs-lisp
;; company
(after! company
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection) ;; use tab to complete (does not work in tui)
  (setq company-selection-wrap-around 1) ;; cycle through options
  (setq company-idle-delay 0.1) ;; get completion buffer faster
  )
```


#### Corfu {#corfu}

I don't use the following `corfu` config because it doen't really work like I want but I still want to tangle it for reference

```emacs-lisp
;; corfu
;; (after! corfu
;;   (setq +corfu-want-tab-prefer-expand-snippets t)
;;   (map! :map corfu-map
;;         ;;;; use tab to accept completion (does not work in tui)
;;         :i [tab] #'corfu-complete                ;; accept completion
;;         :i "TAB" #'corfu-complete
;;         :i "C-j" #'corfu-next                    ;; next completion
;;         :i "C-k" #'corfu-previous                ;; previous completion
;;         :i "C-s" #'corfu-insert-separator        ;; insert blank space (+orderless)
;;         ))
```


### Treemacs {#treemacs}

Project repo [GitHub - Alexander-Miller/treemacs](https://github.com/Alexander-Miller/treemacs)

I use `treemacs` package to display and quickly delete/rename/create files in my projects.

```emacs-lisp
;; treemacs
(after! treemacs
  (treemacs-follow-mode 1)
  (setq treemacs-width 25)
  (setq treemacs-position 'right)
  (setq treemacs-text-scale -1) ;; enable if using emacs gui
  (setq treemacs-git-mode 'extended)
  )
(map! :leader :desc "open treemacs" "o p"   #'+treemacs/toggle )
```


### Gptel {#gptel}

Project repo [GitHub - karthink/gptel: A simple LLM client for Emacs](https://github.com/karthink/gptel)

I like to play with local llms so I installed a package called `gptel`. I use a small version of **deepseek** model locally using `ollama`.

```emacs-lisp
;; llm
(setq
 gptel-model 'deepseek-r1:8b
 gptel-backend (gptel-make-ollama "ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(deepseek-r1:8b)))
```


### Vterm with `tmux` {#vterm-with-tmux}

I use `vterm` for all my terminal needs in emacs. But I also heavily configured it to my likings. When I toggle `vter` buffer it also reads the current workspace name and opens a `tmux` session with the same name. This is quite handy because I can manage my terminals based on workspace and projects. For example I use `projectile` package for project management. And when I open a new project it opens it in a separate workspace with project name. So when I open `vterm` it also automatically creates a new tmux session with project's name.

Setup some environment for `vterm` first. Since I use **NixOS** as my daily driver, I don't have FHS in my system. Therefore packages like `zsh` live in different places

```emacs-lisp
;; vterm
;;; NixOS-specific shell path configuration
(let ((zsh-path (shell-command-to-string "readlink -f $(which zsh)")))
  (setq explicit-shell-file-name (string-trim zsh-path))
  (setq vterm-shell (string-trim zsh-path)))

(setq vterm-environment '("TERM=xterm-256color"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
```

I prefer faster refresh times in my terminal. So it feels snappy and responsive.

```emacs-lisp
(setq vterm-timer-delay 0.005) ;; Reduce delay for better responsiveness
```

Some settings for popup terminal

```emacs-lisp
(after! vterm
  (set-popup-rule! "*doom:vterm-popup"
    :size 0.35
    :select t
    :quit nil))
```

Since I use `tmux` inside my `vterm` terminals, it means that there is <span class="underline">always</span> a process (tmux) running. This causes `vterm` to warn me about the running process when I try to exit from it. I disable this warning and confirmation

```emacs-lisp
;; Avoid prompt when killing vterm buffers with running processes
(remove-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function)
```

I use `Alt+o` for my `tmux` prefix key because it is so easy to reach. But sening this kind of meta key combination to `vterm` is not allowed by default. We fix it like this

```emacs-lisp
;; Allow tmux prefix (M-o) passthrough
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-o") #'vterm--self-insert))
```

Lastly I invoke `tmux` when I open `vterm` with the following config

```emacs-lisp
;; tmux integration
(defvar-local my/vterm-tmux-started nil
  "Whether tmux has already been started in this vterm buffer.")

(defun my/vterm-start-tmux ()
  "Start tmux session in vterm once, avoiding nesting."
  (unless my/vterm-tmux-started
    (setq my/vterm-tmux-started t)
    (let ((workspace (persp-name (get-current-persp))))
      (vterm-send-string
       (format "sh -c 'unset TMUX; exec tmux new-session -A -s \"%s\"'" workspace))
      (vterm-send-return))))

(defun my/vterm-setup ()
  "Custom setup for `vterm-mode`."
  (visual-line-mode -1) ;; Disable line wrap for better zsh modeline rendering
  (run-with-timer 0.3 nil #'my/vterm-start-tmux))

(add-hook 'vterm-mode-hook #'my/vterm-setup)
```


### Centaur Tabs {#centaur-tabs}

Project repo [ema2159/centaur-tabs](https://github.com/ema2159/centaur-tabs)

```emacs-lisp
;;; centaur tabs
(setq
 centaur-tabs-style "alternate"
 centaur-tabs-height 35

 ;; display highlight bar bottom
 centaur-tabs-set-bar 'under
 x-underline-at-descent-line t

 ;; disable "X" close button
 centaur-tabs-close-button " "

 ;; disable new tab button
 centaur-tabs-show-new-tab-button nil

 ;; make new tabs open to the right
 centaur-tabs-adjust-buffer-order 'right

 ;; don't gray out file icons
 centaur-tabs-gray-out-icons 'buffer
 ;; centaur-tabs-gray-out-icons nil
 )

;; disable groupings for tabs
(defun my/centaur-tabs-buffer-groups ()
  "Disable grouping in centaur-tabs. All buffers go in the same group."
  '("Default"))

(setq centaur-tabs-buffer-groups-function 'my/centaur-tabs-buffer-groups)
```


### Emmet {#emmet}

Project repo [smihica/emmet-mode](https://github.com/smihica/emmet-mode)

Emmet helps me spit `html` at light speed. First let's enable `emmet` in the following web related modes

```emacs-lisp
;; emmet
;; hooks
(add-hook 'html-mode-hook  'emmet-mode) ;; enable emmet in html mode
(add-hook 'css-mode-hook  'emmet-mode) ;; enable emmet's css abbreviation.
(add-hook 'rjsx-mode-hook  'emmet-mode) ;; enable emmet in rjsx mode
(add-hook 'js-jsx-mode-hook  'emmet-mode) ;; enable emmet in jsx mode
```

Setup keybindings for `emmet` for seamless usage like in `vscode`

```emacs-lisp
;; keybinds
(map! :map emmet-mode-keymap
      :i "<C-return>" #'emmet-expand-line
      :i "<tab>" #'emmet-next-edit-point
      :i "<backtab>" #'emmet-prev-edit-point
      )
```

Make `emmet` be aware of `jsx` mode

```emacs-lisp
(setq emmet-expand-jsx-className? t) ;; default nil
```


### Tailwind {#tailwind}

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

you also need to do some adjustments in `packages.el` file. Here is the `packages.el` setting.

```emacs-lisp
;; tailwind lsp
(package! lsp-tailwindcss :recipe
  (:type git
   :host github
   :repo "merrickluo/lsp-tailwindcss"))
```


### Org Mode {#org-mode}

This is the biggest reason why I use Emacs. Just like others do.
