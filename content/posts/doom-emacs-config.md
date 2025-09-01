+++
title = "My Literal Doom Emacs Config"
author = ["Kuzey Koç"]
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


### Some QoL Improvements {#some-qol-improvements}

I really don't like the "whole line highlighting" in my editor. it may improve visibility of the current position but I prefer brighter cursor colors like `#00FF00` instead

```emacs-lisp
(setq global-hl-line-modes nil) ; disable current line hightlight
```


## Package Based Customizations {#package-based-customizations}


### Evil {#evil}

```emacs-lisp
(setq global-hl-line-modes nil) ; disable current line hightlight
```
