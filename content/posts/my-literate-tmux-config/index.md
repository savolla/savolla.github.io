+++
title = "My Literate tmux Configuration"
author = ["Kuzey Koç"]
date = 2025-09-12T00:00:00+03:00
tags = ["tmux", "-F"]
draft = false
+++

## General Settings {#general-settings}

```shell
# general settings
setw -g mode-keys vi
set-window-option -g xterm-keys on # for vim
set-option -g status-keys vi
bind-key -Tcopy-mode-vi 'v' send -X begin-selection
bind-key -Tcopy-mode-vi 'y' send -X copy-pipe-and-cancel "xclip -i -sel c"
set -g mode-style bg="#68446a",fg="#ffffff"
unbind C-b # unbind default leader key
set -g prefix M-o # Alt + o prefix
set-option -g history-limit 10000 # large history
set-option -g detach-on-destroy off # ability to kill session without exiting tmux
set-option -g default-terminal "xterm-256color" # "screen-256color" # "screen-256color"
setw -q -g utf8 on # utf8 support
set -g mouse on
set -g terminal-overrides 'xterm*:smcup@:rmcup@'
set -sg escape-time 10
bind-key S-r source-file ~/.config/tmux/tmux.conf # reload tmux conf
bind-key -n C-u copy-mode

# fix for emacs ctrl+enter key not working in terminal mode tmux
set -g allow-passthrough on

```


## Session Management {#session-management}

```shell
# session management
bind-key Space switch-client -l
bind -n C-M-p run-shell "tmux switch-client -p"  # switch to next session
bind -n C-M-n run-shell "tmux switch-client -n"  # switch to prev session
bind-key -n C-M-r command-prompt -p "rename session:" "rename-session '%%'"
bind-key -n C-M-c new-session
bind-key -n C-M-x confirm-before -p "really kill session?" kill-session
bind-key -n C-M-s choose-session # choose session from list

```


## Window Management {#window-management}

```shell
# window management
set -g base-index 1
bind -n C-p previous-window    # switch to prev window
bind -n C-n next-window        # switch to next window
bind-key w new-window         # create a new window
bind-key b choose-window  # choose window from list
bind-key C-x kill-window  # close the current window
bind-key r command-prompt -p "rename window:" "rename-window '%%'"
set-window-option -g window-status-activity-style ''  # disable highlights on events
set-window-option -g window-status-bell-style ''      # disable highlights on events
set -g window-status-format "#[fg=#ffffff,bg=#121212] #W#{?window_zoomed_flag, 󰍉,} "
set-window-option -g window-status-current-format "#[fg=#ffffff,bg=#68446a] #W#{?window_zoomed_flag, 󰍉,} "

```


## Pane Management {#pane-management}

```shell
# pane management
setw -g pane-base-index 1  # start pane numbering from 1 instead of 0
setw -g aggressive-resize on
bind-key -n C-Left resize-pane -L 1   # resize pane
bind-key -n C-Right resize-pane -R 1  # resize pane
bind-key -n C-Up resize-pane -U 1     # resize pane
bind-key -n C-Down resize-pane -D 1   # resize pane
bind-key j select-pane -D  # select pane below
bind-key k select-pane -U  # select pane above
bind-key l select-pane -R  # select pane on the right
bind-key h select-pane -L  # select pane on the left
bind-key v split-window -h -c "#{pane_current_path}"\; select-layout -E
bind-key s split-window -c "#{pane_current_path}"\; select-layout -E
bind-key f run-shell "tmux resize-pane -Z"  # zoom into pane
bind-key S-Right swap-pane -D # move pane down
bind-key S-Left swap-pane -U # move pane up
bind-key x confirm-before -p "kill this pane? (y/n)" kill-pane
bind-key C-g break-pane -a -d # break pane into new window
bind-key t command-prompt -p "window number to join:" "join-pane -s '%%'"

```


## Status Bar {#status-bar}

```shell
# bar
set -g status-right "#[fg=#999999,bg=#111111] #S "
set-option -g status-fg "#afafaf"
set-option -g status-bg "#111111"
set-option -g status-justify left
set-option -g status-position bottom
set-option -g status-interval 60 # refresh status bar per one second
set -g status-left ""

```
