+++
title = "Wallpapers"
author = ["Kuzey Koç"]
date = 2025-09-13T00:00:00+03:00
tags = ["personal", "-F"]
draft = false
+++

```bash
WALLPAPER_DIR="/home/savolla/resource/images/wallpapers/favorites"
for wallpaper in $(ls "$WALLPAPER_DIR")
do

    echo "#+attr_html: :width 256px"
    echo "#+CAPTION: $wallpaper"
    echo "[[file:$WALLPAPER_DIR/$wallpaper]]"
done
```

```bash

```
