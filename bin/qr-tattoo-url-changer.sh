#!/bin/bash
# this script will modify the url inside
# my qr cıde tattoo. run this script with
# a url as parameter

SCRIPT_PATH="../scripts/bilal.js"
echo "window.onload = function() { window.location.href = "$1";}" > $SCRIPT_PATH


