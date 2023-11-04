#!/bin/bash
# this script will modify the url inside
# my qr cıde tattoo. run this script with
# a url as parameter

SCRIPT_PATH="../scripts/qr-tattoo-url-redirector.js"
echo "window.onload = function() { window.location.href = \""$1"\";}" > $SCRIPT_PATH


