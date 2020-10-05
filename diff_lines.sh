#!/bin/sh

FILTER="@@ .* @@"
SED_EXP="s/^@@ \(.*\) @@$/\1/"

git diff -U0 $2 $3 -- $1 | grep "${FILTER}" | sed -e "${SED_EXP}"
